-module(session_server_fsm).
-author('mkorszun@gmail.com').

-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, 'STARTED'/2, 'ACTIVE'/2, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3, code_change/4]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include("logger.hrl").
-include("session_server.hrl").

%% ###############################################################
%% STATE
%% ###############################################################

-record(state, {id, key, timeout, start_time}).

%% ###############################################################
%% API
%% ###############################################################

start_link(ApplicationKey, DeviceId) ->
    gen_fsm:start_link({local, ?L2A(DeviceId)}, ?MODULE, [ApplicationKey, DeviceId], []).

%% ###############################################################
%% GEN_FSM CALLBACKS
%% ###############################################################

init([ApplicationKey, DeviceId]) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Timeout} = application:get_env(App, timeout),
    session_counter_api:add(ApplicationKey, self()),
    {ok, 'STARTED', #state{id = DeviceId, key = ApplicationKey,
        start_time = erlang:now(), timeout = Timeout}, Timeout}.

'STARTED'(ping, #state{timeout=Timeout} = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'STARTED'(timeout, State) ->
    {stop, normal, State};
'STARTED'(stop, State) ->
    {stop, normal, State}.

'ACTIVE'(ping, #state{timeout=Timeout} = State) ->
    {next_state, 'ACTIVE', State, Timeout};
'ACTIVE'(timeout, State) ->
    {stop, normal, State};
'ACTIVE'(stop, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, StateName, #state{id = Id, key = ApplicationKey, start_time = Start}) ->
    update_stats(ApplicationKey, Id, Start, Reason, StateName).

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

update_stats(ApplicationKey, Id, Start, Reason, StateName) ->
    SessionTime = timer:now_diff(erlang:now(), Start) div 1000000,
    Stats = stats(SessionTime, Reason, StateName),
    try session_server_stats:save(?L2B(ApplicationKey), ?L2B(Id), timestamp(Start), Stats) of
        ok ->
            ?INF("Session terminated. Stats saved - ~s ~s ~p: ~p",
                [ApplicationKey, Id, SessionTime, Reason]),
            ok;
        {error, Reason} ->
            ?ERR("Failed to save session time - ~s ~s ~p: ~p",
                [ApplicationKey, Id, SessionTime, Reason]),
            ok
    catch
        _:Reason ->
            ?ERR("Failed to save session time - ~s ~s ~p: ~p",
                [ApplicationKey, Id, SessionTime, Reason]),
            ok
    end.

stats(SessionTime, Reason, StateName) ->
    [{duration, SessionTime}, {reason, Reason}, {state, StateName}].

timestamp({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

%% ###############################################################
%% ###############################################################
%% ###############################################################