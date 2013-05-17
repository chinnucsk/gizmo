-module(session_server_api).
-author('mkorszun@gmail.com').

-export([ping/2]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include_lib("utils/include/types.hrl").

%% ###############################################################
%% API
%% ###############################################################

%% @doc Sends ping to given device process
-spec ping(string(), string()) -> ok | {error, term()}.
ping(ApplicationKey, DeviceId) ->
    case application_obj:exists(?L2B(ApplicationKey)) of
        true ->
            do_ping(ApplicationKey, ?L2A(DeviceId));
        false ->
            {error, application_not_found};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_ping(ApplicationKey, DeviceId) ->
    case ensure_session_started(ApplicationKey, DeviceId) of
        {ok, running} ->
            gen_fsm:send_event(DeviceId, ping);
        {error, Error} ->
            {error, Error}
    end.

ensure_session_started(ApplicationKey, DeviceId) ->
    case session_server_sup:start_child(ApplicationKey, DeviceId) of
        {ok, Pid} -> {ok, running};
        {error, {already_started, _}} -> {ok, running};
        {error, Error} -> {error, Error}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################