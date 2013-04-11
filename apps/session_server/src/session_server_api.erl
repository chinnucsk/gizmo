-module(session_server_api).
-author('mkorszun@gmail.com').

-export([ping/2]).

%% ###############################################################
%% MACROS
%% ###############################################################

-include("session_server.hrl").

%% ###############################################################
%% API
%% ###############################################################

-spec ping(string(), string()) -> ok | {error, term()}.
ping(ApplicationKey, DeviceId) ->
    case application_key:exists(?L2B(ApplicationKey)) of
        true ->
            do_ping(ApplicationKey, DeviceId);
        false ->
            {error, application_not_found}
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

do_ping(ApplicationKey, DeviceId) ->
    case ensure_session_started(ApplicationKey, DeviceId) of
        {ok, running} ->
            gen_fsm:send_event(?L2A(DeviceId), ping);
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