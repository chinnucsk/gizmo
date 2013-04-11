-module(session_server_stats).
-author('mkorszun@gmail.com').

-export([save/4, read/2]).

%% ###############################################################
%% API
%% ###############################################################

-spec save(binary(), binary(), integer(), list()) -> ok | {error, term()}.
save(ApplicationKey, DeviceId, Timestamp, Stats) ->
    statebox_riak:apply_bucket_ops(ApplicationKey,
        [{[DeviceId], statebox_orddict:f_store(Timestamp, Stats)}],
        statebox_riak:new([{riakc_pb_socket, pooler:take_member(gizmo)}])).

-spec read(binary(), binary()) -> {ok, list()} | {error, term()}.
read(ApplicationKey, DeviceId) ->
    statebox_riak:get_value(ApplicationKey, DeviceId,
        statebox_riak:new([{riakc_pb_socket, pooler:take_member(gizmo)}])).

%% ###############################################################
%% ###############################################################
%% ###############################################################