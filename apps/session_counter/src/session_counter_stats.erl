-module(session_counter_stats).
-author('mkorszun@gmail.com').

-export([save/3, read/3]).

%% ###############################################################
%% INCLUDE
%% ###############################################################

-include("session_counter.hrl").

%% ###############################################################
%% MACROS
%% ###############################################################

-define(BUCKET(Key), <<"time_metrics/", Key/binary>>).

%% ###############################################################
%% API
%% ###############################################################

-spec save(binary(), binary(), integer()) -> ok.
save(Key, Timestamp, Count) ->
    Obj = riakc_obj:new(?BUCKET(Key), Timestamp, Count),
    riakc_pb_socket:put(pooler:take_member(gizmo), Obj).

-spec read(binary(), integer(), integer()) -> {ok, list()}.
read(Key, Start, End) ->
    {ok, Res} = riakc_pb_socket:mapred_bucket(pooler:take_member(gizmo),
        ?BUCKET(Key),[
            {map, {qfun, fun(O,_,_) -> map(O, Start, End) end}, none, true},
            {reduce, {qfun, fun(List, _) -> reduce(List) end}, none, true}]),
    result(active_sessions, Res).

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

result(active_sessions, [{0, Results}, {1, Total}]) ->
    {ok, Total ++ Results};
result(active_sessions, [{1, [{struct,[{total,0}]}]}]) ->
    {ok, []};
result(active_sessions, []) ->
    {ok, []}.

map(Object, Start, End) ->
    case list_to_integer(binary_to_list(riak_object:key(Object))) of
        T when T >= Start andalso T =< End ->
            [{struct, [{T, ?B2T(riak_object:get_value(Object))}]}];
        _ ->
            []
    end.

reduce(Objects) ->
    Red = fun({struct, [{_, C}]}, Count) -> Count + C end,
    [{struct, [{total, lists:foldl(Red, 0, Objects)}]}].

%% ###############################################################
%% ###############################################################
%% ###############################################################