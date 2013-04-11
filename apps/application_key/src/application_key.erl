-module(application_key).
-author('mkorszun@gmail.com').

-export([generate/0, exists/1]).

%% ###############################################################
%% MACROS
%% ###############################################################

-define(BUCKET, <<"application_keys">>).

%% ###############################################################
%% API
%% ###############################################################

-spec generate() -> binary().
generate() ->
    Key = list_to_binary(uuid:to_string(uuid:uuid4())),
    Object = riakc_obj:new(?BUCKET, Key, []),
    Conn = pooler:take_member(gizmo),
    case riakc_pb_socket:put(Conn, Object, [return_head]) of
        {ok, Obj} ->
            {ok, riakc_obj:key(Obj)};
        {error, Error} ->
            {error, Error}
    end.

-spec exists(binary() | string()) -> true | false.
exists(GameKey) when is_list(GameKey) ->
    exists(list_to_binary(GameKey));
exists(GameKey) when is_binary(GameKey) ->
    case read(GameKey) of
        {ok, _} ->
            true;
        {error, notfound} ->
            false;
        {error, Reason} ->
            throw(Reason)
    end.

%% ###############################################################
%% INTERNAL FUNCTIONS
%% ###############################################################

read(Key) ->
    case riakc_pb_socket:get(pooler:take_member(gizmo), ?BUCKET, Key) of
        {ok, Obj} ->
            {ok, binary_to_term(riakc_obj:get_value(Obj))};
        {error, Error} ->
            {error, Error}
    end.

%% ###############################################################
%% ###############################################################
%% ###############################################################