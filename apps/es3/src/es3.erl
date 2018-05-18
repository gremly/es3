%%% @doc This is the main interface to manage files
%%%      from the client side.

-module(es3).

-export([read/1, delete/1, get_config/1, write/2]).

-define(CHUNKS, get_config(chunks_number)).

-spec write(Name, Object) -> Res when
      Name   :: iodata(),
      Object :: binary(),
      Res    :: ok | {error, Reason :: any()}.
write(Name, Object) when is_binary(Object) ->
    Chunks = build_chunks(Object),
    es3_cluster:write(Name, Chunks).

-spec read(Name) -> Object when
      Name   :: iodata(),
      Object :: binary() | {error, Reason :: any()}.
read(Name) ->
    es3_cluster:read(Name).

-spec delete(Name) -> Res when
      Name :: iodata(),
      Res  :: ok | {error, Reason :: any()}.
delete(Name) ->
    es3_cluster:delete(Name).

-spec get_config(Key :: atom()) -> Value :: any().
get_config(Key) ->
    {ok, Value} = application:get_env(es3, Key),
    Value.

%% -----------------------------------------
%% Internal functions
%% -----------------------------------------

build_chunks(Object) ->
    ChunkLength = byte_size(Object) div ?CHUNKS,
    divide_object(Object, ChunkLength, []).

divide_object(<<>>, _, Acc) -> Acc;
divide_object(Object, Length, Acc) when byte_size(Object) < (Length * 2) ->
    [Object|Acc];
divide_object(Object, Length, Acc) -> 
    Head = binary_part(Object, 0, Length),
    Tail = binary_part(Object, Length, byte_size(Object) - Length),
    divide_object(Tail, Length, [Head|Acc]).
