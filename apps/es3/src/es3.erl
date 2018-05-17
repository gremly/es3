%%% @doc This is the main interface to manage files
%%%      from the client side.

-module(es3).

-export([read/1, delete/1, write/2]).

-define(CHUNKS, 5).

-spec write(Name, Object) -> Res when
      Name   :: iodata(),
      Object :: binary(),
      Res    :: ok | {error, Reason :: any()}.

write(Name, Object) when is_binary(Object) ->
    Chunks = build_chunks(Object),
    io:format("~p Chunks obtained. ~n", [length(Chunks)]),
    ok.

-spec read(Name) -> Object when
      Name   :: iodata(),
      Object :: binary() | {error, Reason :: any()}.

read(Name) ->
    <<"some binary">>.

-spec delete(Name) -> Res when
      Name :: iodata(),
      Res  :: ok | {error, Reason :: any()}.

delete(Name) ->
    ok.

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
