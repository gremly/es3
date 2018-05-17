-module(es3_chunk).

-export([read/1, delete/1, write/2]).

-spec write(Key :: any(), Chunk :: binary()) -> ok | {error, Reason :: any()}.

write(Key, Chunk) ->
    ok.

-spec read(Key :: any()) -> Chunk :: binary() | {error, Reason :: any()}.

read(Key) ->
    <<"Some binary">>.

-spec delete(Key :: any()) -> ok | {error, Reason :: any()}.

delete(Key) ->
    ok.
