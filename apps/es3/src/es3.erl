%%% @doc This is the main interface to manage files
%%%      from the client side.

-module(es3).

-export([read/1, delete/1, write/2]).

-spec write(Name, Object) -> Res when
      Name   :: iodata(),
      Object :: binary(),
      Res    :: ok | {error, Reason :: any()}.

write(Name, Object) ->
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
