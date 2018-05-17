%%% @doc This module allows to manage file chunks.

-module(es3_chunk).

-behaviour(gen_server).


-export([start_link/1, init/1, terminate/2,
		handle_call/3, handle_cast/2]).

-export([read/1, delete/1, write/2]).

%% Constants definition

-define(SERVER, ?MODULE).
-define(CHUNKS_TABLE, file_chunks).

%% gen_server public API

-spec write(Key :: any(), Chunk :: binary()) -> ok | {error, Reason :: any()}.
write(Key, Chunk) ->
    gen_server:cast(?SERVER, {write_chunk, Key, Chunk}).

-spec read(Key :: any()) -> Chunk :: binary() | {error, Reason :: any()}.
read(Key) ->
    gen_server:call(?SERVER, {get_chunk, Key}).

-spec delete(Key :: any()) -> ok | {error, Reason :: any()}.
delete(Key) ->
    gen_server:cast(?SERVER, {delete_chunk, Key}).

start_link(_) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [debug, {log, trace}]).

init(_) ->
    {ok, _} = dets:open_file(?CHUNKS_TABLE, [{type, set}]),
    {ok, no_state}.

%% gen_server callbacks

handle_cast({write_chunk, Key, Chunk}, State) ->
    dets:insert(?CHUNKS_TABLE, {Key, Chunk}),
    {noreply, State};
handle_cast({delete_chunk, Key}, State) ->
    dets:delete(?CHUNKS_TABLE, Key),
    {noreply, State}.

handle_call({get_chunk, Key}, _From, State) ->
    case dets:lookup(?CHUNKS_TABLE, Key) of
        []      -> {reply, {error, not_found}, State};
        [Chunk] -> {reply, Chunk, State}
    end.

terminate(_Reason, _State) -> ok.
