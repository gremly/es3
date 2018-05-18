%%% @doc This module allows to manage file chunks.

-module(es3_chunk).

-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2,
        handle_call/3, handle_cast/2, handle_info/2,
        code_change/3]).

-export([read/1, delete/1, list/1, write/2]).

%% -------------------------------------------------------------
%% Constants definition
%% -------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CHUNKS_TABLE, file_chunks).

%% -------------------------------------------------------------
%% gen_server public API
%% -------------------------------------------------------------

-spec write(Key :: any(), Chunk :: binary()) -> ok | {error, Reason :: any()}.
write(Key, Chunk) ->
    gen_server:cast(?SERVER, {write_chunk, Key, Chunk}).

-spec read(Key :: any()) -> Chunk :: binary() | {error, Reason :: any()}.
read(Key) ->
    gen_server:call(?SERVER, {get_chunk, Key}).

-spec list(Keys :: list()) -> Chunks :: list() | {error, Reason :: any()}.
list(Keys) ->
    gen_server:call(?SERVER, {list_chunks, Keys}).

-spec delete(Key :: any()) -> ok | {error, Reason :: any()}.
delete(Key) ->
    gen_server:cast(?SERVER, {delete_chunk, Key}).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, _} = dets:open_file(?CHUNKS_TABLE, [{type, set}, {file, "priv/" ++ node()}]),
    {ok, no_state}.

%% -------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------

handle_cast({write_chunk, Key, Chunk}, State) ->
    dets:insert(?CHUNKS_TABLE, {Key, Chunk}),
    {noreply, State};
handle_cast({delete_chunk, Key}, State) ->
    dets:delete(?CHUNKS_TABLE, Key),
    {noreply, State}.

handle_call({get_chunk, Key}, _From, State) ->
    case get_by_key(Key) of
        nil   -> {reply, {error, not_found}, State};
        Chunk -> {reply, Chunk, State}
    end;
handle_call({list_chunks, Keys}, _From, State) ->
    List = lists:map(fun get_by_key/1, Keys),
    {reply, lists:filter(fun (Element) -> Element =/= nil end, List), State}.

handle_info(_, State) -> {noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%% -------------------------------------------------------------
%% Internal Functions
%% -------------------------------------------------------------

get_by_key(Key) ->
    case dets:lookup(?CHUNKS_TABLE, Key) of
        []      -> nil;
        [Chunk] -> Chunk
    end.
