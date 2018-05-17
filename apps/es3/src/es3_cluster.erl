%% @doc This module manages the interaction between nodes
%%      to write, read and delete file chunks in a distributed
%%      fashion.
-module(es3_cluster).

-export([read/1, delete/1, write/2]).

%% ------------------------------------------------
%% Public API 
%% ------------------------------------------------

-spec read(Name :: iodata()) -> binary() | {error, Reason :: any()}.
read(Name) ->
    [{_, Name, NodesChunks}] = es3_repo:read(Name),
    Chunks = lists:map(fun({Node, Keys}) -> rpc:call(Node, es3_chunk, list, [Keys]) end, NodesChunks),
    BinList = lists:map(fun({_, BinChunk}) -> BinChunk end, lists:flatten(Chunks)),
    binary:list_to_bin(BinList).

delete(Name) -> ok.

write(Name, Chunks) -> 
    Nodes = [node() | nodes()],
    NodesInfo = distribute_chunks(Nodes, Chunks),
    ok = es3_repo:write(Name, NodesInfo).


%% ------------------------------------------------
%% Internal Functions
%% ------------------------------------------------


distribute_chunks(Nodes, Chunks) ->
    GroupedChunks = lists:split(length(Chunks) div length(Nodes), Chunks),
    write_chunks(Nodes, GroupedChunks, 1, []).

write_chunks([], _, _, NodesChunks) -> NodesChunks;
write_chunks([Node|Rest], GroupedChunks, Idx, NodesChunks) ->
    NodeInfo = node_chunks(Node, element(Idx, GroupedChunks), []),
    write_chunks(Rest, GroupedChunks, Idx + 1, [NodeInfo | NodesChunks]).

node_chunks(Node, [], Keys)    -> {Node, Keys};
node_chunks(Node, [H|T], Keys) ->
    Key = gen_key(Node),
    rpc:cast(Node, es3_chunk, write, [Key, H]),
    node_chunks(Node, T, [Key|Keys]).

gen_key(Node) -> 
    atom_to_list(Node) ++ integer_to_list(timestamp()).

%% Function adapted from https://gist.github.com/DimitryDushkin/5532071
timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + Micro.
