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
    case es3_repo:read(Name) of
        [] -> {error, file_not_found};
        [{_, Name, NodesChunks}] ->
            %Chunks = lists:map(fun({Node, Keys}) -> rpc:call(Node, es3_chunk, list, [Keys]) end, NodesChunks),
            ChunksMap = lists:foldl(fun({Node, Keys}, Acc) ->
                                         Nodes = [Node | maps:get(nodes, Acc)],
                                         KeysList  = maps:get(keys, Acc) ++ Keys,
                                         Map   = maps:put(nodes, Nodes, Acc),
                                         maps:put(keys, KeysList, Map)
                                 end, #{nodes => [], keys => []}, NodesChunks),
            {Res, _} = rpc:multicall(maps:get(nodes, ChunksMap), es3_chunk, list, [maps:get(keys, ChunksMap)]),
            SortedList = lists:sort(fun({KeyA, _}, {KeyB, _}) -> KeyA > KeyB end, lists:flatten(Res)),
            BinList = lists:map(fun({_, BinChunk}) -> BinChunk end, SortedList),
            binary:list_to_bin(BinList)
    end.

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
    NodeInfo = node_chunks(Node, element(Idx, GroupedChunks), Idx, []),
    write_chunks(Rest, GroupedChunks, Idx + 1, [NodeInfo | NodesChunks]).

node_chunks(Node, [], _, Keys)    -> {Node, Keys};
node_chunks(Node, [H|T], Idx, Keys) ->
    Key = gen_key(Idx),
    rpc:cast(Node, es3_chunk, write, [Key, H]),
    node_chunks(Node, T, Idx, [Key|Keys]).

gen_key(Prefix) ->
    integer_to_list(Prefix) ++ integer_to_list(timestamp()).

%% Function adapted from https://gist.github.com/DimitryDushkin/5532071
timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + Micro.
