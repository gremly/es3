%%% @doc This module isolates the management of the database
%%%      where all files metadata is persisted.
-module(es3_repo).

-export([init/0, ensure_loaded/0, read/1, delete/1, write/2]).

-record(meta_files, {name, nodes}).

%% Public API

%% Creating and initializing schema

init() ->
    Nodes = [node() | nodes()],
    mnesia:stop(),
    mnesia:change_config(extra_db_nodes, Nodes),
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(meta_files, [{disc_copies, Nodes}, {type, set},
                                    {attributes, record_info(fields, meta_files)}]).

ensure_loaded() ->
    ok = mnesia:wait_for_tables([meta_files], 10000).

%% Data management API

-spec write(FileName :: any(), Nodes :: list()) -> any() | {error, Reason :: any()}.
write(FileName, Nodes) ->
    Metadata = #meta_files{name=FileName, nodes=Nodes},
    Fun = fun() -> mnesia:write(Metadata) end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

-spec read(FileName :: any()) -> list() | {error, Reason :: any()}.
read(FileName) ->
    Fun = fun() -> mnesia:read({meta_files, FileName}) end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

-spec delete(FileName :: any()) -> ok | {error, Reason :: any()}.
delete(FileName) ->
    Fun = fun() -> mnesia:delete({meta_files, FileName}) end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
