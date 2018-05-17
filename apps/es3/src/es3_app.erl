%%%-------------------------------------------------------------------
%% @doc es3 public API
%% @end
%%%-------------------------------------------------------------------

-module(es3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Nodes = [node_a@gror, node_b@gror, node_c@gror],
    lists:map(fun net_kernel:connect/1, Nodes),
    es3_repo:init(),
    es3_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
