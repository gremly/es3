%%%-------------------------------------------------------------------
%% @doc es3 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(es3_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupervisorFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs     = [#{id => es3_chunk, start => {es3_chunk, start_link, [es3_chunk]},
                        restart => permanent}],
    {ok, {SupervisorFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
