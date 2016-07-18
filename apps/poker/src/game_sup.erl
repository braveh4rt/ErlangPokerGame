%%%-------------------------------------------------------------------
%% @doc poker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_sup).

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
  {ok, { {simple_one_for_one, 2, 5},
    [{poker_game,
      {game, start, []},
      permanent, 1000, worker, []
    }]}}.

%%====================================================================
%% Internal functions
%%====================================================================
