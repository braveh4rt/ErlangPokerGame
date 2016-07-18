%%%-------------------------------------------------------------------
%% @doc poker public API
%% @end
%%%-------------------------------------------------------------------

-module(poker).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([new_game/0, player_join/3]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _Args) ->
  game_sup:start_link(),
  player_sup:start_link().

new_game() ->
  G = make_ref(),
  supervisor:start_child(game_sup, [G]),
  {ok, G}.

player_join(PlayerId, Tokens, GameId) ->
  supervisor:start_child(player_sup, [PlayerId, Tokens, GameId]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
