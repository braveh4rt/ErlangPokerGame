%%%-------------------------------------------------------------------
%%% @author mihai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2016 4:14 PM
%%%-------------------------------------------------------------------
-module(game_tests).
-author("mihai").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

deal_test() ->
  {ok, Game} = game:start(),
  player:join("P1", 120, Game),
  player:join("P2", 120, Game),
  game:deal(Game),

  ?assertMatch( {state, "P1", _, _, _, [{_, _} | _]}, player:get_state("P1")),
  ?assertMatch( {state, "P2", _, _, _, [{_, _} | _]}, player:get_state("P2")),

  player:leave("P1"),
  player:leave("P2"),
  game:stop(Game).

rank_test() ->
  {ok, Game} = game:start(),
  player:join("P1", 120, Game),
  player:join("P2", 120, Game),

  player:set_cards("P1", [{2,d},{3,d},{4,d},{5,s},{6,d}]),
  player:set_cards("P2", [{2,d},{3,d},{4,d},{5,d},{6,d}]),

  ?assertEqual([["P2"],["P1"]], game:rank(Game)),

  player:set_cards("P1", [{2,d}, {2,d}, {4,c}, {4,d}, {4,s}]),
  player:set_cards("P2", [{3,c}, {3,d}, {3,s}, {9,s}, {9,d}]),

  ?assertEqual([["P1"],["P2"]], game:rank(Game)),

  player:join("P3", 130, Game),
  player:set_cards("P3", [{10, d}, {11, d}, {12, d}, {13, d}, {14, d}]),

  ?assertEqual([["P3"], ["P1"], ["P2"]], game:rank(Game)),
  ?assertEqual([["P3"], ["P1"], ["P2"]], game:rank(Game)),

  player:leave("P1"),
  player:leave("P2"),
  player:leave("P3"),
  game:stop(Game).

equal_hand_test() ->
  {ok, Game} = game:start(),
  player:join("P1", 120, Game),
  player:join("P2", 120, Game),

  player:set_cards("P1", [{2,s},{3,s},{4,s},{5,s},{6,s}]),
  player:set_cards("P2", [{2,d},{3,d},{4,d},{5,d},{6,d}]),

  ?assertEqual([["P2", "P1"]], game:rank(Game)),

  player:leave("P1"),
  player:leave("P2"),
  game:stop(Game).

game_test() ->
  {ok, Game} = game:start(),
  player:join("Mihai", 100, Game),
  player:join("Robert", 100, Game),
  player:join("Razvan", 100, Game),
  player:join("Ilie", 100, Game),
  player:join("Codrut", 100, Game),

  game:deal(Game),

  ?assertMatch( [_,_,_,_,_], game:rank(Game)),

  player:leave("Codrut"),
  player:leave("Ilie"),
  player:leave("Razvan"),
  player:leave("Robert"),
  player:leave("Mihai"),
  game:stop(Game).

betting_test() ->
  {ok, G} = game:start(),
  player:join("Mihai", 100, G),
  player:join("Robert", 100, G),
  player:join("Razvan", 100, G),
  player:join("Ilie", 100, G),
  player:join("Codrut", 100, G),
  game:deal(G),

  %% Round 1 betting
  player:check("Mihai"),
  player:open("Robert", 10),

  player:fold("Razvan"),
  player:call("Ilie"),
  player:raise("Codrut", 20),

  player:call("Mihai"),
  player:call("Robert"),

  player:call("Ilie"),

  ?assertEqual( 80, game:pot(G)),
  ?assertEqual( 20, game:high_bet(G)),

  ?assertEqual( 20, player:current_bet("Mihai")),
  ?assertEqual( 20, player:current_bet("Robert")),

  ?assertEqual( 20, player:current_bet("Ilie")),
  ?assertEqual( 20, player:current_bet("Codrut")),

  %% Discarding
  player:stand("Mihai"),

  [C11 | Rest] = player:hand("Robert"),
  player:discard("Robert", [C11]),
  ?assertNotEqual([C11 | Rest], player:hand("Robert")),

  [C21, C22 | _] = player:hand("Ilie"),
  player:discard("Ilie", [C21, C22]),

  [C31, C32, C33 | _] = player:hand("Codrut"),
  player:discard("Codrut", [C31, C32, C33]),

  %% Round 2 betting ignored
  ?assertMatch([_|_], game:rank(G)),

  player:leave("Codrut"),
  player:leave("Ilie"),
  player:leave("Razvan"),
  player:leave("Robert"),
  player:leave("Mihai"),
  game:stop(G).

stress_test() ->
  stress(100, 5, 10).

stress(GameNr, Deals, PlayerNr) ->

  PlayerNames = gen_names(PlayerNr * GameNr),
  Games = lists:map(
    fun (Idx) ->
      {ok, G} = game:start(),
      Players = lists:map(
        fun(T) -> lists:nth(PlayerNr * ( Idx - 1 ) + T, PlayerNames) end,
        lists:seq(1,PlayerNr)
      ),
      lists:foreach(
        fun(P) -> player:join(P, 100, G) end,
        Players
      ),
      {G, Players}
    end, lists:seq(1, GameNr)),

  lists:foreach(
    fun({G, _}) ->
      lists:foreach(
        fun(_) ->
          game:deal(G),
          ?assertMatch( [ _ | _ ], game:rank(G))
        end,
        lists:seq(1, Deals))
    end,
    Games
  ),

  lists:foreach(
    fun({_G, Players}) ->
      lists:foreach(fun(P) -> player:leave(P) end, Players),
      game:stop(_G)
    end,
    Games
  ).

gen_names(Number) ->
  lists:sublist(gen_names(Number, []), Number).

gen_names(Number, []) ->
  gen_names(Number - 25, lists:map( fun (N) -> [N] end, lists:seq(65,90)));

gen_names(Number, L) ->
  if (Number > 0) ->
      NL =  [ A ++ [B] || A <- L, B <- lists:seq(65,90)],
      gen_names(Number - length(NL), NL);
    true -> L
  end.
