%%%-------------------------------------------------------------------
%%% @author mihai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2016 2:21 PM
%%%-------------------------------------------------------------------
-module(game).
-author("mihai").

-behaviour(gen_server).

%% API
-export([random_hand/1, full_deck/0, group_by/2]).
-export([
  start/1,
  stop/1,
  rank/1,
  deal/1,
  joined/2,
  bet/2,
  high_bet/1,
  pot/1,
  draw/2,
  get_players/1
]).
-export([start_link/1]).

%% Debug
-export([hand_value/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pot = 0, high_bet = 0, deck = full_deck(), players = []}).

%%%===================================================================
%%% API
%%%===================================================================

start(Ref) ->
  start_link(Ref).

stop(Id) -> gen_server:call({global, Id}, terminate ).

deal(GameId) -> gen_server:call({global, GameId}, deal).

rank(GameId) -> gen_server:call({global, GameId}, rank).

bet(GameId, Amount) ->
  gen_server:call({global, GameId}, {bet, Amount}).

joined(GameId, PlayerId) ->
  gen_server:call({global, GameId}, {joined, PlayerId}).

get_players(GameId) -> gen_server:call({global, GameId}, get_players).

high_bet(GameId) -> gen_server:call({global, GameId}, high_bet).

pot(GameId) -> gen_server:call({global, GameId}, pot).

draw(GameId, Num) -> gen_server:call({global, GameId}, {draw, Num}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

-spec(start_link(Id :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Id) ->
  gen_server:start_link({global, Id}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(rank, _From, State) ->

  PlayersOrdered = lists:map(
    fun(RankedPlayers) ->
      lists:map( fun ({P, _}) -> P end, RankedPlayers) end,
      group_by(
        fun({_,V}) -> V end,
        lists:sort(
          fun({_,V1}, {_, V2}) -> V1 > V2 end,
          lists:map(
            fun(P) -> {P, hand_value(player:hand(P))} end,
            State#state.players )))),

  {reply, PlayersOrdered, State#state{deck = full_deck()}};

handle_call(deal, _From, State) ->
  {Hands, RemainingDeck} = lists:foldl(
    fun (_, {Hands, Deck}) ->
      {H,D} = random_hand(Deck),
      {Hands ++ [H], D}
    end,
    {[], State#state.deck},
    State#state.players
  ),

  lists:foreach(
    fun({P, H}) -> player:set_cards(P, H) end,
    lists:zip(State#state.players, Hands)),

  {reply, ok, State#state{deck = RemainingDeck}};

handle_call({bet, Amount}, _From, State) ->
  HighBet = case Amount > State#state.high_bet of
    true -> Amount;
    false -> State#state.high_bet
  end,
  {reply, ok, State#state{
    high_bet = HighBet,
    pot = State#state.pot + Amount
  }};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(high_bet, _From, State) ->
  {reply, State#state.high_bet, State};

handle_call(pot, _From, State) ->
  {reply, State#state.pot, State};

handle_call({draw, Num}, _, State) ->
  Deck = State#state.deck,
  {Cards, D} = random_cards(Num, Deck),
  {reply, Cards, State#state{deck = D}};

handle_call(get_players, _From, State) ->
  {reply, State#state.players, State};

handle_call({joined, PlayerId}, _From, State) ->
  {reply, ok, State#state{players = State#state.players ++ [PlayerId]}};

handle_call(_Req, _From, State) ->
  {reply, State, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).



handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  io:format("~p~n", [State]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hand_nums(Hand) ->
  lists:sort(lists:map( fun ({N, _}) -> N end, Hand)).

max_card(Hand) ->
  hd(lists:reverse(hand_nums(Hand))).

isFlush([{_,S} | RestCards]) ->
  lists:all(fun({_,S1}) -> S =:= S1 end, RestCards).

isStraight(Hand) ->
  Nums = hand_nums(Hand),
  Nums2 = lists:sort(lists:map(
    fun(N) ->
      case N =:= 14 of
        true -> 1;
        false -> N
      end
    end,
    Nums
  )),
  IsStraight = fun ([H|T]) ->
    lists:seq(H, H + length(T)) =:= [H | T]
               end,

  IsStraight(Nums) or IsStraight(Nums2).

isStraightFlush(Hand) -> isStraight(Hand) and isFlush(Hand).

isRoyalFlush(Hand) ->
  {N,_} = hd(lists:sort(Hand)),
  isStraightFlush(Hand) and (N =:= 10).

pairs([]) -> [];
pairs([C]) -> [[C]];
pairs([{N,_S} | Rest]) ->
  Hand = [{N,_S} | Rest],
  {G1, G2} = lists:partition(fun({N1,_S1}) -> N1 =:= N end, Hand ),
  lists:sort([G1| pairs(G2)]).

groups(Hand) ->
  lists:reverse(lists:sort(lists:map(fun(P) -> {length(P), P} end, pairs(Hand)))).

hand_value(Hand) ->

  HandKind = case groups(Hand) of
               [{4, [{N, _} | _]}, {1, [{N2, _}]}] -> {four_of_a_kind, {N, N2}};
               [{3, [{N, _} | _]}, {2, [{N2,_} | _]}] -> {full_house, {N, N2}};
               [{3, [{N, _} | _]}, {1, [{N2,_}]}, {1, [{N3,_} ]}] -> {three_of_a_kind, {N, N2, N3}};
               [{2, [{N1, _}| _]}, {2, [{N2, _} | _]}, {1, [{N3, _}]}] -> {two_pairs, {N1, N2, N3}};
               [{2, [{N1, _} | _]}, {1, [{N2, _} | _]}, {1, [{N3, _} | _]}, {1, [{N4, _} | _]}] -> {one_pair, {N1, N2, N3, N4}};
               _ -> {nothing, 0}
             end,

  {RoyalFlush, StraightFlush, FourOAK, FullHouse, Flush, Straight, ThreeOAK, TwoPairs, OnePair} =
    { isRoyalFlush(Hand),
      isStraightFlush(Hand),
      element(1, HandKind) =:= four_of_a_kind,
      element(1, HandKind) =:= full_house,
      isFlush(Hand),
      isStraight(Hand),
      element(1, HandKind) =:= three_of_a_kind,
      element(1, HandKind) =:= two_pairs,
      element(1, HandKind) =:= one_pair
    },

  if RoyalFlush ->      [10];
    StraightFlush ->   [9, hand_nums(Hand)];
    FourOAK ->
      {four_of_a_kind, {NN, NN2}} = HandKind,
      [8, NN, NN2];
    FullHouse ->
      {full_house, {NN, NN2}} = HandKind,
      [7, NN, NN2];
    Flush ->           [6 | lists:reverse(hand_nums(Hand))];
    Straight ->        [5, max_card(Hand)];
    ThreeOAK ->
      {three_of_a_kind, {NN, NN2, NN3}} = HandKind,
      [4, NN] ++ lists:reverse(lists:sort([NN2, NN3]));
    TwoPairs ->
      {two_pairs, {NN1, NN2, NN3}} = HandKind,
      [3] ++ lists:reverse(lists:sort([NN1, NN2])) ++ [NN3];
    OnePair ->
      {one_pair, {NN1, NN2, NN3, NN4}} = HandKind,
      [2, NN1] ++ lists:reverse(lists:sort([NN2, NN3, NN4]));

    true ->            [1] ++ lists:reverse(hand_nums(Hand))
  end.

full_deck() ->
  [ {Cn, Cs} || Cn <- lists:seq(2, 14), Cs <- [s,d,c,h]].

random_hand(Deck) ->
  random_cards(5, Deck).

random_cards(Number, Deck) ->
  lists:foldl(
    fun (_, {Hand, D}) ->
      {C, Rd} = random_card(D),
      {Hand ++ [C], Rd}
    end,
    {[], Deck},
    lists:seq(1,Number)
  ).

random_card(Deck) ->
  Index = rand:uniform(length(Deck)),
  {FirstHalf, [Card | SecondHalf]} = lists:split(Index - 1, Deck),
  {Card, FirstHalf ++ SecondHalf}.

group_by(_, []) -> [];
group_by(_, [E]) -> [[E]];
group_by(F, [E|R]) ->
  {L1, L2} = lists:splitwith(fun(V) -> F(E) =:= F(V) end, [E|R]),
  [L1 | group_by(F, L2)].