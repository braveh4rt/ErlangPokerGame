%%%-------------------------------------------------------------------
%%% @author mihai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2016 2:21 PM
%%%-------------------------------------------------------------------
-module(player).
-author("mihai").

-behaviour(gen_server).

%% API
-export([join/3, leave/1, set_cards/2, get_state/1,hand/1, tokens/1, current_bet/1, check/1, open/2, fold/1, call/1, raise/2]).
-export([stand/1, discard/2]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {id, tokens, current_bet = 0, gameId = "lobby", hand = []}).

%%%===================================================================
%%% API
%%%===================================================================

join(Id, Tokens, GameId) ->
  {ok, Pid} = start_link(Id, Tokens, GameId),
  game:joined(GameId, Pid),
  {ok, Pid}.

set_cards(Pid, Cards) ->
  gen_server:call(Pid, {set_cards, Cards}).

get_state(Pid) ->
  gen_server:call(Pid, get_state ).

leave(Pid) -> gen_server:call(Pid, terminate).

hand(Pid) ->
  gen_server:call(Pid, get_hand).

current_bet(Pid) ->
  gen_server:call(Pid, current_bet).

tokens(Pid) ->
  gen_server:call(Pid, get_tokens).

%% Betting
check(Pid) ->
  gen_server:call(Pid, check).

open(Pid, Amount) ->
  gen_server:call(Pid, {open, Amount}).

fold(Pid) ->
  gen_server:call(Pid, fold).

call(Pid) ->
  gen_server:call(Pid, call).

raise(Pid, Amount) ->
  gen_server:call(Pid, {raise, Amount}).

%% Discarding
stand(_) -> ok.
discard(Pid, Cards) -> gen_server:call(Pid, {discard, Cards}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

-spec(start_link(Id :: string(), Tokens :: integer(), GameId :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Id, Tokens, GameId) ->
  gen_server:start_link({global, Id}, ?MODULE, [Id, Tokens, GameId], []).

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
-spec(init(Args :: [ string() | integer()]) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Id, Tokens, GameId]) ->
  {ok, #state{id = Id, tokens = Tokens, gameId = GameId}}.

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

handle_call({set_cards, Hand}, _From, State) ->
%%  io:format("~p~n", [Hand]),
  {reply, ok, State#state{hand = Hand}};

handle_call(get_hand, _From, State) ->
  {reply, State#state.hand, State};

handle_call(current_bet, _From, State) ->
  {reply, State#state.current_bet, State};

handle_call(get_state, _From, State) ->
  {reply, State, State};

handle_call(get_tokens, _From, State) ->
  {reply, State#state.tokens, State};

handle_call(check, _From, State) ->
  {reply, ok, State};

handle_call({open, Amount}, _From, State) ->
  game:bet(State#state.gameId, Amount),
  {reply, ok, State#state{current_bet = Amount, tokens = State#state.tokens - Amount}};

handle_call(fold, _From, State) ->
  {reply, ok, State};

handle_call(call, _From, State) ->
  HighBet = game:high_bet(State#state.gameId),
  BetToMatch = HighBet - State#state.current_bet,

  game:bet(State#state.gameId, BetToMatch),

  {reply, ok, State#state{
    current_bet = State#state.current_bet + BetToMatch,
    tokens = State#state.tokens - BetToMatch
  }};

handle_call({raise, Amount}, _From, State) ->
  game:bet(State#state.gameId, Amount),
  {reply, ok, State#state{current_bet = Amount, tokens = State#state.tokens - Amount}};

handle_call({discard, Cards}, _From, State) ->

  HandDiscarded = sets:to_list(sets:subtract(
    sets:from_list(State#state.hand),
    sets:from_list(Cards)
  )),

  NewCards = game:draw(State#state.gameId, length(Cards)),

  {reply, ok, State#state{
    hand = HandDiscarded ++ NewCards
  }};

handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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
