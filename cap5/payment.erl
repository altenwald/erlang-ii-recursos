-module(payment).
-author('manuel@altenwald.com').

-behaviour(gen_statem).

-export([
    start_link/0,
    stop/1,
    give_name/2,
    give_payment_method/2,
    give_card/2,
    give_account/2,
    get_info/1
]).

-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3,
    code_change/4
]).

-type payment_method() :: card | debit.

-record(state, {
        name :: string(),
        payment_method :: payment_method(),
        card :: string(),
        account :: string()
}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

stop(Name) ->
    gen_statem:stop(Name).

give_name(PID, Name) ->
    gen_statem:call(PID, {name, Name}).

give_payment_method(PID, PaymentMethod) ->
    gen_statem:call(PID, {payment_method, PaymentMethod}).

give_card(PID, Card) ->
    gen_statem:call(PID, {card, Card}).

give_account(PID, Account) ->
    gen_statem:call(PID, {account, Account}).

get_info(PID) ->
    gen_statem:call(PID, info).

callback_mode() ->
    handle_event_function.

init([]) ->
    {ok, credentials, #state{}}.

handle_event({call, From}, info, StateName, StateData) ->
    {keep_state_and_data, [{reply, From, {StateName, StateData}}]};

handle_event({call, From}, {name, Name}, credentials, State) ->
    {next_state, payment_method, State#state{name = Name}, [{reply, From, ok}]};
handle_event({call, From}, _Event, credentials, _State) ->
    {keep_state_and_data, [{reply, From, {error, "name required!"}}]};

handle_event({call, From}, {payment_method, card}, payment_method, State) ->
    {next_state, card_payment, State#state{payment_method = card},
     [{reply, From, ok}]};
handle_event({call, From}, {payment_method, debit}, payment_method, State) ->
    {next_state, debit_payment, State#state{payment_method = debit},
     [{reply, From, ok}]};
handle_event({call, From}, _Event, payment_method, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "payment method: card or debit"}}]};

handle_event({call, From}, {card, Card}, card_payment, State) ->
    {next_state, paid, State#state{card = Card},
     [{reply, From, {ok, paid}}, hibernate]};
handle_event({call, From}, _Event, card_payment, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "card required"}}]};

handle_event({call, From}, {account, Account}, debit_payment, State) ->
    {next_state, paid, State#state{account = Account},
     [{reply, From, {ok, paid}}, hibernate]};
handle_event({call, From}, _Event, debit_payment, _State) ->
    {keep_state_and_data,
     [{reply, From, {error, "account required"}}]};

handle_event({call, From}, _Event, paid, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_paid}}, hibernate]}.

terminate(normal, paid, #state{payment_method = card} = State) ->
    io:format("~p Name: ~s~nCard: ~s~nPaid.~n",
              [self(), State#state.name, State#state.card]),
    ok;
terminate(normal, paid, #state{payment_method = debit} = State) ->
    io:format("~p Name: ~s~nAccount: ~s~nPaid.~n",
              [self(), State#state.name, State#state.account]),
    ok;
terminate(normal, _StateName, _StateData) ->
    io:format("~p No paid.~n", [self()]),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
