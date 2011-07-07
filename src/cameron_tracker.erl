%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstractiom used to keep track in a database every state (which step?) of a diagnost
%%      request. In this case, it stores those step in a Redis server. It's possible also to get
%%      stored data.

-module(cameron_tracker).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([step/1]).

%%
%% Includes, Defines, and Records -----------------------------------------------------------------
%%

-include("include/cameron.hrl").

-define(QUEUE_INCOMING, "cameron:queues:incoming").
-define(QUEUE_DONE,     "cameron:queues:done").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec step({incoming_request, Payload}) -> {ok, Ticket} | {error, Reason}
%% @doc This first step kick-off the whole process and so it returns a king of ticket which allow
%%      to get any related information in the future.
step({incoming_request, #diagnostic_request{customer_id = CustomerId, from_id = FromId} = Payload}) ->
  io:format("--- [cameron_tracker] incoming_request :: Payload: ~w~n", [Payload]),
  
  {ok, Ticket} = get_diagnostic_ticket(CustomerId),

  1 = redis(["lpush", ?QUEUE_INCOMING, Ticket]),
  ok = redis(["hmset", Ticket, "step", "incoming", "from_id", FromId]),
  
  {ok, Ticket};

%% @spec step(dispatching_request) -> {ok, Ticket} | {error, Reason}
%% @doc It rpop a ticket (of a request for diagnostic) from the incoming queue at Redis.
step(dispatching_request) ->
  io:format("--- [cameron_tracker] dispatching_request~n"),

  Ticket = redis(["rpop", ?QUEUE_INCOMING]),
  0 = redis(["hset", Ticket, "step", "dispatching"]),

  {ok, Ticket};

%% @spec step(Step) -> ok
%% @doc Fallback.
step(Step) ->
  io:format("--- [cameron_tracker] Undefined step: ~w~n", [Step]),
  ok.

%%
%% Internal Functions -----------------------------------------------------------------------------
%%

redis(Command) ->
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

get_diagnostic_ticket(CustomerId) ->
  {Year, Month, Day} = date(),
  {Hour, Minute, Second} = time(),
  
  Ticket = lists:concat(["cameron:diagnosts:ticket:", CustomerId, ":",
                         maybe_padding(Year), maybe_padding(Month), maybe_padding(Day),
                         maybe_padding(Hour), maybe_padding(Minute), maybe_padding(Second)]),
  
  {ok, Ticket}.
  
maybe_padding(Number) when is_integer(Number) and (Number < 10) ->
  "0" ++ integer_to_list(Number);
maybe_padding(Number) ->
  integer_to_list(Number).
  
maybe_string([]) ->
  [];
maybe_string([Binary | _Tail] = BinaryList) when is_binary(Binary) ->
  maybe_string_(BinaryList, []);
maybe_string(Single) when is_binary(Single) ->
  binary_to_list(Single);
maybe_string(Single) when is_integer(Single) ->
  Single.

maybe_string_([], StringList) ->
  lists:reverse(StringList);
maybe_string_([Binary | Tail], StringList) when is_binary(Binary) ->
  String = binary_to_list(Binary),
  maybe_string_(Tail, [String | StringList]).

maybe_ok("OK") ->
  ok;
maybe_ok(Other) ->
  Other.
  