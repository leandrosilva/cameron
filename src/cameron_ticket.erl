%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction for tickets. It's used to keep track in a database every state (steps?) of a
%%      ticket that refers to a request for diagnost. And in this case, it stores those steps in a
%%      Redis server.

-module(cameron_ticket).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([create/1, pop/0, save_result/1, close/1, uuid/0]).

%%
%% Includes, Defines, and Records -----------------------------------------------------------------
%%

-include("include/cameron.hrl").

-define(QUEUE_INCOMING, "cameron:queues:incoming").
-define(QUEUE_DONE,     "cameron:queues:done").

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec create(Payload) -> {ok, Ticket} | {error, Reason}
%% @doc This first step of the whole process is create a ticket which allow keep track of diagnostic
%%      state and get any related information at any time in the future.
create(#diagnostic_request{customer_id = CustomerId, from_id = FromId} = Payload) ->
  io:format("--- [cameron_ticket] create a ticket // Payload: ~w~n", [Payload]),
  
  {ok, Ticket} = get_diagnostic_ticket(CustomerId),

  redis(["lpush", ?QUEUE_INCOMING, Ticket]),
  ok = redis(["hmset", Ticket, "step", "incoming", "from_id", FromId]),
  
  {ok, business_ticket_uuid(Ticket)}.

%% @spec pop() -> {ok, Ticket} | {error, Reason}
%% @doc It rpop a ticket (of a request for diagnostic) from the incoming queue at Redis.
pop() ->
  io:format("--- [cameron_ticket] pop a ticket~n"),

  Ticket = redis(["rpop", ?QUEUE_INCOMING]),
  0 = redis(["hset", Ticket, "step", "dispatching"]),

  BusinessTicketUUID = business_ticket_uuid(Ticket),

  {ok, BusinessTicketUUID}.

%% @spec save_result(Result) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a diagnostic test result.
save_result(#diagnostic_result{ticket = Ticket, product_id = ProductId, result = Result}) ->
  io:format("--- [cameron_ticket] save a result~n"),

  RedisTicketUUID = redis_ticket_uuid(Ticket),

  1 = redis(["hset", RedisTicketUUID, ProductId, Result]),

  {ok, Ticket}.

%% @spec close(Ticket) -> {ok, Ticket} | {error, Reason}
%% @doc Save to Redis a step close.
close(Ticket) ->
  io:format("--- [cameron_ticket] close a ticket~n"),

  RedisTicketUUID = redis_ticket_uuid(Ticket),

  0 = redis(["hset", RedisTicketUUID, "step", "done"]),
  redis(["lpush", ?QUEUE_DONE, RedisTicketUUID]),

  {ok, Ticket}.

%% @spec uuid() -> Integer as String
%% @doc Get a incr value from Redis.
uuid() ->
  integer_to_list(redis(["incr", "cameron:uuid"])).

%% @step business_ticket_uuid(LongUUID) -> ShortUUID
%% @doc Extract just "{CustomerId}:{RequestId}" from the complete ticket UUID.
%%      That is, without "cameron:diagnostics:ticket:".
business_ticket_uuid(LongUUID) ->
  string:sub_string(LongUUID, 28).

%% @step redis_ticket_uuid(ShortUUID) -> LongUUID
%% @doc Add "cameron:diagnostics:ticket:" to {CustomerId}:{RequestId}.
%%      That is, "cameron:diagnostics:ticket:{CustomerId}:{RequestId}".
redis_ticket_uuid(ShortUUID) ->
  "cameron:diagnostics:ticket:" ++ ShortUUID.
  
%%
%% Internal Functions -----------------------------------------------------------------------------
%%

redis(Command) ->
  Output = redo:cmd(cameron_redo, Command),
  maybe_ok(maybe_string(Output)).

get_diagnostic_ticket(CustomerId) ->
  {Year, Month, Day} = date(),
  {Hour, Minute, Second} = time(),
  
  Ticket = lists:concat(["cameron:diagnostics:ticket:", CustomerId, ":",
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
  