%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc An abstraction to the diagnostic request queue. Basically what happens is:
%%
%%      1. Thru web API, systems publishes customers in the queue to be diagnosed
%%      2. cameron_dispatcher pop theses customers from the queue to make diagnostics

-module(cameron_messaging).
-author('Leandro Silva <leandrodoze@gmail.com>').

% public api
-export([publish_to/2, subscribe_to/1, acknowledge_delivery/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%
%% Types ------------------------------------------------------------------------------------------
%%
%%     QueueName = atom() = awaiting_for_diagnostic_queue
%%
%%     Payload = {request_for_diagnostic, Customer, From}
%%
%%     Customer = {customer, Id}
%%
%%         customer = atom()
%%         Id = {Type, Value}
%%
%%             Type = atom() = login | cpf | cnpj
%%             Value = string()
%%
%%     From = {from, CommunicationSystem}
%%
%%         from = atom()
%%         CommunicationSystem = string()
%%
%%     Channel = A RabbitMQ Channel
%%

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec publish_to(QueueName, Payload) -> ok
%% @doc Publish a customer to be diagnosted.
publish_to(awaiting_for_diagnostic_queue, {request_for_diagnostic, _Customer, _From} = Payload) ->
  Host = get_amqp_server_host(),

  {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  amqp_channel:call(Channel, #'queue.declare'{queue = <<"awaiting_for_diagnostic_queue">>, durable = true}),

  RawPayload = term_to_binary(Payload),
  
  amqp_channel:cast(Channel, #'basic.publish'{exchange = <<"">>, routing_key = <<"awaiting_for_diagnostic_queue">>},
                             #amqp_msg{props = #'P_basic'{delivery_mode = 2}, payload = RawPayload}),

  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

%% @spec subscribe_to(QueueName) -> Channel
%% @doc Subscribe to the diagnostic queue.
subscribe_to(awaiting_for_diagnostic_queue) ->
  Host = get_amqp_server_host(),
  
  {ok, Connection} = amqp_connection:start(#amqp_params_network{host = Host}),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  amqp_channel:call(Channel, #'queue.declare'{queue = <<"awaiting_for_diagnostic_queue">>, durable = true}),

  amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
  amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"awaiting_for_diagnostic_queue">>}, self()),
  
  receive
      #'basic.consume_ok'{} -> ok
  end,

  Channel.
      
%% @spec acknowledge_delivery(Channel, Tag) -> ok
%% @doc Acknowledge a received message.
acknowledge_delivery(Channel, Tag) ->
  amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
  ok.

%%
%% Internal API -----------------------------------------------------------------------------------
%%

get_amqp_server_host() ->
  [{host, Host}] = cameron:get_amqp_server_config(),
  Host.
  