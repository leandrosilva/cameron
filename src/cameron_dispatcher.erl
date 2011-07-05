%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The main gen_server, the "ear" of this diagnostic system which subscribes to "awaiting
%%      queue" in order to be notifyed of every "request for diagnostic" and dispatch it to
%%      cameron_worker gen_server.

-module(cameron_dispatcher).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/0]).
% public api
-export([dispatch/1]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

%%
%% Types ------------------------------------------------------------------------------------------
%%
%%     Payload = {request_for_diagnostic, Customer, From}
%%
%%     Channel = A RabbitMQ Channel
%%

-record(state, {messaging_channel}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(_Options) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start cameron server.
start_link(_Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec diagnostic(Payload) -> ok
%% @doc Async dispatch of a resquest for diagnostic.
dispatch({request_for_diagnostic, _Customer, _From} = Payload) ->
  cameron_messaging:publish_to(awaiting_for_diagnostic_queue, Payload).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(_Options) ->
  Channel = cameron_messaging:subscribe_to(awaiting_for_diagnostic_queue),
  
  {ok, #state{messaging_channel = Channel}}.

%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% handle_call generic fallback
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

%% @spec handle_cast(Msg, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling cast messages.

% manual shutdown
handle_cast(stop, State) ->
  {stop, normal, State};

% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @spec handle_info(Info, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling all non call/cast messages.

% receive messages from awaiting_for_diagnostic_queue
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = RawPayload}}, State) ->
  Payload = binary_to_term(RawPayload),
  
  io:format("~n[cameron_dispatcher] ------------~n"),
  io:format("Payload: ~w~n", [Payload]),
  io:format("Tag: ~w~n", [Tag]),
  io:format("------------------------~n"),
  
  ok = cameron_worker:diagnostic(Payload),
  
  cameron_messaging:acknowledge_delivery(State#state.messaging_channel, Tag),
  
  {noreply, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.
terminate(_Reason, _State) ->
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Internal API -----------------------------------------------------------------------------------
%%
