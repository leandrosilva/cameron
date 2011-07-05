%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The worker gen_server, the senior doctor of this diagnostic system.

-module(cameron_doctor).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/0]).
% public api
-export([diagnostic_it/1, make_diagnostic_for/3]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Types ------------------------------------------------------------------------------------------
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

-record(state, {id = 0}).

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

%% @spec diagnostic_it(Payload) -> ok
%% @doc Make a complete diagnostic to a customer.
diagnostic_it({request_for_diagnostic, _Customer, _From} = Payload) ->
  gen_server:cast(?MODULE, {diagnostic_it, Payload}),
  ok.

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(_Options) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(_Options) ->
  {ok, #state{id = 1}}.

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

% make diagnostic
handle_cast({diagnostic_it, {request_for_diagnostic, Customer, _From} = Payload}, State) ->
  io:format("~n[cameron_doctor] ----------~n"),
  io:format("Payload: ~w~n", [Payload]),
  io:format("------------------------~n"),
  
  Id = State#state.id,
  
  spawn(?MODULE, make_diagnostic_for, [Id, Customer, "Cloud"]),
  spawn(?MODULE, make_diagnostic_for, [Id, Customer, "Hosting"]),
  spawn(?MODULE, make_diagnostic_for, [Id, Customer, "SQL Server"]),
  
  NewState = State#state{id = Id + 1},
  
  {noreply, NewState};

% manual shutdown
handle_cast(stop, State) ->
  {stop, normal, State};
    
% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @spec handle_info(Info, State) ->
%%                  {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @doc Handling all non call/cast messages.

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

make_diagnostic_for(Id, Customer, Product) ->
  io:format("[~w] Customer: ~w~n", [Id, Customer]),
  io:format("[~w] Product: ~w~n", [Id, Product]),
  
  ok.
  