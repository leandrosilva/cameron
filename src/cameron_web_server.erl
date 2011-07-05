%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc The misultin-based web server module of the Cameron application that handles HTTP requests
%%      and WebSocket.

-module(cameron_web_server).
-author('Leandro Silva <leandrodoze@gmail.com>').

-behaviour(gen_server).

% admin api
-export([start_link/1, stop/0]).
% public api
-export([get_host/0, get_port/0, get_backlog/0, get_docroot/0]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% Types ------------------------------------------------------------------------------------------
%%
%%     WebConfig = [{host, Host}, {port, Port}, {backlog, Backlog}, {docroot, DocRoot}]
%%
%%         Host = string()
%%         Port = int()
%%         Backlog = int()
%%         DocRoot = string()
%%

-record(state, {host, port, backlog, docroot}).

%%
%% Admin API --------------------------------------------------------------------------------------
%%

%% @spec start_link(WebConfig) -> {ok, Pid} | ignore | {error, Error}
%% @doc Start misultin HTTP server.
start_link(WebConfig) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, WebConfig, []).

%% @spec stop() -> ok
%% @doc Manually stops the server.
stop() ->
  gen_server:cast(?MODULE, stop).

%%
%% Public API -------------------------------------------------------------------------------------
%%

%% @spec get_host() -> string()
%% @doc Host property from configuration properties.
get_host() ->
  gen_server:call(?MODULE, {get, host}).

%% @spec get_port() -> int()
%% @doc Port property from configuration properties.
get_port() ->
  gen_server:call(?MODULE, {get, port}).

%% @spec get_backlog() -> int()
%% @doc Backlog property from configuration properties.
get_backlog() ->
  gen_server:call(?MODULE, {get, backlog}).

%% @spec get_docroot() -> string()
%% @doc DocRoot property from configuration properties.
get_docroot() ->
  gen_server:call(?MODULE, {get, docroot}).

%%
%% Gen_Server Callbacks ---------------------------------------------------------------------------
%%

%% @spec init(WebConfig) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @doc Initiates the server.
init(WebConfig) ->
  process_flag(trap_exit, true),

  [{host, Host}, {port, Port}, {backlog, Backlog}, {docroot, DocRoot}] = WebConfig,
  
  misultin:start_link([{ip, Host},
                       {port, Port},
                       {backlog, Backlog},
                       {loop, fun(Req) -> handle_http(Req) end},
                       {ws_loop, fun(Ws) -> handle_websocket(Ws) end},
                       {ws_autoexit, false}]),
                       
  erlang:monitor(process, misultin),
  
  {ok, #state{host = Host, port = Port, backlog = Backlog, docroot = DocRoot}}.

%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} | {reply, Reply, State, Timeout} | {noreply, State} |
%%                  {noreply, State, Timeout} | {stop, Reason, Reply, State} | {stop, Reason, State}
%% @doc Handling call messages.

% return host property
handle_call({get, host}, _From, State) ->
  {reply, State#state.host, State};

% return port property
handle_call({get, port}, _From, State) ->
  {reply, State#state.port, State};

% return backlog property
handle_call({get, backlog}, _From, State) ->
  {reply, State#state.backlog, State};

% return docroot property
handle_call({get, docroot}, _From, State) ->
  {reply, State#state.docroot, State};

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

% handle info when misultin server goes down -> take down misultin_gen_server too [the supervisor
% will take everything up again]
handle_info({'DOWN', _Reference, process, {misultin, _}, _Reason}, State) ->
  {stop, normal, State};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to terminate. When it returns,
%%      the gen_server terminates with Reason. The return value is ignored.
terminate(_Reason, _State) ->
  misultin:stop(),
  terminated.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%
%% Misultin Callbacks -----------------------------------------------------------------------------
%%

% callback on HTTP request received
handle_http(Req) ->
  Method = Req:get(method),
  Resource = Req:resource([lowercase, urldecode]),

  cameron_web_api:handle_http(Method, Resource, Req).

% callback on received websockets data
handle_websocket(Ws) ->
  Path = string:tokens(Ws:get(path), "/"),
  
  cameron_web_api:handle_websocket(Path, Ws).

%%
%% Internal API -----------------------------------------------------------------------------------
%%
