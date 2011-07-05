%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2011 Leandro Silva.

%% @doc Helper module to test roomerl_web's websocket.
%%      Thanks to Dave Bryson - https://github.com/davebryson/erlang_websocket

-module(websocket_client_impl).
-behaviour(websocket_client).

-export([start/3]).
-export([onmessage/1, onopen/0, onclose/0, close/0, send/1]).
-export([logger/1, get_log/0]).

start(Ip, Port, Path) ->
  websocket_client:start(Ip, Port, Path, ?MODULE),
  register(websocket_client_logger, spawn(?MODULE, logger, [[]])),
  started.

send(Data) ->
  websocket_client:write(Data),
  add_log(send, Data),
  {sent, Data}.

onmessage(Data) ->
  add_log(onmessage, Data).

onclose() ->
  add_log(onclose).

onopen() ->
  add_log(onopen).

close() ->
  websocket_client:close(),
  add_log(close),
  closed.

% --- websocket_client_logger process -------------------------------------------------------------

logger(State) ->
  receive
    {add, {What, Data}} ->
      NewState = [{What, Data} | State],
      logger(NewState);
    {get, Sender} ->
      Sender ! {log, lists:reverse(State)},
      logger(State)
  end.

add_log(What) ->
  add_log(What, ok).
  
add_log(What, Data) ->
  websocket_client_logger ! {add, {What, Data}},
  ok.
  
get_log() ->
  websocket_client_logger ! {get, self()},
  
  receive
    {log, State} ->
      State;
    _ ->
      error
  end.
  