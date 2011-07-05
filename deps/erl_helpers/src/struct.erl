%% @doc Utilities for working with mochijson2 struct.
%%
%% COPYRIGHT - stickyNotes sample application by beepbeep.

%%  struct example : 
%% 
%%  S = {struct, [  
%%      {<<"name">>, <<"Foo">>}, 
%%      {<<"activity">>, {struct, [
%%        {<<"name">>, <<"Basketball">>}
%%        {<<"duration">>, 60},
%%        {<<"intensity">>, 10}]}}]}
%% 
%%  get_value(<<"name">>, S)
%%  get_value({<<"activity">>, <<"duration">>}, S)
%%  set_value(<<"lastName">>, <<"Bar">>, S)
%%  set_value({<<"activity">>, <<"duration">>}, 75, S)
%%  delete(<<"name">>, S)
%%  delete({<<"activity">>, <<"duration">>}, S)  

-module(struct).

-export([extend/2, withdraw/2, get_value/2, set_value/3, delete/2]).

%% by codezone
-export([from_json/1, from_json/2, to_json/1]).
-export([new/2]).

%% @type key() = binary()
%% @type value() = [integer() | float() | atom() | tuple() | binary() | string() | list()]
%% @type struct() = tuple()
%% @type path() = tuple()
%% @type input() = [integer() | list()]

%% @spec extend(struct(), list()) -> struct()
%% @doc Extend a json struct with one or more json struct (add new leaves and modify the existing ones).
extend(S1, []) ->
  S1;

extend(S1, [S|T]) ->
  NewS = extend(S1, S),
  extend(NewS, T);

extend(S1, S2) ->
  {struct, L1} = S1,
  {struct, L2} = S2,
  ext(L1, L2, []).

ext(L1, [], Result) ->
  {struct, lists:append(Result,L1)};

ext(L1, [{K, {struct, ChildL2}} | T], Result) ->
  case proplists:get_value(K, L1) of
    {struct, ChildL1} ->
      NewL1 = proplists:delete(K, L1),
      ext(NewL1, T, [{K, extend({struct, ChildL1}, {struct, ChildL2})} | Result]);
    _ ->
      NewL1 = proplists:delete(K, L1),
       ext(NewL1, T, [{K, {struct, ChildL2}} | Result])
  end;

ext(L1, [{K, V} | T], Result) ->
  NewL1 = proplists:delete(K, L1),
  ext(NewL1, T, [{K,V} | Result]).


%% @spec withdraw(struct(), structlist()) -> struct()
%% @doc withdraw acts in the exact opposite way of extend (note : you just need to specify the keys).
withdraw(S1, []) ->
  S1;
  
withdraw(S1, [S|T]) ->
  NewS = withdraw(S1, S),
  withdraw(NewS, T);
  
withdraw(S1, S2) ->
  {struct, L1} = S1,
  {struct, L2} = S2,
  wdr(L1, L2, []).

wdr([], _L2, Result) ->
  {struct, Result};

wdr([{K, {struct, ChildL1}} | T], L2, Result) ->
  case proplists:get_value(K, L2) of
    {struct, ChildL2} ->
      wdr(T, L2, [{K, withdraw({struct, ChildL1}, {struct, ChildL2})} | Result]);
    _ ->
      case proplists:is_defined(K, L2) of 
        false ->
          wdr(T, L2, [{K, {struct, ChildL1}} | Result]);
        true ->
          wdr(T, L2, Result)
      end
    end;

wdr([{K, V} | T], L2, Result) ->
  case proplists:is_defined(K, L2) of
    false ->
      wdr(T, L2, [ {K, V} | Result]);
    true ->
      wdr(T, L2, Result)
    end.


%% @spec get_value(path() | key(), struct()) -> value()
get_value(Path, Struct) when is_tuple(Path) ->
  L = tuple_to_list(Path),
  get_val(L, Struct);
  
get_value(Key, Struct) ->
  {struct, L} = Struct,
  proplists:get_value(Key, L).

get_val(_, undefined) ->
  undefined;
  
get_val([Key], Struct) ->
  get_value(Key, Struct);
  
get_val([Key | T], Struct) ->
  NewStruct = get_value(Key, Struct),
  get_val(T, NewStruct).


%% @spec set_value(path() | key(), value(), struct()) -> struct()
set_value(Path, Value, Struct) when is_tuple(Path) ->
  [H | T] = lists:reverse(tuple_to_list(Path)),
  set_val(T, Struct, {struct, [{H, Value}]});
  
set_value(Key, Value, Struct) ->
  extend(Struct, {struct, [{Key, Value}]}).

set_val([], Struct, Result) ->
  extend(Struct, Result);
  
set_val([Key | T], Struct, Result) ->
  set_val(T, Struct, {struct, [{Key, Result}]}).


%% @spec delete(path() | key(), struct()) -> value()
delete(Path, Struct) when is_tuple(Path) ->
  [H | T] = lists:reverse(tuple_to_list(Path)),
  del(T, Struct, {struct, [{H}]});
  
delete(Key, Struct) ->
  {struct, L} = Struct,
  {struct, proplists:delete(Key, L)}.

del([], Struct, Result) ->
  withdraw(Struct, Result);
  
del([Key | T ], Struct, Result) ->
  del(T, Struct, {struct, [{Key, Result}]}).
  

%% @spec from_json(input()) -> struct()
%% @doc by codezone, using mochijson2:decode/1
from_json(JsonInput) ->
  mochijson2:decode(JsonInput).


%% @spec from_json(key(), input()) -> struct()
%% @doc by codezone, using mochijson2:decode/1
from_json(Key, Input) ->
  JsonInput = proplists:get_value(Key, Input),
  mochijson2:decode(JsonInput).
  
  
%% @spec to_json(struct()) -> list()
%% @doc by codezone, using mochijson2:encode/1
to_json(Struct) ->
  mochijson2:encode(Struct).


%% @spec new(key(), value()) -> struct()
%% @doc by codezone
new(Key, Value) when not is_binary(Value) ->
  {struct, [{list_to_binary(Key), list_to_binary(Value)}]};

new(Key, Value) ->
  {struct, [{list_to_binary(Key), Value}]}.
