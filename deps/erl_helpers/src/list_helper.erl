%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to list.

-module(list_helper).
-export([turn_into_properties/1]).

%% @spec turn_into_properties(List) -> PropertyList
%% @doc Turn a list in a property list.
turn_into_properties(Data) ->
  turn_into_properties(Data, []).

turn_into_properties([K, V | Tail], []) ->
  turn_into_properties(Tail, [{K, V}]);
  
turn_into_properties([K, V | Tail], Acc) ->
  turn_into_properties(Tail, [{K, V} | Acc]);
  
turn_into_properties([], Acc) ->
  Acc.
