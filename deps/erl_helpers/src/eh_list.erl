%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to list.

-module(eh_list).
-export([to_properties/1]).

%% @spec to_properties(List) -> PropertyList
%% @doc Turn a list in a property list.
to_properties(Data) ->
  to_properties(Data, []).

to_properties([K, V | Tail], []) ->
  to_properties(Tail, [{K, V}]);
  
to_properties([K, V | Tail], Acc) ->
  to_properties(Tail, [{K, V} | Acc]);
  
to_properties([], Acc) ->
  Acc.
