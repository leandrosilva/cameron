%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to leverage kind of shy "maybe monad".

-module(maybe_helper).
-export([maybe_string/1, maybe_padding/1, maybe_ok/1, maybe_undefined/1]).
-export([maybe_binary/1, maybe_integer/1]).

% --- string --------------------------------------------------------------------------------------

maybe_string(undefined) ->
  undefined;

maybe_string([]) ->
  [];
  
maybe_string([Binary | _Tail] = BinaryList) when is_binary(Binary) ->
  maybe_string_(BinaryList, []);

maybe_string(Single) when is_list(Single) ->
  Single;
  
maybe_string(Single) when is_binary(Single) ->
  binary_to_list(Single);
  
maybe_string(Single) when is_integer(Single) ->
  Single;
  
maybe_string(Single) when is_atom(Single) ->
  atom_to_list(Single).

maybe_string_([], StringList) ->
  lists:reverse(StringList);

maybe_string_([Binary | Tail], StringList) when is_binary(Binary) ->
  String = binary_to_list(Binary),
  maybe_string_(Tail, [String | StringList]).

% --- padding -------------------------------------------------------------------------------------

maybe_padding(Number) when is_integer(Number) and (Number < 10) ->
  "0" ++ integer_to_list(Number);

maybe_padding(Number) when is_integer(Number) and (Number > 60) ->
  List = integer_to_list(Number),
  maybe_padding(List);

maybe_padding(Number) when is_integer(Number) and (Number > 9) and (Number < 61) ->
  integer_to_list(Number);

maybe_padding(List) when is_list(List) ->
  case (string:len(List) =/= 4) and (string:len(List) < 6) of
    true ->
      NewList = "0" ++ List,
      maybe_padding(NewList);
    false ->
      List
  end.

% --- ok ------------------------------------------------------------------------------------------

maybe_ok("OK") ->
  ok;

maybe_ok(<<"OK">>) ->
  ok;

maybe_ok("ok") ->
  ok;

maybe_ok(<<"ok">>) ->
  ok;
  
maybe_ok(Other) ->
  Other.

% --- undefined -----------------------------------------------------------------------------------

maybe_undefined(undefined) ->
  undefined;

maybe_undefined("undefined") ->
  undefined;

maybe_undefined(<<"undefined">>) ->
  undefined;

maybe_undefined(Other) ->
  Other.
  
% --- binary --------------------------------------------------------------------------------------
  
maybe_binary(undefined) ->
  undefined;
  
maybe_binary(Single) when is_binary(Single) ->
  Single;

maybe_binary(Single) when is_integer(Single) ->
  maybe_binary(integer_to_list(Single));

maybe_binary(Single) when is_atom(Single) ->
  erlang:atom_to_binary(Single, utf8);

maybe_binary(Single) when is_list(Single) ->
  list_to_binary(Single).

% --- integer -------------------------------------------------------------------------------------

maybe_integer(undefined) ->
  undefined;

maybe_integer(Single) when is_integer(Single) ->
  Single;

maybe_integer(Single) when is_binary(Single) ->
  maybe_integer(binary_to_list(Single));

maybe_integer(Single) when is_list(Single) ->
  list_to_integer(Single).
