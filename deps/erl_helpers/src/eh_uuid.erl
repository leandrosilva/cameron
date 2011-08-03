%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to generate UUID.

-module(eh_uuid).
-export([new/0, test_unique/0]).

new() ->
  bin_to_hexstr(crypto:rand_bytes(16)).  

test_unique() ->
  Curr = new(),
  test_unique(1, [Curr]).

test_unique(Index, Prevs) ->
  Curr = new(),
  
  case lists:member(Curr, Prevs) of
    true ->
      io:format("[~w] Not unique: ~s~n", [Index, Curr]);
    false ->
      io:format("[~w] Unique: ~s~n", [Index, Curr]),
      test_unique(Index + 1, Prevs)
  end.
  
%%
%% Code below is from:
%%
%% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
%%

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a+(N-10).

% int(C) when $0 =< C, C =< $9 ->
%   C - $0;
% int(C) when $A =< C, C =< $F ->
%   C - $A + 10;
% int(C) when $a =< C, C =< $f ->
%   C - $a + 10.
  
to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) -> 
  [];
list_to_hexstr([H|T]) ->
  to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
  list_to_hexstr(binary_to_list(Bin)).

% hexstr_to_bin(S) ->
%   list_to_binary(hexstr_to_list(S)).

% hexstr_to_list([X,Y|T]) ->
%   [int(X)*16 + int(Y) | hexstr_to_list(T)];
% hexstr_to_list([]) ->
%   [].
  