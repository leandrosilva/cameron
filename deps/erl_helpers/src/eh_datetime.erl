%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to deal with date/time.

-module(eh_datetime).
-export([now/0]).

%% @spec now() -> "MM-DD-YYYY hh:mm:ss"
%% @doc Now as "MM-DD-YYYY hh:mm:ss".
now() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  
  lists:concat([eh_maybe:maybe_padding(Month), "-", eh_maybe:maybe_padding(Day),    "-", eh_maybe:maybe_padding(Year), " ",
                eh_maybe:maybe_padding(Hour),  ":", eh_maybe:maybe_padding(Minute), ":", eh_maybe:maybe_padding(Second)]).
