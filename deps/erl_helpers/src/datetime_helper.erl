%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Helper module to deal with date/time.

-module(datetime_helper).
-export([datetime/0]).

%% @spec datetime() -> "MM-DD-YYYY hh:mm:ss"
%% @doc Now as "MM-DD-YYYY hh:mm:ss".
datetime() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
  
  lists:concat([maybe_helper:maybe_padding(Month), "-", maybe_helper:maybe_padding(Day),    "-", maybe_helper:maybe_padding(Year), " ",
                maybe_helper:maybe_padding(Hour),  ":", maybe_helper:maybe_padding(Minute), ":", maybe_helper:maybe_padding(Second)]).
