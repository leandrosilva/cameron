%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

%% syslog rfc: http://www.faqs.org/rfcs/rfc3164.html

-module(syslog).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0, start_link/1, start_link/3, start_link/4, start_link/5,
         send/1, send/2, send/3,
         emergency/1, emergency/2, emergency/3,
         alert/1, alert/2, alert/3,
         critical/1, critical/2, critical/3,
         error/1, error/2, error/3,
         warning/1, warning/2, warning/3,
         notice/1, notice/2, notice/3,
         info/1, info/2, info/3,
         debug/1, debug/2, debug/3]).

-record(state, {socket, address, port, facility, tag}).

-define(DEFAULT_FACILITY, local0).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    {ok, Host} = inet:gethostname(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?MODULE, Host, 514, ?DEFAULT_FACILITY], []).

start_link(Name) ->
    {ok, Host} = inet:gethostname(),
    gen_server:start_link({local, Name}, ?MODULE, [?MODULE, Host, 514, ?DEFAULT_FACILITY], []).

start_link(Name, Host, Port) when is_atom(Name), is_list(Host), is_integer(Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [?MODULE, Host, Port, ?DEFAULT_FACILITY], []).

start_link(Name, Host, Port, Facility) when is_atom(Name), is_list(Host),
                                           is_integer(Port), is_atom(Facility) ->
    gen_server:start_link({local, Name}, ?MODULE, [?MODULE, Host, Port, Facility], []).

start_link(Name, Tag, Host, Port, Facility) when is_atom(Name), is_atom(Tag), is_list(Host),
                                                 is_integer(Port), is_atom(Facility) ->
    gen_server:start_link({local, Name}, ?MODULE, [Tag, Host, Port, Facility], []).

send(Msg) when is_list(Msg) ->
    send(?MODULE, Msg).

send(Name, Msg) when is_list(Msg) ->
    send(Name, Msg, []).

send(Name, Msg, Opts) when is_list(Msg), is_list(Opts) ->
    Packet = build_packet(Name, Msg, Opts),
    %io:format("~p~n", [Packet]),
    gen_server:cast(Name, {send, Packet}).

emergency(Msg) ->
  emergency(?MODULE, Msg, []).

emergency(Name, Msg) ->
  emergency(Name, Msg, []).

emergency(Name, Msg, Opts) ->
  send(Name, Msg, [{level, emergency}] ++ Opts).

alert(Msg) ->
  alert(?MODULE, Msg, []).

alert(Name, Msg) ->
  alert(Name, Msg, []).

alert(Name, Msg, Opts) ->
  send(Name, Msg, [{level, alert}] ++ Opts).

critical(Msg) ->
  critical(?MODULE, Msg, []).

critical(Name, Msg) ->
  critical(Name, Msg, []).

critical(Name, Msg, Opts) ->
  send(Name, Msg, [{level, critical}] ++ Opts).

error(Msg) ->
  error(?MODULE, Msg, []).

error(Name, Msg) ->
  error(Name, Msg, []).

error(Name, Msg, Opts) ->
  send(Name, Msg, [{level, error}] ++ Opts).

warning(Msg) ->
  warning(?MODULE, Msg, []).

warning(Name, Msg) ->
  warning(Name, Msg, []).

warning(Name, Msg, Opts) ->
  send(Name, Msg, [{level, warning}] ++ Opts).

notice(Msg) ->
  notice(?MODULE, Msg, []).

notice(Name, Msg) ->
  notice(Name, Msg, []).

notice(Name, Msg, Opts) ->
  send(Name, Msg, [{level, notice}] ++ Opts).

info(Msg) ->
  info(?MODULE, Msg, []).

info(Name, Msg) ->
  info(Name, Msg, []).

info(Name, Msg, Opts) ->
  send(Name, Msg, [{level, info}] ++ Opts).

debug(Msg) ->
  debug(?MODULE, Msg, []).

debug(Name, Msg) ->
  debug(Name, Msg, []).

debug(Name, Msg, Opts) ->
  send(Name, Msg, [{level, debug}] ++ Opts).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Tag, Host, Port, Facility]) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    case gen_udp:open(0) of
        {ok, Socket} ->
            {ok, #state{
                    socket = Socket,
                    address = Addr,
                    port = Port,
                    facility = Facility,
                    tag = Tag
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(facility, _From, #state{facility=Facility}=State) ->
    {reply, Facility, State};
handle_call(tag,  _From, #state{tag=Tag}=State) ->
    {reply, Tag, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send, Packet}, #state{socket=Socket, address=Address, port=Port}=State) when is_binary(Packet) ->
    gen_udp:send(Socket, Address, Port, Packet),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_level(Facility, Opts) ->
    Level = atom_to_level(proplists:get_value(level, Opts)),
    integer_to_list((Facility * 8) + Level).

get_tag(Name) ->
    case gen_server:call(Name, tag) of
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end.

get_pid(Opts) ->
    case proplists:get_value(pid, Opts) of
        undefined -> os:getpid();
        Atom when is_atom(Atom) -> atom_to_list(Atom);
        List when is_list(List) -> List;
        Binary when is_binary(Binary) -> Binary
    end.

get_facility(Name) ->
    Facility = gen_server:call(Name, facility),
    facility(Facility).

get_hostname() ->
    {ok, Host} = inet:gethostname(),
    Host.

get_timestamp() ->
    {{_,Month,Day},{Hr,Min,Sec}} = calendar:local_time(),
    StringMonth = httpd_util:month(Month),

    StringMonth ++ " "
        ++ maybe_add_padding(Day) ++ " "
        ++ maybe_add_padding(Hr) ++ ":"
        ++ maybe_add_padding(Min) ++ ":"
        ++ maybe_add_padding(Sec).

maybe_add_padding(Int) when Int < 10 ->
    "0" ++ integer_to_list(Int);
maybe_add_padding(Int) ->
    integer_to_list(Int).

build_packet(Name, Msg, Opts) ->
    Tag = get_tag(Name),
    Pid = get_pid(Opts),
    Hostname = get_hostname(),
    Timestamp = get_timestamp(),

    Facility = get_facility(Name),
    Level = get_level(Facility, Opts),

    Packet = [
              "<", Level, "> ",
              Timestamp, " ", Hostname, " ",
              Tag, "[", Pid, "]: ", Msg, "\n"
             ],

    iolist_to_binary(Packet).

atom_to_level(emergency) -> 0; % system is unusable
atom_to_level(alert) -> 1; % action must be taken immediately
atom_to_level(critical) -> 2; % critical conditions
atom_to_level(error) -> 3; % error conditions
atom_to_level(warning) -> 4; % warning conditions
atom_to_level(notice) -> 5; % normal but significant condition
atom_to_level(info) -> 6; % informational
atom_to_level(debug) -> 7; % debug-level messages
atom_to_level(_) -> atom_to_level(info). % default to info

% paraphrased from https://github.com/ngerakines/syslognif/blob/master/src/syslog.erl#L55
facility(kern) -> 0;      % kernel messages
facility(user) -> 1;      % random user-level messages
facility(mail) -> 2;      % mail system
facility(daemon) -> 3;    % system daemons
facility(auth) -> 4;      % security/authorization messages
facility(syslog) -> 5;    % messages generated internally by syslogd
facility(lpr) -> 6;       % line printer subsystem
facility(news) -> 7;      % network news subsystem
facility(uucp) -> 8;      % UUCP subsystem
facility(cron) -> 9;      % clock daemon
facility(authpriv) -> 10; % security/authorization messages (private)
facility(ftp) -> 11;      % ftp daemon

facility(local0) -> 16;   % reserved for local use
facility(local1) -> 17;   % reserved for local use
facility(local2) -> 18;   % reserved for local use
facility(local3) -> 19;   % reserved for local use
facility(local4) -> 20;   % reserved for local use
facility(local5) -> 21;   % reserved for local use
facility(local6) -> 22;   % reserved for local use
facility(local7) -> 23.   % reserved for local use
