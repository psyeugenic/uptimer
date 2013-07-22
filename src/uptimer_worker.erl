%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer_worker.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%

-module(uptimer_worker).
-behaviour(gen_server).

-include_lib("uptimer.hrl").

%% API
-export([
	start_link/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(request(T), {request_status, T}).

%-define(b2f(B), erlang:binary_to_float(B)).
%-define(b2i(B), erlang:binary_to_integer(B)).
-define(b2f(B), list_to_float(binary_to_list(B))).
-define(b2i(B), list_to_integer(binary_to_list(B))).

-record(state, {
	timers    = gb_trees:empty(),
	timer     = ?DEFAULT_REQUEST_TIME,
	host      = "localhost",
	service   = undefined,
	ssh       = undefined, % ssh connection
	port      = undefined, % used for ping
	reports   = [ping]
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Data)  -> init(Data, #state{}).
init([{host, Host}|Data], S)  -> init(Data, S#state{ host  = Host });
init([{timer, T}|Data], S)    -> init(Data, S#state{ timer = T });
init([{service, M}|Data], S)  -> init(Data, S#state{ service = M });
init([{reports, Rs}|Data], S) -> init(Data, S#state{ reports = Rs });
init([_|Data], S)             -> init(Data, S);
init([], S)                   -> {ok, setup_reporters(S)}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_info(?request(ping), #state{ host=Host, port=P }=S) ->
    command(P, "ping -c 1 " ++ Host),
    {noreply, setup_request(ping, S)};
handle_info({P, {data,"PING"++_ = Msg}}, #state{ port=P }=S) ->
    {Status, Vs} = parse_ping(Msg),
    inform_service(Status, Vs, S),
    {noreply, S};

handle_info(?request(ssh), #state{ ssh=ConRef }=S) ->
    case uptimer_ssh:exec(ConRef, "LANG=C uptime") of
	{0, Value, _} ->
	    Uptime = parse_uptime(Value),
	    io:format("Uptime: ~p~n", [Uptime]);
	    _ -> ok
    end,
    {noreply, setup_request(ssh,S)};

handle_info(_Info, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_reporters(#state{ reports=Rs0 } = S) ->
    Rs = lists:usort(Rs0),
    setup_reporters(Rs, S#state{ reports=Rs }).

setup_reporters([], S) -> S;
setup_reporters([ssh|Rs], #state{ host=Host }=S) ->
    try
	ConRef = uptimer_ssh:connect(Host),
	self() ! ?request(ssh),
	setup_reporters(Rs, setup_request(ssh, S#state{
		    ssh = ConRef
		}))
    catch
	error:_ ->
	    Msg = io_lib:format("Failed to setup ssh service for host: ~s~n", [Host]),
	    error_logger:error_msg(Msg),
	    setup_reporters(Rs, S)
    end;
setup_reporters([ping|Rs], S) ->
    self() ! ?request(ping),
    setup_reporters(Rs, setup_request(ping, S#state{
	    port = start_pinger_port()
	})).

setup_request(Type, #state{ timers = Timers, timer = T } = S) ->
    Ref = erlang:send_after(T, self(), ?request(Type)),
    S#state{ 
	timers = gb_trees:enter(Type, Ref, Timers)
    }.

inform_service(_,_,#state{ service=undefined }) -> ok;
inform_service(Status, Vs, #state{ service=Service, host=Host }) ->
    gen_server:cast(Service, {status, Host, Status, [
		{seen, datetime_string()}|Vs]}).

datetime_string() -> datetime_string(erlang:universaltime()).
datetime_string({{Y,Mon,D},{H,Min,S}}) ->
    Format = "~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
    iolist_to_binary((io_lib:format(Format, [Y,Mon,D,H,Min,S]))).


% master egil@palantir /ldisk/egil/git/uptimer $ ssh ..  "LANG=C uptime"
%   2:25pm  up 194 days  1:23,  15 users,  load average: 0.00, 0.00, 0.00

parse_uptime(Vs) ->
    ReOpts = [global, {capture, all_but_first, binary}],
    Match  = "up ([0-9]+) days,\\s+([0-9]+):([0-9]+),\\s+([0-9]+) users,"
	     "\\s+load average: ([0-9]+\\.[0-9]+),\\s+([0-9]+\\.[0-9]+),\\s+([0-9]+\\.[0-9]+)",
    case re:run(list_to_binary(Vs), Match, ReOpts) of
	{match,[[Days,Hours,Mins,Users,Load1,Load5,Load15]]} -> [
		{uptime, [{days,    ?b2i(Days)},
			  {hours,   ?b2i(Hours)},
			  {minutes, ?b2i(Mins)}
		      ]},
		{users, ?b2i(Users)},
		{load,  [{ 1, ?b2f(Load1)},
			 { 5, ?b2f(Load5)},
		 	 {15, ?b2f(Load15)}
		    ]}
	    ];
	_ -> []
    end.

% PING www.google.se (173.194.40.24) 56(84) bytes of data.
% 64 bytes from mil02s06-in-f24.1e100.net (173.194.40.24): icmp_req=1 ttl=46 time=39.6 ms
% 
% --- www.google.se ping statistics ---
% 1 packets transmitted, 1 received, 0% packet loss, time 0ms
% rtt min/avg/max/mdev = 39.648/39.648/39.648/0.000 ms

parse_ping(PingMsg) ->
    ReOpts   = [global, {capture, all_but_first, binary}],
    case re:run(PingMsg, "time=([0-9]+\.[0-9]+) ms", ReOpts) of
	{match,[[Time]]} ->
	    {match, [[From]]} = re:run(PingMsg, "bytes from (.*) \\(", ReOpts),
	    {?host_up, [
		    {host, From}, 
		    {time, ?b2f(Time)}]};
	_ ->
	    {?host_down, []}
    end.

%% port handling

start_pinger_port() ->
    open_port({spawn, "sh -s uptimer_pinger 2>&1"}, [stream]).

command(Port,Cmd) ->
    Data = io_lib:format("(~s\n) </dev/null; echo  \"\^M\"\n", [Cmd]),
    Port ! {self(), {command, [Data, 10]}},
    ok.

