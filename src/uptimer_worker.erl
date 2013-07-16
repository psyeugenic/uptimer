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

-record(state, {
	timer_ref,
	service = uptimer_service,
	timer   = ?DEFAULT_REQUEST_TIME,
	status  = undefined,
	values  = [],
	host    = "localhost"
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
init([{host, Host}|Data], S) -> init(Data, S#state{ host  = Host });
init([{timer, T}|Data], S)   -> init(Data, S#state{ timer = T });
init([_|Data], S)            -> init(Data, S);
init([], S)                  -> {ok, inform_and_setup_status(S)}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_info(request_status, S) ->
    {noreply, inform_and_setup_status(S)};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

inform_and_setup_status(S) ->
    Status = host_status(S),
    setup_request(inform_status(Status, S)).

inform_status({Status, Vs}, S) ->
    inform_status(Status, Vs, S).

%% same as before
inform_status(Status, Vs, #state{ status=Status } = S) ->
    S#state{ values = Vs };
%% initialized
inform_status(Status, Vs, #state{ status=undefined } = S) ->
    inform_service(Status, Vs, S),
    S#state{ status = Status, values = Vs };
%% change occured
inform_status(Status, Vs, #state{} = S) ->
    inform_service(Status, Vs, S),
    S#state{ status = Status, values = Vs }.

inform_service(Status, Vs, #state{ service = Service, host = Host }) ->
    gen_server:cast(Service, {status, Host, Status, [
		{seen, datetime_string()}|Vs]}).
    
host_status(S) ->
    ping(S).
    % ssh


%PING www.google.se (173.194.40.24) 56(84) bytes of data.
%64 bytes from mil02s06-in-f24.1e100.net (173.194.40.24): icmp_req=1 ttl=46 time=39.6 ms
%
%--- www.google.se ping statistics ---
%1 packets transmitted, 1 received, 0% packet loss, time 0ms
%rtt min/avg/max/mdev = 39.648/39.648/39.648/0.000 ms

ping(#state{ host = Host }) ->
    %{0, Res} = run("ping", ["-c 1", Host]),
    Res = os:cmd("ping -c 1 " ++ Host),
    ReOpts   = [global, {capture, all_but_first, binary}],
    case re:run(Res, "time=([0-9]+\.[0-9]+) ms", ReOpts) of
	{match,[[Time]]} ->
	    {match, [[From]]} = re:run(Res, "bytes from (.*) \\(", ReOpts),
	    {?host_up, [
		    {host, From}, 
		    {time, erlang:binary_to_float(Time)}]};
	_ ->
	    {?host_down, []}
    end.

setup_request(#state{ timer = T } = S) ->
    S#state{ timer_ref = erlang:send_after(T, self(), request_status) }.

datetime_string() -> datetime_string(erlang:universaltime()).
datetime_string({{Y,Mon,D},{H,Min,S}}) ->
    Format = "~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
    iolist_to_binary((io_lib:format(Format, [Y,Mon,D,H,Min,S]))).

%run(Program0, Args) ->
%    Program = case os:find_executable(Program0) of
%	Path when is_list(Path) -> Path;
%	false -> error_msg("Unable to find program: ~s\n", [Program0])
%    end,
%    Options = [{args,Args},binary,exit_status,stderr_to_stdout,{line,4096}],
%    try open_port({spawn_executable,Program}, Options) of
%	Port -> run_loop(Port, [])
%    catch
%	error:_ -> error_msg("Failed to execute ~s\n", [Program0])
%    end.
%
%run_loop(Port, Output) ->
%    receive
%	{Port,{exit_status,Status}} ->
%	    {Status,lists:reverse(Output)};
%	{Port,{data,{eol,Bin}}} ->
%	    run_loop(Port, [Bin|Output]);
%	_Msg ->
%	    %io:format(standard_error,"L: ~p~n", [_Msg]),
%	    run_loop(Port, Output)
%    end.
%
%error_msg(Format, Args) ->
%    Msg = lists:flatten(io_lib:format(Format, Args)),
%    io:put_chars(standard_error, [Msg]),
%    erlang:error(Msg).
