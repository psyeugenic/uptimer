%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer_ssh.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-19
%%
-module(uptimer_ssh).

-behaviour(ssh_channel).

%% API
-export([
	connect/1, connect/2,
	exec/2, exec/3,
	close/1
    ]).

%% ssh_channel callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	handle_msg/2, handle_ssh_msg/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	manager,
	channel,
	timeout,
	from,
	std  = [],
	err  = []
    }).

%%%===================================================================
%%% API
%%%===================================================================

connect(Host) -> connect(Host, []).
connect(Host, Opts) ->
    Port     = proplists:get_value(port, Opts, 22),
    {ok, Cm} = ssh:connect(Host, Port, ssh_opts(Opts, [
		{user_interaction, false},	
		{silently_accept_hosts, true}
	    ])),
    {?MODULE, Cm}.

close({?MODULE, Cm}) ->
    ok = ssh:close(Cm).

%% exec/2 -> {exit_status(), [stdout()], [stderr()]}
exec(Ref, Cmd) -> exec(Ref, Cmd, infinity).
exec({?MODULE, Cm}, Cmd, Tmo) ->
    {ok, Cid} = ssh_connection:session_channel(Cm, infinity),
    {ok, Pid} = ssh_channel:start_link(Cm, Cid, ?MODULE, [Cm, Cid, Tmo]),
    ssh_channel:call(Pid, {exec, Cmd}, infinity).

%%%===================================================================
%%% ssh_channel callbacks
%%%===================================================================

init([Manager,ChannelId,Timeout]) ->
    {ok, #state{
	    manager = Manager,
	    channel = ChannelId,
	    timeout = Timeout
	}}.

%% handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%% Handles channel messages
handle_msg({ssh_channel_up,_Channel,_Manager}, S) ->
    {ok, S};
handle_msg(_Msg, S) ->
    {ok, S}.

%% handle_ssh_msg(Msg, State) -> {ok, State} | {stop, ChannelId, State}
handle_ssh_msg({ssh_cm, _Manager, {exit_status,_,Exit}}, #state{ from=From, std=Std, err=Err}=S) ->
    ssh_channel:reply(From, {Exit, lists:reverse(Std), lists:reverse(Err)}),
    {ok, S};
handle_ssh_msg({ssh_cm, _Manager, {data,_,Type,Data}}, S) ->
    {ok, update_data(Type, Data, S)};
handle_ssh_msg({ssh_cm, _Manager, {eof,Channel}}, S) ->
    {stop, Channel, S};
handle_ssh_msg(_Msg, S) ->
    {ok, S}.

%% handle_call(Msg, From, State)
handle_call({exec, Cmd}, From, #state{ manager=Cm, channel=Id, timeout=Tmo}=S) ->
    success = ssh_connection:exec(Cm, Id, Cmd, Tmo),
    {noreply, S#state{ from=From }};

handle_call(_Request, _From, S) ->
    {reply, no_action, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_data(0, Data, #state{ std=Std } = S) -> S#state{ std=[Data|Std] };
update_data(1, Data, #state{ err=Err } = S) -> S#state{ err=[Data|Err] }.

ssh_opts([{user, _}=Opt|Os], Opts) -> ssh_opts(Os, [Opt|Opts]);
ssh_opts([_|Os], Opts) -> ssh_opts(Os, Opts);
ssh_opts([], Opts) -> Opts.
