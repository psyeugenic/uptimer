%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer_service.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%

-module(uptimer_service).
-behaviour(gen_server).

-include_lib("uptimer.hrl").

%% API
-export([
	add_host/2,
	del_host/2,
	get_host/2,
	hosts/2,
	start_link/0
    ]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	hosts = gb_trees:empty()
    }).

-record(host, {
	worker,
	status,
	values = []
    }).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

hosts(Req, Opts) ->
    gen_server:call(?SERVER, {hosts, Req, Opts}).

del_host(Host, Opts) -> gen_server:call(?SERVER, {del_host, Host, Opts}).
get_host(Host, Opts) -> gen_server:call(?SERVER, {get_host, Host, Opts}).
add_host(Host, Opts) ->
    gen_server:call(?SERVER, {add_host, Host, [{service,?MODULE}|Opts]}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({hosts, all, _}, _From, #state{ hosts=T } = S) ->
    Hs = [{H, Status} || {H, #host{status = Status}} <- gb_trees:to_list(T) ],
    {reply, Hs, S};
handle_call({hosts, down, _}, _From, #state{ hosts=T } = S) ->
    Hs = lists:foldl(fun
	    ({H, #host{status=?host_down, values=Vs}}, Acc) ->
		[{H, proplists:get_value(seen, Vs)}|Acc];
	    (_, Acc) -> Acc
	end, [], gb_trees:to_list(T)),
    {reply, Hs, S};

handle_call({add_host, Host, Opts}, _From, #state{ hosts=T } = S) ->
    case gb_trees:is_defined(Host, T) of
	true  -> {reply, {error, already_added}, S};
	false ->
	    Args = [[{host, Host}|Opts]],
	    {ok, Pid} = supervisor:start_child(uptimer_worker_sup,Args),
	    {reply, ok, S#state{
		    hosts = gb_trees:enter(Host, #host{
			    worker = Pid,
			    status = started
			},T)}}
    end;

handle_call({del_host, Host, _}, _From, #state{ hosts=T } = S) ->
    #host{ worker=Pid } = gb_trees:get(Host, T),
    Res = supervisor:terminate_child(uptimer_worker_sup, Pid),
    {reply, Res, S#state{ hosts = gb_trees:delete(Host,T)}};

handle_call({get_host, Host, _}, _From, #state{ hosts=T } = S) ->
    #host{ status=Status, values=Vs } = gb_trees:get(Host, T),
    {reply, {Status, Vs}, S};

handle_call(Req, _From, State) ->
    {reply, {unknown_request, Req}, State}.

%% {status,"pharazon",reachable,
%%     [{seen,<<"2013-07-12 16:55:23">>},
%% 	{host,<<"pharazon.otp.ericsson.se">>},
%% 	{time,<<"0.152">>},
%%      {worker, <0.156.0>]}

handle_cast({status, Host, Status, Vs}, #state{ hosts = T } = S) ->
    H = gb_trees:get(Host, T),
    {noreply, S#state{ hosts=gb_trees:enter(Host, H#host{
		    status = Status,
		    values = Vs
		}, T) }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
