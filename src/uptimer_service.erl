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
	hosts/0, hosts/1, hosts/2,
	start_link/0
    ]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	hosts = gb_trees:empty()
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

hosts()    -> hosts(all).
hosts(Req) -> hosts(Req, []).
hosts(Req, Opts) ->
    gen_server:call(?SERVER, {hosts, Req, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({hosts, all, _}, _From, #state{ hosts=T } = S) ->
    Hs = [{H, Status} || {H, {Status,_}} <- gb_trees:to_list(T) ],
    {reply, Hs, S};
handle_call({hosts, down, _}, _From, #state{ hosts=T } = S) ->
    Hs = lists:foldl(fun
	    ({H, {?host_down, Vs}}, Acc) ->
		[{H, proplists:get_value(seen, Vs)}|Acc];
	    (_, Acc) -> Acc
	end, [], gb_trees:to_list(T)),
    {reply, Hs, S};
handle_call(Req, _From, State) ->
    {reply, {unknown_request, Req}, State}.


%% {status,"pharazon",reachable,
%%     [{seen,<<"2013-07-12 16:55:23">>},
%% 	{host,<<"pharazon.otp.ericsson.se">>},
%% 	{time,<<"0.152">>},
%%      {worker, <0.156.0>]}

handle_cast({status, Host, Status, Vs}, #state{ hosts = T } = S) ->
    {noreply, S#state{ hosts=gb_trees:enter(Host, {Status, Vs}, T) }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
