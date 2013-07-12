%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%

-module(uptimer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, [{hosts, Hosts}]} = file:consult("test/hosts.dat"),
    Res = uptimer_sup:start_link(),
    start_children(Hosts),
    Res.

start_children([]) -> ok;
start_children([Host|Hosts]) ->
    supervisor:start_child(uptimer_worker_sup,[[{host, Host}]]),
    start_children(Hosts).

stop(_State) ->
    ok.
