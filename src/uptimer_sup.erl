%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer_sup.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%

-module(uptimer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_one, 5, 10},
    Children = [
	?CHILD(uptimer_service, worker),
	?CHILD(uptimer_worker_sup, supervisor)
    ],
    {ok, {RestartStrategy, Children}}.
