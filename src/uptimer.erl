%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%
-module(uptimer).

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%set_envs(App, Envs) ->
%    application:load(App),
%    [application:set_env(App, Key, Val) || {Key,Val} <- Envs].

start_link() ->
    uptimer_sup:start_link().

start() ->
    ensure_started(uptimer).

stop() ->
    application:stop(uptimer).

