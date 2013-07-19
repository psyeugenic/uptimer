%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    uptimer.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-12
%%
-module(uptimer).

-export([
	start/0,
	start_link/0,
	stop/0,

	add_host/1,
	del_host/1,
	get_host/1,
	hosts/0
    ]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

stop()       -> application:stop(uptimer).
start_link() -> uptimer_sup:start_link().
start()      ->
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssh),
    ensure_started(uptimer).


add_host(Host)       -> add_host(Host, []).
add_host(Host, Opts) -> uptimer_service:add_host(Host, Opts).
del_host(Host)       -> del_host(Host, []).
del_host(Host, Opts) -> uptimer_service:del_host(Host, Opts).
get_host(Host)       -> get_host(Host, []).
get_host(Host, Opts) -> uptimer_service:get_host(Host, Opts).

hosts() -> uptimer_service:hosts(all, []).


