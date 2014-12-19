%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc ticktick.

-module(ticktick).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ticktick server.
start() ->
    ticktick_deps:ensure(),
    ensure_started(crypto),
    application:start(ticktick).


%% @spec stop() -> ok
%% @doc Stop the ticktick server.
stop() ->
    application:stop(ticktick).
