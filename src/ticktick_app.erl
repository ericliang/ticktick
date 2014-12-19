%% @author Mochi Media <dev@mochimedia.com>
%% @copyright ticktick Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the ticktick application.

-module(ticktick_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ticktick.
start(_Type, _StartArgs) ->
    ticktick_deps:ensure(),
    ticktick_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ticktick.
stop(_State) ->
    ok.
