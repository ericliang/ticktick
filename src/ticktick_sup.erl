%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Supervisor for the ticktick application.

-module(ticktick_sup).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Web = web_specs(ticktick_web, 8080),
	Id = id_specs(),
    Processes = [Web, Id],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, DefaultPort) ->
	Ip = get_app_env( ip, {0,0,0,0} ),
	Port = get_app_env( port, DefaultPort ),

    WebConfig = [{ip, Ip},
                 {port, Port},
                 {docroot, ticktick_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.

id_specs() ->
	MachineId = get_app_env( machine_id, 0 ),
	{ticktick_id,
	 {ticktick_id, start_link, [MachineId]},
	 permanent, 1000, worker, [ticktick_id]}. %% 1000 timeout to prevent ids-mixed

get_app_env( Key, DefaultValue ) ->
	case application:get_env( ticktick, Key ) of
		{ok, Value } ->
			Value;
		_ ->
			DefaultValue
	end.
