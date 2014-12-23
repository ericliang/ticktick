%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ticktick.

-module(ticktick_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
					"id" ->
						Id = ticktick_id:id_hex(),
						Req:respond({200, [{"Content-Type", "text/plain"}],
									 Id ++ "\n"});
					"explain" ->
						QueryStringData = Req:parse_qs(),
						Id = proplists:get_value("id", QueryStringData, undefined),
						IdPropList = ticktick_id:explain( hex:hexstr_to_bin(Id) ),
						IdJson = jsx:encode(IdPropList),
						Req:respond({200, [{"Content-Type", "application/json"}],
									 IdJson });
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

overall_test_() ->
	{"Simple test on ticktick server.",
	 {setup, fun setup/0, fun cleanup/1,
	  {with, [ fun id_seq/1 ]}}
	}.

setup() ->
	start_link(123).

cleanup(_) ->
	stop().

id_seq(_) ->
	{ok, Id} = id(),
	{ok, Id1} = sibling(Id),

	{ok, TTID} = bin_to_ttid(Id),
	{ok, TTID1} = bin_to_ttid(Id1),
	io:format("~p ~p~n", [TTID, TTID1]),
	?assertEqual( TTID#ttid.sequence + 1, TTID1#ttid.sequence ).	

-endif.
