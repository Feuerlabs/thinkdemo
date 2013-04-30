-module(thinkdemo_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 start_phase/3,
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    thinkdemo_sup:start_link().

start_phase(alarms, _, _) ->
    thinkdemo_alarms:read_config(),
    ok;
start_phase(logging, _, _) ->
    thinkdemo_log:read_config(),
    ok;
start_phase(ping, _, _) ->
    exoport:ping(),
    thinkdemo_log:start_refresh_ping(60000),
    ok;
start_phase(can, _, _) ->
    thinkdemo_can:start_can(),
    ok;
start_phase(waypoints, _, _) ->
    thinkdemo_waypoints:start_waypoints("/dev/ttySAC1"),
    ok.

stop(_State) ->
    ok.


