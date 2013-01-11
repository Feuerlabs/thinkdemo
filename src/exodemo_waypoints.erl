-module(exodemo_waypoints).
-behavior(gen_server).

-export([start_waypoints/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {nmea}).

start_waypoints(Device) ->
    gen_server:call(?MODULE, {start_waypoints, Device}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{nmea = false}}.


handle_cast(_, S) ->
    {noreply, S}.

handle_call({start_waypoints, Device}, _From, _S) ->
    nmea_0183_app:start(false, false),
    Res = nmea_0183_srv:start(Device),
    nmea_0183_srv:subscribe(self(), true),
    {reply, ok, #st { nmea = Res }};

handle_call(_Msg, _From, S) ->
    {reply, error, S}.

handle_info(Msg, S) ->
    io:format("exodemo_waypoints:handle_info(~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
timestamp() ->
    DT = erlang:universaltime(),
    calendar:datetime_to_gregorian_seconds(DT).
