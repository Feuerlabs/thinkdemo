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
    io:format("Starting nmea(~p)~n", [Device]),
    {ok, Pid } = nmea_0183_srv:start(Device),
    io:format("Subscribing nmea~n", []),
    nmea_0183_srv:subscribe(Pid, 1000),
    io:format("Subscribing nmea~n", []),
    {reply, ok, #st { nmea = Pid }};

handle_call(_Msg, _From, S) ->
    {reply, error, S}.


handle_info({nmea_log, _NmeaPid, Tab, Pos, Len, Size}, State) ->
    Wpts = read_wpts(Tab, Pos, Len, Size, []),
    io:format("Waypoints: ~p~n", [Wpts]),
    %% Ulf Wiger. Add waypoint logging here.
%%    case do_some_waypoint_stuff_here({waypoint,Wpts}, nmea_0183_srv, State) of
%%	{noreply,State1} ->
%%	    {noreply, State1};
%%	{reply, What, State1} ->
%%	    {noreply, State1}
%%    end;
     {noreply, State};

handle_info(_Msg, S) ->
    {noreply, S}.

read_wpts(_Tab, _Pos, 0, _Size, Acc) ->
    lists:reverse(Acc);


read_wpts(Tab, Pos, Len, Size, Acc) ->
    case ets:lookup(Tab, Pos) of
	[{_,{position,Lat,Long}}] ->
	    FLat = trunc((Lat+90.0)*100000),
	    FLong = trunc((Long+180.0)*100000),
	    read_wpts(Tab, (Pos+1) rem Size, Len-1, Size,
		      [{absolute_position, FLat, FLong}|Acc]);
	[{_,{timestamp, Ts}}] ->
	    read_wpts(Tab, (Pos+1) rem Size, Len-1, Size,
		      [{absolute_timestamp, Ts}|Acc])
    end.


terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
timestamp() ->
    DT = erlang:universaltime(),
    calendar:datetime_to_gregorian_seconds(DT).
