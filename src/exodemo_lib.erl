-module(exodemo_lib).

-export([timestamp/0,
	 ts_to_datetime/1]).


timestamp() ->
    {_,_,US} = Now = os:timestamp(),
    MS = round(US/1000),
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(Now)) * 1000 + MS.

ts_to_datetime(TS) ->
    MS = TS rem 1000,
    S = TS div 1000,
    {calendar:gregorian_seconds_to_datetime(S), MS}.
