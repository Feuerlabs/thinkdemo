-module(exodemo_log).
-behavior(gen_server).

-export([log_can/3]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {}).

log_can(FrameID, DataLen, Data) ->
    TS = exodemo_lib:timestamp(),
    %% FIXME: ms_timestamp() should be millisec_timestamp() (ms since epoch).
    gen_server:cast(?MODULE, {log_can, TS, FrameID, DataLen, Data}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_cast({log_can, TS, FrameID, DataLen, Data}, S) ->
    io:format("Will log TS:~p FrameID:~p DataLen:~p Data:~p~n",
	      [TS, FrameID, DataLen, Data]),
    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.


handle_call(_Msg, _From, S) ->
    {reply, error, S}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
%% FIXME ms since epoch
%% ms_timestamp() ->

%%     DT = erlang:universaltime(),
%%     calendar:datetime_to_gregorian_seconds(DT).
