-module(exodemo_alarms).
-behavior(gen_server).

-export([set/2,
	 clear/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {alarms = orddict:new()}).
-record(alarm, {status, ts, value}).

-define(TWO_HOURS, 2*60*60).  % in seconds

set(FrameID, Value) ->
    gen_server:cast(?MODULE, {set, timestamp(), FrameID, Value}).

clear(FrameID) ->
    gen_server:cast(?MODULE, {clear, FrameID}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

handle_cast({set, TS, FrameID, Value}, #st{alarms = As} = S) ->
    S1 = case orddict:find(FrameID, As) of
	     {ok, #alarm{status = sent}} ->
		 S;
	     _ ->
		 %% send alarm...
		 self() ! send_alarms,
		 NewAs = orddict:store(
			   FrameID, #alarm{status = set,
					   ts = TS,
					   value = Value}, As),
		 S#st{alarms = NewAs}
	 end,
    {noreply, S1};
handle_cast({clear, FrameID}, #st{alarms = As} = S) ->
    case orddict:find(FrameID, As) of
	error ->
	    {reply, false, S};
	{ok, A} ->
	    NewAs = orddict:store(FrameID, A#alarm{status = cleared}, As),
	    {reply, true, S#st{alarms = NewAs}}
    end;
handle_cast(_, S) ->
    {noreply, S}.


handle_call(_Msg, _From, S) ->
    {reply, error, S}.

handle_info(send_alarms, #st{alarms = As} = S) ->
    flush_send_msgs(),
    NewAlarms = rpc(As),
    {noreply, S#st{alarms = NewAlarms}}.

terminate(_Reason, _S) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions

timestamp() ->
    DT = erlang:universaltime(),
    calendar:datetime_to_gregorian_seconds(DT) - ?TWO_HOURS.  % GMT

flush_send_msgs() ->
    receive
	send_alarms ->
	    flush_send_msgs()
    after 0 ->
	    ok
    end.

rpc(Alarms) ->
    {ToSend, NewAlarms} =
	lists:mapfoldr(
	  fun({FrameID, #alarm{status = set,
			       ts = TS,
			       value = Value} = A}, Acc) ->
		  {[{struct, [{'ts', TS},
			      {'can-frame-id', FrameID},
			      {'can-value', Value}]} | Acc],
		   {FrameID, A#alarm{status = sent}}};
	     (Entry, Acc) ->
		  {Entry, Acc}
	  end, [], Alarms),
    exoport:rpc(exodm_rpc, rpc, [<<"demo">>, <<"process-alarms">>,
				 [{'alarms', {array, ToSend}}]]),
    NewAlarms.
