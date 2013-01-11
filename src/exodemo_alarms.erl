-module(exodemo_alarms).
-behavior(gen_server).

-export([
	 %% set/2,
	 %% clear/1,
	 check_alarm/3]).
-export([config_update/1]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include_lib("lager/include/log.hrl").

-record(st, {alarms = orddict:new()}).
-record(alarm, {status = clear, set, reset, ts, value}).

-define(TWO_HOURS, 2*60*60).  % in seconds

%% set(FrameID, Value) ->
%%     gen_server:cast(?MODULE, {set, timestamp(), FrameID, Value}).

check_alarm(FrameID, DataLen, Data) ->
    gen_server:cast(?MODULE, {check_alarm, timestamp(), FrameID, Data, DataLen}).

config_update(Data) ->
    gen_server:cast(?MODULE, {config, alarms(Data)}).

alarms(ConfigEntries) ->
    ?debug("alarms(~p)~n", [ConfigEntries]),
    [].

%% clear(FrameID) ->
%%     gen_server:cast(?MODULE, {clear, FrameID}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #st{}}.

%% handle_cast({set, TS, FrameID, Value}, #st{alarms = As} = S) ->
%%     S1 = case orddict:find(FrameID, As) of
%% 	     {ok, #alarm{status = sent}} ->
%% 		 S;
%% 	     _ ->
%% 		 %% send alarm...
%% 		 self() ! send_alarms,
%% 		 NewAs = orddict:store(
%% 			   FrameID, #alarm{status = set,
%% 					   ts = TS,
%% 					   value = Value}, As),
%% 		 S#st{alarms = NewAs}
%% 	 end,
%%     {noreply, S1};
%% handle_cast({clear, FrameID}, #st{alarms = As} = S) ->
%%     case orddict:find(FrameID, As) of
%% 	error ->
%% 	    {reply, false, S};
%% 	{ok, A} ->
%% 	    NewAs = orddict:store(FrameID, A#alarm{status = cleared}, As),
%% 	    {reply, true, S#st{alarms = NewAs}}
%%     end;
handle_cast({check_alarm, TS, FrameID, Data, DataLen},
	    #st{alarms = As} = S) ->
    S1 =case orddict:find(FrameID, As) of
	    {ok, #alarm{set = SThr, reset = CThr} = Alarm} ->
		case can_data_value(DataLen, Data) of
		    I when is_integer(I) ->
			if I > SThr -> set_alarm(TS, FrameID, I, Alarm, S);
			   I < CThr -> clear_alarm(TS, FrameID, I, Alarm, S)
			end;
		    _ ->
			S
		end;
	    _ ->
		S
	end,
    {noreply, S1};

handle_cast({config_update, _Thresholds}, S) ->
    {noreply, S};

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

set_alarm(TS, FrameID, Value, Alarm, #st{alarms = As} = S) ->
    case Alarm of
	#alarm{status = sent} ->
	    S;
	_ ->
	    %% send alarm...
	    self() ! send_alarms,
	    NewAs = orddict:store(
		      FrameID, Alarm#alarm{status = set,
					   ts = TS,
					   value = Value}, As),
	    S#st{alarms = NewAs}
    end.

clear_alarm(TS, FrameID, Value, Alarm, #st{alarms = As} = S) ->
    case Alarm of
	#alarm{status = cleared} ->
	    S;
	_ ->
	    NewAs = orddict:store(FrameID, Alarm#alarm{status = cleared,
						       ts = TS,
						       value = Value}, As),
	    S#st{alarms = NewAs}
    end.

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

can_data_value(Len, Bin) ->
    <<Val:Len/integer-unit:8>> = Bin,
    Val.

