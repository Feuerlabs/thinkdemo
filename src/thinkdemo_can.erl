-module(thinkdemo_can).
-behavior(gen_server).

-export([start_can/0]).


-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, {
	  iface = undefined,  %% CAN Interface
	  speed = undefined,  %% Vehicle Speed, as last sent to the server
	  soc = undefined,    %% State of charge, as last sent to the server
	  keypos = undefined  %% Ignition key position, as last sent to the server
	 }).

-define(SPEED_ID, 1024).  %% Can frame id to report to Exosense Server.
-define(SOC_ID, 1025).  %% Can frame id to report to Exosense Server.
-define(KEYPOS_ID, 1026).  %% Can frame id to report to Exosense Server.

start_can() ->
    io:format("thinkdemo_can:start_can()~n", []),
    gen_server:call(?MODULE, {start_can, "can0"}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    io:format("thinkdemo_can:init()~n"),
    can_router:start(),
    {ok, #st{}}.

handle_cast(_, S) ->
    {noreply, S}.


handle_call({start_can, Interface}, _From, #st { iface = OldInterface } = _St) ->
    io:format("thinkdemo_can:handle_call(start_can, ~p, ~p)~n", [Interface, OldInterface]),
    case OldInterface of
	undefined ->
	    true;
	_ ->
	    can_sock:stop(OldInterface)
    end,
    io:format("thinkdemo_can:handle_call(start_can, ~p): Starting router~n", [Interface]),

    can_sock:start(Interface),
    can_router:attach(),
    {reply, ok, #st { iface = Interface }};

handle_call(Msg, From, S) ->
    io:format("thinkdemo_can:handle_call(~p, ~p, ~p)~n", [Msg, From, S]),
    {reply, error, S}.


handle_info({can_frame, FrameID, _DataLen, Data, _A, _B}, St) ->
    %% Dissect the mofo can frame
    NSt = case FrameID of
	     16#200 ->
		  <<StateOfCharge:8, Speed:8, _:3, _Charger:1, KeyState:4,_/binary>> = Data,
		  report_to_logger(Speed, StateOfCharge, KeyState, St);
	      _->
		  St
	  end,

    { noreply, NSt };


handle_info(Msg,  S) ->
    io:format("thinkdemo_can:handle_info(?? ~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

report_to_logger(Speed, StateOfCharge, KeyPos, State) ->
    #st { iface = OIface,
	  soc = OStateOfCharge,
	  speed = OSpeed,
	  keypos = OKeyPos } = State,

    NSpeed = case Speed of
	undefined -> OSpeed;
		 _ -> Speed
	     end,

    if NSpeed =/= OSpeed, NSpeed =/= undefined ->
	    io:format("Sending speed ~p~n", [ NSpeed ]),
	    thinkdemo_log:log_can(?SPEED_ID, 1, NSpeed),
	    thinkdemo_alarms:check_alarm(?SPEED_ID, 1, NSpeed);
       true ->
	    true
    end,

    NKeyPos = case KeyPos of
	undefined -> OKeyPos;
		 _ -> KeyPos
	     end,

    if NKeyPos =/= OKeyPos, NKeyPos =/= undefined ->
	    io:format("Sending  keypos ~p~n", [ NKeyPos ]),
	    thinkdemo_log:log_can(?KEYPOS_ID, 1, NKeyPos),
	    thinkdemo_alarms:check_alarm(?KEYPOS_ID, 1, NKeyPos);
       true ->
	    true
    end,

    NStateOfCharge = case StateOfCharge of
	undefined -> OStateOfCharge;
		 _ -> StateOfCharge
	     end,

    if NStateOfCharge =/= OStateOfCharge, NStateOfCharge =/= undefined ->
	    io:format("Sending  soc ~p~n", [ NStateOfCharge ]),
	    thinkdemo_log:log_can(?SOC_ID, 1, NStateOfCharge),
	    thinkdemo_alarms:check_alarm(?SOC_ID, 1, NStateOfCharge);
       true ->
	    true
    end,

    #st { iface = OIface,
	  speed = NSpeed,
	  soc = NStateOfCharge,
	  keypos = NKeyPos
	}.

