-module(exodemo_can).
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
	  read_speed = undefined,  %% Vehicle Speed, kph, as read from CAN bus
	  read_soc = undefined,    %% State of charge. Percent, as read from CAN bus
	  read_keypos = undefined,  %% Ignition key position off, reverse, drive1, drive2.
	  sent_speed = undefined,  %% Vehicle Speed, as last sent to the server
	  sent_soc = undefined,    %% State of charge, as last sent to the server
	  sent_keypos = undefined  %% Ignition key position, as last sent to the server
	 }).

-define(SPEED_ID, 1).  %% Can frame id to report to Exosense Server.
-define(SOC_ID, 2).  %% Can frame id to report to Exosense Server.
-define(KEYPOS_ID, 2).  %% Can frame id to report to Exosense Server.

start_can() ->
    io:format("exodemo_can:start_can()~n", []),
    gen_server:call(?MODULE, {start_can, "can0", "can_mcp2515_drv"}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    io:format("exodemo_can:init()~n"),
    can_router:start(),
    {ok, #st{}}.

handle_cast(_, S) ->
    {noreply, S}.


handle_call({start_can, Interface, Driver}, _From, #st { iface = OldInterface } = _St) ->
    io:format("exodemo_can:handle_call(start_can, ~p, ~p, ~p)~n", [Interface, Driver, OldInterface]),
    case OldInterface of
	undefined ->
	    true;
	_ ->
	    can_sock:stop(OldInterface)
    end,
    io:format("exodemo_can:handle_call(start_can, ~p, ~p): Starting router~n", [Interface, Driver]),


    can_sock:start(Interface, Driver, []),
    can_router:attach(),
    {reply, ok, #st { iface = Interface }};

handle_call(Msg, From, S) ->
    io:format("exodemo_can:handle_call(~p, ~p, ~p)~n", [Msg, From, S]),
    {reply, error, S}.


handle_info({can_frame, FrameID, DataLen, Data, _A, _B}, St) ->
    io:format("exodemo_can:handle_info(can_frame, ~p, ~p)~n", [FrameID, DataLen]),
    %% Dissect the mofo can frame
    NSt = case FrameID of
	      1027 ->
		  <<Speed:8, StateOfCharge:8, _/binary>> = Data,
		  set_read_state(Speed, StateOfCharge, undefined, St);
	      1040 ->
		  set_read_state(undefined, undefined, drive1, St);
	      _->
		  St
	  end,

    %% Ugly, for now.
    exodemo_alarms:check_alarm(?SPEED_ID, 1, NSt#st.read_speed),
    exodemo_alarms:check_alarm(?SOC_ID, 1, NSt#st.read_soc),
    exodemo_log:log_can(?SPEED_ID, 1, NSt#st.read_speed),
    exodemo_log:log_can(?SOC_ID, 1, NSt#st.read_soc),
    exodemo_log:log_can(?KEYPOS_ID, 1, NSt#st.read_keypos),
    io:format("exodemo_can:handle_info(State)~p ~n", [NSt]),

    {noreply, St};

handle_info(Msg,  S) ->
    io:format("exodemo_can:handle_info(?? ~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions
set_read_state(Speed, StateOfCharge, KeyPos, State) ->
    io:format("set_read_state(~p, ~p, ~p, ~p)~n", [ Speed, StateOfCharge, KeyPos, State]),
    #st { iface = OIface,
	  read_soc=OStateOfCharge,
	  read_speed = OSpeed,
	  read_keypos=OKeyPos } = State,

    NSpeed = case Speed of
	undefined -> OSpeed;
		 _ -> Speed
	     end,

    NKeyPos = case KeyPos of
	undefined -> OKeyPos;
		 _ -> KeyPos
	     end,


    NStateOfCharge = case StateOfCharge of
	undefined -> OStateOfCharge;
		 _ -> StateOfCharge
	     end,




    #st { iface = OIface,
	  read_speed = NSpeed,
	  read_soc= NStateOfCharge,
	  read_keypos = NKeyPos,
	  sent_speed = State#st.sent_speed,
	  sent_soc= State#st.sent_speed,
	  sent_keypos = State#st.sent_speed
	}.

set_sent_state(Speed, StateOfCharge, KeyPos, State) ->
    #st { iface = OIface,
	  sent_soc=OStateOfCharge,
	  sent_speed = OSpeed,
	  sent_keypos=OKeyPos } = State,

    if Speed =/= OSpeed ->
	    NSpeed = Speed;
       true ->
	    NSpeed = OSpeed
    end,

    if KeyPos =/= OKeyPos ->
	    NKeyPos = KeyPos;
       true ->
	    NKeyPos = OKeyPos
    end,

    if StateOfCharge =/= OStateOfCharge ->
	    NStateOfCharge = StateOfCharge;
       true ->
	    NStateOfCharge = OStateOfCharge
    end,

    #st {
      iface = OIface,
      read_speed = State#st.read_speed,
      read_keypos = State#st.read_keypos,
      read_soc = State#st.read_soc,
      sent_speed = NSpeed,
      sent_soc = NStateOfCharge,
      sent_keypos = NKeyPos
     }.

