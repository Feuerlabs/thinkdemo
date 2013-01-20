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

-record(st, { iface=false }).

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
    io:format("exodemo_can:handle_call(start_can, ~p, ~p)~n", [Interface, Driver]),
    case OldInterface of
	false ->
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
%%    io:format("exodemo_can:handle_info(can_frame, ~p, ~p)~n", [FrameID, DataLen]),
    exodemo_log:log_can(FrameID, DataLen, Data),
    exodemo_alarms:check_alarm(FrameID, DataLen, Data),
    {noreply, St};

handle_info(Msg,  S) ->
    io:format("exodemo_can:handle_info(~p, ~p)~n", [Msg, S]),
    {noreply, S}.

terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions

