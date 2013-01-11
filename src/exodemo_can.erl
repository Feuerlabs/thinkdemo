-module(exodemo_can).
-behavior(gen_server).

-export([start_can/2]).

-export([start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(st, { iface=false, eff=false}).

start_can(Interface, ExtendedFrameFormatFlag) ->
    gen_server:cast(?MODULE, {start_can, Interface, ExtendedFrameFormatFlag}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    can_router:start(),
    {ok, #st{}}.

handle_cast(_, S) ->
    {noreply, S}.


handle_call({start_can, Interface, ExtendedFrameFormatFlag}, _From, #st { iface = OldInterface } = _St) ->
    case OldInterface of
	false ->
	    true;
	_ ->
	    can_sock:stop(OldInterface)
    end,

    can_sock:start(Interface),
    #st { iface = Interface, eff = ExtendedFrameFormatFlag };

handle_call(_Msg, _From, S) ->
    {reply, error, S}.


handle_info({can_frame, FrameID, DataLen, Data, _A, _B}, St) ->
    exodemo_log:log(timestamp(), FrameID, DataLen, Data),
    {noreply, St};

handle_info(_Msg,  S) ->
    {noreply, S}.


terminate(_Reason, _S) ->
    can_router:stop(),
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.

%% helper functions

%% FIXME ms since epoch
timestamp() ->
    DT = erlang:universaltime(),
    calendar:datetime_to_gregorian_seconds(DT).
