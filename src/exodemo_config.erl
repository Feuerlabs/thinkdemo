-module(exodemo_config).

-export(['update-config-entry-request'/1]).

-include_lib("lager/include/log.hrl").

root() ->
    <<"exodemo">>.

status(complete) -> 1;
status(_) -> 5. % device-error


'update-config-entry-request'(Elems) ->
    ?debug("update-config-entry-request(~p)~n", [Elems]),
    TransactionID = 1,
    {notify, "demo:update-config-entry-callback",
     [{'transaction-id', TransactionID},
      {'status-code', status(complete)},
      {'final', true}]}.
