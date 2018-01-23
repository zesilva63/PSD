-module(exchangeProducer).
-export([run/2, new_order/2]).

run(Host, Port) -> 
	{ok, Context} = erlzmq:context(1),
	{ok, Sock} = erlzmq:socket(Context, [push, {active, false}]),
	ok = erlzmq:connect(Sock, "tcp://" ++ Host ++ ":" ++ integer_to_list(Port)),
	spawn( fun() -> exchangeProducer(Sock) end).

new_order(Msg, Pid) ->
	Pid ! {order, Msg, self()},
	receive
		Rep -> Rep
	end.

exchangeProducer(Sock) ->
	receive
		{order, Msg, From} ->
			case erlzmq:send(Sock, Msg) of
				ok -> From ! ok;
				_ -> From ! error
			end
	end.
