-module(exchangeConsumer).
-export([run/2]).

run(Host, Port) -> 
	{ok, Context} = erlzmq:context(),
	{ok, Sock} = erlzmq:socket(Context, [pull, {active, false}]),
	ok = erlzmq:connect(Sock, "tcp://" ++ Host ++ ":" ++ integer_to_list(Port)),
	register(?MODULE, spawn ( fun() -> exchangeConsumer(Sock) end) ).

exchangeConsumer(Sock) ->
	case erlzmq:recv(Sock) of
		{ok, Bin} ->
			Msg = protocol:decode_msg(Bin, "Message"),
			Cli = maps:get(user, Msg),
			User = maps:get(username, Cli),
			{ok, Pid} = loginManager:user_pid(User),
			userSession:send(Msg, User),
			exchangeConsumer(Sock);
		{error, _} -> 
			exchangeConsumer(Sock)
	end.
