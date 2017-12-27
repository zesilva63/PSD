-module(frontend).
-export([server/1]).

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]),
    acceptor(LSock).


acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    receive
        {tcp, Sock, Bin} ->
            protocol:decode_msg(Bin,'AuthenticationRequest');
    	{tcp_closed, _} ->
    		acceptor(LSock);
    	{tcp_error, _} ->
    		acceptor(LSock)
    end.

