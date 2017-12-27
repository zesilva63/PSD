-module(frontend).
-export([server/1]).
-record('ClientData',{username,password}).
-record('Response',{result,description}).
-record('Message',{type,clientData,response}).


server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]),
    acceptor(LSock).


acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    login_manager(Sock).

login_manager(Sock) ->    
    receive
        {tcp, Sock, Bin} ->
            protocol:decode_msg(Bin,'Message'),
            UserBin = protocol:encode_msg(#'Message'{type = "RESPONSE",clientData = undefined, response = #'Response'{result = "OK", description = "You are now registered!"}}),
            gen_tcp:send(Sock,UserBin),
            login_manager(Sock)
    end.
