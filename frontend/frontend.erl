-module(frontend).
-export([server/1]).
-record('ClientData',{username, password}).
-record('Request',{company, quantity, price}).
-record('Response',{result, description}).
-record('Message',{type, user, request, response}).


server(Port) ->
	LoginManager = spawn(fun()-> loginManager(#{}) end),
    {ok, LSock} = gen_tcp:listen(Port, [binary,{packet, 0}, {reuseaddr, true}, {active, true}]),
    acceptor(LSock, LoginManager).


acceptor(LSock, LoginManager) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, LoginManager) end),
    waitLogin(Sock, LoginManager).


waitLogin(Sock, LoginManager) ->
    receive
        {tcp, Sock, Bin} ->
            protocol:decode_msg(Bin,'Message'),
            UserBin = protocol:encode_msg(#{type => "RESPONSE", user => undefined, request => undefined, response => #{ result => "OK", description => "You are now registered!"} }),
            gen_tcp:send(Sock,UserBin),
            waitLogin(Sock, LoginManager)
    end.


loginManager(M) ->
  	receive
    	{create_account, U, P, From} ->
      		case maps:find(U, M) of
        		error ->
          			From ! {?MODULE, created},
          			loginManager(maps:put(U, {P, false}, M));
        		_ -> 
          			From ! {?MODULE, user_exists},
          			loginManager(M)
      		end;
    	{{close_account, U, P}, From} ->
      		case maps:find(U, M) of
        		{ok,{P, _}} ->
          			From ! {?MODULE, ok},
          			loginManager(maps:remove(U, M));
        		_ -> 
          			From ! {?MODULE, invalid},
          			loginManager(M)
      		end;
    	{login, U, P, From} ->
      		case maps:find(U, M) of
        		{ok,{P, false}} ->
          			From ! {?MODULE, logged},
          			loginManager(maps:update(U,{P,true}, M));
        		_ -> 
          			From ! {?MODULE, invalid},
          			loginManager(M)
      		end;
    	{{logout, U}, From} -> 
      		From ! {?MODULE, ok},
      		{P,_} = maps:get(U, M),
      		loginManager(maps:update(U, {P,false}, M))
    end.