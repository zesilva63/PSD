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
            M = protocol:decode_msg(Bin,'Message'),
          	I = maps:get(user,M),
          	U = maps:get(username,I),
          	P = maps:get(password,I),
            
            case maps:get(type,M) of
            	"REGISTER" ->
          			LoginManager ! {create_account, U, P, self()},
          			receive
            			{_, created} ->
            				UserBin = protocol:encode_msg(#{type => "RESPONSE", user => #{}, request => #{}, response => #{ result => "OK", description => "You are now registered."}},'Message'),
            				gen_tcp:send(Sock,UserBin),
            		  		manager(Sock);
            			{_, user_exists} ->
            		  		UserBina = protocol:encode_msg(#{type => "RESPONSE", user => undefined, request => undefined, response => #{ result => "ERROR", description => "User is already registered."}},'Message'),
            				gen_tcp:send(Sock,UserBina),
            		  		waitLogin(Sock, LoginManager)
          			end;

          		"LOGIN" ->
          			LoginManager ! {login, U, P, self()},
          			receive
            			{_, logged} ->
            				UserBin = protocol:encode_msg(#{type => "RESPONSE", user => #{}, request => #{}, response => #{ result => "OK", description => "You are now logged in."}},'Message'),
            				gen_tcp:send(Sock,UserBin),
            		  		manager(Sock);
            			{_, invalid} ->
            		  		UserBina = protocol:encode_msg(#{type => "RESPONSE", user => undefined, request => undefined, response => #{ result => "ERROR", description => "Wrong username or password."}},'Message'),
            				gen_tcp:send(Sock,UserBina),
            		  		waitLogin(Sock, LoginManager)
          			end
          	end
    end.


manager(Sock) ->



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