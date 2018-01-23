-module(loginManager).
-export([run/0, create_account/2, close_account/2, login/2, logout/1, user_pid/1]).

run() -> 
	register(?MODULE, spawn ( fun() -> loginManager(#{}) end) ).

create_account(User, Pass) ->
	?MODULE ! {create_account, User, Pass, self()},
	receiveReply().

close_account(User, Pass) ->
	?MODULE ! {close_account, User, Pass, self()},
	receiveReply().

login(User, Pass) ->
	?MODULE ! {login, User, Pass, self()},
	receiveReply().

logout(User) ->
	?MODULE ! {logout, User, self()},
	receiveReply().

% Replys error | disconnected | {ok, Pid} 
user_pid(User) ->
	?MODULE ! {user_pid, User, self()},
	receiveReply().

loginManager(Map) ->
  	receive
    	{create_account, User, Pass, From} ->
      		case maps:find(User, Map) of
        		error ->
          			From ! {?MODULE, created},
					io:format("New user ~p\n", [User]),
          			loginManager(maps:put(User, {Pass, false}, Map));
        		_ -> 
          			From ! {?MODULE, user_exists},
          			loginManager(Map)
      		end;

    	{close_account, User, Pass, From} ->
      		case maps:find(User, Map) of
        		{ok, {Pass, _}} ->
          			From ! {?MODULE, ok},
          			loginManager(maps:remove(User, Map));
        		_ -> 
          			From ! {?MODULE, invalid},
          			loginManager(Map)
      		end;

    	{login, User, Pass, From} ->
      		case maps:find(User, Map) of
        		{ok, {Pass, false}} ->
          			From ! {?MODULE, logged},
          			loginManager(maps:update(User, {Pass, From}, Map));
        		_ -> 
          			From ! {?MODULE, invalid},
          			loginManager(Map)
      		end;

    	{logout, User, From} -> 
      		From ! {?MODULE, ok},
      		{Pass, _} = maps:get(User, Map),
      		loginManager(maps:update(User, {Pass, false}, Map));

		{user_pid, User, From} ->
			case maps:find(User, Map) of
				{ok, {_, false}} ->
					io:format("User ~p is not online\n", [User]),
					From ! {?MODULE, disconnected};
				{ok, {_, Pid}} ->
					io:format("User ~p found!\n", [User]),
					From ! {?MODULE, {ok, Pid}};
				_ ->
					io:format("User ~p not found in ~p!\n", [User, Map]),
					From ! {?MODULE, error}
			end,
			loginManager(Map)
    end.

receiveReply() ->
	receive
		{?MODULE, Reply} -> 
			Reply
	end.
