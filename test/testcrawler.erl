%%
%% testcrawler.erl
%% Kevin Lynx
%% 06.15.2013
%%
-module(testcrawler).
-export([start/0, stop/0]).

start() ->
	crawler_sup:start_link(6776, 100, "localhost", 27017, 3).

stop() ->
	crawler_sup:stop().
		
