%%
%% crawler_app.erl
%% Kevin Lynx
%% 06.19.2013
%%
-module(crawler_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	File = "priv/dhtcrawler.config",
	io:format("load config file ~s ", [File]),
	case file:consult(File) of
		{error, _Reason} ->
			do_default_start(File);
		{ok, [Cfg]} ->
			do_start(Cfg)
	end.

stop(_State) ->
	crawler_sup:stop().

do_default_start(File) ->
	List = [{start_port, 6776},
		{node_count, 10},
		{loglevel, 3},
		{dbhost, "localhost"},
		{dbport, 27071}],
	file:write_file(File, io_lib:fwrite("~p.\n",[List])),
	do_start(List).

do_start(List) ->
	StartPort = proplists:get_value(start_port, List),
	Count = proplists:get_value(node_count, List),
	LogLevel = proplists:get_value(loglevel, List),
	DBHost = proplists:get_value(dbhost, List),
	DBPort = proplists:get_value(dbport, List),
	io:format("dhtcrawler startup ~p, ~p, ~p:~p", [StartPort, Count, DBHost, DBPort]),
	crawler_sup:start_link(StartPort, Count, DBHost, DBPort, LogLevel).




