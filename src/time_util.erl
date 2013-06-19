%%
%% time_util.erl
%% Kevin Lynx
%% 06.18.2013
%%
-module(time_util).
-compile(export_all).

now_seconds() ->
	{Megasecs, Secs, Microsecs} = now(),
	(Megasecs * 1000000) + Secs + (Microsecs div 1000000).

seconds_to_local_time(Secs) ->
	{{Y, M, D}, Time} = calendar:gregorian_seconds_to_datetime(Secs),
 	calendar:universal_time_to_local_time({{Y + 1970, M, D}, Time}).

 	