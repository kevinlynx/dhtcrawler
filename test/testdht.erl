%%
%% testdht.erl
%% Kevin Lynx
%%
-module(testdht).
-include("vlog.hrl").
-export([start/0, stop/1, handle_event/2]).
-export([tell_more_nodes/1]).

start() ->
	vlog:start_link("kdht.txt", ?TRACE),
	random:seed(now()),
	kdht:start_link("dhtstate.dat", 6882, ?MODULE, dht_id:random()).

stop(Pid) ->
	kdht:stop(Pid).

handle_event(announce_peer, {InfoHash, _IP, _BTPort}) ->
	MagHash = dht_id:tohex(InfoHash),
	{ok, FP} = file:open("magnet-link.txt", [append]),
	io:format(FP, "~s~n", [MagHash]),
	file:close(FP);

handle_event(startup, {MyID}) ->
	spawn_link(?MODULE, tell_more_nodes, [MyID]).

tell_more_nodes(MyID) ->
	[search:get_peers(MyID, dht_id:random()) || _ <- lists:seq(1, 10)],
	?I("tell more nodes done").
