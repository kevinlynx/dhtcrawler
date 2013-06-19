%% 
%% torrent_download.erl
%% Kevin Lynx
%% 06.12.2013
%% download torrent file by a magnet url
%% Dependent apps: inets, ssl
%% 
-module(torrent_download).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_link/0,
		 download/2,
		 download/1,
		 stop/0]).
-export([handle_torrent/3]).
-record(state, {reqs}).

start_link() ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [], []).

stop() ->
	gen_server:cast(srv_name(), stop).

download(MagHash, Mod) when is_list(MagHash) ->
	40 = length(MagHash),
	gen_server:call(srv_name(), {download, MagHash, Mod}).

download(MagHash) when is_list(MagHash) ->
	40 = length(MagHash),
	gen_server:call(srv_name(), {download, MagHash, ?MODULE}).

srv_name() ->
	torrent_download.

init([]) ->
	{ok, #state{reqs = gb_trees:empty()}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info({http, {ReqID, Result}}, State) ->
	NewReqs = case Result of
		{{_Version, 200, _R}, Headers, Body} ->
			handle_ok_response(State, ReqID, Headers, Body);
		{200, Body} ->
			handle_ok_response(State, ReqID, [], Body);
		{Status, _H, _B} ->
			?W(?FMT("http request ~p failed ~p", [ReqID, Status])),
			handle_next(State, ReqID);
		{Code, _B} ->
			?W(?FMT("http request (code) ~p failed ~p", [ReqID, Code])),
			handle_next(State, ReqID);
		Ex ->
			?E(?FMT("unhandled result type ~p", [Ex])),
			State#state.reqs
	end,
	{noreply, State#state{reqs = NewReqs}};

handle_info(_, State) ->
    {noreply, State}.

handle_call({download, MagHash, Mod}, _From, State) ->
	#state{reqs = Reqs} = State,
	NewReqs = create_download(Reqs, MagHash, Mod),
	{reply, ok, State#state{reqs = NewReqs}}.

create_download(Reqs, MagHash, Mod) when is_list(MagHash) ->
	40 = length(MagHash),
	U1 = "http://torrage.com/torrent/" ++ MagHash ++ ".torrent",
	U2 = "https://zoink.it/torrent/" ++ MagHash ++ ".torrent",
	U3 = format_btbox_url(MagHash),
	{ok, ReqID, NewURLs} = request_next([U1, U2, U3]),
	Req = {MagHash, NewURLs, Mod},
	NewReqs = gb_trees:insert(ReqID, Req, Reqs),
	NewReqs.

format_btbox_url(MagHash) ->
	H = lists:sublist(MagHash, 2),
	T = lists:nthtail(38, MagHash),
	"http://bt.box.n0808.com/" ++ H ++ "/" ++ T ++ "/" ++ MagHash ++ ".torrent".

request_next([URL|T]) ->
	HttpOptions = [{timeout, 5*60*100}],
	{ok, ReqID} = httpc:request(get, {URL, []}, HttpOptions, [{sync, false}]),
	?T(?FMT("start a http request ~p to ~s", [ReqID, URL])),
	{ok, ReqID, T};

request_next([]) ->
	{error, empty}.

handle_next(State, ReqID) ->
	#state{reqs = Reqs} = State,
	{MagHash, URLs, Mod} = gb_trees:get(ReqID, Reqs),
	NewReqs = case request_next(URLs) of
		{error, empty} ->
			?I(?FMT("download torrent failed for ~s", [MagHash])),
			Mod:handle_torrent(error, MagHash, nil),
			gb_trees:delete(ReqID, Reqs);
		{ok, NewID, NewURLs} ->
			IReqs = gb_trees:insert(NewID, {MagHash, NewURLs, Mod}, Reqs),
			gb_trees:delete(ReqID, IReqs)
	end,
	NewReqs.

handle_ok_response(State, ReqID, Headers, Body) 
when is_binary(Body), byte_size(Body) > 0 ->
	#state{reqs = Reqs} = State,
	{MagHash, _, Mod} = gb_trees:get(ReqID, Reqs),
	GZIP = is_gzip_body(Headers),
	B = Body,
	case unzip_content(B, GZIP) of
		error ->
			?W("get torrent response error"),
			NewReqs = gb_trees:delete(ReqID, Reqs),
			Mod:handle_torrent(error, MagHash, nil),
			NewReqs;
		TContent ->	
			?I(?FMT("download torrent file ~s success", [MagHash])),
			NewReqs = gb_trees:delete(ReqID, Reqs),
			Mod:handle_torrent(ok, MagHash, TContent),
			NewReqs
	end;

handle_ok_response(#state{reqs = Reqs}, ReqID, Headers, Body) ->
	?W(?FMT("http response data error headers ~p, body ~p ", [Headers, Body])),
	{MagHash, _, Mod} = gb_trees:get(ReqID, Reqs),
	NewReqs = gb_trees:delete(ReqID, Reqs),
	Mod:handle_torrent(error, MagHash, nil),
	NewReqs.

unzip_content(B, GZIP) ->
	if 
		GZIP -> 
			case catch(zlib:gunzip(B)) of
				{'EXIT', _} -> error;
				Res -> Res
			end;
		true -> B
	end.	

is_gzip_body(Headers) ->
	lists:member({"content-encoding", "gzip"}, Headers).

handle_torrent(ok, MagHash, TContent) ->
	io:format("download ~s success (~p byts)~n", [MagHash, byte_size(TContent)]),
	ok;

handle_torrent(error, MagHash, _TContent) ->
	io:format("download ~s failed~n", [MagHash]),
	ok.

