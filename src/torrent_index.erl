%%
%% torrent_index.erl
%% Kevin Lynx
%% build text to torrent index in couchdb
%% require applications: ssl, public_key, jiffy, ibrowse, couchbeam
%% 06.15.2013
%%
-module(torrent_index).
-include("vlog.hrl").
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_link/2,
		 start_link/3,
		 start_link/4,
		 insert/3,
		 insert/4,
		 search/1,
		 inc_announce/1,
		 inc_announce/2,
		 top/0,
		 recent/0,
		 index/1,
		 count/0,
		 stop/0]).
-record(state, {mod = nil}).
-define(DBSTORE, db_store_mongo).
-define(CONN_COUNT, 2).
-define(QUERY_TIMEOUT, 10*1000).

start_link(Host, Port) ->
	start_link(Host, Port, nil).

start_link(Host, Port, Mod) ->
	start_link(Host, Port, Mod, ?CONN_COUNT).

start_link(Host, Port, Mod, Count) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [Host, Port, Mod, Count], []).

stop() ->
	gen_server:cast(srv_name(), stop).	

insert(Hash, Name, Length, Files) ->
	gen_server:cast(srv_name(), {insert, Hash, Name, Length, Files}).

insert(Hash, Name, Length) ->
	insert(Hash, Name, Length, []).

count() ->
	gen_server:call(srv_name(), {count}).

% {SearchList, Stats} 
search(Key) ->
	gen_server:call(srv_name(), {search, Key}, ?QUERY_TIMEOUT).

% [{single,Hash,{"movie  name",1024},1},
%  {multi,Hash,{"this movie update name",
%         	[{"file1",512},{"file2",128}]}, 5}]
top() ->
	gen_server:call(srv_name(), {top}, ?QUERY_TIMEOUT).

recent() ->
	gen_server:call(srv_name(), {recent}, ?QUERY_TIMEOUT).

index(MagHash) ->
	gen_server:call(srv_name(), {index, MagHash}).

%% if not `infinity` it may timeout (>5000ms), database busy?
inc_announce(Hash, Timeout) when is_list(Hash) ->
	gen_server:call(srv_name(), {inc_announce, Hash}, Timeout).

inc_announce(Hash) when is_list(Hash) ->
	inc_announce(Hash, infinity).

srv_name() ->
	torrent_index.

init([Host, Port, Mod, ConnCount]) ->
	db_conn_pool:start_link(Host, Port, ConnCount),
	DB = db_conn_pool:get(),
	if Mod /= nil -> Mod:handle_init(db_store_mongo:count(DB));
		true -> ok end,
	{ok, #state{mod = Mod}}.

handle_call({count}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:count(DB), State};

handle_call({search, Key}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:search(DB, Key), State};

handle_call({index, Hash}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:index(DB, Hash), State};

handle_call({top}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:search_announce_top(DB, 50), State};

handle_call({recent}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:search_recently(DB, 50), State};

handle_call({inc_announce, Hash}, _From, State) ->
	DB = db_conn_pool:get(),
	{reply, ?DBSTORE:inc_announce(DB, Hash), State};

handle_call(_, _From, State) ->
	{reply, not_implemented, State}.

handle_cast({insert, Hash, Name, Length, Files}, State) ->
	#state{mod = Mod} = State,
	DB = db_conn_pool:get(),
	try 
		Type = case ?DBSTORE:insert(DB, Hash, Name, Length, Files) of
			{new, _} -> new;
			{update, _} -> update
		end,
		case {Type, Mod} of
			{_, nil} -> ok;
			{new, _} -> Mod:handle_new();
			{update, _} -> Mod:handle_update()
		end
	catch
		throw:Error ->
			?E(?FMT("insert to db failed ~s ~s ~p", [binary_to_list(Hash), Name, Error]))
	end,
	{noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(_, State) ->
    {noreply, State}.

