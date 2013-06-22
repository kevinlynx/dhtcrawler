%%
%% db_conn_pool.erl
%% Kevin Lynx
%% 06.21.2013
%%
-module(db_conn_pool).
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_link/3,
		 stop/0,
		 get/0]).
-record(state, {pos, pool = []}).

start_link(Host, Port, Count) ->
	gen_server:start_link({local, srv_name()}, ?MODULE, [Host, Port, Count], []).

stop() ->
	gen_server:cast(srv_name(), stop).

% get one connection in the pool
get() ->
	gen_server:call(srv_name(), get_one).

srv_name() ->
	db_conn_pool.

init([Host, Port, Count]) ->
	{ok, {Host, Port, Count}, 0}.

handle_call(get_one, _From, State) ->
	#state{pool = Pool, pos = Pos} = State,
	Conn = lists:nth(Pos, Pool),
	NewPos = case Pos == length(Pool) of
		true -> 1;
		false -> Pos + 1
	end,
	{reply, Conn, State#state{pos = NewPos}};

handle_call(_, _From, State) ->
	{reply, not_implemented, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
	case State of
		{_, _, _} -> ok;
		_ ->
			#state{pool = Pool} = State,
			close_conn(Pool)
	end,
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_info(timeout, State) ->
	NewState = init_conn(State),
	{noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

init_conn({Host, Port, Count}) ->
	Pool = [db_store_mongo:init(Host, Port) || _ <- lists:seq(1, Count)],
	#state{pos = 1, pool = Pool}.

close_conn(Pool) ->
	[db_store_mongo:close(Conn) || Conn <- Pool].

