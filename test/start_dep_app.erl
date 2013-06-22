%%
%% start_dep_app.erl
%% Kevin Lynx
%% 06.15.2013
%%
-module(start_dep_app).
-export([startcouchdb/0, startmongo/0]).

%% Damn, couchbeam requires jiffy, ibrowse, public_key, sasl, crypto.
startcouchdb() ->
	code:add_path("e:/prj/ibrowse/ebin"),   
	code:add_path("e:/prj/couchbeam/ebin"),   
	code:add_path("e:/prj/jiffy/ebin"),   
	Apps = [crypto, public_key, ssl, sasl, inets, jiffy, ibrowse, couchbeam],
	[application:start(App) || App <- Apps].

startmongo() ->
	code:add_path("deps/bson/ebin"),
	code:add_path("deps/mongodb/ebin"),
	code:add_path("deps/kdht/ebin"),
	Apps = [crypto, public_key, ssl, inets, bson, mongodb],	
	[application:start(App) || App <- Apps].

