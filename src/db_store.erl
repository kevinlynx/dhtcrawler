%%
%% db_store.erl
%% Kevin Lynx
%% 06.15.2013
%%
-module(db_store).
-export([init/2,
	     insert/5,
	     build_substr_index/1,
	     search_from_index/2,
	     search/2]).
-compile(export_all).
-define(DBNAME, "torrents").
-define(SEARCH_DESIGN, "searchname").
-define(SEARCH_DOCID, list_to_binary("_design/"++?SEARCH_DESIGN)).
-define(SEARCH_VIEW, "search").

-define(INDEX_DESIGN, "indexname").
-define(INDEX_DOCID, list_to_binary("_design/"++?INDEX_DESIGN)).
-define(INDEX_VIEW, "index").

create_search_view(Key) ->
	Formater = "function(doc) { if(doc.name && doc.name.match(\"~s\")) { emit(doc.name, doc);}}",
	Content = lists:flatten(io_lib:format(Formater, [Key])),
	{[{list_to_binary(?SEARCH_VIEW),
		{[{<<"map">>, list_to_binary(Content)}]}
	}]}.
		
create_index_view() ->
	Content = 
		"function(doc) {
			var i;
			if (doc.name) 
				for (i = 0; i < doc.name.length; i+=1) {
					emit(doc.name.slice(i), doc);
					if (doc.name.charCodeAt(i) >= 128) i += 1;
				}
		}",
	{[{list_to_binary(?INDEX_VIEW),
		{[{<<"map">>, list_to_binary(Content)}]}
	}]}.

create_search_doc(Db, Key) ->
	Id = ?SEARCH_DOCID,
	Doc = {[
		{<<"_id">>, Id}, {<<"language">>, <<"javascript">>},
		{<<"views">>, create_search_view(Key)}
	]},
	couchbeam:save_doc(Db, Doc).

create_index_doc(Db) ->
	Id = ?INDEX_DOCID,
	Doc = {[
		{<<"_id">>, Id}, {<<"language">>, <<"javascript">>},
		{<<"views">>, create_index_view()}
	]},
	couchbeam:save_doc(Db, Doc).

update_or_create_search_doc(Db, Key) ->
	Id = ?SEARCH_DOCID,
	case couchbeam:open_doc(Db, Id) of
		{ok, Doc} ->
			NDoc = couchbeam_doc:set_value("views", create_search_view(Key), Doc),
			couchbeam:save_doc(Db, NDoc);
		_ ->
			create_search_doc(Db, Key)
	end.

check_create_index_doc(Db) ->
	Id = ?INDEX_DOCID,
	case couchbeam:open_doc(Db, Id) of
		{ok, _} -> ok;
		_ ->
			create_index_doc(Db)
	end.

init(Host, Port) ->
	S = connect(Host, Port),
	{ok, Db} = couchbeam:open_or_create_db(S, ?DBNAME),
	check_create_index_doc(Db),
	Db.

stop(_Db) -> % NOTE: not implemented
	ok.

% [{single,Hash, {"movie  name",1024},1},
%  {multi,Hash, {"this movie update name",
%         	[{"file1",512},{"file2",128}]}, 5}]
search(Db, Key) ->
	update_or_create_search_doc(Db, Key),
	DesignName = ?SEARCH_DESIGN,
	ViewName = ?SEARCH_VIEW,
	case couchbeam_view:fetch(Db, {DesignName, ViewName}) of
		{ok, []} ->
			[];
		{ok, Results} ->
			decode_search_rets(Results)
	end.

search_from_index(Db, Key) ->
	DesignName = ?INDEX_DESIGN,
	ViewName = ?INDEX_VIEW,
	Options = [{limit, 100}, {start_key, list_to_binary(Key)}],
	case couchbeam_view:fetch(Db, {DesignName, ViewName}, Options) of
		{ok, []} ->
			[];
		{ok, Results} ->
			decode_search_rets(Results)
	end.

% Files: [{Name, Length}, {Name, Length}] or []
insert(Db, Hash, Name, Length, Files) when is_binary(Hash) ->
	case couchbeam:open_doc(Db, Hash) of
		{ok, Doc} ->
			update_torrent(Db, Hash, Name, Length, Doc, Files);
		_ ->
			DocS = create_torrent_desc(Hash, Name, Length, 1, Files),
			couchbeam:save_doc(Db, DocS)
	end.

build_substr_index(Db) ->
	DesignName = ?INDEX_DESIGN,
	ViewName = ?INDEX_VIEW,
	Options = [{limit, 0}],
	couchbeam_view:fetch(Db, {DesignName, ViewName}, Options),
	ok.	

connect(Host, Port) ->
	Prefix = [],
	Options = [],
	couchbeam:server_connection(Host, Port, Prefix, Options).

create_torrent_desc(Hash, Name, Length, Announce, Files) ->
	{[{<<"_id">>, Hash},
	  {<<"name">>, list_to_binary(Name)},
	  {<<"length">>, Length},
	  {<<"announce">>, Announce}
	] ++
	if 
		length(Files) > 0 -> [{<<"files">>, encode_file_list(Files)}];
		true -> []
	end}.

encode_file_list(Files) ->
	Keys = ["file"++integer_to_list(Index) || Index <- lists:seq(1, length(Files))],
	Generator = lists:zip(Keys, Files),
	{[{list_to_binary(Key), {[{<<"name">>, list_to_binary(Name)}, {<<"length">>, Length}]}}
		|| {Key, {Name, Length}} <- Generator]}.

update_torrent(Db, Hash, Name, Length, Doc, Files) ->
	Announce = couchbeam_doc:get_value("announce", Doc) + 1,
	{InnerDocS} = create_torrent_desc(Hash, Name, Length, Announce, Files),
	Rev = couchbeam_doc:get_rev(Doc),
	NewDoc = {InnerDocS ++ [{<<"_rev">>, Rev}]},
	couchbeam:save_doc(Db, NewDoc).

decode_search_rets(Results) ->
	[decode_doc(RetDoc) || {RetDoc} <- Results].

decode_doc(RetDoc) when is_list(RetDoc) ->
	{Doc} = proplists:get_value(<<"value">>, RetDoc),
	Name = binary_to_list(proplists:get_value(<<"name">>, Doc)),
	Hash = binary_to_list(proplists:get_value(<<"_id">>, Doc)),
	Length = proplists:get_value(<<"length">>, Doc),
	Announce = proplists:get_value(<<"announce">>, Doc),
	case proplists:is_defined(<<"files">>, Doc) of
		true ->
			Files = decode_files(proplists:get_value(<<"files">>, Doc)),
			{multi, Hash, {Name, Files}, Announce};
		false ->
			{single, Hash, {Name, Length}, Announce}
	end.

decode_files({FilesDoc}) ->
	[{binary_to_list(Name), Length} || {_Index, {[{_, Name}, {_, Length}]}} <- FilesDoc].


