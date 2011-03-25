-module(silo_server).
-behaviour(gen_server).
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
	start_link/0,
	create_entry/3,
	update_entry/3,
	remove_entry/1,
	fetch_entry/3
]).

-include_lib("xmerl/include/xmerl.hrl").

-record(state, {entries}).

 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create_entry(atom, xml, EntryBinary) ->  gen_server:call(?MODULE, {entry, create, {atom, xml}, EntryBinary})

create_entry(atom, json, EntryBinary) -> unsupported_format_error(atom, json);

create_entry(EntryType, EntryFormat, EntryBinary) -> unsupported_format_error(EntryType, EntryFormat).


update_entry(atom, xml, EntryBinary) ->  gen_server:call(?MODULE, {entry, update, {atom, xml}, EntryBinary});

update_entry(atom, json, EntryBinary) -> unsupported_format_error(atom, json);

update_entry(EntryType, EntryFormat, EntryBinary) -> unsupported_format_error(EntryType, EntryFormat).


remove_entry(EntryId) ->  gen_server:call(?MODULE, {entry, remove, EntryId});


fetch_entry(atom, xml, EntryId) ->  gen_server:call(?MODULE, {entry, fetch, {atom, xml}, EntryId});

fetch_entry(atom, json, EntryId) -> unsupported_format_error(atom, json);

fetch_entry(EntryType, EntryFormat, EntryBinary) -> unsupported_format_error(EntryType, EntryFormat).
  
init([]) ->
       PrivDir = code:priv_dir(silo),
       EntriesFile = PrivDir ++ "/entries.dets",
       EntriesTab = dets:open_file(EntriesFile, [{type, set}]),
       Tab = ets:new(?MODULE, []),
       {ok, #state{entries=EntriesTab}}.

handle_call({entry, create, {atom, xml}, Entry}, _From, State) ->
       Reply = case entry_id(Entry) of
       	       	    {EntryId, EntryNode} = KVPair -> dets:insert_new(State#state.entries, KVPair), {ok, EntryId};
		    _ -> ill_formed_entry_error()
		end;
       {reply, Reply, State};

handle_call({entry, update, {atom, xml}, Entry}, _From, State) ->
       Reply = case entry_id(Entry) of
       	       	    {EntryId, EntryNode} = KVPair -> dets:insert(State#state.entries, KVPair), {ok, EntryId};
		    _ -> ill_formed_entry_error()
		end;
       {reply, Reply, State};

handle_call({entry, remove, EntryId}, _From, State) ->
       Reply = dets:delete(State#state.entries, EntryId),
       {reply, Reply, State};

handle_call({entry, fetch, {atom, xml}, EntryId}, _From, State) ->
       Reply = case dets:lookup(State#state.entries, EntryId) of 
               	    [EntryNode] ->
		    	ExportList = xmerl:export_simple_content([EntryNode], xmerl_xml),
			Entry = lists:flatten(ExportList),
			{ok, EntryId, {atom, xml}, Entry};
	    	    [] -> not_found_error(EntryId)
       	        end;
       {reply, Reply, State};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, State) ->  dets:close(State#state.entries), ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

ill_formed_entry_error() ->  {error, ill_formed_entry}.

unsupported_format_error(Type, Format) ->  {error, unsupported_format, {Type, Format}}. 

not_found_error(EntryId) -> {error, not_found, {entry, EntryId}}. 

entry_id(atom, xml, Entry) ->
       { [Node], _Rest } = xmerl_scan:string(Entry),
       [ #xmlText{value=Id} ] = xmerl_xpath:string("id/text()", Node),
       {Id, Node}.
       