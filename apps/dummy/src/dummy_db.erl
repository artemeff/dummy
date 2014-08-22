-module(dummy_db).
-behaviour(gen_server).
-export([ start_link/0
        , get/1
        , set/2
        , version/0
        % gen_server callbacks
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(db, {vsn = 1, store = []}).

%%
%% API
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

version() ->
    gen_server:call(?MODULE, version).

%%
%% GenServer callbacks
%%

init(_) ->
    {ok, #db{store = []}}.

handle_call({get, Key}, _From, State) ->
    Reply = case proplists:get_value(Key, State#db.store) of
        undefined -> not_found;
        Value -> {ok, Value}
    end,
    {reply, Reply, State};

handle_call({set, Key, Value}, _From, State) ->
    Store = [{Key, Value} | State#db.store],
    {reply, {ok, Key, Value}, State#db{store = Store}};

handle_call(version, _From, State) ->
    {reply, State#db.vsn, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
