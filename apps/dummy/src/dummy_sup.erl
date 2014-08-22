-module(dummy_sup).
-behaviour(supervisor).
-export([ start_link/0
        , init/1
        ]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DummyDB = ?CHILD(dummy_db, worker),
    {ok, {{one_for_one, 5, 10}, [DummyDB]}}.
