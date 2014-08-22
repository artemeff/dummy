-module(dummy).
-export([start/0]).

start() ->
    application:start(dummy).
