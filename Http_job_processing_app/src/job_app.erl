%% job_app

-module(job_app).
-behaviour(application).
-export([start/2, stop/1]).

-spec start(any(), any()) -> {ok, pid()} | {error, any()}.
start(_Type, _Args) ->
    io:format("Starting Http job processing app~n"),
    job_app_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
