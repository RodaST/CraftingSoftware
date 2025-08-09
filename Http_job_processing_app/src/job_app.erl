%% job_app

-module(job_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting Http job processing app~n"),
    job_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
