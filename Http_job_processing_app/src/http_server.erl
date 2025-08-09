%% job_manager

-module(http_server).
-behaviour(application).
-export([start_link/0, start/0, stop/0]).

start_link() ->
    start().

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/jobs",          http_handler, []},
            {"/jobs/script",   http_handler, []} 
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    io:format("HTTP server listening on 8080~n"),
    {ok, self()}.

stop() ->
    cowboy:stop_listener(http_listener).