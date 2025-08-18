%% job_app_sup

-module(job_app_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/jobs",        http_handler, []},
            {"/jobs/script", http_handler, []}
        ]}
    ]),

    HttpChild =
        ranch:child_spec(http_listener, ranch_tcp,
            #{
              num_acceptors => 50,
              socket_opts   => [{port, 8080}]
            },
            cowboy_clear, #{
              env => #{dispatch => Dispatch}
            }),

    JobManagerChild =
        #{ id       => job_manager
         , start    => {job_manager, start_link, []}
         , restart  => permanent
         , shutdown => 5000
         , type     => worker
         , modules  => [job_manager]
         },

    {ok, {{one_for_one, 5, 10}, [JobManagerChild, HttpChild]}}.

