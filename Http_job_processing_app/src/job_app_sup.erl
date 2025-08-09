%% job_app_sup

-module(job_app_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 2,
                 period => 10},
    Childs = [
                {job_manager, {job_manager, start_link, []}, permanent, 5000, worker, [job_manager]},
                {http_server, {http_server, start_link, []}, permanent, 5000, worker, [http_server]}],
    {ok, {SupFlags, Childs}}.

%% internal functions
