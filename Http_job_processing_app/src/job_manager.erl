-module(job_manager).
-behaviour(gen_server).
-export([start_link/0, run/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(job, {id,
              tasks,
              status,
              pid,
              result,
              waiter}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run(TasksMap) ->
    gen_server:call(?MODULE, {run, TasksMap}, infinity).

init([]) ->
    {ok, #{jobs => #{}, next_id => 1}}.

%%gen_server

handle_call({run, Tasks}, From, State) ->
    Id = maps:get(next_id, State),
    Pid = job_worker:start_link(Id, Tasks, self()),
    Job = #job{id=Id, tasks=Tasks, status=running, pid=Pid, result=undefined, waiter=From},
    Jobs = maps:put(Id, Job, maps:get(jobs, State)),
    {noreply, State#{jobs => Jobs, next_id => Id+1}};

handle_call(_,_,S) -> {reply, ok, S}.

handle_cast(_,S) -> {noreply,S}.

handle_info({job_finished, Id, Result}, State) ->
    Jobs = maps:get(jobs, State),
    case maps:get(Id, Jobs, undefined) of
        undefined ->
            {noreply, State};

        #job{} = Job ->
            {Status1, ResultBody} =
                case Result of
                    {ok, Report0}    -> {success, Report0};
                    {error, Reason0} -> {failed, #{status => failed, reason => Reason0}}
                end,

            Job2    = Job#job{status = Status1, result = ResultBody},
            NewJobs = maps:put(Id, Job2, Jobs),

            _ =
                case Job#job.waiter of
                    undefined -> ok;
                    From      -> gen_server:reply(From, Result)
                end,

            {noreply, State#{jobs => NewJobs}}
    end.
