%% job_worker

-module(job_worker).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("types.hrl").

-record(state, {
    id      :: job_id(),
    tasks   :: tasks_map(),
    manager :: pid(),
    order   :: [task_name()] | []
}).

-spec start_link(job_id(), tasks_map(), pid()) ->
          {ok, pid()} | {error, any()}.
start_link(Id, Tasks, ManagerPid) ->
    gen_server:start_link(?MODULE, {Id, Tasks, ManagerPid}, []).

-spec init({job_id(), tasks_map(), pid()}) ->
          {ok, #state{}}.
init({Id, Tasks, Manager}) ->
    self() ! start,
    {ok, #state{id=Id, tasks=Tasks, manager=Manager, order=[]}}.

handle_call(_,_,S) -> {reply,ok,S}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_,S) -> {noreply,S}.

-spec handle_info(term(), #state{}) ->
          {noreply, #state{}} | {stop, normal, #state{}}.
handle_info(start, State=#state{id=Id, tasks=Tasks, manager=Manager}) ->
    Res = run(Tasks),
    Manager ! {job_finished, Id, Res},
    {stop, normal, State};
handle_info(_,S) -> {noreply,S}.

%% internal functions

-spec run(tasks_map()) -> {ok, map()} | {error, any()}.
run(Tasks) ->
    case job_sort:job_sort(Tasks) of
        {error, Reason} ->
            {error, Reason};
        Order ->
            ExececuteResult = execute(Order, Tasks, []),
            case ExececuteResult of
                {ok, Steps}     -> {ok, #{status => success, tasks => Steps}};
                {error, Report} -> {error, Report}
            end
    end.

-spec execute([task_name()], tasks_map(), [map()]) ->
          {ok, [map()]} | {error, map()}.
execute([], _Tasks, Acc) ->
    {ok, lists:reverse(Acc)};
execute([Name | Rest], Tasks, Acc) ->
    Task = maps:get(Name, Tasks),
    Cmd  = maps:get(cmd, Task),
    case run_cmd(Cmd) of
        {ok, Out} ->
            Step = #{name => Name, command => Cmd, status => ok, output => Out},
            execute(Rest, Tasks, [Step | Acc]);
        {error, Err} ->
            {error, #{status => failed,
                      failed_task => Name,
                      command => Cmd,
                      reason => Err,
                      partial => lists:reverse(Acc)}}
    end.

-spec run_cmd(command()) -> {ok, binary()} | {error, any()}.
run_cmd(Cmd0) ->
    Cmd = to_list(Cmd0),
    {Family, _} = os:type(),
    case Family of
        win32 ->
            CmdExe = os:find_executable("cmd"),
            Exec   = case CmdExe of false -> "C:\\Windows\\System32\\cmd.exe"; P -> P end,
            Args   = ["/c", Cmd],
            Port   = open_port({spawn_executable, Exec},
                               [exit_status, use_stdio, binary, {args, Args}]),
            collect_port(Port, []);
        _Unix ->
            Sh    = "/bin/sh",
            Args  = ["-c", Cmd],
            Port  = open_port({spawn_executable, Sh},
                              [exit_status, use_stdio, binary, stderr_to_stdout, {args, Args}]),
            collect_port(Port, [])
    end.

-spec collect_port(port(), [binary()]) ->
          {ok, binary()} | {error, any()}.
collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}}     -> collect_port(Port, [Data | Acc]);
        {Port, {exit_status, 0}} -> {ok, iolist_to_binary(lists:reverse(Acc))};
        {Port, {exit_status, S}} -> {error, {exit_status, S}}
    after 60000 ->
        {error, timeout}
    end.

-spec to_list(term()) -> string().
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L)   -> L;
to_list(X)                   -> lists:flatten(io_lib:format("~p", [X])).
