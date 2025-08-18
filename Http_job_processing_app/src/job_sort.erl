%% job_sort

-module(job_sort).
-export([convert_json_to_map/1, job_sort/1]).

-include("types.hrl").

-spec convert_json_to_map([map()]) -> tasks_map().
convert_json_to_map(JsonList) ->
    maps:from_list(
        [ {maps:get(<<"name">>, Task),
           #{cmd => maps:get(<<"command">>, Task),
             deps => maps:get(<<"requires">>, Task, [])}}
         || Task <- JsonList ]
    ).

-spec job_sort(tasks_map()) ->
          [task_name()]
        | {error, {unknown_deps, [task_name()]}}
        | {error, {cycle, [task_name()]}}.
job_sort(Tasks) when is_map(Tasks) ->
    Names = lists:sort(maps:keys(Tasks)),  
    Unknown = [Dep || {_, #{deps := Deps}} <- maps:to_list(Tasks),
                    Dep <- Deps, not maps:is_key(Dep, Tasks)],
    case Unknown of
        [] -> sort(Tasks, Names, []);
        _  -> {error, {unknown_deps, lists:usort(Unknown)}}
    end.

-spec sort(tasks_map(), [task_name()], [task_name()]) ->
          [task_name()] | {error, {cycle, [task_name()]}}.
sort(_Tasks, [], Acc) ->
    lists:reverse(Acc);
sort(Tasks, Pending, Acc) ->
    case ready(Pending, Tasks, Acc) of
        {ok, Name, Rest} ->
            sort(Tasks, Rest, [Name | Acc]);
        error ->
            {error, {cycle, Pending}}
    end.

-spec ready([task_name()], tasks_map(), [task_name()]) ->
          {ok, task_name(), [task_name()]} | error.
ready([Name | Rest], Tasks, Acc) ->
    Deps = maps:get(deps, maps:get(Name, Tasks, #{}), []),
    case all_in(Deps, Acc) of
        true  -> {ok, Name, Rest};
        false ->
            case ready(Rest, Tasks, Acc) of
                {ok, NewName, NewRest} -> {ok, NewName, [Name | NewRest]};
                error        -> error
            end
    end;
ready([], _Tasks, _Acc) ->
    error.

-spec all_in([task_name()], [task_name()]) -> boolean().
all_in([], _Acc) -> true;
all_in([Deps | T], Acc) ->
    case lists:member(Deps, Acc) of
        true  -> all_in(T, Acc);
        false -> false
    end.