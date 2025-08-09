%% job_sort

-module(job_sort).
-export([convert_json_to_map/1, job_sort/1]).

convert_json_to_map(JsonList) ->
    maps:from_list(
        [ {maps:get(<<"name">>, Task),
           #{cmd => maps:get(<<"command">>, Task),
             deps => maps:get(<<"requires">>, Task, [])}}
         || Task <- JsonList ]
    ).

job_sort(Tasks) when is_map(Tasks) ->
    Names = lists:sort(maps:keys(Tasks)),  
    Unknown = [Dep || {_, #{deps := Deps}} <- maps:to_list(Tasks),
                    Dep <- Deps, not maps:is_key(Dep, Tasks)],
    case Unknown of
        [] -> sort(Tasks, Names, []);
        _  -> {error, {unknown_deps, lists:usort(Unknown)}}
    end.

sort(_Tasks, [], Acc) ->
    lists:reverse(Acc);
sort(Tasks, Pending, Acc) ->
    case ready(Pending, Tasks, Acc) of
        {ok, Name, Rest} ->
            sort(Tasks, Rest, [Name | Acc]);
        error ->
            {error, {cycle, Pending}}
    end.

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

all_in([], _Acc) -> true;
all_in([Deps | T], Acc) ->
    case lists:member(Deps, Acc) of
        true  -> all_in(T, Acc);
        false -> false
    end.