%% http_handler

-module(script_gen).
-export([from_tasks/1]).

-include("types.hrl").

-spec from_tasks(tasks_map()) ->
          {ok, binary()}
        | {error, {unknown_deps, [task_name()]}}
        | {error, {cycle, [task_name()]}}
        | {error, any()}.
from_tasks(Tasks) when is_map(Tasks) ->
    case job_sort:job_sort(Tasks) of
        {error, Reason} ->
            {error, Reason};
        Order ->
            Lines =
                [<<"#!/usr/bin/env bash">>,
                 <<"">>]
                ++ [to_bin(maps:get(cmd, maps:get(Name, Tasks))) || Name <- Order],
            {ok, iolist_to_binary(join_nl(Lines))}
    end.

-spec to_bin(term()) -> binary().
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L)   -> list_to_binary(L);
to_bin(X)                   -> list_to_binary(io_lib:format("~p", [X])).

-spec join_nl([binary()]) -> iodata().
join_nl([])      -> [];
join_nl([X])     -> [X, <<"\n">>];
join_nl([X|Xs])  -> [X, <<"\n">> | join_nl(Xs)].
