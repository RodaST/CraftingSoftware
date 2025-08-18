%% http_handler

-module(http_handler).
-behaviour(cowboy_handler).
-export([init/2]).

-type json_map() :: map().

-spec init(cowboy_req:req(), any()) ->
          {ok, cowboy_req:req(), any()}.
init(Req0, _State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),
    case {Method, Path} of
        {<<"POST">>, <<"/jobs">>}        -> handle_post_jobs(Req0);
        {<<"POST">>, <<"/jobs/script">>} -> handle_post_script(Req0);
        _ -> reply_json(404, #{error => <<"Not Found">>}, Req0)
    end.

-spec handle_post_jobs(cowboy_req:req()) ->
          {ok, cowboy_req:req(), any()}.
handle_post_jobs(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case decode(Body) of
        {error, invalid_json} ->
            reply_json(400, #{error => <<"invalid json">>}, Req1);
        Json ->
            JsonTasks = maps:get(<<"tasks">>, Json, []),
            MapTasks  = job_sort:convert_json_to_map(JsonTasks),
            case job_manager:run(MapTasks) of
                {ok, Report} ->
                    reply_json(200, Report, Req1);
                {error, {unknown_deps, Unknown}} ->
                    reply_json(400, #{error => <<"unknown deps">>, details => Unknown}, Req1);
                {error, {cycle, Pending}} ->
                    reply_json(400, #{error => <<"deps error">>, pending => Pending}, Req1);
                {error, Other} ->
                    reply_json(400, #{error => <<"execution failed">>, details => Other}, Req1)
            end
    end.

-spec handle_post_script(cowboy_req:req()) ->
          {ok, cowboy_req:req(), any()}.
handle_post_script(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case decode(Body) of
        {error, invalid_json} ->
            reply_json(400, #{error => <<"invalid json">>}, Req1);
        Json ->
            JsonTasks = maps:get(<<"tasks">>, Json, []),
            MapTasks  = job_sort:convert_json_to_map(JsonTasks),
            case script_gen:from_tasks(MapTasks) of
                {ok, ScriptBin} ->
                    reply_text(200, ScriptBin, Req1);
                {error, {unknown_deps, Unknown}} ->
                    reply_json(400, #{error => <<"unknown deps">>, details => Unknown}, Req1);
                {error, {cycle, Pending}} ->
                    reply_json(400, #{error => <<"deps error">>, pending => Pending}, Req1);
                {error, Other} ->
                    reply_json(400, #{error => <<"execution failed">>, details => Other}, Req1)
            end
    end.

%% internal functions

-spec decode(binary()) -> {error, invalid_json} | json_map().
decode(Body) ->
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} -> {error, invalid_json};
        Term        -> Term
    end.

-spec reply_json(non_neg_integer(), term(), cowboy_req:req()) ->
          {ok, cowboy_req:req(), any()}.
reply_json(Code, JsonData, Req0) ->
    Body = jsx:encode(JsonData),
    Req1 = cowboy_req:reply(Code,
        #{
          <<"content-type">>   => <<"application/json">>,
          <<"content-length">> => integer_to_binary(byte_size(Body))
        },
        Body, Req0),
    {ok, Req1, #{}}.

-spec reply_text(non_neg_integer(), iodata(), cowboy_req:req()) ->
          {ok, cowboy_req:req(), any()}.
reply_text(Code, BodyBin, Req0) ->
    Req1 = cowboy_req:reply(Code,
        #{
          <<"content-type">>   => <<"text/plain">>,
          <<"content-length">> => integer_to_binary(byte_size(BodyBin))
        },
        BodyBin, Req0),
    {ok, Req1, #{}}.
