%% types

-type task_name()  :: binary().
-type command()    :: binary() | string().
-type task()       :: #{cmd := command(),
                        deps := [task_name()]}.
-type tasks_map()  :: #{task_name() => task()}.

-type job_id()     :: pos_integer().
-type job_status() :: running | success | failed.
