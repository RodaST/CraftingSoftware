Http_job_processing_app
=====

An OTP application

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell

Test1 - [cmd execution]
  (other shell) run in win power shell
-----
$resp = Invoke-RestMethod -Uri "http://localhost:8080/jobs" `
  -Method POST -ContentType "application/json" `
  -Body '{
    "tasks": [
      {"name":"task-1","command":"echo Creating file...","requires":[]},
      {"name":"task-2","command":"echo Hello World! > testfile.txt","requires":["task-1"]},
      {"name":"task-3","command":"type testfile.txt","requires":["task-2"]},
      {"name":"task-4","command":"del testfile.txt","requires":["task-3"]}
    ]
  }'

$resp | ConvertTo-Json -Depth 10

Test2 - [bash script]
  (other shell) run in win power shell
----- 
Invoke-WebRequest -Uri "http://localhost:8080/jobs/script" `
  -Method POST -ContentType "application/json" `
  -Body '{
    "tasks": [
      { "name": "task-1", "command": "touch /tmp/file1", "requires": [] },
      { "name": "task-2", "command": "echo \"Hello World!\" > /tmp/file1", "requires": ["task-1"] },
      { "name": "task-3", "command": "cat /tmp/file1", "requires": ["task-2"] },
      { "name": "task-4", "command": "rm /tmp/file1", "requires": ["task-3"] }
    ]
  }' | Select-Object -ExpandProperty Content
