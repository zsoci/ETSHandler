{application, multiserver,
 [{description,"Application for testing ETSServer"},
  {vsn, "1.0"},
  {modules, [multiserver_app,multiserver_sup]},
  {registered, [multiserver_app]},
  {applications,[kernel,stdlib]},
  {mod,{multiserver_app,[]}},
  {start_phases,[]}
 ]
}.

