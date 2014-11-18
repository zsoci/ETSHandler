{application, ETSApp,
 [{description,"Application for testing ever living ETS tables"},
  {vsn, "1.0"},
  {modules, [etsapp_app,etsapp_sup]},
  {registered, [etsapp_app]},
  {applications,[kernel,stdlib]},
  {mod,{etsapp_app,[]}},
  {start_phases,[]}
 ]
}.

