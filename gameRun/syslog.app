%% This is the application resource file (.app file) for the 'base'
%% application.
{application, syslog,
[{description, "syslog wrapper"},
 {vsn, "0.0.1"},
 {modules, [ syslog_app, syslog_sup, syslog_svr ]},
 {registered,[syslog_sup, syslog_svr]},
 {applications, [kernel,stdlib]},
 {mod, {syslog_app,[]}},
 {start_phases, []}
]}.

