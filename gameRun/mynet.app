%% This is the application resource file (.app file) for the 'base'
%% application.
{application, mynet,
[{description, "net wrapper"},
 {vsn, "0.0.1"},
 {modules, [ mynet_app, mynet_sup, mynet_svr ]},
 {registered,[mynet]},
 {applications, [kernel,stdlib]},
 {mod, {mynet_app,[]}},
 {start_phases, []}
]}.

