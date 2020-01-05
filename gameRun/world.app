%% This is the application resource file (.app file) for the 'base'
%% application.
{application, world,
[{description, "public server wrapper"},
 {vsn, "0.0.1"},
 {modules, [ world_app, world_sup, world_svr ]},
 {registered,[world]},
 {applications, [kernel,stdlib]},
 {mod, {world_app,[]}},
 {start_phases, []}
]}.