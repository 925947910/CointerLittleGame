%% This is the application resource file (.app file) for the 'base'
%% application.
{application, scene,
[{description, "scene wrapper"},
 {vsn, "0.0.1"},
 {modules, [scene_app, scene_sup, scene_svr ]},
 {registered,[scene]},
 {applications, [kernel,stdlib]},
 {mod, {scene_app,[]}},
 {start_phases, []}
]}.

