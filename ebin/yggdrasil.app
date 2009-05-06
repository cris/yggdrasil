{application, yggdrasil,
 [{description, "Yggdrasil"},
  {vsn, "0.0.1"},
  {modules, [
	  yggdrasil,
	  yggdrasil_sup,
	  yggdrasil_listener,
	  yggdrasil_receiver,
          yggdrasil_json_protocol,
          yggresource_world,
          yggresource_space_sup,
          yggresource_space,
          yggresource_actor_sup,
          yggresource_actor,
          mochijson2
      ]},
  {registered, [
	  yggdrasil,
	  yggdrasil_sup,
	  yggdrasil_listener,
          yggresource_world
      ]},
  {applications, [kernel, stdlib, sasl]},
  {env, []},
  {mod, {yggdrasil, []}}]}.

%% vim: set filetype=erlang tabstop=8:

