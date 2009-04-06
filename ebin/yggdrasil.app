{application, odin,
 [{description, "Yggdrasil"},
  {vsn, "0.0.1"},
  {modules, [
	  yggdrasil,
	  yggdrasil_listener,
	  yggdrasil_sup
      ]},
  {registered, [
	  yggdrasil_sup,
	  yggdrasil_listener
      ]},
  {applications, [kernel, stdlib, sasl, mnesia, os_mon]},
  {env, []},
  {mod, {yggdrasil, []}}]}.

%% vim: set filetype=erlang tabstop=8:

