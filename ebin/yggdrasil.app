{application, yggdrasil,
 [{description, "Yggdrasil"},
  {vsn, "0.0.1"},
  {modules, [
	  yggdrasil,
	  yggdrasil_sup,
	  yggdrasil_listener,
	  yggdrasil_receiver
      ]},
  {registered, [
	  yggdrasil,
	  yggdrasil_sup,
	  yggdrasil_listener
      ]},
  {applications, [kernel, stdlib, sasl]},
  {env, []},
  {mod, {yggdrasil, []}}]}.

%% vim: set filetype=erlang tabstop=8:

