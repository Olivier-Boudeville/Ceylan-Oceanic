% Description of the Oceanic OTP active application, typically used by rebar3.

% Note: if this file is named oceanic.app, it is a *generated* file, whose real
% source is conf/oceanic.app.src, from which _build/lib/oceanic/ebin/oceanic.app
% is obtained and copied to ebin/oceanic.app; finally src/oceanic.app.src is a
% mere symlink to this last file, so we have:
%
% ./conf/oceanic.app.src [only real source]
% ./_build/lib/oceanic/ebin/oceanic.app
% ./ebin/oceanic.app
% ./src/oceanic.app.src -> ../ebin/oceanic.app
%
% For more information see the Ceylan-Myriad 'create-app-file' make
% target and its associated comments.


% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, oceanic,
 [{description, "Ceylan-Oceanic, a library for the support of the Enocean building automation system, as an OTP application library here (see http://oceanic.esperide.org)"},
  {vsn, "1.4.4"},

  % No process registered:
  {registered, []},

  % Regarding Myriad, see http://myriad.esperide.org/myriad.html#otp:
  % (however myriad is a library application, not an active one)
  {applications, [kernel, stdlib, myriad]},

  %{env,[]},

  % Flat hierarchy in ebin here:
  {modules, [oceanic, oceanic_constants, send_telegram_app, telegram_decoding, telegram_sending]},

  {licenses, ["Ceylan-Oceanic is licensed by its author (Olivier Boudeville) under a disjunctive tri-license, giving you the choice of one of the three following sets of free software/open source licensing terms:
	- the Mozilla Public License (MPL), version 1.1 or later (very close to the former Erlang Public License, except aspects regarding Ericsson and/or the Swedish law)
	- the GNU General Public License (GPL), version 3.0 or later
	- the GNU Lesser General Public License (LGPL), version 3.0 or later"]},

  % Library application, not an active one, so no specific behaviour of its own:
  % {mod, {oceanic_app,[]}}

  { links, [ {"Official website", "http://oceanic.esperide.org" },
			 {"Github", "https://github.com/Olivier-Boudeville/Ceylan-Oceanic"} ]}

  %{exclude_files, []}

 ]}.
