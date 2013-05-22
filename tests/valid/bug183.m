% Regression test for bug #183.
%
% When compiled with --warn-dead-procs and one of the C backends (or indeed the
% Erlang backend), rotd-2013-05-21 and before emitted an incorrect warning
% about handle_event_excp/1 being dead.  This was because dead procedure
% elimination did not take foreign_export pragmas for languages other than
% those supported by the current backend into account when determining if that
% warning should be emitted.

:- module bug183.
:- interface.

:- type foo ---> foo.

:- implementation.

:- import_module univ.

:- impure pred handle_event_excp(string::in, string::in, univ::in) is det.
:- pragma foreign_export("C#", handle_event_excp(in, in, in),
    "SSDB_handle_event_excp").
:- pragma foreign_export("Java", handle_event_excp(in, in, in),
    "SSDB_handle_event_excp").

handle_event_excp(_, _, _) :-
	impure impure_true.
