% Regression test.  The bodies of generated table reset and statistics were
% being written to .opt files but they refer to C variables only accessible
% from the defining module.

:- module reset_stats_intermod.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module reset_stats_intermod_2.

%-----------------------------------------------------------------------------%

main(!IO) :-
    reset(!IO),
    statistics(!IO),
    io.write_string("ok\n", !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
