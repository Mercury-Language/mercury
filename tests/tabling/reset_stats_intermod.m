%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. The bodies of generated table reset and statistics were
% being written to .opt files but they refer to C variables only accessible
% from the defining module.

:- module reset_stats_intermod.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module reset_stats_intermod_helper_1.

:- pragma require_feature_set([memo]).

main(!IO) :-
    reset(!IO),
    statistics(!IO),
    io.write_string("ok\n", !IO).

