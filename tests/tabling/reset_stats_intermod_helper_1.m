%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reset_stats_intermod_helper_1.
:- interface.

:- import_module int.
:- import_module io.

:- func plus1(int) = int.
:- pred reset(io::di, io::uo) is det.
:- pred statistics(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pragma require_feature_set([memo]).

:- pragma memo(plus1/1, [allow_reset, statistics]).

plus1(X) = X + 1.

reset(!IO) :-
    table_reset_for_plus1_1(!IO).

statistics(!IO) :-
    table_statistics_for_plus1_1(_Stats, !IO).
