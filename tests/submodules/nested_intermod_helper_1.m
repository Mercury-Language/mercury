%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nested_intermod_helper_1.

:- interface.

:- import_module int.

:- pred foo(int).
:- mode foo(in) is semidet.

    :- module sub.

    :- interface.

    :- pred fu(int).
    :- mode fu(in) is semidet.
    :- end_module sub.

:- implementation.

    :- module sub.
    :- implementation.

    fu(X) :-
        X < 4.

    :- end_module sub.

:- pragma inline(foo/1).

foo(X) :-
    bar(X).

:- pred bar(int).
:- mode bar(in) is semidet.

bar(X) :-
    X > 3.

:- end_module nested_intermod_helper_1.
