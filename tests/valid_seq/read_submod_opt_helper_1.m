%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module read_submod_opt_helper_1.

:- interface.

:- type foo.

%---------------------------------------------------------------------------%
    :- module read_submod_opt_helper_2.

    :- interface.

    :- import_module int.

    :- pred bar(int, foo).
    :- mode bar(in, out) is det.

    :- end_module read_submod_opt_helper_2.
%---------------------------------------------------------------------------%

:- implementation.

:- type foo
    --->    foo(int).

%---------------------------------------------------------------------------%
    :- module read_submod_opt_helper_2.

    :- implementation.

    bar(X, foo(Y)) :-
        Y = X + 1.

    :- end_module read_submod_opt_helper_2.
%---------------------------------------------------------------------------%
