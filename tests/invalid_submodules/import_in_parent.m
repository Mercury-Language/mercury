%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module import_in_parent.

:- interface.

:- import_module bool.

:- type foo.

:- implementation.

    :- type foo
        --->    foo.

    :- module import_in_parent.sub.

    :- interface.

    :- pred foo(bool::in) is semidet.

    :- implementation.

    foo(X) :-
        bool.foo(X).

    :- end_module import_in_parent.sub.

:- end_module import_in_parent.
