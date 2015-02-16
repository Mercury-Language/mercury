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

    :- module import_in_parent__sub.

    :- interface.

    :- pred foo(bool::in) is semidet.

    :- implementation.

    foo(X) :-
        bool__foo(X).

    :- end_module import_in_parent__sub.

:- end_module import_in_parent.
