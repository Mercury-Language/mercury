%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_abstract.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module subtype_abstract_2.

:- type foo(T, U)
    --->    foo(
                my_field :: abstract_pair(T, U)
            ).

main(!IO) :-
    X = foo(make_abstract_pair(42, 3.14159)),
    io.print_line(X, !IO).
