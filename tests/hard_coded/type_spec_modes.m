%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module type_spec_modes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

:- pred my_unify(T, T).
:- mode my_unify(in, in) is semidet.
:- mode my_unify(in, out) is det.
:- mode my_unify(out, in) is det.
:- pragma type_spec(my_unify(in, in), T = list(int)).
:- pragma type_spec(my_unify(in, in), T = int).
:- pragma type_spec(my_unify(in, out), T = int).
:- pragma type_spec(my_unify(out, in), T = list(int)).

:- pragma no_inline(my_unify/2).

my_unify(X, X).

main(!IO) :-
    ( if my_unify(1, 1) then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ),
    ( if my_unify([1, 2, 3], [1, 2, 6]) then
        io.write_string("no\n", !IO)
    else
        io.write_string("yes\n", !IO)
    ),
    my_unify(X, [1, 2, 3]),
    ( if X = [1, 2, 3] then
        io.write_string("yes\n", !IO)
    else
        io.write_string("no\n", !IO)
    ).
