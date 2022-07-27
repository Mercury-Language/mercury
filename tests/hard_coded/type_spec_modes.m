%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_spec_modes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.

:- pred my_unify(T, T).
:- mode my_unify(in, in) is semidet.
:- mode my_unify(in, out) is det.
:- mode my_unify(out, in) is det.

:- pragma no_inline(my_unify/2).
:- pragma type_spec(my_unify(in, in), T = list(int)).
:- pragma type_spec(my_unify(in, in), T = int).
:- pragma type_spec(my_unify(in, out), T = int).
:- pragma type_spec(my_unify(out, in), T = list(int)).

my_unify(X, X).

:- func is_ascending(T, list(T)) = bool.
:- mode is_ascending(in, in) = out.

:- pragma type_spec(is_ascending(in, in) = out, T = int).

is_ascending(A, Bs) = IsAscending :-
    (
        Bs = [],
        IsAscending = yes
    ;
        Bs = [B | Cs],
        compare(CmpRes, A, B),
        (
            CmpRes = (<),
            IsAscending = is_ascending(B, Cs)
        ;
            ( CmpRes = (=)
            ; CmpRes = (>)
            ),
            IsAscending = no
        )
    ).

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
    ),
    IsAscending123 = is_ascending(1, [2, 3]),
    io.write_line(IsAscending123, !IO),
    IsAscending133 = is_ascending(1, [3, 3]),
    io.write_line(IsAscending133, !IO),
    IsAscending213 = is_ascending(2, [1, 3]),
    io.write_line(IsAscending213, !IO),
    IsAscendingABC = is_ascending("a", ["b", "c"]),
    io.write_line(IsAscendingABC, !IO),
    IsAscendingACC = is_ascending("a", ["c", "c"]),
    io.write_line(IsAscendingACC, !IO),
    IsAscendingBAC = is_ascending("b", ["a", "c"]),
    io.write_line(IsAscendingBAC, !IO).
