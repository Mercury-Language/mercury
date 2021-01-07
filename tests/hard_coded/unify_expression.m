%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unify_expression.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module std_util.

:- type t
    --->    f(int, int)
    ;       g(t).

main(!IO) :-
    ( if p(g(f(1, 2)), X) then
        io.write_line(X, !IO)
    else
        io.write_string("Error: p failed\n", !IO)
    ),
    ( if q(1, 2) then
        io.print_line("Error: q succeeded", !IO)
    else
        io.print_line("q failed (as expected)", !IO)
    ),

    ( if r(1, 2) then
        io.print_line("Error: r succeeded", !IO)
    else
        io.print_line("r failed (as expected)", !IO)
    ).

:- pred p(t::in, t::out) is semidet.

p(X @ f(_, _), X).
p(g(X @ f(_, _)), X).

:- pred q(int::in, int::in) is semidet.
q(X, X @ g(_, _)).

:- pred r(int::in, int::in) is semidet.
r(X, X @ g(1, 2)).

:- func g(int, int) = int.
:- mode g(in, in) = out is semidet.
:- mode g(out, out) = in is semidet.
g(1, 2) = X :-
    ( if semidet_succeed then
        error("g called")
    else
        X = 3
    ).
