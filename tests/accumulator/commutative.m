%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Implementation of a commutative predicate which is not associative,
% it should not have accumulators introduced.
%

:- module commutative.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type t
    --->    a
    ;       b
    ;       c.

main(!IO) :-
    io.write_string("p: ", !IO),
    p([a, b, c], R),
    io.write_line(R, !IO).

:- pred p(list(t)::in, t::out) is det.

p([], a).
p([H | T], R) :-
    p(T, R0),
    c(H, R0, R).

    % We declare c to be commutative, but not to be associative.
    %
:- pred c(t::in, t::in, t::out) is det.
:- promise all [A, B, C] ( c(A, B, C) <=> c(B, A, C) ).

c(a, a, a).
c(a, b, a).
c(a, c, c).
c(b, a, a).
c(b, b, b).
c(b, c, b).
c(c, a, c).
c(c, b, b).
c(c, c, c).
