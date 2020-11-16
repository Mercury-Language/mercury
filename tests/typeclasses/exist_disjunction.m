%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exist_disjunction.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t
    --->    a(int)
    ;       b(int).

:- type u
    --->    some [T] (u(T) => v(T))
    % --->  some [T] u(T)
    ;       f.

:- typeclass v(T) where [].

:- type w
    --->    f(int).

:- instance v(w) where [].

:- pred p(t::in, u::out) is det.

p(T, V) :-
    (
        T = a(C),
        V = 'new u'(f(C))
    ;
        T = b(C),
        V = 'new u'(f(C))
    ).

main(!IO) :-
    p(a(42), X),
    io.write_line(X, !IO).
