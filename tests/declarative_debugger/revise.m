%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module revise.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    p("foo", S),
    io.write_string(S, !IO),
    io.nl(!IO).

:- pred p(string::in, string::out) is multi.
:- pred q(string::in, string::out) is det.
:- pred r(string::in, string::out) is multi.
:- pred s(string::in, string::out) is det.
:- pred a(string::in, string::out) is det.
:- pred b(string::in, string::out) is det.
:- pred c(string::in, string::out) is det.

p --> q, r, s.

q --> [].
r --> a, b.
r --> c.
s --> [].

a --> [].
b --> [].
c --> :=("bar").

