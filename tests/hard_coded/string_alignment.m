%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests for possible problems that unaligned string literals
% would cause if tagged.
%

:- module string_alignment.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module require.

:- type t
    --->    f1(string)
    ;       f2(string)
    ;       f3(string)
    ;       f4(string).

main -->
    show(f1("foo")),
    show(f2("foo")),
    show(f1("oo")),
    show(f2("oo")).

:- pred show(t::in, io::di, io::uo) is det.

show(f1(S), !IO) :-
    io.write_string("f1: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).
show(f2(S), !IO) :-
    io.write_string("f2: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).
show(f3(S), !IO) :-
    io.write_string("f3: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).
show(f4(S), !IO) :-
    io.write_string("f4: ", !IO),
    io.write_string(S, !IO),
    io.nl(!IO).

