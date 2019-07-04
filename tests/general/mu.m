%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test.
% Mercury 0.6 generated incorrect code for this test case.

:- module mu.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    ( if mu(N, [m, (.)], Charlist, N0) then
        string.from_char_list(Charlist, S),
        io.write_string(S, !IO),
        io.write_string("\n", !IO),
        write_pint(N, !IO),
        write_pint(N0, !IO)
    else
        io.write_string("Failed\n", !IO)
    ).

:- type pint
    --->    z
    ;       s(pint).

:- pred write_pint(pint::in, io::di, io::uo) is det.

write_pint(z, !IO) :-
    io.write_string("0\n", !IO).
write_pint(s(N), !IO) :-
    io.write_string("1+", !IO),
    write_pint(N, !IO).

:- pred mu(pint::out, list(char)::in, list(char)::out, pint::out) is nondet.

mu(z, ['.' | Charlist], Charlist, z) .
mu(s(z), [m | Charlist1], Charlist1, N0) :-
    % s(N)=s(z), % if this line is commented out --> SEGV
    mu(N0, Charlist1, _Charlist, _).
