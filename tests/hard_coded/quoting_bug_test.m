%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module quoting_bug_test.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module list.
:- import_module quoting_bug_test_helper_1.

main(!IO) :-
    write_token(*, !IO),  io.nl(!IO),
    write_token(&&, !IO), io.nl(!IO),
    write_token(-=, !IO), io.nl(!IO),
    write_token(+=, !IO), io.nl(!IO),
    write_token(?, !IO),  io.nl(!IO),
    test([*, &&, -=, +=, ?], !IO).

:- pred write_token(token::in, state::di, state::uo) is det.

write_token(T, !IO) :-
    io.write(T, !IO).

:- pred test(list(token)::in, state::di, state::uo) is det.

test(List, !IO) :-
    io.write_list(List, " ", write_token, !IO),
    io.nl(!IO).

