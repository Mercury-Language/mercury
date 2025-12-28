%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trans_intermod_user_eq.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module trans_intermod_user_eq_helper_1.

main(!IO) :-
    make_bar(0, 1, Bar),
    use_bar(Bar, N),
    io.write_int(N, !IO),
    io.nl(!IO).
