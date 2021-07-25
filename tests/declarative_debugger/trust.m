%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trust.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module trust_1.
:- import_module trust_2.

main(!IO) :-
    dostuff(w(S), R),
    write_string(S, !IO),
    io.nl(!IO),
    io.write_line(R, !IO).

:- pred dostuff(w::out, comparison_result::uo) is cc_multi.

dostuff(W, R) :-
    compare(R, w("aaB"), w("aAB")),
    concat(w("aaa"), w("bbb"), W).
