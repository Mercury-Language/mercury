%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module cc_nondet_disj.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module list.

main(!IO) :-
    io.read_line(Res, !IO),
    ( Res = ok(['y' | _]), Message = "Yes\n"
    ; Res = ok(['n' | _]), Message = "No\n"
    ; Message = "Huh?\n"
    ),
    io.write_string(Message, !IO).

/***
% This test used to be written as follows, but currently
% the unique mode analysis is not smart enough to realize
% that the disjuncts which update the I/O state won't
% backtrack over I/O if the code is written like that.
main --> io.read_line(Res),
    ( { Res = ok(['y' | _]) }, io.write_string("Yes\n")
    ; { Res = ok(['n' | _]) }, io.write_string("No\n")
    ; io.write_string("Huh?\n")
    ).
***/
