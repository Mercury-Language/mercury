%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inst_matches_final_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- inst two_to_six == bound(2;3;4;5;6).

main(!IO) :-
    call_with_two_to_six(return_three, !IO).

:- func return_three = int.
:- mode return_three = out(bound(3)) is det.

return_three = return_three_2.

:- func return_three_2 = int.

return_three_2 = 1.

:- pred call_with_two_to_six(int::in(two_to_six), io::di, io::uo) is det.
:- pragma no_inline(call_with_two_to_six/3).

call_with_two_to_six(2, !IO) :-
    io.write_string("Got two\n", !IO).
call_with_two_to_six(3, !IO) :-
    io.write_string("Got three\n", !IO).
call_with_two_to_six(4, !IO) :-
    io.write_string("Got four\n", !IO).
call_with_two_to_six(5, !IO) :-
    io.write_string("Got five\n", !IO).
call_with_two_to_six(6, !IO) :-
    io.write_string("Got six\n", !IO).
