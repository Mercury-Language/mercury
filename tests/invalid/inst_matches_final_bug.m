:- module inst_matches_final_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- inst two_to_six == bound(2;3;4;5;6).

main -->
	call_with_two_to_six(return_three).

:- func return_three = int.
:- mode return_three = out(bound(3)) is det.

return_three = return_three_2.

:- func return_three_2 = int.

return_three_2 = 1.

:- pred call_with_two_to_six(int::in(two_to_six),
		io__state::di, io__state::uo) is det.
:- pragma no_inline(call_with_two_to_six/3).

call_with_two_to_six(2) -->
	io__write_string("Got two\n").
call_with_two_to_six(3) -->
	io__write_string("Got three\n").
call_with_two_to_six(4) -->
	io__write_string("Got four\n").
call_with_two_to_six(5) -->
	io__write_string("Got five\n").
call_with_two_to_six(6) -->
	io__write_string("Got six\n").
