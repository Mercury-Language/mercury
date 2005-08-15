:- module poly_io_retry2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, std_util.

main(!IO) :-
	io_set_globals(univ(3), !IO),
	list_foldl2(test1, [0, 1, 2], 0, _, !IO),
	nl(!IO).

:- pred test1(int::in, int::in, int::out, io::di, io::uo) is det.

test1(X, Z, Y + X + Z, !IO) :-
	io_get_globals(U, !IO),
	( if univ_to_type(U, Y0) then
		Y = Y0
	else
		Y = 1
	),
	io_set_globals(univ(Y + X + Z), !IO).

:- pred list_foldl2(pred(L, A, A, Z, Z), list(L), A, A, Z, Z).
:- mode list_foldl2(pred(in, in, out, di, uo) is det,
	in, in, out, di, uo) is det.

list_foldl2(_, [], !A, !B).
list_foldl2(P, [H | T], !A, !B) :-
	call(P, H, !A, !B),
	list_foldl2(P, T, !A, !B).

:- pred io_get_globals(univ::out, io::di, io::uo) is det.

:- pred io_set_globals(univ::in, io::di, io::uo) is det.

:- pragma foreign_decl("C", "
	static MR_Word	poly_io_retry_test_globals;
").

:- pragma foreign_proc("C",
	io_get_globals(Globals::out, IOState0::di, IOState::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Globals = poly_io_retry_test_globals;
	MR_update_io(IOState0, IOState);
").

:- pragma foreign_proc("C",
	io_set_globals(Globals::in, IOState0::di, IOState::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	poly_io_retry_test_globals = Globals;
	MR_update_io(IOState0, IOState);
").
