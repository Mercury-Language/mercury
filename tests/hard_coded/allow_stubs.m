% test case for the `--allow-stubs' option.

:- module allow_stubs.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception, univ.

main -->
	hello,
	trap_exceptions(how_are_you),
	trap_exceptions(going_today),
	goodbye.

hello --> print("hello world"), nl.

:- pred goodbye(io::di, io::uo) is det.
goodbye --> print("goodbye"), nl.

:- pred how_are_you(io::di, io::uo) is det.

:- mode going_today(di, uo) is det.
going_today -->
	print("going "),
	today.

:- pred today(io::di, io::uo) is det.

:- pred unused1(T::di, T::uo) is det.
:- pred unused2(T::di, T::uo) is det.
unused1(IO0, IO) :- unused2(IO0, IO).

:- impure pred imp(io::di, io::uo) is det.

:- mode trap_exceptions(pred(di, uo) is det, di, uo) is cc_multi.
trap_exceptions(IOGoal) -->
	try_io((pred({}::out, di, uo) is det --> IOGoal), Res),
	( { Res = succeeded({}) }
	; { Res = exception(Exception) },
	  print("[caught exception: "),
	  print(univ_value(Exception)),
	  print("]\n")
	).

