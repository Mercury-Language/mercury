%      Patricia Fasel
%      Los Alamos National Laboratory
%      1990 August
%      Translated from Haskell to Mercury by Peter Schachte, 2010-12-23

:- module pic.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module pic_calc.
:- import_module pic_type.
:- import_module consts.
:- import_module list, string.

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
	progname_base("", Progname, !IO),
	write_string("usage:  " ++ Progname ++ " num\n", !IO).

main(!IO) :-
	command_line_arguments(Cmdline, !IO),
	(
		Cmdline = [Numstr],
		to_int(Numstr, Num)
	->
		pic(Num, Result),
		write_float(Result, !IO),
		nl(!IO)
	;
		usage(!IO),
		set_exit_status(1, !IO)
	).
