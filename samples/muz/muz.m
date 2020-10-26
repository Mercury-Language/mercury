%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: muz.cpp
% main author: philip

:- module muz.
%:- pragma source_file("muz.cpp").

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module word, ztoken_io, zparser, zabstract,
	string, ztype, typecheck, pair, maybe, list, dict,
	getopt, require, bool.



:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(string::in, option::out, option_data::out) is multi.

:- type option
	--->	abbreviate
	;	debug
	;	help
	;	toolkit

	.
	
short_option('a', abbreviate).
short_option('d', debug).
short_option('?', help).
short_option('h', help).
short_option('p', toolkit).		% used by fuzz: 'p' for prelude
short_option('t', toolkit).


long_option("abbreviate", abbreviate).
long_option("debug", debug).
long_option("help", help).
long_option("prelude", toolkit).	% cf. short option 'p' used by fuzz
long_option("toolkit", toolkit).


option_defaults(_, abbreviate, bool(bool.yes)).
option_defaults(_, debug, bool(bool.no)).
option_defaults(_, help, bool(bool.no)).
option_defaults(T, toolkit, maybe_string(yes(T))).



:- func get_flags(option_table(option)) = flags.
get_flags(Option_Table) = F :-
	F0 = defaults,
	getopt__lookup_bool_option(Option_Table, debug, Debug),
	( Debug = yes -> set_debugging_on(F0, F1) ; F1 = F0 ),

	Generate = no,

	( Generate = yes -> set_generating_logic(on, F1, F2) ; F2 = F1 ),
	getopt__lookup_maybe_string_option(Option_Table, toolkit, Toolkit),
	set_toolkit(Toolkit, F2, F).

main -->
	io__set_exit_status(0),
	io__get_environment_var("MUZ_TOOLKIT", MT),
	{ MT = no, DT = "/usr/local/apps/muz/lib/toolkit.tex"
	; MT = yes(DT)
	},
	{Option_Ops=
		option_ops_multi(short_option, long_option, option_defaults(DT))
	},
	io__command_line_arguments(AL0),
	{getopt__process_options(Option_Ops, AL0, AL, Maybe_Option_Table)},
	( {Maybe_Option_Table = error(Message)},
		zmessage("muz", [option_error_to_string(Message)]),
		usage
	; {Maybe_Option_Table = ok(Option_Table)},
		( {getopt__lookup_bool_option(Option_Table, help, yes)} ->
			usage
		; {AL = []} ->
			zmessage("muz",
				["Filename expected (for stdin use -)"]),
			usage
		;	main(Option_Table, AL),
			io__get_exit_status(Status),
			io__stderr_stream(StdErr),
			( {Status = 0} ->
				io__write_string(StdErr,"No errors detected.\n")
			;	io__write_string(StdErr,"Errors detected.\n")
			)
		)
	).

:- pred main(option_table(option), list(string), io__state, io__state).
:- mode main(in, in, di, uo) is det.
main(Option_Table, AL) -->
	{Flags0 = get_flags(Option_Table),
	P0 = (pred(I::in, IF::out) is det :- IF = I - Flags0),
	list__map(P0, AL, AL1),
	MToolkit = toolkit(Flags0),
	( MToolkit = no,
		AL2 = AL1
	; MToolkit = yes(Toolkit),
		AL2 = [Toolkit-defaults|AL1]
	)},
	openInputs(AL2, IOResults),
	io__get_exit_status(Status),
	( {Status = 0} ->
		{getopt__lookup_bool_option(Option_Table, abbreviate, Abbrev)},
		% The handling of flags and pragmas here is really ugly.
		% These two structures need to be rethought.
		processFiles(IOResults, Abbrev, zpragmaInit, ZPragma,
			finish(dict__init, [])-init_schema_table, Phase),
		{set_zpragma(ZPragma, Flags0, _Flags)},
		( {Phase = finish(_Dict, _TP)-_} ->

			{true}

		;	io__set_exit_status(1)
		)
	;	{true}
	).

:- type zinput ---> zinput(io__input_stream, flags).

:- pred openInputs(list(pair(string, flags)), list(zinput),
						io__state, io__state).
:- mode openInputs(in, out, di, uo) is det.
openInputs([], []) --> [].
openInputs([Filename-Flags|T], Oks) -->
	( {Filename = "-"} ->
		io__input_stream(Stdin),
		{Result = ok(Stdin)},
		io__input_stream_name(Filename1) %Filename1="<standard input>"
	;	io__open_input(Filename, Result),
		{Filename1 = Filename}
	),
	( {Result = error(IOError),
		io__error_message(IOError, Message)},
		zmessage(Filename1, [Message]),	% sets exit status to 1
		openInputs(T, Oks)
	; {Result = ok(Stream),
		Oks = [zinput(Stream, Flags)|Oks1]},
				% Kludge to handle first line pragmas
		io__putback_char(Stream, '\n'),
		openInputs(T, Oks1)
	).

:- pred zmessage(string::in, list(string)::in, io__state::di, io__state::uo)
								is det.
zmessage(F, ML) -->
	{P = (pred(S0::in, S::out) is det :-
			string__append_list([F, ": ", S0, ".\n"], S)),
	list__map(P, ML, ML1),
	list__reverse(ML1, ML2)},
	io__stderr_stream(StdErr),
	io__write_strings(StdErr, ML2),
	io__set_exit_status(1).

:- pred processFiles(list(zinput), bool, zpragma, zpragma, zphase0, zphase0,
							io__state, io__state).
:- mode processFiles(in, in, in, out, in, out, di, uo) is det.
processFiles([], _, ZPragma, ZPragma, Phase, Phase) --> [].
processFiles([zinput(Stream, Flags0)|Rest], Abbrev, ZPragma0, ZPragma,
							Phase0, Phase)-->
	io__set_input_stream(Stream, _),
	{set_zpragma(ZPragma0, Flags0, Flags)},
	io__input_stream_name(Filename),
	processFile(Filename, Abbrev, Flags, Flags1, Phase0, Phase1),
	io__close_input(Stream),
	( {Phase1 = finish(_, _)-_} ->
				% Dont process later files if earlier errors
		processFiles(Rest, Abbrev, zpragma(Flags1), ZPragma,
								Phase1, Phase)
	;	{ZPragma = zpragma(Flags1), Phase = Phase1}
	).

:- type typed_par == triple(par, subst, ptypes).

:- type zphase0 == pair(zphase, schema_table).

% Used to indicate the earliest phase in which errors have occured.
:- type zphase
	--->	lexical
	;	syntax
	;	typecheck(dict)
	;	finish(dict, list(pair(typed_par, flag))).

:- pred processFile(string, bool, flags, flags, zphase0, zphase0,
							io__state, io__state).
:- mode processFile(in, in, in, out, in, out, di, uo) is det.
processFile(Filename, Abbrev, F0, F, P0-ST0, P) -->
	( {debugging(F0)} ->
		io__write_strings(["Processing ", Filename, " ...\n"])
	;	{true}
	),
	readTokenList(operators(F0), TResult),
	( {TResult = ok(TS)},
		( {debugging(F0)} -> writeTokenList(TS) ; {true} ),
		{specification(TS, Result, ST0, ST1, F0, F1)},
		{ Abbrev = no -> set_abbreviations([], F1, F2) ; F2 = F1 },
		( {Result = ok(Spec)},
			( {debugging(F2)} -> writeSpec(Spec) ; {true} ),
			( {P0 = typecheck(D) ; P0 = finish(D, _)} ->
				zcheck(F2, Spec, Status1, D, D1),
				{(Status1=yes(TSpec1), P0=finish(_, TSpec0)) ->
					G = generating_logic(F2),
					HoP = (pred(TP::in, TPG::out) is det :-
						TPG = TP - G),
					list__map(HoP, TSpec1, TSpec2),
					list__append(TSpec0, TSpec2, TSpec),
					P1 = finish(D1, TSpec)
				;	P1 = typecheck(D1)
				}
			;	{P1 = syntax}
			)
		; {Result = error(ErrorList)},
			zmessage(Filename, ErrorList),
			{P1 = syntax}
		),
		processFile(Filename, Abbrev, F2, F, P1-ST1, P)
	; {TResult = eof,
		F = F0, P = P0-ST0}
	; {TResult = error(S)},
		zmessage(Filename, [S]),
		{F = F0, P = lexical-ST0}
	),
	( {debugging(F0)} ->
		io__write_strings(["... finished ", Filename, "\n"])
	;	{true}
	).

:- pred usage(io__state::di, io__state::uo) is det.
usage -->
	io__stderr_stream(StdErr),
	io__write_strings(StdErr, [
"Melbourne University Z typechecker, version 0.1\n",
"Copyright (C) 1996, 1997, 1998 The University of Melbourne\n",
"Usage: muz [options] <filename(s)>\n",
"Options:\n",
	"\t-a-, --no-abbreviate\n",
		"\t\tTurn off use of type abbreviations.\n",
	"\t-t <toolkit>, --toolkit <toolkit>\n",
		"\t\tTypecheck with the specified toolkit, overiding the\n",
		"\t\tbuiltin default and MUZ_TOOLKIT environment variable\n",
		"\t\t(-t- for typechecking without a toolkit).\n",

	"\t-?, -h, --help\n",
		"\t\tPrint this usage message.\n",
	"\t-d, --debug\n",
		"\t\tWrite debugging information to stdout.\n"
	]),
	io__set_exit_status(1).
