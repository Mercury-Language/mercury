:- module midimon.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module midi.
:- import_module global, stream, spawn.
:- import_module bool, getopt, int, list, require, std_util, string.

main -->
	io__command_line_arguments(Args0),
	{ process_options(
		option_ops(short_option, long_option, option_defaults),
		Args0, _Args, MOpts) },
	(
		{ MOpts = ok(Opts) },
		{ lookup_bool_option(Opts, help, Help) },
		( { Help = yes } ->
			help
		;
			{ lookup_maybe_string_option(Opts, input, MInfile) },
			open_input(MInfile, InFileOpened),
			(
				{ InFileOpened = yes },
				new(Bytes0),
				new(Messages),
				spawn((pred(di, uo) is cc_multi -->
					read_midi(Bytes0, Messages)
				)),
				spawn((pred(di, uo) is cc_multi -->
					print_messages(Messages)
				)),
				read_input(Bytes0)
			;
				{ InFileOpened = no }
			)
		)
	;
		{ MOpts = error(Msg) },
		stderr_stream(StdErr),
		format(StdErr, "%s\n", [s(Msg)])
	).

:- pred open_input(maybe(string), bool, io__state, io__state).
:- mode open_input(in, out, di, uo) is det.

open_input(no, Opened) -->
	see_binary("/dev/midi", Res),
	(
		{ Res = ok },
		{ Opened = yes }
	;
		{ Res = error(Err) },
		{ error_message(Err, Msg) },
		stderr_stream(StdErr),
		format(StdErr, "error opening `/dev/midi': %s\n", [s(Msg)]),
		{ Opened = no }
	).
open_input(yes(FileName), Opened) -->
	( { FileName = "-" } ->
			% use stdin
		{ Opened = yes }
	;
		see_binary(FileName, Res),
		(
			{ Res = ok },
			{ Opened = yes }
		;
			{ Res = error(Err) },
			{ error_message(Err, Msg) },
			stderr_stream(StdErr),
			format(StdErr, "error opening `%s': %s\n",
				[s(FileName), s(Msg)]),
			{ Opened = no }
		)
	).

:- pred read_input(stream(byte), io__state, io__state).
:- mode read_input(in, di, uo) is det.

read_input(Stream) -->
	io__read_byte(Res0),
	(
		{ Res0 = eof },
		end(Stream)
	;
		{ Res0 = error(Err) },
		{ io__error_message(Err, Msg) },
		error(Stream, Msg)
	;
		{ Res0 = ok(Byte) },
		put(Stream, Byte),
		read_input(Stream)
	).

:- pred print_messages(stream(message), io__state, io__state).
:- mode print_messages(in, di, uo) is det.

print_messages(Stream) -->
	get(Stream, Res0),
	(
		{ Res0 = ok(Msg) },
		write(Msg), write_string(".\n"),
		print_messages(Stream)
	;
		{ Res0 = end }
	;
		{ Res0 = error(Msg) },
		write_string(Msg), nl
	).

:- type option_table		== option_table(option).
:- type maybe_option_table	== maybe_option_table(option).

	% The master list of options.

:- type option
	--->	help
	;	input
	.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred long_option(string::in, option::out) is semidet.
long_option("help",			help).
long_option("input-file",		input).

:- pred short_option(character::in, option::out) is semidet.
short_option('h', help).
short_option('i', input).

:- pred option_defaults(option :: out, option_data :: out) is nondet.
option_defaults(Opt, Data) :-
	semidet_succeed,
	option_defaults0(Opt, Data).

:- pred option_defaults0(option :: out, option_data :: out) is multi.
option_defaults0(help,		bool(no)).
option_defaults0(input, 	maybe_string(no)).

:- pred help(io__state, io__state).
:- mode help(di, uo) is det.

help -->
	write_strings([
"usage: midimon [--help|-h] [--input-file|-i <filename>]\n",
"	--help|-h		print this help message.\n",
"	--input-file|-i <file>	read from <file> (default is /dev/midi).\n"
	]).

%-----------------------------------------------------------------------------%

