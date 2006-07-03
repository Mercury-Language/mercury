%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: diff.m.
% Main authors: bromage, Marnix Klooster <marnix@worldonline.nl>
% 
% Something very similar to the standard diff utility.  Sort of.  :-)
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module diff.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module options, myers, diff_out, globals, filter, match.
:- import_module string, list, file, maybe, require, getopt.

%-----------------------------------------------------------------------------%

	% main: top-level predicate.
main -->
	io__command_line_arguments(Args0),
	{ options__get_option_ops(OptionOps) },
	{ getopt__process_options(OptionOps, Args0, Args, Result0) },
	postprocess_options(Result0, Result), 
	( { Result = yes(Msg) },
		usage_error(Msg)
	; { Result = no },
		globals__io_get_output_style(OutputStyle),
		( { OutputStyle = help_only } ->
			usage
		; { OutputStyle = version_only } ->
			version
		;
			main_2(Args)
		)
	).

%-----------------------------------------------------------------------------%

:- pred usage_error(string :: in, io__state :: di, io__state :: uo) is det.
usage_error(Msg) -->
	io__progname_base("diff", ProgName),
	io__stderr_stream(StdErr),
	io__write_strings(StdErr, [ProgName, ": ", Msg, "\n"]),
	io__set_exit_status(1),
	usage.

:- pred usage_io_error(io__error, io__state, io__state).
:- mode usage_io_error(in, di, uo) is det.
usage_io_error(Error) -->
	{ io__error_message(Error, Msg) },
	usage_error(Msg).

:- pred usage(io__state :: di, io__state :: uo) is det.
usage -->
	io__write_string("Usage: diff [options] from-file to-file\n\n"),
	options_help.

:- pred version(io__state :: di, io__state :: uo) is det.
version -->
	io__write_string("diff - Mercury diff version 0.4\n").

%-----------------------------------------------------------------------------%

	% main_2 analyses the command-line arguments which are not
	% options and calls diff__do_diff.
:- pred main_2(list(string), io__state, io__state).
:- mode main_2(in, di, uo) is det.
main_2([]) -->
	usage_error("missing operand").
main_2([Fname1 | Rest]) -->
	( { Rest = [Fname2 | _] },
		( { Fname1 = Fname2 } ->
			% Not sure why anyone would want to diff two
			% files with the same name, but just in case...
			( { Fname1 = "-" } ->
				file__read_input(Fname1, Contents1),
				{ Contents1 = Contents2 }
			;
				file__read_file(Fname1, Contents1),
				{ Contents1 = Contents2 }
			)
		;
			% If either file is "-", simply use standard input.
			% (Note: Both can't be "-" since that was dealt with
			% in the previous case.)
			( { Fname1 = "-" } ->
				file__read_input(Fname1, Contents1),
				file__read_file(Fname2, Contents2)
			; { Fname2 = "-" } ->
				file__read_file(Fname1, Contents1),
				file__read_input(Fname2, Contents2)
			;
			% Otherwise read the files normally.
				file__read_file(Fname1, Contents1),
				file__read_file(Fname2, Contents2)
			)
		),
		% Now do the diff.
		( { Contents1 = ok(File1), Contents2 = ok(File2) } ->
			diff__do_diff(File1, File2)
		; { Contents1 = error(Msg) } ->
			usage_io_error(Msg)
		; { Contents2 = error(Msg) } ->
			usage_io_error(Msg)
		;
			{ error("main2") }
		)
	; { Rest = [] },
		usage_error("missing operand")
	).

%-----------------------------------------------------------------------------%

	% diff__do_diff takes the files plus all the command
	% line options and determines what to do with them.
	%
	% At the moment, we're organised into four passes:
	%
	%	- build_matches determines which lines from the
	%	  input files match (using the appropriate command-
	%	  line options).
	%	- diff_by_myers takes the matches produced and
	%	  computes a diff between them.
	%	- filter_diff analyses the diff, filtering out
	%	  any edits which the user said that they didn't
	%	  want to see (using the appropriate command-line
	%	  options).
	%	- display_diff outputs the diff in whatever output
	%	  format the user chose.
	%
:- pred diff__do_diff(file, file, io__state, io__state).
:- mode diff__do_diff(in, in, di, uo) is det.
diff__do_diff(File1, File2) -->
	build_matches(File1, File2, FileX, FileY),
	diff_by_myers(FileX, FileY, Diff0),
	filter_diff(Diff0, File1, File2, Diff),
	display_diff(File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
