%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: diff.m
% Main author: bromage

% Something very similar to the standard diff utility.  Sort of.  :-)

% On the still-to-do list:
%	- Add command-line options.  Probably:
%		--ignore-all-space
%		--ignore-blank-lines
%		--ignore-case
%		--rcs
%	  What others are easy and don't break up the code?

:- module diff.

:- interface.
:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, list, file, lcss, std_util, require.

%-----------------------------------------------------------------------------%

	% main: top-level predicate.
main -->
	io__command_line_arguments(Args0),
%	{ getopt__process_options(Args0, Args, Result0) },
%	postprocess_options(Result0, Result), 
%	main_2(Result, Args).
	main_2(no, Args0).

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
	io__write_string("Usage: diff [options] from-file to-file\n"),
	io__write_string("Options:\n"),
	io__write_string("\tnone yet :-)\n").

%-----------------------------------------------------------------------------%

	% main_2 
:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.
main_2(yes(Msg), _) -->
	usage_error(Msg).
main_2(no, []) -->
	usage_error("missing operand").
main_2(no, [Fname1 | Rest]) -->
	( { Rest = [Fname2 | _] },
	    ( { Fname1 = Fname2 } ->
		% There are no differences between identical files.
	    	[]
	    ;
		% If either file is "-", simply use standard input.
		% (Note: Both can't be "-" since that was dealt with
		% in the previous case.)
	        ( { Fname1 = "-" } ->
	    	    file__read_input(Contents1),
	    	    file__read_file(Fname2, Contents2)
	    	; { Fname2 = "-" } ->
	    	    file__read_file(Fname1, Contents1),
	    	    file__read_input(Contents2)
	    	;
		% Otherwise read the files normally.
	    	    file__read_file(Fname1, Contents1),
	    	    file__read_file(Fname2, Contents2)
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
	    )
	; { Rest = [] },
	    usage_error("missing operand")
	).

%-----------------------------------------------------------------------------%

	% diff__do_diff takes the files plus all the command
	% line options (all zero of them) and determines what
	% to do with them.
:- pred diff__do_diff(file, file, io__state, io__state).
:- mode diff__do_diff(in, in, di, uo) is det.
diff__do_diff(File1, File2) -->
	{ lcss__find_diff(File1, File2, Diff) },
	lcss__display_diff(File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
