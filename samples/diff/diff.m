%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% On the still-to-do list:
%	- Add command-line options.  Probably -d, -i and -w.
%	  What others are easy and don't break up the code?
%
%	- Some real error messages rather than calls to error/1.
%	  (Happy now, Fergus?)

:- module diff.
% Main author: bromage

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

	% main_2 
:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.
main_2(yes(_), _) --> [].
main_2(no, []) --> []. % Should display usage
main_2(no, [File1 | Rest]) -->
	( { Rest = [ File2 | _ ] },
	    ( { File1 = File2 } ->
		% There are no differences between identical files.
	    	[]
	    ;
		% If either file is "-", simply use standard input.
		% (Note: Both can't be "-" since that was dealt with
		% in the previous case.)
	        ( { File1 = "-" } ->
	    	    file__read_input(Contents1),
	    	    file__read_file(File2, Contents2)
	    	; { File2 = "-" } ->
	    	    file__read_file(File1, Contents1),
	    	    file__read_input(Contents2)
	    	;
		% Otherwise read the files normally.
	    	    file__read_file(File1, Contents1),
	    	    file__read_file(File2, Contents2)
	    	),
		% Now do the diff.
	        lcss__show_diff(Contents1, Contents2)
	    )
	; { Rest = [] },
	    [] % Should display usage
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
