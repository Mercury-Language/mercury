%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: io.nl.
% Main author: fjh.
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using Prolog's horrible
% non-logical I/O primitives.
%
% This library is still pretty yucko because it's based on
% the old dec-10 Prolog I/O (see, seeing, seen, tell, telling, told)
% instead of the stream-based I/O. 
% TODO: fix this.
%
%-----------------------------------------------------------------------------%

:- module io.
:- import_module char, int, float, string.
:- interface.

% External interface: imported predicate

% :- pred main_predicate(list(string), io__state, io__state).
%	main_predicate(ArgStrings, IOState0, IOState1).
%		This module provides startup code which calls main_predicate/3.

% External interface: exported types

:- export_type io__state.

% External interface: exported predicates

:- pred io__progname(string, io__state, io__state).
:- mode io__progname(output, di, uo).
%		Returns the name that the program was invoked with.
%		Does not modify the IO state.

:- pred io__write_string(string, io__state, io__state).
:- mode io__write_string(input, di, uo).
%		Writes a string to standard output.

:- pred io__write_char(character, io__state, io__state).
:- mode io__write_char(input, di, uo).
%		Writes a character to standard output.

:- pred io__write_int(int, io__state, io__state).
:- mode io__write_int(input, di, uo).
%		Writes an integer to standard output.

:- pred io__write_float(float, io__state, io__state).
%	io__write_float(Float, IO0, IO1).
%		Writes a floating point number to standard output.

:- pred io__write_anything(_T, io__state, io__state).
:- mode io__write_anything(input, di, uo).
%		Writes it's argument to standard output.
%		The argument may be of any type.

:- type res ---> ok ; error.
:- pred io__see(string, res, io__state, io__state).
:- mode io__see(input, output, di, uo).
%	io__see(File, Result, IO0, IO1).
%		Redirects the current input stream.
%		As per Prolog see/1. Result is either 'ok' or 'error'.

:- pred io__seen(io__state, io__state).
:- mode io__seen(di, uo).
%		Closes the current input stream.
%		The default input stream reverts to standard input.
%		As per Prolog seen/0.

:- pred io__tell(string, res, io__state, io__state).
:- mode io__tell(input, output, di, uo).
%	io__tell(File, Result, IO0, IO1).
%		Redirects the current output stream.
%		As per Prolog tell/1. Result is either 'ok' or 'error'.

:- pred io__told(io__state, io__state).
:- mode io__told(di, uo).
%	io__told(IO0, IO1).
%		Closes the current output stream.
%		The default output stream reverts to standard output.
%		As per Prolog told/0.

:- pred io__get_line_number(int, io__state, io__state).
:- mode io__get_line_number(output, di, uo).
%	Return the line number of the current stream
%	(as per NU-Prolog lineCount/1).

% XXX The type and mode of io__gc_call/3 are a bit tricky.
% :- pred io__gc_call(pred(M, io__state::di, io__state::uo) :: M,
%			io__state::di, io__state::uo).
%	io__gc_call(Goal, IO0, IO1).
%		Execute Goal, passing IO0, and IO1, and
%		collect any garbage created during it's execution.

:- pred io__flush_output(io__state, io__state).
:- mode io__flush_output(di, uo).
%	Flush the output buffer of the current output stream.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

/* Most of these predicates are implemented using non-logical NU-Prolog code
   in io.nu.nl. */

:- type io__state ---> io__state(io__state_2).
:- type io__state_2 ---> old ; current.

/*
:- external("NU-Prolog", io__progname/2).
:- external("NU-Prolog", io__write_string/3).
:- external("NU-Prolog", io__write_char/3).
:- external("NU-Prolog", io__write_int/3).
:- external("NU-Prolog", io__write_float/3).
:- external("NU-Prolog", io__write_anything/3).
:- external("NU-Prolog", io__see/4).
:- external("NU-Prolog", io__seen/2).
:- external("NU-Prolog", io__tell/4).
:- external("NU-Prolog", io__told/2).
:- external("NU-Prolog", io__get_line_number/3).
:- external("NU-Prolog", io__gc_call/3).
:- external("NU-Prolog", io__flush_output/2).
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
