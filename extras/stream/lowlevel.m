%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% File: lowlevel.m.
% Main author: petdr
% Stability: exceptionally low.
%
% A lowlevel pure interface for describing streams.  You may also want
% to look at the impure lowlevel interface in `impure.m'.
%
% This file provides a typeclass for people who want to map streams
% to a foreign language binding while doing the minimizing the amount of
% work.  In particular you need to write much less foreign language
% code, since you only need to implement a few predicates with a well
% defined interface.
%
% This file provides throwing exceptions, grabbing error messages and
% packaging results into ok/error/eof.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lowlevel.

:- interface.

:- import_module stream.
:- import_module bool, char.

	% A handle on the lowlevel stream.
:- type lowlevel(S).

:- typeclass lowlevel(S) where [
		% Did an error occur processing the stream?
		% This predicate must also clear the error status of a
		% stream after reporting the error.
		% The bool indicates whether there was an error.  If the
		% bool is yes, then the string returned holds the error
		% message.
	pred get_error(S::in, string::out, bool::out,
			io__state::di, io__state::uo) is det
].

:- typeclass lowlevel__input(S) <= lowlevel(S) where [
		% Attempt to read one character from the stream
		% described by S.  
		% The bool indicates whether the character was
		% successfully read.
	pred read_char(S::in, char::out, bool::out,
			io__state::di, io__state::uo) is det,

		% The bool will be yes iff S is at the end-of-file (eof).
	pred is_eof(S::in, bool::out, io__state::di, io__state::uo) is det
].

:- typeclass output(S) <= lowlevel(S) where [
		% Attempt to write one character to the current stream.
		% The bool indicates whether the character was
		% successfully written.
	pred write_char(S::in, char::in, bool::out,
			io__state::di, io__state::uo) is det
].

:- pred init(S::in, lowlevel(S)::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

	% Read one character of input.  This read character
	% implementation can be used in instance declarations for the
	% stream__input type class.

:- pred low_read_char(lowlevel(S), stream__result(char),
		io__state, io__state) <= lowlevel__input(S).
:- mode low_read_char(in, out, di, uo) is det.

	% Write one character of output.  This write character
	% implementation can be used in instance declarations for the
	% stream__output type class.

:- pred low_write_char(lowlevel(S), char,
		io__state, io__state) <= lowlevel__output(S).
:- mode low_write_char(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.
:- import_module exception, std_util.

:- type lowlevel(S)
	--->	lowlevel(
			S,		% Handle
			mutvar(unit)	% Mutvar used as a semaphore to
					% ensure the atomicity of
					% operations.
		).

init(S, lowlevel(S, MVar)) -->
	mutvar__init(MVar),
	mutvar__put(MVar, unit).

low_read_char(lowlevel(Stream, MVar), Result) -->
	mutvar__take(MVar, Unit),
	read_char(Stream, Chr, ReadBool), 
	( { ReadBool = yes } ->
		{ Result = ok(Chr) }
	;
		get_error(Stream, Error, ErrorBool), 
		( { ErrorBool = yes } ->
			{ Result = error(Error) }
		;
			is_eof(Stream, EofBool),
			( { EofBool = yes } ->
				{ Result = eof }
			;
				{ ErrorStr = "read char failed for an unknown reason" },
				{ Result = error(ErrorStr) }
			)
		)
	),
	mutvar__put(MVar, Unit).

%-----------------------------------------------------------------------------%

low_write_char(lowlevel(Stream, MVar), Chr) -->
	mutvar__take(MVar, Unit),
	write_char(Stream, Chr, WriteBool),
	( { WriteBool = yes } ->
		mutvar__put(MVar, Unit)
	;
		get_error(Stream, Err0, ErrorBool),
		{ ErrorBool = yes ->
			Err = Err0
		;
			Err = "write char failed but there is no error message"
		},
		mutvar__put(MVar, Unit),
		{ throw(stream_error(Err)) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
