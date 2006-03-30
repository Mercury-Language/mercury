%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% File: impure.m.
% Main author: petdr
% Stability: exceptionally low.
%
% An impure interface for describing streams.  You may want to also look
% at the pure lowlevel interface in `lowlevel.m'.
%
% This file provides a typeclass for people who want to map streams
% to a foreign language binding while doing the minimum amount of work.  In
% particular you need to write much less foreign language code, since
% you only need to implement a few impure predicates with a well defined
% interface.
%
% This file provides throwing exceptions, grabbing error messages,
% results packaged into ok/error/eof, and turning C style handle based
% IO into Mercury di/uo.  That's all it does, but it's something you'll
% have to do and get right every time you implement a stream, so we have
% done it for you.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module (impure).

:- interface.

:- import_module stream.
:- import_module char.
:- import_module io.

	% A handle on the impure stream.
:- type impure(S).

:- typeclass impure(S) where [
		% Did an error occur processing the stream?
		% This predicate must also clear the error status of a
		% stream after reporting the error.
	impure pred impure__get_error(S::in, string::out) is semidet
].

:- typeclass impure__input(S) <= impure(S) where [
		% Read one character from the stream described by S.
		% Fail if we reach eof or some error condition.
	impure pred impure__read_char(S::in, char::out) is semidet,

		% Have we reached the eof for S?
	semipure pred impure__is_eof(S::in) is semidet
].

:- typeclass impure__output(S) <= impure(S) where [
		% Read one character from the current stream.
	impure pred impure__write_char(S::in, char::in) is semidet
].

:- pred impure_init(S::in, impure(S)::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

	% Read one character of input.  This read character
	% implementation can be used in instance declarations for the
	% stream__input type class.

:- pred pure_read_char(impure(S), stream__result(char),
		io__state, io__state) <= impure__input(S).
:- mode pure_read_char(in, out, di, uo) is det.

	% Write one character of output.  This write character
	% implementation can be used in instance declarations for the
	% stream__output type class.

:- pred pure_write_char(impure(S), char,
		io__state, io__state) <= impure__output(S).
:- mode pure_write_char(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mvar.
:- import_module exception, unit.

:- type impure(S)
	--->	impure(
			S,		% Handle
			mvar(unit)	% Mvar used as a semaphore to
					% ensure the atomicity of
					% operations.
		).

impure_init(S, impure(S, MVar)) -->
	mvar__init(MVar),
	mvar__put(MVar, unit).

:- pragma promise_pure(pure_read_char/4).
pure_read_char(impure(Stream, MVar), Result, IO0, IO) :-
	mvar__take(MVar, Unit, IO0, IO1),
	( impure impure__read_char(Stream, Chr) ->
		Result = ok(Chr)
	;
		( impure impure__get_error(Stream, Error) ->
			Result = error(Error)
		; semipure impure__is_eof(Stream) ->
			Result = eof
		;
			Error = "read char failed for an unknown reason",
			Result = error(Error)
		)
	),
	mvar__put(MVar, Unit, IO1, IO).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(pure_write_char/4).
pure_write_char(impure(Stream, MVar), Chr, IO0, IO) :-
	mvar__take(MVar, Unit, IO0, IO1),
	( impure impure__write_char(Stream, Chr) ->
		mvar__put(MVar, Unit, IO1, IO)
	;
		( impure impure__get_error(Stream, Err0) ->
			Err = Err0
		;
			Err = "write char failed but there is no error message"
		),
		mvar__put(MVar, Unit, IO1, IO),
		throw(stream_error(Err))
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
