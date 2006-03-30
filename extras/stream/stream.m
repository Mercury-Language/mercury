%-----------------------------------------------------------------------------%*
% Copyright (C) 2000-2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% File: stream.m.
% Main author: petdr
% Stability: exceptionally low.
%
% This file provides a typeclass for defining streams in Mercury.
% It is completely pure and you are encouraged to use it to write
% streams in Mercury.  If however you are a library implementor then you
% may want to look at the lowlevel interface described in lowlevel.m or
% the impure interface described in impure.m
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module stream.

:- interface.

:- import_module char, io, list.

	% The state of one stream of type S.
:- type stream(S).

	% The type of exceptions thrown by this module.
:- type stream_error ---> stream_error(string).

:- type stream__result(T)
	--->	ok(T)
	;	eof
	;	error(string)
	.

:- type stream__result
	--->	ok
	;	eof
	;	error(string)
	.

:- type stream__res
	--->	ok
	;	error(string)
	.

%-----------------------------------------------------------------------------%

%
% The pure interface to streams.
%

	% The root of the stream class hierarchy
:- typeclass stream(S) where [
		% A human-readable name describing the current stream
		% suitable for use in (e.g.) error messages.
	func stream__name(S) = string
].

	% Streams from which you can read input.
:- typeclass stream__input(S) <= stream(S) where [
		% Read one character from the stream S.
		% Errors are reported via the stream__result type.
	pred stream__read_char(S::in, stream__result(char)::out,
			io__state::di, io__state::uo) is det
].

	% Streams to which you can write output.
:- typeclass stream__output(S) <= stream(S) where [
		% Write one character to the stream S.
		% Throws a stream_error exception if a problem occurs.
	pred stream__write_char(S::in, char::in,
			io__state::di, io__state::uo) is det
].

	% Streams which can be both read from and written to.
:- typeclass stream__duplex(S)
		<= (stream__input(S), stream__output(S)) where [].

	% Stream for which characters can be put back at the start of
	% the stream.
:- typeclass stream__putback(S) <= stream__input(S) where [
		% Putback one character on the input stream.
		% The implementation must guarantee at least one
		% character of putback and throw a stream_error
		% exception if a problem is encountered during the
		% putback.
	pred stream__putback_char(S::in, char::in,
			io__state::di, io__state::uo) is det
].

	% Stream with an unbounded amounts of putback.
	% This adds no new methods, just a guarantee:
	% there's no limit on the amount of putback except available
	% memory, so unless you run out of heap space, the putback_char
	% method must always succeed.
:- typeclass stream__unbounded_putback(S) <= stream__putback(S) where [].

:- typeclass stream__line(S) <= stream__input(S) where [
		% Return the line number of the input stream.
		% Lines are numbered starting from one.
	pred stream__line_number(S::in, int::out,
			io__state::di, io__state::uo) is det,

		% Set the line number of the input stream.
	pred stream__set_line_number(S::in, int::in,
			io__state::di, io__state::uo) is det

].

%-----------------------------------------------------------------------------%

	%
	% A input stream with infinite putback.
	%
:- type putback(S).
:- instance stream(putback(S)) <= stream(S).
:- instance stream__input(putback(S)) <= stream__input(S).
:- instance stream__putback(putback(S)) <= stream__input(S).

	% Create the putback stream.
:- pred putback_stream(S::in, putback(S)::out,
		io__state::di, io__state::uo) is det <= stream__input(S).

	% Retrieve the original stream.
:- func putback_base_stream(putback(S)) = S.

%-----------------------------------------------------------------------------%

	%
	% A stream which records which line of the input stream we are
	% up to.  Lines are numbered starting from one.
	%
:- type linenumber(S).
:- instance stream(linenumber(S)) <= stream(S).
:- instance stream__input(linenumber(S)) <= stream__input(S).
:- instance stream__putback(linenumber(S)) <= stream__putback(S).
:- instance stream__line(linenumber(S)) <= stream__input(S).

	% Create a line-number counting stream.
:- pred linenumber_stream(S::in, linenumber(S)::out,
		io__state::di, io__state::uo) is det <= stream__input(S).

	% Retrieve the original stream.
:- func linenumber_base_stream(linenumber(S)) = S.

%-----------------------------------------------------------------------------%

% XXX If/when default type class implementations are introduced,
% some of the following predicates should probably become members of the
% relevant type classes.

% Predicates which require an input stream.

	% Reads one line of input from the current input stream.
:- pred stream__read_line(S::in, stream__result(list(char))::out,
		io__state::di, io__state::uo) is det <= stream__input(S).

%-----------------------------------------------------------------------------%

% Predicates which require an input stream with putback.

	% Reads a whitespace delimited word from the current input stream.
:- pred stream__read_word(S::in, stream__result(list(char))::out,
		io__state::di, io__state::uo) is det <= stream__putback(S).

	% Discards all the whitespace from the input stream.
:- pred stream__ignore_whitespace(S::in, stream__result::out,
		io__state::di, io__state::uo) is det <= stream__putback(S).

%-----------------------------------------------------------------------------%

% Predicates which require an output stream.
% On failure these predicates will throw an stream_error exception.

	% Write the string to the output stream.
:- pred stream__write_string(S::in, string::in,
		io__state::di, io__state::uo) is det <= stream__output(S).

%-----------------------------------------------------------------------------%

% Predicates which require an input and a output stream.

	% Echo stream InputS onto stream OutputS.
	% Errors associated with stream InputS are reported through the
	% stream__res argument.  Errors associated with stream OutputS
	% throw a stream_error exception.
:- pred cat(InputS::in, OutputS::in, stream__res::out,
		io__state::di, io__state::uo) is det
		<= (stream__input(InputS), stream__output(OutputS)).

%-----------------------------------------------------------------------------%

:- implementation.

	% These two imports are only so that when building a stand-alone
	% stream library we include the following modules in the
	% library.
:- import_module (impure), lowlevel.

:- import_module mvar.
:- import_module int, string.


:- type stream(S) ---> stream.

:- type putback(S)
	--->	pb(
			S,
			mvar(list(char))
		).

:- instance stream(putback(S)) <= stream(S) where [
	(stream__name(pb(S, _)) = stream__name(S))
].

:- instance stream__input(putback(S)) <= stream__input(S) where [
	pred(stream__read_char/4) is putback_read_char
].

:- instance stream__putback(putback(S)) <= stream__input(S) where [
	pred(stream__putback_char/4) is putback_putback_char
].

putback_stream(Stream, pb(Stream, MPutbackChars)) -->
	mvar__init(MPutbackChars),
	mvar__put(MPutbackChars, []).

putback_base_stream(pb(Stream, _)) = Stream.

:- pred putback_read_char(putback(S)::in, stream__result(char)::out,
		io__state::di, io__state::uo) is det <= stream__input(S).

putback_read_char(pb(Stream, MPutbackChars), Result) -->
	mvar__take(MPutbackChars, PutbackChars),
	(
		{ PutbackChars = [] },
		{ NewPutbackChars = PutbackChars },
		stream__read_char(Stream, Result)
	;
		{ PutbackChars = [Char | NewPutbackChars] },
		{ Result = ok(Char) }
	),
	mvar__put(MPutbackChars, NewPutbackChars).

:- pred putback_putback_char(putback(S)::in, char::in,
		io__state::di, io__state::uo) is det <= stream__input(S).

putback_putback_char(pb(_Stream, MPutbackChars), Char) -->
	mvar__take(MPutbackChars, PutbackChars),
	mvar__put(MPutbackChars, [Char | PutbackChars]).

%-----------------------------------------------------------------------------%

:- type linenumber(S)
	--->	line(
			S,		% stream
			mvar(int)	% line number
		).

:- instance stream(linenumber(S)) <= stream(S) where [
	(stream__name(line(S, _)) = stream__name(S))
].

:- instance stream__input(linenumber(S)) <= stream__input(S) where [
	pred(stream__read_char/4) is linenumber_read_char
].

:- instance stream__putback(linenumber(S)) <= stream__putback(S) where [
	pred(stream__putback_char/4) is linenumber_putback_char
].

:- instance stream__line(linenumber(S)) <= stream__input(S) where [
	pred(stream__line_number/4) is linenumber,
	pred(stream__set_line_number/4) is set_linenumber
].

linenumber_stream(S, line(S, MLine)) -->
	mvar__init(MLine),
	mvar__put(MLine, 0).

linenumber_base_stream(line(Stream, _)) = Stream.

:- pred linenumber_read_char(linenumber(S)::in, stream__result(char)::out,
		io__state::di, io__state::uo) is det <= stream__input(S).

linenumber_read_char(line(Stream, MLine), Result) -->
	stream__read_char(Stream, Result),
	( { Result = ok('\n') } ->
		mvar__take(MLine, Line),
		mvar__put(MLine, Line + 1)
	;
		[]
	).

:- pred linenumber_putback_char(linenumber(S)::in, char::in,
		io__state::di, io__state::uo) is det <= stream__putback(S).

linenumber_putback_char(line(Stream, MLine), Char) -->
	stream__putback_char(Stream, Char),
	( { Char = '\n' } ->
		mvar__take(MLine, Line),
		mvar__put(MLine, Line - 1)
	;
		[]
	).

:- pred linenumber(linenumber(S)::in, int::out,
		io__state::di, io__state::uo) is det.

linenumber(line(_, MLine), Line) -->
	mvar__take(MLine, Line),
	mvar__put(MLine, Line).

:- pred set_linenumber(linenumber(S)::in, int::in,
		io__state::di, io__state::uo) is det.

set_linenumber(line(_, MLine), Line) -->
	mvar__take(MLine, _OldLine),
	mvar__put(MLine, Line).

%-----------------------------------------------------------------------------%

read_line(Stream, Result) -->
	stream__read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { Char = '\n' } ->
			{ Result = ok([Char]) }
		;
			read_line(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)	
	).

%-----------------------------------------------------------------------------%

read_word(Stream, Result) -->
	ignore_whitespace(Stream, WSResult),
	(
		{ WSResult = error(Error) },
		{ Result = error(Error) }
	;
		{ WSResult = eof },
		{ Result = eof }
	;
		{ WSResult = ok },
		read_word_2(Stream, Result)
	).

:- pred read_word_2(S::in, stream__result(list(char))::out,
		io__state::di, io__state::uo) is det <= stream__putback(S).

read_word_2(Stream, Result) -->
	read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { char__is_whitespace(Char) } ->
			putback_char(Stream, Char),
			{ Result = ok([]) }
		;
			read_word_2(Stream, Result0),
			(
				{ Result0 = ok(Chars) },
				{ Result = ok([Char | Chars]) }
			;
				{ Result0 = error(_) },
				{ Result = Result0 }
			;
				{ Result0 = eof },
				{ Result = ok([Char]) }
			)
		)	
	).

ignore_whitespace(Stream, Result) -->
	read_char(Stream, CharResult),
	(
		{ CharResult = error(Error) },
		{ Result = error(Error) }
	;
		{ CharResult = eof },
		{ Result = eof }
	;
		{ CharResult = ok(Char) },
		( { char__is_whitespace(Char) } ->
			ignore_whitespace(Stream, Result)
		;
			putback_char(Stream, Char),
			{ Result = ok }
		)	
	).

%-----------------------------------------------------------------------------%

write_string(Stream, String) -->
	string__foldl(write_char(Stream), String).
	
%-----------------------------------------------------------------------------%

cat(In, Out, Result) -->
	stream__read_char(In, Res),
	(
		{ Res = ok(Char) },
		stream__write_char(Out, Char),
		cat(In, Out, Result)
	;
		{ Res = eof },
		{ Result = ok }
	;
		{ Res = error(Error) },
		{ Result = error(Error) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
