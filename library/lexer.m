%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: lexer.m.
% main author: fjh.
% stability: high.
%
% Lexical analysis.  This module defines the representation of tokens
% and exports predicates for reading in tokens from an input stream.
%
% See ISO Prolog 6.4.  Also see the comments at the top of parser.m.
%
%-----------------------------------------------------------------------------%

:- module lexer.
:- interface.
:- import_module io, char.

:- type	token
	--->	name(string)
	;	variable(string)
	;	integer(int)
	;	float(float)
	;	string(string)		% "...."
	;	open			% '('
	;	open_ct			% '(' without any preceding whitespace
	;	close			% ')'
	;	open_list		% '['
	;	close_list		% ']'
	;	open_curly		% '{'
	;	close_curly		% '}'
	;	ht_sep			% '|'
	;	comma			% ','
	;	end			% '.'
	;	junk(char)		% junk character in the input stream
	;	error(string)		% some other invalid token
	;	io_error(io__error)	% error reading from the input stream
	;	eof.			% end-of-file

% For every token, we record the line number of the line on
% which the token occurred.

:- type token_context == int.	% line number

% This "fat list" representation is more efficient than a list of pairs.
:- type token_list	--->	token_cons(token, token_context, token_list)
			;	token_nil.

:- pred lexer__get_token_list(token_list, io__state, io__state).
:- mode lexer__get_token_list(out, di, uo) is det.
%	Read a list of tokens from the current input stream.
%	Keep reading until we encounter either an `end' token
%	(i.e. a full stop followed by whitespace) or the end-of-file.

% The type `offset' represents a (zero-based) offset into a string.
:- type offset == int.

:- pred lexer__string_get_token_list(string, offset, token_list, posn, posn).
:- mode lexer__string_get_token_list(in, in, out, in, out) is det.
% lexer__string_get_token_list(String, MaxOffset, Tokens,
%		InitialPos, FinalPos):
%	Scan a list of tokens from a string,
%	starting at the current offset specified by InitialPos.
%	Keep scanning until either we encounter either an `end' token
%	(i.e. a full stop followed by whitespace) or until
%	we reach MaxOffset.  (MaxOffset must be =< the length of the string.)
%	Return the tokens scanned in Tokens, and return the position
%	one character past the end of the last token in FinalPos.

:- pred lexer__string_get_token_list(string, token_list, posn, posn).
:- mode lexer__string_get_token_list(in, out, in, out) is det.
% lexer__string_get_token_list(String, Tokens, InitialPos, FinalPos):
%	calls string_get_token_list/5 above with MaxPos = length of String.

:- pred lexer__token_to_string(token, string).
:- mode lexer__token_to_string(in, out) is det.
%	Convert a token to a human-readable string describing the token.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%
:- interface.

	% lexer__graphic_token_char(Char): true iff `Char'
	% is "graphic token char" (ISO Prolog 6.4.2).
	% This is exported for use by term_io__quote_atom.
:- pred lexer__graphic_token_char(char).
:- mode lexer__graphic_token_char(in) is semidet.
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, term, require, string, int.

%
% Note that there are two implementations of most predicates here:
% one which deals with strings, the other that deals with io__states.
% We can't write the io__state version in terms of the string
% version because we don't know how much string to slurp up
% until after we've lexically analysed it.  Some interactive
% applications require the old Prolog behaviour of stopping
% after an end token (i.e. `.' plus whitespace) rather than
% reading in whole lines.
% Conversely, we can't write the string version using the io__state
% version, since that would require either cheating with the io__state
% or ruining the string interface.
% 
% An alternative would be to write both versions in terms
% of a generic "char_stream" typeclass, with instances
% for io__states and for strings.
% However, for this to be acceptably efficient it would
% require the compiler to specialize the code, which
% currently (13 May 98) it is not capable of doing.
%
% In fact, the string version is still not as efficient as I would
% like.  The compiler ought to (but currently doesn't) unfold all
% the instances of the `posn' type.  We could do this type unfolding
% by hand, but it would be very tedious and it would make the code
% less readable.  If/when there is compiler support for this, we
% should also think about moving the `String' and `Len' arguments
% into the posn (or making a new `lexer_state' struct which contains
% both the posn and the String and Len arguments).
%

%-----------------------------------------------------------------------------%

lexer__token_to_string(name(Name), String) :-
	string__append_list(["token '", Name, "'"], String).
lexer__token_to_string(variable(Var), String) :-
	string__append_list(["variable `", Var, "'"], String).
lexer__token_to_string(integer(Int), String) :-
	string__int_to_string(Int, IntString),
	string__append_list(["integer `", IntString, "'"], String).
lexer__token_to_string(float(Float), String) :-
	string__float_to_string(Float, FloatString),
	string__append_list(["float `", FloatString, "'"], String).
lexer__token_to_string(string(Token), String) :-
	string__append_list(["string """, Token, """"], String).
lexer__token_to_string(open, "token ` ('").
lexer__token_to_string(open_ct, "token `('").
lexer__token_to_string(close, "token `)'").
lexer__token_to_string(open_list, "token `['").
lexer__token_to_string(close_list, "token `]'").
lexer__token_to_string(open_curly, "token `{'").
lexer__token_to_string(close_curly, "token `}'").
lexer__token_to_string(ht_sep, "token `|'").
lexer__token_to_string(comma, "token `,'").
lexer__token_to_string(end, "token `. '").
lexer__token_to_string(eof, "end-of-file").
lexer__token_to_string(junk(JunkChar), String) :-
	char__to_int(JunkChar, Code),
	string__int_to_base_string(Code, 16, Hex),
	string__append_list(["illegal character <<0x", Hex, ">>"], String).
lexer__token_to_string(io_error(IO_Error), String) :-
	io__error_message(IO_Error, IO_ErrorMessage),
	string__append("I/O error: ", IO_ErrorMessage, String).
lexer__token_to_string(error(Message), String) :-
	string__append_list(["illegal token (", Message, ")"], String).

	% We build the tokens up as lists of characters in reverse order.
	% When we get to the end of each token, we call
	% `lexer__rev_char_list_to_string/2' to convert that representation
	% into a string.

	% Comments of the form
	%	foo --> bar . baz
	% mean that we are parsing a `foo', and we've already scanned
	% past the `bar', so now we need to match with a `baz'.

lexer__get_token_list(Tokens) -->
	lexer__get_token(Token, Context),
	( { Token = eof } ->
		{ Tokens = token_nil }
	; { Token = end ; Token = error(_) ; Token = io_error(_) } ->
		{ Tokens = token_cons(Token, Context, token_nil) }
	;
		{ Tokens = token_cons(Token, Context, Tokens1) },
		lexer__get_token_list(Tokens1)
	).

lexer__string_get_token_list(String, Tokens) -->
	{ string__length(String, Len) },
	lexer__string_get_token_list(String, Len, Tokens).

lexer__string_get_token_list(String, Len, Tokens) -->
	lexer__string_get_token(String, Len, Token, Context),
	( { Token = eof } ->
		{ Tokens = token_nil }
	; { Token = end ; Token = error(_) ; Token = io_error(_) } ->
		{ Tokens = token_cons(Token, Context, token_nil) }
	;
		{ Tokens = token_cons(Token, Context, Tokens1) },
		lexer__string_get_token_list(String, Len, Tokens1)
	).

%-----------------------------------------------------------------------------%

% some low-level routines

:- pred lexer__get_context(token_context, io__state, io__state).
:- mode lexer__get_context(out, di, uo) is det.

lexer__get_context(Context) -->
	io__get_line_number(Context).

:- type string_token_context == token_context.

:- pred lexer__string_get_context(posn, string_token_context, posn, posn).
:- mode lexer__string_get_context(in, out, in, out) is det.

lexer__string_get_context(StartPosn, Context, EndPosn, EndPosn) :-
	StartPosn = posn(StartLineNum, _, _),
	Context = StartLineNum.
	% In future, we might want to modify this code to read something
	% like this:
	%	posn_to_line_and_column(StartPosn, StartLineNum, StartColumn),
	%	posn_to_line_and_column(EndPosn, EndLineNum, EndColumn),
	%	Context = detailed(StartLine, StartColumn, EndLine, EndColumn).

:- pred lexer__string_read_char(string, int, char, posn, posn).
:- mode lexer__string_read_char(in, in, out, in, out) is semidet.

:- pragma inline(lexer__string_read_char/5).

lexer__string_read_char(String, Len, Char, Posn0, Posn) :-
	Posn0 = posn(LineNum0, LineOffset0, Offset0),
	Offset0 < Len,
	string__unsafe_index(String, Offset0, Char),
	Offset is Offset0 + 1,
	( Char = '\n' ->
		LineNum is LineNum0 + 1,
		Posn = posn(LineNum, Offset, Offset)
	;
		Posn = posn(LineNum0, LineOffset0, Offset)
	).

:- pred lexer__string_ungetchar(string, posn, posn).
:- mode lexer__string_ungetchar(in, in, out) is det.

lexer__string_ungetchar(String, Posn0, Posn) :-
	Posn0 = posn(LineNum0, LineOffset0, Offset0),
	Offset is Offset0 - 1,
	string__unsafe_index(String, Offset, Char),
	( Char = '\n' ->
		LineNum is LineNum0 - 1,
		Posn = posn(LineNum, Offset, Offset)
	;
		Posn = posn(LineNum0, LineOffset0, Offset)
	).

:- pred lexer__grab_string(string, posn, string, posn, posn).
:- mode lexer__grab_string(in, in, out, in, out) is det.

lexer__grab_string(String, Posn0, SubString, Posn, Posn) :-
	Posn0 = posn(_, _, Offset0),
	Posn = posn(_, _, Offset),
	Count is Offset - Offset0,
	string__unsafe_substring(String, Offset0, Count, SubString).

:- pred lexer__string_set_line_number(int, posn, posn).
:- mode lexer__string_set_line_number(in, in, out) is det.

lexer__string_set_line_number(LineNumber, Posn0, Posn) :-
	Posn0 = posn(_, _, Offset),
	Posn = posn(LineNumber, Offset, Offset).

%-----------------------------------------------------------------------------%

:- pred lexer__get_token(token, token_context, io__state, io__state).
:- mode lexer__get_token(out, out, di, uo) is det.

lexer__get_token(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = ' ' ; Char = '\t' ; Char = '\n' } ->
			lexer__get_token_2(Token, Context)
		; { char__is_upper(Char) ; Char = '_' } ->
			lexer__get_context(Context),
			lexer__get_variable([Char], Token)
		; { char__is_lower(Char) } ->
			lexer__get_context(Context),
			lexer__get_name([Char], Token)
		; { Char = '0' } ->
			lexer__get_context(Context),
			lexer__get_zero(Token)
		; { char__is_digit(Char) } ->
			lexer__get_context(Context),
			lexer__get_number([Char], Token)
		; { lexer__special_token(Char, SpecialToken) } ->
			lexer__get_context(Context),
			{ SpecialToken = open ->
				Token = open_ct
			;
				Token = SpecialToken
			}
		; { Char = ('.') } ->
			lexer__get_context(Context),
			lexer__get_dot(Token)
		; { Char = ('%') } ->
			lexer__skip_to_eol(Token, Context)
		; { Char = '"' ; Char = '''' } ->
			lexer__get_context(Context),
			lexer__get_quoted_name(Char, [], Token)
		; { Char = ('/') } ->
			lexer__get_slash(Token, Context)
		; { Char = ('#') } ->
			lexer__get_source_line_number([], Token, Context)
		; { Char = ('`') } ->
			lexer__get_context(Context),
			{ Token = name("`") }
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_context(Context),
			lexer__get_graphic([Char], Token)
		;
			lexer__get_context(Context),
			{ Token = junk(Char) }
		)
	).

:- pred lexer__string_get_token(string, int, token, token_context, posn, posn).
:- mode lexer__string_get_token(in, in, out, out, in, out) is det.

lexer__string_get_token(String, Len, Token, Context) -->
	=(Posn0),
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ' ' ; Char = '\t' ; Char = '\n' } ->
			lexer__string_get_token_2(String, Len, Token, Context)
		; { char__is_upper(Char) ; Char = '_' } ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context)
		; { char__is_lower(Char) } ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context)
		; { Char = '0' } ->
			lexer__string_get_zero(String, Len, Posn0,
				Token, Context)
		; { char__is_digit(Char) } ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context)
		; { lexer__special_token(Char, SpecialToken) } ->
			lexer__string_get_context(Posn0, Context),
			{ SpecialToken = open ->
				Token = open_ct
			;
				Token = SpecialToken
			}
		; { Char = ('.') } ->
			lexer__string_get_dot(String, Len, Posn0,
				Token, Context)
		; { Char = ('%') } ->
			lexer__string_skip_to_eol(String, Len, Token, Context)
		; { Char = '"' ; Char = '''' } ->
			lexer__string_get_quoted_name(String, Len, Char, [], 
				Posn0, Token, Context)
		; { Char = ('/') } ->
			lexer__string_get_slash(String, Len, Posn0,
				Token, Context)
		; { Char = ('#') } ->
			=(Posn1),
			lexer__string_get_source_line_number(String, Len,
				Posn1, Token, Context)
		; { Char = ('`') } ->
			lexer__string_get_context(Posn0, Context),
			{ Token = name("`") }
		; { lexer__graphic_token_char(Char) } ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_get_context(Posn0, Context),
			{ Token = junk(Char) }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = eof }
	).

%-----------------------------------------------------------------------------%

:- pred lexer__get_token_2(token, token_context, io__state, io__state).
:- mode lexer__get_token_2(out, out, di, uo) is det.

	% This is just like get_token, except that we have already
	% scanned past some whitespace, so '(' gets scanned as `open'
	% rather than `open_ct'.

lexer__get_token_2(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = ' ' ; Char = '\t' ; Char = '\n' } ->
			lexer__get_token_2(Token, Context)
		; { char__is_upper(Char) ; Char = '_' } ->
			lexer__get_context(Context),
			lexer__get_variable([Char], Token)
		; { char__is_lower(Char) } ->
			lexer__get_context(Context),
			lexer__get_name([Char], Token)
		; { Char = '0' } ->
			lexer__get_context(Context),
			lexer__get_zero(Token)
		; { char__is_digit(Char) } ->
			lexer__get_context(Context),
			lexer__get_number([Char], Token)
		; { lexer__special_token(Char, SpecialToken) } ->
			lexer__get_context(Context),
			{ Token = SpecialToken }
		; { Char = ('.') } ->
			lexer__get_context(Context),
			lexer__get_dot(Token)
		; { Char = ('%') } ->
			lexer__skip_to_eol(Token, Context)
		; { Char = '"' ; Char = '''' } ->
			lexer__get_context(Context),
			lexer__get_quoted_name(Char, [], Token)
		; { Char = ('/') } ->
			lexer__get_slash(Token, Context)
		; { Char = ('#') } ->
			lexer__get_source_line_number([], Token, Context)
		; { Char = ('`') } ->
			lexer__get_context(Context),
			{ Token = name("`") }
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_context(Context),
			lexer__get_graphic([Char], Token)
		;
			lexer__get_context(Context),
			{ Token = junk(Char) }
		)
	).

:- pred lexer__string_get_token_2(string, int, token, token_context,
					posn, posn).
:- mode lexer__string_get_token_2(in, in, out, out, in, out) is det.

lexer__string_get_token_2(String, Len, Token, Context) -->
	=(Posn0),
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ' ' ; Char = '\t' ; Char = '\n' } ->
			lexer__string_get_token_2(String, Len, Token, Context)
		; { char__is_upper(Char) ; Char = '_' } ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context)
		; { char__is_lower(Char) } ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context)
		; { Char = '0' } ->
			lexer__string_get_zero(String, Len, Posn0,
				Token, Context)
		; { char__is_digit(Char) } ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context)
		; { lexer__special_token(Char, SpecialToken) } ->
			lexer__string_get_context(Posn0, Context),
			{ Token = SpecialToken }
		; { Char = ('.') } ->
			lexer__string_get_dot(String, Len, Posn0,
				Token, Context)
		; { Char = ('%') } ->
			lexer__string_skip_to_eol(String, Len, Token, Context)
		; { Char = '"' ; Char = '''' } ->
			lexer__string_get_quoted_name(String, Len, Char, [],
				Posn0, Token, Context)
		; { Char = ('/') } ->
			lexer__string_get_slash(String, Len, Posn0,
				Token, Context)
		; { Char = ('#') } ->
			=(Posn1),
			lexer__string_get_source_line_number(String, Len,
				Posn1, Token, Context)
		; { Char = ('`') } ->
			lexer__string_get_context(Posn0, Context),
			{ Token = name("`") }
		; { lexer__graphic_token_char(Char) } ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_get_context(Posn0, Context),
			{ Token = junk(Char) }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = eof }
	).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

:- pred lexer__special_token(char, token).
:- mode lexer__special_token(in, out) is semidet.

lexer__special_token('(', open).	% May get converted to open_ct
lexer__special_token(')', close).
lexer__special_token('[', open_list).
lexer__special_token(']', close_list).
lexer__special_token('{', open_curly).
lexer__special_token('}', close_curly).
lexer__special_token('|', ht_sep).
lexer__special_token(',', comma).
lexer__special_token(';', name(";")).
lexer__special_token('!', name("!")).

lexer__graphic_token_char('#').
lexer__graphic_token_char('$').
lexer__graphic_token_char('&').
lexer__graphic_token_char('*').
lexer__graphic_token_char('+').
lexer__graphic_token_char('-').
lexer__graphic_token_char('.').
lexer__graphic_token_char('/').
lexer__graphic_token_char(':').
lexer__graphic_token_char('<').
lexer__graphic_token_char('=').
lexer__graphic_token_char('>').
lexer__graphic_token_char('?').
lexer__graphic_token_char('@').
lexer__graphic_token_char('^').
lexer__graphic_token_char('~').
lexer__graphic_token_char('\\').

%-----------------------------------------------------------------------------%

:- pred lexer__get_dot(token, io__state, io__state).
:- mode lexer__get_dot(out, di, uo) is det.

lexer__get_dot(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = end }
	; { Result = ok(Char) },
		( { lexer__whitespace_after_dot(Char) } ->
			io__putback_char(Char),
			{ Token = end }
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_graphic([Char, '.'], Token)
		;
			io__putback_char(Char),
			{ Token = name(".") }
		)
	).

:- pred lexer__string_get_dot(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_dot(in, in, in, out, out, in, out) is det.

lexer__string_get_dot(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { lexer__whitespace_after_dot(Char) } ->
			lexer__string_ungetchar(String),
			lexer__string_get_context(Posn0, Context),
			{ Token = end }
		; { lexer__graphic_token_char(Char) } ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__string_get_context(Posn0, Context),
			{ Token = name(".") }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = end }
	).

:- pred lexer__whitespace_after_dot(char).
:- mode lexer__whitespace_after_dot(in) is semidet.

lexer__whitespace_after_dot(' ').
lexer__whitespace_after_dot('\t').
lexer__whitespace_after_dot('\n').
lexer__whitespace_after_dot('%').

%-----------------------------------------------------------------------------%

	% comments

:- pred lexer__skip_to_eol(token, token_context, io__state, io__state).
:- mode lexer__skip_to_eol(out, out, di, uo) is det.

lexer__skip_to_eol(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = eof }
	; { Result = ok(Char) },
		( { Char = '\n' } ->
			lexer__get_token_2(Token, Context)
		;
			lexer__skip_to_eol(Token, Context)
		)
	).

:- pred lexer__string_skip_to_eol(string, int, token, token_context,
					posn, posn).
:- mode lexer__string_skip_to_eol(in, in, out, out, in, out) is det.

lexer__string_skip_to_eol(String, Len, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = '\n' } ->
			lexer__string_get_token_2(String, Len, Token, Context)
		;
			lexer__string_skip_to_eol(String, Len, Token, Context)
		)
	;
		=(Posn),
		lexer__string_get_context(Posn, Context),
		{ Token = eof }
	).

:- pred lexer__get_slash(token, token_context, io__state, io__state).
:- mode lexer__get_slash(out, out, di, uo) is det.

lexer__get_slash(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = name("/") }
	; { Result = ok(Char) },
		( { Char = ('*') } ->
			lexer__get_comment(Token, Context)
		; { lexer__graphic_token_char(Char) } ->
			lexer__get_context(Context),
			lexer__get_graphic([Char, '/'], Token)
		;
			io__putback_char(Char),
			lexer__get_context(Context),
			{ Token = name("/") }
		)
	).

:- pred lexer__string_get_slash(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_slash(in, in, in, out, out, in, out) is det.

lexer__string_get_slash(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ('*') } ->
			lexer__string_get_comment(String, Len, Posn0,
				Token, Context)
		; { lexer__graphic_token_char(Char) } ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__string_get_context(Posn0, Context),
			{ Token = name("/") }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = name("/") }
	).

:- pred lexer__get_comment(token, token_context, io__state, io__state).
:- mode lexer__get_comment(out, out, di, uo) is det.

lexer__get_comment(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = error("unterminated '/*' comment") }
	; { Result = ok(Char) },
		( { Char = ('*') } ->
			lexer__get_comment_2(Token, Context)
		;
			lexer__get_comment(Token, Context)
		)
	).

:- pred lexer__string_get_comment(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_comment(in, in, in, out, out, in, out) is det.

lexer__string_get_comment(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ('*') } ->
			lexer__string_get_comment_2(String, Len, Posn0,
					Token, Context)
		;
			lexer__string_get_comment(String, Len, Posn0,
					Token, Context)
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = error("unterminated '/*' comment") }
	).

:- pred lexer__get_comment_2(token, token_context, io__state, io__state).
:- mode lexer__get_comment_2(out, out, di, uo) is det.

lexer__get_comment_2(Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = error("unterminated '/*' comment") }
	; { Result = ok(Char) },
		( { Char = ('/') } ->
			% end of /* ... */ comment, so get next token
			lexer__get_token_2(Token, Context)
		; { Char = ('*') } ->
			lexer__get_comment_2(Token, Context)
		;
			lexer__get_comment(Token, Context)
		)
	).

:- pred lexer__string_get_comment_2(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_comment_2(in, in, in, out, out, in, out) is det.

lexer__string_get_comment_2(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ('/') } ->
			% end of /* ... */ comment, so get next token
			lexer__string_get_token_2(String, Len, Token, Context)
		; { Char = ('*') } ->
			lexer__string_get_comment_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_get_comment(String, Len, Posn0,
				Token, Context)
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = error("unterminated '/*' comment") }
	).

%-----------------------------------------------------------------------------%

	% quoted names and quoted strings

:- pred lexer__get_quoted_name(char, list(char), token,
				io__state, io__state).
:- mode lexer__get_quoted_name(in, in, out, di, uo) is det.

lexer__get_quoted_name(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) },
		( { Char = QuoteChar } ->
			lexer__get_quoted_name_quote(QuoteChar, Chars, Token)
		; { Char = ('\\') } ->
			lexer__get_quoted_name_escape(QuoteChar, Chars, Token)
		;
			lexer__get_quoted_name(QuoteChar, [Char | Chars], Token)
		)
	).

:- pred lexer__string_get_quoted_name(string, int, char, list(char), posn,
				token, string_token_context, posn, posn).
:- mode lexer__string_get_quoted_name(in, in, in, in, in, out, out, in, out)
	is det.

lexer__string_get_quoted_name(String, Len, QuoteChar, Chars, Posn0,
		Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = QuoteChar } ->
			lexer__string_get_quoted_name_quote(String, Len,
				QuoteChar, Chars, Posn0, Token, Context)
		; { Char = ('\\') } ->
			lexer__string_get_quoted_name_escape(String, Len,
				QuoteChar, Chars, Posn0, Token, Context)
		;
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context)
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = error("unterminated quote") }
	).

:- pred lexer__get_quoted_name_quote(char, list(char), token,
				io__state, io__state).
:- mode lexer__get_quoted_name_quote(in, in, out, di, uo) is det.

lexer__get_quoted_name_quote(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
	; { Result = ok(Char) },
		( { Char = QuoteChar } ->
			lexer__get_quoted_name(QuoteChar, [Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
		)
	).

:- pred lexer__string_get_quoted_name_quote(string, int, char, list(char),
				posn, token, string_token_context, posn, posn).
:- mode lexer__string_get_quoted_name_quote(in, in, in, in, in, out, out,
				in, out) is det.

lexer__string_get_quoted_name_quote(String, Len, QuoteChar, Chars, Posn0,
		Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = QuoteChar } ->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__string_get_context(Posn0, Context),
			{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ lexer__finish_quoted_name(QuoteChar, Chars, Token) }
	).

:- pred lexer__finish_quoted_name(char, list(char), token).
:- mode lexer__finish_quoted_name(in, in, out) is det.

lexer__finish_quoted_name(QuoteChar, Chars, Token) :-
	lexer__rev_char_list_to_string(Chars, String),
	( QuoteChar = '''' ->
		Token = name(String)
	; QuoteChar = '"' ->
		Token = string(String)
	;
		error("lexer.m: unknown quote character")
	).

:- pred lexer__get_quoted_name_escape(char, list(char), token,
					io__state, io__state).
:- mode lexer__get_quoted_name_escape(in, in, out, di, uo) is det.

lexer__get_quoted_name_escape(QuoteChar, Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated quoted name") }
	; { Result = ok(Char) },
		( { Char = '\n' } ->
			lexer__get_quoted_name(QuoteChar, Chars, Token)
		; { lexer__escape_char(Char, EscapedChar) } ->
			{ Chars1 = [EscapedChar | Chars] },
			lexer__get_quoted_name(QuoteChar, Chars1, Token)
		; { Char = 'x' } ->
			lexer__get_hex_escape(QuoteChar, Chars, [], Token)
		; { char__is_octal_digit(Char) } ->
			lexer__get_octal_escape(QuoteChar, Chars, [Char],
				Token)
		;
			{ Token = error("invalid escape character") }
		)
	).

:- pred lexer__string_get_quoted_name_escape(string, int, char, list(char),
					posn, token, string_token_context,
					posn, posn).
:- mode lexer__string_get_quoted_name_escape(in, in, in, in, in, out, out,
					in, out) is det.

lexer__string_get_quoted_name_escape(String, Len, QuoteChar, Chars, Posn0,
		Token, Context) -->
	=(Posn1),
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = '\n' } ->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				Chars, Posn0, Token, Context)
		; { lexer__escape_char(Char, EscapedChar) } ->
			{ Chars1 = [EscapedChar | Chars] },
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				Chars1, Posn0, Token, Context)
		; { Char = 'x' } ->
			lexer__string_get_hex_escape(String, Len, QuoteChar,
				Chars, [], Posn0, Token, Context)
		; { char__is_octal_digit(Char) } ->
			lexer__string_get_octal_escape(String, Len, QuoteChar,
				Chars, [Char], Posn0, Token, Context)
		;
			lexer__string_get_context(Posn1, Context),
			{ Token = error("invalid escape character") }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = error("unterminated quoted name") }
	).

:- pred lexer__escape_char(char, char).
:- mode lexer__escape_char(in, out) is semidet.

lexer__escape_char('a', '\a').
lexer__escape_char('b', '\b').
lexer__escape_char('r', '\r').
lexer__escape_char('f', '\f').
lexer__escape_char('t', '\t').
lexer__escape_char('n', '\n').
lexer__escape_char('v', '\v').
lexer__escape_char('\\', '\\').
lexer__escape_char('''', '''').
lexer__escape_char('"', '"').
lexer__escape_char('`', '`').

:- pred lexer__get_hex_escape(char, list(char), list(char),
				token, io__state, io__state).
:- mode lexer__get_hex_escape(in, in, in, out, di, uo) is det.

lexer__get_hex_escape(QuoteChar, Chars, HexChars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) },
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_escape(QuoteChar, Chars,
						[Char | HexChars], Token)
		; { Char = ('\\') } ->
			lexer__finish_hex_escape(QuoteChar, Chars, HexChars,
				Token)
		;
			{ Token = error("unterminated hex escape") }
		)
	).

:- pred lexer__string_get_hex_escape(string, int, char, list(char), list(char),
				posn, token, string_token_context, posn, posn).
:- mode lexer__string_get_hex_escape(in, in, in, in, in, in, out, out, in, out)
	is det.

lexer__string_get_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
		Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_hex_digit(Char) } ->
			lexer__string_get_hex_escape(String, Len, QuoteChar,
				    Chars, [Char | HexChars], Posn0,
				    Token, Context)
		; { Char = ('\\') } ->
			lexer__string_finish_hex_escape(String, Len, QuoteChar,
				    Chars, HexChars, Posn0, Token, Context)
		;
			lexer__string_get_context(Posn0, Context),
			{ Token = error("unterminated hex escape") }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = error("unterminated quote") }
	).

:- pred lexer__finish_hex_escape(char, list(char), list(char),
				token, io__state, io__state).
:- mode lexer__finish_hex_escape(in, in, in, out, di, uo) is det.

lexer__finish_hex_escape(QuoteChar, Chars, HexChars, Token) -->
	( { HexChars = [] } ->
		{ Token = error("empty hex escape") }
	;
		{ lexer__rev_char_list_to_string(HexChars, HexString) },
		(
			{ string__base_string_to_int(16, HexString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__get_quoted_name(QuoteChar, [Char|Chars], Token) 
		;
			{ Token = error("invalid hex escape") }
		)
	).

:- pred lexer__string_finish_hex_escape(string, int, char, list(char),
				list(char), posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_finish_hex_escape(in, in, in, in, in, in, out, out, in, out)
				is det.

lexer__string_finish_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
		Token, Context) -->
	( { HexChars = [] } ->
		lexer__string_get_context(Posn0, Context),
		{ Token = error("empty hex escape") }
	;
		{ lexer__rev_char_list_to_string(HexChars, HexString) },
		(
			{ string__base_string_to_int(16, HexString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char|Chars], Posn0, Token, Context)
		;
			lexer__string_get_context(Posn0, Context),
			{ Token = error("invalid hex escape") }
		)
	).

:- pred lexer__get_octal_escape(char, list(char), list(char),
				token, io__state, io__state).
:- mode lexer__get_octal_escape(in, in, in, out, di, uo) is det.

lexer__get_octal_escape(QuoteChar, Chars, OctalChars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated quote") }
	; { Result = ok(Char) },
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_escape(QuoteChar, Chars,
						[Char | OctalChars], Token)
		; { Char = ('\\') } ->
			lexer__finish_octal_escape(QuoteChar, Chars, OctalChars,
				Token)
		;
			/****** 
				% We don't report this as an error since
				% we need bug-for-bug compatibility with
				% NU-Prolog
			{ Token = error("unterminated octal escape") }
			******/
			io__putback_char(Char),
			lexer__finish_octal_escape(QuoteChar, Chars, OctalChars,
				Token)
		)
	).

:- pred lexer__string_get_octal_escape(string, int, char, list(char),
				list(char), posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_octal_escape(in, in, in, in, in, in, out, out,
				in, out) is det.

lexer__string_get_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
		Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_octal_digit(Char) } ->
			lexer__string_get_octal_escape(String, Len,
				QuoteChar, Chars, [Char | OctalChars], Posn0,
				Token, Context)
		; { Char = ('\\') } ->
			lexer__string_finish_octal_escape(String, Len,
				QuoteChar, Chars, OctalChars, Posn0,
				Token, Context)
		;
			/****** 
				% We don't report this as an error since
				% we need bug-for-bug compatibility with
				% NU-Prolog
			{ Token = error("unterminated octal escape") }
			******/
			lexer__string_ungetchar(String),
			lexer__string_finish_octal_escape(String, Len,
				QuoteChar, Chars, OctalChars, Posn0,
				Token, Context)
		)
	;
		{ Token = error("unterminated quote") },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__finish_octal_escape(char, list(char), list(char),
				token, io__state, io__state).
:- mode lexer__finish_octal_escape(in, in, in, out, di, uo) is det.

lexer__finish_octal_escape(QuoteChar, Chars, OctalChars, Token) -->
	( { OctalChars = [] } ->
		{ Token = error("empty octal escape") }
	;
		{ lexer__rev_char_list_to_string(OctalChars, OctalString) },
		(
			{ string__base_string_to_int(8, OctalString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__get_quoted_name(QuoteChar, [Char|Chars], Token) 
		;
			{ Token = error("invalid octal escape") }
		)
	).

:- pred lexer__string_finish_octal_escape(string, int, char, list(char),
				list(char), posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_finish_octal_escape(in, in, in, in, in, in, out, out,
				in, out) is det.

lexer__string_finish_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
		Posn0, Token, Context) -->
	( { OctalChars = [] } ->
		{ Token = error("empty octal escape") },
		lexer__string_get_context(Posn0, Context)
	;
		{ lexer__rev_char_list_to_string(OctalChars, OctalString) },
		(
			{ string__base_string_to_int(8, OctalString, Int) },
			{ char__to_int(Char, Int) }
		->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char|Chars], Posn0, Token, Context) 
		;
			{ Token = error("invalid octal escape") },
			lexer__string_get_context(Posn0, Context)
		)
	).

%-----------------------------------------------------------------------------%

	% names and variables

:- pred lexer__get_name(list(char), token, io__state, io__state).
:- mode lexer__get_name(in, out, di, uo) is det.

lexer__get_name(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_string(Chars, Name) },
		{ Token = name(Name) }
	; { Result = ok(Char) },
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__get_name([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, Name) },
			{ Token = name(Name) }
		)
	).

:- pred lexer__string_get_name(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_name(in, in, in, out, out, in, out) is det.

lexer__string_get_name(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, Name),
			{ Token = name(Name) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, Name),
		{ Token = name(Name) },
		lexer__string_get_context(Posn0, Context)
	).

	%
	% A line number directive token is `#' followed by an integer
	% (specifying the line number) followed by a newline.
	% Such a token sets the source line number for the next
	% line, but it is otherwise ignored.  This means that line number
	% directives may appear anywhere that a token may appear, including
	% in the middle of terms.
	% (The source file name can be set with a `:- pragma source_file' 
	% declaration.)
	%

:- pred lexer__get_source_line_number(list(char), token, token_context,
	io__state, io__state).
:- mode lexer__get_source_line_number(in, out, out, di, uo) is det.

lexer__get_source_line_number(Chars, Token, Context) -->
	io__read_char(Result),
	( { Result = error(Error) },
		lexer__get_context(Context),
		{ Token = io_error(Error) }
	; { Result = eof },
		lexer__get_context(Context),
		{ Token = error(
			"unexpected end-of-file in `#' line number directive") }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_source_line_number([Char | Chars],
				Token, Context)
		; { Char = '\n' } ->
			{ lexer__rev_char_list_to_string(Chars, String) },
			(
				{ string__base_string_to_int(10, String, Int) },
				{ Int > 0 }
			->
				io__set_line_number(Int),
				lexer__get_token(Token, Context)
			;
				lexer__get_context(Context),
				{ string__append_list([
					"invalid line number `", String,
					"' in `#' line number directive"],
					Message) },
				{ Token = error(Message) }
			)
		;
			lexer__get_context(Context),
			{ string__from_char_list([Char], String) },
			{ string__append_list([
				"invalid character `", String,
				"' in `#' line number directive"],
				Message) },
			{ Token = error(Message) }
		)
	).

:- pred lexer__string_get_source_line_number(string, int, posn,
		token, token_context, posn, posn).
:- mode lexer__string_get_source_line_number(in, in, in, out, out, in, out)
		is det.

lexer__string_get_source_line_number(String, Len, Posn1, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_source_line_number(String, Len,
				Posn1, Token, Context)
		; { Char = '\n' } ->
			lexer__grab_string(String, Posn1, LineNumString),
			(
				{ string__base_string_to_int(10, LineNumString,
					LineNum) },
				{ LineNum > 0 }
			->
				lexer__string_set_line_number(LineNum),
				lexer__string_get_token(String, Len, Token, Context)
			;
				lexer__string_get_context(Posn1, Context),
				{ string__append_list([
					"invalid line number `", LineNumString,
					"' in `#' line number directive"],
					Message) },
				{ Token = error(Message) }
			)
		;
			lexer__string_get_context(Posn1, Context),
			{ string__from_char_list([Char], DirectiveString) },
			{ string__append_list([
				"invalid character `", DirectiveString,
				"' in `#' line number directive"],
				Message) },
			{ Token = error(Message) }
		)
	;
		lexer__string_get_context(Posn1, Context),
		{ Token = error(
			"unexpected end-of-file in `#' line number directive") }
	).

:- pred lexer__get_graphic(list(char), token, io__state, io__state).
:- mode lexer__get_graphic(in, out, di, uo) is det.

lexer__get_graphic(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_string(Chars, Name) },
		{ Token = name(Name) }
	; { Result = ok(Char) },
		( { lexer__graphic_token_char(Char) } ->
			lexer__get_graphic([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, Name) },
			{ Token = name(Name) }
		)
	).

:- pred lexer__string_get_graphic(string, int, posn, token,
			string_token_context, posn, posn).
:- mode lexer__string_get_graphic(in, in, in, out, out, in, out) is det.

lexer__string_get_graphic(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { lexer__graphic_token_char(Char) } ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, Name),
			{ Token = name(Name) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, Name),
		lexer__string_get_context(Posn0, Context),
		{ Token = name(Name) }
	).

:- pred lexer__get_variable(list(char), token, io__state, io__state).
:- mode lexer__get_variable(in, out, di, uo) is det.

lexer__get_variable(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_string(Chars, VariableName) },
		{ Token = variable(VariableName) }
	; { Result = ok(Char) },
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__get_variable([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_string(Chars, VariableName) },
			{ Token = variable(VariableName) }
		)
	).

:- pred lexer__string_get_variable(string, int, posn, token,
					string_token_context, posn, posn).
:- mode lexer__string_get_variable(in, in, in, out, out, in, out) is det.

lexer__string_get_variable(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_alnum_or_underscore(Char) } ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, VariableName),
			{ Token = variable(VariableName) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, VariableName),
		{ Token = variable(VariableName) },
		lexer__string_get_context(Posn0, Context)
	).

%-----------------------------------------------------------------------------%

	% integer and float literals

:- pred lexer__get_zero(token, io__state, io__state).
:- mode lexer__get_zero(out, di, uo) is det.

lexer__get_zero(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = integer(0) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_number([Char], Token)
		; { Char = '''' } ->
			lexer__get_char_code(Token)
		; { Char = 'b' } ->
			lexer__get_binary(Token)
		; { Char = 'o' } ->
			lexer__get_octal(Token)
		; { Char = 'x' } ->
			lexer__get_hex(Token)
		; { Char = ('.') } ->
			lexer__get_int_dot(['0'], Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char, '0'], Token)
		;
			io__putback_char(Char),
			{ Token = integer(0) }
		)
	).

:- pred lexer__string_get_zero(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_zero(in, in, in, out, out, in, out) is det.

lexer__string_get_zero(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context)
		; { Char = '''' } ->
			lexer__string_get_char_code(String, Len, Posn0,
				Token, Context)
		; { Char = 'b' } ->
			lexer__string_get_binary(String, Len, Posn0,
				Token, Context)
		; { Char = 'o' } ->
			lexer__string_get_octal(String, Len, Posn0,
				Token, Context)
		; { Char = 'x' } ->
			lexer__string_get_hex(String, Len, Posn0,
				Token, Context)
		; { Char = ('.') } ->
			lexer__string_get_int_dot(String, Len, Posn0,
				Token, Context)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__string_get_context(Posn0, Context),
			{ Token = integer(0) }
		)
	;
		lexer__string_get_context(Posn0, Context),
		{ Token = integer(0) }
	).

:- pred lexer__get_char_code(token, io__state, io__state).
:- mode lexer__get_char_code(out, di, uo) is det.

lexer__get_char_code(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated char code constant") }
	; { Result = ok(Char) },
		{ char__to_int(Char, CharCode) },
		{ Token = integer(CharCode) }
	).

:- pred lexer__string_get_char_code(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_char_code(in, in, in, out, out, in, out) is det.

lexer__string_get_char_code(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		{ char__to_int(Char, CharCode) },
		{ Token = integer(CharCode) },
		lexer__string_get_context(Posn0, Context)
	;
		{ Token = error("unterminated char code constant") },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_binary(token, io__state, io__state).
:- mode lexer__get_binary(out, di, uo) is det.

lexer__get_binary(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated binary constant") }
	; { Result = ok(Char) },
		( { char__is_binary_digit(Char) } ->
			lexer__get_binary_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated binary constant") }
		)
	).

:- pred lexer__string_get_binary(string, int, posn, token, string_token_context,
		posn, posn).
:- mode lexer__string_get_binary(in, in, in, out, out, in, out) is det.

lexer__string_get_binary(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_binary_digit(Char) } ->
			lexer__string_get_binary_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			{ Token = error("unterminated binary constant") },
			lexer__string_get_context(Posn0, Context)
		)
	;
		{ Token = error("unterminated binary constant") },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_binary_2(list(char), token, io__state, io__state).
:- mode lexer__get_binary_2(in, out, di, uo) is det.

lexer__get_binary_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_int(Chars, 2, Token) }
	; { Result = ok(Char) },
		( { char__is_binary_digit(Char) } ->
			lexer__get_binary_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 2, Token) }
		)
	).

:- pred lexer__string_get_binary_2(string, int, posn, token,
			string_token_context, posn, posn).
:- mode lexer__string_get_binary_2(in, in, in, out, out, in, out) is det.

lexer__string_get_binary_2(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_binary_digit(Char) } ->
			lexer__string_get_binary_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, BinaryString),
			{ lexer__conv_string_to_int(BinaryString, 2, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString),
		{ lexer__conv_string_to_int(BinaryString, 2, Token) },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_octal(token, io__state, io__state).
:- mode lexer__get_octal(out, di, uo) is det.

lexer__get_octal(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated octal constant") }
	; { Result = ok(Char) },
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated octal constant") }
		)
	).

:- pred lexer__string_get_octal(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_octal(in, in, in, out, out, in, out) is det.

lexer__string_get_octal(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_octal_digit(Char) } ->
			lexer__string_get_octal_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			{ Token = error("unterminated octal constant") },
			lexer__string_get_context(Posn0, Context)
		)
	;
		{ Token = error("unterminated octal constant") },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_octal_2(list(char), token, io__state, io__state).
:- mode lexer__get_octal_2(in, out, di, uo) is det.

lexer__get_octal_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_int(Chars, 8, Token) }
	; { Result = ok(Char) },
		( { char__is_octal_digit(Char) } ->
			lexer__get_octal_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 8, Token) }
		)
	).

:- pred lexer__string_get_octal_2(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_octal_2(in, in, in, out, out, in, out) is det.

lexer__string_get_octal_2(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_octal_digit(Char) } ->
			lexer__string_get_octal_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, BinaryString),
			{ lexer__conv_string_to_int(BinaryString, 8, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString),
		{ lexer__conv_string_to_int(BinaryString, 8, Token) },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_hex(token, io__state, io__state).
:- mode lexer__get_hex(out, di, uo) is det.

lexer__get_hex(Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated hex constant") }
	; { Result = ok(Char) },
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_2([Char], Token)
		;
			io__putback_char(Char),
			{ Token = error("unterminated hex constant") }
		)
	).

:- pred lexer__string_get_hex(string, int, posn, token, string_token_context,
				posn, posn).
:- mode lexer__string_get_hex(in, in, in, out, out, in, out) is det.

lexer__string_get_hex(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_hex_digit(Char) } ->
			lexer__string_get_hex_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			{ Token = error("unterminated hex constant") },
			lexer__string_get_context(Posn0, Context)
		)
	;
		{ Token = error("unterminated hex constant") },
		lexer__string_get_context(Posn0, Context)
	).


:- pred lexer__get_hex_2(list(char), token, io__state, io__state).
:- mode lexer__get_hex_2(in, out, di, uo) is det.

lexer__get_hex_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_int(Chars, 16, Token) }
	; { Result = ok(Char) },
		( { char__is_hex_digit(Char) } ->
			lexer__get_hex_2([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 16, Token) }
		)
	).

:- pred lexer__string_get_hex_2(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_hex_2(in, in, in, out, out, in, out) is det.

lexer__string_get_hex_2(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_hex_digit(Char) } ->
			lexer__string_get_hex_2(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, BinaryString),
			{ lexer__conv_string_to_int(BinaryString, 16, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString),
		{ lexer__conv_string_to_int(BinaryString, 16, Token) },
		lexer__string_get_context(Posn0, Context)
	).


:- pred lexer__get_number(list(char), token, io__state, io__state).
:- mode lexer__get_number(in, out, di, uo) is det.

lexer__get_number(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_int(Chars, 10, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_number([Char | Chars], Token)
		; { Char = ('.') } ->
			lexer__get_int_dot(Chars, Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_int(Chars, 10, Token) }
		)
	).

:- pred lexer__string_get_number(string, int, posn, token,
					string_token_context, posn, posn).
:- mode lexer__string_get_number(in, in, in, out, out, in, out) is det.

lexer__string_get_number(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context)
		; { Char = ('.') } ->
			lexer__string_get_int_dot(String, Len, Posn0,
				Token, Context)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, NumberString),
			{ lexer__conv_string_to_int(NumberString, 10, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, NumberString),
		{ lexer__conv_string_to_int(NumberString, 10, Token) },
		lexer__string_get_context(Posn0, Context)
	).

	% XXX the float literal syntax doesn't match ISO Prolog

:- pred lexer__get_int_dot(list(char), token, io__state, io__state).
:- mode lexer__get_int_dot(in, out, di, uo) is det.

lexer__get_int_dot(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		io__putback_char('.'),
		{ lexer__rev_char_list_to_int(Chars, 10, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_decimals([Char, '.' | Chars], Token)
		;
			io__putback_char(Char),
			io__putback_char('.'),
			{ lexer__rev_char_list_to_int(Chars, 10, Token) }
		)
	).

:- pred lexer__string_get_int_dot(string, int, posn, token,
					string_token_context, posn, posn).
:- mode lexer__string_get_int_dot(in, in, in, out, out, in, out) is det.

lexer__string_get_int_dot(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_float_decimals(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, NumberString),
			{ lexer__conv_string_to_int(NumberString, 10, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__string_ungetchar(String),
		lexer__grab_string(String, Posn0, NumberString),
		{ lexer__conv_string_to_int(NumberString, 10, Token) },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_float_decimals(list(char), token, io__state, io__state).
:- mode lexer__get_float_decimals(in, out, di, uo) is det.

	% we've read past the decimal point, so now get the decimals

lexer__get_float_decimals(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_decimals([Char | Chars], Token)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__get_float_exponent([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_float(Chars, Token) }
		)
	).

:- pred lexer__string_get_float_decimals(string, int, posn, token,
					string_token_context, posn, posn).
:- mode lexer__string_get_float_decimals(in, in, in, out, out, in, out) is det.

lexer__string_get_float_decimals(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_float_decimals(String, Len, Posn0,
				Token, Context)
		; { Char = 'e' ; Char = 'E' } ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, FloatString),
			{ lexer__conv_to_float(FloatString, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, FloatString),
		{ lexer__conv_to_float(FloatString, Token) },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_float_exponent(list(char), token, io__state, io__state).
:- mode lexer__get_float_exponent(in, out, di, uo) is det.

lexer__get_float_exponent(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { Char = ('+') ; Char = ('-') } ->
			lexer__get_float_exponent_2([Char | Chars], Token)
		; { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ Token =
			  error("unterminated exponent in float token") }
		)
	).

:- pred lexer__string_get_float_exponent(string, int, posn, token, 	
				string_token_context, posn, posn).
:- mode lexer__string_get_float_exponent(in, in, in, out, out, in, out) is det.

lexer__string_get_float_exponent(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { Char = ('+') ; Char = ('-') } ->
			lexer__string_get_float_exponent_2(String, Len, Posn0,
				Token, Context)
		; { char__is_digit(Char) } ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			{ Token =
			  error("unterminated exponent in float token") },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, FloatString),
		{ lexer__conv_to_float(FloatString, Token) },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_float_exponent_2(list(char), token,
				io__state, io__state).
:- mode lexer__get_float_exponent_2(in, out, di, uo) is det.

	% we've read past the E signalling the start of the exponent -
	% make sure that there's at least one digit following,
	% and then get the remaining digits

lexer__get_float_exponent_2(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ Token = error("unterminated exponent in float token") }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ Token =
			  error("unterminated exponent in float token") }
		)
	).

:- pred lexer__string_get_float_exponent_2(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_float_exponent_2(in, in, in, out, out, in, out)
				is det.

	% we've read past the E signalling the start of the exponent -
	% make sure that there's at least one digit following,
	% and then get the remaining digits

lexer__string_get_float_exponent_2(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			{ Token =
			  error("unterminated exponent in float token") },
			lexer__string_get_context(Posn0, Context)
		)
	;
		{ Token = error("unterminated exponent in float token") },
		lexer__string_get_context(Posn0, Context)
	).

:- pred lexer__get_float_exponent_3(list(char), token,
					io__state, io__state).
:- mode lexer__get_float_exponent_3(in, out, di, uo) is det.

	% we've read past the first digit of the exponent -
	% now get the remaining digits

lexer__get_float_exponent_3(Chars, Token) -->
	io__read_char(Result),
	( { Result = error(Error) },
		{ Token = io_error(Error) }
	; { Result = eof },
		{ lexer__rev_char_list_to_float(Chars, Token) }
	; { Result = ok(Char) },
		( { char__is_digit(Char) } ->
			lexer__get_float_exponent_3([Char | Chars], Token)
		;
			io__putback_char(Char),
			{ lexer__rev_char_list_to_float(Chars, Token) }
		)
	).

:- pred lexer__string_get_float_exponent_3(string, int, posn, token,
				string_token_context, posn, posn).
:- mode lexer__string_get_float_exponent_3(in, in, in, out, out, in, out)
				is det.

lexer__string_get_float_exponent_3(String, Len, Posn0, Token, Context) -->
	( lexer__string_read_char(String, Len, Char) ->
		( { char__is_digit(Char) } ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context)
		;
			lexer__string_ungetchar(String),
			lexer__grab_string(String, Posn0, FloatString),
			{ lexer__conv_to_float(FloatString, Token) },
			lexer__string_get_context(Posn0, Context)
		)
	;
		lexer__grab_string(String, Posn0, FloatString),
		{ lexer__conv_to_float(FloatString, Token) },
		lexer__string_get_context(Posn0, Context)
	).

%-----------------------------------------------------------------------------%

	% Utility routines

:- pred lexer__rev_char_list_to_int(list(char), int, token).
:- mode lexer__rev_char_list_to_int(in, in, out) is det.

lexer__rev_char_list_to_int(RevChars, Base, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	lexer__conv_string_to_int(String, Base, Token).

:- pred lexer__conv_string_to_int(string, int, token).
:- mode lexer__conv_string_to_int(in, in, out) is det.

lexer__conv_string_to_int(String, Base, Token) :-
	( string__base_string_to_int(Base, String, Int) ->
		Token = integer(Int)
	;
		Token = error("invalid integer token")
	).

:- pred lexer__rev_char_list_to_float(list(char), token).
:- mode lexer__rev_char_list_to_float(in, out) is det.

lexer__rev_char_list_to_float(RevChars, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	lexer__conv_to_float(String, Token).

:- pred lexer__conv_to_float(string, token).
:- mode lexer__conv_to_float(in, out) is det.

lexer__conv_to_float(String, Token) :-
	( string__to_float(String, Float) ->
		Token = float(Float)
	;
		Token = error("invalid float token")
	).

:- pred lexer__rev_char_list_to_string(list(char), string).
:- mode lexer__rev_char_list_to_string(in, out) is det.

lexer__rev_char_list_to_string(RevChars, String) :-
       string__from_rev_char_list(RevChars, String).

%-----------------------------------------------------------------------------%
