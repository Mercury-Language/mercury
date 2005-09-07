%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2005 The University of Melbourne.
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

:- import_module char.
:- import_module io.

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
	;	eof			% end-of-file
	;	integer_dot(int).	% the lexer will never return this.
					% The integer_dot/1 token is used
					% internally in the lexer, to keep
					% the grammar LL(1) so that only one
					% character of pushback is needed.
					% But the lexer will convert
					% integer_dot/1 tokens to integer/1
					% tokens before returning them.

	% For every token, we record the line number of the line on
	% which the token occurred.
	%
:- type token_context == int.	% line number

	% This "fat list" representation is more efficient than a list of
	% pairs.
	%
:- type token_list
	--->	token_cons(token, token_context, token_list)
	;	token_nil.

	% Read a list of tokens from the current input stream.
	% Keep reading until we encounter either an `end' token
	% (i.e. a full stop followed by whitespace) or the end-of-file.
	%
:- pred lexer__get_token_list(token_list::out, io::di, io::uo) is det.

	% The type `offset' represents a (zero-based) offset into a string.
	%
:- type offset == int.

	% lexer__string_get_token_list(String, MaxOffset, Tokens,
	%		InitialPos, FinalPos):
	% Scan a list of tokens from a string,
	% starting at the current offset specified by InitialPos.
	% Keep scanning until either we encounter either an `end' token
	% (i.e. a full stop followed by whitespace) or until
	% we reach MaxOffset.  (MaxOffset must be =< the length of the string.)
	% Return the tokens scanned in Tokens, and return the position
	% one character past the end of the last token in FinalPos.
	%
:- pred lexer__string_get_token_list(string::in, offset::in, token_list::out,
	posn::in, posn::out) is det.

	% lexer__string_get_token_list(String, Tokens, InitialPos, FinalPos):
	% calls string_get_token_list/5 above with MaxPos = length of String.
	%
:- pred lexer__string_get_token_list(string::in, token_list::out,
	posn::in, posn::out) is det.

	% Convert a token to a human-readable string describing the token.
	%
:- pred lexer__token_to_string(token::in, string::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%

:- interface.

	% lexer__graphic_token_char(Char): true iff `Char'
	% is "graphic token char" (ISO Prolog 6.4.2).
	% This is exported for use by term_io__quote_atom.
:- pred lexer__graphic_token_char(char::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module term.

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
lexer__token_to_string(integer_dot(Int), String) :-
	string__int_to_string(Int, IntString),
	string__append_list(["integer `", IntString, "'."], String).

	% We build the tokens up as lists of characters in reverse order.
	% When we get to the end of each token, we call
	% `lexer__rev_char_list_to_string/2' to convert that representation
	% into a string.

	% Comments of the form
	%	foo --> bar . baz
	% mean that we are parsing a `foo', and we've already scanned
	% past the `bar', so now we need to match with a `baz'.

lexer__get_token_list(Tokens, !IO) :-
	lexer__get_token(Token, Context, !IO),
	lexer__get_token_list_2(Token, Context, Tokens, !IO).

:- pred lexer__get_token_list_2(token::in, token_context::in, token_list::out,
	io::di, io::uo) is det.

lexer__get_token_list_2(Token0, Context0, Tokens, !IO) :-
	( Token0 = eof ->
		Tokens = token_nil
	; ( Token0 = end ; Token0 = error(_) ; Token0 = io_error(_) ) ->
		Tokens = token_cons(Token0, Context0, token_nil)
	; Token0 = integer_dot(Int) ->
		lexer__get_context(Context1, !IO),
		lexer__get_dot(Token1, !IO),
		lexer__get_token_list_2(Token1, Context1, Tokens1, !IO),
		Tokens = token_cons(integer(Int), Context0, Tokens1)
	;
		lexer__get_token(Token1, Context1, !IO),
		lexer__get_token_list_2(Token1, Context1, Tokens1, !IO),
		Tokens = token_cons(Token0, Context0, Tokens1)
	).

lexer__string_get_token_list(String, Tokens, !Posn) :-
	string__length(String, Len),
	lexer__string_get_token_list(String, Len, Tokens, !Posn).

lexer__string_get_token_list(String, Len, Tokens, !Posn) :-
	lexer__string_get_token(String, Len, Token, Context, !Posn),
	( Token = eof ->
		Tokens = token_nil
	; ( Token = end ; Token = error(_) ; Token = io_error(_) ) ->
		Tokens = token_cons(Token, Context, token_nil)
	;
		Tokens = token_cons(Token, Context, Tokens1),
		lexer__string_get_token_list(String, Len, Tokens1, !Posn)
	).

%-----------------------------------------------------------------------------%

% some low-level routines

:- pred lexer__get_context(token_context::out, io::di, io::uo) is det.

lexer__get_context(Context, !IO) :-
	io__get_line_number(Context, !IO).

:- type string_token_context == token_context.

:- pred lexer__string_get_context(posn::in, string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_context(StartPosn, Context, !Posn) :-
	StartPosn = posn(StartLineNum, _, _),
	Context = StartLineNum.
	% In future, we might want to modify this code to read something
	% like this:
	%	posn_to_line_and_column(StartPosn, StartLineNum, StartColumn),
	%	posn_to_line_and_column(!.Posn, EndLineNum, EndColumn),
	%	Context = detailed(StartLine, StartColumn, EndLine, EndColumn).

:- pred lexer__string_read_char(string::in, int::in, char::out,
	posn::in, posn::out) is semidet.

:- pragma inline(lexer__string_read_char/5).

lexer__string_read_char(String, Len, Char, Posn0, Posn) :-
	Posn0 = posn(LineNum0, LineOffset0, Offset0),
	Offset0 < Len,
	string__unsafe_index(String, Offset0, Char),
	Offset = Offset0 + 1,
	( Char = '\n' ->
		LineNum = LineNum0 + 1,
		Posn = posn(LineNum, Offset, Offset)
	;
		Posn = posn(LineNum0, LineOffset0, Offset)
	).

:- pred lexer__string_ungetchar(string::in, posn::in, posn::out) is det.

lexer__string_ungetchar(String, Posn0, Posn) :-
	Posn0 = posn(LineNum0, LineOffset0, Offset0),
	Offset = Offset0 - 1,
	string__unsafe_index(String, Offset, Char),
	( Char = '\n' ->
		LineNum = LineNum0 - 1,
		Posn = posn(LineNum, Offset, Offset)
	;
		Posn = posn(LineNum0, LineOffset0, Offset)
	).

:- pred lexer__grab_string(string::in, posn::in, string::out,
	posn::in, posn::out) is det.

lexer__grab_string(String, Posn0, SubString, Posn, Posn) :-
	Posn0 = posn(_, _, Offset0),
	Posn = posn(_, _, Offset),
	Count = Offset - Offset0,
	string__unsafe_substring(String, Offset0, Count, SubString).

:- pred lexer__string_set_line_number(int::in, posn::in, posn::out) is det.

lexer__string_set_line_number(LineNumber, Posn0, Posn) :-
	Posn0 = posn(_, _, Offset),
	Posn = posn(LineNumber, Offset, Offset).

%-----------------------------------------------------------------------------%

:- pred lexer__get_token(token::out, token_context::out, io::di, io::uo)
	is det.

lexer__get_token(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = eof
	;
		Result = ok(Char),
		( char__is_whitespace(Char) ->
			lexer__get_token_2(Token, Context, !IO)
		; ( char__is_upper(Char) ; Char = '_' ) ->
			lexer__get_context(Context, !IO),
			lexer__get_variable([Char], Token, !IO)
		; char__is_lower(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_name([Char], Token, !IO)
		; Char = '0' ->
			lexer__get_context(Context, !IO),
			lexer__get_zero(Token, !IO)
		; char__is_digit(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_number([Char], Token, !IO)
		; lexer__special_token(Char, SpecialToken) ->
			lexer__get_context(Context, !IO),
			( SpecialToken = open ->
				Token = open_ct
			;
				Token = SpecialToken
			)
		; Char = ('.') ->
			lexer__get_context(Context, !IO),
			lexer__get_dot(Token, !IO)
		; Char = ('%') ->
			lexer__skip_to_eol(Token, Context, !IO)
		; ( Char = '"' ; Char = '''' ) ->
			lexer__get_context(Context, !IO),
			lexer__get_quoted_name(Char, [], Token, !IO)
		; Char = ('/') ->
			lexer__get_slash(Token, Context, !IO)
		; Char = ('#') ->
			lexer__get_source_line_number([], Token, Context, !IO)
		; Char = ('`') ->
			lexer__get_context(Context, !IO),
			Token = name("`")
		; lexer__graphic_token_char(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_graphic([Char], Token, !IO)
		;
			lexer__get_context(Context, !IO),
			Token = junk(Char)
		)
	).

:- pred lexer__string_get_token(string::in, int::in, token::out,
	token_context::out, posn::in, posn::out) is det.

lexer__string_get_token(String, Len, Token, Context, !Posn) :-
	Posn0 = !.Posn,
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_whitespace(Char) ->
			lexer__string_get_token_2(String, Len, Token, Context,
				!Posn)
		; ( char__is_upper(Char) ; Char = '_' ) ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context, !Posn)
		; char__is_lower(Char) ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = '0' ->
			lexer__string_get_zero(String, Len, Posn0,
				Token, Context, !Posn)
		; char__is_digit(Char) ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context, !Posn)
		; lexer__special_token(Char, SpecialToken) ->
			lexer__string_get_context(Posn0, Context, !Posn),
			( SpecialToken = open ->
				Token = open_ct
			;
				Token = SpecialToken
			)
		; Char = ('.') ->
			lexer__string_get_dot(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('%') ->
			lexer__string_skip_to_eol(String, Len, Token, Context,
			!Posn)
		; ( Char = '"' ; Char = '''' ) ->
			lexer__string_get_quoted_name(String, Len, Char, [],
				Posn0, Token, Context, !Posn)
		; Char = ('/') ->
			lexer__string_get_slash(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('#') ->
			lexer__string_get_source_line_number(String, Len,
				!.Posn, Token, Context, !Posn)
		; Char = ('`') ->
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = name("`")
		; lexer__graphic_token_char(Char) ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = junk(Char)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = eof
	).

%-----------------------------------------------------------------------------%

:- pred lexer__get_token_2(token::out, token_context::out, io::di, io::uo)
	is det.

	% This is just like get_token, except that we have already
	% scanned past some whitespace, so '(' gets scanned as `open'
	% rather than `open_ct'.

lexer__get_token_2(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = eof
	;
		Result = ok(Char),
		( char__is_whitespace(Char) ->
			lexer__get_token_2(Token, Context, !IO)
		; ( char__is_upper(Char) ; Char = '_' ) ->
			lexer__get_context(Context, !IO),
			lexer__get_variable([Char], Token, !IO)
		; char__is_lower(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_name([Char], Token, !IO)
		; Char = '0' ->
			lexer__get_context(Context, !IO),
			lexer__get_zero(Token, !IO)
		; char__is_digit(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_number([Char], Token, !IO)
		; lexer__special_token(Char, SpecialToken) ->
			lexer__get_context(Context, !IO),
			Token = SpecialToken
		; Char = ('.') ->
			lexer__get_context(Context, !IO),
			lexer__get_dot(Token, !IO)
		; Char = ('%') ->
			lexer__skip_to_eol(Token, Context, !IO)
		; ( Char = '"' ; Char = '''' ) ->
			lexer__get_context(Context, !IO),
			lexer__get_quoted_name(Char, [], Token, !IO)
		; Char = ('/') ->
			lexer__get_slash(Token, Context, !IO)
		; Char = ('#') ->
			lexer__get_source_line_number([], Token, Context, !IO)
		; Char = ('`') ->
			lexer__get_context(Context, !IO),
			Token = name("`")
		; lexer__graphic_token_char(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_graphic([Char], Token, !IO)
		;
			lexer__get_context(Context, !IO),
			Token = junk(Char)
		)
	).

:- pred lexer__string_get_token_2(string::in, int::in, token::out,
	token_context::out, posn::in, posn::out) is det.

lexer__string_get_token_2(String, Len, Token, Context, !Posn) :-
	Posn0 = !.Posn,
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_whitespace(Char) ->
			lexer__string_get_token_2(String, Len, Token, Context,
				!Posn)
		; ( char__is_upper(Char) ; Char = '_' ) ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context, !Posn)
		; char__is_lower(Char) ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = '0' ->
			lexer__string_get_zero(String, Len, Posn0,
				Token, Context, !Posn)
		; char__is_digit(Char) ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context, !Posn)
		; lexer__special_token(Char, SpecialToken) ->
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = SpecialToken
		; Char = ('.') ->
			lexer__string_get_dot(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('%') ->
			lexer__string_skip_to_eol(String, Len, Token, Context,
			!Posn)
		; ( Char = '"' ; Char = '''' ) ->
			lexer__string_get_quoted_name(String, Len, Char, [],
				Posn0, Token, Context, !Posn)
		; Char = ('/') ->
			lexer__string_get_slash(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('#') ->
			lexer__string_get_source_line_number(String, Len,
				!.Posn, Token, Context, !Posn)
		; Char = ('`') ->
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = name("`")
		; lexer__graphic_token_char(Char) ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = junk(Char)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = eof
	).

%-----------------------------------------------------------------------------%

:- pred lexer__special_token(char::in, token::out) is semidet.

lexer__special_token('(', open).	% May get converted to open_ct
lexer__special_token(')', close).
lexer__special_token('[', open_list).
lexer__special_token(']', close_list).
lexer__special_token('{', open_curly).
lexer__special_token('}', close_curly).
lexer__special_token('|', ht_sep).
lexer__special_token(',', comma).
lexer__special_token(';', name(";")).

lexer__graphic_token_char('!').
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

:- pred lexer__get_dot(token::out, io::di, io::uo) is det.

lexer__get_dot(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = end
	;
		Result = ok(Char),
		( lexer__whitespace_after_dot(Char) ->
			io__putback_char(Char, !IO),
			Token = end
		; lexer__graphic_token_char(Char) ->
			lexer__get_graphic([Char, '.'], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = name(".")
		)
	).

:- pred lexer__string_get_dot(string::in, int::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_dot(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( lexer__whitespace_after_dot(Char) ->
			lexer__string_ungetchar(String, !Posn),
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = end
		; lexer__graphic_token_char(Char) ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = name(".")
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = end
	).

:- pred lexer__whitespace_after_dot(char::in) is semidet.

lexer__whitespace_after_dot(Char) :-
	( char__is_whitespace(Char) 
	; Char = '%'
	).

%-----------------------------------------------------------------------------%

	% comments

:- pred lexer__skip_to_eol(token::out, token_context::out, io::di, io::uo)
	is det.

lexer__skip_to_eol(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = eof
	;
		Result = ok(Char),
		( Char = '\n' ->
			lexer__get_token_2(Token, Context, !IO)
		;
			lexer__skip_to_eol(Token, Context, !IO)
		)
	).

:- pred lexer__string_skip_to_eol(string::in, int::in, token::out,
	token_context::out, posn::in, posn::out) is det.

lexer__string_skip_to_eol(String, Len, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = '\n' ->
			lexer__string_get_token_2(String, Len, Token, Context,
				!Posn)
		;
			lexer__string_skip_to_eol(String, Len, Token, Context,
				!Posn)
		)
	;
		lexer__string_get_context(!.Posn, Context, !Posn),
		Token = eof
	).

:- pred lexer__get_slash(token::out, token_context::out, io::di, io::uo)
	is det.

lexer__get_slash(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = name("/")
	;
		Result = ok(Char),
		( Char = ('*') ->
			lexer__get_comment(Token, Context, !IO)
		; lexer__graphic_token_char(Char) ->
			lexer__get_context(Context, !IO),
			lexer__get_graphic([Char, '/'], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__get_context(Context, !IO),
			Token = name("/")
		)
	).

:- pred lexer__string_get_slash(string::in, int::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_slash(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = ('*') ->
			lexer__string_get_comment(String, Len, Posn0,
				Token, Context, !Posn)
		; lexer__graphic_token_char(Char) ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = name("/")
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = name("/")
	).

:- pred lexer__get_comment(token::out, token_context::out,
	io::di, io::uo) is det.

lexer__get_comment(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = error("unterminated '/*' comment")
	;
		Result = ok(Char),
		( Char = ('*') ->
			lexer__get_comment_2(Token, Context, !IO)
		;
			lexer__get_comment(Token, Context, !IO)
		)
	).

:- pred lexer__string_get_comment(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_comment(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = ('*') ->
			lexer__string_get_comment_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_get_comment(String, Len, Posn0,
				Token, Context, !Posn)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("unterminated '/*' comment")
	).

:- pred lexer__get_comment_2(token::out, token_context::out, io::di, io::uo)
	is det.

lexer__get_comment_2(Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = error("unterminated '/*' comment")
	;
		Result = ok(Char),
		( Char = ('/') ->
			% end of /* ... */ comment, so get next token
			lexer__get_token_2(Token, Context, !IO)
		; Char = ('*') ->
			lexer__get_comment_2(Token, Context, !IO)
		;
			lexer__get_comment(Token, Context, !IO)
		)
	).

:- pred lexer__string_get_comment_2(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_comment_2(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = ('/') ->
			% end of /* ... */ comment, so get next token
			lexer__string_get_token_2(String, Len, Token, Context,
				!Posn)
		; Char = ('*') ->
			lexer__string_get_comment_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_get_comment(String, Len, Posn0,
				Token, Context, !Posn)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("unterminated '/*' comment")
	).

%-----------------------------------------------------------------------------%

	% quoted names and quoted strings

:- pred lexer__get_quoted_name(char::in, list(char)::in, token::out,
	io::di, io::uo) is det.

lexer__get_quoted_name(QuoteChar, Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated quote")
	;
		Result = ok(Char),
		( Char = QuoteChar ->
			lexer__get_quoted_name_quote(QuoteChar, Chars, Token,
				!IO)
		; Char = ('\\') ->
			lexer__get_quoted_name_escape(QuoteChar, Chars, Token,
				!IO)
		;
			lexer__get_quoted_name(QuoteChar, [Char | Chars],
				Token, !IO)
		)
	).

:- pred lexer__string_get_quoted_name(string::in, int::in, char::in,
	list(char)::in, posn::in, token::out, string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_quoted_name(String, Len, QuoteChar, Chars, Posn0,
		Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = QuoteChar ->
			lexer__string_get_quoted_name_quote(String, Len,
				QuoteChar, Chars, Posn0, Token, Context, !Posn)
		; Char = ('\\') ->
			lexer__string_get_quoted_name_escape(String, Len,
				QuoteChar, Chars, Posn0, Token, Context, !Posn)
		;
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context, !Posn)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("unterminated quote")
	).

:- pred lexer__get_quoted_name_quote(char::in, list(char)::in, token::out,
	io::di, io::uo) is det.

lexer__get_quoted_name_quote(QuoteChar, Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__finish_quoted_name(QuoteChar, Chars, Token)
	;
		Result = ok(Char),
		( Char = QuoteChar ->
			lexer__get_quoted_name(QuoteChar, [Char | Chars],
				Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__finish_quoted_name(QuoteChar, Chars, Token)
		)
	).

:- pred lexer__string_get_quoted_name_quote(string::in, int::in, char::in,
	list(char)::in, posn::in, token::out, string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_quoted_name_quote(String, Len, QuoteChar, Chars, Posn0,
		Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = QuoteChar ->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__string_get_context(Posn0, Context, !Posn),
			lexer__finish_quoted_name(QuoteChar, Chars, Token)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		lexer__finish_quoted_name(QuoteChar, Chars, Token)
	).

:- pred lexer__finish_quoted_name(char::in, list(char)::in, token::out) is det.

lexer__finish_quoted_name(QuoteChar, Chars, Token) :-
	lexer__rev_char_list_to_string(Chars, String),
	( QuoteChar = '''' ->
		Token = name(String)
	; QuoteChar = '"' ->
		Token = string(String)
	;
		error("lexer.m: unknown quote character")
	).

:- pred lexer__get_quoted_name_escape(char::in, list(char)::in, token::out,
	io::di, io::uo) is det.

lexer__get_quoted_name_escape(QuoteChar, Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated quoted name")
	;
		Result = ok(Char),
		( Char = '\n' ->
			lexer__get_quoted_name(QuoteChar, Chars, Token, !IO)
		; Char = '\r' ->
			% Files created on Windows may have an extra
			% return character.
			lexer__get_quoted_name_escape(QuoteChar, Chars, Token, 
				!IO)
		; lexer__escape_char(Char, EscapedChar) ->
			Chars1 = [EscapedChar | Chars],
			lexer__get_quoted_name(QuoteChar, Chars1, Token, !IO)
		; Char = 'x' ->
			lexer__get_hex_escape(QuoteChar, Chars, [], Token, !IO)
		; char__is_octal_digit(Char) ->
			lexer__get_octal_escape(QuoteChar, Chars, [Char],
				Token, !IO)
		;
			Token = error("invalid escape character")
		)
	).

:- pred lexer__string_get_quoted_name_escape(string::in, int::in, char::in,
	list(char)::in, posn::in, token::out, string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_quoted_name_escape(String, Len, QuoteChar, Chars, Posn0,
		Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( Char = '\n' ->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				Chars, Posn0, Token, Context, !Posn)
		; Char = '\r' -> 
			% Files created on Windows may have an extra
			% return character.
			lexer__string_get_quoted_name_escape(String, Len, 
				QuoteChar, Chars, Posn0, Token, Context, !Posn)
		; lexer__escape_char(Char, EscapedChar) ->
			Chars1 = [EscapedChar | Chars],
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				Chars1, Posn0, Token, Context, !Posn)
		; Char = 'x' ->
			lexer__string_get_hex_escape(String, Len, QuoteChar,
				Chars, [], Posn0, Token, Context, !Posn)
		; char__is_octal_digit(Char) ->
			lexer__string_get_octal_escape(String, Len, QuoteChar,
				Chars, [Char], Posn0, Token, Context, !Posn)
		;
			lexer__string_get_context(!.Posn, Context, !Posn),
			Token = error("invalid escape character")
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("unterminated quoted name")
	).

:- pred lexer__escape_char(char::in, char::out) is semidet.

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

:- pred lexer__get_hex_escape(char::in, list(char)::in, list(char)::in,
	token::out, io::di, io::uo) is det.

lexer__get_hex_escape(QuoteChar, Chars, HexChars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated quote")
	;
		Result = ok(Char),
		( char__is_hex_digit(Char) ->
			lexer__get_hex_escape(QuoteChar, Chars,
				[Char | HexChars], Token, !IO)
		; Char = ('\\') ->
			lexer__finish_hex_escape(QuoteChar, Chars, HexChars,
				Token, !IO)
		;
			Token = error("unterminated hex escape")
		)
	).

:- pred lexer__string_get_hex_escape(string::in, int::in, char::in,
	list(char)::in, list(char)::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
		Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_hex_digit(Char) ->
			lexer__string_get_hex_escape(String, Len, QuoteChar,
				    Chars, [Char | HexChars], Posn0,
				    Token, Context, !Posn)
		; Char = ('\\') ->
			lexer__string_finish_hex_escape(String, Len, QuoteChar,
				    Chars, HexChars, Posn0, Token, Context,
				    !Posn)
		;
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = error("unterminated hex escape")
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("unterminated quote")
	).

:- pred lexer__finish_hex_escape(char::in, list(char)::in, list(char)::in,
	token::out, io::di, io::uo) is det.

lexer__finish_hex_escape(QuoteChar, Chars, HexChars, Token, !IO) :-
	( HexChars = [] ->
		Token = error("empty hex escape")
	;
		lexer__rev_char_list_to_string(HexChars, HexString),
		(
			string__base_string_to_int(16, HexString, Int),
			char__to_int(Char, Int)
		->
			lexer__get_quoted_name(QuoteChar, [Char|Chars], Token,
				!IO)
		;
			Token = error("invalid hex escape")
		)
	).

:- pred lexer__string_finish_hex_escape(string::in, int::in, char::in,
	list(char)::in, list(char)::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_finish_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
		Token, Context, !Posn) :-
	( HexChars = [] ->
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = error("empty hex escape")
	;
		lexer__rev_char_list_to_string(HexChars, HexString),
		(
			string__base_string_to_int(16, HexString, Int),
			char__to_int(Char, Int)
		->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context, !Posn)
		;
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = error("invalid hex escape")
		)
	).

:- pred lexer__get_octal_escape(char::in, list(char)::in, list(char)::in,
	token::out, io::di, io::uo) is det.

lexer__get_octal_escape(QuoteChar, Chars, OctalChars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated quote")
	;
		Result = ok(Char),
		( char__is_octal_digit(Char) ->
			lexer__get_octal_escape(QuoteChar, Chars,
				[Char | OctalChars], Token, !IO)
		; Char = ('\\') ->
			lexer__finish_octal_escape(QuoteChar, Chars,
				OctalChars, Token, !IO)
		;
			/******
				% We don't report this as an error since
				% we need bug-for-bug compatibility with
				% NU-Prolog XXX
			Token = error("unterminated octal escape")
			******/
			io__putback_char(Char, !IO),
			lexer__finish_octal_escape(QuoteChar, Chars,
				OctalChars, Token, !IO)
		)
	).

:- pred lexer__string_get_octal_escape(string::in, int::in, char::in,
	list(char)::in, list(char)::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
		Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_octal_digit(Char) ->
			lexer__string_get_octal_escape(String, Len,
				QuoteChar, Chars, [Char | OctalChars], Posn0,
				Token, Context, !Posn)
		; Char = ('\\') ->
			lexer__string_finish_octal_escape(String, Len,
				QuoteChar, Chars, OctalChars, Posn0,
				Token, Context, !Posn)
		;
			/******
				% We don't report this as an error since
				% we need bug-for-bug compatibility with
				% NU-Prolog XXX
			Token = error("unterminated octal escape")
			******/
			lexer__string_ungetchar(String, !Posn),
			lexer__string_finish_octal_escape(String, Len,
				QuoteChar, Chars, OctalChars, Posn0,
				Token, Context, !Posn)
		)
	;
		Token = error("unterminated quote"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__finish_octal_escape(char::in, list(char)::in, list(char)::in,
	token::out, io::di, io::uo) is det.

lexer__finish_octal_escape(QuoteChar, Chars, OctalChars, Token, !IO) :-
	( OctalChars = [] ->
		Token = error("empty octal escape")
	;
		lexer__rev_char_list_to_string(OctalChars, OctalString),
		(
			string__base_string_to_int(8, OctalString, Int),
			char__to_int(Char, Int)
		->
			lexer__get_quoted_name(QuoteChar, [Char | Chars],
				Token, !IO)
		;
			Token = error("invalid octal escape")
		)
	).

:- pred lexer__string_finish_octal_escape(string::in, int::in, char::in,
	list(char)::in, list(char)::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_finish_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
		Posn0, Token, Context, !Posn) :-
	( OctalChars = [] ->
		Token = error("empty octal escape"),
		lexer__string_get_context(Posn0, Context, !Posn)
	;
		lexer__rev_char_list_to_string(OctalChars, OctalString),
		(
			string__base_string_to_int(8, OctalString, Int),
			char__to_int(Char, Int)
		->
			lexer__string_get_quoted_name(String, Len, QuoteChar,
				[Char | Chars], Posn0, Token, Context, !Posn)
		;
			Token = error("invalid octal escape"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	).

%-----------------------------------------------------------------------------%

	% names and variables

:- pred lexer__get_name(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_name(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_string(Chars, Name),
		Token = name(Name)
	;
		Result = ok(Char),
		( char__is_alnum_or_underscore(Char) ->
			lexer__get_name([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_string(Chars, Name),
			Token = name(Name)
		)
	).

:- pred lexer__string_get_name(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_name(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_alnum_or_underscore(Char) ->
			lexer__string_get_name(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, Name, !Posn),
			Token = name(Name),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, Name, !Posn),
		Token = name(Name),
		lexer__string_get_context(Posn0, Context, !Posn)
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

:- pred lexer__get_source_line_number(list(char)::in, token::out,
	token_context::out, io::di, io::uo) is det.

lexer__get_source_line_number(Chars, Token, Context, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		lexer__get_context(Context, !IO),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__get_context(Context, !IO),
		Token = error(
			"unexpected end-of-file in `#' line number directive")
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_source_line_number([Char | Chars],
				Token, Context, !IO)
		; Char = '\n' ->
			lexer__rev_char_list_to_string(Chars, String),
			(
				string__base_string_to_int(10, String, Int),
				Int > 0
			->
				io__set_line_number(Int, !IO),
				lexer__get_token(Token, Context, !IO)
			;
				lexer__get_context(Context, !IO),
				string__append_list([
					"invalid line number `", String,
					"' in `#' line number directive"],
					Message),
				Token = error(Message)
			)
		;
			lexer__get_context(Context, !IO),
			string__from_char_list([Char], String),
			string__append_list([
				"invalid character `", String,
				"' in `#' line number directive"],
				Message),
			Token = error(Message)
		)
	).

:- pred lexer__string_get_source_line_number(string::in, int::in, posn::in,
	token::out, token_context::out, posn::in, posn::out) is det.

lexer__string_get_source_line_number(String, Len, Posn1, Token, Context,
		!Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_source_line_number(String, Len,
				Posn1, Token, Context, !Posn)
		; Char = '\n' ->
			lexer__grab_string(String, Posn1, LineNumString, !Posn),
			(
				string__base_string_to_int(10, LineNumString,
					LineNum),
				LineNum > 0
			->
				lexer__string_set_line_number(LineNum, !Posn),
				lexer__string_get_token(String, Len, Token,
					Context, !Posn)
			;
				lexer__string_get_context(Posn1, Context,
					!Posn),
				string__append_list([
					"invalid line number `", LineNumString,
					"' in `#' line number directive"],
					Message),
				Token = error(Message)
			)
		;
			lexer__string_get_context(Posn1, Context, !Posn),
			string__from_char_list([Char], DirectiveString),
			string__append_list([
				"invalid character `", DirectiveString,
				"' in `#' line number directive"],
				Message),
			Token = error(Message)
		)
	;
		lexer__string_get_context(Posn1, Context, !Posn),
		Token = error(
			"unexpected end-of-file in `#' line number directive")
	).

:- pred lexer__get_graphic(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_graphic(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_string(Chars, Name),
		Token = name(Name)
	;
		Result = ok(Char),
		( lexer__graphic_token_char(Char) ->
			lexer__get_graphic([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_string(Chars, Name),
			Token = name(Name)
		)
	).

:- pred lexer__string_get_graphic(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_graphic(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( lexer__graphic_token_char(Char) ->
			lexer__string_get_graphic(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, Name, !Posn),
			Token = name(Name),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, Name, !Posn),
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = name(Name)
	).

:- pred lexer__get_variable(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_variable(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_string(Chars, VariableName),
		Token = variable(VariableName)
	;
		Result = ok(Char),
		( char__is_alnum_or_underscore(Char) ->
			lexer__get_variable([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_string(Chars, VariableName),
			Token = variable(VariableName)
		)
	).

:- pred lexer__string_get_variable(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_variable(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_alnum_or_underscore(Char) ->
			lexer__string_get_variable(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, VariableName, !Posn),
			Token = variable(VariableName),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, VariableName, !Posn),
		Token = variable(VariableName),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

%-----------------------------------------------------------------------------%

	% integer and float literals

:- pred lexer__get_zero(token::out, io::di, io::uo) is det.

lexer__get_zero(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = integer(0)
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_number([Char], Token, !IO)
		; Char = '''' ->
			lexer__get_char_code(Token, !IO)
		; Char = 'b' ->
			lexer__get_binary(Token, !IO)
		; Char = 'o' ->
			lexer__get_octal(Token, !IO)
		; Char = 'x' ->
			lexer__get_hex(Token, !IO)
		; Char = ('.') ->
			lexer__get_int_dot(['0'], Token, !IO)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__get_float_exponent([Char, '0'], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = integer(0)
		)
	).

:- pred lexer__string_get_zero(string::in, int::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_zero(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = '''' ->
			lexer__string_get_char_code(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = 'b' ->
			lexer__string_get_binary(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = 'o' ->
			lexer__string_get_octal(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = 'x' ->
			lexer__string_get_hex(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('.') ->
			lexer__string_get_int_dot(String, Len, Posn0,
				Token, Context, !Posn)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__string_get_context(Posn0, Context, !Posn),
			Token = integer(0)
		)
	;
		lexer__string_get_context(Posn0, Context, !Posn),
		Token = integer(0)
	).

:- pred lexer__get_char_code(token::out, io::di, io::uo) is det.

lexer__get_char_code(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated char code constant")
	;
		Result = ok(Char),
		char__to_int(Char, CharCode),
		Token = integer(CharCode)
	).

:- pred lexer__string_get_char_code(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_char_code(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		char__to_int(Char, CharCode),
		Token = integer(CharCode),
		lexer__string_get_context(Posn0, Context, !Posn)
	;
		Token = error("unterminated char code constant"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_binary(token::out, io::di, io::uo) is det.

lexer__get_binary(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated binary constant")
	;
		Result = ok(Char),
		( char__is_binary_digit(Char) ->
			lexer__get_binary_2([Char], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = error("unterminated binary constant")
		)
	).

:- pred lexer__string_get_binary(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_binary(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_binary_digit(Char) ->
			lexer__string_get_binary_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			Token = error("unterminated binary constant"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		Token = error("unterminated binary constant"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_binary_2(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_binary_2(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_int(Chars, 2, Token)
	;
		Result = ok(Char),
		( char__is_binary_digit(Char) ->
			lexer__get_binary_2([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_int(Chars, 2, Token)
		)
	).

:- pred lexer__string_get_binary_2(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_binary_2(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_binary_digit(Char) ->
			lexer__string_get_binary_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, BinaryString, !Posn),
			lexer__conv_string_to_int(BinaryString, 2, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString, !Posn),
		lexer__conv_string_to_int(BinaryString, 2, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_octal(token::out, io::di, io::uo) is det.

lexer__get_octal(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated octal constant")
	;
		Result = ok(Char),
		( char__is_octal_digit(Char) ->
			lexer__get_octal_2([Char], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = error("unterminated octal constant")
		)
	).

:- pred lexer__string_get_octal(string::in, int::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_octal(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_octal_digit(Char) ->
			lexer__string_get_octal_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			Token = error("unterminated octal constant"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		Token = error("unterminated octal constant"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_octal_2(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_octal_2(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_int(Chars, 8, Token)
	;
		Result = ok(Char),
		( char__is_octal_digit(Char) ->
			lexer__get_octal_2([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_int(Chars, 8, Token)
		)
	).

:- pred lexer__string_get_octal_2(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_octal_2(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_octal_digit(Char) ->
			lexer__string_get_octal_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, BinaryString, !Posn),
			lexer__conv_string_to_int(BinaryString, 8, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString, !Posn),
		lexer__conv_string_to_int(BinaryString, 8, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_hex(token::out, io::di, io::uo) is det.

lexer__get_hex(Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated hex constant")
	;
		Result = ok(Char),
		( char__is_hex_digit(Char) ->
			lexer__get_hex_2([Char], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = error("unterminated hex constant")
		)
	).

:- pred lexer__string_get_hex(string::in, int::in, posn::in, token::out,
	string_token_context::out,
	posn::in, posn::out) is det.

lexer__string_get_hex(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_hex_digit(Char) ->
			lexer__string_get_hex_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			Token = error("unterminated hex constant"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		Token = error("unterminated hex constant"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_hex_2(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_hex_2(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_int(Chars, 16, Token)
	;
		Result = ok(Char),
		( char__is_hex_digit(Char) ->
			lexer__get_hex_2([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_int(Chars, 16, Token)
		)
	).

:- pred lexer__string_get_hex_2(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_hex_2(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_hex_digit(Char) ->
			lexer__string_get_hex_2(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, BinaryString, !Posn),
			lexer__conv_string_to_int(BinaryString, 16, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, BinaryString, !Posn),
		lexer__conv_string_to_int(BinaryString, 16, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_number(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_number(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_int(Chars, 10, Token)
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_number([Char | Chars], Token, !IO)
		; Char = ('.') ->
			lexer__get_int_dot(Chars, Token, !IO)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__get_float_exponent([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_int(Chars, 10, Token)
		)
	).

:- pred lexer__string_get_number(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_number(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_number(String, Len, Posn0,
				Token, Context, !Posn)
		; Char = ('.') ->
			lexer__string_get_int_dot(String, Len, Posn0,
				Token, Context, !Posn)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, NumberString, !Posn),
			lexer__conv_string_to_int(NumberString, 10, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, NumberString, !Posn),
		lexer__conv_string_to_int(NumberString, 10, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

	% XXX the float literal syntax doesn't match ISO Prolog

:- pred lexer__get_int_dot(list(char)::in, token::out, io::di, io::uo) is det.

lexer__get_int_dot(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		io__putback_char('.', !IO),
		lexer__rev_char_list_to_int(Chars, 10, Token)
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_float_decimals([Char, '.' | Chars], Token,
				!IO)
		;
			io__putback_char(Char, !IO),
			% We can't putback the ".", because io__putback_char
			% only guarantees one character of pushback.
			% So instead, we return an `integer_dot' token;
			% the main loop of lexer__get_token_list_2 will
			% handle this appropriately.
			lexer__rev_char_list_to_int(Chars, 10, Token0),
			( Token0 = integer(Int) ->
				Token = integer_dot(Int)
			;
				Token = Token0
			)
		)
	).

:- pred lexer__string_get_int_dot(string::in, int::in, posn::in, token::out,
	string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_int_dot(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_float_decimals(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, NumberString, !Posn),
			lexer__conv_string_to_int(NumberString, 10, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__string_ungetchar(String, !Posn),
		lexer__grab_string(String, Posn0, NumberString, !Posn),
		lexer__conv_string_to_int(NumberString, 10, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_float_decimals(list(char)::in, token::out,
	io::di, io::uo) is det.

	% we've read past the decimal point, so now get the decimals

lexer__get_float_decimals(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_float(Chars, Token)
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_float_decimals([Char | Chars], Token, !IO)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__get_float_exponent([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_float(Chars, Token)
		)
	).

:- pred lexer__string_get_float_decimals(string::in, int::in, posn::in,
	token::out, string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_float_decimals(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_float_decimals(String, Len, Posn0,
				Token, Context, !Posn)
		; ( Char = 'e' ; Char = 'E' ) ->
			lexer__string_get_float_exponent(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, FloatString, !Posn),
			lexer__conv_to_float(FloatString, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, FloatString, !Posn),
		lexer__conv_to_float(FloatString, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_float_exponent(list(char)::in, token::out,
	io::di, io::uo) is det.

lexer__get_float_exponent(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_float(Chars, Token)
	;
		Result = ok(Char),
		( ( Char = ('+') ; Char = ('-') ) ->
			lexer__get_float_exponent_2([Char | Chars], Token, !IO)
		; char__is_digit(Char) ->
			lexer__get_float_exponent_3([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = error("unterminated exponent in float token")
		)
	).

:- pred lexer__string_get_float_exponent(string::in, int::in, posn::in,
	token::out, string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_float_exponent(String, Len, Posn0, Token, Context, !Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( ( Char = ('+') ; Char = ('-') ) ->
			lexer__string_get_float_exponent_2(String, Len, Posn0,
				Token, Context, !Posn)
		; char__is_digit(Char) ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			Token =
			  error("unterminated exponent in float token"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, FloatString, !Posn),
		lexer__conv_to_float(FloatString, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_float_exponent_2(list(char)::in, token::out,
	io::di, io::uo) is det.

	% we've read past the E signalling the start of the exponent -
	% make sure that there's at least one digit following,
	% and then get the remaining digits

lexer__get_float_exponent_2(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		Token = error("unterminated exponent in float token")
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_float_exponent_3([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			Token = error("unterminated exponent in float token")
		)
	).

:- pred lexer__string_get_float_exponent_2(string::in, int::in, posn::in,
	token::out, string_token_context::out, posn::in, posn::out) is det.

	% we've read past the E signalling the start of the exponent -
	% make sure that there's at least one digit following,
	% and then get the remaining digits

lexer__string_get_float_exponent_2(String, Len, Posn0, Token, Context,
		!Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			Token = error("unterminated exponent in float token"),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		Token = error("unterminated exponent in float token"),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

:- pred lexer__get_float_exponent_3(list(char)::in, token::out,
	io::di, io::uo) is det.

	% we've read past the first digit of the exponent -
	% now get the remaining digits

lexer__get_float_exponent_3(Chars, Token, !IO) :-
	io__read_char(Result, !IO),
	(
		Result = error(Error),
		Token = io_error(Error)
	;
		Result = eof,
		lexer__rev_char_list_to_float(Chars, Token)
	;
		Result = ok(Char),
		( char__is_digit(Char) ->
			lexer__get_float_exponent_3([Char | Chars], Token, !IO)
		;
			io__putback_char(Char, !IO),
			lexer__rev_char_list_to_float(Chars, Token)
		)
	).

:- pred lexer__string_get_float_exponent_3(string::in, int::in, posn::in,
	token::out, string_token_context::out, posn::in, posn::out) is det.

lexer__string_get_float_exponent_3(String, Len, Posn0, Token, Context,
		!Posn) :-
	( lexer__string_read_char(String, Len, Char, !Posn) ->
		( char__is_digit(Char) ->
			lexer__string_get_float_exponent_3(String, Len, Posn0,
				Token, Context, !Posn)
		;
			lexer__string_ungetchar(String, !Posn),
			lexer__grab_string(String, Posn0, FloatString, !Posn),
			lexer__conv_to_float(FloatString, Token),
			lexer__string_get_context(Posn0, Context, !Posn)
		)
	;
		lexer__grab_string(String, Posn0, FloatString, !Posn),
		lexer__conv_to_float(FloatString, Token),
		lexer__string_get_context(Posn0, Context, !Posn)
	).

%-----------------------------------------------------------------------------%

	% Utility routines

:- pred lexer__rev_char_list_to_int(list(char)::in, int::in, token::out)
	is det.

lexer__rev_char_list_to_int(RevChars, Base, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	lexer__conv_string_to_int(String, Base, Token).

:- pred lexer__conv_string_to_int(string::in, int::in, token::out) is det.

lexer__conv_string_to_int(String, Base, Token) :-
	( string__base_string_to_int(Base, String, Int) ->
		Token = integer(Int)
	;
		Token = error("invalid integer token")
	).

:- pred lexer__rev_char_list_to_float(list(char)::in, token::out) is det.

lexer__rev_char_list_to_float(RevChars, Token) :-
	lexer__rev_char_list_to_string(RevChars, String),
	lexer__conv_to_float(String, Token).

:- pred lexer__conv_to_float(string::in, token::out) is det.

lexer__conv_to_float(String, Token) :-
	( string__to_float(String, Float) ->
		Token = float(Float)
	;
		Token = error("invalid float token")
	).

:- pred lexer__rev_char_list_to_string(list(char)::in, string::out) is det.

lexer__rev_char_list_to_string(RevChars, String) :-
       string__from_rev_char_list(RevChars, String).

%-----------------------------------------------------------------------------%
