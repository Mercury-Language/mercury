%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: lexer.m.
% Main author: fjh.
% Stability: high.
% 
% Lexical analysis.  This module defines the representation of tokens
% and exports predicates for reading in tokens from an input stream.
%
% See ISO Prolog 6.4.  Also see the comments at the top of parser.m.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lexer.
:- interface.

:- import_module char.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type token
    --->    name(string)
    ;       variable(string)
    ;       integer(int)
    ;       float(float)
    ;       string(string)      % "...."
    ;       open                % '('
    ;       open_ct             % '(' without any preceding whitespace
    ;       close               % ')'
    ;       open_list           % '['
    ;       close_list          % ']'
    ;       open_curly          % '{'
    ;       close_curly         % '}'
    ;       ht_sep              % '|'
    ;       comma               % ','
    ;       end                 % '.'
    ;       junk(char)          % junk character in the input stream
    ;       error(string)       % some other invalid token
    ;       io_error(io.error) % error reading from the input stream
    ;       eof                 % end-of-file
    ;       integer_dot(int).   % the lexer will never return this.
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
:- type token_context == int.   % line number

    % This "fat list" representation is more efficient than a list of pairs.
    %
:- type token_list
    --->    token_cons(token, token_context, token_list)
    ;       token_nil.

    % Read a list of tokens from the current input stream.
    % Keep reading until we encounter either an `end' token
    % (i.e. a full stop followed by whitespace) or the end-of-file.
    %
:- pred get_token_list(token_list::out, io::di, io::uo) is det.

    % The type `offset' represents a (zero-based) offset into a string.
    %
:- type offset == int.

    % string_get_token_list_max(String, MaxOffset, Tokens,
    %   InitialPos, FinalPos):
    %
    % Scan a list of tokens from a string, starting at the current offset
    % specified by InitialPos. Keep scanning until either we encounter either
    % an `end' token (i.e. a full stop followed by whitespace) or until we
    % reach MaxOffset. (MaxOffset must be =< the length of the string.)
    % Return the tokens scanned in Tokens, and return the position one
    % character past the end of the last token in FinalPos.
    %
:- pred string_get_token_list_max(string::in, offset::in, token_list::out,
    posn::in, posn::out) is det.

    % string_get_token_list(String, Tokens, InitialPos, FinalPos):
    %
    % calls string_get_token_list_max above with MaxPos = length of String.
    %
:- pred string_get_token_list(string::in, token_list::out,
    posn::in, posn::out) is det.

    % Convert a token to a human-readable string describing the token.
    %
:- pred token_to_string(token::in, string::out) is det.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%

:- interface.

    % graphic_token_char(Char): true iff `Char'
    % is "graphic token char" (ISO Prolog 6.4.2).
    % This is exported for use by term_io.quote_atom.
    %
:- pred graphic_token_char(char::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module term.

% Note that there are two implementations of most predicates here:
% one which deals with strings, the other that deals with io.states.
% We can't write the io.state version in terms of the string
% version because we don't know how much string to slurp up
% until after we've lexically analysed it.  Some interactive
% applications require the old Prolog behaviour of stopping
% after an end token (i.e. `.' plus whitespace) rather than
% reading in whole lines.
% Conversely, we can't write the string version using the io.state
% version, since that would require either cheating with the io.state
% or ruining the string interface.
%
% An alternative would be to write both versions in terms
% of a generic "char_stream" typeclass, with instances
% for io.states and for strings.
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

%-----------------------------------------------------------------------------%

token_to_string(name(Name), String) :-
    string.append_list(["token '", Name, "'"], String).
token_to_string(variable(Var), String) :-
    string.append_list(["variable `", Var, "'"], String).
token_to_string(integer(Int), String) :-
    string.int_to_string(Int, IntString),
    string.append_list(["integer `", IntString, "'"], String).
token_to_string(float(Float), String) :-
    string.float_to_string(Float, FloatString),
    string.append_list(["float `", FloatString, "'"], String).
token_to_string(string(Token), String) :-
    string.append_list(["string """, Token, """"], String).
token_to_string(open, "token ` ('").
token_to_string(open_ct, "token `('").
token_to_string(close, "token `)'").
token_to_string(open_list, "token `['").
token_to_string(close_list, "token `]'").
token_to_string(open_curly, "token `{'").
token_to_string(close_curly, "token `}'").
token_to_string(ht_sep, "token `|'").
token_to_string(comma, "token `,'").
token_to_string(end, "token `. '").
token_to_string(eof, "end-of-file").
token_to_string(junk(JunkChar), String) :-
    char.to_int(JunkChar, Code),
    string.int_to_base_string(Code, 16, Hex),
    string.append_list(["illegal character <<0x", Hex, ">>"], String).
token_to_string(io_error(IO_Error), String) :-
    io.error_message(IO_Error, IO_ErrorMessage),
    string.append("I/O error: ", IO_ErrorMessage, String).
token_to_string(error(Message), String) :-
    string.append_list(["illegal token (", Message, ")"], String).
token_to_string(integer_dot(Int), String) :-
    string.int_to_string(Int, IntString),
    string.append_list(["integer `", IntString, "'."], String).

    % We build the tokens up as lists of characters in reverse order.
    % When we get to the end of each token, we call
    % `rev_char_list_to_string/2' to convert that representation
    % into a string.

    % Comments of the form
    %   foo --> bar . baz
    % mean that we are parsing a `foo', and we've already scanned
    % past the `bar', so now we need to match with a `baz'.

get_token_list(Tokens, !IO) :-
    get_token(Token, Context, !IO),
    get_token_list_2(Token, Context, Tokens, !IO).

:- pred get_token_list_2(token::in, token_context::in, token_list::out,
    io::di, io::uo) is det.

get_token_list_2(Token0, Context0, Tokens, !IO) :-
    ( Token0 = eof ->
        Tokens = token_nil
    ; ( Token0 = end ; Token0 = error(_) ; Token0 = io_error(_) ) ->
        Tokens = token_cons(Token0, Context0, token_nil)
    ; Token0 = integer_dot(Int) ->
        get_context(Context1, !IO),
        get_dot(Token1, !IO),
        get_token_list_2(Token1, Context1, Tokens1, !IO),
        Tokens = token_cons(integer(Int), Context0, Tokens1)
    ;
        get_token(Token1, Context1, !IO),
        get_token_list_2(Token1, Context1, Tokens1, !IO),
        Tokens = token_cons(Token0, Context0, Tokens1)
    ).

string_get_token_list(String, Tokens, !Posn) :-
    string.length(String, Len),
    string_get_token_list_max(String, Len, Tokens, !Posn).

string_get_token_list_max(String, Len, Tokens, !Posn) :-
    string_get_token(String, Len, Token, Context, !Posn),
    ( Token = eof ->
        Tokens = token_nil
    ; ( Token = end ; Token = error(_) ; Token = io_error(_) ) ->
        Tokens = token_cons(Token, Context, token_nil)
    ;
        Tokens = token_cons(Token, Context, Tokens1),
        string_get_token_list_max(String, Len, Tokens1, !Posn)
    ).

%-----------------------------------------------------------------------------%
%
% Some low-level routines.

:- pred get_context(token_context::out, io::di, io::uo) is det.

get_context(Context, !IO) :-
    io.get_line_number(Context, !IO).

:- type string_token_context == token_context.

:- pred string_get_context(posn::in, string_token_context::out,
    posn::in, posn::out) is det.

string_get_context(StartPosn, Context, !Posn) :-
    StartPosn = posn(StartLineNum, _, _),
    Context = StartLineNum.
    % In future, we might want to modify this code to read something
    % like this:
    %
    % posn_to_line_and_column(StartPosn, StartLineNum, StartColumn),
    % posn_to_line_and_column(!.Posn, EndLineNum, EndColumn),
    % Context = detailed(StartLine, StartColumn, EndLine, EndColumn).

:- pred string_read_char(string::in, int::in, char::out,
    posn::in, posn::out) is semidet.

:- pragma inline(string_read_char/5).

string_read_char(String, Len, Char, Posn0, Posn) :-
    Posn0 = posn(LineNum0, LineOffset0, Offset0),
    Offset0 < Len,
    string.unsafe_index(String, Offset0, Char),
    Offset = Offset0 + 1,
    ( Char = '\n' ->
        LineNum = LineNum0 + 1,
        Posn = posn(LineNum, Offset, Offset)
    ;
        Posn = posn(LineNum0, LineOffset0, Offset)
    ).

:- pred string_ungetchar(string::in, posn::in, posn::out) is det.

string_ungetchar(String, Posn0, Posn) :-
    Posn0 = posn(LineNum0, LineOffset0, Offset0),
    Offset = Offset0 - 1,
    string.unsafe_index(String, Offset, Char),
    ( Char = '\n' ->
        LineNum = LineNum0 - 1,
        Posn = posn(LineNum, Offset, Offset)
    ;
        Posn = posn(LineNum0, LineOffset0, Offset)
    ).

:- pred grab_string(string::in, posn::in, string::out,
    posn::in, posn::out) is det.

grab_string(String, Posn0, SubString, Posn, Posn) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    Count = Offset - Offset0,
    string.unsafe_substring(String, Offset0, Count, SubString).

:- pred string_set_line_number(int::in, posn::in, posn::out) is det.

string_set_line_number(LineNumber, Posn0, Posn) :-
    Posn0 = posn(_, _, Offset),
    Posn = posn(LineNumber, Offset, Offset).

%-----------------------------------------------------------------------------%

:- pred get_token(token::out, token_context::out, io::di, io::uo) is det.

get_token(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = eof
    ;
        Result = ok(Char),
        ( char.is_whitespace(Char) ->
            get_token_2(Token, Context, !IO)
        ; ( char.is_upper(Char) ; Char = '_' ) ->
            get_context(Context, !IO),
            get_variable([Char], Token, !IO)
        ; char.is_lower(Char) ->
            get_context(Context, !IO),
            get_name([Char], Token, !IO)
        ; Char = '0' ->
            get_context(Context, !IO),
            get_zero(Token, !IO)
        ; char.is_digit(Char) ->
            get_context(Context, !IO),
            get_number([Char], Token, !IO)
        ; special_token(Char, SpecialToken) ->
            get_context(Context, !IO),
            ( SpecialToken = open ->
                Token = open_ct
            ;
                Token = SpecialToken
            )
        ; Char = ('.') ->
            get_context(Context, !IO),
            get_dot(Token, !IO)
        ; Char = ('%') ->
            skip_to_eol(Token, Context, !IO)
        ; ( Char = '"' ; Char = '''' ) ->
            get_context(Context, !IO),
            get_quoted_name(Char, [], Token, !IO)
        ; Char = ('/') ->
            get_slash(Token, Context, !IO)
        ; Char = ('#') ->
            get_source_line_number([], Token, Context, !IO)
        ; Char = ('`') ->
            get_context(Context, !IO),
            Token = name("`")
        ; graphic_token_char(Char) ->
            get_context(Context, !IO),
            get_graphic([Char], Token, !IO)
        ;
            get_context(Context, !IO),
            Token = junk(Char)
        )
    ).

:- pred string_get_token(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_get_token(String, Len, Token, Context, !Posn) :-
    Posn0 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_whitespace(Char) ->
            string_get_token_2(String, Len, Token, Context, !Posn)
        ; ( char.is_upper(Char) ; Char = '_' ) ->
            string_get_variable(String, Len, Posn0, Token, Context, !Posn)
        ; char.is_lower(Char) ->
            string_get_name(String, Len, Posn0, Token, Context, !Posn)
        ; Char = '0' ->
            string_get_zero(String, Len, Posn0, Token, Context, !Posn)
        ; char.is_digit(Char) ->
            string_get_number(String, Len, Posn0, Token, Context, !Posn)
        ; special_token(Char, SpecialToken) ->
            string_get_context(Posn0, Context, !Posn),
            ( SpecialToken = open ->
                Token = open_ct
            ;
                Token = SpecialToken
            )
        ; Char = ('.') ->
            string_get_dot(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('%') ->
            string_skip_to_eol(String, Len, Token, Context, !Posn)
        ; ( Char = '"' ; Char = '''' ) ->
            string_get_quoted_name(String, Len, Char, [], Posn0, Token,
                Context, !Posn)
        ; Char = ('/') ->
            string_get_slash(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('#') ->
            string_get_source_line_number(String, Len, !.Posn, Token, Context,
                !Posn)
        ; Char = ('`') ->
            string_get_context(Posn0, Context, !Posn),
            Token = name("`")
        ; graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = junk(Char)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

%-----------------------------------------------------------------------------%

    % This is just like get_token, except that we have already scanned past
    % some whitespace, so '(' gets scanned as `open' rather than `open_ct'.
    %
:- pred get_token_2(token::out, token_context::out, io::di, io::uo)
    is det.

get_token_2(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = eof
    ;
        Result = ok(Char),
        ( char.is_whitespace(Char) ->
            get_token_2(Token, Context, !IO)
        ; ( char.is_upper(Char) ; Char = '_' ) ->
            get_context(Context, !IO),
            get_variable([Char], Token, !IO)
        ; char.is_lower(Char) ->
            get_context(Context, !IO),
            get_name([Char], Token, !IO)
        ; Char = '0' ->
            get_context(Context, !IO),
            get_zero(Token, !IO)
        ; char.is_digit(Char) ->
            get_context(Context, !IO),
            get_number([Char], Token, !IO)
        ; special_token(Char, SpecialToken) ->
            get_context(Context, !IO),
            Token = SpecialToken
        ; Char = ('.') ->
            get_context(Context, !IO),
            get_dot(Token, !IO)
        ; Char = ('%') ->
            skip_to_eol(Token, Context, !IO)
        ; ( Char = '"' ; Char = '''' ) ->
            get_context(Context, !IO),
            get_quoted_name(Char, [], Token, !IO)
        ; Char = ('/') ->
            get_slash(Token, Context, !IO)
        ; Char = ('#') ->
            get_source_line_number([], Token, Context, !IO)
        ; Char = ('`') ->
            get_context(Context, !IO),
            Token = name("`")
        ; graphic_token_char(Char) ->
            get_context(Context, !IO),
            get_graphic([Char], Token, !IO)
        ;
            get_context(Context, !IO),
            Token = junk(Char)
        )
    ).

:- pred string_get_token_2(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_get_token_2(String, Len, Token, Context, !Posn) :-
    Posn0 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_whitespace(Char) ->
            string_get_token_2(String, Len, Token, Context, !Posn)
        ; ( char.is_upper(Char) ; Char = '_' ) ->
            string_get_variable(String, Len, Posn0, Token, Context, !Posn)
        ; char.is_lower(Char) ->
            string_get_name(String, Len, Posn0, Token, Context, !Posn)
        ; Char = '0' ->
            string_get_zero(String, Len, Posn0, Token, Context, !Posn)
        ; char.is_digit(Char) ->
            string_get_number(String, Len, Posn0, Token, Context, !Posn)
        ; special_token(Char, SpecialToken) ->
            string_get_context(Posn0, Context, !Posn),
            Token = SpecialToken
        ; Char = ('.') ->
            string_get_dot(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('%') ->
            string_skip_to_eol(String, Len, Token, Context, !Posn)
        ; ( Char = '"' ; Char = '''' ) ->
            string_get_quoted_name(String, Len, Char, [], Posn0, Token,
                Context, !Posn)
        ; Char = ('/') ->
            string_get_slash(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('#') ->
            string_get_source_line_number(String, Len, !.Posn, Token, Context,
                !Posn)
        ; Char = ('`') ->
            string_get_context(Posn0, Context, !Posn),
            Token = name("`")
        ; graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = junk(Char)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

%-----------------------------------------------------------------------------%

:- pred special_token(char::in, token::out) is semidet.

special_token('(', open).    % May get converted to open_ct
special_token(')', close).
special_token('[', open_list).
special_token(']', close_list).
special_token('{', open_curly).
special_token('}', close_curly).
special_token('|', ht_sep).
special_token(',', comma).
special_token(';', name(";")).

graphic_token_char('!').
graphic_token_char('#').
graphic_token_char('$').
graphic_token_char('&').
graphic_token_char('*').
graphic_token_char('+').
graphic_token_char('-').
graphic_token_char('.').
graphic_token_char('/').
graphic_token_char(':').
graphic_token_char('<').
graphic_token_char('=').
graphic_token_char('>').
graphic_token_char('?').
graphic_token_char('@').
graphic_token_char('^').
graphic_token_char('~').
graphic_token_char('\\').

%-----------------------------------------------------------------------------%

:- pred get_dot(token::out, io::di, io::uo) is det.

get_dot(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = end
    ;
        Result = ok(Char),
        ( whitespace_after_dot(Char) ->
            io.putback_char(Char, !IO),
            Token = end
        ; graphic_token_char(Char) ->
            get_graphic([Char, '.'], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = name(".")
        )
    ).

:- pred string_get_dot(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_dot(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( whitespace_after_dot(Char) ->
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = end
        ; graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = name(".")
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = end
    ).

:- pred whitespace_after_dot(char::in) is semidet.

whitespace_after_dot(Char) :-
    ( char.is_whitespace(Char)
    ; Char = '%'
    ).

%-----------------------------------------------------------------------------%
%
% Comments.

:- pred skip_to_eol(token::out, token_context::out, io::di, io::uo)
    is det.

skip_to_eol(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = eof
    ;
        Result = ok(Char),
        ( Char = '\n' ->
            get_token_2(Token, Context, !IO)
        ;
            skip_to_eol(Token, Context, !IO)
        )
    ).

:- pred string_skip_to_eol(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_skip_to_eol(String, Len, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = '\n' ->
            string_get_token_2(String, Len, Token, Context, !Posn)
        ;
            string_skip_to_eol(String, Len, Token, Context, !Posn)
        )
    ;
        string_get_context(!.Posn, Context, !Posn),
        Token = eof
    ).

:- pred get_slash(token::out, token_context::out, io::di, io::uo) is det.

get_slash(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = name("/")
    ;
        Result = ok(Char),
        ( Char = ('*') ->
            get_comment(Token, Context, !IO)
        ; graphic_token_char(Char) ->
            get_context(Context, !IO),
            get_graphic([Char, '/'], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            get_context(Context, !IO),
            Token = name("/")
        )
    ).

:- pred string_get_slash(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_slash(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = ('*') ->
            string_get_comment(String, Len, Posn0, Token, Context, !Posn)
        ; graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = name("/")
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = name("/")
    ).

:- pred get_comment(token::out, token_context::out,
    io::di, io::uo) is det.

get_comment(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok(Char),
        ( Char = ('*') ->
            get_comment_2(Token, Context, !IO)
        ;
            get_comment(Token, Context, !IO)
        )
    ).

:- pred string_get_comment(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_comment(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = ('*') ->
            string_get_comment_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_get_comment(String, Len, Posn0, Token, Context, !Posn)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = error("unterminated '/*' comment")
    ).

:- pred get_comment_2(token::out, token_context::out, io::di, io::uo) is det.

get_comment_2(Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok(Char),
        ( Char = ('/') ->
            % end of /* ... */ comment, so get next token
            get_token_2(Token, Context, !IO)
        ; Char = ('*') ->
            get_comment_2(Token, Context, !IO)
        ;
            get_comment(Token, Context, !IO)
        )
    ).

:- pred string_get_comment_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_comment_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = ('/') ->
            % end of /* ... */ comment, so get next token
            string_get_token_2(String, Len, Token, Context, !Posn)
        ; Char = ('*') ->
            string_get_comment_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_get_comment(String, Len, Posn0, Token, Context, !Posn)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = error("unterminated '/*' comment")
    ).

%-----------------------------------------------------------------------------%
%
% Quoted names and quoted strings.

:- pred get_quoted_name(char::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_quoted_name(QuoteChar, Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated quote")
    ;
        Result = ok(Char),
        ( Char = QuoteChar ->
            get_quoted_name_quote(QuoteChar, Chars, Token, !IO)
        ; Char = ('\\') ->
            get_quoted_name_escape(QuoteChar, Chars, Token, !IO)
        ;
            get_quoted_name(QuoteChar, [Char | Chars], Token, !IO)
        )
    ).

:- pred string_get_quoted_name(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name(String, Len, QuoteChar, Chars, Posn0, Token, Context,
        !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = QuoteChar ->
            string_get_quoted_name_quote(String, Len, QuoteChar, Chars,
                Posn0, Token, Context, !Posn)
        ; Char = ('\\') ->
            string_get_quoted_name_escape(String, Len, QuoteChar, Chars,
                Posn0, Token, Context, !Posn)
        ;
            string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                Posn0, Token, Context, !Posn)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = error("unterminated quote")
    ).

:- pred get_quoted_name_quote(char::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_quoted_name_quote(QuoteChar, Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        finish_quoted_name(QuoteChar, Chars, Token)
    ;
        Result = ok(Char),
        ( Char = QuoteChar ->
            get_quoted_name(QuoteChar, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            finish_quoted_name(QuoteChar, Chars, Token)
        )
    ).

:- pred string_get_quoted_name_quote(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name_quote(String, Len, QuoteChar, Chars, Posn0,
        Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = QuoteChar ->
            string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            finish_quoted_name(QuoteChar, Chars, Token)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        finish_quoted_name(QuoteChar, Chars, Token)
    ).

:- pred finish_quoted_name(char::in, list(char)::in, token::out) is det.

finish_quoted_name(QuoteChar, Chars, Token) :-
    rev_char_list_to_string(Chars, String),
    ( QuoteChar = '''' ->
        Token = name(String)
    ; QuoteChar = '"' ->
        Token = string(String)
    ;
        error("lexer.m: unknown quote character")
    ).

:- pred get_quoted_name_escape(char::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_quoted_name_escape(QuoteChar, Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated quoted name")
    ;
        Result = ok(Char),
        ( Char = '\n' ->
            get_quoted_name(QuoteChar, Chars, Token, !IO)
        ; Char = '\r' ->
            % Files created on Windows may have an extra return character.
            get_quoted_name_escape(QuoteChar, Chars, Token, !IO)
        ; escape_char(Char, EscapedChar) ->
            Chars1 = [EscapedChar | Chars],
            get_quoted_name(QuoteChar, Chars1, Token, !IO)
        ; Char = 'x' ->
            get_hex_escape(QuoteChar, Chars, [], Token, !IO)
        ; Char = 'u' ->
            get_unicode_escape(4, QuoteChar, Chars, [], Token, !IO)
        ; Char = 'U' ->
            get_unicode_escape(8, QuoteChar, Chars, [], Token, !IO)
        ; char.is_octal_digit(Char) ->
            get_octal_escape(QuoteChar, Chars, [Char], Token, !IO)
        ;
            Token = error("invalid escape character")
        )
    ).

:- pred string_get_quoted_name_escape(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name_escape(String, Len, QuoteChar, Chars, Posn0,
        Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( Char = '\n' ->
            string_get_quoted_name(String, Len, QuoteChar, Chars,
                Posn0, Token, Context, !Posn)
        ; Char = '\r' ->
            % Files created on Windows may have an extra return character.
            string_get_quoted_name_escape(String, Len, QuoteChar, Chars,
                Posn0, Token, Context, !Posn)
        ; escape_char(Char, EscapedChar) ->
            Chars1 = [EscapedChar | Chars],
            string_get_quoted_name(String, Len, QuoteChar, Chars1,
                Posn0, Token, Context, !Posn)
        ; Char = 'x' ->
            string_get_hex_escape(String, Len, QuoteChar, Chars, [],
                Posn0, Token, Context, !Posn)
        ; Char = 'u' ->
            string_get_unicode_escape(4, String, Len, QuoteChar, Chars,
                    [], Posn0, Token, Context, !Posn)
        ; Char = 'U' ->
            string_get_unicode_escape(8, String, Len, QuoteChar, Chars,
                    [], Posn0, Token, Context, !Posn)
        ; char.is_octal_digit(Char) ->
            string_get_octal_escape(String, Len, QuoteChar, Chars, [Char],
                Posn0, Token, Context, !Posn)
        ;
            string_get_context(!.Posn, Context, !Posn),
            Token = error("invalid escape character")
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = error("unterminated quoted name")
    ).

:- pred escape_char(char::in, char::out) is semidet.

escape_char('a', '\a').
escape_char('b', '\b').
escape_char('r', '\r').
escape_char('f', '\f').
escape_char('t', '\t').
escape_char('n', '\n').
escape_char('v', '\v').
escape_char('\\', '\\').
escape_char('''', '''').
escape_char('"', '"').
escape_char('`', '`').

:- pred get_hex_escape(char::in, list(char)::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_hex_escape(QuoteChar, Chars, HexChars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated quote")
    ;
        Result = ok(Char),
        ( char.is_hex_digit(Char) ->
            get_hex_escape(QuoteChar, Chars, [Char | HexChars], Token, !IO)
        ; Char = ('\\') ->
            finish_hex_escape(QuoteChar, Chars, HexChars, Token, !IO)
        ;
            Token = error("unterminated hex escape")
        )
    ).

:- pred get_unicode_escape(int::in, char::in, list(char)::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_unicode_escape(NumHexChars, QuoteChar, Chars, HexChars, Token, !IO) :-
    ( if NumHexChars = list.length(HexChars) then
        rev_char_list_to_string(HexChars, HexString),
        ( if
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            convert_unicode_char_to_target_chars(UnicodeCharCode, UTFChars)
        then
            get_quoted_name(QuoteChar, list.reverse(UTFChars) ++ Chars,
                Token, !IO)
        else
            Token = error("invalid Unicode character code")
        )
    else
        io.read_char(Result, !IO),
        (
            Result = error(Error),
            Token = io_error(Error)
        ;
            Result = eof,
            Token = error("unterminated quote")
        ;
            Result = ok(Char),
            ( if char.is_hex_digit(Char) then
                get_unicode_escape(NumHexChars, QuoteChar, Chars,
                    [Char | HexChars], Token, !IO)
            else
                Token = error("invalid hex character in Unicode escape")
            )
        )
    ).

:- pred string_get_unicode_escape(int::in, string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_unicode_escape(NumHexChars, String, Len, QuoteChar, Chars,
        HexChars, Posn0, Token, Context, !Posn) :-
    ( if NumHexChars = list.length(HexChars) then
        rev_char_list_to_string(HexChars, HexString),
        ( if
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            convert_unicode_char_to_target_chars(UnicodeCharCode, UTFChars)
        then
            RevCharsWithUnicode = list.reverse(UTFChars) ++ Chars,
            string_get_quoted_name(String, Len, QuoteChar, RevCharsWithUnicode,
                Posn0, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("invalid Unicode character code")
        )
    else
        ( if string_read_char(String, Len, Char, !Posn) then
            ( if char.is_hex_digit(Char) then
                string_get_unicode_escape(NumHexChars, String, Len, QuoteChar,
                    Chars, [Char | HexChars], Posn0, Token, Context, !Posn)
            else
                string_get_context(Posn0, Context, !Posn),
                Token = error("invalid hex character in Unicode escape")
            )
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("unterminated quote")
        )
    ).

:- pred convert_unicode_char_to_target_chars(int::in, list(char)::out)
    is semidet.

convert_unicode_char_to_target_chars(UnicodeCharCode, Chars) :-
    BackendEncoding = backend_unicode_encoding,
    (
        BackendEncoding = utf8,
        encode_unicode_char_as_utf8(UnicodeCharCode, Chars)
    ;
        BackendEncoding = utf16,
        encode_unicode_char_as_utf16(UnicodeCharCode, Chars)
    ).

:- pred encode_unicode_char_as_utf8(int::in, list(char)::out) is semidet.

encode_unicode_char_as_utf8(UnicodeCharCode, UTF8Chars) :-
    allowed_unicode_char_code(UnicodeCharCode),
    %
    % Refer to table 3-5 of the Unicode 4.0.0 standard (available from
    % www.unicode.org) for documentation on the bit distribution patterns used
    % below.
    %
    ( if UnicodeCharCode =< 0x00007F then
        UTF8Chars = [char.det_from_int(UnicodeCharCode)]
    else if UnicodeCharCode =< 0x0007FF then
        Part1 = (0b11111000000 /\ UnicodeCharCode) >> 6,
        Part2 =  0b00000111111 /\ UnicodeCharCode,
        char.det_from_int(Part1 \/ 0b11000000, UTF8Char1),
        char.det_from_int(Part2 \/ 0b10000000, UTF8Char2),
        UTF8Chars = [UTF8Char1, UTF8Char2]
    else if UnicodeCharCode =< 0x00FFFF then
        Part1 = (0b1111000000000000 /\ UnicodeCharCode) >> 12,
        Part2 = (0b0000111111000000 /\ UnicodeCharCode) >> 6,
        Part3 =  0b0000000000111111 /\ UnicodeCharCode,
        char.det_from_int(Part1 \/ 0b11100000, UTF8Char1),
        char.det_from_int(Part2 \/ 0b10000000, UTF8Char2),
        char.det_from_int(Part3 \/ 0b10000000, UTF8Char3),
        UTF8Chars = [UTF8Char1, UTF8Char2, UTF8Char3]
    else
        Part1 = (0b111000000000000000000 /\ UnicodeCharCode) >> 18,
        Part2 = (0b000111111000000000000 /\ UnicodeCharCode) >> 12,
        Part3 = (0b000000000111111000000 /\ UnicodeCharCode) >> 6,
        Part4 =  0b000000000000000111111 /\ UnicodeCharCode,
        char.det_from_int(Part1 \/ 0b11110000, UTF8Char1),
        char.det_from_int(Part2 \/ 0b10000000, UTF8Char2),
        char.det_from_int(Part3 \/ 0b10000000, UTF8Char3),
        char.det_from_int(Part4 \/ 0b10000000, UTF8Char4),
        UTF8Chars = [UTF8Char1, UTF8Char2, UTF8Char3, UTF8Char4]
    ).

:- pred encode_unicode_char_as_utf16(int::in, list(char)::out) is semidet.

    % This predicate should only be called on backends that have
    % a 16 bit character type.
    %
encode_unicode_char_as_utf16(UnicodeCharCode, UTF16Chars) :-
    allowed_unicode_char_code(UnicodeCharCode),
    %
    % If the code point is less than or equal to 0xFFFF
    % then the UTF-16 encoding is simply the code point value,
    % otherwise we construct a surrogate pair.
    %
    ( if UnicodeCharCode =< 0xFFFF then
        char.det_from_int(UnicodeCharCode, Char),
        UTF16Chars = [Char]
    else
        %
        % Refer to table 3-4 of the Unicode 4.0.0 standard (available from
        % www.unicode.org) for documentation on the bit distribution patterns
        % used below.
        %
        UUUUU =      (0b111110000000000000000 /\ UnicodeCharCode) >> 16,
        XXXXXX =     (0b000001111110000000000 /\ UnicodeCharCode) >> 10,
        XXXXXXXXXX = (0b000000000001111111111 /\ UnicodeCharCode),
        WWWWW = UUUUU - 1,
        Surrogate1Lead = 0b1101100000000000,
        Surrogate2Lead = 0b1101110000000000,
        Surrogate1 = Surrogate1Lead \/ (WWWWW << 6) \/ XXXXXX,
        Surrogate2 = Surrogate2Lead \/ XXXXXXXXXX,
        char.det_from_int(Surrogate1, Surrogate1Char),
        char.det_from_int(Surrogate2, Surrogate2Char),
        UTF16Chars = [Surrogate1Char, Surrogate2Char]
    ).

:- pred allowed_unicode_char_code(int::in) is semidet.

    % Succeeds if the give code point is a legal Unicode code point
    % (regardless of whether it is reserved for private use or not).
    %
allowed_unicode_char_code(Code) :-
    Code >= 0,
    Code =< 0x10FFFF,
    % The following range is reserved for surrogates.
    not (
        Code >= 0xD800, Code =< 0xDFFF
    ).

:- type unicode_encoding
    --->    utf8
    ;       utf16.

:- func backend_unicode_encoding = unicode_encoding.

backend_unicode_encoding = Encoding :-
    Int = backend_unicode_encoding_int,
    ( unicode_encoding_int_to_encoding(Int, Encoding0) ->
        Encoding = Encoding0
    ;
        error("backend_unicode_encoding: unexpected Unicode encoding code")
    ).

:- pred unicode_encoding_int_to_encoding(int::in, unicode_encoding::out)
    is semidet.

unicode_encoding_int_to_encoding(0, utf8).
unicode_encoding_int_to_encoding(1, utf16).

:- func backend_unicode_encoding_int = int.

:- pragma inline(backend_unicode_encoding_int/0).

:- pragma foreign_proc("C",
    backend_unicode_encoding_int = (EncodingInt::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    EncodingInt = 0;
").

:- pragma foreign_proc("Java",
    backend_unicode_encoding_int = (EncodingInt::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    EncodingInt = 1;
").

:- pragma foreign_proc("C#",
    backend_unicode_encoding_int = (EncodingInt::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
    EncodingInt = 1;
").

:- pred string_get_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
        Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_hex_digit(Char) ->
            string_get_hex_escape(String, Len, QuoteChar, Chars,
                [Char | HexChars], Posn0, Token, Context, !Posn)
        ; Char = ('\\') ->
            string_finish_hex_escape(String, Len, QuoteChar, Chars,
                HexChars, Posn0, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = error("unterminated hex escape")
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = error("unterminated quote")
    ).

:- pred finish_hex_escape(char::in, list(char)::in, list(char)::in,
    token::out, io::di, io::uo) is det.

finish_hex_escape(QuoteChar, Chars, HexChars, Token, !IO) :-
    (
        HexChars = [],
        Token = error("empty hex escape")
    ;
        HexChars = [_ | _],
        rev_char_list_to_string(HexChars, HexString),
        (
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        ->
            get_quoted_name(QuoteChar, [Char|Chars], Token, !IO)
        ;
            Token = error("invalid hex escape")
        )
    ).

:- pred string_finish_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_finish_hex_escape(String, Len, QuoteChar, Chars, HexChars, Posn0,
        Token, Context, !Posn) :-
    (
        HexChars = [],
        string_get_context(Posn0, Context, !Posn),
        Token = error("empty hex escape")
    ;
        HexChars = [_ | _],
        rev_char_list_to_string(HexChars, HexString),
        (
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        ->
            string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                Posn0, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = error("invalid hex escape")
        )
    ).

:- pred get_octal_escape(char::in, list(char)::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_octal_escape(QuoteChar, Chars, OctalChars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated quote")
    ;
        Result = ok(Char),
        ( char.is_octal_digit(Char) ->
            get_octal_escape(QuoteChar, Chars, [Char | OctalChars], Token, !IO)
        ; Char = ('\\') ->
            finish_octal_escape(QuoteChar, Chars,
                OctalChars, Token, !IO)
        ;
            % XXX We don't report this as an error since we need bug-for-bug
            % compatibility with NU-Prolog.
            % Token = error("unterminated octal escape")
            io.putback_char(Char, !IO),
            finish_octal_escape(QuoteChar, Chars, OctalChars, Token, !IO)
        )
    ).

:- pred string_get_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
        Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_octal_digit(Char) ->
            string_get_octal_escape(String, Len, QuoteChar, Chars,
                [Char | OctalChars], Posn0, Token, Context, !Posn)
        ; Char = ('\\') ->
            string_finish_octal_escape(String, Len, QuoteChar, Chars,
                OctalChars, Posn0, Token, Context, !Posn)
        ;
            % XXX We don't report this as an error since we need bug-for-bug
            % compatibility with NU-Prolog.
            % Token = error("unterminated octal escape")
            string_ungetchar(String, !Posn),
            string_finish_octal_escape(String, Len, QuoteChar, Chars,
                OctalChars, Posn0, Token, Context, !Posn)
        )
    ;
        Token = error("unterminated quote"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred finish_octal_escape(char::in, list(char)::in, list(char)::in,
    token::out, io::di, io::uo) is det.

finish_octal_escape(QuoteChar, Chars, OctalChars, Token, !IO) :-
    (
        OctalChars = [],
        Token = error("empty octal escape")
    ;
        OctalChars = [_ | _],
        rev_char_list_to_string(OctalChars, OctalString),
        (
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        ->
            get_quoted_name(QuoteChar, [Char | Chars], Token, !IO)
        ;
            Token = error("invalid octal escape")
        )
    ).

:- pred string_finish_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_finish_octal_escape(String, Len, QuoteChar, Chars, OctalChars,
        Posn0, Token, Context, !Posn) :-
    (
        OctalChars = [],
        Token = error("empty octal escape"),
        string_get_context(Posn0, Context, !Posn)
    ;
        OctalChars = [_ | _],
        rev_char_list_to_string(OctalChars, OctalString),
        (
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        ->
            string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                Posn0, Token, Context, !Posn)
        ;
            Token = error("invalid octal escape"),
            string_get_context(Posn0, Context, !Posn)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Names and variables.

:- pred get_name(list(char)::in, token::out, io::di, io::uo) is det.

get_name(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_string(Chars, Name),
        Token = name(Name)
    ;
        Result = ok(Char),
        ( char.is_alnum_or_underscore(Char) ->
            get_name([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_string(Chars, Name),
            Token = name(Name)
        )
    ).

:- pred string_get_name(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_name(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_alnum_or_underscore(Char) ->
            string_get_name(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, Name, !Posn),
            Token = name(Name),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, Name, !Posn),
        Token = name(Name),
        string_get_context(Posn0, Context, !Posn)
    ).

    % A line number directive token is `#' followed by an integer
    % (specifying the line number) followed by a newline.
    % Such a token sets the source line number for the next
    % line, but it is otherwise ignored.  This means that line number
    % directives may appear anywhere that a token may appear, including
    % in the middle of terms.
    % (The source file name can be set with a `:- pragma source_file'
    % declaration.)
    %
:- pred get_source_line_number(list(char)::in, token::out,
    token_context::out, io::di, io::uo) is det.

get_source_line_number(Chars, Token, Context, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        get_context(Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Context, !IO),
        Token = error("unexpected end-of-file in `#' line number directive")
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_source_line_number([Char | Chars], Token, Context, !IO)
        ; Char = '\n' ->
            rev_char_list_to_string(Chars, String),
            (
                string.base_string_to_int(10, String, Int),
                Int > 0
            ->
                io.set_line_number(Int, !IO),
                get_token(Token, Context, !IO)
            ;
                get_context(Context, !IO),
                string.append_list(["invalid line number `", String,
                    "' in `#' line number directive"], Message),
                Token = error(Message)
            )
        ;
            get_context(Context, !IO),
            string.from_char_list([Char], String),
            string.append_list(["invalid character `", String,
                "' in `#' line number directive"], Message),
            Token = error(Message)
        )
    ).

:- pred string_get_source_line_number(string::in, int::in, posn::in,
    token::out, token_context::out, posn::in, posn::out) is det.

string_get_source_line_number(String, Len, Posn1, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_source_line_number(String, Len, Posn1, Token, Context,
                !Posn)
        ; Char = '\n' ->
            grab_string(String, Posn1, LineNumString, !Posn),
            (
                string.base_string_to_int(10, LineNumString, LineNum),
                LineNum > 0
            ->
                string_set_line_number(LineNum, !Posn),
                string_get_token(String, Len, Token, Context, !Posn)
            ;
                string_get_context(Posn1, Context, !Posn),
                string.append_list(["invalid line number `", LineNumString,
                    "' in `#' line number directive"], Message),
                Token = error(Message)
            )
        ;
            string_get_context(Posn1, Context, !Posn),
            string.from_char_list([Char], DirectiveString),
            string.append_list(["invalid character `", DirectiveString,
                "' in `#' line number directive"], Message),
            Token = error(Message)
        )
    ;
        string_get_context(Posn1, Context, !Posn),
        Token = error("unexpected end-of-file in `#' line number directive")
    ).

:- pred get_graphic(list(char)::in, token::out, io::di, io::uo) is det.

get_graphic(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_string(Chars, Name),
        Token = name(Name)
    ;
        Result = ok(Char),
        ( graphic_token_char(Char) ->
            get_graphic([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_string(Chars, Name),
            Token = name(Name)
        )
    ).

:- pred string_get_graphic(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_graphic(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, Name, !Posn),
            Token = name(Name),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, Name, !Posn),
        string_get_context(Posn0, Context, !Posn),
        Token = name(Name)
    ).

:- pred get_variable(list(char)::in, token::out, io::di, io::uo) is det.

get_variable(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_string(Chars, VariableName),
        Token = variable(VariableName)
    ;
        Result = ok(Char),
        ( char.is_alnum_or_underscore(Char) ->
            get_variable([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_string(Chars, VariableName),
            Token = variable(VariableName)
        )
    ).

:- pred string_get_variable(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_variable(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_alnum_or_underscore(Char) ->
            string_get_variable(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, VariableName, !Posn),
            Token = variable(VariableName),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, VariableName, !Posn),
        Token = variable(VariableName),
        string_get_context(Posn0, Context, !Posn)
    ).

%-----------------------------------------------------------------------------%
%
% Integer and float literals.

:- pred get_zero(token::out, io::di, io::uo) is det.

get_zero(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = integer(0)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_number([Char], Token, !IO)
        ; Char = '''' ->
            get_char_code(Token, !IO)
        ; Char = 'b' ->
            get_binary(Token, !IO)
        ; Char = 'o' ->
            get_octal(Token, !IO)
        ; Char = 'x' ->
            get_hex(Token, !IO)
        ; Char = ('.') ->
            get_int_dot(['0'], Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent([Char, '0'], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = integer(0)
        )
    ).

:- pred string_get_zero(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_zero(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_number(String, Len, Posn0, Token, Context, !Posn)
        ; Char = '''' ->
            string_get_char_code(String, Len, Posn0, Token, Context, !Posn)
        ; Char = 'b' ->
            string_get_binary(String, Len, Posn0, Token, Context, !Posn)
        ; Char = 'o' ->
            string_get_octal(String, Len, Posn0, Token, Context, !Posn)
        ; Char = 'x' ->
            string_get_hex(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('.') ->
            string_get_int_dot(String, Len, Posn0, Token, Context, !Posn)
        ; ( Char = 'e' ; Char = 'E' ) ->
            string_get_float_exponent(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = integer(0)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = integer(0)
    ).

:- pred get_char_code(token::out, io::di, io::uo) is det.

get_char_code(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated char code constant")
    ;
        Result = ok(Char),
        char.to_int(Char, CharCode),
        Token = integer(CharCode)
    ).

:- pred string_get_char_code(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_char_code(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        char.to_int(Char, CharCode),
        Token = integer(CharCode),
        string_get_context(Posn0, Context, !Posn)
    ;
        Token = error("unterminated char code constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_binary(token::out, io::di, io::uo) is det.

get_binary(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated binary constant")
    ;
        Result = ok(Char),
        ( char.is_binary_digit(Char) ->
            get_binary_2([Char], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = error("unterminated binary constant")
        )
    ).

:- pred string_get_binary(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_binary(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_binary_digit(Char) ->
            string_get_binary_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated binary constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated binary constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_binary_2(list(char)::in, token::out, io::di, io::uo) is det.

get_binary_2(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 2, Token)
    ;
        Result = ok(Char),
        ( char.is_binary_digit(Char) ->
            get_binary_2([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_int(Chars, 2, Token)
        )
    ).

:- pred string_get_binary_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_binary_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_binary_digit(Char) ->
            string_get_binary_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 2, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 2, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_octal(token::out, io::di, io::uo) is det.

get_octal(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated octal constant")
    ;
        Result = ok(Char),
        ( char.is_octal_digit(Char) ->
            get_octal_2([Char], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = error("unterminated octal constant")
        )
    ).

:- pred string_get_octal(string::in, int::in, posn::in, token::out,
    string_token_context::out,
    posn::in, posn::out) is det.

string_get_octal(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_octal_digit(Char) ->
            string_get_octal_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated octal constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated octal constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_octal_2(list(char)::in, token::out, io::di, io::uo) is det.

get_octal_2(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 8, Token)
    ;
        Result = ok(Char),
        ( char.is_octal_digit(Char) ->
            get_octal_2([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_int(Chars, 8, Token)
        )
    ).

:- pred string_get_octal_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_octal_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_octal_digit(Char) ->
            string_get_octal_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 8, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 8, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_hex(token::out, io::di, io::uo) is det.

get_hex(Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated hex constant")
    ;
        Result = ok(Char),
        ( char.is_hex_digit(Char) ->
            get_hex_2([Char], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = error("unterminated hex constant")
        )
    ).

:- pred string_get_hex(string::in, int::in, posn::in, token::out,
    string_token_context::out,
    posn::in, posn::out) is det.

string_get_hex(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_hex_digit(Char) ->
            string_get_hex_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated hex constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated hex constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_hex_2(list(char)::in, token::out, io::di, io::uo) is det.

get_hex_2(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 16, Token)
    ;
        Result = ok(Char),
        ( char.is_hex_digit(Char) ->
            get_hex_2([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_int(Chars, 16, Token)
        )
    ).

:- pred string_get_hex_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_hex_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_hex_digit(Char) ->
            string_get_hex_2(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 16, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 16, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_number(list(char)::in, token::out, io::di, io::uo) is det.

get_number(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 10, Token)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_number([Char | Chars], Token, !IO)
        ; Char = ('.') ->
            get_int_dot(Chars, Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_int(Chars, 10, Token)
        )
    ).

:- pred string_get_number(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_number(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_number(String, Len, Posn0, Token, Context, !Posn)
        ; Char = ('.') ->
            string_get_int_dot(String, Len, Posn0, Token, Context, !Posn)
        ; ( Char = 'e' ; Char = 'E' ) ->
            string_get_float_exponent(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, NumberString, !Posn),
            conv_string_to_int(NumberString, 10, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, NumberString, !Posn),
        conv_string_to_int(NumberString, 10, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_int_dot(list(char)::in, token::out, io::di, io::uo) is det.

get_int_dot(Chars, Token, !IO) :-
    % XXX The float literal syntax doesn't match ISO Prolog.
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        io.putback_char('.', !IO),
        rev_char_list_to_int(Chars, 10, Token)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_decimals([Char, '.' | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            % We can't putback the ".", because io.putback_char only
            % guarantees one character of pushback. So instead, we return
            % an `integer_dot' token; the main loop of get_token_list_2 will
            % handle this appropriately.
            rev_char_list_to_int(Chars, 10, Token0),
            ( Token0 = integer(Int) ->
                Token = integer_dot(Int)
            ;
                Token = Token0
            )
        )
    ).

:- pred string_get_int_dot(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_int_dot(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_float_decimals(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, NumberString, !Posn),
            conv_string_to_int(NumberString, 10, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        string_ungetchar(String, !Posn),
        grab_string(String, Posn0, NumberString, !Posn),
        conv_string_to_int(NumberString, 10, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_float_decimals(list(char)::in, token::out,
    io::di, io::uo) is det.

    % We've read past the decimal point, so now get the decimals.
    %
get_float_decimals(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_decimals([Char | Chars], Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_float(Chars, Token)
        )
    ).

:- pred string_get_float_decimals(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_decimals(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_float_decimals(String, Len, Posn0, Token, Context,
                !Posn)
        ; ( Char = 'e' ; Char = 'E' ) ->
            string_get_float_exponent(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, FloatString, !Posn),
            conv_to_float(FloatString, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, FloatString, !Posn),
        conv_to_float(FloatString, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_float_exponent(list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok(Char),
        ( ( Char = ('+') ; Char = ('-') ) ->
            get_float_exponent_2([Char | Chars], Token, !IO)
        ; char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = error("unterminated exponent in float token")
        )
    ).

:- pred string_get_float_exponent(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( ( Char = ('+') ; Char = ('-') ) ->
            string_get_float_exponent_2(String, Len, Posn0, Token, Context,
                !Posn)
        ; char.is_digit(Char) ->
            string_get_float_exponent_3(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated exponent in float token"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, FloatString, !Posn),
        conv_to_float(FloatString, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_float_exponent_2(list(char)::in, token::out,
    io::di, io::uo) is det.

    % We've read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
get_float_exponent_2(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated exponent in float token")
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            Token = error("unterminated exponent in float token")
        )
    ).

:- pred string_get_float_exponent_2(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

    % We've read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
string_get_float_exponent_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_float_exponent_3(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated exponent in float token"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated exponent in float token"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_float_exponent_3(list(char)::in, token::out,
    io::di, io::uo) is det.

    % We've read past the first digit of the exponent -
    % now get the remaining digits.
    %
get_float_exponent_3(Chars, Token, !IO) :-
    io.read_char(Result, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok(Char),
        ( char.is_digit(Char) ->
            get_float_exponent_3([Char | Chars], Token, !IO)
        ;
            io.putback_char(Char, !IO),
            rev_char_list_to_float(Chars, Token)
        )
    ).

:- pred string_get_float_exponent_3(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent_3(String, Len, Posn0, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_digit(Char) ->
            string_get_float_exponent_3(String, Len, Posn0, Token, Context,
                !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, FloatString, !Posn),
            conv_to_float(FloatString, Token),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        grab_string(String, Posn0, FloatString, !Posn),
        conv_to_float(FloatString, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

%-----------------------------------------------------------------------------%
%
% Utility routines.

:- pred rev_char_list_to_int(list(char)::in, int::in, token::out) is det.

rev_char_list_to_int(RevChars, Base, Token) :-
    rev_char_list_to_string(RevChars, String),
    conv_string_to_int(String, Base, Token).

:- pred conv_string_to_int(string::in, int::in, token::out) is det.

conv_string_to_int(String, Base, Token) :-
    ( string.base_string_to_int(Base, String, Int) ->
        Token = integer(Int)
    ;
        Token = error("invalid integer token")
    ).

:- pred rev_char_list_to_float(list(char)::in, token::out) is det.

rev_char_list_to_float(RevChars, Token) :-
    rev_char_list_to_string(RevChars, String),
    conv_to_float(String, Token).

:- pred conv_to_float(string::in, token::out) is det.

conv_to_float(String, Token) :-
    ( string.to_float(String, Float) ->
        Token = float(Float)
    ;
        Token = error("invalid float token")
    ).

:- pred rev_char_list_to_string(list(char)::in, string::out) is det.

rev_char_list_to_string(RevChars, String) :-
   string.from_rev_char_list(RevChars, String).

%-----------------------------------------------------------------------------%
