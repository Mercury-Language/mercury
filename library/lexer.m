%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2008, 2011-2012 The University of Melbourne.
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module lexer.
:- interface.

:- import_module char.
:- import_module io.

%---------------------------------------------------------------------------%

:- type token
    --->    name(string)
    ;       variable(string)
    ;       integer(int)
    ;       big_integer(string) % does not fit in int
    ;       float(float)
    ;       string(string)      % "...."
    ;       implementation_defined(string) % $name
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

:- interface.

    % graphic_token_char(Char): true iff `Char'
    % is "graphic token char" (ISO Prolog 6.4.2).
    % This is exported for use by term_io.quote_atom.
    %
:- pred graphic_token_char(char::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

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

%---------------------------------------------------------------------------%

token_to_string(name(Name), String) :-
    string.append_list(["token '", Name, "'"], String).
token_to_string(variable(Var), String) :-
    string.append_list(["variable `", Var, "'"], String).
token_to_string(integer(Int), String) :-
    string.int_to_string(Int, IntString),
    string.append_list(["integer `", IntString, "'"], String).
token_to_string(big_integer(BigInt), String) :-
    string.append_list(["big integer `", BigInt, "'"], String).
token_to_string(float(Float), String) :-
    string.float_to_string(Float, FloatString),
    string.append_list(["float `", FloatString, "'"], String).
token_to_string(string(Token), String) :-
    string.append_list(["string """, Token, """"], String).
token_to_string(implementation_defined(Name), String) :-
    string.append_list(["implementation-defined `$", Name, "'"], String).
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
    io.input_stream(Stream, !IO),
    get_token(Stream, Token, Context, !IO),
    get_token_list_2(Stream, Token, Context, Tokens, !IO).

:- pred get_token_list_2(io.input_stream::in, token::in, token_context::in,
    token_list::out, io::di, io::uo) is det.

get_token_list_2(Stream, Token0, Context0, Tokens, !IO) :-
    (
        Token0 = eof,
        Tokens = token_nil
    ;
        ( Token0 = end
        ; Token0 = error(_)
        ; Token0 = io_error(_)
        ),
        Tokens = token_cons(Token0, Context0, token_nil)
    ;
        Token0 = integer_dot(Int),
        get_context(Stream, Context1, !IO),
        get_dot(Stream, Token1, !IO),
        get_token_list_2(Stream, Token1, Context1, Tokens1, !IO),
        Tokens = token_cons(integer(Int), Context0, Tokens1)
    ;
        ( Token0 = float(_)
        ; Token0 = string(_)
        ; Token0 = variable(_)
        ; Token0 = integer(_)
        ; Token0 = big_integer(_)
        ; Token0 = implementation_defined(_)
        ; Token0 = junk(_)
        ; Token0 = name(_)
        ; Token0 = open
        ; Token0 = open_ct
        ; Token0 = close
        ; Token0 = open_list
        ; Token0 = close_list
        ; Token0 = open_curly
        ; Token0 = close_curly
        ; Token0 = comma
        ; Token0 = ht_sep
        ),
        get_token(Stream, Token1, Context1, !IO),
        get_token_list_2(Stream, Token1, Context1, Tokens1, !IO),
        Tokens = token_cons(Token0, Context0, Tokens1)
    ).

string_get_token_list(String, Tokens, !Posn) :-
    string.length(String, Len),
    string_get_token_list_max(String, Len, Tokens, !Posn).

string_get_token_list_max(String, Len, Tokens, !Posn) :-
    string_get_token(String, Len, Token, Context, !Posn),
    (
        Token = eof,
        Tokens = token_nil
    ;
        ( Token = end
        ; Token = error(_)
        ; Token = io_error(_)
        ),
        Tokens = token_cons(Token, Context, token_nil)
    ;
        ( Token = float(_)
        ; Token = string(_)
        ; Token = variable(_)
        ; Token = integer(_)
        ; Token = big_integer(_)
        ; Token = integer_dot(_)
        ; Token = implementation_defined(_)
        ; Token = junk(_)
        ; Token = name(_)
        ; Token = open
        ; Token = open_ct
        ; Token = close
        ; Token = open_list
        ; Token = close_list
        ; Token = open_curly
        ; Token = close_curly
        ; Token = comma
        ; Token = ht_sep
        ),
        Tokens = token_cons(Token, Context, Tokens1),
        string_get_token_list_max(String, Len, Tokens1, !Posn)
    ).

%---------------------------------------------------------------------------%
%
% Some low-level routines.

:- pred get_context(io.input_stream::in, token_context::out, io::di, io::uo)
    is det.

get_context(Stream, Context, !IO) :-
    io.get_line_number(Stream, Context, !IO).

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
    string.unsafe_index_next(String, Offset0, Offset, Char),
    ( Char = '\n' ->
        LineNum = LineNum0 + 1,
        Posn = posn(LineNum, Offset, Offset)
    ;
        Posn = posn(LineNum0, LineOffset0, Offset)
    ).

:- pred string_ungetchar(string::in, posn::in, posn::out) is det.

string_ungetchar(String, Posn0, Posn) :-
    Posn0 = posn(LineNum0, LineOffset0, Offset0),
    ( string.unsafe_prev_index(String, Offset0, Offset, Char) ->
        ( Char = '\n' ->
            LineNum = LineNum0 - 1,
            Posn = posn(LineNum, Offset, Offset)
        ;
            Posn = posn(LineNum0, LineOffset0, Offset)
        )
    ;
        Posn = Posn0
    ).

:- pred grab_string(string::in, posn::in, string::out,
    posn::in, posn::out) is det.

grab_string(String, Posn0, SubString, Posn, Posn) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    string.unsafe_between(String, Offset0, Offset, SubString).

:- pred string_set_line_number(int::in, posn::in, posn::out) is det.

string_set_line_number(LineNumber, Posn0, Posn) :-
    Posn0 = posn(_, _, Offset),
    Posn = posn(LineNumber, Offset, Offset).

%---------------------------------------------------------------------------%

:- type get_token_action
    --->    action_whitespace
    ;       action_alpha_lower
    ;       action_alpha_upper_uscore
    ;       action_zero
    ;       action_nonzero_digit
    ;       action_special_token
    ;       action_dot
    ;       action_percent
    ;       action_quote
    ;       action_slash
    ;       action_hash
    ;       action_backquote
    ;       action_dollar
    ;       action_graphic_token.

:- type scanned_past_whitespace
    --->    scanned_past_whitespace
    ;       not_scanned_past_whitespace.

:- pred get_token(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

get_token(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = eof
    ;
        Result = ok,
        ( lookup_token_action(Char, Action) ->
            execute_get_token_action(Stream, Char, Action,
                not_scanned_past_whitespace, Token, Context, !IO)
        ;
            get_context(Stream, Context, !IO),
            Token = junk(Char)
        )
    ).

    % This is just like get_token, except that we have already scanned past
    % some whitespace, so '(' gets scanned as `open' rather than `open_ct'.
    %
:- pred get_token_2(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

get_token_2(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = eof
    ;
        Result = ok,
        ( lookup_token_action(Char, Action) ->
            execute_get_token_action(Stream, Char, Action,
                scanned_past_whitespace, Token, Context, !IO)
        ;
            get_context(Stream, Context, !IO),
            Token = junk(Char)
        )
    ).

:- pred string_get_token(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_get_token(String, Len, Token, Context, !Posn) :-
    Posn0 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( lookup_token_action(Char, Action) ->
            execute_string_get_token_action(String, Len, Posn0, Char, Action,
                not_scanned_past_whitespace, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = junk(Char)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

:- pred string_get_token_2(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_get_token_2(String, Len, Token, Context, !Posn) :-
    Posn0 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( lookup_token_action(Char, Action) ->
            execute_string_get_token_action(String, Len, Posn0, Char, Action,
                scanned_past_whitespace, Token, Context, !Posn)
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = junk(Char)
        )
    ;
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

    % Decide on how the given character should be treated. Note that
    % performance suffers significantly if this predicate is not inlined.
    %
:- pragma inline(lookup_token_action/2).
:- pred lookup_token_action(char::in, get_token_action::out) is semidet.

lookup_token_action(Char, Action) :-
    % The body of this predicate should be turned into a single table lookup
    % by the compiler.
    (
        % This list of characters comes from the code of char.is_whitespace.
        % Any update here will also require an update there.
        ( Char = ' '
        ; Char = '\t'
        ; Char = '\n'
        ; Char = '\r'
        ; Char = '\f'
        ; Char = '\v'
        ),
        Action = action_whitespace
    ;
        % This list of characters comes from char.is_alnum_or_underscore and
        % char.lower_upper.
        ( Char = 'a' ; Char = 'b' ; Char = 'c' ; Char = 'd'
        ; Char = 'e' ; Char = 'f' ; Char = 'g' ; Char = 'h'
        ; Char = 'i' ; Char = 'j' ; Char = 'k' ; Char = 'l'
        ; Char = 'm' ; Char = 'n' ; Char = 'o' ; Char = 'p'
        ; Char = 'q' ; Char = 'r' ; Char = 's' ; Char = 't'
        ; Char = 'u' ; Char = 'v' ; Char = 'w' ; Char = 'x'
        ; Char = 'y' ; Char = 'z'
        ),
        Action = action_alpha_lower
    ;
        % This list of characters comes from char.is_alnum_or_underscore and
        % char.lower_upper.
        ( Char = '_'
        ; Char = 'A' ; Char = 'B' ; Char = 'C' ; Char = 'D'
        ; Char = 'E' ; Char = 'F' ; Char = 'G' ; Char = 'H'
        ; Char = 'I' ; Char = 'J' ; Char = 'K' ; Char = 'L'
        ; Char = 'M' ; Char = 'N' ; Char = 'O' ; Char = 'P'
        ; Char = 'Q' ; Char = 'R' ; Char = 'S' ; Char = 'T'
        ; Char = 'U' ; Char = 'V' ; Char = 'W' ; Char = 'X'
        ; Char = 'Y' ; Char = 'Z'
        ),
        Action = action_alpha_upper_uscore
    ;
        Char = '0',
        Action = action_zero
    ;
        % This list of characters comes from char.is_alnum_or_underscore and
        % char.is_digit.
        ( Char = '1' ; Char = '2' ; Char = '3' ; Char = '4'
        ; Char = '5' ; Char = '6' ; Char = '7' ; Char = '8'
        ; Char = '9'
        ),
        Action = action_nonzero_digit
    ;
        % These are the characters for which special_token succeeds.
        ( Char = ('(')
        ; Char = (')')
        ; Char = ('[')
        ; Char = (']')
        ; Char = ('{')
        ; Char = ('}')
        ; Char = ('|')
        ; Char = (',')
        ; Char = (';')
        ),
        Action = action_special_token
    ;
        Char = ('.'),
        Action = action_dot
    ;
        Char = ('%'),
        Action = action_percent
    ;
        ( Char = '"'
        ; Char = ''''
        ),
        Action = action_quote
    ;
        Char = ('/'),
        Action = action_slash
    ;
        Char = ('#'),
        Action = action_hash
    ;
        Char = ('`'),
        Action = action_backquote
    ;
        Char = ('$'),
        Action = action_dollar
    ;
        % These are the characters for which graphic_token_char succeeds.
        % The ones that are commented out have their own actions.
        ( Char = ('!')
        % ; Char = ('#')    handled as action_hash
        % ; Char = ('$')    handled as action_dollar
        ; Char = ('&')
        ; Char = ('*')
        ; Char = ('+')
        ; Char = ('-')
        % ; Char = ('.')    handled as action_dot
        % ; Char = ('/')    handled as action_slash
        ; Char = (':')
        ; Char = ('<')
        ; Char = ('=')
        ; Char = ('>')
        ; Char = ('?')
        ; Char = ('@')
        ; Char = ('^')
        ; Char = ('~')
        ; Char = ('\\')
        ),
        Action = action_graphic_token
    ).

%---------------------------------------------------------------------------%

    % Handle the character we just read the way lookup_token_action decided
    % it should be treated. Note that inlining this predicate does not
    % significantly affect performance.
    %
% :- pragma inline(execute_get_token_action/8).
:- pred execute_get_token_action(io.input_stream::in, char::in,
    get_token_action::in, scanned_past_whitespace::in, token::out,
    token_context::out, io::di, io::uo) is det.

execute_get_token_action(Stream, Char, Action, ScannedPastWhiteSpace,
        Token, Context, !IO) :-
    (
        Action = action_whitespace,
        get_token_2(Stream, Token, Context, !IO)
    ;
        Action = action_alpha_upper_uscore,
        get_context(Stream, Context, !IO),
        get_variable(Stream, [Char], Token, !IO)
    ;
        Action = action_alpha_lower,
        get_context(Stream, Context, !IO),
        get_name(Stream, [Char], Token, !IO)
    ;
        Action = action_zero,
        get_context(Stream, Context, !IO),
        get_zero(Stream, Token, !IO)
    ;
        Action = action_nonzero_digit,
        get_context(Stream, Context, !IO),
        get_number(Stream, [Char], Token, !IO)
    ;
        Action = action_special_token,
        get_context(Stream, Context, !IO),
        handle_special_token(Char, ScannedPastWhiteSpace, Token)
    ;
        Action = action_dot,
        get_context(Stream, Context, !IO),
        get_dot(Stream, Token, !IO)
    ;
        Action = action_percent,
        skip_to_eol(Stream, Token, Context, !IO)
    ;
        Action = action_quote,
        get_context(Stream, Context, !IO),
        start_quoted_name(Stream, Char, [], Token, !IO)
    ;
        Action = action_slash,
        get_slash(Stream, Token, Context, !IO)
    ;
        Action = action_hash,
        get_source_line_number(Stream, [], Token, Context, !IO)
    ;
        Action = action_backquote,
        get_context(Stream, Context, !IO),
        Token = name("`")
    ;
        Action = action_dollar,
        get_context(Stream, Context, !IO),
        get_implementation_defined_literal_rest(Stream, Token, !IO)
    ;
        Action = action_graphic_token,
        get_context(Stream, Context, !IO),
        get_graphic(Stream, [Char], Token, !IO)
    ).

    % The string version of execute_get_token_action.
    %
% :- pragma inline(execute_string_get_token_action/10).
:- pred execute_string_get_token_action(string::in, int::in, posn::in,
    char::in, get_token_action::in, scanned_past_whitespace::in, token::out,
    token_context::out, posn::in, posn::out) is det.

execute_string_get_token_action(String, Len, Posn0, Char, Action,
        ScannedPastWhiteSpace, Token, Context, !Posn) :-
    (
        Action = action_whitespace,
        string_get_token_2(String, Len, Token, Context, !Posn)
    ;
        Action = action_alpha_upper_uscore,
        string_get_variable(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_alpha_lower,
        string_get_name(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_zero,
        string_get_zero(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_nonzero_digit,
        string_get_number(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_special_token,
        string_get_context(Posn0, Context, !Posn),
        handle_special_token(Char, ScannedPastWhiteSpace, Token)
    ;
        Action = action_dot,
        string_get_dot(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_percent,
        string_skip_to_eol(String, Len, Token, Context, !Posn)
    ;
        Action = action_quote,
        string_start_quoted_name(String, Len, Char, [], Posn0, Token,
            Context, !Posn)
    ;
        Action = action_slash,
        string_get_slash(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_hash,
        string_get_source_line_number(String, Len, !.Posn, Token, Context,
            !Posn)
    ;
        Action = action_backquote,
        string_get_context(Posn0, Context, !Posn),
        Token = name("`")
    ;
        Action = action_dollar,
        string_get_implementation_defined_literal_rest(String, Len, Posn0,
            Token, Context, !Posn)
    ;
        Action = action_graphic_token,
        string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
    ).

%---------------------------------------------------------------------------%

    % Decide what to do for a token which consists of a special character.
    % The reason for inlining this predicate is that each caller has a
    % specific value for ScannedPastWhiteSpace, and thus after inlining,
    % the compiler should be able to eliminate the switch on
    % ScannedPastWhiteSpace.
    %
:- pragma inline(handle_special_token/3).
:- pred handle_special_token(char::in, scanned_past_whitespace::in, token::out)
    is det.

handle_special_token(Char, ScannedPastWhiteSpace, Token) :-
    ( special_token(Char, SpecialToken) ->
        (
            ScannedPastWhiteSpace = not_scanned_past_whitespace,
            ( SpecialToken = open ->
                Token = open_ct
            ;
                Token = SpecialToken
            )
        ;
            ScannedPastWhiteSpace = scanned_past_whitespace,
            Token = SpecialToken
        )
    ;
        error("lexer.m, handle_special_token: unknown special token")
    ).

:- pred special_token(char::in, token::out) is semidet.

% The list of characters here is duplicated in lookup_token_action above.
special_token('(', open).    % May get converted to open_ct above.
special_token(')', close).
special_token('[', open_list).
special_token(']', close_list).
special_token('{', open_curly).
special_token('}', close_curly).
special_token('|', ht_sep).
special_token(',', comma).
special_token(';', name(";")).

% The list of characters here is duplicated in lookup_token_action above.
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

%---------------------------------------------------------------------------%

:- pred get_dot(io.input_stream::in, token::out, io::di, io::uo) is det.

get_dot(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = end
    ;
        Result = ok,
        ( whitespace_after_dot(Char) ->
            io.putback_char(Stream, Char, !IO),
            Token = end
        ; graphic_token_char(Char) ->
            get_graphic(Stream, [Char, '.'], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

%---------------------------------------------------------------------------%
%
% Comments.

:- pred skip_to_eol(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

skip_to_eol(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = eof
    ;
        Result = ok,
        ( Char = '\n' ->
            get_token_2(Stream, Token, Context, !IO)
        ;
            skip_to_eol(Stream, Token, Context, !IO)
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

:- pred get_slash(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

get_slash(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = name("/")
    ;
        Result = ok,
        ( Char = ('*') ->
            get_comment(Stream, Token, Context, !IO)
        ; graphic_token_char(Char) ->
            get_context(Stream, Context, !IO),
            get_graphic(Stream, [Char, '/'], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            get_context(Stream, Context, !IO),
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

:- pred get_comment(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

get_comment(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok,
        ( Char = ('*') ->
            get_comment_2(Stream, Token, Context, !IO)
        ;
            get_comment(Stream, Token, Context, !IO)
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

:- pred get_comment_2(io.input_stream::in, token::out, token_context::out,
    io::di, io::uo) is det.

get_comment_2(Stream, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok,
        ( Char = ('/') ->
            % end of /* ... */ comment, so get next token
            get_token_2(Stream, Token, Context, !IO)
        ; Char = ('*') ->
            get_comment_2(Stream, Token, Context, !IO)
        ;
            get_comment(Stream, Token, Context, !IO)
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

%---------------------------------------------------------------------------%
%
% Quoted names and quoted strings.

:- pred start_quoted_name(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

start_quoted_name(Stream, QuoteChar, Chars, Token, !IO) :-
    get_quoted_name(Stream, QuoteChar, Chars, Token0, !IO),
    ( Token0 = error(_) ->
        % Skip to the end of the string or name.
        start_quoted_name(Stream, QuoteChar, Chars, _, !IO),
        Token = Token0
    ; Token0 = eof ->
        Token = error("unterminated quote")
    ;
        Token = Token0
    ).

:- pred string_start_quoted_name(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_start_quoted_name(String, Len, QuoteChar, Chars, Posn0,
        Token, Context, !Posn) :-
    string_get_quoted_name(String, Len, QuoteChar, Chars, Posn0,
        Token0, Context, !Posn),
    ( Token0 = error(_) ->
        % Skip to the end of the string or name.
        string_start_quoted_name(String, Len, QuoteChar, Chars,
            Posn0, _, _, !Posn),
        Token = Token0
    ; Token0 = eof ->
        Token = error("unterminated quote")
    ;
        Token = Token0
    ).

:- pred get_quoted_name(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name(Stream, QuoteChar, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( Char = QuoteChar ->
            get_quoted_name_quote(Stream, QuoteChar, Chars, Token, !IO)
        ; Char = ('\\') ->
            get_quoted_name_escape(Stream, QuoteChar, Chars, Token, !IO)
        ;
            get_quoted_name(Stream, QuoteChar, [Char | Chars], Token, !IO)
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
        Token = eof
    ).

:- pred get_quoted_name_quote(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name_quote(Stream, QuoteChar, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        finish_quoted_name(QuoteChar, Chars, Token)
    ;
        Result = ok,
        ( Char = QuoteChar ->
            get_quoted_name(Stream, QuoteChar, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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
    ( rev_char_list_to_string(Chars, String) ->
        ( QuoteChar = '''' ->
            Token = name(String)
        ; QuoteChar = '"' ->
            Token = string(String)
        ;
            error("lexer.m: unknown quote character")
        )
    ;
        Token = error("invalid character in quoted name")
    ).

:- pred get_quoted_name_escape(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name_escape(Stream, QuoteChar, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( Char = '\n' ->
            get_quoted_name(Stream, QuoteChar, Chars, Token, !IO)
        ; Char = '\r' ->
            % Files created on Windows may have an extra return character.
            get_quoted_name_escape(Stream, QuoteChar, Chars, Token, !IO)
        ; escape_char(Char, EscapedChar) ->
            Chars1 = [EscapedChar | Chars],
            get_quoted_name(Stream, QuoteChar, Chars1, Token, !IO)
        ; Char = 'x' ->
            get_hex_escape(Stream, QuoteChar, Chars, [], Token, !IO)
        ; Char = 'u' ->
            get_unicode_escape(Stream, 4, QuoteChar, Chars, [], Token, !IO)
        ; Char = 'U' ->
            get_unicode_escape(Stream, 8, QuoteChar, Chars, [], Token, !IO)
        ; char.is_octal_digit(Char) ->
            get_octal_escape(Stream, QuoteChar, Chars, [Char], Token, !IO)
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
        Token = eof
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

:- pred get_unicode_escape(io.input_stream::in, int::in, char::in,
    list(char)::in, list(char)::in, token::out, io::di, io::uo) is det.

get_unicode_escape(Stream, NumHexChars, QuoteChar, Chars, HexChars, Token,
        !IO) :-
    ( if NumHexChars = list.length(HexChars) then
        ( if
            rev_char_list_to_string(HexChars, HexString),
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            allowed_unicode_char_code(UnicodeCharCode),
            char.from_int(UnicodeCharCode, UnicodeChar)
        then
            ( if UnicodeCharCode = 0 then
                Token = null_character_error
            else
                get_quoted_name(Stream, QuoteChar,
                    [UnicodeChar | Chars], Token, !IO)
            )
        else
            Token = error("invalid Unicode character code")
        )
    else
        io.read_char_unboxed(Stream, Result, Char, !IO),
        (
            Result = error(Error),
            Token = io_error(Error)
        ;
            Result = eof,
            Token = eof
        ;
            Result = ok,
            ( if char.is_hex_digit(Char) then
                get_unicode_escape(Stream, NumHexChars, QuoteChar, Chars,
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
        ( if
            rev_char_list_to_string(HexChars, HexString),
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            allowed_unicode_char_code(UnicodeCharCode),
            char.from_int(UnicodeCharCode, UnicodeChar)
        then
            RevCharsWithUnicode = [UnicodeChar | Chars],
            ( if UnicodeCharCode = 0 then
                string_get_context(Posn0, Context, !Posn),
                Token = null_character_error
            else
                string_get_quoted_name(String, Len, QuoteChar,
                    RevCharsWithUnicode, Posn0, Token, Context, !Posn)
            )
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
            Token = eof
        )
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

:- pred get_hex_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_hex_escape(Stream, QuoteChar, Chars, HexChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( char.is_hex_digit(Char) ->
            get_hex_escape(Stream, QuoteChar, Chars, [Char | HexChars], Token,
                !IO)
        ; Char = ('\\') ->
            finish_hex_escape(Stream, QuoteChar, Chars, HexChars, Token, !IO)
        ;
            Token = error("unterminated hex escape")
        )
    ).

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
        Token = eof
    ).

:- pred finish_hex_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

finish_hex_escape(Stream, QuoteChar, Chars, HexChars, Token, !IO) :-
    (
        HexChars = [],
        Token = error("empty hex escape")
    ;
        HexChars = [_ | _],
        (
            rev_char_list_to_string(HexChars, HexString),
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        ->
            ( Int = 0 ->
                Token = null_character_error
            ;
                get_quoted_name(Stream, QuoteChar, [Char|Chars], Token, !IO)
            )
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
        (
            rev_char_list_to_string(HexChars, HexString),
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        ->
            ( Int = 0 ->
                Token = null_character_error,
                string_get_context(Posn0, Context, !Posn)
            ;
                string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                    Posn0, Token, Context, !Posn)
            )
        ;
            string_get_context(Posn0, Context, !Posn),
            Token = error("invalid hex escape")
        )
    ).

:- pred get_octal_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_octal_escape(Stream, QuoteChar, Chars, OctalChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( char.is_octal_digit(Char) ->
            get_octal_escape(Stream, QuoteChar, Chars, [Char | OctalChars],
                Token, !IO)
        ; Char = ('\\') ->
            finish_octal_escape(Stream, QuoteChar, Chars,
                OctalChars, Token, !IO)
        ;
            % XXX We don't report this as an error since we need bug-for-bug
            % compatibility with NU-Prolog.
            % Token = error("unterminated octal escape")
            io.putback_char(Stream, Char, !IO),
            finish_octal_escape(Stream, QuoteChar, Chars, OctalChars, Token,
                !IO)
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
        Token = eof,
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred finish_octal_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

finish_octal_escape(Stream, QuoteChar, Chars, OctalChars, Token, !IO) :-
    (
        OctalChars = [],
        Token = error("empty octal escape")
    ;
        OctalChars = [_ | _],
        (
            rev_char_list_to_string(OctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        ->
            ( Int = 0 ->
                Token = null_character_error
            ;
                get_quoted_name(Stream, QuoteChar, [Char | Chars], Token, !IO)
            )
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
        (
            rev_char_list_to_string(OctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        ->
            ( Int = 0 ->
                Token = null_character_error,
                string_get_context(Posn0, Context, !Posn)
            ;
                string_get_quoted_name(String, Len, QuoteChar, [Char | Chars],
                    Posn0, Token, Context, !Posn)
            )
        ;
            Token = error("invalid octal escape"),
            string_get_context(Posn0, Context, !Posn)
        )
    ).

%---------------------------------------------------------------------------%
%
% Names and variables.

:- pred get_name(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_name(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( rev_char_list_to_string(Chars, Name) ->
            Token = name(Name)
        ;
            Token = error("invalid character in name")
        )
    ;
        Result = ok,
        ( char.is_alnum_or_underscore(Char) ->
            get_name(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            ( rev_char_list_to_string(Chars, Name) ->
                Token = name(Name)
            ;
                Token = error("invalid character in name")
            )
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

:- pred get_implementation_defined_literal_rest(io.input_stream::in,
    token::out, io::di, io::uo) is det.

get_implementation_defined_literal_rest(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = name("$")
    ;
        Result = ok,
        ( char.is_lower(Char) ->
            get_name(Stream, [Char], Token0, !IO),
            ( Token0 = name(S) ->
                Token = implementation_defined(S)
            ;
                Token = Token0
            )
        ; graphic_token_char(Char) ->
            get_graphic(Stream, [Char, '$'], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Token = name("$")
        )
    ).

:- pred string_get_implementation_defined_literal_rest(string::in, int::in,
    posn::in, token::out, string_token_context::out, posn::in, posn::out)
    is det.

string_get_implementation_defined_literal_rest(String, Len, Posn0,
        Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_lower(Char) ->
            string_get_name(String, Len, Posn1, Token0, Context, !Posn),
            ( Token0 = name(S) ->
                Token = implementation_defined(S)
            ;
                Token = Token0
            )
        ; graphic_token_char(Char) ->
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = name("$"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = name("$"),
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
:- pred get_source_line_number(io.input_stream::in, list(char)::in, token::out,
    token_context::out, io::di, io::uo) is det.

get_source_line_number(Stream, Chars, Token, Context, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        get_context(Stream, Context, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        get_context(Stream, Context, !IO),
        Token = error("unexpected end-of-file in `#' line number directive")
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_source_line_number(Stream, [Char | Chars], Token, Context, !IO)
        ; Char = '\n' ->
            ( rev_char_list_to_string(Chars, String) ->
                (
                    string.base_string_to_int(10, String, Int),
                    Int > 0
                ->
                    io.set_line_number(Stream, Int, !IO),
                    get_token(Stream, Token, Context, !IO)
                ;
                    get_context(Stream, Context, !IO),
                    string.append_list(["invalid line number `", String,
                        "' in `#' line number directive"], Message),
                    Token = error(Message)
                )
            ;
                get_context(Stream, Context, !IO),
                Token = error("invalid character in `#' line number directive")
            )
        ;
            get_context(Stream, Context, !IO),
            ( char.to_int(Char, 0) ->
                String = "NUL"
            ;
                string.from_char_list([Char], String)
            ),
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
            ( char.to_int(Char, 0) ->
                DirectiveString = "NUL"
            ;
                string.from_char_list([Char], DirectiveString)
            ),
            string.append_list(["invalid character `", DirectiveString,
                "' in `#' line number directive"], Message),
            Token = error(Message)
        )
    ;
        string_get_context(Posn1, Context, !Posn),
        Token = error("unexpected end-of-file in `#' line number directive")
    ).

:- pred get_graphic(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_graphic(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( rev_char_list_to_string(Chars, Name) ->
            Token = name(Name)
        ;
            Token = error("invalid character in graphic token")
        )
    ;
        Result = ok,
        ( graphic_token_char(Char) ->
            get_graphic(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            ( rev_char_list_to_string(Chars, Name) ->
                Token = name(Name)
            ;
                Token = error("invalid character in graphic token")
            )
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

:- pred get_variable(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_variable(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( rev_char_list_to_string(Chars, VariableName) ->
            Token = variable(VariableName)
        ;
            Token = error("invalid character in variable")
        )
    ;
        Result = ok,
        ( char.is_alnum_or_underscore(Char) ->
            get_variable(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            ( rev_char_list_to_string(Chars, VariableName) ->
                Token = variable(VariableName)
            ;
                Token = error("invalid character in variable")
            )
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

%---------------------------------------------------------------------------%
%
% Integer and float literals.

:- pred get_zero(io.input_stream::in, token::out, io::di, io::uo) is det.

get_zero(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = integer(0)
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_number(Stream, [Char], Token, !IO)
        ; Char = '''' ->
            get_char_code(Stream, Token, !IO)
        ; Char = 'b' ->
            get_binary(Stream, Token, !IO)
        ; Char = 'o' ->
            get_octal(Stream, Token, !IO)
        ; Char = 'x' ->
            get_hex(Stream, Token, !IO)
        ; Char = ('.') ->
            get_int_dot(Stream, ['0'], Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent(Stream, [Char, '0'], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

:- pred get_char_code(io.input_stream::in, token::out, io::di, io::uo) is det.

get_char_code(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated char code constant")
    ;
        Result = ok,
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

:- pred get_binary(io.input_stream::in, token::out, io::di, io::uo) is det.

get_binary(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated binary constant")
    ;
        Result = ok,
        ( char.is_binary_digit(Char) ->
            get_binary_2(Stream, [Char], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated binary constant")
        )
    ).

:- pred string_get_binary(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_binary(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_binary_digit(Char) ->
            string_get_binary_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated binary constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated binary constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_binary_2(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_binary_2(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 2, Token)
    ;
        Result = ok,
        ( char.is_binary_digit(Char) ->
            get_binary_2(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            rev_char_list_to_int(Chars, 2, Token)
        )
    ).

:- pred string_get_binary_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_binary_2(String, Len, Posn1, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_binary_digit(Char) ->
            string_get_binary_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 2, Token),
            string_get_context(Posn1, Context, !Posn)
        )
    ;
        grab_string(String, Posn1, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 2, Token),
        string_get_context(Posn1, Context, !Posn)
    ).

:- pred get_octal(io.input_stream::in, token::out, io::di, io::uo) is det.

get_octal(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated octal constant")
    ;
        Result = ok,
        ( char.is_octal_digit(Char) ->
            get_octal_2(Stream, [Char], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated octal constant")
        )
    ).

:- pred string_get_octal(string::in, int::in, posn::in, token::out,
    string_token_context::out,
    posn::in, posn::out) is det.

string_get_octal(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_octal_digit(Char) ->
            string_get_octal_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated octal constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated octal constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_octal_2(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_octal_2(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 8, Token)
    ;
        Result = ok,
        ( char.is_octal_digit(Char) ->
            get_octal_2(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            rev_char_list_to_int(Chars, 8, Token)
        )
    ).

:- pred string_get_octal_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_octal_2(String, Len, Posn1, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_octal_digit(Char) ->
            string_get_octal_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 8, Token),
            string_get_context(Posn1, Context, !Posn)
        )
    ;
        grab_string(String, Posn1, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 8, Token),
        string_get_context(Posn1, Context, !Posn)
    ).

:- pred get_hex(io.input_stream::in, token::out, io::di, io::uo) is det.

get_hex(Stream, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated hex constant")
    ;
        Result = ok,
        ( char.is_hex_digit(Char) ->
            get_hex_2(Stream, [Char], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated hex constant")
        )
    ).

:- pred string_get_hex(string::in, int::in, posn::in, token::out,
    string_token_context::out,
    posn::in, posn::out) is det.

string_get_hex(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_hex_digit(Char) ->
            string_get_hex_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            Token = error("unterminated hex constant"),
            string_get_context(Posn0, Context, !Posn)
        )
    ;
        Token = error("unterminated hex constant"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_hex_2(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_hex_2(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 16, Token)
    ;
        Result = ok,
        ( char.is_hex_digit(Char) ->
            get_hex_2(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            rev_char_list_to_int(Chars, 16, Token)
        )
    ).

:- pred string_get_hex_2(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_hex_2(String, Len, Posn1, Token, Context, !Posn) :-
    ( string_read_char(String, Len, Char, !Posn) ->
        ( char.is_hex_digit(Char) ->
            string_get_hex_2(String, Len, Posn1, Token, Context, !Posn)
        ;
            string_ungetchar(String, !Posn),
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, 16, Token),
            string_get_context(Posn1, Context, !Posn)
        )
    ;
        grab_string(String, Posn1, BinaryString, !Posn),
        conv_string_to_int(BinaryString, 16, Token),
        string_get_context(Posn1, Context, !Posn)
    ).

:- pred get_number(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_number(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(Chars, 10, Token)
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_number(Stream, [Char | Chars], Token, !IO)
        ; Char = ('.') ->
            get_int_dot(Stream, Chars, Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

:- pred get_int_dot(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_int_dot(Stream, Chars, Token, !IO) :-
    % XXX The float literal syntax doesn't match ISO Prolog.
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        io.putback_char(Stream, '.', !IO),
        rev_char_list_to_int(Chars, 10, Token)
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_float_decimals(Stream, [Char, '.' | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

:- pred get_float_decimals(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

    % We've read past the decimal point, so now get the decimals.
    %
get_float_decimals(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_float_decimals(Stream, [Char | Chars], Token, !IO)
        ; ( Char = 'e' ; Char = 'E' ) ->
            get_float_exponent(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

:- pred get_float_exponent(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok,
        ( ( Char = ('+') ; Char = ('-') ) ->
            get_float_exponent_2(Stream, [Char | Chars], Token, !IO)
        ; char.is_digit(Char) ->
            get_float_exponent_3(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

    % We've read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
:- pred get_float_exponent_2(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent_2(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated exponent in float token")
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_float_exponent_3(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated exponent in float token")
        )
    ).

    % We've read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
:- pred string_get_float_exponent_2(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

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

    % We've read past the first digit of the exponent -
    % now get the remaining digits.
    %
:- pred get_float_exponent_3(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent_3(Stream, Chars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(Chars, Token)
    ;
        Result = ok,
        ( char.is_digit(Char) ->
            get_float_exponent_3(Stream, [Char | Chars], Token, !IO)
        ;
            io.putback_char(Stream, Char, !IO),
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

%---------------------------------------------------------------------------%
%
% Utility routines.

:- pred rev_char_list_to_int(list(char)::in, int::in, token::out) is det.

rev_char_list_to_int(RevChars, Base, Token) :-
    ( rev_char_list_to_string(RevChars, String) ->
        conv_string_to_int(String, Base, Token)
    ;
        Token = error("invalid character in int")
    ).

:- pred conv_string_to_int(string::in, int::in, token::out) is det.

conv_string_to_int(String, Base, Token) :-
    ( string.base_string_to_int(Base, String, Int) ->
        Token = integer(Int)
    ; Base = 10 ->
        Token = big_integer(String)
    ;
        Token = error("invalid integer token")
    ).

:- pred rev_char_list_to_float(list(char)::in, token::out) is det.

rev_char_list_to_float(RevChars, Token) :-
    ( rev_char_list_to_string(RevChars, String) ->
        conv_to_float(String, Token)
    ;
        Token = error("invalid character in int")
    ).

:- pred conv_to_float(string::in, token::out) is det.

conv_to_float(String, Token) :-
    ( string.to_float(String, Float) ->
        Token = float(Float)
    ;
        Token = error("invalid float token")
    ).

:- pred rev_char_list_to_string(list(char)::in, string::out) is semidet.

rev_char_list_to_string(RevChars, String) :-
   string.semidet_from_rev_char_list(RevChars, String).

:- func null_character_error = token.

null_character_error =
    error("null character is illegal in strings and names").

%---------------------------------------------------------------------------%
