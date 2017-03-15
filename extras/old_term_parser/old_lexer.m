%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2008, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: old_lexer.m.
% Main author: fjh.
% Stability: high.
%
% Lexical analysis. This module defines the representation of tokens
% and exports predicates for reading in tokens from an input stream.
%
% See ISO Prolog 6.4. Also see the comments at the top of parser.m.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module old_lexer.
:- interface.

:- import_module char.
:- import_module io.
:- import_module integer.

%---------------------------------------------------------------------------%

:- type token
    --->    name(string)
    ;       variable(string)
    ;       integer(int)

    ;       big_integer(integer_base, integer)
            % An integer that is too big for `int'.

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
    ;       io_error(io.error)  % error reading from the input stream
    ;       eof                 % end-of-file

    ;       integer_dot(int).
            % The lexer will never return integer_dot. This token is used
            % internally in the lexer, to keep the grammar LL(1) so that
            % only one character of pushback is needed. But the lexer will
            % convert integer_dot/1 tokens to integer/1 tokens before
            % returning them.

:- type integer_base
    --->    base_2
    ;       base_8
    ;       base_10
    ;       base_16.

    % For every token, we record the line number of the line on
    % which the token occurred.
    %
:- type token_context == int.   % line number

    % This "fat list" representation is more efficient than a list of pairs.
    %
:- type token_list
    --->    token_cons(token, token_context, token_list)
    ;       token_nil.

    % Read a list of tokens either from the current input stream
    % or from the specified input stream.
    % Keep reading until we encounter either an `end' token
    % (i.e. a full stop followed by whitespace) or the end-of-file.
    %
:- pred get_token_list(token_list::out, io::di, io::uo) is det.
:- pred get_token_list(io.text_input_stream::in, token_list::out,
    io::di, io::uo) is det.

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
% one that deals with strings, and another that deals with io.states.
% We can't write the io.state version in terms of the string version
% because we don't know how much string to slurp up until after we have
% lexically analysed it. Some interactive applications require the old
% Prolog behaviour of stopping after an end token (i.e. `.' plus whitespace)
% rather than reading in whole lines. Conversely, we can't write the string
% version using the io.state version, since that would require either
% cheating with the io.state or ruining the string interface.
%
% An alternative would be to write both versions in terms of a generic
% "char_stream" typeclass, with instances for io.states and for strings.
% However, for this to be acceptably efficient it would require the compiler
% to specialize the code, which currently (13 May 98) it is not capable
% of doing.
%
% In fact, the string version is still not as efficient as I would like.
% The compiler ought to (but currently doesn't) unfold all the instances
% of the `posn' type.  We could do this type unfolding by hand, but
% it would be very tedious and it would make the code less readable.
% If and when there is compiler support for this, we should also think about
% moving the `String' and `Len' arguments into the posn (or making a new
% `lexer_state' struct which contains both the posn and the String and Len
% arguments).

get_token_list(Tokens, !IO) :-
    io.input_stream(Stream, !IO),
    get_token_list(Stream, Tokens, !IO).

get_token_list(Stream, Tokens, !IO) :-
    % We build the tokens up as lists of characters in reverse order.
    % When we get to the end of each token, we call `rev_char_list_to_string/2'
    % to convert that representation into a string.
    %
    % Comments of the form
    %   foo --> bar . baz
    % mean that we are parsing a `foo', and we've already scanned past
    % the `bar', so now we need to match with a `baz'.
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
        ; Token0 = big_integer(_, _)
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
        ; Token = big_integer(_, _)
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

string_get_token_list(String, Tokens, !Posn) :-
    string.length(String, Len),
    string_get_token_list_max(String, Len, Tokens, !Posn).

%---------------------------------------------------------------------------%
%
% Some low-level routines.
%

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
    % In future, we might want to modify this code to read something like this:
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
    ( if Char = '\n' then
        LineNum = LineNum0 + 1,
        Posn = posn(LineNum, Offset, Offset)
    else
        Posn = posn(LineNum0, LineOffset0, Offset)
    ).

:- pred string_ungetchar(string::in, posn::in, posn::out) is det.

string_ungetchar(String, Posn0, Posn) :-
    Posn0 = posn(LineNum0, LineOffset0, Offset0),
    ( if string.unsafe_prev_index(String, Offset0, Offset, Char) then
        ( if Char = '\n' then
            LineNum = LineNum0 - 1,
            Posn = posn(LineNum, Offset, Offset)
        else
            Posn = posn(LineNum0, LineOffset0, Offset)
        )
    else
        Posn = Posn0
    ).

:- pred grab_string(string::in, posn::in, string::out,
    posn::in, posn::out) is det.

grab_string(String, Posn0, SubString, Posn, Posn) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    string.unsafe_between(String, Offset0, Offset, SubString).

    % As above, but the string is known to represent a float literal.
    % Filter out any underscore characters from the returned string.
    % We have to do this since the underlying mechanisms we currently use for
    % converting strings into floats (sscanf in C, parseDouble in Java etc)
    % cannot handle underscores in their input.
    %
:- pred grab_float_string(string::in, posn::in, string::out,
    posn::in, posn::out) is det.

grab_float_string(String, Posn0, FloatString, Posn, Posn) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    unsafe_get_float_between(String, Offset0, Offset, FloatString).

:- pred unsafe_get_float_between(string::in, int::in, int::in,
    string::uo) is det.
:- pragma foreign_proc("C",
    unsafe_get_float_between(Str::in, Start::in, End::in, FloatStr::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, no_sharing],
"
    int src;
    int dst = 0;

    MR_allocate_aligned_string_msg(FloatStr, End - Start, MR_ALLOC_ID);
    for (src = Start; src < End; src++) {
        if (Str[src] != '_') {
            FloatStr[dst] = Str[src];
            dst++;
        }
    }
    FloatStr[dst] = '\\0';
").

:- pragma foreign_proc("C#",
    unsafe_get_float_between(Str::in, Start::in, End::in, SubString::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SubString = Str.Substring(Start, End - Start).Replace(\"_\", \"\");
").

:- pragma foreign_proc("Java",
    unsafe_get_float_between(Str::in, Start::in, End::in, FloatStr::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FloatStr = Str.substring(Start, End).replace(\"_\", \"\");
").

    % For use by the Erlang backend.
    %
unsafe_get_float_between(Str, Start, End, FloatStr) :-
    string.unsafe_between(Str, Start, End, FloatStr0),
    ( if string.contains_char(FloatStr0, '_') then
        string.to_char_list(FloatStr0, Digits0),
        list.negated_filter(is_underscore, Digits0, Digits),
        string.from_char_list(Digits, FloatStr)
    else
        FloatStr = FloatStr0
    ).

:- pred is_underscore(char::in) is semidet.

is_underscore('_').

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
    get_token_2(Stream, not_scanned_past_whitespace, Token, Context, !IO).

    % If passed `scanned_past_whitespace' then we have already scanned past
    % some whitespace, so '(' gets scanned as `open' rather than `open_ct'.
    %
    % `get_token_2' must be inlined into `execute_get_token_action' so that
    % the recursive call can be compiled to a loop on backends that cannot
    % eliminate tail calls in general.
    %
:- pragma inline(get_token_2/6).
:- pred get_token_2(io.input_stream::in, scanned_past_whitespace::in,
    token::out, token_context::out, io::di, io::uo) is det.

get_token_2(Stream, ScannedPastWhiteSpace, Token, Context, !IO) :-
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
        ( if lookup_token_action(Char, Action) then
            execute_get_token_action(Stream, Char, Action,
                ScannedPastWhiteSpace, Token, Context, !IO)
        else
            get_context(Stream, Context, !IO),
            Token = junk(Char)
        )
    ).

:- pred string_get_token(string::in, int::in, token::out,
    token_context::out, posn::in, posn::out) is det.

string_get_token(String, Len, Token, Context, !Posn) :-
    string_get_token_2(String, Len, not_scanned_past_whitespace,
        Token, Context, !Posn).

:- pragma inline(string_get_token_2/7). % see get_token_2
:- pred string_get_token_2(string::in, int::in, scanned_past_whitespace::in,
    token::out, token_context::out, posn::in, posn::out) is det.

string_get_token_2(String, Len, ScannedPastWhiteSpace, Token, Context, !Posn)
        :-
    Posn0 = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if lookup_token_action(Char, Action) then
            execute_string_get_token_action(String, Len, Posn0, Char, Action,
                ScannedPastWhiteSpace, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context, !Posn),
            Token = junk(Char)
        )
    else
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

    % Decide on how the given character should be treated. Note that
    % performance suffers significantly if this predicate is not inlined.
    %
:- pred lookup_token_action(char::in, get_token_action::out) is semidet.
:- pragma inline(lookup_token_action/2).

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

    % Some descendant predicates of `execute_get_token_action' have the job of
    % consuming input that does not correspond to a token, e.g. skip_to_eol
    % skips to the end of line and does not produce a token unless it
    % encounters the end-of-file or an I/O error.
    %
    % If a descendant predicate does not produce a token, then it must return
    % an indication back to `execute_get_token_action' that it did not, then
    % `execute_get_token_action' will call itself recursively to get the next
    % token.
    %
    % An alternative would be for the descendant predicate which has not
    % produced a token to call `execute_get_token_action' (indirectly) to get
    % the next token. However, `execute_get_token_action' calling itself is
    % preferable as the direct recursion can be compiled to a loop by backends
    % that cannot otherwise eliminate tail calls.
    %
    % We would like to define a type to represent token values being produced
    % or not:
    %
    %   :- type maybe_token
    %       --->    yes(token, token_context)
    %       ;       no.
    %
    % but the heap allocation required to return "yes(Token, Context)" would be
    % a significant overhead. Instead, each predicate that might not produce a
    % token returns two values, of type `token' and `maybe_have_valid_token'
    % (below).
    %
    % If the predicate does produce a token then it returns the token and the
    % context. This corresponds to the "yes(Token, Context)" case.
    %
    % If the predicate does not produce a token then it returns a dummy token
    % value (that must be ignored) and an invalid context, i.e. one for which
    % have_token_with_context fails. This corresponds to the "no" case.
    %
:- type maybe_have_valid_token
    --->    maybe_have_valid_token(token_context).

:- pred have_token(io.input_stream::in, maybe_have_valid_token::out,
    io::di, io::uo) is det.

have_token(Stream, maybe_have_valid_token(Context), !IO) :-
    get_context(Stream, Context, !IO).

:- pred string_have_token(posn::in, maybe_have_valid_token::out,
    posn::in, posn::out) is det.

string_have_token(Posn0, maybe_have_valid_token(Context), !Posn) :-
    string_get_context(Posn0, Context, !Posn).

:- pred do_not_have_token(token::out, maybe_have_valid_token::out) is det.

do_not_have_token(Token, HaveToken) :-
    Token = eof, % dummy
    HaveToken = maybe_have_valid_token(-1). % invalid context

:- pred have_token_with_context(maybe_have_valid_token::in, token_context::out)
    is semidet.

have_token_with_context(maybe_have_valid_token(Context), Context) :-
    Context \= -1.

%---------------------------------------------------------------------------%

    % Handle the character we just read the way lookup_token_action decided
    % it should be treated. Note that inlining this predicate does not
    % significantly affect performance.
    %
:- pred execute_get_token_action(io.input_stream::in, char::in,
    get_token_action::in, scanned_past_whitespace::in, token::out,
    token_context::out, io::di, io::uo) is det.
% :- pragma inline(execute_get_token_action/8).

execute_get_token_action(Stream, Char, Action, ScannedPastWhiteSpace,
        Token, Context, !IO) :-
    (
        Action = action_whitespace,
        get_token_2(Stream, scanned_past_whitespace, Token, Context, !IO)
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
        get_number(Stream, last_digit_is_not_underscore, [Char], Token, !IO)
    ;
        Action = action_special_token,
        get_context(Stream, Context, !IO),
        handle_special_token(Char, ScannedPastWhiteSpace, Token)
    ;
        Action = action_dot,
        get_context(Stream, Context, !IO),
        get_dot(Stream, Token, !IO)
    ;
        Action = action_quote,
        get_context(Stream, Context, !IO),
        start_quoted_name(Stream, Char, [], Token, !IO)
    ;
        (
            Action = action_percent,
            skip_to_eol(Stream, Token0, HaveToken0, !IO)
        ;
            Action = action_slash,
            get_slash(Stream, Token0, HaveToken0, !IO)
        ),
        ( if have_token_with_context(HaveToken0, Context0) then
            Token = Token0,
            Context = Context0
        else
            get_token_2(Stream, scanned_past_whitespace, Token, Context, !IO)
        )
    ;
        Action = action_hash,
        get_source_line_number(Stream, [], Token0, HaveToken0, !IO),
        ( if have_token_with_context(HaveToken0, Context0) then
            Token = Token0,
            Context = Context0
        else
            get_token_2(Stream, not_scanned_past_whitespace, Token, Context,
                !IO)
        )
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
:- pred execute_string_get_token_action(string::in, int::in, posn::in,
    char::in, get_token_action::in, scanned_past_whitespace::in, token::out,
    token_context::out, posn::in, posn::out) is det.
% :- pragma inline(execute_string_get_token_action/10).

execute_string_get_token_action(String, Len, Posn0, Char, Action,
        ScannedPastWhiteSpace, Token, Context, !Posn) :-
    (
        Action = action_whitespace,
        string_get_token_2(String, Len, scanned_past_whitespace,
            Token, Context, !Posn)
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
        LastDigit = last_digit_is_not_underscore,
        string_get_number(String, LastDigit, Len, Posn0, Token, Context,
            !Posn)
    ;
        Action = action_special_token,
        string_get_context(Posn0, Context, !Posn),
        handle_special_token(Char, ScannedPastWhiteSpace, Token)
    ;
        Action = action_dot,
        string_get_dot(String, Len, Posn0, Token, Context, !Posn)
    ;
        Action = action_quote,
        string_start_quoted_name(String, Len, Char, [], Posn0, Token,
            Context, !Posn)
    ;
        (
            Action = action_percent,
            string_skip_to_eol(String, Len, Token0, HaveToken0, !Posn)
        ;
            Action = action_slash,
            string_get_slash(String, Len, Posn0, Token0, HaveToken0, !Posn)
        ),
        ( if have_token_with_context(HaveToken0, Context0) then
            Token = Token0,
            Context = Context0
        else
            string_get_token_2(String, Len, scanned_past_whitespace,
                Token, Context, !Posn)
        )
    ;
        Action = action_hash,
        string_get_source_line_number(String, Len, !.Posn, Token0, HaveToken0,
            !Posn),
        ( if have_token_with_context(HaveToken0, Context0) then
            Token = Token0,
            Context = Context0
        else
            string_get_token_2(String, Len, not_scanned_past_whitespace,
                Token, Context, !Posn)
        )
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
:- pred handle_special_token(char::in, scanned_past_whitespace::in, token::out)
    is det.
:- pragma inline(handle_special_token/3).

handle_special_token(Char, ScannedPastWhiteSpace, Token) :-
    ( if special_token(Char, SpecialToken) then
        (
            ScannedPastWhiteSpace = not_scanned_past_whitespace,
            ( if SpecialToken = open then
                Token = open_ct
            else
                Token = SpecialToken
            )
        ;
            ScannedPastWhiteSpace = scanned_past_whitespace,
            Token = SpecialToken
        )
    else
        error("old_lexer.m: handle_special_token: unknown special token")
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
        ( if whitespace_after_dot(Char) then
            io.putback_char(Stream, Char, !IO),
            Token = end
        else if graphic_token_char(Char) then
            get_graphic(Stream, [Char, '.'], Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = name(".")
        )
    ).

:- pred string_get_dot(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_dot(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if whitespace_after_dot(Char) then
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = end
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = name(".")
        )
    else
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
%

:- pred skip_to_eol(io.input_stream::in, token::out,
    maybe_have_valid_token::out, io::di, io::uo) is det.

skip_to_eol(Stream, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        have_token(Stream, HaveToken, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        have_token(Stream, HaveToken, !IO),
        Token = eof
    ;
        Result = ok,
        ( if Char = '\n' then
            do_not_have_token(Token, HaveToken)
        else
            skip_to_eol(Stream, Token, HaveToken, !IO)
        )
    ).

:- pred string_skip_to_eol(string::in, int::in, token::out,
    maybe_have_valid_token::out, posn::in, posn::out) is det.

string_skip_to_eol(String, Len, Token, HaveToken, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = '\n' then
            do_not_have_token(Token, HaveToken)
        else
            string_skip_to_eol(String, Len, Token, HaveToken, !Posn)
        )
    else
        string_have_token(!.Posn, HaveToken, !Posn),
        Token = eof
    ).

:- pred get_slash(io.input_stream::in, token::out, maybe_have_valid_token::out,
    io::di, io::uo) is det.

get_slash(Stream, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        have_token(Stream, HaveToken, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        have_token(Stream, HaveToken, !IO),
        Token = name("/")
    ;
        Result = ok,
        ( if Char = ('*') then
            get_comment(Stream, Token, HaveToken, !IO)
        else if graphic_token_char(Char) then
            get_graphic(Stream, [Char, '/'], Token, !IO),
            have_token(Stream, HaveToken, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            have_token(Stream, HaveToken, !IO),
            Token = name("/")
        )
    ).

:- pred string_get_slash(string::in, int::in, posn::in, token::out,
    maybe_have_valid_token::out, posn::in, posn::out) is det.

string_get_slash(String, Len, Posn0, Token, HaveToken, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = ('*') then
            string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn),
            HaveToken = maybe_have_valid_token(Context)
        else
            string_ungetchar(String, !Posn),
            string_have_token(Posn0, HaveToken, !Posn),
            Token = name("/")
        )
    else
        string_have_token(Posn0, HaveToken, !Posn),
        Token = name("/")
    ).

:- pred get_comment(io.input_stream::in, token::out,
    maybe_have_valid_token::out, io::di, io::uo) is det.

get_comment(Stream, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        have_token(Stream, HaveToken, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        have_token(Stream, HaveToken, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok,
        ( if Char = ('*') then
            get_comment_2(Stream, Token, HaveToken, !IO)
        else
            get_comment(Stream, Token, HaveToken, !IO)
        )
    ).

:- pred string_get_comment(string::in, int::in, posn::in, token::out,
    maybe_have_valid_token::out, posn::in, posn::out) is det.

string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = ('*') then
            string_get_comment_2(String, Len, Posn0, Token, HaveToken, !Posn)
        else
            string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
        )
    else
        string_have_token(Posn0, HaveToken, !Posn),
        Token = error("unterminated '/*' comment")
    ).

:- pred get_comment_2(io.input_stream::in, token::out,
    maybe_have_valid_token::out, io::di, io::uo) is det.

get_comment_2(Stream, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        have_token(Stream, HaveToken, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        have_token(Stream, HaveToken, !IO),
        Token = error("unterminated '/*' comment")
    ;
        Result = ok,
        ( if Char = ('/') then
            % end of /* ... */ comment, so get next token
            do_not_have_token(Token, HaveToken)
        else if Char = ('*') then
            get_comment_2(Stream, Token, HaveToken, !IO)
        else
            get_comment(Stream, Token, HaveToken, !IO)
        )
    ).

:- pred string_get_comment_2(string::in, int::in, posn::in, token::out,
    maybe_have_valid_token::out, posn::in, posn::out) is det.

string_get_comment_2(String, Len, Posn0, Token, HaveToken, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = ('/') then
            % end of /* ... */ comment, so get next token
            do_not_have_token(Token, HaveToken)
        else if Char = ('*') then
            string_get_comment_2(String, Len, Posn0, Token, HaveToken, !Posn)
        else
            string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
        )
    else
        string_have_token(Posn0, HaveToken, !Posn),
        Token = error("unterminated '/*' comment")
    ).

%---------------------------------------------------------------------------%
%
% Quoted names and quoted strings.
%

:- pred start_quoted_name(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

start_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO) :-
    get_quoted_name(Stream, QuoteChar, !.RevChars, Token0, !IO),
    ( if Token0 = error(_) then
        % Skip to the end of the string or name.
        start_quoted_name(Stream, QuoteChar, !.RevChars, _, !IO),
        Token = Token0
    else if Token0 = eof then
        Token = error("unterminated quote")
    else
        Token = Token0
    ).

:- pred string_start_quoted_name(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_start_quoted_name(String, Len, QuoteChar, !.RevChars, Posn0,
        Token, Context, !Posn) :-
    string_get_quoted_name(String, Len, QuoteChar, !.RevChars, Posn0,
        Token0, Context, !Posn),
    ( if Token0 = error(_) then
        % Skip to the end of the string or name.
        string_start_quoted_name(String, Len, QuoteChar, !.RevChars,
            Posn0, _, _, !Posn),
        Token = Token0
    else if Token0 = eof then
        Token = error("unterminated quote")
    else
        Token = Token0
    ).

:- pred get_quoted_name(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( if Char = QuoteChar then
            get_quoted_name_quote(Stream, QuoteChar, !.RevChars, Token, !IO)
        else if Char = ('\\') then
            get_quoted_name_escape(Stream, QuoteChar, !.RevChars, Token, !IO)
        else
            !:RevChars = [Char | !.RevChars],
            get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
        )
    ).

:- pred string_get_quoted_name(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
        Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = QuoteChar then
            string_get_quoted_name_quote(String, Len, QuoteChar, !.RevChars,
                Posn0, Token, Context, !Posn)
        else if Char = ('\\') then
            string_get_quoted_name_escape(String, Len, QuoteChar, !.RevChars,
                Posn0, Token, Context, !Posn)
        else
            !:RevChars = [Char | !.RevChars],
            string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                Posn0, Token, Context, !Posn)
        )
    else
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

:- pred get_quoted_name_quote(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name_quote(Stream, QuoteChar, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        finish_quoted_name(QuoteChar, !.RevChars, Token)
    ;
        Result = ok,
        ( if Char = QuoteChar then
            !:RevChars = [Char | !.RevChars],
            get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            finish_quoted_name(QuoteChar, !.RevChars, Token)
        )
    ).

:- pred string_get_quoted_name_quote(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name_quote(String, Len, QuoteChar, !.RevChars,
        Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = QuoteChar then
            !:RevChars = [Char | !.RevChars],
            string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            finish_quoted_name(QuoteChar, !.RevChars, Token)
        )
    else
        string_get_context(Posn0, Context, !Posn),
        finish_quoted_name(QuoteChar, !.RevChars, Token)
    ).

:- pred finish_quoted_name(char::in, list(char)::in, token::out) is det.

finish_quoted_name(QuoteChar, RevChars, Token) :-
    ( if rev_char_list_to_string(RevChars, String) then
        ( if QuoteChar = '''' then
            Token = name(String)
        else if QuoteChar = '"' then
            Token = string(String)
        else
            error("old_lexer.m: unknown quote character")
        )
    else
        Token = error("invalid character in quoted name")
    ).

:- pred get_quoted_name_escape(io.input_stream::in, char::in, list(char)::in,
    token::out, io::di, io::uo) is det.

get_quoted_name_escape(Stream, QuoteChar, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( if Char = '\n' then
            get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
        else if Char = '\r' then
            % Files created on Windows may have an extra return character.
            get_quoted_name_escape(Stream, QuoteChar, !.RevChars, Token, !IO)
        else if escape_char(Char, EscapedChar) then
            !:RevChars = [EscapedChar | !.RevChars],
            get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
        else if Char = 'x' then
            get_hex_escape(Stream, QuoteChar, !.RevChars, [], Token, !IO)
        else if Char = 'u' then
            get_unicode_escape(Stream, 4, QuoteChar, !.RevChars, [],
                Token, !IO)
        else if Char = 'U' then
            get_unicode_escape(Stream, 8, QuoteChar, !.RevChars, [],
                Token, !IO)
        else if char.is_octal_digit(Char) then
            get_octal_escape(Stream, QuoteChar, !.RevChars, [Char], Token, !IO)
        else
            Token = error("invalid escape character")
        )
    ).

:- pred string_get_quoted_name_escape(string::in, int::in, char::in,
    list(char)::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_quoted_name_escape(String, Len, QuoteChar, !.RevChars, Posn0,
        Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = '\n' then
            string_get_quoted_name(String, Len, QuoteChar,
                !.RevChars, Posn0, Token, Context, !Posn)
        else if Char = '\r' then
            % Files created on Windows may have an extra return character.
            string_get_quoted_name_escape(String, Len, QuoteChar,
                !.RevChars, Posn0, Token, Context, !Posn)
        else if escape_char(Char, EscapedChar) then
            !:RevChars = [EscapedChar | !.RevChars],
            string_get_quoted_name(String, Len, QuoteChar,
                !.RevChars, Posn0, Token, Context, !Posn)
        else if Char = 'x' then
            string_get_hex_escape(String, Len, QuoteChar,
                !.RevChars, [], Posn0, Token, Context, !Posn)
        else if Char = 'u' then
            string_get_unicode_escape(4, String, Len, QuoteChar,
                !.RevChars, [], Posn0, Token, Context, !Posn)
        else if Char = 'U' then
            string_get_unicode_escape(8, String, Len, QuoteChar,
                !.RevChars, [], Posn0, Token, Context, !Posn)
        else if char.is_octal_digit(Char) then
            string_get_octal_escape(String, Len, QuoteChar,
                !.RevChars, [Char], Posn0, Token, Context, !Posn)
        else
            string_get_context(!.Posn, Context, !Posn),
            Token = error("invalid escape character")
        )
    else
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

get_unicode_escape(Stream, NumHexChars, QuoteChar, !.RevChars, !.RevHexChars,
        Token, !IO) :-
    ( if NumHexChars = list.length(!.RevHexChars) then
        ( if
            rev_char_list_to_string(!.RevHexChars, HexString),
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            allowed_unicode_char_code(UnicodeCharCode),
            char.from_int(UnicodeCharCode, UnicodeChar)
        then
            ( if UnicodeCharCode = 0 then
                Token = null_character_error
            else
                !:RevChars = [UnicodeChar | !.RevChars],
                get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
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
                !:RevHexChars = [Char | !.RevHexChars],
                get_unicode_escape(Stream, NumHexChars, QuoteChar,
                    !.RevChars, !.RevHexChars, Token, !IO)
            else
                Token = error("invalid hex character in Unicode escape")
            )
        )
    ).

:- pred string_get_unicode_escape(int::in, string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_unicode_escape(NumHexChars, String, Len, QuoteChar,
        !.RevChars, !.RevHexChars, Posn0, Token, Context, !Posn) :-
    ( if NumHexChars = list.length(!.RevHexChars) then
        ( if
            rev_char_list_to_string(!.RevHexChars, HexString),
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            allowed_unicode_char_code(UnicodeCharCode),
            char.from_int(UnicodeCharCode, UnicodeChar)
        then
            ( if UnicodeCharCode = 0 then
                string_get_context(Posn0, Context, !Posn),
                Token = null_character_error
            else
                !:RevChars = [UnicodeChar | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("invalid Unicode character code")
        )
    else
        ( if string_read_char(String, Len, Char, !Posn) then
            ( if char.is_hex_digit(Char) then
                !:RevHexChars = [Char | !.RevHexChars],
                string_get_unicode_escape(NumHexChars, String, Len, QuoteChar,
                    !.RevChars, !.RevHexChars, Posn0, Token, Context, !Posn)
            else
                string_get_context(Posn0, Context, !Posn),
                Token = error("invalid hex character in Unicode escape")
            )
        else
            string_get_context(Posn0, Context, !Posn),
            Token = eof
        )
    ).

    % Succeeds if the give code point is a legal Unicode code point
    % (regardless of whether it is reserved for private use or not).
    %
:- pred allowed_unicode_char_code(int::in) is semidet.

allowed_unicode_char_code(Code) :-
    Code >= 0,
    Code =< 0x10FFFF,
    % The following range is reserved for surrogates.
    not (
        Code >= 0xD800, Code =< 0xDFFF
    ).

:- pred get_hex_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_hex_escape(Stream, QuoteChar, !.RevChars, !.RevHexChars, Token, !IO) :-
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
            !:RevHexChars = [Char | !.RevHexChars],
            get_hex_escape(Stream, QuoteChar, !.RevChars, !.RevHexChars,
                Token, !IO)
        else if Char = ('\\') then
            finish_hex_escape(Stream, QuoteChar, !.RevChars, !.RevHexChars,
                Token, !IO)
        else
            Token = error("unterminated hex escape")
        )
    ).

:- pred string_get_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_hex_escape(String, Len, QuoteChar, !.RevChars, !.RevHexChars,
        Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_hex_digit(Char) then
            !:RevHexChars = [Char | !.RevHexChars],
            string_get_hex_escape(String, Len, QuoteChar,
                !.RevChars, !.RevHexChars, Posn0, Token, Context, !Posn)
        else if Char = ('\\') then
            string_finish_hex_escape(String, Len, QuoteChar, !.RevChars,
                !.RevHexChars, Posn0, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("unterminated hex escape")
        )
    else
        string_get_context(Posn0, Context, !Posn),
        Token = eof
    ).

:- pred finish_hex_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

finish_hex_escape(Stream, QuoteChar, !.RevChars, !.RevHexChars, Token, !IO) :-
    (
        !.RevHexChars = [],
        Token = error("empty hex escape")
    ;
        !.RevHexChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevHexChars, HexString),
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error
            else
                !:RevChars = [Char | !.RevChars],
                get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
            )
        else
            Token = error("invalid hex escape")
        )
    ).

:- pred string_finish_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_finish_hex_escape(String, Len, QuoteChar, !.RevChars, !.RevHexChars,
        Posn0, Token, Context, !Posn) :-
    (
        !.RevHexChars = [],
        string_get_context(Posn0, Context, !Posn),
        Token = error("empty hex escape")
    ;
        !.RevHexChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevHexChars, HexString),
            string.base_string_to_int(16, HexString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error,
                string_get_context(Posn0, Context, !Posn)
            else
                !:RevChars = [Char | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("invalid hex escape")
        )
    ).

:- pred get_octal_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_octal_escape(Stream, QuoteChar, !.RevChars, !.RevOctalChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = eof
    ;
        Result = ok,
        ( if char.is_octal_digit(Char) then
            !:RevOctalChars = [Char | !.RevOctalChars],
            get_octal_escape(Stream, QuoteChar, !.RevChars, !.RevOctalChars,
                Token, !IO)
        else if Char = ('\\') then
            finish_octal_escape(Stream, QuoteChar, !.RevChars, !.RevOctalChars,
                Token, !IO)
        else
            Token = error("unterminated octal escape")
        )
    ).

:- pred string_get_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_octal_escape(String, Len, QuoteChar, !.RevChars, !.RevOctalChars,
        Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_octal_digit(Char) then
            !:RevOctalChars = [Char | !.RevOctalChars],
            string_get_octal_escape(String, Len, QuoteChar,
                !.RevChars, !.RevOctalChars, Posn0, Token, Context, !Posn)
        else if Char = ('\\') then
            string_finish_octal_escape(String, Len, QuoteChar,
                !.RevChars, !.RevOctalChars, Posn0, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context, !Posn),
            Token = error("unterminated octal escape")
        )
    else
        Token = eof,
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred finish_octal_escape(io.input_stream::in, char::in, list(char)::in,
    list(char)::in, token::out, io::di, io::uo) is det.

finish_octal_escape(Stream, QuoteChar, !.RevChars, !.RevOctalChars,
        Token, !IO) :-
    (
        !.RevOctalChars = [],
        Token = error("empty octal escape")
    ;
        !.RevOctalChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevOctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error
            else
                !:RevChars = [Char | !.RevChars],
                get_quoted_name(Stream, QuoteChar, !.RevChars, Token, !IO)
            )
        else
            Token = error("invalid octal escape")
        )
    ).

:- pred string_finish_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_finish_octal_escape(String, Len, QuoteChar, !.RevChars, !.RevOctalChars,
        Posn0, Token, Context, !Posn) :-
    (
        !.RevOctalChars = [],
        Token = error("empty octal escape"),
        string_get_context(Posn0, Context, !Posn)
    ;
        !.RevOctalChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevOctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error,
                string_get_context(Posn0, Context, !Posn)
            else
                !:RevChars = [Char | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            Token = error("invalid octal escape"),
            string_get_context(Posn0, Context, !Posn)
        )
    ).

%---------------------------------------------------------------------------%
%
% Names and variables.
%

:- pred get_name(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_name(Stream, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( if rev_char_list_to_string(!.RevChars, Name) then
            Token = name(Name)
        else
            Token = error("invalid character in name")
        )
    ;
        Result = ok,
        ( if char.is_alnum_or_underscore(Char) then
            !:RevChars = [Char | !.RevChars],
            get_name(Stream, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            ( if rev_char_list_to_string(!.RevChars, Name) then
                Token = name(Name)
            else
                Token = error("invalid character in name")
            )
        )
    ).

:- pred string_get_name(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_name(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_alnum_or_underscore(Char) then
            string_get_name(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, Name, !Posn),
            Token = name(Name),
            string_get_context(Posn0, Context, !Posn)
        )
    else
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
        ( if char.is_lower(Char) then
            get_name(Stream, [Char], Token0, !IO),
            ( if Token0 = name(S) then
                Token = implementation_defined(S)
            else
                Token = Token0
            )
        else if graphic_token_char(Char) then
            get_graphic(Stream, [Char, '$'], Token, !IO)
        else
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
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_lower(Char) then
            string_get_name(String, Len, Posn1, Token0, Context, !Posn),
            ( if Token0 = name(S) then
                Token = implementation_defined(S)
            else
                Token = Token0
            )
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = name("$"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        Token = name("$"),
        string_get_context(Posn0, Context, !Posn)
    ).

    % A line number directive token is `#' followed by an integer
    % (specifying the line number) followed by a newline.
    % Such a token sets the source line number for the next line, but it is
    % otherwise ignored. This means that line number directives may appear
    % anywhere that a token may appear, including in the middle of terms.
    % (The source file name can be set with a `:- pragma source_file'
    % declaration.)
    %
:- pred get_source_line_number(io.input_stream::in, list(char)::in, token::out,
    maybe_have_valid_token::out, io::di, io::uo) is det.

get_source_line_number(Stream, !.RevChars, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        have_token(Stream, HaveToken, !IO),
        Token = io_error(Error)
    ;
        Result = eof,
        have_token(Stream, HaveToken, !IO),
        Token = error("unexpected end-of-file in `#' line number directive")
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            get_source_line_number(Stream, !.RevChars, Token, HaveToken, !IO)
        else if Char = '\n' then
            ( if rev_char_list_to_string(!.RevChars, String) then
                ( if
                    string.base_string_to_int(10, String, Int),
                    Int > 0
                then
                    io.set_line_number(Stream, Int, !IO),
                    do_not_have_token(Token, HaveToken)
                else
                    have_token(Stream, HaveToken, !IO),
                    string.append_list(["invalid line number `", String,
                        "' in `#' line number directive"], Message),
                    Token = error(Message)
                )
            else
                have_token(Stream, HaveToken, !IO),
                Token = error("invalid character in `#' line number directive")
            )
        else
            have_token(Stream, HaveToken, !IO),
            ( if char.to_int(Char, 0) then
                String = "NUL"
            else
                string.from_char_list([Char], String)
            ),
            string.append_list(["invalid character `", String,
                "' in `#' line number directive"], Message),
            Token = error(Message)
        )
    ).

:- pred string_get_source_line_number(string::in, int::in, posn::in,
    token::out, maybe_have_valid_token::out, posn::in, posn::out) is det.

string_get_source_line_number(String, Len, Posn1, Token, HaveToken, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            string_get_source_line_number(String, Len, Posn1, Token, HaveToken,
                !Posn)
        else if Char = '\n' then
            grab_string(String, Posn1, LineNumString, !Posn),
            ( if
                string.base_string_to_int(10, LineNumString, LineNum),
                LineNum > 0
            then
                string_set_line_number(LineNum, !Posn),
                do_not_have_token(Token, HaveToken)
            else
                string_have_token(Posn1, HaveToken, !Posn),
                string.append_list(["invalid line number `", LineNumString,
                    "' in `#' line number directive"], Message),
                Token = error(Message)
            )
        else
            string_have_token(Posn1, HaveToken, !Posn),
            ( if char.to_int(Char, 0) then
                DirectiveString = "NUL"
            else
                string.from_char_list([Char], DirectiveString)
            ),
            string.append_list(["invalid character `", DirectiveString,
                "' in `#' line number directive"], Message),
            Token = error(Message)
        )
    else
        string_have_token(Posn1, HaveToken, !Posn),
        Token = error("unexpected end-of-file in `#' line number directive")
    ).

:- pred get_graphic(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_graphic(Stream, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( if rev_char_list_to_string(!.RevChars, Name) then
            Token = name(Name)
        else
            Token = error("invalid character in graphic token")
        )
    ;
        Result = ok,
        ( if graphic_token_char(Char) then
            !:RevChars = [Char | !.RevChars],
            get_graphic(Stream, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            ( if rev_char_list_to_string(!.RevChars, Name) then
                Token = name(Name)
            else
                Token = error("invalid character in graphic token")
            )
        )
    ).

:- pred string_get_graphic(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_graphic(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, Name, !Posn),
            Token = name(Name),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        grab_string(String, Posn0, Name, !Posn),
        string_get_context(Posn0, Context, !Posn),
        Token = name(Name)
    ).

:- pred get_variable(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_variable(Stream, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        ( if rev_char_list_to_string(!.RevChars, VariableName) then
            Token = variable(VariableName)
        else
            Token = error("invalid character in variable")
        )
    ;
        Result = ok,
        ( if char.is_alnum_or_underscore(Char) then
            !:RevChars = [Char | !.RevChars],
            get_variable(Stream, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            ( if rev_char_list_to_string(!.RevChars, VariableName) then
                Token = variable(VariableName)
            else
                Token = error("invalid character in variable")
            )
        )
    ).

:- pred string_get_variable(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_variable(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_alnum_or_underscore(Char) then
            string_get_variable(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            grab_string(String, Posn0, VariableName, !Posn),
            Token = variable(VariableName),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        grab_string(String, Posn0, VariableName, !Posn),
        Token = variable(VariableName),
        string_get_context(Posn0, Context, !Posn)
    ).

%---------------------------------------------------------------------------%
%
% Integer and float literals.
%

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
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            get_number(Stream, LastDigit, [Char], Token, !IO)
        else if Char = '_' then
            LastDigit = last_digit_is_underscore,
            get_number(Stream, LastDigit, [], Token, !IO)
        else if Char = '''' then
            get_char_code(Stream, Token, !IO)
        else if Char = 'b' then
            get_binary(Stream, Token, !IO)
        else if Char = 'o' then
            get_octal(Stream, Token, !IO)
        else if Char = 'x' then
            get_hex(Stream, Token, !IO)
        else if Char = ('.') then
            LastDigit = last_digit_is_not_underscore,
            get_int_dot(Stream, LastDigit, ['0'], Token, !IO)
        else if ( Char = 'e' ; Char = 'E' ) then
            get_float_exponent(Stream, [Char, '0'], Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = integer(0)
        )
    ).

    % This type records whether the last "digit" seen by the
    % lexer as it process a numeric token was an underscore or not.
    % This is needed to detect invalid uses of underscores in numeric
    % literals.
    % Note that there may be other intervening characters in the
    % token between the last digit and the current one (e.g. the
    % decimal point or beginning of an exponent a float literal.)
    %
:- type last_digit_is_underscore
    --->    last_digit_is_underscore
    ;       last_digit_is_not_underscore.

:- pred string_get_zero(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_zero(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_number(String, LastDigit, Len, Posn0, Token, Context,
                !Posn)
        else if Char = '_' then
            LastDigit = last_digit_is_underscore,
            string_get_number(String, LastDigit, Len, Posn0, Token, Context,
                !Posn)
        else if Char = '''' then
            string_get_char_code(String, Len, Posn0, Token, Context, !Posn)
        else if Char = 'b' then
            string_get_binary(String, Len, Posn0, Token, Context, !Posn)
        else if Char = 'o' then
            string_get_octal(String, Len, Posn0, Token, Context, !Posn)
        else if Char = 'x' then
            string_get_hex(String, Len, Posn0, Token, Context, !Posn)
        else if Char = ('.') then
            LastDigit = last_digit_is_not_underscore,
            string_get_int_dot(String, LastDigit, Len, Posn0, Token, Context,
                !Posn)
        else if ( Char = 'e' ; Char = 'E' ) then
            string_get_float_exponent(String, Len, Posn0, Token, Context,
                !Posn)
        else
            string_ungetchar(String, !Posn),
            string_get_context(Posn0, Context, !Posn),
            Token = integer(0)
        )
    else
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
        Token = error("unterminated char code literal")
    ;
        Result = ok,
        char.to_int(Char, CharCode),
        Token = integer(CharCode)
    ).

:- pred string_get_char_code(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_char_code(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        char.to_int(Char, CharCode),
        Token = integer(CharCode),
        string_get_context(Posn0, Context, !Posn)
    else
        Token = error("unterminated char code literal"),
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
        Token = error("unterminated binary literal")
    ;
        Result = ok,
        ( if char.is_binary_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            get_binary_2(Stream, LastDigit, [Char], Token, !IO)
        else if Char = '_' then
            get_binary(Stream, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated binary literal")
        )
    ).

:- pred string_get_binary(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_binary(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_binary_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_binary_2(String, LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else if Char = '_' then
            string_get_binary(String, Len, Posn1, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = error("unterminated binary literal"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        Token = error("unterminated binary literal"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_binary_2(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_binary_2(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_int(!.RevChars, base_2, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated binary literal")
        )
    ;
        Result = ok,
        ( if char.is_binary_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_binary_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            get_binary_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_2, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated binary literal")
            )
        )
    ).

:- pred string_get_binary_2(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_binary_2(String, !.LastDigit, Len, Posn1, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_binary_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_binary_2(String, !.LastDigit, Len, Posn1, Token,
                Context, !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_binary_2(String, !.LastDigit, Len, Posn1, Token,
                Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn1, BinaryString, !Posn),
                conv_string_to_int(BinaryString, base_2, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated binary literal")
            ),
            string_get_context(Posn1, Context, !Posn)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, base_2, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated binary literal")
        ),
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
        Token = error("unterminated octal literal")
    ;
        Result = ok,
        ( if char.is_octal_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            get_octal_2(Stream, LastDigit, [Char], Token, !IO)
        else if Char = '_' then
            get_octal(Stream, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated octal literal")
        )
    ).

:- pred string_get_octal(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_octal(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_octal_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_octal_2(String, LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else if Char = '_' then
            string_get_octal(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = error("unterminated octal literal"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        Token = error("unterminated octal literal"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_octal_2(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_octal_2(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_int(!.RevChars, base_8, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated octal literal")
        )
    ;
        Result = ok,
        ( if char.is_octal_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_octal_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            get_octal_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_8, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated octal literal")
            )
        )
    ).

:- pred string_get_octal_2(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_octal_2(String, !.LastDigit, Len, Posn1, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_octal_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_octal_2(String, !.LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_octal_2(String, !.LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn1, BinaryString, !Posn),
                conv_string_to_int(BinaryString, base_8, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated octal literal")
            ),
            string_get_context(Posn1, Context, !Posn)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, base_8, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated octal literal")
        ),
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
        Token = error("unterminated hexadecimal literal")
    ;
        Result = ok,
        ( if char.is_hex_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            get_hex_2(Stream, LastDigit, [Char], Token, !IO)
        else if Char = '_' then
            get_hex(Stream, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated hexadecimal literal")
        )
    ).

:- pred string_get_hex(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_hex(String, Len, Posn0, Token, Context, !Posn) :-
    Posn1 = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_hex_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_hex_2(String, LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else if Char = '_' then
            string_get_hex(String, Len, Posn0, Token, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = error("unterminated hexadecimal literal"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        Token = error("unterminated hexadecimal literal"),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_hex_2(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_hex_2(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_int(!.RevChars, base_16, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated hexadecimal literal")
        )
    ;
        Result = ok,
        ( if char.is_hex_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_hex_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            get_hex_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_16, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated hexadecimal literal")
            )
        )
    ).

:- pred string_get_hex_2(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_hex_2(String, !.LastDigit, Len, Posn1, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_hex_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_hex_2(String, !.LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_hex_2(String, !.LastDigit, Len, Posn1, Token, Context,
                !Posn)
        else
            (
                !.LastDigit = last_digit_is_not_underscore,
                string_ungetchar(String, !Posn),
                grab_string(String, Posn1, BinaryString, !Posn),
                conv_string_to_int(BinaryString, base_16, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated hexadecimal literal")
            ),
            string_get_context(Posn1, Context, !Posn)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, BinaryString, !Posn),
            conv_string_to_int(BinaryString, base_16, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated hexadecimal literal")
        ),
        string_get_context(Posn1, Context, !Posn)
    ).

:- pred get_number(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_number(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_int(!.RevChars, base_10, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        )
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_number(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            get_number(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = ('.') then
            (
                !.LastDigit = last_digit_is_not_underscore,
                get_int_dot(Stream, !.LastDigit, !.RevChars, Token, !IO)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            )
        else if ( Char = 'e' ; Char = 'E' ) then
            (
                !.LastDigit = last_digit_is_not_underscore,
                !:RevChars = [Char | !.RevChars],
                get_float_exponent(Stream, !.RevChars, Token, !IO)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("underscore before exponent")
            )
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_10, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            )
        )
    ).

:- pred string_get_number(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_number(String, !.LastDigit, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_number(String, !.LastDigit, Len, Posn0, Token, Context,
                !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_number(String, !.LastDigit, Len, Posn0, Token, Context,
                !Posn)
        else if Char = ('.') then
            (
                !.LastDigit = last_digit_is_not_underscore,
                string_get_int_dot(String, !.LastDigit, Len, Posn0, Token,
                    Context, !Posn)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal"),
                string_get_context(Posn0, Context, !Posn)
            )
        else if ( Char = 'e' ; Char = 'E' ) then
            (
                !.LastDigit = last_digit_is_not_underscore,
                string_get_float_exponent(String, Len, Posn0, Token, Context,
                    !Posn)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("underscore before exponent"),
                string_get_context(Posn0, Context, !Posn)
            )
        else
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn0, NumberString, !Posn),
                conv_string_to_int(NumberString, base_10, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn0, NumberString, !Posn),
            conv_string_to_int(NumberString, base_10, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_int_dot(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_int_dot(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    % XXX The float literal syntax doesn't match ISO Prolog.
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        io.putback_char(Stream, '.', !IO),
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_int(!.RevChars, base_10, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        )
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char, '.' | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_float_decimals(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            Token = error("underscore following decimal point")
        else
            io.putback_char(Stream, Char, !IO),
            % We can't putback the ".", because io.putback_char only
            % guarantees one character of pushback. So instead, we return
            % an `integer_dot' token; the main loop of get_token_list_2 will
            % handle this appropriately.
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_10, Token0),
                ( if Token0 = integer(Int) then
                    Token = integer_dot(Int)
                else
                    Token = Token0
                )
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            )
        )
    ).

:- pred string_get_int_dot(string::in, last_digit_is_underscore::in, int::in,
    posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_int_dot(String, !.LastDigit, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_float_decimals(String, !.LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else if Char = '_' then
            Token = error("underscore following decimal point"),
            string_get_context(Posn0, Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn0, NumberString, !Posn),
                conv_string_to_int(NumberString, base_10, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        string_ungetchar(String, !Posn),
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn0, NumberString, !Posn),
            conv_string_to_int(NumberString, base_10, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        string_get_context(Posn0, Context, !Posn)
    ).

    % We have read past the decimal point, so now get the decimals.
    %
:- pred get_float_decimals(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_float_decimals(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_float(!.RevChars, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("fractional part of float terminated by underscore")
        )
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_float_decimals(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit=  last_digit_is_underscore,
            get_float_decimals(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if ( Char = 'e' ; Char = 'E' ) then
            !:RevChars = [Char | !.RevChars],
            get_float_exponent(Stream, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_float(!.RevChars, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token =
                    error("fractional part of float terminated by underscore")
            )
        )
    ).

:- pred string_get_float_decimals(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_float_decimals(String, !.LastDigit, Len, Posn0, Token, Context,
        !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_float_decimals(String, !.LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_float_decimals(String, !.LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else if ( Char = 'e' ; Char = 'E' ) then
            string_get_float_exponent(String, Len, Posn0, Token, Context,
                !Posn)
        else
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_float_string(String, Posn0, FloatString, !Posn),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token =
                    error("fractional part of float terminated by underscore")
            ),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_float_string(String, Posn0, FloatString, !Posn),
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("fractional part of float terminated by underscore")
        ),
        string_get_context(Posn0, Context, !Posn)
    ).

:- pred get_float_exponent(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent(Stream, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_float(!.RevChars, Token)
    ;
        Result = ok,
        ( if ( Char = ('+') ; Char = ('-') ) then
            !:RevChars = [Char | !.RevChars],
            get_float_exponent_2(Stream, !.RevChars, Token, !IO)
        else if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            LastDigit = last_digit_is_not_underscore,
            get_float_exponent_3(Stream, LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated exponent in float literal")
        )
    ).

:- pred string_get_float_exponent(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if ( Char = ('+') ; Char = ('-') ) then
            string_get_float_exponent_2(String, Len, Posn0, Token, Context,
                !Posn)
        else if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_float_exponent_3(String, LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = error("unterminated exponent in float literal"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        grab_float_string(String, Posn0, FloatString, !Posn),
        conv_to_float(FloatString, Token),
        string_get_context(Posn0, Context, !Posn)
    ).

    % We have read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
:- pred get_float_exponent_2(io.input_stream::in, list(char)::in, token::out,
    io::di, io::uo) is det.

get_float_exponent_2(Stream, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("unterminated exponent in float literal")
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            LastDigit = last_digit_is_not_underscore,
            get_float_exponent_3(Stream, LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = error("unterminated exponent in float literal")
        )
    ).

    % We have read past the E signalling the start of the exponent -
    % make sure that there's at least one digit following,
    % and then get the remaining digits.
    %
:- pred string_get_float_exponent_2(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent_2(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_float_exponent_3(String, LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            Token = error("unterminated exponent in float literal"),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        Token = error("unterminated exponent in float literal"),
        string_get_context(Posn0, Context, !Posn)
    ).

    % We have read past the first digit of the exponent -
    % now get the remaining digits.
    %
:- pred get_float_exponent_3(io.input_stream::in, last_digit_is_underscore::in,
    list(char)::in, token::out, io::di, io::uo) is det.

get_float_exponent_3(Stream, !.LastDigit, !.RevChars, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        (
            !.LastDigit = last_digit_is_not_underscore,
            rev_char_list_to_float(!.RevChars, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated exponent in float literal")
        )
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            !:RevChars = [Char | !.RevChars],
            !:LastDigit = last_digit_is_not_underscore,
            get_float_exponent_3(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            get_float_exponent_3(Stream, !.LastDigit, !.RevChars, Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_float(!.RevChars, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated exponent in float literal")
            )
        )
    ).

:- pred string_get_float_exponent_3(string::in, last_digit_is_underscore::in,
    int::in, posn::in, token::out, string_token_context::out,
    posn::in, posn::out) is det.

string_get_float_exponent_3(String, !.LastDigit, Len, Posn0, Token, Context,
        !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_float_exponent_3(String, !.LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            string_get_float_exponent_3(String, !.LastDigit, Len, Posn0, Token,
                Context, !Posn)
        else
            string_ungetchar(String, !Posn),
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_float_string(String, Posn0, FloatString, !Posn),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated exponent in float literal")
            ),
            string_get_context(Posn0, Context, !Posn)
        )
    else
        grab_float_string(String, Posn0, FloatString, !Posn),
        (
            !.LastDigit = last_digit_is_not_underscore,
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated exponent in float literal")
        ),
        string_get_context(Posn0, Context, !Posn)
    ).

%---------------------------------------------------------------------------%
%
% Utility routines.
%

:- pred rev_char_list_to_int(list(char)::in, integer_base::in, token::out)
    is det.

rev_char_list_to_int(RevChars, Base, Token) :-
    ( if rev_char_list_to_string(RevChars, String) then
        conv_string_to_int(String, Base, Token)
    else
        Token = error("invalid character in int")
    ).

:- pred conv_string_to_int(string::in, integer_base::in, token::out) is det.

conv_string_to_int(String, Base, Token) :-
    BaseInt = integer_base_int(Base),
    ( if string.base_string_to_int_underscore(BaseInt, String, Int) then
        Token = integer(Int)
    else if integer.from_base_string_underscore(BaseInt, String, Integer) then
        Token = big_integer(Base, Integer)
    else
        Token = error("invalid character in int")
    ).

:- func integer_base_int(integer_base) = int.

integer_base_int(base_2) = 2.
integer_base_int(base_8) = 8.
integer_base_int(base_10) = 10.
integer_base_int(base_16) = 16.

:- pred rev_char_list_to_float(list(char)::in, token::out) is det.

rev_char_list_to_float(RevChars, Token) :-
    ( if rev_char_list_to_string(RevChars, String) then
        conv_to_float(String, Token)
    else
        Token = error("invalid character in int")
    ).

:- pred conv_to_float(string::in, token::out) is det.

conv_to_float(String, Token) :-
    ( if string.to_float(String, Float) then
        Token = float(Float)
    else
        Token = error("invalid float token")
    ).

:- pred rev_char_list_to_string(list(char)::in, string::out) is semidet.

rev_char_list_to_string(RevChars, String) :-
   string.semidet_from_rev_char_list(RevChars, String).

:- func null_character_error = token.

null_character_error =
    error("null character is illegal in strings and names").

%---------------------------------------------------------------------------%

token_to_string(Token, String) :-
    (
        Token = name(Name),
        string.append_list(["token '", Name, "'"], String)
    ;
        Token = variable(Var),
        string.append_list(["variable `", Var, "'"], String)
    ;
        Token = integer(Int),
        string.int_to_string(Int, IntString),
        string.append_list(["integer `", IntString, "'"], String)
    ;
        Token = big_integer(Base, Integer),
        (
            Base = base_2,
            BaseInt = 2,
            Prefix = "0b"
        ;
            Base = base_8,
            BaseInt = 8,
            Prefix = "0o"
        ;
            Base = base_10,
            BaseInt = 10,
            Prefix = ""
        ;
            Base = base_16,
            BaseInt = 16,
            Prefix = "0x"
        ),
        IntString = integer.to_base_string(Integer, BaseInt),
        string.append_list(["integer `", Prefix, IntString, "'"], String)
    ;
        Token = float(Float),
        string.float_to_string(Float, FloatString),
        string.append_list(["float `", FloatString, "'"], String)
    ;
        Token = string(TokenString),
        string.append_list(["string """, TokenString, """"], String)
    ;
        Token = implementation_defined(Name),
        string.append_list(["implementation-defined `$", Name, "'"], String)
    ;
        Token = open,
        String = "token ` ('"
    ;
        Token = open_ct,
        String = "token `('"
    ;
        Token = close,
        String = "token `)'"
    ;
        Token = open_list,
        String = "token `['"
    ;
        Token = close_list,
        String = "token `]'"
    ;
        Token = open_curly,
        String = "token `{'"
    ;
        Token = close_curly,
        String = "token `}'"
    ;
        Token = ht_sep,
        String = "token `|'"
    ;
        Token = comma,
        String = "token `,'"
    ;
        Token = end,
        String = "token `. '"
    ;
        Token = eof,
        String = "end-of-file"
    ;
        Token = junk(JunkChar),
        char.to_int(JunkChar, Code),
        string.int_to_base_string(Code, 16, Hex),
        string.append_list(["illegal character <<0x", Hex, ">>"], String)
    ;
        Token = io_error(IO_Error),
        io.error_message(IO_Error, IO_ErrorMessage),
        string.append("I/O error: ", IO_ErrorMessage, String)
    ;
        Token = error(Message),
        string.append_list(["illegal token (", Message, ")"], String)
    ;
        Token = integer_dot(Int),
        string.int_to_string(Int, IntString),
        string.append_list(["integer `", IntString, "'."], String)
    ).

%---------------------------------------------------------------------------%
