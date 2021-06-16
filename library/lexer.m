%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2008, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: lexer.m.
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

:- module lexer.
:- interface.

:- import_module char.
:- import_module io.
:- import_module integer.

%---------------------------------------------------------------------------%

:- type token
    --->    name(string)
    ;       variable(string)
    ;       integer(integer_base, integer, signedness, integer_size)
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

    ;       integer_dot(integer).
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

:- type signedness
    --->    signed
    ;       unsigned.

:- type integer_size
    --->    size_word
    ;       size_8_bit
    ;       size_16_bit
    ;       size_32_bit
    ;       size_64_bit.

    % For every token, we record the line number of the line on
    % which the token occurred.
    %
:- type token_context == int.   % line number

    % This "fat list" representation is more efficient than a list of pairs.
    %
:- type token_list
    --->    token_cons(token, token_context, token_list)
    ;       token_nil.

    % A line_context and a line_posn together contain exactly the same
    % fields as a posn, with the same semantics. The difference is that
    % stepping past a single character requires no memory allocation
    % whatsoever *unless* that character is a newline.
    %
    % XXX We should consider making both fields of line_context into uint32s,
    % to allow them to fit into a single 64 bit word. Simplicity would then
    % require line_posn's argument being a uint32 as well.

:- type line_context
    --->    line_context(
                line_context_current_line_number        :: int,
                line_context_offset_of_start_of_line    :: int
            ).

:- type line_posn
    --->    line_posn(
                line_posn_current_offset_in_file        :: int
            ).

    % Read a list of tokens either from the current input stream
    % or from the specified input stream.
    % Keep reading until we encounter either an `end' token
    % (i.e. a full stop followed by whitespace) or the end-of-file.
    %
    % See `char.is_whitespace' for the definition of whitespace characters
    % used by this predicate.
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
    % See `char.is_whitespace' for the definition of whitespace characters
    % used by this predicate.
    %
:- pred string_get_token_list_max(string::in, offset::in, token_list::out,
    posn::in, posn::out) is det.

:- pred linestr_get_token_list_max(string::in, offset::in, token_list::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

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

% Note that there are three implementations of most predicates here:
%
% - one that reads data from a stream, and passes around I/O states;
% - one that reads data from a fixed string, marking each position
%   in the string using a value of type "posn", and
% - one that reads data from a fixed string, marking each position
%   in the string using two values, of types "line_context" and line_posn".
%
% In each such group of three predicates, The first version (in both
% the above list and historically) has no prefix. The second has the prefix
% "string_", while the third has the prefix "linestr_".
%
% We can't write the io.state version in terms of the string versions
% because we don't know how many characters to read from the input stream
% until *after* we have lexically analysed the characters we *have* read in.
% This replaces the old Prolog behavior of stopping after an end token
% (i.e. `.' plus whitespace), without reading even the rest of the line,
% which some interactive applications require.
%
% Conversely, we can't write the string versions using the io.state version,
% since that would require either cheating with the io.state or ruining
% the string interface.
%
% The string version using posns is simpler than the version using
% line context/position pairs, but it allocates memory after reading in
% *every* character, while the latter does so only after reading in a newline.
% It pays a price for this in the form of more parameter passing.
% Bundling all the static data (such as String and Len) into a structure
% could reduce parameter passing overhead, but at the cost of slower access,
% which is probably not a good idea.
%
% XXX We should consider whether we want to keep the !Posn versions long-term.
%
% An alternative would be to write all versions in terms of a generic
% "char_stream" typeclass, with instances for io.states, posns and
% line context/position pairs. However, while we have optimizations
% that eliminate the overhead of generic calls in *most* cases, we cannot
% (yet) ensure the elimination of this overhead in *all* cases.

get_token_list(Tokens, !IO) :-
    io.input_stream(Stream, !IO),
    get_token_list(Stream, Tokens, !IO).

get_token_list(Stream, Tokens, !IO) :-
    % We build the tokens up as lists of characters in reverse order.
    % When we get to the end of each token, we call `rev_char_list_to_string/2'
    % to convert that representation into a string.
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
        Token0 = integer_dot(Integer),
        get_context(Stream, Context1, !IO),
        get_dot(Stream, Token1, !IO),
        get_token_list_2(Stream, Token1, Context1, Tokens1, !IO),
        Tokens = token_cons(integer(base_10, Integer, signed, size_word),
            Context0, Tokens1)
    ;
        ( Token0 = float(_)
        ; Token0 = string(_)
        ; Token0 = variable(_)
        ; Token0 = integer(_, _, _, _)
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
        ; Token = integer(_, _, _, _)
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
        disable_warning [suspicious_recursion] (
            string_get_token_list_max(String, Len, Tokens1, !Posn)
        ),
        Tokens = token_cons(Token, Context, Tokens1)
    ).

linestr_get_token_list_max(String, Len, Tokens, !LineContext, !LinePosn) :-
    linestr_get_token(String, Len, Token, Context, !LineContext, !LinePosn),
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
        ; Token = integer(_, _, _, _)
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
        disable_warning [suspicious_recursion] (
            linestr_get_token_list_max(String, Len, Tokens1,
                !LineContext, !LinePosn)
        ),
        Tokens = token_cons(Token, Context, Tokens1)
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

:- pred string_get_context(posn::in, string_token_context::out) is det.

string_get_context(StartPosn, Context) :-
    StartPosn = posn(StartLineNum, _, _),
    Context = StartLineNum.
    % In future, we might want to modify this code to read something like this:
    %
    % posn_to_line_and_column(StartPosn, StartLineNum, StartColumn),
    % posn_to_line_and_column(!.Posn, EndLineNum, EndColumn),
    % Context = detailed(StartLine, StartColumn, EndLine, EndColumn).

:- pred linestr_get_context(line_context::in, string_token_context::out)
    is det.

linestr_get_context(StartLineContext, Context) :-
    StartLineContext = line_context(StartLineNum, _),
    Context = StartLineNum.

:- pred string_read_char(string::in, int::in, char::out,
    posn::in, posn::out) is semidet.
:- pragma inline(pred(string_read_char/5)).

string_read_char(String, Len, Char, Posn0, Posn) :-
    Posn0 = posn(LineNum0, LineStartOffset0, Offset0),
    Offset0 < Len,
    string.unsafe_index_next(String, Offset0, Offset, Char),
    ( if Char = '\n' then
        Posn = posn(LineNum0 + 1, Offset, Offset)
    else
        Posn = posn(LineNum0, LineStartOffset0, Offset)
    ).

:- pred linestr_read_char(string::in, int::in, char::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out)
    is semidet.
:- pragma inline(pred(linestr_read_char/7)).

linestr_read_char(String, Len, Char,
        LineContext0, LineContext, LinePosn0, LinePosn) :-
    LinePosn0 = line_posn(Offset0),
    Offset0 < Len,
    string.unsafe_index_next(String, Offset0, Offset, Char),
    LinePosn = line_posn(Offset),
    ( if Char = '\n' then
        LineContext0 = line_context(LineNum0, _LineStartOffset),
        LineContext = line_context(LineNum0 + 1, Offset)
    else
        LineContext = LineContext0
    ).

    % We used to use this predicate as the equivalent in !Posn variants
    % of io.putback_char in the !IO variants. However, it is simpler to
    % simply remember the position *before* we stepped over the character
    % we want to put back. The obsolete pragma is so that people get
    % a warning if they use it without knowing this, while the consider_used
    % allows us to keep it around anyway, at least for now.
    %
:- pred string_ungetchar(string::in, posn::in, posn::out) is det.
:- pragma obsolete(pred(string_ungetchar/3)).
:- pragma consider_used(pred(string_ungetchar/3)).

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

:- pred grab_string(string::in, posn::in, posn::in, string::out) is det.

grab_string(String, Posn0, Posn, SubString) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    string.unsafe_between(String, Offset0, Offset, SubString).

:- pred linestr_grab_string(string::in, line_posn::in, line_posn::in,
    string::out) is det.

linestr_grab_string(String, LinePosn0, LinePosn, SubString) :-
    LinePosn0 = line_posn(Offset0),
    LinePosn = line_posn(Offset),
    string.unsafe_between(String, Offset0, Offset, SubString).

    % As above, but the string is known to represent a float literal.
    % Filter out any underscore characters from the returned string.
    % We have to do this since the underlying mechanisms we currently use for
    % converting strings into floats (sscanf in C, parseDouble in Java etc)
    % cannot handle underscores in their input.
    %
:- pred grab_float_string(string::in, posn::in, posn::in, string::out) is det.

grab_float_string(String, Posn0, Posn, FloatString) :-
    Posn0 = posn(_, _, Offset0),
    Posn = posn(_, _, Offset),
    unsafe_get_float_between(String, Offset0, Offset, FloatString).

:- pred linestr_grab_float_string(string::in, line_posn::in, line_posn::in,
    string::out) is det.

linestr_grab_float_string(String, LinePosn0, LinePosn, FloatString) :-
    LinePosn0 = line_posn(Offset0),
    LinePosn = line_posn(Offset),
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

    % Default implementation.
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
:- pragma consider_used(pred(is_underscore/1)).

is_underscore('_').

:- pred string_set_line_number(int::in, posn::in, posn::out) is det.

string_set_line_number(LineNumber, Posn0, Posn) :-
    Posn0 = posn(_, _, Offset),
    Posn = posn(LineNumber, Offset, Offset).

:- pred linestr_set_line_number(int::in, line_context::out, line_posn::in)
    is det.

linestr_set_line_number(LineNumber, LineContext, LinePosn0) :-
    LinePosn0 = line_posn(Offset),
    LineContext = line_context(LineNumber, Offset).

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
:- pred get_token_2(io.input_stream::in, scanned_past_whitespace::in,
    token::out, token_context::out, io::di, io::uo) is det.
:- pragma inline(pred(get_token_2/6)).

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

:- pred linestr_get_token(string::in, int::in, token::out, token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_token(String, Len, Token, Context, !LineContext, !LinePosn) :-
    linestr_get_token_2(String, Len, not_scanned_past_whitespace,
        Token, Context, !LineContext, !LinePosn).

:- pred string_get_token_2(string::in, int::in, scanned_past_whitespace::in,
    token::out, token_context::out, posn::in, posn::out) is det.
% Due to the inlining of the calls to lookup_token_action and the only call
% to the !Posn version of execute_get_token_action (which could thus be
% deleted), all the calls to string_get_token_2 are self-tail-recursive,
% with one exception: the call from string_get_token. Given this fact,
% unlike get_token_2, this call should not be inlined: the elimination
% of the overhead of one call would be outweighed by the pollution
% of the instruction cache.
% :- pragma inline(string_get_token_2/7).

string_get_token_2(String, Len, ScannedPastWhiteSpace,
        Token, Context, !Posn) :-
    Posn0 = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if
            % The condition of this if-then-else is effectively
            % this conjunction,
            %
            %   lookup_token_action(Char, Action),
            %   execute_string_get_token_action(String, Len, Posn0, Char,
            %       Action, ScannedPastWhiteSpace, Token, Context, !Posn)
            %
            % with both calls inlined and then Action being optimized away
            % through deforestation, saving the second switch.
            %
            % Note that get_token_2, the !IO variant of the predicate,
            % cannot use this approach, because the switch in
            % lookup_token_action is incomplete, and we cannot update !IO
            % in the arms of such switches.
            (
                % This list of characters comes from the code of
                % char.is_whitespace. Any update here will also require
                % an update there.
                ( Char = ' '
                ; Char = '\t'
                ; Char = '\n'
                ; Char = '\r'
                ; Char = '\f'
                ; Char = '\v'
                ),
                % Action = action_whitespace
                string_get_token_2(String, Len, scanned_past_whitespace,
                    TokenPrime, ContextPrime, !Posn)
            ;
                % This list of characters comes
                % from char.is_alnum_or_underscore and char.lower_upper.
                ( Char = 'a' ; Char = 'b' ; Char = 'c' ; Char = 'd'
                ; Char = 'e' ; Char = 'f' ; Char = 'g' ; Char = 'h'
                ; Char = 'i' ; Char = 'j' ; Char = 'k' ; Char = 'l'
                ; Char = 'm' ; Char = 'n' ; Char = 'o' ; Char = 'p'
                ; Char = 'q' ; Char = 'r' ; Char = 's' ; Char = 't'
                ; Char = 'u' ; Char = 'v' ; Char = 'w' ; Char = 'x'
                ; Char = 'y' ; Char = 'z'
                ),
                % Action = action_alpha_lower
                string_get_name(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                % This list of characters comes from
                % char.is_alnum_or_underscore and char.lower_upper.
                ( Char = '_'
                ; Char = 'A' ; Char = 'B' ; Char = 'C' ; Char = 'D'
                ; Char = 'E' ; Char = 'F' ; Char = 'G' ; Char = 'H'
                ; Char = 'I' ; Char = 'J' ; Char = 'K' ; Char = 'L'
                ; Char = 'M' ; Char = 'N' ; Char = 'O' ; Char = 'P'
                ; Char = 'Q' ; Char = 'R' ; Char = 'S' ; Char = 'T'
                ; Char = 'U' ; Char = 'V' ; Char = 'W' ; Char = 'X'
                ; Char = 'Y' ; Char = 'Z'
                ),
                % Action = action_alpha_upper_uscore
                string_get_variable(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = '0',
                % Action = action_zero
                string_get_zero(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                % This list of characters comes from
                % char.is_alnum_or_underscore and char.is_digit.
                ( Char = '1' ; Char = '2' ; Char = '3' ; Char = '4'
                ; Char = '5' ; Char = '6' ; Char = '7' ; Char = '8'
                ; Char = '9'
                ),
                % Action = action_nonzero_digit
                LastDigit = last_digit_is_not_underscore,
                string_get_number(String, Len, LastDigit, Posn0,
                    TokenPrime, ContextPrime, !Posn)
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
                % Action = action_special_token
                handle_special_token(Char, ScannedPastWhiteSpace, TokenPrime),
                string_get_context(Posn0, ContextPrime)
            ;
                Char = ('.'),
                % Action = action_dot
                string_get_dot(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = ('%'),
                % Action = action_percent
                string_skip_to_eol(String, Len, TokenPrime0, HaveToken0,
                    !Posn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    string_get_token_2(String, Len, scanned_past_whitespace,
                        TokenPrime, ContextPrime, !Posn)
                )
            ;
                Char = ('/'),
                % Action = action_slash
                string_get_slash(String, Len, Posn0, TokenPrime0, HaveToken0,
                    !Posn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    string_get_token_2(String, Len, scanned_past_whitespace,
                        TokenPrime, ContextPrime, !Posn)
                )
            ;
                ( Char = '"'
                ; Char = ''''
                ),
                % Action = action_quote
                string_start_quoted_name(String, Len, Char, [], Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = ('#'),
                % Action = action_hash
                string_get_source_line_number(String, Len, !.Posn,
                    TokenPrime0, HaveToken0, !Posn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    string_get_token_2(String, Len,
                        not_scanned_past_whitespace,
                        TokenPrime, ContextPrime, !Posn)
                )
            ;
                Char = ('`'),
                % Action = action_backquote
                string_get_context(Posn0, ContextPrime),
                TokenPrime = name("`")
            ;
                Char = ('$'),
                % Action = action_dollar
                string_get_implementation_defined_literal_rest(String, Len,
                    Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                % These are the characters for which graphic_token_char
                % succeeds. The ones that are commented out have their own
                % actions.
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
                % Action = action_graphic_token
                string_get_graphic(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            Token = junk(Char),
            string_get_context(Posn0, Context)
        )
    else
        Token = eof,
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_token_2(string::in, int::in, scanned_past_whitespace::in,
    token::out, token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.
% The comment on inlining string_get_token_2 applies here as well.

linestr_get_token_2(String, Len, ScannedPastWhiteSpace,
        Token, Context, !LineContext, !LinePosn) :-
    LineContext0 = !.LineContext,
    LinePosn0 = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if
            % The condition of this if-then-else is effectively
            % this conjunction,
            %
            %   lookup_token_action(Char, Action),
            %   execute_linestr_get_token_action(String, Len,
            %       LineContext0, LinePosn0, Char,
            %       Action, ScannedPastWhiteSpace, Token, Context,
            %       !LineContext, !LinePosn)
            %
            % with both calls inlined and then Action being optimized away
            % through deforestation, saving the second switch.
            %
            % Note that get_token_2, the !IO variant of the predicate,
            % cannot use this approach, because the switch in
            % lookup_token_action is incomplete, and we cannot update !IO
            % in the arms of such switches.
            (
                % This list of characters comes from the code of
                % char.is_whitespace. Any update here will also require
                % an update there.
                ( Char = ' '
                ; Char = '\t'
                ; Char = '\n'
                ; Char = '\r'
                ; Char = '\f'
                ; Char = '\v'
                ),
                % Action = action_whitespace
                linestr_get_token_2(String, Len, scanned_past_whitespace,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                % This list of characters comes
                % from char.is_alnum_or_underscore and char.lower_upper.
                ( Char = 'a' ; Char = 'b' ; Char = 'c' ; Char = 'd'
                ; Char = 'e' ; Char = 'f' ; Char = 'g' ; Char = 'h'
                ; Char = 'i' ; Char = 'j' ; Char = 'k' ; Char = 'l'
                ; Char = 'm' ; Char = 'n' ; Char = 'o' ; Char = 'p'
                ; Char = 'q' ; Char = 'r' ; Char = 's' ; Char = 't'
                ; Char = 'u' ; Char = 'v' ; Char = 'w' ; Char = 'x'
                ; Char = 'y' ; Char = 'z'
                ),
                % Action = action_alpha_lower
                linestr_get_name(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                % This list of characters comes from
                % char.is_alnum_or_underscore and char.lower_upper.
                ( Char = '_'
                ; Char = 'A' ; Char = 'B' ; Char = 'C' ; Char = 'D'
                ; Char = 'E' ; Char = 'F' ; Char = 'G' ; Char = 'H'
                ; Char = 'I' ; Char = 'J' ; Char = 'K' ; Char = 'L'
                ; Char = 'M' ; Char = 'N' ; Char = 'O' ; Char = 'P'
                ; Char = 'Q' ; Char = 'R' ; Char = 'S' ; Char = 'T'
                ; Char = 'U' ; Char = 'V' ; Char = 'W' ; Char = 'X'
                ; Char = 'Y' ; Char = 'Z'
                ),
                % Action = action_alpha_upper_uscore
                linestr_get_variable(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = '0',
                % Action = action_zero
                linestr_get_zero(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                % This list of characters comes from
                % char.is_alnum_or_underscore and char.is_digit.
                ( Char = '1' ; Char = '2' ; Char = '3' ; Char = '4'
                ; Char = '5' ; Char = '6' ; Char = '7' ; Char = '8'
                ; Char = '9'
                ),
                % Action = action_nonzero_digit
                LastDigit = last_digit_is_not_underscore,
                linestr_get_number(String, Len, LastDigit,
                    LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
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
                % Action = action_special_token
                handle_special_token(Char, ScannedPastWhiteSpace, TokenPrime),
                linestr_get_context(LineContext0, ContextPrime)
            ;
                Char = ('.'),
                % Action = action_dot
                linestr_get_dot(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = ('%'),
                % Action = action_percent
                linestr_skip_to_eol(String, Len, TokenPrime0, HaveToken0,
                    !LineContext, !LinePosn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    linestr_get_token_2(String, Len, scanned_past_whitespace,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                )
            ;
                Char = ('/'),
                % Action = action_slash
                linestr_get_slash(String, Len, LineContext0, LinePosn0,
                    TokenPrime0, HaveToken0, !LineContext, !LinePosn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    linestr_get_token_2(String, Len, scanned_past_whitespace,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                )
            ;
                ( Char = '"'
                ; Char = ''''
                ),
                % Action = action_quote
                linestr_start_quoted_name(String, Len, Char, [], LineContext0,
                LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = ('#'),
                % Action = action_hash
                linestr_get_source_line_number(String, Len,
                    !.LineContext, !.LinePosn,
                    TokenPrime0, HaveToken0, !LineContext, !LinePosn),
                ( if have_token_with_context(HaveToken0, ContextPrime0) then
                    TokenPrime = TokenPrime0,
                    ContextPrime = ContextPrime0
                else
                    linestr_get_token_2(String, Len,
                        not_scanned_past_whitespace,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                )
            ;
                Char = ('`'),
                % Action = action_backquote
                linestr_get_context(LineContext0, ContextPrime),
                TokenPrime = name("`")
            ;
                Char = ('$'),
                % Action = action_dollar
                linestr_get_implementation_defined_literal_rest(String, Len,
                    LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                % These are the characters for which graphic_token_char
                % succeeds. The ones that are commented out have their own
                % actions.
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
                % Action = action_graphic_token
                linestr_get_graphic(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            linestr_get_context(LineContext0, Context),
            Token = junk(Char)
        )
    else
        linestr_get_context(LineContext0, Context),
        Token = eof
    ).

    % Decide on how the given character should be treated. Note that
    % performance suffers significantly if this predicate is not inlined.
    %
    % Note that string_get_token_2 contains an inlined copy of this predicate,
    % so any changes here should be reflected there, and vice versa.
    %
:- pred lookup_token_action(char::in, get_token_action::out) is semidet.
:- pragma inline(pred(lookup_token_action/2)).

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

:- pred string_have_token(posn::in, maybe_have_valid_token::out) is det.

string_have_token(Posn0, maybe_have_valid_token(Context)) :-
    string_get_context(Posn0, Context).

:- pred linestr_have_token(line_context::in, maybe_have_valid_token::out)
    is det.

linestr_have_token(LineContext0, maybe_have_valid_token(Context)) :-
    linestr_get_context(LineContext0, Context).

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
            get_token_2(Stream, not_scanned_past_whitespace,
                Token, Context, !IO)
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

%---------------------------------------------------------------------------%

    % Decide what to do for a token which consists of a special character.
    % The reason for inlining this predicate is that each caller has a
    % specific value for ScannedPastWhiteSpace, and thus after inlining,
    % the compiler should be able to eliminate the switch on
    % ScannedPastWhiteSpace.
    %
:- pred handle_special_token(char::in, scanned_past_whitespace::in, token::out)
    is det.
:- pragma inline(pred(handle_special_token/3)).

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
        error($pred, "unknown special token")
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
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if whitespace_after_dot(Char) then
            !:Posn = LastCharPosn,
            Token = end,
            string_get_context(Posn0, Context)
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        else
            !:Posn = LastCharPosn,
            Token = name("."),
            string_get_context(Posn0, Context)
        )
    else
        Token = end,
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_dot(string::in, int::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_dot(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastCharLineContext = !.LineContext,
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if whitespace_after_dot(Char) then
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            linestr_get_context(LineContext0, Context),
            Token = end
        else if graphic_token_char(Char) then
            linestr_get_graphic(String, Len, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            linestr_get_context(LineContext0, Context),
            Token = name(".")
        )
    else
        linestr_get_context(LineContext0, Context),
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
            disable_warning [suspicious_recursion] (
                string_skip_to_eol(String, Len, Token, HaveToken, !Posn)
            )
        )
    else
        Token = eof,
        string_have_token(!.Posn, HaveToken)
    ).

:- pred linestr_skip_to_eol(string::in, int::in,
    token::out, maybe_have_valid_token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_skip_to_eol(String, Len, Token, HaveToken, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = '\n' then
            do_not_have_token(Token, HaveToken)
        else
            disable_warning [suspicious_recursion] (
                linestr_skip_to_eol(String, Len, Token, HaveToken,
                    !LineContext, !LinePosn)
            )
        )
    else
        Token = eof,
        linestr_have_token(!.LineContext, HaveToken)
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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = ('*') then
            string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn),
            HaveToken = maybe_have_valid_token(Context)
        else
            !:Posn = LastPosn,
            Token = name("/"),
            string_have_token(Posn0, HaveToken)
        )
    else
        Token = name("/"),
        string_have_token(Posn0, HaveToken)
    ).

:- pred linestr_get_slash(string::in, int::in,
    line_context::in, line_posn::in, token::out, maybe_have_valid_token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_slash(String, Len, LineContext0, LinePosn0, Token, HaveToken,
        !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = ('*') then
            linestr_get_comment(String, Len, LineContext0, LinePosn0,
                Token, HaveToken, !LineContext, !LinePosn)
        else if graphic_token_char(Char) then
            linestr_get_graphic(String, Len, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn),
            HaveToken = maybe_have_valid_token(Context)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = name("/"),
            linestr_have_token(LineContext0, HaveToken)
        )
    else
        Token = name("/"),
        linestr_have_token(LineContext0, HaveToken)
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
            disable_warning [suspicious_recursion] (
                string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
            )
        )
    else
        Token = error("unterminated '/*' comment"),
        string_have_token(Posn0, HaveToken)
    ).

:- pred linestr_get_comment(string::in, int::in,
    line_context::in, line_posn::in, token::out, maybe_have_valid_token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_comment(String, Len, LineContext0, LinePosn0, Token, HaveToken,
        !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = ('*') then
            linestr_get_comment_2(String, Len, LineContext0, LinePosn0,
                Token, HaveToken, !LineContext, !LinePosn)
        else
            disable_warning [suspicious_recursion] (
                linestr_get_comment(String, Len, LineContext0, LinePosn0,
                    Token, HaveToken, !LineContext, !LinePosn)
            )
        )
    else
        Token = error("unterminated '/*' comment"),
        linestr_have_token(LineContext0, HaveToken)
    ).

:- pred get_comment_2(io.input_stream::in, token::out,
    maybe_have_valid_token::out, io::di, io::uo) is det.

get_comment_2(Stream, Token, HaveToken, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error),
        have_token(Stream, HaveToken, !IO)
    ;
        Result = eof,
        Token = error("unterminated '/*' comment"),
        have_token(Stream, HaveToken, !IO)
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
            disable_warning [suspicious_recursion] (
                string_get_comment_2(String, Len, Posn0, Token, HaveToken,
                    !Posn)
            )
        else
            string_get_comment(String, Len, Posn0, Token, HaveToken, !Posn)
        )
    else
        Token = error("unterminated '/*' comment"),
        string_have_token(Posn0, HaveToken)
    ).

:- pred linestr_get_comment_2(string::in, int::in,
    line_context::in, line_posn::in, token::out, maybe_have_valid_token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_comment_2(String, Len, LineContext0, LinePosn0, Token, HaveToken,
        !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = ('/') then
            % end of /* ... */ comment, so get next token
            do_not_have_token(Token, HaveToken)
        else if Char = ('*') then
            disable_warning [suspicious_recursion] (
                linestr_get_comment_2(String, Len, LineContext0, LinePosn0,
                    Token, HaveToken, !LineContext, !LinePosn)
            )
        else
            linestr_get_comment(String, Len, LineContext0, LinePosn0,
                Token, HaveToken, !LineContext, !LinePosn)
        )
    else
        Token = error("unterminated '/*' comment"),
        linestr_have_token(LineContext0, HaveToken)
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
        disable_warning [suspicious_recursion] (
            string_start_quoted_name(String, Len, QuoteChar, !.RevChars,
                Posn0, _, _, !Posn)
        ),
        Token = Token0
    else if Token0 = eof then
        Token = error("unterminated quote")
    else
        Token = Token0
    ).

:- pred linestr_start_quoted_name(string::in, int::in, char::in,
    list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_start_quoted_name(String, Len, QuoteChar, !.RevChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
        LineContext0, LinePosn0, Token0, Context, !LineContext, !LinePosn),
    ( if Token0 = error(_) then
        % Skip to the end of the string or name.
        disable_warning [suspicious_recursion] (
            linestr_start_quoted_name(String, Len, QuoteChar, !.RevChars,
                LineContext0, LinePosn0, _, _, !LineContext, !LinePosn)
        ),
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
            disable_warning [suspicious_recursion] (
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        )
    else
        Token = eof,
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_quoted_name(string::in, int::in, char::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = QuoteChar then
            linestr_get_quoted_name_quote(String, Len, QuoteChar, !.RevChars,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else if Char = ('\\') then
            linestr_get_quoted_name_escape(String, Len, QuoteChar, !.RevChars,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:RevChars = [Char | !.RevChars],
            disable_warning [suspicious_recursion] (
                linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        )
    else
        Token = eof,
        linestr_get_context(LineContext0, Context)
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
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = QuoteChar then
            !:RevChars = [Char | !.RevChars],
            string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                Posn0, Token, Context, !Posn)
        else
            !:Posn = LastCharPosn,
            finish_quoted_name(QuoteChar, !.RevChars, Token),
            string_get_context(Posn0, Context)
        )
    else
        finish_quoted_name(QuoteChar, !.RevChars, Token),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_quoted_name_quote(string::in, int::in, char::in,
    list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_quoted_name_quote(String, Len, QuoteChar, !.RevChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    LastCharLineContext = !.LineContext,
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = QuoteChar then
            !:RevChars = [Char | !.RevChars],
            linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            finish_quoted_name(QuoteChar, !.RevChars, Token),
            linestr_get_context(LineContext0, Context)
        )
    else
        finish_quoted_name(QuoteChar, !.RevChars, Token),
        linestr_get_context(LineContext0, Context)
    ).

:- pred finish_quoted_name(char::in, list(char)::in, token::out) is det.

finish_quoted_name(QuoteChar, RevChars, Token) :-
    ( if rev_char_list_to_string(RevChars, String) then
        ( if QuoteChar = '''' then
            Token = name(String)
        else if QuoteChar = '"' then
            Token = string(String)
        else
            error("lexer.m: unknown quote character")
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
        ( if
            % Note that get_quoted_name, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '\n',
                string_get_quoted_name(String, Len, QuoteChar,
                    !.RevChars, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = '\r',
                % Files created on Windows may have an extra return character.
                disable_warning [suspicious_recursion] (
                    string_get_quoted_name_escape(String, Len, QuoteChar,
                        !.RevChars, Posn0,
                        TokenPrime, ContextPrime, !Posn)
                )
            ;
                Char = 'x',
                string_get_hex_escape(String, Len, QuoteChar,
                    !.RevChars, [], Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'u',
                string_get_unicode_escape(4, String, Len, QuoteChar,
                    !.RevChars, [], Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'U',
                string_get_unicode_escape(8, String, Len, QuoteChar,
                    !.RevChars, [], Posn0,
                    TokenPrime, ContextPrime, !Posn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else if
            char.is_octal_digit(Char)
        then
            string_get_octal_escape(String, Len, QuoteChar, !.RevChars, [Char],
                Posn0, Token, Context, !Posn)
        else if
            escape_char(Char, EscapedChar)
        then
            !:RevChars = [EscapedChar | !.RevChars],
            string_get_quoted_name(String, Len, QuoteChar, !.RevChars, Posn0,
                Token, Context, !Posn)
        else
            Token = error("invalid escape character"),
            string_get_context(!.Posn, Context)
        )
    else
        Token = eof,
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_quoted_name_escape(string::in, int::in,
    char::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_quoted_name_escape(String, Len, QuoteChar, !.RevChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if
            % Note that get_quoted_name, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '\n',
                linestr_get_quoted_name(String, Len, QuoteChar,
                    !.RevChars, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = '\r',
                % Files created on Windows may have an extra return character.
                disable_warning [suspicious_recursion] (
                    linestr_get_quoted_name_escape(String, Len, QuoteChar,
                        !.RevChars, LineContext0, LinePosn0,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                )
            ;
                Char = 'x',
                linestr_get_hex_escape(String, Len, QuoteChar,
                    !.RevChars, [], LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'u',
                linestr_get_unicode_escape(4, String, Len, QuoteChar,
                    !.RevChars, [], LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'U',
                linestr_get_unicode_escape(8, String, Len, QuoteChar,
                    !.RevChars, [], LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else if
            char.is_octal_digit(Char)
        then
            linestr_get_octal_escape(String, Len, QuoteChar,
                !.RevChars, [Char], LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else if
            escape_char(Char, EscapedChar)
        then
            !:RevChars = [EscapedChar | !.RevChars],
            linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            Token = error("invalid escape character"),
            linestr_get_context(!.LineContext, Context)
        )
    else
        Token = eof,
        linestr_get_context(LineContext0, Context)
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
                Token = null_character_error,
                string_get_context(Posn0, Context)
            else
                !:RevChars = [UnicodeChar | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            Token = error("invalid Unicode character code"),
            string_get_context(Posn0, Context)
        )
    else
        ( if string_read_char(String, Len, Char, !Posn) then
            ( if char.is_hex_digit(Char) then
                !:RevHexChars = [Char | !.RevHexChars],
                disable_warning [suspicious_recursion] (
                    string_get_unicode_escape(NumHexChars, String, Len,
                        QuoteChar, !.RevChars, !.RevHexChars, Posn0,
                        Token, Context, !Posn)
                )
            else
                Token = error("invalid hex character in Unicode escape"),
                string_get_context(Posn0, Context)
            )
        else
            Token = eof,
            string_get_context(Posn0, Context)
        )
    ).

:- pred linestr_get_unicode_escape(int::in, string::in, int::in, char::in,
    list(char)::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_unicode_escape(NumHexChars, String, Len, QuoteChar,
        !.RevChars, !.RevHexChars, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    ( if NumHexChars = list.length(!.RevHexChars) then
        ( if
            rev_char_list_to_string(!.RevHexChars, HexString),
            string.base_string_to_int(16, HexString, UnicodeCharCode),
            allowed_unicode_char_code(UnicodeCharCode),
            char.from_int(UnicodeCharCode, UnicodeChar)
        then
            ( if UnicodeCharCode = 0 then
                linestr_get_context(LineContext0, Context),
                Token = null_character_error
            else
                !:RevChars = [UnicodeChar | !.RevChars],
                linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            Token = error("invalid Unicode character code"),
            linestr_get_context(LineContext0, Context)
        )
    else
        ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
            ( if char.is_hex_digit(Char) then
                !:RevHexChars = [Char | !.RevHexChars],
                disable_warning [suspicious_recursion] (
                    linestr_get_unicode_escape(NumHexChars, String, Len,
                        QuoteChar, !.RevChars, !.RevHexChars,
                        LineContext0, LinePosn0,
                        Token, Context, !LineContext, !LinePosn)
                )
            else
                Token = error("invalid hex character in Unicode escape"),
                linestr_get_context(LineContext0, Context)
            )
        else
            Token = eof,
            linestr_get_context(LineContext0, Context)
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
            disable_warning [suspicious_recursion] (
                string_get_hex_escape(String, Len, QuoteChar,
                    !.RevChars, !.RevHexChars, Posn0, Token, Context, !Posn)
            )
        else if Char = ('\\') then
            string_finish_hex_escape(String, Len, QuoteChar, !.RevChars,
                !.RevHexChars, Posn0, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context),
            Token = error("unterminated hex escape")
        )
    else
        string_get_context(Posn0, Context),
        Token = eof
    ).

:- pred linestr_get_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_hex_escape(String, Len, QuoteChar, !.RevChars, !.RevHexChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_hex_digit(Char) then
            !:RevHexChars = [Char | !.RevHexChars],
            disable_warning [suspicious_recursion] (
                linestr_get_hex_escape(String, Len, QuoteChar,
                    !.RevChars, !.RevHexChars, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = ('\\') then
            linestr_finish_hex_escape(String, Len, QuoteChar, !.RevChars,
                !.RevHexChars, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            Token = error("unterminated hex escape"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = eof,
        linestr_get_context(LineContext0, Context)
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
        string_get_context(Posn0, Context),
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
                string_get_context(Posn0, Context)
            else
                !:RevChars = [Char | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            string_get_context(Posn0, Context),
            Token = error("invalid hex escape")
        )
    ).

:- pred linestr_finish_hex_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_finish_hex_escape(String, Len, QuoteChar, !.RevChars, !.RevHexChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    (
        !.RevHexChars = [],
        linestr_get_context(LineContext0, Context),
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
                linestr_get_context(LineContext0, Context)
            else
                !:RevChars = [Char | !.RevChars],
                linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            linestr_get_context(LineContext0, Context),
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
            disable_warning [suspicious_recursion] (
                string_get_octal_escape(String, Len, QuoteChar,
                    !.RevChars, !.RevOctalChars, Posn0, Token, Context, !Posn)
            )
        else if Char = ('\\') then
            string_finish_octal_escape(String, Len, QuoteChar,
                !.RevChars, !.RevOctalChars, Posn0, Token, Context, !Posn)
        else
            string_get_context(Posn0, Context),
            Token = error("unterminated octal escape")
        )
    else
        Token = eof,
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_octal_escape(String, Len, QuoteChar, !.RevChars, !.RevOctalChars,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_octal_digit(Char) then
            !:RevOctalChars = [Char | !.RevOctalChars],
            disable_warning [suspicious_recursion] (
                linestr_get_octal_escape(String, Len, QuoteChar,
                    !.RevChars, !.RevOctalChars, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = ('\\') then
            linestr_finish_octal_escape(String, Len, QuoteChar,
                !.RevChars, !.RevOctalChars, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            linestr_get_context(LineContext0, Context),
            Token = error("unterminated octal escape")
        )
    else
        Token = eof,
        linestr_get_context(LineContext0, Context)
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
        string_get_context(Posn0, Context)
    ;
        !.RevOctalChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevOctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error,
                string_get_context(Posn0, Context)
            else
                !:RevChars = [Char | !.RevChars],
                string_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    Posn0, Token, Context, !Posn)
            )
        else
            Token = error("invalid octal escape"),
            string_get_context(Posn0, Context)
        )
    ).

:- pred linestr_finish_octal_escape(string::in, int::in, char::in,
    list(char)::in, list(char)::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_finish_octal_escape(String, Len, QuoteChar,
        !.RevChars, !.RevOctalChars, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    (
        !.RevOctalChars = [],
        Token = error("empty octal escape"),
        linestr_get_context(LineContext0, Context)
    ;
        !.RevOctalChars = [_ | _],
        ( if
            rev_char_list_to_string(!.RevOctalChars, OctalString),
            string.base_string_to_int(8, OctalString, Int),
            char.to_int(Char, Int)
        then
            ( if Int = 0 then
                Token = null_character_error,
                linestr_get_context(LineContext0, Context)
            else
                !:RevChars = [Char | !.RevChars],
                linestr_get_quoted_name(String, Len, QuoteChar, !.RevChars,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            Token = error("invalid octal escape"),
            linestr_get_context(LineContext0, Context)
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
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_alnum_or_underscore(Char) then
            disable_warning [suspicious_recursion] (
                string_get_name(String, Len, Posn0, Token, Context, !Posn)
            )
        else
            grab_string(String, Posn0, LastCharPosn, Name),
            !:Posn = LastCharPosn,
            Token = name(Name),
            string_get_context(Posn0, Context)
        )
    else
        grab_string(String, Posn0, !.Posn, Name),
        Token = name(Name),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_name(string::in, int::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_name(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastCharLineContext = !.LineContext,
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_alnum_or_underscore(Char) then
            disable_warning [suspicious_recursion] (
                linestr_get_name(String, Len, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            linestr_grab_string(String, LinePosn0, LastCharLinePosn, Name),
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            Token = name(Name),
            linestr_get_context(LineContext0, Context)
        )
    else
        linestr_grab_string(String, LinePosn0, !.LinePosn, Name),
        Token = name(Name),
        linestr_get_context(LineContext0, Context)
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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_lower(Char) then
            string_get_name(String, Len, LastPosn, Token0, Context, !Posn),
            ( if Token0 = name(S) then
                Token = implementation_defined(S)
            else
                Token = Token0
            )
        else if graphic_token_char(Char) then
            string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
        else
            !:Posn = LastPosn,
            Token = name("$"),
            string_get_context(Posn0, Context)
        )
    else
        Token = name("$"),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_implementation_defined_literal_rest(string::in, int::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_implementation_defined_literal_rest(String, Len,
        LineContext0, LinePosn0, Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_lower(Char) then
            linestr_get_name(String, Len, LastLineContext, LastLinePosn,
                Token0, Context, !LineContext, !LinePosn),
            ( if Token0 = name(S) then
                Token = implementation_defined(S)
            else
                Token = Token0
            )
        else if graphic_token_char(Char) then
            linestr_get_graphic(String, Len, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = name("$"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = name("$"),
        linestr_get_context(LineContext0, Context)
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
                    string.format("invalid line number `%s' " ++
                        "in `#' line number directive", [s(String)], Message),
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
            string.format("invalid character `%s' " ++
                "in `#' line number directive", [s(String)], Message),
            Token = error(Message)
        )
    ).

:- pred string_get_source_line_number(string::in, int::in, posn::in,
    token::out, maybe_have_valid_token::out, posn::in, posn::out) is det.

string_get_source_line_number(String, Len, Posn1, Token, HaveToken, !Posn) :-
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            disable_warning [suspicious_recursion] (
                string_get_source_line_number(String, Len, Posn1,
                    Token, HaveToken, !Posn)
            )
        else if Char = '\n' then
            grab_string(String, Posn1, LastCharPosn, LineNumString),
            ( if
                string.base_string_to_int(10, LineNumString, LineNum),
                LineNum > 0
            then
                string_set_line_number(LineNum, !Posn),
                do_not_have_token(Token, HaveToken)
            else
                string_have_token(Posn1, HaveToken),
                string.format("invalid line number `%s' " ++
                    "in `#' line number directive", [s(LineNumString)],
                    Message),
                Token = error(Message)
            )
        else
            string_have_token(Posn1, HaveToken),
            ( if char.to_int(Char, 0) then
                DirectiveString = "NUL"
            else
                string.from_char_list([Char], DirectiveString)
            ),
            string.format("invalid character `%s' " ++
                "in `#' line number directive", [s(DirectiveString)], Message),
            Token = error(Message)
        )
    else
        string_have_token(Posn1, HaveToken),
        Token = error("unexpected end-of-file in `#' line number directive")
    ).

:- pred linestr_get_source_line_number(string::in, int::in,
    line_context::in, line_posn::in, token::out, maybe_have_valid_token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_source_line_number(String, Len, LineContext1, LinePosn1,
        Token, HaveToken, !LineContext, !LinePosn) :-
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            disable_warning [suspicious_recursion] (
                linestr_get_source_line_number(String, Len,
                    LineContext1, LinePosn1,
                    Token, HaveToken, !LineContext, !LinePosn)
            )
        else if Char = '\n' then
            linestr_grab_string(String, LinePosn1, LastCharLinePosn,
                LineNumString),
            ( if
                string.base_string_to_int(10, LineNumString, LineNum),
                LineNum > 0
            then
                linestr_set_line_number(LineNum, !:LineContext, !.LinePosn),
                do_not_have_token(Token, HaveToken)
            else
                linestr_have_token(LineContext1, HaveToken),
                string.format("invalid line number `%s' " ++
                    "in `#' line number directive", [s(LineNumString)],
                    Message),
                Token = error(Message)
            )
        else
            linestr_have_token(LineContext1, HaveToken),
            ( if char.to_int(Char, 0) then
                DirectiveString = "NUL"
            else
                string.from_char_list([Char], DirectiveString)
            ),
            string.format("invalid character `%s' " ++
                "in `#' line number directive", [s(DirectiveString)], Message),
            Token = error(Message)
        )
    else
        linestr_have_token(LineContext1, HaveToken),
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
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if graphic_token_char(Char) then
            disable_warning [suspicious_recursion] (
                string_get_graphic(String, Len, Posn0, Token, Context, !Posn)
            )
        else
            !:Posn = LastCharPosn,
            grab_string(String, Posn0, !.Posn, Name),
            Token = name(Name),
            string_get_context(Posn0, Context)
        )
    else
        grab_string(String, Posn0, !.Posn, Name),
        string_get_context(Posn0, Context),
        Token = name(Name)
    ).

:- pred linestr_get_graphic(string::in, int::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_graphic(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastCharLineContext = !.LineContext,
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if graphic_token_char(Char) then
            disable_warning [suspicious_recursion] (
                linestr_get_graphic(String, Len, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            linestr_grab_string(String, LinePosn0, !.LinePosn, Name),
            Token = name(Name),
            linestr_get_context(LineContext0, Context)
        )
    else
        linestr_grab_string(String, LinePosn0, !.LinePosn, Name),
        Token = name(Name),
        linestr_get_context(LineContext0, Context)
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
    LastCharPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_alnum_or_underscore(Char) then
            disable_warning [suspicious_recursion] (
                string_get_variable(String, Len, Posn0, Token, Context, !Posn)
            )
        else
            !:Posn = LastCharPosn,
            grab_string(String, Posn0, !.Posn, VariableName),
            Token = variable(VariableName),
            string_get_context(Posn0, Context)
        )
    else
        grab_string(String, Posn0, !.Posn, VariableName),
        Token = variable(VariableName),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_variable(string::in, int::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_variable(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastCharLineContext = !.LineContext,
    LastCharLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_alnum_or_underscore(Char) then
            disable_warning [suspicious_recursion] (
                linestr_get_variable(String, Len, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            !:LineContext = LastCharLineContext,
            !:LinePosn = LastCharLinePosn,
            linestr_grab_string(String, LinePosn0, !.LinePosn, VariableName),
            Token = variable(VariableName),
            linestr_get_context(LineContext0, Context)
        )
    else
        linestr_grab_string(String, LinePosn0, !.LinePosn, VariableName),
        Token = variable(VariableName),
        linestr_get_context(LineContext0, Context)
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
        Token = integer(base_10, integer.zero, signed, size_word)
    ;
        Result = ok,
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            get_number(Stream, LastDigit, [Char], Token, !IO)
        else if Char = '_' then
            LastDigit = last_digit_is_underscore,
            % We need to pass ['0'] to get_number/6 here because the initial
            % '0' may in fact be the last digit in the token (e.g. if what
            % appears after the '_' is a signedness / size suffix).
            get_number(Stream, LastDigit, ['0'], Token, !IO)
        else if Char = '''' then
            get_char_code(Stream, Token, !IO)
        else if Char = 'b' then
            get_binary(Stream, Token, !IO)
        else if Char = 'o' then
            get_octal(Stream, Token, !IO)
        else if Char = 'x' then
            get_hex(Stream, Token, !IO)
        else if Char = 'u' then
            % In this (and the following) case '0' is the only digit in the
            % token; we need to pass ['0'] to get_integer_size_suffix/7 because
            % we would otherwise invoke rev_char_list_to_int/5 with an empty
            % list.
            get_integer_size_suffix(Stream, ['0'], base_10, unsigned,
                Token, !IO)
        else if Char = 'i' then
            get_integer_size_suffix(Stream, ['0'], base_10, signed,
                Token, !IO)
        else if Char = ('.') then
            LastDigit = last_digit_is_not_underscore,
            get_int_dot(Stream, LastDigit, ['0'], Token, !IO)
        else if ( Char = 'e' ; Char = 'E' ) then
            get_float_exponent(Stream, [Char, '0'], Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            Token = integer(base_10, integer.zero, signed, size_word)
        )
    ).

    % This type records whether the last "digit" seen by the lexer
    % as it process a numeric token was an underscore or not.
    % We need this to detect invalid uses of underscores in numeric literals.
    % Note that there may be other intervening characters in the token
    % between the last digit and the current one (e.g. the decimal point
    % or beginning of an exponent a float literal.)
    %
:- type last_digit_is_underscore
    --->    last_digit_is_underscore
    ;       last_digit_is_not_underscore.

:- pred string_get_zero(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_zero(String, Len, Posn0, Token, Context, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_number(String, Len, LastDigit, Posn0,
                Token, Context, !Posn)
        else if
            % Note that get_zero, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '_',
                LastDigit = last_digit_is_underscore,
                string_get_number(String, Len, LastDigit, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = '''',
                string_get_char_code(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'b',
                string_get_binary(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'o',
                string_get_octal(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'x',
                string_get_hex(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            ;
                Char = 'u',
                string_get_integer_size_suffix(String, Len, Posn0, LastPosn,
                    base_10, unsigned, TokenPrime, !Posn),
                string_get_context(Posn0, ContextPrime)
            ;
                Char = 'i',
                string_get_integer_size_suffix(String, Len, Posn0, LastPosn,
                    base_10, signed, TokenPrime, !Posn),
                string_get_context(Posn0, ContextPrime)
            ;
                Char = ('.'),
                LastDigit = last_digit_is_not_underscore,
                string_get_int_dot(String, Len, LastDigit, Posn0, LastPosn,
                    TokenPrime, ContextPrime, !Posn)
            ;
                ( Char = 'e'
                ; Char = 'E'
                ),
                string_get_float_exponent(String, Len, Posn0,
                    TokenPrime, ContextPrime, !Posn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            !:Posn = LastPosn,
            Token = integer(base_10, integer.zero, signed, size_word),
            string_get_context(Posn0, Context)
        )
    else
        Token = integer(base_10, integer.zero, signed, size_word),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_zero(string::in, int::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_zero(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            linestr_get_number(String, Len, LastDigit, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else if
            % Note that get_zero, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '_',
                LastDigit = last_digit_is_underscore,
                linestr_get_number(String, Len, LastDigit,
                    LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = '''',
                linestr_get_char_code(String, Len, LineContext0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'b',
                linestr_get_binary(String, Len, LineContext0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'o',
                linestr_get_octal(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'x',
                linestr_get_hex(String, Len, LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                Char = 'u',
                linestr_get_integer_size_suffix(String, Len,
                    LinePosn0, LastLinePosn,
                    base_10, unsigned, TokenPrime, !LineContext, !LinePosn),
                linestr_get_context(LineContext0, ContextPrime)
            ;
                Char = 'i',
                linestr_get_integer_size_suffix(String, Len,
                    LinePosn0, LastLinePosn,
                    base_10, signed, TokenPrime, !LineContext, !LinePosn),
                linestr_get_context(LineContext0, ContextPrime)
            ;
                Char = ('.'),
                LastDigit = last_digit_is_not_underscore,
                linestr_get_int_dot(String, Len, LastDigit,
                    LineContext0, LinePosn0, LastLineContext, LastLinePosn,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            ;
                ( Char = 'e'
                ; Char = 'E'
                ),
                linestr_get_float_exponent(String, Len,
                    LineContext0, LinePosn0,
                    TokenPrime, ContextPrime, !LineContext, !LinePosn)
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = integer(base_10, integer.zero, signed, size_word),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = integer(base_10, integer.zero, signed, size_word),
        linestr_get_context(LineContext0, Context)
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
        Token = integer(base_10, integer(CharCode), signed, size_word)
    ).

:- pred string_get_char_code(string::in, int::in, posn::in, token::out,
    string_token_context::out, posn::in, posn::out) is det.

string_get_char_code(String, Len, Posn0, Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        char.to_int(Char, CharCode),
        Token = integer(base_10, integer(CharCode), signed, size_word)
    else
        Token = error("unterminated char code literal")
    ),
    string_get_context(Posn0, Context).

:- pred linestr_get_char_code(string::in, int::in, line_context::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_char_code(String, Len, LineContext0, Token, Context,
        !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        char.to_int(Char, CharCode),
        Token = integer(base_10, integer(CharCode), signed, size_word)
    else
        Token = error("unterminated char code literal")
    ),
    linestr_get_context(LineContext0, Context).

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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_binary_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_binary_2(String, Len, LastDigit, LastPosn,
                Token, Context, !Posn)
        else if Char = '_' then
            string_get_binary(String, Len, LastPosn, Token, Context, !Posn)
        else
            !:Posn = LastPosn,
            Token = error("unterminated binary literal"),
            string_get_context(Posn0, Context)
        )
    else
        Token = error("unterminated binary literal"),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_binary(string::in, int::in, line_context::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_binary(String, Len, LineContext0, Token, Context,
        !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_binary_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            linestr_get_binary_2(String, Len, LastDigit,
                LastLineContext, LastLinePosn,
                Token, Context, !LineContext, !LinePosn)
        else if Char = '_' then
            linestr_get_binary(String, Len, LastLineContext,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = error("unterminated binary literal"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = error("unterminated binary literal"),
        linestr_get_context(LineContext0, Context)
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
            rev_char_list_to_int(!.RevChars, base_2, signed, size_word,
                Token)
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
        else if Char = 'u' then
            get_integer_size_suffix(Stream, !.RevChars, base_2, unsigned,
                Token, !IO)
        else if Char = 'i' then
            get_integer_size_suffix(Stream, !.RevChars, base_2, signed,
                Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_2, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated binary literal")
            )
        )
    ).

:- pred string_get_binary_2(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_binary_2(String, Len, !.LastDigit, Posn1, Token, Context, !Posn) :-
    % The last character we saw _may_ be the last digit (or underscore) in the
    % token; save its position as LastDigitPosn. In the event that the next
    % character is the beginning of a signedness / size suffix, Posn1 to
    % LastDigitPosn will define the substring that needs to passed to the
    % integer conversion procedure.

    LastDigitPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_binary_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_binary_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                string_get_binary_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = 'u' then
            string_get_integer_size_suffix(String, Len, Posn1,
                LastDigitPosn, base_2, unsigned, Token, !Posn),
            string_get_context(Posn1, Context)
        else if Char = 'i' then
            string_get_integer_size_suffix(String, Len, Posn1,
                LastDigitPosn, base_2, signed, Token, !Posn),
            string_get_context(Posn1, Context)
        else
            !:Posn = LastDigitPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn1, !.Posn, BinaryString),
                conv_string_to_int(BinaryString, base_2, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated binary literal")
            ),
            string_get_context(Posn1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, !.Posn, BinaryString),
            conv_string_to_int(BinaryString, base_2, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated binary literal")
        ),
        string_get_context(Posn1, Context)
    ).

:- pred linestr_get_binary_2(string::in, int::in, last_digit_is_underscore::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_binary_2(String, Len, !.LastDigit, LineContext1, LinePosn1,
        Token, Context, !LineContext, !LinePosn) :-
    % The last character we saw _may_ be the last digit (or underscore) in the
    % token; save its position as LastDigitPosn. In the event that the next
    % character is the beginning of a signedness / size suffix, Posn1 to
    % LastDigitPosn will define the substring that needs to passed to the
    % integer conversion procedure.

    LastDigitLineContext = !.LineContext,
    LastDigitLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_binary_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_binary_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_binary_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = 'u' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn,
                base_2, unsigned, Token, !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else if Char = 'i' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn,
                base_2, signed, Token, !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else
            !:LineContext = LastDigitLineContext,
            !:LinePosn = LastDigitLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_string(String, LinePosn1, !.LinePosn,
                    BinaryString),
                conv_string_to_int(BinaryString, base_2, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated binary literal")
            ),
            linestr_get_context(LineContext1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_string(String, LinePosn1, !.LinePosn, BinaryString),
            conv_string_to_int(BinaryString, base_2, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated binary literal")
        ),
        linestr_get_context(LineContext1, Context)
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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_octal_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_octal_2(String, Len, LastDigit, LastPosn,
                Token, Context, !Posn)
        else if Char = '_' then
            disable_warning [suspicious_recursion] (
                string_get_octal(String, Len, Posn0, Token, Context, !Posn)
            )
        else
            !:Posn = LastPosn,
            Token = error("unterminated octal literal"),
            string_get_context(Posn0, Context)
        )
    else
        Token = error("unterminated octal literal"),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_octal(string::in, int::in,
    line_context::in, line_posn::in, token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_octal(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_octal_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            linestr_get_octal_2(String, Len, LastDigit,
                LastLineContext, LastLinePosn,
                Token, Context, !LineContext, !LinePosn)
        else if Char = '_' then
            disable_warning [suspicious_recursion] (
                linestr_get_octal(String, Len, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = error("unterminated octal literal"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = error("unterminated octal literal"),
        linestr_get_context(LineContext0, Context)
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
            rev_char_list_to_int(!.RevChars, base_8, signed, size_word,
                Token)
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
        else if Char = 'u' then
            get_integer_size_suffix(Stream, !.RevChars, base_8, unsigned,
                Token, !IO)
        else if Char = 'i' then
            get_integer_size_suffix(Stream, !.RevChars, base_8, signed,
                Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_8, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated octal literal")
            )
        )
    ).

:- pred string_get_octal_2(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_octal_2(String, Len, !.LastDigit, Posn1, Token, Context, !Posn) :-
    LastDigitPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_octal_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_octal_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                string_get_octal_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = 'u' then
            string_get_integer_size_suffix(String, Len, Posn1, LastDigitPosn,
                base_8, unsigned, Token, !Posn),
            string_get_context(Posn1, Context)
        else if Char = 'i' then
            string_get_integer_size_suffix(String, Len, Posn1, LastDigitPosn,
                base_8, signed, Token, !Posn),
            string_get_context(Posn1, Context)
        else
            !:Posn = LastDigitPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn1, !.Posn, BinaryString),
                conv_string_to_int(BinaryString, base_8, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated octal literal")
            ),
            string_get_context(Posn1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, !.Posn, BinaryString),
            conv_string_to_int(BinaryString, base_8, signed, size_word, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated octal literal")
        ),
        string_get_context(Posn1, Context)
    ).

:- pred linestr_get_octal_2(string::in, int::in, last_digit_is_underscore::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_octal_2(String, Len, !.LastDigit, LineContext1, LinePosn1,
        Token, Context, !LineContext, !LinePosn) :-
    LastDigitLineContext = !.LineContext,
    LastDigitLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_octal_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_octal_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_octal_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = 'u' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn,
                base_8, unsigned, Token, !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else if Char = 'i' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn,
                base_8, signed, Token, !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else
            !:LineContext = LastDigitLineContext,
            !:LinePosn = LastDigitLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_string(String, LinePosn1, !.LinePosn,
                    BinaryString),
                conv_string_to_int(BinaryString, base_8, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated octal literal")
            ),
            linestr_get_context(LineContext1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_string(String, LinePosn1, !.LinePosn, BinaryString),
            conv_string_to_int(BinaryString, base_8, signed, size_word, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated octal literal")
        ),
        linestr_get_context(LineContext1, Context)
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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_hex_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_hex_2(String, Len, LastDigit, LastPosn,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            disable_warning [suspicious_recursion] (
                string_get_hex(String, Len, Posn0, Token, Context, !Posn)
            )
        else
            !:Posn = LastPosn,
            Token = error("unterminated hexadecimal literal"),
            string_get_context(Posn0, Context)
        )
    else
        Token = error("unterminated hexadecimal literal"),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_hex(string::in, int::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_hex(String, Len, LineContext0, LinePosn0, Token, Context,
        !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_hex_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_hex_2(String, Len, LastDigit,
                    LastLineContext, LastLinePosn,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            disable_warning [suspicious_recursion] (
                linestr_get_hex(String, Len, LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = error("unterminated hexadecimal literal"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = error("unterminated hexadecimal literal"),
        linestr_get_context(LineContext0, Context)
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
            rev_char_list_to_int(!.RevChars, base_16, signed, size_word,
                Token)
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
            disable_warning [suspicious_recursion] (
                get_hex_2(Stream, !.LastDigit, !.RevChars, Token, !IO)
            )
        else if Char = 'u' then
            get_integer_size_suffix(Stream, !.RevChars, base_16, unsigned,
                Token, !IO)
        else if Char = 'i' then
            get_integer_size_suffix(Stream, !.RevChars, base_16, signed,
                Token, !IO)
        else
            io.putback_char(Stream, Char, !IO),
            (
                !.LastDigit = last_digit_is_not_underscore,
                rev_char_list_to_int(!.RevChars, base_16, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated hexadecimal literal")
            )
        )
    ).

:- pred string_get_hex_2(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_hex_2(String, Len, !.LastDigit, Posn1, Token, Context, !Posn) :-
    LastDigitPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_hex_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_hex_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                string_get_hex_2(String, Len, !.LastDigit, Posn1,
                    Token, Context, !Posn)
            )
        else if Char = 'u' then
            string_get_integer_size_suffix(String, Len, Posn1,
                LastDigitPosn, base_16, unsigned, Token, !Posn),
            string_get_context(Posn1, Context)
        else if Char = 'i' then
            string_get_integer_size_suffix(String, Len, Posn1,
                LastDigitPosn, base_16, signed, Token, !Posn),
            string_get_context(Posn1, Context)
        else
            !:Posn = LastDigitPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn1, !.Posn, BinaryString),
                conv_string_to_int(BinaryString, base_16, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated hexadecimal literal")
            ),
            string_get_context(Posn1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn1, !.Posn, BinaryString),
            conv_string_to_int(BinaryString, base_16, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated hexadecimal literal")
        ),
        string_get_context(Posn1, Context)
    ).

:- pred linestr_get_hex_2(string::in, int::in,
    last_digit_is_underscore::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_hex_2(String, Len, !.LastDigit, LineContext1, LinePosn1,
        Token, Context, !LineContext, !LinePosn) :-
    LastDigitLineContext = !.LineContext,
    LastDigitLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_hex_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_hex_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_hex_2(String, Len, !.LastDigit,
                    LineContext1, LinePosn1,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = 'u' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn, base_16, unsigned, Token,
                !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else if Char = 'i' then
            linestr_get_integer_size_suffix(String, Len,
                LinePosn1, LastDigitLinePosn, base_16, signed, Token,
                !LineContext, !LinePosn),
            linestr_get_context(LineContext1, Context)
        else
            !:LineContext = LastDigitLineContext,
            !:LinePosn = LastDigitLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_string(String, LinePosn1, !.LinePosn,
                    BinaryString),
                conv_string_to_int(BinaryString, base_16, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated hexadecimal literal")
            ),
            linestr_get_context(LineContext1, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_string(String, LinePosn1, !.LinePosn, BinaryString),
            conv_string_to_int(BinaryString, base_16, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated hexadecimal literal")
        ),
        linestr_get_context(LineContext1, Context)
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
            rev_char_list_to_int(!.RevChars, base_10, signed, size_word,
                Token)
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
        else if Char = 'u' then
            get_integer_size_suffix(Stream, !.RevChars, base_10, unsigned,
                Token, !IO)
        else if Char = 'i' then
            get_integer_size_suffix(Stream, !.RevChars, base_10, signed,
                Token, !IO)
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
                rev_char_list_to_int(!.RevChars, base_10, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            )
        )
    ).

:- pred string_get_number(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_number(String, Len, !.LastDigit, Posn0, Token, Context, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_number(String, Len, !.LastDigit, Posn0,
                    Token, Context, !Posn)
            )
        else if
            % Note that get_number, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '_',
                !:LastDigit = last_digit_is_underscore,
                disable_warning [suspicious_recursion] (
                    string_get_number(String, Len, !.LastDigit, Posn0,
                        TokenPrime, ContextPrime, !Posn)
                )
            ;
                Char = ('.'),
                (
                    !.LastDigit = last_digit_is_not_underscore,
                    string_get_int_dot(String, Len, !.LastDigit, Posn0,
                        LastPosn, TokenPrime, ContextPrime, !Posn)
                ;
                    !.LastDigit = last_digit_is_underscore,
                    TokenPrime = error("unterminated decimal literal"),
                    string_get_context(Posn0, ContextPrime)
                )
            ;
                Char = 'u',
                string_get_integer_size_suffix(String, Len, Posn0, LastPosn,
                    base_10, unsigned, TokenPrime, !Posn),
                string_get_context(Posn0, ContextPrime)
            ;
                Char = 'i',
                string_get_integer_size_suffix(String, Len, Posn0, LastPosn,
                    base_10, signed, TokenPrime, !Posn),
                string_get_context(Posn0, ContextPrime)
            ;
                ( Char = 'e'
                ; Char = 'E'
                ),
                (
                    !.LastDigit = last_digit_is_not_underscore,
                    string_get_float_exponent(String, Len, Posn0,
                        TokenPrime, ContextPrime, !Posn)
                ;
                    !.LastDigit = last_digit_is_underscore,
                    TokenPrime = error("underscore before exponent"),
                    string_get_context(Posn0, ContextPrime)
                )
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            !:Posn = LastPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn0, !.Posn, NumberString),
                conv_string_to_int(NumberString, base_10, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            string_get_context(Posn0, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn0, !.Posn, NumberString),
            conv_string_to_int(NumberString, base_10, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_number(string::in, int::in,
    last_digit_is_underscore::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_number(String, Len, !.LastDigit, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_number(String, Len, !.LastDigit,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if
            % Note that get_number, the !IO variant of the predicate,
            % cannot use this switch, because it is incomplete,
            % and we cannot update !IO in the arms of such switches.
            (
                Char = '_',
                !:LastDigit = last_digit_is_underscore,
                disable_warning [suspicious_recursion] (
                    linestr_get_number(String, Len, !.LastDigit,
                        LineContext0, LinePosn0,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                )
            ;
                Char = ('.'),
                (
                    !.LastDigit = last_digit_is_not_underscore,
                    linestr_get_int_dot(String, Len, !.LastDigit,
                        LineContext0, LinePosn0, LastLineContext, LastLinePosn,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                ;
                    !.LastDigit = last_digit_is_underscore,
                    TokenPrime = error("unterminated decimal literal"),
                    linestr_get_context(LineContext0, ContextPrime)
                )
            ;
                Char = 'u',
                linestr_get_integer_size_suffix(String, Len,
                    LinePosn0, LastLinePosn, base_10, unsigned,
                    TokenPrime, !LineContext, !LinePosn),
                linestr_get_context(LineContext0, ContextPrime)
            ;
                Char = 'i',
                linestr_get_integer_size_suffix(String, Len,
                    LinePosn0, LastLinePosn, base_10, signed,
                    TokenPrime, !LineContext, !LinePosn),
                linestr_get_context(LineContext0, ContextPrime)
            ;
                ( Char = 'e'
                ; Char = 'E'
                ),
                (
                    !.LastDigit = last_digit_is_not_underscore,
                    linestr_get_float_exponent(String, Len,
                        LineContext0, LinePosn0,
                        TokenPrime, ContextPrime, !LineContext, !LinePosn)
                ;
                    !.LastDigit = last_digit_is_underscore,
                    TokenPrime = error("underscore before exponent"),
                    linestr_get_context(LineContext0, ContextPrime)
                )
            )
        then
            Token = TokenPrime,
            Context = ContextPrime
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_string(String, LinePosn0, !.LinePosn,
                    NumberString),
                conv_string_to_int(NumberString, base_10, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            linestr_get_context(LineContext0, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_string(String, LinePosn0, !.LinePosn, NumberString),
            conv_string_to_int(NumberString, base_10, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        linestr_get_context(LineContext0, Context)
    ).

:- pred get_integer_size_suffix(io.input_stream::in, list(char)::in,
    integer_base::in, signedness::in, token::out, io::di, io::uo) is det.

get_integer_size_suffix(Stream, RevChars, Base, Signedness, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        rev_char_list_to_int(RevChars, Base, Signedness, size_word, Token)
    ;
        Result = ok,
        ( if Char = '8' then
            rev_char_list_to_int(RevChars, Base, Signedness, size_8_bit, Token)
        else if Char = '1' then
            get_integer_size_suffix_2(Stream, RevChars, Base, Signedness,
                '6', size_16_bit, Token, !IO)
        else if Char = '3' then
            get_integer_size_suffix_2(Stream, RevChars, Base, Signedness,
                '2', size_32_bit, Token, !IO)
        else if Char = '6' then
            get_integer_size_suffix_2(Stream, RevChars, Base, Signedness,
                '4', size_64_bit, Token, !IO)
        else if char.is_digit(Char) then
            Token = error("invalid integer size suffix")
        else
            io.putback_char(Stream, Char, !IO),
            rev_char_list_to_int(RevChars, Base, Signedness, size_word, Token)
        )
    ).

:- pred string_get_integer_size_suffix(string::in, int::in, posn::in,
    posn::in, integer_base::in, signedness::in, token::out,
    posn::in, posn::out) is det.

string_get_integer_size_suffix(String, Len, Posn1, LastDigitPosn, Base,
        Signedness, Token, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = '8' then
            grab_string(String, Posn1, LastDigitPosn, DigitString),
            conv_string_to_int(DigitString, Base, Signedness, size_8_bit,
                Token)
        else if Char = '1' then
            string_get_integer_size_suffix_2(String, Len, Posn1, LastDigitPosn,
                Base, Signedness, '6', size_16_bit, Token, !Posn)
        else if Char = '3' then
            string_get_integer_size_suffix_2(String, Len, Posn1, LastDigitPosn,
                Base, Signedness, '2', size_32_bit, Token, !Posn)
        else if Char = '6' then
            string_get_integer_size_suffix_2(String, Len, Posn1, LastDigitPosn,
                Base, Signedness, '4', size_64_bit, Token, !Posn)
        else if char.is_digit(Char) then
            Token = error("invalid integer size suffix")
        else
            !:Posn = LastPosn,
            grab_string(String, Posn1, LastDigitPosn, DigitString),
            conv_string_to_int(DigitString, Base, Signedness, size_word,
                Token)
        )
    else
        grab_string(String, Posn1, LastDigitPosn, DigitString),
        conv_string_to_int(DigitString, Base, Signedness, size_word,
            Token)
    ).

:- pred linestr_get_integer_size_suffix(string::in, int::in,
    line_posn::in, line_posn::in, integer_base::in, signedness::in, token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_integer_size_suffix(String, Len, LinePosn1, LastDigitLinePosn,
        Base, Signedness, Token, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = '8' then
            linestr_grab_string(String, LinePosn1, LastDigitLinePosn,
                DigitString),
            conv_string_to_int(DigitString, Base, Signedness, size_8_bit,
                Token)
        else if Char = '1' then
            linestr_get_integer_size_suffix_2(String, Len,
                LinePosn1, LastDigitLinePosn,
                Base, Signedness, '6', size_16_bit, Token,
                !LineContext, !LinePosn)
        else if Char = '3' then
            linestr_get_integer_size_suffix_2(String, Len,
                LinePosn1, LastDigitLinePosn,
                Base, Signedness, '2', size_32_bit, Token,
                !LineContext, !LinePosn)
        else if Char = '6' then
            linestr_get_integer_size_suffix_2(String, Len,
                LinePosn1, LastDigitLinePosn,
                Base, Signedness, '4', size_64_bit, Token,
                !LineContext, !LinePosn)
        else if char.is_digit(Char) then
            Token = error("invalid integer size suffix")
        else
            !:LinePosn = LastLinePosn,
            !:LineContext = LastLineContext,
            linestr_grab_string(String, LinePosn1, LastDigitLinePosn,
                DigitString),
            conv_string_to_int(DigitString, Base, Signedness, size_word,
                Token)
        )
    else
        linestr_grab_string(String, LinePosn1, LastDigitLinePosn, DigitString),
        conv_string_to_int(DigitString, Base, Signedness, size_word,
            Token)
    ).

:- pred get_integer_size_suffix_2(io.input_stream::in, list(char)::in,
    integer_base::in, signedness::in, char::in, integer_size::in,
    token::out, io::di, io::uo) is det.

get_integer_size_suffix_2(Stream, RevChars, Base, Signedness, ExpectedNextChar,
        ExpectedSize, Token, !IO) :-
    io.read_char_unboxed(Stream, Result, Char, !IO),
    (
        Result = error(Error),
        Token = io_error(Error)
    ;
        Result = eof,
        Token = error("invalid integer size suffix")
    ;
        Result = ok,
        ( if Char = ExpectedNextChar then
            rev_char_list_to_int(RevChars, Base, Signedness, ExpectedSize,
                Token)
        else
            Token = error("invalid integer size suffix")
        )
    ).

:- pred string_get_integer_size_suffix_2(string::in, int::in,
    posn::in, posn::in, integer_base::in, signedness::in, char::in,
    integer_size::in, token::out, posn::in, posn::out) is det.

string_get_integer_size_suffix_2(String, Len, Posn1, LastDigitPosn,
        Base, Signedness, ExpectedChar, Size, Token, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if Char = ExpectedChar then
            grab_string(String, Posn1, LastDigitPosn, DigitString),
            conv_string_to_int(DigitString, Base, Signedness, Size, Token)
        else
            Token = error("invalid integer size suffix")
        )
    else
        Token = error("invalid integer size suffix")
    ).

:- pred linestr_get_integer_size_suffix_2(string::in, int::in,
    line_posn::in, line_posn::in,
    integer_base::in, signedness::in, char::in,
    integer_size::in, token::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_integer_size_suffix_2(String, Len, LinePosn1, LastDigitLinePosn,
        Base, Signedness, ExpectedChar, Size, Token,
        !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if Char = ExpectedChar then
            linestr_grab_string(String, LinePosn1, LastDigitLinePosn,
                DigitString),
            conv_string_to_int(DigitString, Base, Signedness, Size, Token)
        else
            Token = error("invalid integer size suffix")
        )
    else
        Token = error("invalid integer size suffix")
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
            rev_char_list_to_int(!.RevChars, base_10, signed, size_word,
                Token)
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
                rev_char_list_to_int(!.RevChars, base_10, signed, size_word,
                    Token0),
                ( if Token0 = integer(_, Integer, _, _) then
                    Token = integer_dot(Integer)
                else
                    Token = Token0
                )
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            )
        )
    ).

:- pred string_get_int_dot(string::in, int::in,
    last_digit_is_underscore::in, posn::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_int_dot(String, Len, !.LastDigit, Posn0, PosnBeforeDot,
        Token, Context, !Posn) :-
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            string_get_float_decimals(String, Len, !.LastDigit, Posn0,
                Token, Context, !Posn)
        else if Char = '_' then
            Token = error("underscore following decimal point"),
            string_get_context(Posn0, Context)
        else
            !:Posn = PosnBeforeDot,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_string(String, Posn0, !.Posn, NumberString),
                conv_string_to_int(NumberString, base_10, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            string_get_context(Posn0, Context)
        )
    else
        !:Posn = PosnBeforeDot,
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_string(String, Posn0, !.Posn, NumberString),
            conv_string_to_int(NumberString, base_10, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_int_dot(string::in, int::in,
    last_digit_is_underscore::in,
    line_context::in, line_posn::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_int_dot(String, Len, !.LastDigit,
        LineContext0, LinePosn0, LineContextBeforeDot, LinePosnBeforeDot,
        Token, Context, !LineContext, !LinePosn) :-
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            linestr_get_float_decimals(String, Len, !.LastDigit,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else if Char = '_' then
            Token = error("underscore following decimal point"),
            linestr_get_context(LineContext0, Context)
        else
            !:LineContext = LineContextBeforeDot,
            !:LinePosn = LinePosnBeforeDot,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_string(String, LinePosn0, !.LinePosn,
                    NumberString),
                conv_string_to_int(NumberString, base_10, signed, size_word,
                    Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated decimal literal")
            ),
            linestr_get_context(LineContext0, Context)
        )
    else
        !:LineContext = LineContextBeforeDot,
        !:LinePosn = LinePosnBeforeDot,
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_string(String, LinePosn0, !.LinePosn, NumberString),
            conv_string_to_int(NumberString, base_10, signed, size_word,
                Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated decimal literal")
        ),
        linestr_get_context(LineContext0, Context)
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

:- pred string_get_float_decimals(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_decimals(String, Len, !.LastDigit, Posn0,
        Token, Context, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_float_decimals(String, Len, !.LastDigit, Posn0,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                string_get_float_decimals(String, Len, !.LastDigit, Posn0,
                    Token, Context, !Posn)
            )
        else if ( Char = 'e' ; Char = 'E' ) then
            string_get_float_exponent(String, Len, Posn0,
                Token, Context, !Posn)
        else
            !:Posn = LastPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_float_string(String, Posn0, !.Posn, FloatString),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token =
                    error("fractional part of float terminated by underscore")
            ),
            string_get_context(Posn0, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            grab_float_string(String, Posn0, !.Posn, FloatString),
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("fractional part of float terminated by underscore")
        ),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_float_decimals(string::in, int::in,
    last_digit_is_underscore::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_float_decimals(String, Len, !.LastDigit, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_float_decimals(String, Len, !.LastDigit,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_float_decimals(String, Len, !.LastDigit,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if ( Char = 'e' ; Char = 'E' ) then
            linestr_get_float_exponent(String, Len, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_float_string(String, LinePosn0, !.LinePosn,
                    FloatString),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token =
                    error("fractional part of float terminated by underscore")
            ),
            linestr_get_context(LineContext0, Context)
        )
    else
        (
            !.LastDigit = last_digit_is_not_underscore,
            linestr_grab_float_string(String, LinePosn0, !.LinePosn,
                FloatString),
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("fractional part of float terminated by underscore")
        ),
        linestr_get_context(LineContext0, Context)
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
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if ( Char = ('+') ; Char = ('-') ) then
            string_get_float_exponent_2(String, Len, Posn0,
                Token, Context, !Posn)
        else if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_float_exponent_3(String, Len, LastDigit, Posn0,
                Token, Context, !Posn)
        else
            !:Posn = LastPosn,
            Token = error("unterminated exponent in float literal"),
            string_get_context(Posn0, Context)
        )
    else
        grab_float_string(String, Posn0, !.Posn, FloatString),
        conv_to_float(FloatString, Token),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_float_exponent(string::in, int::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_float_exponent(String, Len, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if ( Char = ('+') ; Char = ('-') ) then
            linestr_get_float_exponent_2(String, Len, LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            linestr_get_float_exponent_3(String, Len, LastDigit,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = error("unterminated exponent in float literal"),
            linestr_get_context(LineContext0, Context)
        )
    else
        linestr_grab_float_string(String, LinePosn0, !.LinePosn, FloatString),
        conv_to_float(FloatString, Token),
        linestr_get_context(LineContext0, Context)
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

:- pred string_get_float_exponent_2(string::in, int::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent_2(String, Len, Posn0, Token, Context, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            string_get_float_exponent_3(String, Len, LastDigit, Posn0,
                Token, Context, !Posn)
        else
            !:Posn = LastPosn,
            Token = error("unterminated exponent in float literal"),
            string_get_context(Posn0, Context)
        )
    else
        Token = error("unterminated exponent in float literal"),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_float_exponent_2(string::in, int::in,
    line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_float_exponent_2(String, Len, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            LastDigit = last_digit_is_not_underscore,
            linestr_get_float_exponent_3(String, Len, LastDigit,
                LineContext0, LinePosn0,
                Token, Context, !LineContext, !LinePosn)
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            Token = error("unterminated exponent in float literal"),
            linestr_get_context(LineContext0, Context)
        )
    else
        Token = error("unterminated exponent in float literal"),
        linestr_get_context(LineContext0, Context)
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

:- pred string_get_float_exponent_3(string::in, int::in,
    last_digit_is_underscore::in, posn::in,
    token::out, string_token_context::out, posn::in, posn::out) is det.

string_get_float_exponent_3(String, Len, !.LastDigit, Posn0,
        Token, Context, !Posn) :-
    LastPosn = !.Posn,
    ( if string_read_char(String, Len, Char, !Posn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                string_get_float_exponent_3(String, Len, !.LastDigit, Posn0,
                    Token, Context, !Posn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                string_get_float_exponent_3(String, Len, !.LastDigit, Posn0,
                    Token, Context, !Posn)
            )
        else
            !:Posn = LastPosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                grab_float_string(String, Posn0, !.Posn, FloatString),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated exponent in float literal")
            ),
            string_get_context(Posn0, Context)
        )
    else
        grab_float_string(String, Posn0, !.Posn, FloatString),
        (
            !.LastDigit = last_digit_is_not_underscore,
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated exponent in float literal")
        ),
        string_get_context(Posn0, Context)
    ).

:- pred linestr_get_float_exponent_3(string::in, int::in,
    last_digit_is_underscore::in, line_context::in, line_posn::in,
    token::out, string_token_context::out,
    line_context::in, line_context::out, line_posn::in, line_posn::out) is det.

linestr_get_float_exponent_3(String, Len, !.LastDigit, LineContext0, LinePosn0,
        Token, Context, !LineContext, !LinePosn) :-
    LastLineContext = !.LineContext,
    LastLinePosn = !.LinePosn,
    ( if linestr_read_char(String, Len, Char, !LineContext, !LinePosn) then
        ( if char.is_digit(Char) then
            !:LastDigit = last_digit_is_not_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_float_exponent_3(String, Len, !.LastDigit,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else if Char = '_' then
            !:LastDigit = last_digit_is_underscore,
            disable_warning [suspicious_recursion] (
                linestr_get_float_exponent_3(String, Len, !.LastDigit,
                    LineContext0, LinePosn0,
                    Token, Context, !LineContext, !LinePosn)
            )
        else
            !:LineContext = LastLineContext,
            !:LinePosn = LastLinePosn,
            (
                !.LastDigit = last_digit_is_not_underscore,
                linestr_grab_float_string(String, LinePosn0, !.LinePosn,
                    FloatString),
                conv_to_float(FloatString, Token)
            ;
                !.LastDigit = last_digit_is_underscore,
                Token = error("unterminated exponent in float literal")
            ),
            linestr_get_context(LineContext0, Context)
        )
    else
        linestr_grab_float_string(String, LinePosn0, !.LinePosn, FloatString),
        (
            !.LastDigit = last_digit_is_not_underscore,
            conv_to_float(FloatString, Token)
        ;
            !.LastDigit = last_digit_is_underscore,
            Token = error("unterminated exponent in float literal")
        ),
        linestr_get_context(LineContext0, Context)
    ).

%---------------------------------------------------------------------------%
%
% Utility routines.
%

:- pred rev_char_list_to_int(list(char)::in, integer_base::in,
    signedness::in, integer_size::in, token::out)
    is det.

rev_char_list_to_int(RevChars, Base, Signedness, Size, Token) :-
    ( if rev_char_list_to_string(RevChars, String) then
        conv_string_to_int(String, Base, Signedness, Size, Token)
    else
        Token = error("invalid character in int")
    ).

:- pred conv_string_to_int(string::in, integer_base::in, signedness::in,
    integer_size::in, token::out) is det.

conv_string_to_int(String, Base, Signedness, Size, Token) :-
    BaseInt = integer_base_int(Base),
    ( if integer.from_base_string_underscore(BaseInt, String, Integer) then
        Token = integer(Base, Integer, Signedness, Size)
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
        Token = integer(Base, Integer, _Signedness, _Size),
        base_to_int_and_prefix(Base, BaseInt, Prefix),
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
        Token = integer_dot(Integer),
        IntString = integer.to_string(Integer),
        string.append_list(["integer `", IntString, "'."], String)
    ).

:- pred base_to_int_and_prefix(integer_base::in, int::out, string::out)
    is det.

base_to_int_and_prefix(base_2, 2, "0b").
base_to_int_and_prefix(base_8, 8, "0o").
base_to_int_and_prefix(base_10, 10, "").
base_to_int_and_prefix(base_16, 16, "0x").

%---------------------------------------------------------------------------%

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
