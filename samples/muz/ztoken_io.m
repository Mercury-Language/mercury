%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: token_io.m
% main author: philip

:- module ztoken_io.
:- interface.

:- import_module io.
:- import_module word.
:- import_module ztoken.

:- type ztoken_result
    --->    eof
    ;       error(string)
    ;       ok(ztoken_list).

:- pred readTokenList(operators::in, ztoken_result::out,
    io::di, io::uo) is det.

:- pred writeTokenList(ztoken_list::in, io::di, io::uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- implementation.

:- import_module char.
:- import_module higher_order.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

% :- pred main(io::di, io::uo) is det.
% main --> readTokenList(TS), writeTokenList(TS).

% :- pred lerror(list(string)::in, io::di, io::uo) is det.
%
% lerror(L, !IO) :-
%   io.input_stream_name(F, !IO),
%   io.get_line_number(LN, !IO),
%   string.int_to_string(LN, LNS),
%   io.write_strings([F, "(", LNS, ")", ":"| L], !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Global state (and predicates for operating on it)
%

:- type lex_state
    --->    in_Z
    ;       out_of_Z.

:- type globals
    --->    globals(
                ztoken_list,
                maybe(pair(ztoken_list, lex_state)),
                operators
            ).

:- mutable(global_var, globals, globals([], no, map.init), ground,
    [untrailed, attach_to_io_state]).

:- pred get_globals(globals::out, io::di, io::uo) is det.

get_globals(G, !IO) :-
    get_global_var(G, !IO).

:- pred init_globals(operators::in, io::di, io::uo) is det.

init_globals(O, !IO) :-
    set_global_var(globals([], no, O), !IO).

:- pred set_globals(globals::in, io::di, io::uo) is det.

set_globals(G, !IO) :-
    set_global_var(G, !IO).

% BUG: add_token uses the wrong line number (+1) if the token ends at
% end-of-line because the input is on the next line when add_token is called.
:- pred add_token(ztoken::in, io::di, io::uo) is det.

add_token(T, !IO) :-
    io.get_line_number(LN, !IO),
    get_globals(globals(TL, P, O), !IO),
    set_globals(globals([T - LN | TL], P, O), !IO).

:- pred get_operat_state(operators::out, io::di, io::uo) is det.

get_operat_state(O, !IO) :-
    get_globals(globals(_, _, O), !IO).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readTokenList(Ops, Result, !IO) :-
    init_globals(Ops, !IO),
    io.read_char(C, !IO),
    ztokens(C, Result, !IO).
    % get_globals(G, !IO), io.write(G, !IO), io.write_string("\n", !IO),

:- pred finished(char::in, ztoken_result::out, io::di, io::uo) is det.

finished(C, ok(TL), !IO) :-
    io.putback_char(C, !IO),
    get_globals(globals(TL0, _, _), !IO),
    reverse_removing_soft_newlines(TL0, TL).

% The \verb and \verb* commands and the verbatim and verbatim* environments
% should be added.
% Lines with spaces only indicate end-of-paragraph

:- type tstate
    --->    in_block
    ;       in_pragma.

:- pred ztokens(io.result(char)::in, ztoken_result::out,
    io::di, io::uo) is det.

ztokens(eof, eof, !IO).
ztokens(error(M), error(S), !IO) :-
    io.error_message(M, S).
ztokens(ok(C), Result, !IO) :-
    ( if C = '\n' then
        check_start_pragma(C1, YesOrNo, !IO),        % FIX THIS
        (
            YesOrNo = yes,
            in_ztokens(in_pragma, C1, Result, !IO)
        ;
            YesOrNo = no,
            ztokens(C1, Result, !IO)
        )
    else if C = '%' then
        comment(C1, !IO),
        ztokens(C1, Result, !IO)
    else if C = ('\\') then
        next_lex("begin", F, C2, !IO),
        (
            F = was_found,
            latex_arg("begin", C2, Arg0, C1, !IO),
            string.from_char_list(Arg0, Arg),
            ( if start_of_Z(Arg, T) then
                add_token(T, !IO),
                in_ztokens(in_block, C1, Result, !IO)
            else
                ztokens(C1, Result, !IO)
            )
        ;
            F = not_found,
            ztokens(C2, Result, !IO)
        )
    else
        io.read_char(C1, !IO),
        ztokens(C1, Result, !IO)
    ).

:- pred in_ztokens(tstate::in, io.result(char)::in, ztoken_result::out,
    io::di, io::uo) is det.

in_ztokens(_, eof, error("End of file before end of paragraph"), !IO).
in_ztokens(_, error(M), error(S), !IO) :-
    io.error_message(M, S).
in_ztokens(State, ok(C), Result, !IO) :-
    ( if C = '\n' then
        (
            State = in_block,
            io.read_char(C1, !IO),
            in_ztokens(State, C1, Result, !IO)
        ;
            State = in_pragma,
            add_token(zEND, !IO),
            finished('\n', Result, !IO)
        )
    else if C = '%' then
        comment(C1, !IO),
        in_ztokens(State, C1, Result, !IO)
    else if ztoken_io.is_whitespace(C) then
        io.read_char(C1, !IO),
        in_ztokens(State, C1, Result, !IO)
    else if C = ('\\') then
        escape(MT, C1, !IO),
        (
            MT = no,
            in_ztokens(State, C1, Result, !IO)
        ;
            MT = yes(T),
            add_token(T, !IO),
            ( if C1 = ok(Char1), T = zEND then
                finished(Char1, Result, !IO)
            else
                in_ztokens(State, C1, Result, !IO)
            )
        )
    else if char.is_alpha(C) then
        word([C], T, C1, !IO),
        add_token(T, !IO),
        in_ztokens(State, C1, Result, !IO)
    else if char.is_digit(C) then
        digits(L, C1, !IO),
        string.from_char_list([C | L], S),
        add_token(number(S), !IO),
        in_ztokens(State, C1, Result, !IO)
    else if is_predecoration(C) then
        decoration(C, C1, [], D, !IO),
        add_token(decoration(D), !IO),
        in_ztokens(State, C1, Result, !IO)
    else if char_switch(C, T) then
        io.read_char(C1, !IO),
        add_token(T, !IO),
        in_ztokens(State, C1, Result, !IO)
    else if symbol(C) then
        symbol(L, C1, !IO),
        string.from_char_list([C | L], S),
        special_word(S, [], T, !IO),
        add_token(T, !IO),
        in_ztokens(State, C1, Result, !IO)
    else
        char.to_int(C, Code),
        string.int_to_base_string(Code, 10, Decimal),
        string.int_to_base_string(Code, 16, Hex),
        string.append_list([
            "Illegal input character 0x", Hex, " (", Decimal, ")"],
            Error),
        Result = error(Error)
    ).

:- pred is_predecoration(char::in) is semidet.

is_predecoration('!').
is_predecoration('?').
is_predecoration('''').
is_predecoration('_').

:- pred decoration(io.result(char)::in, io.result(char)::out,
    list(stroke)::out, io::di, io::uo) is det.

decoration(C0, C, L, !IO) :-
    ( if C0 = ok(DC), is_predecoration(DC) then
        decoration(DC, C, [], L, !IO)
    else
        C = C0,
        L = []
    ).

:- pred decoration(char::in, io.result(char)::out,
    list(stroke)::in, list(stroke)::out, io::di, io::uo) is det.

decoration(DC, C, L0, L, !IO) :-
    ( if DC = ('!') then
        M = no,
        S = exclamation_mark
    else if DC = '?' then
        M = no,
        S = question_mark
    else if DC = '''' then
        M = no,
        S = prime
    else if DC = '_' then
        io.read_char(C0, !IO),
        ( if C0 = ok(C1) then %, char.is_alnum(C1) then
            M = no,
            string.char_to_string(C1, S1)
        else
            M = yes(C0),
            S1 = ""
        ),
        S = subscript(S1)
    else
        error("decoration/6---impossible decoration character")
    ),
    (
        M = no,
        io.read_char(C2, !IO)
    ;
        M = yes(C2)
    ),
    L1 = [S | L0],
    ( if C2 = ok(C3), is_predecoration(C3) then
        decoration(C3, C, L1, L, !IO)
    else
        C = C2,
        L = L1
    ).

:- pred is_whitespace(char::in) is semidet.

is_whitespace(' ').
is_whitespace('~').
is_whitespace('&').
is_whitespace('\t').
is_whitespace('\r').
is_whitespace('\f').
is_whitespace('\v').

:- type yes_or_no
    --->    yes
    ;       no.

:- pred check_start_pragma(io.result(char)::out, yes_or_no::out,
    io::di, io::uo) is det.

check_start_pragma(C, Result, !IO) :-
    io.read_char(C0, !IO),
    ( if C0 = ok('%') then
        io.read_char(C1, !IO),
        ( if C1 = ok(Char1) then
            ( if Char1 = '\n' then
                C = C1, Result = no % need to see newline
            else if Char1 = '%' then
                alpha(L, C2, !IO),
                % ( if L = [], C2 = ok(Char2) then
                %   ( if Char2 = ' ';Char2 = '\t' then
                %       io.read_char(C),
                %       Result = yes
                %   else
                %       comment(C),
                %       Result = no
                %   )
                ( if string.from_char_list(L, S), pragma(S, P) then
                    add_token(pragma(P), !IO),
                    C = C2,
                    Result = yes
                else
                    comment(C, !IO),
                    Result = no
                )
            else
                comment(C, !IO),
                Result = no
            )
        else
            C = C1,
            Result = no
        )
    else
        C = C0,
        Result = no
    ).

:- pred comment(io.result(char)::out, io::di, io::uo) is det.

comment(Char, !IO) :-
    io.read_line(L0, !IO),
    (
        L0 = error(I),
        Char = error(I)
    ;
        L0 = eof,
        Char = eof
    ;
        L0 = ok(L1),
        ( if list.remove_suffix(L1, ['\n'], _) then
            Char = ok('\n')
        else
            Char = eof
        )
    ).

:- pred word(list(char)::in, ztoken::out, io.result(char)::out,
    io::di, io::uo) is det.

word(L, T, R, !IO) :-
    io.read_char(R0, !IO),
    ( if R0 = ok(C0), C0 = ('\\') then
        io.read_char(R1, !IO),
        ( if R1 = ok(C1) then
            ( if C1 = '_' then
                word(['_' | L], T, R, !IO)
            else
                io.putback_char(C1, !IO),
                word_chars_to_token(L, [], T, !IO),
                R = R0
            )
        else
            T = newline,
            R = R1 % Arbit. token returned for error
        )
    else if R0 = ok(C0), char.is_alnum(C0) then
        word([C0 | L], T, R, !IO)
    else
        decoration(R0, R, D, !IO),
        word_chars_to_token(L, D, T, !IO)
    ).

:- pred word_chars_to_token(list(char)::in, decoration::in, ztoken::out,
    io::di, io::uo) is det.

word_chars_to_token(L, D, T, !IO) :-
    list.reverse(L, L1),
    string.from_char_list(L1, S),
    special_word(S, D, T, !IO).

:- pred digits(list(char)::out, io.result(char)::out, io::di, io::uo) is det.

digits(L, C1, !IO) :-
    io.read_char(C0, !IO),
    ( if C0 = ok(C), char.is_digit(C) then
        L = [C | L1],
        digits(L1, C1, !IO)
    else
        L = [],
        C1 = C0
    ).

:- pred special_word(string::in, decoration::in, ztoken::out,
    io::di, io::uo) is det.

special_word(S, D, T, !IO) :-
    I = id(no, S, D),
    ( if I = id(no, "\\also", []) then % special case because
        T = newline       % \also is an alias for \\
    else if keyword(_, I, T0) then
        T = T0
    else
        get_operat_state(OM, !IO),
        ( if search_operators(OM, I, Op) then T = op(Op, I) else T = name(I) )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred escape(maybe(ztoken)::out, io.result(char)::out,
    io::di, io::uo) is det.

escape(T, C, !IO) :-
    io.read_char(C1, !IO),
    ( if C1 = ok(Char1) then
        ( if char.is_alpha(Char1) then
            alpha(L, C2, !IO),
            string.from_char_list(['\\', Char1 | L], S),
            ( if S = "\\end" then  % what if \end in pragma??
                % latex_arg(C2, L, C), %should check that this
                % string.from_char_list(L, S), %matches \begin
                T = yes(zEND),
                C = C2
            else if S = "\\quad" then % em sized space
                T = no,
                C = C2
            else if S = "\\qquad" then    % 2 em sized space
                T = no,
                C = C2
            else if S = "\\t" then    % \t is a tab which may be
                T = no,   % followed by a number
                ( if C2 = ok(C3), char.is_digit(C3) then
                    digits(_, C, !IO)
                else
                    C = C2
                )
            else
                % ( if S = "\\inrel" then
                %       latex_arg("inrel", C2, Arg0, C),
                %   string.from_char_list(Arg0, Arg),
                %   T1 = op(inrel, id(no, Arg, []))
                % else
                decoration(C2, C, D, !IO),
                special_word(S, D, T1, !IO),
                T = yes(T1)
                % )
            )
        else
            io.read_char(C, !IO),
            ( if escape_switch(Char1, T0) then
                T = yes(T0)
            else
                string.from_char_list(['\\', Char1], S),
                T = yes(name(id(no, S, [])))
            )
        )
    else
        T = no, C = C1
    ).

:- pred escape_switch(char::in, ztoken::out) is semidet.

escape_switch('\\', newline).
escape_switch('_', underscore).
escape_switch('{', zSETBRA).
escape_switch('}', zSETKET).

:- pred alpha(list(char)::out, io.result(char)::out, io::di, io::uo) is det.

alpha(L, C1, !IO) :-
    io.read_char(C0, !IO),
    ( if C0 = ok(C), char.is_alpha(C) then
        L = [C | L1],
        alpha(L1, C1, !IO)
    else
        L = [], C1 = C0
    ).

:- pred symbol(list(char)::out, io.result(char)::out, io::di, io::uo) is det.

symbol(L, C1, !IO) :-
    io.read_char(C0, !IO),
    ( if C0 = ok(C), symbol(C) then
        L = [C | L1],
        symbol(L1, C1, !IO)
    else
        L = [],
        C1 = C0
    ).

% :- pred alpha1(list(char), io.result(char), io::di, io::uo) is det.
% :- mode alpha1(out, out, di, uo) is det.
%
% alpha1(L, C1, !IO) :-
%   lex_string(char.is_alpha, L, C1, !IO).
%
% :- pred lex_string(pred(char), list(char), io.result(char), io, io).
% :- mode lex_string(pred(in) is semidet, out, out, di, uo) is det.
%
% lex_string(P, L, C1, !IO) :-
%   io.read_char(C0, !IO),
%   ( if C0 = ok(C), call(P, C) then
%       L = [C | L1],
%       lex_string(P, L1, C1, !IO)
%   else
%       L = [],
%       C1 = C0
%   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred start_of_Z(string::in, ztoken::out) is semidet.

start_of_Z("zed", zBEGIN).
start_of_Z("syntax", zBEGIN).
start_of_Z("axdef", zAX).
start_of_Z("schema", zSCH).
start_of_Z("gendef", zGEN).

:- type found
    --->    was_found
    ;       not_found.

:- pred next_lex(string::in, found::out, io.result(char)::out,
    io::di, io::uo) is det.

next_lex(S, F, C, !IO) :-
    io.read_char(C0, !IO),
    ( if string.first_char(S, H, T) then
        ( if C0 = ok(H) then
            next_lex(T, F, C, !IO)
        else
            F = not_found,
            C = C0
        )
    else
        F = was_found,
        C = C0
    ).

:- pred latex_arg(string::in, io.result(char)::in, list(char)::out,
    io.result(char)::out, io::di, io::uo) is det.

latex_arg(S, C0, L, C, !IO) :-
    C1 = C0, % eat_white_space(C0, C1),
    ( if C1 = ok('{') then
        upto_close_brace(S, L, C, !IO)
    else
        L = [],
        C = C1
    ).

:- pred upto_close_brace(string::in, list(char)::out, io.result(char)::out,
    io::di, io::uo) is det.

upto_close_brace(S, L, C1, !IO) :-
    io.read_char(C0, !IO),
    ( if C0 = ok(C) then
        ( if C = '}' then
            L = [],
            io.read_char(C1, !IO)
        else
            L = [C | L1],
            upto_close_brace(S, L1, C1, !IO)
        )
    else
        L = [],
        C1 = C0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred symbol(char::in) is semidet.

symbol(',').
symbol('.').
symbol('/').
symbol(':').
symbol(';').
symbol('=').
symbol('[').
symbol(']').
symbol('|').

symbol('`').
symbol('-').
symbol('*').
symbol('@').
symbol('+').
symbol('>').
symbol('<').

:- pred char_switch(char::in, ztoken::out) is semidet.

char_switch('(', zBRA).
char_switch(')', zKET).
char_switch('{', left_brace).
char_switch('}', right_brace).
char_switch('^', caret).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT

writeTokenList(L, !IO) :-
    SL = string_portray_list(ztokenToString, "[\n", ",\n", "\n]\n", L),
    io.write_string(SL, !IO).

:- func ztokenToString(pair(ztoken, zcontext)) = string.

ztokenToString(T-_) = S :-
    ( if T = name(I) then
        string.append_list(["name('", identPortray(I), "')"], S)
    else if T = op(Op, I) then
        S0 = op_to_string(Op),
        string.append_list(["op(", S0, ", '", identPortray(I), "')"], S)
    else if T = number(S0) then
        string.append_list(["number(\"", S0, "\")"], S)
    else if T = decoration(D) then
        string.append_list(strokeLPortray(D), S0),
        string.append_list(["decoration(\"", S0, "\")"], S)
    else if T = string(S0) then
        string.append_list(["string(\"", S0, "\")"], S)
    else if T = pragma(P) then
        pragma(S0, P), string.append_list(["pragma(\"", S0, "\")"], S)
    else if keyword(b, I, T) then
        S = identPortray(I)
    else
        error("impossible token in ztokenToString")
    ).
