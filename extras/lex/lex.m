%----------------------------------------------------------------------------- %
% lex.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Sun Aug 20 09:08:46 BST 2000
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% This module puts everything together, compiling a list of lexemes
% into state machines and turning the input stream into a token stream.
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%----------------------------------------------------------------------------- %

:- module lex.

:- interface.

:- import_module std_util, string, char, list, io.



:- type annotated_lexeme(Token)
    ==      lexeme(annotated_token(Token)).

:- type lexeme(Token)
    --->    lexeme(
                lxm_token               :: Token,
                lxm_regexp              :: regexp
            ).

:- type annotated_token(Token)
    --->    noval(Token)                % Just return ok(Token) on success.
    ;       value(Token)                % Return ok(Token, String) on success.
    ;       ignore.                     % Just skip over these tokens.

:- type lexer(Token, Source).
:- inst lexer
    ==      bound(lexer(ground, read_pred)).

:- type lexer_state(Token, Source).

:- type lexer_result(Token)
    --->    ok(Token)                   % Noval token matched.
    ;       ok(Token, string)           % Value token matched.
    ;       eof                         % End of input.
    ;       error(int).                 % No matches for string at this offset.



:- type offset
    ==      int.                        % Byte offset into the source data.

    % Any errors should be reported by raising an exception.
    %
:- type read_result
    --->    ok(char)
    ;       eof.

    % read_pred(Offset, Result, SrcIn, SrcOut) reads the char at
    % Offset from SrcIn and returns SrcOut.
    %
:- type read_pred(T)
    ==      pred(offset, read_result, T, T).
:- inst read_pred
    ==      ( pred(in, out, di, uo) is det ).



    % The type of regular expressions.
    %
:- type regexp
    --->    null                % The empty regexp
    ;       atom(char)          % Match a single char
    ;       (regexp >> regexp)  % Concatenation
    ;       (regexp \/ regexp)  % Alternation
    ;       star(regexp)        % Kleene closure
    .



    % Some basic non-primitive regexps.
    %
:- func str(string) = regexp.   % str("abc") = atom(a) >> atom(b) >> atom(c)
:- func any(string) = regexp.   % any("abc") = atom(a) \/ atom(b) \/ atom(c)
:- func anybut(string) = regexp.% anybut("abc") is complement of any("abc")
:- func opt(regexp) = regexp.   % opt(R)     = R \/ null
:- func plus(regexp) = regexp.  % plus(R)    = R \/ star(R)

    % Some useful single-char regexps.
    %
:- func digit = regexp.         % digit      = any("0123456789")
:- func lower = regexp.         % lower      = any("abc...z")
:- func upper = regexp.         % upper      = any("ABC...Z")
:- func alpha = regexp.         % alpha      = lower \/ upper
:- func alphanum = regexp.      % alphanum   = alpha \/ digit
:- func identstart = regexp.    % identstart = alpha \/ str("_")
:- func ident = regexp.         % ident      = alphanum \/ str("_")
:- func nl = regexp.            % nl         = str("\n")
:- func tab = regexp.           % tab        = str("\t")
:- func spc = regexp.           % spc        = str(" ")
:- func wspc = regexp.          % wspc       = any(" \t\n\r\f\v")
:- func dot = regexp.           % dot        = any("<except \n>")

    % Some useful compound regexps.
    %
:- func nat = regexp.           % nat        = plus(digit)
:- func signed_int = regexp.    % signed_int = opt(any("+-")) >> nat
:- func real = regexp.          % real       = \d+((.\d+([eE]int)?)|[eE]int)
:- func identifier = regexp.    % identifier = identstart >> star(ident)
:- func whitespace = regexp.    % whitespace = star(wspc)
:- func junk = regexp.          % junk       = star(dot)



    % Construct a lexer from which we can generate running
    % instances.
    %
:- func init(list(annotated_lexeme(Tok)), read_pred(Src)) = lexer(Tok, Src).
:- mode init(in, in(read_pred)) = out(lexer) is det.

    % Handy read predicates.
    %
:- pred read_from_stdin(offset, read_result, io__state, io__state).
:- mode read_from_stdin(in, out, di, uo) is det.

:- pred read_from_string(offset, read_result, string, string).
:- mode read_from_string(in, out, in, out) is det.

    % Generate a running instance of a lexer on some input source.
    % Note that you can't get the input source back until you stop
    % lexing.
    %
:- func start(lexer(Tok, Src), Src) = lexer_state(Tok, Src).
:- mode start(in(lexer), di) = uo is det.

:- pred read(lexer_result(Tok), lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode read(out, di, uo) is det.

    % Stop a running instance of a lexer and retrieve the input source.
    %
:- func stop(lexer_state(_Tok, Src)) = Src.
:- mode stop(di) = uo is det.

    % Sometimes (e.g. when lexing the io__state) you want access to the
    % input stream without interrupting the lexing process.  This pred
    % provides that sort of access.
    %
:- pred manipulate_source(pred(Src, Src),
                lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode manipulate_source(pred(di, uo) is det, di, uo) is det.

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %

:- implementation.

:- include_module lex__automata.
:- include_module lex__buf.
:- include_module lex__convert_NFA_to_DFA.
:- include_module lex__lexeme.
:- include_module lex__regexp.

:- import_module map, char, bool, int, exception, array.
:- import_module lex__regexp, lex__automata, lex__convert_NFA_to_DFA.
:- import_module lex__lexeme, lex__buf.



:- type lexer(Token, Source)
    --->    lexer(
                lex_compiled_lexemes    :: list(live_lexeme(Token)),
                lex_buf_read_pred       :: read_pred(Source)
            ).



:- type lexer_instance(Token, Source)
    --->    lexer_instance(
                lexi_init_lexemes       :: list(live_lexeme(Token)),
                lexi_live_lexemes       :: list(live_lexeme(Token)),
                lexi_current_winner     :: winner(Token),
                lexi_buf_state          :: buf_state(Source)
            ).
:- inst lexer_instance
    ==      bound(lexer_instance(ground, ground, ground, buf_state)).



:- type live_lexeme(Token)
    ==      compiled_lexeme(annotated_token(Token)).



:- type winner(Token)
    ==      maybe(pair(annotated_token(Token), offset)).

%----------------------------------------------------------------------------- %

init(Lexemes, BufReadPred) = lexer(CompiledLexemes, BufReadPred) :-
    CompiledLexemes = list__map(lexeme__compile_lexeme, Lexemes).

%----------------------------------------------------------------------------- %

start(Lexer, Src) = LexerState :-
    init_lexer_instance(Lexer, Instance, Buf),
    LexerState = args_lexer_state(Instance, Buf, Src).

%----------------------------------------------------------------------------- %

:- pred init_lexer_instance(lexer(Tok, Src), lexer_instance(Tok, Src), buf).
:- mode init_lexer_instance(in(lexer), out(lexer_instance), array_uo) is det.

init_lexer_instance(Lexer, Instance, Buf) :-
    buf__init(Lexer ^ lex_buf_read_pred, BufState, Buf),
    InitLexemes = Lexer ^ lex_compiled_lexemes,
    Instance    = lexer_instance(InitLexemes, InitLexemes, no, BufState).

%----------------------------------------------------------------------------- %

stop(LexerState) = Src :-
    lexer_state_args(LexerState, _Instance, _Buf, Src).

%----------------------------------------------------------------------------- %

read(Result, LexerState0, LexerState) :-
    lexer_state_args(LexerState0, Instance0, Buf0, Src0),
    read_0(Result, Instance0, Instance, Buf0, Buf, Src0, Src),
    LexerState = args_lexer_state(Instance, Buf, Src).



:- pred read_0(lexer_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf, buf, Src, Src).
:- mode read_0(out,
            in(lexer_instance), out(lexer_instance),
            array_di, array_uo, di, uo) is det.

    % Basically, just read chars from the buf and advance the live lexemes
    % until we have a winner or hit an error (no parse).
    %
read_0(Result, Instance0, Instance, Buf0, Buf, Src0, Src) :-

    BufState0    = Instance0 ^ lexi_buf_state,

    buf__read(BufReadResult, BufState0, BufState1, Buf0, Buf1, Src0, Src1),
    (
        BufReadResult = ok(Char),
        process_char(Result, Char,
                Instance0, Instance, BufState1, Buf1, Buf, Src1, Src)
    ;
        BufReadResult = eof,
        Buf = Buf1,
        Src = Src1,
        process_eof(Result, Instance0, Instance, BufState1, Buf)
    ).

%----------------------------------------------------------------------------- %

:- pred process_char(lexer_result(Tok), char,
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf, buf, Src, Src).
:- mode process_char(out, in, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_char(Result, Char, Instance0, Instance,
        BufState, Buf0, Buf, Src0, Src) :-

    LiveLexemes0 = Instance0 ^ lexi_live_lexemes,
    Winner0      = Instance0 ^ lexi_current_winner,

    advance_live_lexemes(Char, buf__cursor_offset(BufState),
            LiveLexemes0, LiveLexemes, Winner0, Winner),
    (
        LiveLexemes = [],               % Nothing left to consider.

        process_any_winner(Result, Winner, Instance0, Instance, BufState,
                Buf0, Buf, Src0, Src)
    ;
        LiveLexemes = [_ | _],          % Still some open possibilities.

        Instance1 = (((Instance0
                            ^ lexi_live_lexemes   := LiveLexemes)
                            ^ lexi_current_winner := Winner)
                            ^ lexi_buf_state      := BufState),
        read_0(Result, Instance1, Instance, Buf0, Buf, Src0, Src)
    ).

%----------------------------------------------------------------------------- %

:- pred process_any_winner(lexer_result(Tok), winner(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src), 
            buf_state(Src), buf, buf, Src, Src).
:- mode process_any_winner(out, in,
            in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_any_winner(Result, yes(ATok - Offset), Instance0, Instance,
        BufState0, Buf0, Buf, Src0, Src) :-

    BufState1 = buf__rewind_cursor(Offset, BufState0),
    Instance1 = ((( Instance0
                        ^ lexi_live_lexemes   := Instance0 ^ lexi_init_lexemes)
                        ^ lexi_current_winner := no)
                        ^ lexi_buf_state      := buf__commit(BufState1)),
    (
        ATok     = noval(Token),
        Result   = ok(Token),
        Instance = Instance1,
        Buf      = Buf0,
        Src      = Src0
    ;
        ATok     = value(Token),
        Result   = ok(Token, buf__string_to_cursor(BufState1, Buf)),
        Instance = Instance1,
        Buf      = Buf0,
        Src      = Src0
    ;
        ATok     = ignore,
        read_0(Result, Instance1, Instance, Buf0, Buf, Src0, Src)
    ).

process_any_winner(Result, no, Instance0, Instance,
        BufState0, Buf, Buf, Src, Src) :-

    Start     = buf__start_offset(BufState0),
    BufState1 = buf__rewind_cursor(Start + 1, BufState0),
    Result    = error(Start),
    Instance  = ((( Instance0
                        ^ lexi_live_lexemes   :=
                                Instance0 ^ lexi_init_lexemes)
                        ^ lexi_current_winner := no)
                        ^ lexi_buf_state      := buf__commit(BufState1)).

%----------------------------------------------------------------------------- %

:- pred process_eof(lexer_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf).
:- mode process_eof(out, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_ui) is det.

process_eof(Result, Instance0, Instance, BufState, Buf) :-

    ( if

        live_lexeme_in_accepting_state(Instance0 ^ lexi_live_lexemes, ATok)

      then

            % Return the token and set things up so that we return
            % eof next.
        (
            ATok   = noval(Token),
            Result = ok(Token)
        ;
            ATok   = value(Token),
            Result = ok(Token, buf__string_to_cursor(BufState, Buf))
        ;
            ATok   = ignore,
            Result = eof
        )

      else

        Result     = eof
    ),
    Instance  = ((Instance0
                        ^ lexi_live_lexemes := [])
                        ^ lexi_buf_state    := buf__commit(BufState)).

%----------------------------------------------------------------------------- %

:- pred advance_live_lexemes(char, offset,
            list(live_lexeme(Token)), list(live_lexeme(Token)),
            winner(Token), winner(Token)).
:- mode advance_live_lexemes(in, in, in, out, in, out) is det.

advance_live_lexemes(_Char, _Offset, [], [], Winner, Winner).

advance_live_lexemes(Char, Offset, [L0 | Ls0], Ls, Winner0, Winner) :-

    State0        = L0 ^ clxm_state,
    ATok          = L0 ^ clxm_token,

    ( if next_state(L0, State0, Char, State, IsAccepting) then

        (
            IsAccepting = no,
            Winner1     = Winner0
        ;
            IsAccepting = yes,
            Winner1     = yes(ATok - Offset)
        ),
        advance_live_lexemes(Char, Offset, Ls0, Ls1, Winner1, Winner),
        Ls = [( L0 ^ clxm_state := State ) | Ls1]

      else

        advance_live_lexemes(Char, Offset, Ls0, Ls, Winner0, Winner)
    ).

%----------------------------------------------------------------------------- %

:- pred live_lexeme_in_accepting_state(list(live_lexeme(Tok)),
                annotated_token(Tok)).
:- mode live_lexeme_in_accepting_state(in, out) is semidet.

live_lexeme_in_accepting_state([L | Ls], Token) :-
    ( if in_accepting_state(L)
      then Token = L ^ clxm_token
      else live_lexeme_in_accepting_state(Ls, Token)
    ).

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %

    % It's much more convenient (especially for integration with, e.g.
    % parsers such as moose) to package up the lexer_instance, buf
    % and Src in a single object.

:- type lexer_state(Tok, Src)
    --->    lexer_state(
                lxr_instance            :: lexer_instance(Tok, Src),
                lxr_buf                 :: buf,
                lxr_src                 :: Src
            ).

%------------------------------------------------------------------------------%

:- func args_lexer_state(lexer_instance(Tok, Src), buf, Src) =
            lexer_state(Tok, Src).
:- mode args_lexer_state(in(lexer_instance), array_di, di) = uo is det.

args_lexer_state(Instance, Buf, Src) = LexerState :-
    unsafe_promise_unique(lexer_state(Instance, Buf, Src), LexerState).

%----------------------------------------------------------------------------- %

:- pred lexer_state_args(lexer_state(Tok,Src),lexer_instance(Tok,Src),buf,Src).
:- mode lexer_state_args(di, out(lexer_instance), array_uo, uo)  is det.

lexer_state_args(lexer_state(Instance, Buf0, Src0), Instance, Buf, Src) :-
    unsafe_promise_unique(Buf0, Buf),
    unsafe_promise_unique(Src0, Src).

%----------------------------------------------------------------------------- %

manipulate_source(P, State0, State) :-
    lexer_state_args(State0, Instance, Buf, Src0),
    P(Src0, Src),
    State = args_lexer_state(Instance, Buf, Src).

% ---------------------------------------------------------------------------- %

read_from_stdin(_Offset, Result) -->
    io__read_char(IOResult),
    { IOResult = ok(Char),              Result = ok(Char)
    ; IOResult = eof,                   Result = eof
    ; IOResult = error(_E),             throw(IOResult)
    }.

% ---------------------------------------------------------------------------- %

read_from_string(Offset, Result, String, String) :-
    ( if Offset < string__length(String) then
        Result = ok(string__unsafe_index(String, Offset))
      else
        Result = eof
    ).

%----------------------------------------------------------------------------- %
% Some basic non-primitive regexps.

str(S) = R :-
    ( if S = "" then
        R = null
      else
        L = string__length(S),
        C = string__index_det(S, L - 1),
        R = str_foldr(func(Cx, Rx) = (atom(Cx) >> Rx), S, atom(C), L - 2)
    ).

any(S) = R :-
    ( if S = "" then
        R = null
      else
        L = string__length(S),
        C = string__index_det(S, L - 1),
        R = str_foldr(func(Cx, Rx) = (atom(Cx) \/ Rx), S, atom(C), L - 2)
    ).

anybut(S0) = R :-
    S = string__from_char_list(
            list__filter_map(
                ( func(X) = C is semidet :-
                    char__to_int(C, X),
                    not string__contains_char(S0, C)
                ),
                0x01 `..` 0xff
            )
        ),
    R = any(S).

:- func str_foldr(func(char, T) = T, string, T, int) = T.

str_foldr(Fn, S, X, I) =
    ( if I < 0 then X
               else str_foldr(Fn, S, Fn(string__index_det(S, I), X), I - 1)
    ).

opt(R)  = (R \/ null).

plus(R) = (R >> star(R)).

%----------------------------------------------------------------------------- %
% Some useful single-char regexps.

    % We invite the compiler to memo the values of these constants that
    % (a) are likely to be quite common in practice and (b) take *some*
    % time to compute.
    %
:- pragma memo(digit/0).
:- pragma memo(lower/0).
:- pragma memo(upper/0).
:- pragma memo(wspc/0).
:- pragma memo(dot/0).

digit      = any("0123456789").
lower      = any("abcdefghijklmnopqrstuvwxyz").
upper      = any("ABCDEFGHIJKLMNOPQRSTUVWXYZ").
wspc       = any(" \t\n\r\f\v").
dot        = anybut("\n").
alpha      = (lower \/ upper).
alphanum   = (alpha \/ digit).
identstart = (alpha \/ atom('_')).
ident      = (alphanum \/ atom('_')).
nl         = atom('\n').
tab        = atom('\t').
spc        = atom(' ').

%----------------------------------------------------------------------------- %
% Some useful compound regexps.

nat        = plus(digit).
signed_int = (opt(any("+-")) >> nat).
real       = (signed_int >> (
                (atom('.') >> nat >> opt(any("eE") >> signed_int)) \/
                (any("eE") >> signed_int)
             )).
identifier = (identstart >> star(ident)).
whitespace = star(wspc).
junk       = star(dot).

%----------------------------------------------------------------------------- %
%----------------------------------------------------------------------------- %
