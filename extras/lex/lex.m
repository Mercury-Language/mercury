%------------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% lex.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Sun Aug 20 09:08:46 BST 2000
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
% Thu Jul 26 07:45:47 UTC 2001
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms 
%   of the GNU Lesser General Public License, see the file COPYING.LGPL
%   in this directory.
%
% This module puts everything together, compiling a list of lexemes
% into state machines and turning the input stream into a token stream.
%
%------------------------------------------------------------------------------%

:- module lex.

:- interface.

:- import_module std_util, string, char, list, io.

:- type token_creator(Token)
    ==                        (func(string) = Token).
:- inst token_creator
    ==                        (func(in) = out is det).

:- type lexeme(Token)
    ==                        pair(regexp, token_creator(Token)).

:- inst lexeme(Inst)
    ---> (ground - Inst).

:- type lexer(Token, Source).
:- inst lexer
    ---> lexer(ground, ignore_pred, read_pred).

:- type lexer_state(Token, Source).

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

    % ignore_pred(Token): if it does not fail, Token must be ignored
    %
:- type ignore_pred(Tok)
    ==      pred(Tok).
:- inst ignore_pred
    ==      ( pred(in) is semidet ).

    % The type of regular expressions.
    %
:- type regexp.

    % The typeclass for types having a natural converter to regexp's
    %
:- typeclass regexp(T) where [
           func re(T) = regexp
].

    % Handling regexp's based on the typeclass regexp(T)
    %
:- func  null      = regexp.
:- func  T1 ++ T2  = regexp  <= (regexp(T1), regexp(T2)).
:- func  *(T)      = regexp  <= (regexp(T)).
    % One of the following two functions may be deprecated
    % in future, depending upon whether there's a concensus
    % concerning which is preferable.  Both express
    % alternation.
    %
:- func  T1 \/ T2  = regexp  <= (regexp(T1), regexp(T2)).
:- func (T1 or T2) = regexp  <= (regexp(T1), regexp(T2)).

    % Some instances of typeclass regexp(T)
    %
:- instance regexp(regexp).
:- instance regexp(char).
:- instance regexp(string).

    % Some basic non-primitive regexps.
    %
:- func any(string) = regexp.        % any("abc") = ('a') or ('b') or ('c')
:- func anybut(string) = regexp.     % anybut("abc") is complement of any("abc")
:- func ?(T) = regexp <= regexp(T).  % ?(R)       = R or null
:- func +(T) = regexp <= regexp(T).  % +(R)       = R ++ *(R)

    % Some useful single-char regexps.
    %
:- func digit = regexp.         % digit      = any("0123456789")
:- func lower = regexp.         % lower      = any("abc...z")
:- func upper = regexp.         % upper      = any("ABC...Z")
:- func alpha = regexp.         % alpha      = lower or upper
:- func alphanum = regexp.      % alphanum   = alpha or digit
:- func identstart = regexp.    % identstart = alpha or "_"
:- func ident = regexp.         % ident      = alphanum or "_"
:- func nl = regexp.            % nl         = re("\n")
:- func tab = regexp.           % tab        = re("\t")
:- func spc = regexp.           % spc        = re(" ")
:- func wspc = regexp.          % wspc       = any(" \t\n\r\f\v")
:- func dot = regexp.           % dot        = anybut("\n")

    % Some useful compound regexps.
    %
:- func nat = regexp.           % nat        = +(digit)
:- func signed_int = regexp.    % signed_int = ?("+" or "-") ++ nat
:- func real = regexp.          % real       = \d+((.\d+([eE]int)?)|[eE]int)
:- func identifier = regexp.    % identifier = identstart ++ *(ident)
:- func whitespace = regexp.    % whitespace = *(wspc)
:- func junk = regexp.          % junk       = *(dot)

   % Utility predicate to create ignore_pred's.
   % Use it in the form `ignore(my_token)' to ignore just `my_token'.
   % 
:- pred ignore(Token::in, Token::in) is semidet.

   % Utility function to return noval tokens.
   % Use it in the form `return(my_token) inside a lexeme definition.
   %
:- func return(T,string) = T.

   % Utility operator to create lexemes.
   %
:- func (T1 -> token_creator(Tok)) = pair(regexp,token_creator(Tok))
            <= regexp(T1).

    % Construct a lexer from which we can generate running
    % instances.
    %
    % NOTE: If several lexemes match the same string only
    % the token generated by the one closest to the start
    % of the list of lexemes is returned.
    %
:- func init(list(lexeme(Tok)), read_pred(Src)) = lexer(Tok, Src).
:- mode init(in, in(read_pred)) = out(lexer) is det.

    % Construct a lexer from which we can generate running
    % instances. If we construct a lexer with init/4, we
    % can additionally ignore specific tokens.
    %
    % NOTE: If several lexemes match the same string only
    % the token generated by the one closest to the start
    % of the list of lexemes is returned.
    %
:- func init(list(lexeme(Tok)), read_pred(Src), ignore_pred(Tok)) =
            lexer(Tok, Src).
:- mode init(in, in(read_pred), in(ignore_pred)) = out(lexer) is det.

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

:- pred read(io__read_result(Tok), lexer_state(Tok, Src),
            lexer_state(Tok, Src)).
:- mode read(out, di, uo) is det.

    % Stop a running instance of a lexer and retrieve the input source.
    %
:- func stop(lexer_state(_Tok, Src)) = Src.
:- mode stop(di) = uo is det.

    % Sometimes (e.g. when lexing the io__io) you want access to the
    % input stream without interrupting the lexing process.  This pred
    % provides that sort of access.
    %
:- pred manipulate_source(pred(Src, Src),
                lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode manipulate_source(pred(di, uo) is det, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

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
                lex_ignore_pred         :: ignore_pred(Token),
                lex_buf_read_pred       :: read_pred(Source)
            ).

:- type lexer_instance(Token, Source)
    --->    lexer_instance(
                init_lexemes            :: list(live_lexeme(Token)),
                live_lexemes            :: list(live_lexeme(Token)),
                current_winner          :: winner(Token),
                buf_state               :: buf_state(Source),
                ignore_pred             :: ignore_pred(Token)
            ).

:- inst lexer_instance
    --->    lexer_instance(
                live_lexeme_list, 
                live_lexeme_list, 
                winner, 
                buf__buf_state,
                ignore_pred
            ).

:- type live_lexeme(Token)
    ==      compiled_lexeme(Token).
:- inst live_lexeme
    ==      compiled_lexeme(token_creator).
:- inst live_lexeme_list
    ==      list__list_skel(live_lexeme).



:- type winner(Token)
    ==      maybe(pair(token_creator(Token), offset)).
:- inst winner
    --->    yes(pair(token_creator, ground))
    ;       no.

%------------------------------------------------------------------------------%

ignore(Tok,Tok).

%------------------------------------------------------------------------------%

return(Token, _) = Token.

%------------------------------------------------------------------------------%

(R1 -> TC) = (re(R1) - TC).

%------------------------------------------------------------------------------%

init(Lexemes, BufReadPred) = init(Lexemes, BufReadPred, DontIgnoreAnything) :-
    DontIgnoreAnything = ( pred(_::in) is semidet :- semidet_fail ).

init(Lexemes, BufReadPred, IgnorePred) =
    lexer(CompiledLexemes, IgnorePred, BufReadPred)
 :-
    CompiledLexemes = list__map(compile_lexeme, Lexemes).

%------------------------------------------------------------------------------%

start(Lexer, Src) = LexerState :-
    init_lexer_instance(Lexer, Instance, Buf),
    LexerState = args_lexer_state(Instance, Buf, Src).

%------------------------------------------------------------------------------%

:- pred init_lexer_instance(lexer(Tok, Src), lexer_instance(Tok, Src), buf).
:- mode init_lexer_instance(in(lexer), out(lexer_instance), array_uo) is det.

init_lexer_instance(Lexer, Instance, Buf) :-
    buf__init(Lexer ^ lex_buf_read_pred, BufState, Buf),
    InitLexemes = Lexer ^ lex_compiled_lexemes,
    IgnorePred  = Lexer ^ lex_ignore_pred,
    Instance    = lexer_instance(InitLexemes, InitLexemes, no,
                        BufState, IgnorePred).

%------------------------------------------------------------------------------%

stop(LexerState) = Src :-
    lexer_state_args(LexerState, _Instance, _Buf, Src).

%------------------------------------------------------------------------------%

read(Result, LexerState0, LexerState) :-
    lexer_state_args(LexerState0, Instance0, Buf0, Src0),
    read_0(Result, Instance0, Instance, Buf0, Buf, Src0, Src),
    LexerState = args_lexer_state(Instance, Buf, Src).



:- pred read_0(io__read_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf, buf, Src, Src).
:- mode read_0(out,
            in(lexer_instance), out(lexer_instance),
            array_di, array_uo, di, uo) is det.

    % Basically, just read chars from the buf and advance the live lexemes
    % until we have a winner or hit an error (no parse).
    %
read_0(Result, Instance0, Instance, Buf0, Buf, Src0, Src) :-

    BufState0    = Instance0 ^ buf_state,

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

%------------------------------------------------------------------------------%

:- pred process_char(io__read_result(Tok), char,
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf, buf, Src, Src).
:- mode process_char(out, in, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_char(Result, Char, Instance0, Instance,
        BufState, Buf0, Buf, Src0, Src) :-

    LiveLexemes0 = Instance0 ^ live_lexemes,
    Winner0      = Instance0 ^ current_winner,

    advance_live_lexemes(Char, buf__cursor_offset(BufState),
            LiveLexemes0, LiveLexemes, Winner0, Winner),
    (
        LiveLexemes = [],               % Nothing left to consider.

        process_any_winner(Result, Winner, Instance0, Instance, BufState,
                Buf0, Buf, Src0, Src)
    ;
        LiveLexemes = [_ | _],          % Still some open possibilities.

        Instance1 = (((Instance0
                            ^ live_lexemes   := LiveLexemes)
                            ^ current_winner := Winner)
                            ^ buf_state      := BufState),
        read_0(Result, Instance1, Instance, Buf0, Buf, Src0, Src)
    ).

%------------------------------------------------------------------------------%

:- pred process_any_winner(io__read_result(Tok), winner(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src), 
            buf_state(Src), buf, buf, Src, Src).
:- mode process_any_winner(out, in(winner),
            in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_any_winner(Result, yes(TokenCreator - Offset), Instance0, Instance,
        BufState0, Buf0, Buf, Src0, Src) :-

    BufState1 = buf__rewind_cursor(Offset, BufState0),
    Instance1 = ((( Instance0
                        ^ live_lexemes   := Instance0 ^ init_lexemes)
                        ^ current_winner := no)
                        ^ buf_state      := buf__commit(BufState1)),
    ( if

         get_token_from_buffer(BufState1, Buf0, Instance0, TokenCreator, Token)

      then
    
         Result   = ok(Token),
         Instance = Instance1,
         Buf      = Buf0,
         Src      = Src0
    
      else
    
         read_0(Result, Instance1, Instance, Buf0, Buf, Src0, Src)
    ).

process_any_winner(Result, no, Instance0, Instance,
        BufState0, Buf, Buf, Src, Src) :-

    Start     = buf__start_offset(BufState0),
    BufState1 = buf__rewind_cursor(Start + 1, BufState0),
    Result    = error("input not matched by any regexp", Start),
    Instance  = ((( Instance0
                        ^ live_lexemes   :=
                                Instance0 ^ init_lexemes)
                        ^ current_winner := no)
                        ^ buf_state      := buf__commit(BufState1)).

%------------------------------------------------------------------------------%

:- pred process_eof(io__read_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf).
:- mode process_eof(out, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_ui) is det.

process_eof(Result, Instance0, Instance, BufState, Buf) :-

    Result   =
        ( if
            live_lexeme_in_accepting_state(Instance0 ^ live_lexemes,
                        TokenCreator),
            get_token_from_buffer(BufState, Buf, Instance0,
                        TokenCreator, Token)
          then ok(Token)
          else eof
        ),
    Instance = ((Instance0
                        ^ live_lexemes := [])
                        ^ buf_state    := buf__commit(BufState)).

%------------------------------------------------------------------------------%

:- pred get_token_from_buffer(buf_state(Src), buf, lexer_instance(Tok, Src),
                  token_creator(Tok), Tok).
:- mode get_token_from_buffer(in(buf_state), array_ui, in(lexer_instance),
                  in(token_creator), out) is semidet.

get_token_from_buffer(BufState, Buf, Instance, TokenCreator, Token) :-
    Match      = buf__string_to_cursor(BufState, Buf),
    Token      = TokenCreator(Match),
    IgnorePred = Instance ^ ignore_pred,
    not IgnorePred(Token).

%------------------------------------------------------------------------------%

    % Note that in the case where two or more lexemes match the same
    % string, the win is given to the earliest such lexeme in the list.
    % This matches the behaviour of standard C lex.
    %
:- pred advance_live_lexemes(char, offset,
            list(live_lexeme(Token)), list(live_lexeme(Token)),
            winner(Token), winner(Token)).
:- mode advance_live_lexemes(in, in, in(live_lexeme_list), 
            out(live_lexeme_list), 
            in(winner), out(winner)) is det.

advance_live_lexemes(_Char, _Offset, [], [], Winner, Winner).

advance_live_lexemes(Char, Offset, [L0 | Ls0], Ls, Winner0, Winner) :-

    State0        = L0 ^ state,
    ATok          = L0 ^ token,

    ( if next_state(L0, State0, Char, State, IsAccepting) then

        (
            IsAccepting = no,
            Winner1     = Winner0
        ;
            IsAccepting = yes,
            Winner1     = ( if   Winner0 = yes(_ATok0 - Offset0),
                                 Offset  = Offset0
                            then Winner0
                            else yes(ATok - Offset)
                          )
        ),
        advance_live_lexemes(Char, Offset, Ls0, Ls1, Winner1, Winner),
        Ls = [( L0 ^ state := State ) | Ls1]

      else

        advance_live_lexemes(Char, Offset, Ls0, Ls, Winner0, Winner)
    ).

%------------------------------------------------------------------------------%

:- pred live_lexeme_in_accepting_state(list(live_lexeme(Tok)),
                token_creator(Tok)).
:- mode live_lexeme_in_accepting_state(in(live_lexeme_list), 
                out(token_creator)) is semidet.

live_lexeme_in_accepting_state([L | Ls], Token) :-
    ( if   in_accepting_state(L)
      then Token = L ^ token
      else live_lexeme_in_accepting_state(Ls, Token)
    ).


%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

    % It's much more convenient (especially for integration with, e.g.
    % parsers such as moose) to package up the lexer_instance, buf
    % and Src in a single object.

:- type lexer_state(Tok, Src)
    --->    lexer_state(
                run                     :: lexer_instance(Tok, Src),
                buf                     :: buf,
                src                     :: Src
            ).

%------------------------------------------------------------------------------%

:- func args_lexer_state(lexer_instance(Tok, Src), buf, Src) =
            lexer_state(Tok, Src).
:- mode args_lexer_state(in(lexer_instance), array_di, di) = uo is det.

args_lexer_state(Instance, Buf, Src) = LexerState :-
    unsafe_promise_unique(lexer_state(Instance, Buf, Src), LexerState).

%------------------------------------------------------------------------------%

:- pred lexer_state_args(lexer_state(Tok,Src),lexer_instance(Tok,Src),buf,Src).
:- mode lexer_state_args(di, out(lexer_instance), array_uo, uo)  is det.

lexer_state_args(lexer_state(Instance, Buf0, Src0), Instance, Buf, Src) :-
    unsafe_promise_unique(Buf0, Buf),
    unsafe_promise_unique(Src0, Src).

%------------------------------------------------------------------------------%

manipulate_source(P, State0, State) :-
    lexer_state_args(State0, Instance, Buf, Src0),
    P(Src0, Src),
    State = args_lexer_state(Instance, Buf, Src).

%------------------------------------------------------------------------------%

read_from_stdin(_Offset, Result) -->
    io__read_char(IOResult),
    {   IOResult = ok(Char),              Result = ok(Char)
    ;   IOResult = eof,                   Result = eof
    ;   IOResult = error(_E),             throw(IOResult)
    }.

%------------------------------------------------------------------------------%

read_from_string(Offset, Result, String, String) :-
    ( if   Offset < string__length(String)
      then Result = ok(string__unsafe_index(String, Offset))
      else Result = eof
    ).

%------------------------------------------------------------------------------%
% The type of regular expressions.

:- type regexp
    --->    eps                    % The empty regexp
    ;       atom(char)             % Match a single char
    ;       conc(regexp,regexp)    % Concatenation
    ;       alt(regexp, regexp)    % Alternation
    ;       star(regexp).          % Kleene closure

%------------------------------------------------------------------------------%

:- instance regexp(regexp) where [
    re(RE) = RE
].

:- instance regexp(char) where [
    re(C) = atom(C)
].

:- instance regexp(string) where [
    re(S) =  R :-
        ( if S = "" then
            R = null
          else
            L = string__length(S),
            C = string__index_det(S, L - 1),
            R = str_foldr(func(Cx, Rx) = (Cx ++ Rx), S, re(C), L - 2)
        )
].

%------------------------------------------------------------------------------%
% Basic primitive regexps.

 null      = eps.
 R1 ++ R2  = conc(re(R1), re(R2)).
 R1 \/ R2  = alt(re(R1), re(R2)).
(R1 or R2) = alt(re(R1), re(R2)).
 *(R1)     = star(re(R1)).

%------------------------------------------------------------------------------%
% Some basic non-primitive regexps.

any(S) = R :-
    ( if S = "" then
        R = null
      else
        L = string__length(S),
        C = string__index_det(S, L - 1),
        R = str_foldr(func(Cx, Rx) = (Cx or Rx), S, re(C), L - 2)
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

?(R) = (R or null).

+(R) = (R ++ *(R)).

%------------------------------------------------------------------------------%
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
alpha      = (lower or upper).
alphanum   = (alpha or digit).
identstart = (alpha or ('_')).
ident      = (alphanum or ('_')).
nl         = re('\n').
tab        = re('\t').
spc        = re(' ').

%------------------------------------------------------------------------------%
% Some useful compound regexps.

nat        = +(digit).
signed_int = ?("+" or "-") ++ nat.
real       = signed_int ++ (
                ("." ++ nat ++ ?(("e" or "E") ++ signed_int)) or
                (                ("e" or "E") ++ signed_int)
             ).
identifier = (identstart ++ *(ident)).
whitespace = *(wspc).
junk       = *(dot).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
