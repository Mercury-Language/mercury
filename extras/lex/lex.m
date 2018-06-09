%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%-----------------------------------------------------------------------------%
%
% lex.m
% Copyright (C) 2001-2002 Ralph Becket <rbeck@microsoft.com>
% Sun Aug 20 09:08:46 BST 2000
% Copyright (C) 2001-2002 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms
%   of the GNU Lesser General Public License, see the file COPYING.LGPL
%   in this directory.
% Copyright (C) 2002, 2006, 2010-2011 The University of Melbourne
% Copyright (C) 2014, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%
% This module puts everything together, compiling a list of lexemes into state
% machines and turning the input stream into a token stream.
%
% Note that the astral characters (in Unicode) are not included in the range of
% Unicode characters, as the astral planes are very sparsely assigned.
%
%-----------------------------------------------------------------------------%

:- module lex.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module sparse_bitset.
:- import_module enum.

%-----------------------------------------------------------------------------%

:- type token_creator(Token)
    ==                        (func(string) = Token).
:- inst token_creator
    ==                        (func(in) = out is det).

:- type lexeme(Token)
    ==                        pair(regexp, token_creator(Token)).

:- inst lexeme(Inst)
    ---> (ground - Inst).

:- type lexer(Token, Source).

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

    % Represents a set of Unicode characters
    %
:- type charset
    ==      sparse_bitset(char).

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
:- instance regexp(sparse_bitset(T)) <= (regexp(T),enum(T)).

    % Some basic non-primitive regexps.
    %
:- func any(string) = regexp.        % any("abc") = ('a') or ('b') or ('c')
:- func anybut(string) = regexp.     % anybut("abc") is complement of any("abc")
:- func ?(T) = regexp <= regexp(T).  % ?(R)       = R or null
:- func +(T) = regexp <= regexp(T).  % +(R)       = R ++ *(R)
:- func range(char, char) = regexp.  % range('a', 'z') = any("ab...xyz")
:- func (T * int) = regexp <= regexp(T). % R * N = R ++ ... ++ R

    % Some useful single-char regexps.
    %
:- func digit = regexp.         % digit      = any("0123456789")
:- func lower = regexp.         % lower      = any("abc...z")
:- func upper = regexp.         % upper      = any("ABC...Z")
:- func alpha = regexp.         % alpha      = lower or upper
:- func alphanum = regexp.      % alphanum   = alpha or digit
:- func identstart = regexp.    % identstart = alpha or "_"
:- func ident = regexp.         % ident      = alphanum or "_"
:- func tab = regexp.           % tab        = re("\t")
:- func spc = regexp.           % spc        = re(" ")
:- func wspc = regexp.          % wspc       = any(" \t\n\r\f\v")
:- func dot = regexp.           % dot        = anybut("\r\n")

    % Some useful compound regexps.
    %
:- func nl = regexp.            % nl         = ?("\r") ++ re("\n")
:- func nat = regexp.           % nat        = +(digit)
:- func signed_int = regexp.    % signed_int = ?("+" or "-") ++ nat
:- func real = regexp.          % real       = \d+((.\d+([eE]int)?)|[eE]int)
:- func identifier = regexp.    % identifier = identstart ++ *(ident)
:- func whitespace = regexp.    % whitespace = *(wspc)
:- func junk = regexp.          % junk       = *(dot)

    % A range of charicters, inclusive of both the first and last values.
    %
:- type char_range
    --->    char_range(
                cr_first        :: int,
                cr_last         :: int
            ).

    % charset(Start, End) = charset(Start `..` End)
    %
    % Throws an exception if Start > End.
    %
:- func charset(int, int) = charset.

    % Function to create a sparse bitset from a range of Unicode
    % codepoints. These codepoints are checked for validity, any invalid
    % codepoints are ignored.  Throws an exception if cr_first value is less
    % than cr_last.
    %
:- func charset(char_range) = charset.

    % Creates a union of all char ranges in the list.  Returns the empty
    % set if the list is empty.  Any invalid codepoints are ignored.
    %
:- func charset_from_ranges(list(char_range)) = charset.

    % Latin is comprised of the following Unicode blocks:
    %  * Basic Latin
    %  * Latin1 Supplement
    %  * Latin Extended-A
    %  * Latin Extended-B
    %
:- func latin_chars = charset is det.

   % Utility predicate to create ignore_pred's.
   % Use it in the form `ignore(my_token)' to ignore just `my_token'.
   %
:- pred ignore(Token::in, Token::in) is semidet.

   % Utility function to return noval tokens.
   % Use it in the form `return(my_token) inside a lexeme definition.
   %
:- func return(T, string) = T.

   % Utility operator to create lexemes.
   %
:- func (T1 -> token_creator(Tok)) = pair(regexp, token_creator(Tok))
            <= regexp(T1).

    % Construct a lexer from which we can generate running
    % instances.
    %
    % NOTE: If several lexemes match the same string only
    % the token generated by the one closest to the start
    % of the list of lexemes is returned.
    %
:- func init(list(lexeme(Tok)), read_pred(Src)) = lexer(Tok, Src).
:- mode init(in, in(read_pred)) = out is det.

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
:- mode init(in, in(read_pred), in(ignore_pred)) = out is det.

    % Handy read predicates.
    %
:- pred read_from_stdin(offset, read_result, io, io).
:- mode read_from_stdin(in, out, di, uo) is det.

:- pred read_from_string(offset, read_result, string, string).
:- mode read_from_string(in, out, di, uo) is det.

    % Generate a running instance of a lexer on some input source.
    % If you want to lex strings, you must ensure they are unique
    % by calling either copy/1 or unsafe_promise_unique/1 on the
    % source string argument.
    %
    % Note that you can't get the input source back until you stop
    % lexing.
    %
:- func start(lexer(Tok, Src), Src) = lexer_state(Tok, Src).
:- mode start(in, di) = uo is det.

    % Read the next token from the input stream.
    %
    % CAVEAT: if the token returned happened to match the empty
    % string then you must use read_char/3 (below) to consume
    % the next char in the input stream before calling read/3
    % again, since matching the empty string does not consume
    % any chars from the input stream and will otherwise mean
    % you simply get the same match ad infinitum.
    %
    % An alternative solution is to always include a "catch all"
    % lexeme that matches any unexpected char at the end of the
    % list of lexemes.
    %
:- pred read(io.read_result(Tok),
            lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode read(out, di, uo) is det.

    % Calling offset_from_start/3 immediately prior to calling read/3
    % will give the offset in chars from the start of the input stream
    % for the result returned by the read/3 operation.
    %
:- pred offset_from_start(offset,
            lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode offset_from_start(out, di, uo) is det.

    % Stop a running instance of a lexer and retrieve the input source.
    %
:- func stop(lexer_state(_Tok, Src)) = Src.
:- mode stop(di) = uo is det.

    % Sometimes (e.g. when lexing the io.io) you want access to the
    % input stream without interrupting the lexing process.  This pred
    % provides that sort of access.
    %
:- pred manipulate_source(pred(Src, Src),
            lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode manipulate_source(pred(di, uo) is det, di, uo) is det.

    % This is occasionally useful.  It reads the next char from the
    % input stream, without attempting to match it against a lexeme.
    %
:- pred read_char(read_result, lexer_state(Tok, Src), lexer_state(Tok, Src)).
:- mode read_char(out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module lex.automata.
:- include_module lex.buf.
:- include_module lex.convert_NFA_to_DFA.
:- include_module lex.lexeme.
:- include_module lex.regexp.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module require.
:- import_module int.
:- import_module map.

:- import_module lex.automata.
:- import_module lex.buf.
:- import_module lex.convert_NFA_to_DFA.
:- import_module lex.lexeme.
:- import_module lex.regexp.

%-----------------------------------------------------------------------------%

:- type lexer(Token, Source)
    --->    lexer(
                lex_compiled_lexemes    :: list(live_lexeme(Token)),
                lex_ignore_pred         :: ignore_pred(Token),
                lex_buf_read_pred       :: read_pred(Source)
            ).

:- inst lexer
    --->    lexer(ground, ignore_pred, read_pred).

:- type lexer_instance(Token, Source)
    --->    lexer_instance(
                init_lexemes            :: list(live_lexeme(Token)),
                init_winner_func        :: init_winner_func(Token),
                live_lexemes            :: list(live_lexeme(Token)),
                current_winner          :: winner(Token),
                buf_state               :: buf_state(Source),
                ignore_pred             :: ignore_pred(Token)
            ).

:- inst lexer_instance
    --->    lexer_instance(
                live_lexeme_list,
                init_winner_func,
                live_lexeme_list,
                winner,
                buf.buf_state,
                ignore_pred
            ).

:- type live_lexeme(Token)
    ==      compiled_lexeme(Token).
:- inst live_lexeme
    ==      compiled_lexeme.
:- inst live_lexeme_list
    ==      list.list_skel(live_lexeme).

:- type init_winner_func(Token)
    ==      ( func(offset) = winner(Token) ).
:- inst init_winner_func
    ==      ( func(in)     = out is det    ).



:- type winner(Token)
    ==      maybe(pair(token_creator(Token), offset)).
:- inst winner
    --->    yes(pair(token_creator, ground))
    ;       no.

%-----------------------------------------------------------------------------%

ignore(Tok, Tok).

%-----------------------------------------------------------------------------%

return(Token, _) = Token.

%-----------------------------------------------------------------------------%

(R1 -> TC) = (re(R1) - TC).

%-----------------------------------------------------------------------------%

init(Lexemes, BufReadPred) = init(Lexemes, BufReadPred, DontIgnoreAnything) :-
    DontIgnoreAnything = ( pred(_::in) is semidet :- semidet_fail ).

init(Lexemes, BufReadPred, IgnorePred) =
    lexer(CompiledLexemes, IgnorePred, BufReadPred)
 :-
    CompiledLexemes = list.map(compile_lexeme, Lexemes).

%-----------------------------------------------------------------------------%

start(Lexer0, Src) = State :-
    Lexer = lexer_inst_cast(Lexer0),
    init_lexer_instance(Lexer, Instance, Buf),
    State = args_lexer_state(Instance, Buf, Src).

:- func lexer_inst_cast(lexer(Tok, Src)::in) = (lexer(Tok, Src)::out(lexer))
    is det.

:- pragma foreign_proc("C",
    lexer_inst_cast(Lexer0::in) = (Lexer::out(lexer)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Lexer = Lexer0;
").

:- pragma foreign_proc("Java",
    lexer_inst_cast(Lexer0::in) = (Lexer::out(lexer)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Lexer = Lexer0;
").

:- pragma foreign_proc("C#",
    lexer_inst_cast(Lexer0::in) = (Lexer::out(lexer)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Lexer = Lexer0;
").

%-----------------------------------------------------------------------------%

:- pred init_lexer_instance(lexer(Tok, Src), lexer_instance(Tok, Src), buf).
:- mode init_lexer_instance(in(lexer), out(lexer_instance), array_uo) is det.

init_lexer_instance(Lexer, Instance, Buf) :-
    buf.init(Lexer ^ lex_buf_read_pred, BufState, Buf),
    Start          = BufState ^ start_offset,
    InitWinnerFunc = initial_winner_func(InitLexemes),
    InitLexemes    = Lexer ^ lex_compiled_lexemes,
    InitWinner     = InitWinnerFunc(Start),
    IgnorePred     = Lexer ^ lex_ignore_pred,
    Instance       = lexer_instance(InitLexemes, InitWinnerFunc, InitLexemes,
                           InitWinner, BufState, IgnorePred).

%-----------------------------------------------------------------------------%

    % Lexing may *start* with a candidate winner if one of the lexemes
    % accepts the empty string.  We pick the first such, if any, since
    % that lexeme has priority.
    %
:- func initial_winner_func(list(live_lexeme(Token))) = init_winner_func(Token).
:- mode initial_winner_func(in(live_lexeme_list)    ) = out(init_winner_func)
            is det.

initial_winner_func([]       ) =
    ( func(_) = no ).

initial_winner_func( [L | Ls]) =
    ( if   in_accepting_state(L)
      then ( func(Offset) = yes(L ^ token - Offset) )
      else initial_winner_func(Ls)
    ).

%----------------------------------------------------------------------------%

offset_from_start(Offset, !State) :-
    Offset  = !.State ^ run ^ buf_state ^ buf_cursor,
    !:State = unsafe_promise_unique(!.State).

%-----------------------------------------------------------------------------%

stop(State) = Src :-
    lexer_state_args(State, _Instance, _Buf, Src).

%-----------------------------------------------------------------------------%

read(Result, State0, State) :-

    lexer_state_args(State0, Instance0, Buf0, Src0),
    BufState0  = Instance0 ^ buf_state,
    Start      = BufState0 ^ start_offset,
    InitWinner = ( Instance0 ^ init_winner_func )(Start),
    Instance1  = ( Instance0 ^ current_winner := InitWinner ),
    read_2(Result, Instance1, Instance, Buf0, Buf, Src0, Src),
    State      = args_lexer_state(Instance, Buf, Src).



:- pred read_2(io.read_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf, buf, Src, Src).
:- mode read_2(out,
            in(lexer_instance), out(lexer_instance),
            array_di, array_uo, di, uo) is det.

    % Basically, just read chars from the buf and advance the live lexemes
    % until we have a winner or hit an error (no parse).
    %
read_2(Result, !Instance, !Buf, !Src) :-

    BufState0 = !.Instance ^ buf_state,

    buf.read(BufReadResult, BufState0, BufState, !Buf, !Src),
    (
        BufReadResult = ok(Char),
        process_char(Result, Char, !Instance, BufState, !Buf, !Src)
    ;
        BufReadResult = eof,
        process_eof(Result, !Instance, BufState, !.Buf)
    ).

%-----------------------------------------------------------------------------%

:- pred process_char(io.read_result(Tok), char,
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf, buf, Src, Src).
:- mode process_char(out, in, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_char(Result, Char, !Instance, BufState, !Buf, !Src) :-

    LiveLexemes0 = !.Instance ^ live_lexemes,
    Winner0      = !.Instance ^ current_winner,

    advance_live_lexemes(Char, BufState ^ cursor_offset,
            LiveLexemes0, LiveLexemes, Winner0, Winner),
    (
        LiveLexemes = [],               % Nothing left to consider.

        process_any_winner(Result, Winner, !Instance, BufState, !Buf, !Src)
    ;
        LiveLexemes = [_ | _],          % Still some open possibilities.

        !:Instance  = (((!.Instance ^ live_lexemes   := LiveLexemes )
                                    ^ current_winner := Winner      )
                                    ^ buf_state      := BufState    ),
        read_2(Result, !Instance, !Buf, !Src)
    ).

%-----------------------------------------------------------------------------%

:- pred process_any_winner(io.read_result(Tok), winner(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf, buf, Src, Src).
:- mode process_any_winner(out, in(winner),
            in(lexer_instance), out(lexer_instance),
            in(buf_state), array_di, array_uo, di, uo) is det.

process_any_winner(Result, yes(TokenCreator - Offset), Instance0, Instance,
        BufState0, Buf0, Buf, Src0, Src) :-

    BufState1  = rewind_cursor(Offset, BufState0),
    String     = string_to_cursor(BufState1, Buf0),
    Token      = TokenCreator(String),
    IgnorePred = Instance0 ^ ignore_pred,
    InitWinner = ( Instance0 ^ init_winner_func )(Offset),
    Instance1  = ((( Instance0
                       ^ live_lexemes       := Instance0 ^ init_lexemes )
                       ^ current_winner     := InitWinner               )
                       ^ buf_state          := commit(BufState1)        ),

    ( if IgnorePred(Token) then

            % We have to be careful to avoid an infinite loop here.
            % If the longest match was the empty string, then the
            % next char in the input stream cannot start a match,
            % so it must be reported as an error.
            %
        ( if String = "" then
            buf.read(BufResult, BufState1, BufState, Buf0, Buf, Src0, Src),
            (
                BufResult = ok(_),
                Result    = error("input not matched by any regexp", Offset)
            ;
                BufResult = eof,
                Result    = eof
            ),
            Instance = ( Instance1 ^ buf_state := commit(BufState) )
          else
            read_2(Result, Instance1, Instance, Buf0, Buf, Src0, Src)
        )
      else
        Result   = ok(Token),
        Instance = Instance1,
        Buf      = Buf0,
        Src      = Src0
    ).

process_any_winner(Result, no, !Instance,
        BufState0, !Buf, !Src) :-

    Start      = BufState0 ^ start_offset,
    BufState   = rewind_cursor(Start + 1, BufState0),
    Result     = error("input not matched by any regexp", Start),

    InitWinner = ( !.Instance ^ init_winner_func )(Start),
    !:Instance = ((( !.Instance ^ live_lexemes   := !.Instance ^ init_lexemes )
                                ^ current_winner := InitWinner                )
                                ^ buf_state      := commit(BufState)          ).

%-----------------------------------------------------------------------------%

:- pred process_eof(io.read_result(Tok),
            lexer_instance(Tok, Src), lexer_instance(Tok, Src),
            buf_state(Src), buf).
:- mode process_eof(out, in(lexer_instance), out(lexer_instance),
            in(buf_state), array_ui) is det.

process_eof(Result, !Instance, !.BufState, !.Buf) :-

    CurrentWinner = !.Instance ^ current_winner,
    (
        CurrentWinner = no,
        Offset        = !.BufState ^ cursor_offset,
        Result        = eof
    ;
        CurrentWinner = yes(TokenCreator - Offset),
        String        = string_to_cursor(!.BufState, !.Buf),
        Token         = TokenCreator(String),
        IgnorePred    = !.Instance ^ ignore_pred,
        Result        = ( if IgnorePred(Token) then eof else ok(Token) )
    ),
    InitWinner = ( !.Instance ^ init_winner_func )(Offset),
    !:Instance = ((( !.Instance ^ live_lexemes   := !.Instance ^ init_lexemes )
                                ^ current_winner := InitWinner                )
                                ^ buf_state      := commit(!.BufState)        ).

%-----------------------------------------------------------------------------%

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

advance_live_lexemes(_Char, _Offset, [], [], !Winner).

advance_live_lexemes(Char, Offset, [L | Ls0], Ls, !Winner) :-

    State0        = L ^ state,

    ( if next_state(L, State0, Char, State, IsAccepting) then

        (
            IsAccepting = no
        ;
            IsAccepting = yes,
            !:Winner    = ( if   !.Winner = yes(_ - Offset)
                            then !.Winner
                            else yes(L ^ token - Offset)
                          )
        ),
        advance_live_lexemes(Char, Offset, Ls0, Ls1, !Winner),
        Ls = [( L ^ state := State ) | Ls1]

      else

        advance_live_lexemes(Char, Offset, Ls0, Ls, !Winner)
    ).

%-----------------------------------------------------------------------------%

:- pred live_lexeme_in_accepting_state(list(live_lexeme(Tok)),
                token_creator(Tok)).
:- mode live_lexeme_in_accepting_state(in(live_lexeme_list),
                out(token_creator)) is semidet.

live_lexeme_in_accepting_state([L | Ls], Token) :-
    ( if   in_accepting_state(L)
      then Token = L ^ token
      else live_lexeme_in_accepting_state(Ls, Token)
    ).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % It's much more convenient (especially for integration with, e.g.
    % parsers such as moose) to package up the lexer_instance, buf
    % and Src in a single object.

:- type lexer_state(Tok, Src)
    --->    lexer_state(
                run                     :: lexer_instance(Tok, Src),
                buf                     :: buf,
                src                     :: Src
            ).

%-----------------------------------------------------------------------------%

:- func args_lexer_state(lexer_instance(Tok, Src), buf, Src) =
            lexer_state(Tok, Src).
:- mode args_lexer_state(in(lexer_instance), array_di, di) = uo is det.

args_lexer_state(Instance, Buf, Src) = LexerState :-
    unsafe_promise_unique(lexer_state(Instance, Buf, Src), LexerState).

%-----------------------------------------------------------------------------%

:- pred lexer_state_args(lexer_state(Tok, Src), lexer_instance(Tok, Src),
            buf, Src).
:- mode lexer_state_args(di, out(lexer_instance),
            array_uo, uo)  is det.

lexer_state_args(lexer_state(Instance, Buf0, Src0), Instance, Buf, Src) :-
    unsafe_promise_unique(Buf0, Buf),
    unsafe_promise_unique(Src0, Src).

%-----------------------------------------------------------------------------%

manipulate_source(P, !State) :-
    lexer_state_args(!.State, Instance, Buf, Src0),
    P(Src0, Src),
    !:State = args_lexer_state(Instance, Buf, Src).

%----------------------------------------------------------------------------%

read_char(Result, !State) :-

    lexer_state_args(!.State, Instance0, Buf0, Src0),

    BufState0 = Instance0 ^ buf_state,
    buf.read(Result, BufState0, BufState, Buf0, Buf, Src0, Src),
    Instance  = ( Instance0 ^ buf_state := commit(BufState) ),

    !:State = args_lexer_state(Instance, Buf, Src).

%-----------------------------------------------------------------------------%

read_from_stdin(_Offset, Result) -->
    io.read_char(IOResult),
    {   IOResult = ok(Char),              Result = ok(Char)
    ;   IOResult = eof,                   Result = eof
    ;   IOResult = error(_E),             throw(IOResult)
    }.

%-----------------------------------------------------------------------------%

    % XXX This is bad for long strings!  We should cache the string
    % length somewhere rather than recomputing it each time we read
    % a char.
    %
read_from_string(Offset, Result, String, unsafe_promise_unique(String)) :-
    ( if   Offset < string.length(String)
      then Result = ok(string.unsafe_index(String, Offset))
      else Result = eof
    ).

%-----------------------------------------------------------------------------%
% The type of regular expressions.

:- type regexp
    --->    eps                    % The empty regexp
    ;       atom(char)             % Match a single char
    ;       conc(regexp, regexp)   % Concatenation
    ;       alt(regexp, regexp)    % Alternation
    ;       star(regexp)           % Kleene closure
    ;       charset(charset).      % Matches any char in the set

%-----------------------------------------------------------------------------%

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
            R = string.foldl(func(Char, R0) = R1 :-
                ( if R0 = eps then R1 = re(Char) else R1 = R0 ++ re(Char) ),
                S,
                eps)
        )
].

:- instance regexp(sparse_bitset(T)) <= (regexp(T),enum(T)) where [
    re(SparseBitset) = charset(Charset) :-
        Charset = sparse_bitset.foldl(
            func(Enum, Set0) = insert(Set0, char.det_from_int(to_int(Enum))),
            SparseBitset,
            sparse_bitset.init)
].

%-----------------------------------------------------------------------------%
% Basic primitive regexps.

 null      = eps.
 R1 ++ R2  = conc(re(R1), re(R2)).
 R1 \/ R2  = alt(re(R1), re(R2)).
(R1 or R2) = alt(re(R1), re(R2)).
 *(R1)     = star(re(R1)).

%-----------------------------------------------------------------------------%
% Some basic non-primitive regexps.

    % int_is_valid_char(Int) = Char.
    %
    % True iff Int is Char and is in [0x0..0x10ffff] and not a surrogate
    % character.
    %
:- func int_is_valid_char(int) = char is semidet.

int_is_valid_char(Value) = Char :-
    char.from_int(Value, Char),
    not char.is_surrogate(Char).

charset(Start, End) = build_charset(Start, End, sparse_bitset.init) :-
    expect(Start =< End, $file, $pred,
        "Start must be less than or equal to End").

charset(char_range(First, Last)) = charset(First, Last).

:- func build_charset(int, int, charset) = charset.

build_charset(First, Last, Charset0) = Charset :-
    if First =< Last then
        ( if int_is_valid_char(First) = Char then
            Charset1 = sparse_bitset.insert(Charset0, Char)
        else
            Charset1 = Charset0
        ),
        Charset = build_charset(First + 1, Last, Charset1)
    else
        Charset = Charset0.

charset_from_ranges(ListOfRanges) =
    union_list(map(charset, ListOfRanges)).

latin_chars = charset_from_ranges([
                char_range(0x40, 0x7d),
                char_range(0xc0, 0xff),
                char_range(0x100, 0x2ff)
              ]).

:- func valid_unicode_chars = charset.

valid_unicode_chars = charset(char_range(0x01, 0xffff)).

any(S) = R :-
    ( if S = "" then
        R = null
      else
        R = re(sparse_bitset.list_to_set(string.to_char_list(S)))
    ).

anybut(S) = R :-
    ExcludedChars = sparse_bitset.list_to_set(string.to_char_list(S)),
    R = re(sparse_bitset.difference(valid_unicode_chars, ExcludedChars)).

?(R) = (R or null).

+(R) = (R ++ *(R)).

range(Start, End) = re(charset(char.to_int(Start), char.to_int(End))).

R * N = Result :-
    ( N < 0 ->
        unexpected($file, $pred, "N must be a non-negative number")
    ; N = 0 ->
        Result = null
    ; N = 1 ->
        Result = re(R)
    ;
        Result = conc(re(R), (R * (N - 1)))
    ).

%-----------------------------------------------------------------------------%
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
dot        = anybut("\r\n").
alpha      = (lower or upper).
alphanum   = (alpha or digit).
identstart = (alpha or ('_')).
ident      = (alphanum or ('_')).
tab        = re('\t').
spc        = re(' ').

%-----------------------------------------------------------------------------%
% Some useful compound regexps.

nl         = (?('\r') ++ '\n').  % matches both Posix and Windows newline.
nat        = +(digit).
signed_int = ?("+" or "-") ++ nat.
real       = signed_int ++ (
                ("." ++ nat ++ ?(("e" or "E") ++ signed_int)) or
                (                ("e" or "E") ++ signed_int)
             ).
identifier = (identstart ++ *(ident)).
whitespace = *(wspc).
junk       = *(dot).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
