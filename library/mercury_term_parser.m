%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2001, 2003-2008, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: mercury_term_parser.m.
% Main author: fjh.
% Stability: high.
%
% This file exports the predicate read_term, which reads a term from the
% current input stream. The read_term_from_*string predicates are the same as
% the read_term predicates, except that the term is read from a string rather
% than from the current input stream. The parse_tokens predicate is
% similar, but it takes a list of tokens rather than a string.
%
% The parser is a relatively straight-forward top-down recursive descent
% parser, made somewhat complicated by the need to handle operator precedences.
% It uses mercury_term_lexer.get_token_list to read a list of tokens.
% It uses the routines from the ops module to look up operator precedences.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mercury_term_parser.
:- interface.

:- import_module io.
:- import_module mercury_term_lexer.
:- import_module ops.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type read_term(T)
    --->    eof
            % We have reached the end-of-file.
    ;       error(string, int)
            % We have found an error described the message string
            % on the given line number in the input.
    ;       term(varset(T), term(T)).
            % We have read in the given term with the given varset.

:- type read_term == read_term(generic).

    % read_term(Result, !IO):
    % read_term(Stream, Result, !IO):
    %
    % Reads a Mercury term from the current input stream, or from Stream.
    %
:- pred read_term(read_term(T)::out, io::di, io::uo) is det.
:- pred read_term(io.text_input_stream::in, read_term(T)::out,
    io::di, io::uo) is det.

    % read_term_with_op_table(Ops, Result, !IO):
    % read_term_with_op_table(Stream, Ops, Result, !IO):
    %
    % Reads a term from the current input stream, or from Stream,
    % using the given op_table to interpret the operators.
    %
:- pred read_term_with_op_table(Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).
:- pred read_term_with_op_table(io.text_input_stream::in, Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).

    % read_term_filename(FileName, Result, !IO):
    % read_term_filename(Stream, FileName, Result, !IO):
    %
    % Reads a term from the current input stream, or from Stream.
    % The string is the filename to use for the stream; this is used
    % in constructing the term_contexts in the read term.
    % This interface is used to support the `:- pragma source_file' directive.
    %
:- pred read_term_filename(string::in,
    read_term(T)::out, io::di, io::uo) is det.
:- pred read_term_filename(io.text_input_stream::in, string::in,
    read_term(T)::out, io::di, io::uo) is det.

    % read_term_filename_with_op_table(Ops, FileName, Result, !IO):
    % read_term_filename_with_op_table(Stream, Ops, FileName, Result, !IO):
    %
    % As above but using the given op_table.
    %
:- pred read_term_filename_with_op_table(Ops::in,
    string::in, read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).
:- pred read_term_filename_with_op_table(io.text_input_stream::in, Ops::in,
    string::in, read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).

%---------------------------------------------------------------------------%

    % The read_term_from_string predicates are the same as the read_term
    % predicates, except that the term is read from a string rather than from
    % the current input stream. The returned value EndPos is the position
    % one character past the end of the term read. The arguments StringLen
    % and StartPos in the read_term_from_substring* versions specify
    % the length of the string and the position within the string
    % at which to start parsing.

    % read_term_from_string(FileName, String, EndPos, Term).
    %
:- pred read_term_from_string(string::in, string::in, posn::out,
    read_term(T)::out) is det.

    % read_term_from_string_with_op_table(Ops, FileName, String, EndPos, Term).
    %
:- pred read_term_from_string_with_op_table(Ops::in, string::in,
    string::in, posn::out, read_term(T)::out) is det <= op_table(Ops).

    % read_term_from_substring(FileName, String, StringLen,
    %   StartPos, EndPos, Term).
    % read_term_from_linestr(FileName, String, StringLen,
    %   StartLineContext, EndLineContext, StartLinePosn, EndLinePosn, Term).
    %
:- pred read_term_from_substring(string::in, string::in, int::in,
    posn::in, posn::out, read_term(T)::out) is det.
:- pred read_term_from_linestr(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    read_term(T)::out) is det.

    % read_term_from_substring_with_op_table(Ops, FileName, String, StringLen,
    %   StartPos, EndPos, Term).
    % read_term_from_linestr_with_op_table(Ops, FileName, String, StringLen,
    %   StartLineContext, EndLineContext, StartLinePosn, EndLinePosn, Term).
    %
:- pred read_term_from_substring_with_op_table(Ops::in, string::in,
    string::in, int::in, posn::in, posn::out, read_term(T)::out) is det
    <= op_table(Ops).
:- pred read_term_from_linestr_with_op_table(Ops::in, string::in,
    string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    read_term(T)::out) is det <= op_table(Ops).

%---------------------------------------------------------------------------%

    % parse_tokens(FileName, TokenList, Result):
    %
:- pred parse_tokens(string::in, token_list::in, read_term(T)::out) is det.

    % parse_tokens(FileName, TokenList, Result):
    %
:- pred parse_tokens_with_op_table(Ops::in, string::in, token_list::in,
    read_term(T)::out) is det <= op_table(Ops).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term_context.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type parse_result(T)
    --->    pr_ok(T)
    ;       pr_error(string, token_list).

    % Are we parsing an ordinary term, an argument or a list element?
:- type term_kind
    --->    ordinary_term
    ;       argument
    ;       list_elem.

%---------------------------------------------------------------------------%

read_term(Result, !IO) :-
    io.input_stream(Stream, !IO),
    mercury_term_parser.read_term(Stream, Result, !IO).

read_term(Stream, Result, !IO) :-
    io.input_stream_name(Stream, FileName, !IO),
    read_term_filename_with_op_table(Stream, ops.init_mercury_op_table,
        FileName, Result, !IO).

read_term_with_op_table(Ops, Result, !IO) :-
    io.input_stream(Stream, !IO),
    read_term_with_op_table(Stream, Ops, Result, !IO).

read_term_with_op_table(Stream, Ops, Result, !IO) :-
    io.input_stream_name(Stream, FileName, !IO),
    read_term_filename_with_op_table(Stream, Ops, FileName, Result, !IO).

read_term_filename(FileName, Result, !IO) :-
    io.input_stream(Stream, !IO),
    read_term_filename(Stream, FileName, Result, !IO).

read_term_filename(Stream, FileName, Result, !IO) :-
    read_term_filename_with_op_table(Stream, ops.init_mercury_op_table,
        FileName, Result, !IO).

read_term_filename_with_op_table(Ops, FileName, Result, !IO) :-
    io.input_stream(Stream, !IO),
    read_term_filename_with_op_table(Stream, Ops, FileName, Result, !IO).

read_term_filename_with_op_table(Stream, Ops, FileName, Result, !IO) :-
    get_token_list(Stream, Tokens, !IO),
    parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%---------------------%

read_term_from_string(FileName, String, EndPos, Result) :-
    read_term_from_string_with_op_table(ops.init_mercury_op_table,
        FileName, String, EndPos, Result).

read_term_from_string_with_op_table(Ops, FileName, String, EndPos, Result) :-
    string.length(String, Len),
    StartPos = init_posn,
    read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result).

read_term_from_substring(FileName, String, Len, StartPos, EndPos, Result) :-
    read_term_from_substring_with_op_table(ops.init_mercury_op_table,
        FileName, String, Len, StartPos, EndPos, Result).

read_term_from_linestr(FileName, String, Len, StartLineContext, EndLineContext,
        StartLinePosn, EndLinePosn, Result) :-
    read_term_from_linestr_with_op_table(ops.init_mercury_op_table,
        FileName, String, Len, StartLineContext, EndLineContext,
        StartLinePosn, EndLinePosn, Result).

read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result) :-
    string_get_token_list_max(String, Len, Tokens, StartPos, EndPos),
    parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

read_term_from_linestr_with_op_table(Ops, FileName, String, Len,
        StartLineContext, EndLineContext, StartLinePosn, EndLinePosn,
        Result) :-
    linestr_get_token_list_max(String, Len, Tokens,
        StartLineContext, EndLineContext, StartLinePosn, EndLinePosn),
    parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%---------------------------------------------------------------------------%

parse_tokens(FileName, Tokens, Result) :-
    parse_tokens_with_op_table(ops.init_mercury_op_table, FileName,
        Tokens, Result).

parse_tokens_with_op_table(Ops, FileName, Tokens, Result) :-
    (
        Tokens = token_nil,
        Result = eof
    ;
        Tokens = token_cons(_, _, _),
        init_parser_state(Ops, FileName, ParserState0),
        parse_whole_term(Term, Tokens, LeftOverTokens,
            ParserState0, ParserState),
        final_parser_state(ParserState, VarSet),
        check_for_errors(Term, VarSet, Tokens, LeftOverTokens, Result)
    ).

:- pred check_for_errors(parse_result(term(T))::in, varset(T)::in,
    token_list::in, token_list::in, read_term(T)::out) is det.

check_for_errors(Parse, VarSet, Tokens, LeftOverTokens, Result) :-
    (
        Parse = pr_error(ErrorMessage, ErrorTokens),
        % Check if the error was caused by a bad token.
        ( if check_for_bad_token(Tokens, BadTokenMessage, BadTokenLineNum) then
            Message = BadTokenMessage,
            LineNum = BadTokenLineNum
        else
            % Find the token that caused the error.
            (
                ErrorTokens = token_cons(ErrorTok, ErrorTokLineNum, _),
                token_to_string(ErrorTok, TokString),
                string.format("Syntax error at %s: %s",
                    [s(TokString), s(ErrorMessage)], Message),
                LineNum = ErrorTokLineNum
            ;
                ErrorTokens = token_nil,
                (
                    Tokens = token_cons(_, LineNum, _)
                ;
                    Tokens = token_nil,
                    error("token_nil in check_for_errors")
                ),
                string.format("Syntax error: %s", [s(ErrorMessage)], Message)
            )
        ),
        Result = error(Message, LineNum)
    ;
        Parse = pr_ok(Term),
        ( if check_for_bad_token(Tokens, Message, LineNum) then
            Result = error(Message, LineNum)
        else
            (
                LeftOverTokens = token_cons(Token, LineNum, _),
                token_to_string(Token, TokString),
                string.format("Syntax error: unexpected %s",
                    [s(TokString)], Message),
                Result = error(Message, LineNum)
            ;
                LeftOverTokens = token_nil,
                Result = term(VarSet, Term)
            )
        )
    ).

:- pred check_for_bad_token(token_list::in, string::out, int::out) is semidet.

check_for_bad_token(token_cons(Token, LineNum0, Tokens), Message, LineNum) :-
    require_complete_switch [Token]
    (
        Token = io_error(IO_Error),
        io.error_message(IO_Error, IO_ErrorMessage),
        string.format("I/O error: %s", [s(IO_ErrorMessage)], Message),
        LineNum = LineNum0
    ;
        Token = junk(Char),
        char.to_int(Char, Code),
        string.int_to_base_string(Code, 10, Decimal),
        string.int_to_base_string(Code, 16, Hex),
        string.format("Syntax error: Illegal character 0x%s (%s) in input",
            [s(Hex), s(Decimal)], Message),
        LineNum = LineNum0
    ;
        Token = error(ErrorMessage),
        string.format("Syntax error: %s", [s(ErrorMessage)], Message),
        LineNum = LineNum0
    ;
        ( Token = name(_)
        ; Token = variable(_)
        ; Token = integer(_, _, _, _)
        ; Token = float(_)
        ; Token = string(_)
        ; Token = implementation_defined(_)
        ; Token = open
        ; Token = open_ct
        ; Token = close
        ; Token = open_list
        ; Token = close_list
        ; Token = open_curly
        ; Token = close_curly
        ; Token = ht_sep
        ; Token = comma
        ; Token = end
        ; Token = eof
        ; Token = integer_dot(_)
        ),
        check_for_bad_token(Tokens, Message, LineNum)
    ).
check_for_bad_token(token_nil, _, _) :-
    fail.

:- pred parse_whole_term(parse_result(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_whole_term(Term, !TokensLeft, !PS) :-
    parse_term(Term0, !TokensLeft, !PS),
    (
        Term0 = pr_ok(_),
        ( if !.TokensLeft = token_cons(end, _Context, !:TokensLeft) then
            Term = Term0
        else
            parser_unexpected("operator or `.' expected", Term,
                !TokensLeft, !.PS)
        )
    ;
        Term0 = pr_error(_, _),
        % Propagate error upwards.
        Term = Term0
    ).

:- pred parse_term(parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_term(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    ArgPriority = ops.universal_priority(OpTable),
    do_parse_term(ArgPriority, ordinary_term, Term, !TokensLeft, !PS).

:- pred parse_arg(parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_arg(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse '::'/2 in arguments
    % the way we want to. Perhaps a better solution would be to change the
    % priority of '::'/2, but we need to analyse the impact of that further.
    ArgPriority = ops.universal_priority(OpTable),
    do_parse_term(ArgPriority, argument, Term, !TokensLeft, !PS).

:- pred parse_list_elem(parse_result(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_list_elem(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse promise_pure/0 in
    % foreign attribute lists.
    ArgPriority = ops.universal_priority(OpTable),
    do_parse_term(ArgPriority, list_elem, Term, !TokensLeft, !PS).

:- pred do_parse_term(priority::in, term_kind::in,
    parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

do_parse_term(MinPriority, TermKind, Term, !TokensLeft, !PS) :-
    parse_left_term(MinPriority, TermKind, LeftPriority, LeftTerm0,
        !TokensLeft, !PS),
    (
        LeftTerm0 = pr_ok(LeftTerm),
        parse_rest(MinPriority, TermKind, LeftPriority, LeftTerm, Term,
            !TokensLeft, !PS)
    ;
        LeftTerm0 = pr_error(_, _),
        % propagate error upwards
        Term = LeftTerm0
    ).

:- pred parse_left_term(priority::in, term_kind::in, priority::out,
    parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_left_term(MinPriority, TermKind, OpPriority, Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    (
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        ( if
            % Check for unary minus of an integer or a float.
            Token = name(TokenName),
            TokenName = "-",
            !.TokensLeft = token_cons(NextToken, _NextContext, !:TokensLeft),
            (
                NextToken = integer(LexerBase, X, signed, LexerSize),
                NegX = -X,
                Base = lexer_base_to_term_base(LexerBase),
                Size = lexer_size_to_term_size(LexerSize),
                NewFunctor = integer(Base, NegX, signed, Size)
            ;
                NextToken = float(F),
                NegF = 0.0 - F,
                NewFunctor = float(NegF)
            )
        then
            parser_get_term_context(!.PS, Context, TermContext),
            % The fact that in terms constructed by this module,
            % the argument list of an integer or float is guaranteed to be []
            % is documented in term.m, and the compiler relies on it.
            Term = pr_ok(term.functor(NewFunctor, [], TermContext)),
            OpPriority = tightest_op_priority(OpTable)
        else if
            Token = name(TokenName),
            ops.lookup_op_infos(OpTable, TokenName, OpInfos)
        then
            ( if
                % Check for binary prefix op.
                %
                % Since most tokens aren't binary prefix ops, the test
                % will almost always fail.
                OpInfos ^ oi_binary_prefix = 
                    bin_pre(BinOpPriority, GeOrGtA, GeOrGtB),
                priority_ge(BinOpPriority, MinPriority),
                !.TokensLeft = token_cons(NextToken, _, _),
                could_start_term(NextToken, yes),
                NextToken \= open_ct
            then
                OpPriority = BinOpPriority,
                PrioA = min_priority_for_arg(OpPriority, GeOrGtA),
                PrioB = min_priority_for_arg(OpPriority, GeOrGtB),
                do_parse_term(PrioA, TermKind, ResultA, !TokensLeft, !PS),
                (
                    ResultA = pr_ok(TermA),
                    do_parse_term(PrioB, TermKind, ResultB, !TokensLeft, !PS),
                    (
                        ResultB = pr_ok(TermB),
                        parser_get_term_context(!.PS, Context, TermContext),
                        Term = pr_ok(term.functor(term.atom(TokenName),
                            [TermA, TermB], TermContext))
                    ;
                        ResultB = pr_error(_, _),
                        % Propagate error upwards.
                        Term = ResultB
                    )
                ;
                    ResultA = pr_error(_, _),
                    % Propagate error upwards.
                    Term = ResultA
                )
            else if
                % Check for prefix op.
                %
                % Since most tokens aren't prefix ops, the first test here
                % will almost always fail.
                OpInfos ^ oi_prefix = pre(UnOpPriority, GeOrGtA),
                priority_ge(UnOpPriority, MinPriority),
                !.TokensLeft = token_cons(NextToken, _, _),
                could_start_term(NextToken, yes),
                NextToken \= open_ct
            then
                OpPriority = UnOpPriority,
                PrioA = min_priority_for_arg(OpPriority, GeOrGtA),
                do_parse_term(PrioA, TermKind, ResultA, !TokensLeft, !PS),
                (
                    ResultA = pr_ok(TermA),
                    parser_get_term_context(!.PS, Context, TermContext),
                    Term = pr_ok(term.functor(term.atom(TokenName),
                        [TermA], TermContext))
                ;
                    ResultA = pr_error(_, _),
                    % Propagate error upwards.
                    Term = ResultA
                )
            else
                % TokenName is an operator, but not of a kind that
                % we should handle here.
                parse_simple_term(Token, Context, MinPriority, Term,
                    !TokensLeft, !PS),
                OpPriority = tightest_op_priority(OpTable)
            )
        else
            % TokenName is not an operator.
            parse_simple_term(Token, Context, MinPriority, Term,
                !TokensLeft, !PS),
            OpPriority = tightest_op_priority(OpTable)
        )
    ;
        !.TokensLeft = token_nil,
        Term = pr_error("unexpected end-of-file at start of sub-term",
            !.TokensLeft),
        OpPriority = tightest_op_priority(OpTable)
    ).

:- pred parse_rest(priority::in, term_kind::in, priority::in,
    term(T)::in, parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_rest(MinPriority, TermKind, LeftPriority, LeftTerm, Term,
        !TokensLeft, !PS) :-
    ( if
        % Infix op.
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        (
            Token = comma,
            TermKind = ordinary_term,
            Op0 = ","
        ;
            Token = ht_sep,
            TermKind \= list_elem,
            Op0 = "|"
        ;
            Token = name(Op0)
        ),
        ( if
            % A token surrounded by backquotes is a prefix token being used
            % in an infix manner.
            Op0 = "`",
            OpTable = parser_state_get_ops_table(!.PS),
            ops.lookup_operator_term(OpTable, OpPriority0,
                LeftGtOrGe0, RightGtOrGe0)
        then
            OpPriority = OpPriority0,
            LeftGtOrGe = LeftGtOrGe0,
            RightGtOrGe = RightGtOrGe0,
            parse_backquoted_operator(MaybeQualifier, Op, VariableTerms,
                !TokensLeft, !PS),
            !.TokensLeft = token_cons(name("`"), _Context, !:TokensLeft)
        else
            Op = Op0,
            VariableTerms = [],
            MaybeQualifier = no,
            OpTable = parser_state_get_ops_table(!.PS),
            ops.lookup_infix_op(OpTable, Op, OpPriority,
                LeftGtOrGe, RightGtOrGe)
        ),
        priority_ge(OpPriority, MinPriority),
        check_priority(LeftGtOrGe, OpPriority, LeftPriority)
    then
        RightPriority = min_priority_for_arg(OpPriority, RightGtOrGe),
        do_parse_term(RightPriority, TermKind, RightTerm0, !TokensLeft, !PS),
        (
            RightTerm0 = pr_ok(RightTerm),
            parser_get_term_context(!.PS, Context, TermContext),
            OpTermArgs0 = VariableTerms ++ [LeftTerm, RightTerm],
            OpTerm0 = term.functor(term.atom(Op), OpTermArgs0, TermContext),
            (
                MaybeQualifier = no,
                OpTerm = OpTerm0
            ;
                MaybeQualifier = yes(QTerm),
                OpTerm = term.functor(term.atom("."), [QTerm, OpTerm0],
                    TermContext)
            ),
            parse_rest(MinPriority, TermKind, OpPriority, OpTerm, Term,
                !TokensLeft, !PS)
        ;
            RightTerm0 = pr_error(_, _),
            % Propagate error upwards.
            Term = RightTerm0
        )
    else if
        % Postfix op.
        !.TokensLeft = token_cons(name(Op), Context, !:TokensLeft),
        OpTable = parser_state_get_ops_table(!.PS),
        ops.lookup_postfix_op(OpTable, Op, OpPriority, LeftGtOrGe),
        priority_ge(OpPriority, MinPriority),
        check_priority(LeftGtOrGe, OpPriority, LeftPriority)
    then
        parser_get_term_context(!.PS, Context, TermContext),
        OpTerm = term.functor(term.atom(Op), [LeftTerm], TermContext),
        parse_rest(MinPriority, TermKind, OpPriority, OpTerm, Term,
            !TokensLeft, !PS)
    else
        Term = pr_ok(LeftTerm)
    ).

:- pred parse_backquoted_operator(maybe(term(T))::out, string::out,
    list(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is semidet
    <= op_table(Ops).

parse_backquoted_operator(MaybeQualifier, OpName, VariableTerms,
        !TokensLeft, !PS) :-
    !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
    parser_get_term_context(!.PS, Context, TermContext),
    (
        Token = variable(VariableOp),
        MaybeQualifier = no,
        OpName = "",
        add_var(VariableOp, Var, !PS),
        VariableTerms = [variable(Var, TermContext)]
    ;
        Token = name(OpName0),
        VariableTerms = [],
        parse_backquoted_operator_qualifier(no, MaybeQualifier, TermContext,
            OpName0, OpName, !TokensLeft, !PS)
    ).

:- pred parse_backquoted_operator_qualifier(
    maybe(term(T))::in, maybe(term(T))::out, term_context::in, string::in,
    string::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_backquoted_operator_qualifier(MaybeQualifier0, MaybeQualifier, OpCtxt0,
        OpName0, OpName, !TokensLeft, !PS) :-
    ( if
        !.TokensLeft = token_cons(name("."), SepContext, !:TokensLeft),
        !.TokensLeft = token_cons(name(OpName1), NameContext, !:TokensLeft),
        OpName1 \= "`"
    then
        QTerm1 = term.functor(atom(OpName0), [], OpCtxt0),
        (
            MaybeQualifier0 = no,
            MaybeQualifier01 = yes(QTerm1)
        ;
            MaybeQualifier0 = yes(QTerm0),
            parser_get_term_context(!.PS, SepContext, SepCtxt),
            QTerm01 = functor(atom("."), [QTerm0, QTerm1], SepCtxt),
            MaybeQualifier01 = yes(QTerm01)
        ),
        parser_get_term_context(!.PS, NameContext, OpCtxt1),
        parse_backquoted_operator_qualifier(MaybeQualifier01, MaybeQualifier,
            OpCtxt1, OpName1, OpName, !TokensLeft, !PS)
    else
        MaybeQualifier = MaybeQualifier0,
        OpName = OpName0
    ).

%---------------------------------------------------------------------------%

    % term --> integer                  % tightest_op_priority
    % term --> float                    % tightest_op_priority
    % term --> implementation_defined   % tightest_op_priority
    % term --> name("-") integer        % tightest_op_priority
    % term --> name("-") float          % tightest_op_priority
    % term --> atom(NonOp)              % tightest_op_priority
    % term --> atom(Op)                 % universal_priority
    %   atom --> name
    %   atom --> open_list, close_list
    %   atom --> open_curly, close_curly
    % term --> variable             % priority 0
    % term --> atom, open_ct, arg_list, close
    %   arg_list --> arg
    %   arg_list --> arg, comma, arg_list
    % term --> open, term, close
    % term --> open_ct, term, close
    % term --> term, op, term       % with various conditions
    % term --> op, term             % with various conditions
    % term --> term, op             % with various conditions

:- pred parse_simple_term(token::in, token_context::in, priority::in,
    parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_simple_term(Token, Context, Prec, TermParse, !TokensLeft, !PS) :-
    (
        Token = name(Atom),
        parser_get_term_context(!.PS, Context, TermContext),
        ( if !.TokensLeft = token_cons(open_ct, _Context, !:TokensLeft) then
            parse_args(ArgsParse, !TokensLeft, !PS),
            (
                ArgsParse = pr_ok(Args),
                BaseTerm = functor(atom(Atom), Args, TermContext),
                BaseTermParse = pr_ok(BaseTerm)
            ;
                ArgsParse = pr_error(Message, Tokens),
                % Propagate error upwards, after changing type.
                BaseTermParse = pr_error(Message, Tokens)
            )
        else
            OpTable = parser_state_get_ops_table(!.PS),
            ( if
                ops.is_op(OpTable, Atom),
                priority_ge(Prec, ops.loosest_op_priority(OpTable))
            then
                parser_unexpected_tok(Token, Context,
                    "unexpected token at start of (sub)term",
                    BaseTermParse, !TokensLeft, !.PS)
            else
                BaseTerm = functor(atom(Atom), [], TermContext),
                BaseTermParse = pr_ok(BaseTerm)
            )
        )
    ;
        Token = variable(VarName),
        add_var(VarName, Var, !PS),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = term.variable(Var, TermContext),
        BaseTermParse = pr_ok(BaseTerm)
    ;
        Token = integer(LexerBase, Integer, LexerSignedness, LexerSize),
        Base = lexer_base_to_term_base(LexerBase),
        Signedness = lexer_signedness_to_term_signedness(LexerSignedness),
        Size = lexer_size_to_term_size(LexerSize),
        parser_get_term_context(!.PS, Context, TermContext),
        % The fact that in terms constructed by this module,
        % the argument list of an integer is guaranteed to be []
        % is documented in term.m, and the compiler relies on it.
        BaseTerm = functor(integer(Base, Integer, Signedness, Size), [],
            TermContext),
        BaseTermParse = pr_ok(BaseTerm)
    ;
        Token = float(Float),
        % The fact that in terms constructed by this module,
        % the argument list of a float is guaranteed to be []
        % is documented in term.m, and the compiler relies on it.
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(float(Float), [], TermContext),
        BaseTermParse = pr_ok(BaseTerm)
    ;
        Token = string(String),
        % The fact that in terms constructed by this module,
        % the argument list of a string is guaranteed to be []
        % is documented in term.m, and the compiler relies on it.
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(string(String), [], TermContext),
        BaseTermParse = pr_ok(BaseTerm)
    ;
        Token = implementation_defined(Name),
        % The fact that in terms constructed by this module,
        % the argument list of an implementation_defined is guaranteed to be []
        % is documented in term.m, and the compiler relies on it.
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(implementation_defined(Name), [], TermContext),
        BaseTermParse = pr_ok(BaseTerm)
    ;
        ( Token = open
        ; Token = open_ct
        ),
        parse_term(SubTermParse, !TokensLeft, !PS),
        (
            SubTermParse = pr_ok(_),
            ( if !.TokensLeft = token_cons(close, _Context, !:TokensLeft) then
                BaseTermParse = SubTermParse
            else
                parser_unexpected("expecting `)' or operator", BaseTermParse,
                    !TokensLeft, !.PS)
            )
        ;
            SubTermParse = pr_error(_, _),
            % Propagate error upwards.
            BaseTermParse = SubTermParse
        )
    ;
        Token = open_list,
        parser_get_term_context(!.PS, Context, TermContext),
        ( if !.TokensLeft = token_cons(close_list, _Context, !:TokensLeft) then
            parse_special_atom("[]", TermContext, BaseTermParse,
                !TokensLeft, !PS)
        else
            parse_list(BaseTermParse, !TokensLeft, !PS)
        )
    ;
        Token = open_curly,
        parser_get_term_context(!.PS, Context, TermContext),
        ( if
            !.TokensLeft = token_cons(close_curly, _Context, !:TokensLeft)
        then
            parse_special_atom("{}", TermContext, BaseTermParse,
                !TokensLeft, !PS)
        else
            % This is a slight departure from ISO Prolog syntax -- instead of
            % parsing "{1,2,3}" as "'{}'(','(1, ','(2, 3)))", we parse it as
            % "'{}'(1,2,3)". This makes the structure of tuple functors
            % the same as other functors.
            parse_term(SubTermParse, !TokensLeft, !PS),
            (
                SubTermParse = pr_ok(SubTerm),
                conjunction_to_list(SubTerm, ArgTerms),
                ( if
                    !.TokensLeft = token_cons(close_curly, _Context,
                        !:TokensLeft)
                then
                    BaseTerm = functor(atom("{}"), ArgTerms, TermContext),
                    BaseTermParse = pr_ok(BaseTerm)
                else
                    parser_unexpected("expecting `}' or operator",
                        BaseTermParse, !TokensLeft, !.PS)
                )
            ;
                SubTermParse = pr_error(_, _),
                % Propagate error upwards.
                BaseTermParse = SubTermParse
            )
        )
    ;
        ( Token = close
        ; Token = close_list
        ; Token = close_curly
        ; Token = ht_sep
        ; Token = comma
        ; Token = end
        ; Token = junk(_)
        ; Token = error(_)
        ; Token = io_error(_)
        ; Token = eof
        ; Token = integer_dot(_)
        ),
        parser_unexpected_tok(Token, Context,
            "unexpected token at start of (sub)term", BaseTermParse,
            !TokensLeft, !.PS)
    ),
    ( if
        BaseTermParse = pr_ok(BaseTermOpen),
        !.TokensLeft = token_cons(open_ct, _OpenContext, !:TokensLeft)
    then
        parse_higher_order_term_rest(BaseTermOpen, Context, TermParse,
            !TokensLeft, !PS)
    else
        TermParse = BaseTermParse
    ).

    % As an extension to ISO Prolog syntax, we check for the syntax
    % "Term(Args)", and parse it as the term ''(Term, Args). The aim
    % of this extension is to provide a nicer syntax for higher-order stuff.
    %
    % Our caller should call us after it has seen "Term("; we parse
    % the remainder, "Args)".
    %
    % The recursive call allows us to parse "Term(Args1)(Args2)" as well.
    %
:- pred parse_higher_order_term_rest(term(T)::in, token_context::in,
    parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_higher_order_term_rest(BaseTerm, Context, TermParse, !TokensLeft, !PS) :-
    parser_get_term_context(!.PS, Context, TermContext),
    parse_args(ArgsParse, !TokensLeft, !PS),
    (
        ArgsParse = pr_ok(Args),
        ApplyTerm = functor(atom(""), [BaseTerm | Args], TermContext),
        ( if
            !.TokensLeft = token_cons(open_ct, _OpenContext, !:TokensLeft)
        then
            parse_higher_order_term_rest(ApplyTerm, Context, TermParse,
                !TokensLeft, !PS)
        else
            TermParse = pr_ok(ApplyTerm)
        )
    ;
        ArgsParse = pr_error(Message, Tokens),
        % Propagate error upwards, after changing type.
        TermParse = pr_error(Message, Tokens)
    ).

:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

conjunction_to_list(Term, ArgTerms) :-
    ( if Term = term.functor(term.atom(","), [LeftTerm, RightTerm], _) then
        conjunction_to_list(RightTerm, ArgTerms0),
        ArgTerms = [LeftTerm | ArgTerms0]
    else
        ArgTerms = [Term]
    ).

:- pred parse_special_atom(string::in, term_context::in,
    parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_special_atom(Atom, TermContext, Term, !TokensLeft, !PS) :-
    ( if !.TokensLeft = token_cons(open_ct, _Context, !:TokensLeft) then
        parse_args(Args0, !TokensLeft, !PS),
        (
            Args0 = pr_ok(Args),
            Term = pr_ok(term.functor(term.atom(Atom), Args, TermContext))
        ;
            Args0 = pr_error(Message, Tokens),
            % Propagate error upwards.
            Term = pr_error(Message, Tokens)
        )
    else
        Term = pr_ok(term.functor(term.atom(Atom), [], TermContext))
    ).

:- pred parse_list(parse_result(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_list(List, !TokensLeft, !PS) :-
    parse_list_elem(Arg0, !TokensLeft, !PS),
    (
        Arg0 = pr_ok(Arg),
        parse_list_tail(Arg, List, !TokensLeft, !PS)
    ;
        Arg0 = pr_error(_, _),
        % Propagate error.
        List = Arg0
    ).

:- pred parse_list_tail(term(T)::in, parse_result(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_list_tail(Arg, List, !TokensLeft, !PS) :-
    (
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        parser_get_term_context(!.PS, Context, TermContext),
        ( if Token = comma then
            parse_list(Tail0, !TokensLeft, !PS),
            (
                Tail0 = pr_ok(Tail),
                List = pr_ok(term.functor(term.atom("[|]"), [Arg, Tail],
                    TermContext))
            ;
                Tail0 = pr_error(_, _),
                % Propagate error.
                List = Tail0
            )
        else if Token = ht_sep then
            parse_arg(Tail0, !TokensLeft, !PS),
            (
                Tail0 = pr_ok(Tail),
                ( if
                    !.TokensLeft = token_cons(close_list, _Context,
                        !:TokensLeft)
                then
                    List = pr_ok(term.functor(term.atom("[|]"), [Arg, Tail],
                        TermContext))
                else
                    parser_unexpected("expecting ']' or operator", List,
                        !TokensLeft, !.PS)
                )
            ;
                Tail0 = pr_error(_, _),
                % Propagate error.
                List = Tail0
            )
        else if Token = close_list then
            Tail = term.functor(term.atom("[]"), [], TermContext),
            List = pr_ok(term.functor(term.atom("[|]"), [Arg, Tail],
                TermContext))
        else
            parser_unexpected_tok(Token, Context,
                "expected comma, `|', `]', or operator",
                List, !TokensLeft, !.PS)
        )
    ;
        !.TokensLeft = token_nil,
        % XXX The error message should state the line that the list started on.
        List = pr_error("unexpected end-of-file in list", !.TokensLeft)
    ).

:- pred parse_args(parse_result(list(term(T)))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_args(List, !TokensLeft, !PS) :-
    parse_arg(Arg0, !TokensLeft, !PS),
    (
        Arg0 = pr_ok(Arg),
        (
            !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
            ( if Token = comma then
                disable_warning [suspicious_recursion] (
                    parse_args(Tail0, !TokensLeft, !PS)
                ),
                (
                    Tail0 = pr_ok(Tail),
                    List = pr_ok([Arg|Tail])
                ;
                    Tail0 = pr_error(_, _),
                    % Propagate error upwards.
                    List = Tail0
                )
            else if Token = close then
                List = pr_ok([Arg])
            else
                parser_unexpected_tok(Token, Context,
                    "expected `,', `)', or operator", List, !TokensLeft, !.PS)
            )
        ;
            !.TokensLeft = token_nil,
            List = pr_error("unexpected end-of-file in argument list",
                !.TokensLeft)
        )
    ;
        Arg0 = pr_error(Message, Tokens),
        % Propagate error upwards.
        List = pr_error(Message, Tokens)
    ).

%---------------------------------------------------------------------------%

    % We encountered an error. See if the next token was an infix or postfix
    % operator. If so, it would normally form part of the term, so the error
    % must have been an operator precedence error. Otherwise, it was some
    % other sort of error, so issue the usual error message.
    %
:- pred parser_unexpected(string::in, parse_result(U)::out,
    token_list::in, token_list::out, parser_state(Ops, T)::in) is det
    <= op_table(Ops).

parser_unexpected(UsualMessage, Error, !TokensLeft, PS) :-
    (
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        parser_unexpected_tok(Token, Context, UsualMessage, Error,
            !TokensLeft, PS)
    ;
        !.TokensLeft = token_nil,
        Error = pr_error(UsualMessage, !.TokensLeft)
    ).

:- pred parser_unexpected_tok(token::in, token_context::in, string::in,
    parse_result(U)::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in) is det <= op_table(Ops).

parser_unexpected_tok(Token, Context, UsualMessage, Error, !TokensLeft, PS) :-
    % Push the token back, so that the error message points at *it*
    % rather than at the following token.
    !:TokensLeft = token_cons(Token, Context, !.TokensLeft),
    ( if
        ( Token = name(Op)
        ; Token = comma, Op = ","
        ),
        OpTable = parser_state_get_ops_table(PS),
        ( ops.lookup_infix_op(OpTable, Op, _, _, _)
        ; ops.lookup_postfix_op(OpTable, Op, _, _)
        )
    then
        Error = pr_error("operator precedence error", !.TokensLeft)
    else
        Error = pr_error(UsualMessage, !.TokensLeft)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % XXX OPS Rename.
:- pred check_priority(arg_prio_gt_or_ge::in, priority::in, priority::in)
    is semidet.

check_priority(arg_ge, prio(OpPriority), prio(Priority)) :-
    Priority >= OpPriority.
check_priority(arg_gt, prio(OpPriority), prio(Priority)) :-
    Priority > OpPriority.

:- pred parser_get_term_context(parser_state(Ops, T)::in, token_context::in,
    term_context::out) is det.

parser_get_term_context(ParserState, TokenContext, TermContext) :-
    FileName = parser_state_get_stream_name(ParserState),
    TermContext = term_context.context_init(FileName, TokenContext).

%---------------------------------------------------------------------------%

:- pred could_start_term(token::in, bool::out) is det.

could_start_term(name(_), yes).
could_start_term(variable(_), yes).
could_start_term(integer(_, _, _, _), yes).
could_start_term(float(_), yes).
could_start_term(string(_), yes).
could_start_term(implementation_defined(_), yes).
could_start_term(open, yes).
could_start_term(open_ct, yes).
could_start_term(close, no).
could_start_term(open_list, yes).
could_start_term(close_list, no).
could_start_term(open_curly, yes).
could_start_term(close_curly, no).
could_start_term(ht_sep, no).
could_start_term(comma, no).
could_start_term(end, no).
could_start_term(junk(_), no).
could_start_term(error(_), no).
could_start_term(io_error(_), no).
could_start_term(eof, no).
could_start_term(integer_dot(_), no).

%---------------------------------------------------------------------------%

:- func lexer_base_to_term_base(mercury_term_lexer.integer_base)
    = term.integer_base.

lexer_base_to_term_base(base_2) = base_2.
lexer_base_to_term_base(base_8) = base_8.
lexer_base_to_term_base(base_10) = base_10.
lexer_base_to_term_base(base_16) = base_16.

:- func lexer_signedness_to_term_signedness(mercury_term_lexer.signedness)
    = term.signedness.

lexer_signedness_to_term_signedness(unsigned) = unsigned.
lexer_signedness_to_term_signedness(signed) = signed.

:- func lexer_size_to_term_size(mercury_term_lexer.integer_size)
    = term.integer_size.

lexer_size_to_term_size(size_word) = size_word.
lexer_size_to_term_size(size_8_bit) = size_8_bit.
lexer_size_to_term_size(size_16_bit) = size_16_bit.
lexer_size_to_term_size(size_32_bit) = size_32_bit.
lexer_size_to_term_size(size_64_bit) = size_64_bit.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The representation of the parser state apart from the remaining token list.
%

:- type parser_state(Ops, T)   % <= op_table(Ops)
    --->    parser_state(
                % The name of the stream being parsed.
                ps_stream_name  :: string,

                % The current set of operators.
                ps_ops_table    :: Ops,

                % The names of the variables in the term being parsed.
                ps_varset       :: varset(T),

                % A map from variable names to variables. We use it to decide
                % whether we have seen a variable before, or whether we have
                % to create it.
                ps_var_names    :: map(string, var(T))
            ).

:- pred init_parser_state(Ops::in, string::in, parser_state(Ops, T)::out)
    is det <= op_table(Ops).

init_parser_state(Ops, FileName, ParserState) :-
    varset.init(VarSet),
    map.init(Names),
    ParserState = parser_state(FileName, Ops, VarSet, Names).

:- pred final_parser_state(parser_state(Ops, T)::in, varset(T)::out) is det.

final_parser_state(ParserState, VarSet) :-
    VarSet = parser_state_get_varset(ParserState).

%---------------------------------------------------------------------------%

:- func parser_state_get_stream_name(parser_state(Ops, T)) = string.
:- func parser_state_get_ops_table(parser_state(Ops, T)) = Ops.
:- func parser_state_get_varset(parser_state(Ops, T)) = varset(T).
:- func parser_state_get_var_names(parser_state(Ops, T)) = map(string, var(T)).

:- pred parser_state_set_varset(varset(T)::in,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det.
:- pred parser_state_set_var_names(map(string, var(T))::in,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det.

% If you want profiling to tell you the frequencies of these operations,
% change the inline pragmas to no_inline pragmas.

:- pragma inline(func(parser_state_get_stream_name/1)).
:- pragma inline(func(parser_state_get_ops_table/1)).
:- pragma inline(func(parser_state_get_varset/1)).
:- pragma inline(func(parser_state_get_var_names/1)).

:- pragma inline(pred(parser_state_set_varset/3)).
:- pragma inline(pred(parser_state_set_var_names/3)).

parser_state_get_stream_name(ParserState) = X :-
    X = ParserState ^ ps_stream_name.
parser_state_get_ops_table(ParserState) = X :-
    X = ParserState ^ ps_ops_table.
parser_state_get_varset(ParserState) = X :-
    X = ParserState ^ ps_varset.
parser_state_get_var_names(ParserState) = X :-
    X = ParserState ^ ps_var_names.

parser_state_set_varset(X, !ParserState) :-
    !ParserState ^ ps_varset := X.
parser_state_set_var_names(X, !ParserState) :-
    !ParserState ^ ps_var_names := X.

:- pred add_var(string::in, var(T)::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det.

add_var(VarName, Var, !ParserState) :-
    ( if VarName = "_" then
        VarSet0 = parser_state_get_varset(!.ParserState),
        varset.new_var(Var, VarSet0, VarSet),
        parser_state_set_varset(VarSet, !ParserState)
    else
        Names0 = parser_state_get_var_names(!.ParserState),
        ( if map.search(Names0, VarName, Var0) then
            Var = Var0
        else
            VarSet0 = parser_state_get_varset(!.ParserState),
            varset.new_named_var(VarName, Var, VarSet0, VarSet),
            map.det_insert(VarName, Var, Names0, Names),
            parser_state_set_varset(VarSet, !ParserState),
            parser_state_set_var_names(Names, !ParserState)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module mercury_term_parser.
%---------------------------------------------------------------------------%
