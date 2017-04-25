%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2001, 2003-2008, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parser.m.
% Main author: fjh.
% Stability: high.
%
% This file exports the predicate read_term, which reads
% a term from the current input stream.
% The read_term_from_string predicates are the same as the
% read_term predicates, except that the term is read from
% a string rather than from the current input stream.
% The parse_token_list predicate is similar,
% but it takes a list of tokens rather than a string.
%
% The parser and lexer are intended to exactly follow ISO Prolog
% syntax, but there are some departures from that for three reasons:
%
%   (1) I wrote some of the code at home when the ISO Prolog draft
%       was at uni - so in some places I just guessed.
%   (2) In some places the lexer reports an error when it shouldn't.
%   (3) There are a couple of hacks to make it compatible with NU-Prolog
%       syntax.
%
% The parser is a relatively straight-forward top-down recursive descent
% parser, made somewhat complicated by the need to handle operator
% precedences.  It uses `lexer.get_token_list' to read a list of tokens.
% It uses the routines in module `ops' to look up operator precedences.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parser.
:- interface.

:- import_module io.
:- import_module lexer.
:- import_module ops.
:- import_module term_io.

%---------------------------------------------------------------------------%

    % read_term(Result, !IO):
    % read_term(Stream, Result, !IO):
    %
    % Reads a Mercury term from the current input stream or from Stream.
    %
:- pred read_term(read_term(T)::out, io::di, io::uo) is det.
:- pred read_term(io.text_input_stream::in, read_term(T)::out,
    io::di, io::uo) is det.

    % read_term_with_op_table(Ops, Result, !IO):
    % read_term_with_op_table(Stream, Ops, Result, !IO):
    %
    % Reads a term from the current input stream or from Stream,
    % using the given op_table to interpret the operators.
    %
:- pred read_term_with_op_table(Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).
:- pred read_term_with_op_table(io.text_input_stream::in, Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).

    % read_term_filename(FileName, Result, !IO):
    % read_term_filename(Stream, FileName, Result, !IO):
    %
    % Reads a term from the current input stream or from Stream.
    % The string is the filename to use for the stream; this is used
    % in constructing the term.contexts in the read term.
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
    % the current input stream. The returned value `EndPos' is the position
    % one character past the end of the term read. The arguments `MaxOffset'
    % and `StartPos' in the six-argument version specify the length of the
    % string and the position within the string at which to start parsing.

    % read_term_from_string(FileName, String, EndPos, Term).
    %
:- pred read_term_from_string(string::in, string::in, posn::out,
    read_term(T)::out) is det.

    % read_term_from_string_with_op_table(Ops, FileName,
    %   String, EndPos, Term).
    %
:- pred read_term_from_string_with_op_table(Ops::in, string::in,
    string::in, posn::out, read_term(T)::out) is det <= op_table(Ops).

    % read_term_from_string(FileName, String, MaxOffset, StartPos,
    %   EndPos, Term).
    %
:- pred read_term_from_substring(string::in, string::in, int::in,
    posn::in, posn::out, read_term(T)::out) is det.

    % read_term_from_string_with_op_table(Ops, FileName, String,
    %   MaxOffset, StartPos, EndPos, Term).
    %
:- pred read_term_from_substring_with_op_table(Ops::in, string::in,
    string::in, int::in, posn::in, posn::out, read_term(T)::out) is det
    <= op_table(Ops).

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
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type parse(T)
    --->    ok(T)
    ;       error(string, token_list).

    % Are we parsing an ordinary term, an argument or a list element?
:- type term_kind
    --->    ordinary_term
    ;       argument
    ;       list_elem.

%---------------------------------------------------------------------------%

read_term(Result, !IO) :-
    io.input_stream(Stream, !IO),
    parser.read_term(Stream, Result, !IO).

read_term(Stream, Result, !IO) :-
    io.input_stream_name(Stream, FileName, !IO),
    parser.read_term_filename_with_op_table(Stream, ops.init_mercury_op_table,
        FileName, Result, !IO).

read_term_with_op_table(Ops, Result, !IO) :-
    io.input_stream(Stream, !IO),
    parser.read_term_with_op_table(Stream, Ops, Result, !IO).

read_term_with_op_table(Stream, Ops, Result, !IO) :-
    io.input_stream_name(Stream, FileName, !IO),
    parser.read_term_filename_with_op_table(Stream, Ops,
        FileName, Result, !IO).

read_term_filename(FileName, Result, !IO) :-
    io.input_stream(Stream, !IO),
    parser.read_term_filename(Stream, FileName, Result, !IO).

read_term_filename(Stream, FileName, Result, !IO) :-
    parser.read_term_filename_with_op_table(Stream, ops.init_mercury_op_table,
        FileName, Result, !IO).

read_term_filename_with_op_table(Ops, FileName, Result, !IO) :-
    io.input_stream(Stream, !IO),
    parser.read_term_filename_with_op_table(Stream, Ops,
        FileName, Result, !IO).

read_term_filename_with_op_table(Stream, Ops, FileName, Result, !IO) :-
    lexer.get_token_list(Stream, Tokens, !IO),
    parser.parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%---------------------%

read_term_from_string(FileName, String, EndPos, Result) :-
    parser.read_term_from_string_with_op_table(ops.init_mercury_op_table,
        FileName, String, EndPos, Result).

read_term_from_string_with_op_table(Ops, FileName, String, EndPos, Result) :-
    string.length(String, Len),
    StartPos = posn(1, 0, 0),
    parser.read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result).

read_term_from_substring(FileName, String, Len, StartPos, EndPos, Result) :-
    parser.read_term_from_substring_with_op_table(ops.init_mercury_op_table,
        FileName, String, Len, StartPos, EndPos, Result).

read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result) :-
    lexer.string_get_token_list_max(String, Len, Tokens, StartPos, EndPos),
    parser.parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%---------------------------------------------------------------------------%

parse_tokens(FileName, Tokens, Result) :-
    parser.parse_tokens_with_op_table(ops.init_mercury_op_table,
        FileName, Tokens, Result).

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

:- pred check_for_errors(parse(term(T))::in, varset(T)::in,
    token_list::in, token_list::in, read_term(T)::out) is det.

check_for_errors(Parse, VarSet, Tokens, LeftOverTokens, Result) :-
    (
        Parse = error(ErrorMessage, ErrorTokens),
        % Check if the error was caused by a bad token.
        ( if check_for_bad_token(Tokens, BadTokenMessage, BadTokenLineNum) then
            Message = BadTokenMessage,
            LineNum = BadTokenLineNum
        else
            % Find the token that caused the error.
            (
                ErrorTokens = token_cons(ErrorTok, ErrorTokLineNum, _),
                lexer.token_to_string(ErrorTok, TokString),
                Message =
                    "Syntax error at " ++ TokString ++ ": " ++ ErrorMessage,
                LineNum = ErrorTokLineNum
            ;
                ErrorTokens = token_nil,
                (
                    Tokens = token_cons(_, LineNum, _)
                ;
                    Tokens = token_nil,
                    error("check_for_errors")
                ),
                Message = "Syntax error: " ++ ErrorMessage
            )
        ),
        Result = error(Message, LineNum)
    ;
        Parse = ok(Term),
        ( if check_for_bad_token(Tokens, Message, LineNum) then
            Result = error(Message, LineNum)
        else
            (
                LeftOverTokens = token_cons(Token, LineNum, _),
                lexer.token_to_string(Token, TokString),
                Message = "Syntax error: unexpected " ++ TokString,
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
        string.append("I/O error: ", IO_ErrorMessage, Message),
        LineNum = LineNum0
    ;
        Token = junk(Char),
        char.to_int(Char, Code),
        string.int_to_base_string(Code, 10, Decimal),
        string.int_to_base_string(Code, 16, Hex),
        string.append_list(["Syntax error: Illegal character 0x", Hex,
            " (", Decimal, ") in input"], Message),
        LineNum = LineNum0
    ;
        Token = error(ErrorMessage),
        string.append("Syntax error: ", ErrorMessage, Message),
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

:- pred parse_whole_term(parse(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_whole_term(Term, !TokensLeft, !PS) :-
    parse_term(Term0, !TokensLeft, !PS),
    (
        Term0 = ok(_),
        ( if !.TokensLeft = token_cons(end, _Context, !:TokensLeft) then
            Term = Term0
        else
            parser_unexpected("operator or `.' expected", Term,
                !TokensLeft, !.PS)
        )
    ;
        % Propagate error upwards.
        Term0 = error(_, _),
        Term = Term0
    ).

:- pred parse_term(parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_term(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    do_parse_term(ops.max_priority(OpTable) + 1, ordinary_term, Term,
        !TokensLeft, !PS).

:- pred parse_arg(parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_arg(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse '::'/2 in arguments
    % the way we want to.  Perhaps a better solution would be to change the
    % priority of '::'/2, but we need to analyse the impact of that further.
    ArgPriority = ops.max_priority(OpTable) + 1,
    do_parse_term(ArgPriority, argument, Term, !TokensLeft, !PS).

:- pred parse_list_elem(parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_list_elem(Term, !TokensLeft, !PS) :-
    OpTable = parser_state_get_ops_table(!.PS),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse promise_pure/0 in
    % foreign attribute lists.
    ArgPriority = ops.max_priority(OpTable) + 1,
    do_parse_term(ArgPriority, list_elem, Term, !TokensLeft, !PS).

:- pred do_parse_term(int::in, term_kind::in, parse(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

do_parse_term(MaxPriority, TermKind, Term, !TokensLeft, !PS) :-
    parse_left_term(MaxPriority, TermKind, LeftPriority, LeftTerm0,
        !TokensLeft, !PS),
    (
        LeftTerm0 = ok(LeftTerm),
        parse_rest(MaxPriority, TermKind, LeftPriority, LeftTerm, Term,
            !TokensLeft, !PS)
    ;
        LeftTerm0 = error(_, _),
        % propagate error upwards
        Term = LeftTerm0
    ).

:- pred parse_left_term(int::in, term_kind::in, int::out, parse(term(T))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_left_term(MaxPriority, TermKind, OpPriority, Term, !TokensLeft, !PS) :-
    (
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        ( if
            % Check for unary minus of an integer or a float.
            Token = name(TokenName),
            TokenName = "-",
            !.TokensLeft =
                token_cons(NextToken, _NextContext, !:TokensLeft),
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
            Term = ok(term.functor(NewFunctor, [], TermContext)),
            OpPriority = 0
        else if
            Token = name(TokenName),
            OpTable = parser_state_get_ops_table(!.PS),
            ops.lookup_op_infos(OpTable, TokenName, OpInfo, OtherOpInfos)
        then
            ( if
                % Check for binary prefix op.
                %
                % Since most tokens aren't binary prefix ops, the first test
                % here will almost always fail.
                find_first_binary_prefix_op(OpInfo, OtherOpInfos,
                    BinOpPriority, RightAssoc, RightRightAssoc),
                BinOpPriority =< MaxPriority,
                !.TokensLeft = token_cons(NextToken, _, _),
                could_start_term(NextToken, yes),
                NextToken \= open_ct

            then
                OpPriority = BinOpPriority,
                adjust_priority_for_assoc(OpPriority,
                    RightAssoc, RightPriority),
                adjust_priority_for_assoc(OpPriority,
                    RightRightAssoc, RightRightPriority),
                do_parse_term(RightPriority, TermKind, RightResult,
                    !TokensLeft, !PS),
                (
                    RightResult = ok(RightTerm),
                    do_parse_term(RightRightPriority, TermKind,
                        RightRightResult, !TokensLeft, !PS),
                    (
                        RightRightResult = ok(RightRightTerm),
                        parser_get_term_context(!.PS, Context, TermContext),
                        Term = ok(term.functor(term.atom(TokenName),
                            [RightTerm, RightRightTerm], TermContext))
                    ;
                        RightRightResult = error(_, _),
                        % Propagate error upwards.
                        Term = RightRightResult
                    )
                ;
                    RightResult = error(_, _),
                    % Propagate error upwards.
                    Term = RightResult
                )
            else if
                % Check for prefix op.
                %
                % Since most tokens aren't prefix ops, the first test
                % here will almost always fail.
                find_first_prefix_op(OpInfo, OtherOpInfos,
                    UnOpPriority, RightAssoc),
                UnOpPriority =< MaxPriority,
                !.TokensLeft = token_cons(NextToken, _, _),
                could_start_term(NextToken, yes),
                NextToken \= open_ct
            then
                OpPriority = UnOpPriority,
                adjust_priority_for_assoc(OpPriority, RightAssoc,
                    RightPriority),
                do_parse_term(RightPriority, TermKind, RightResult,
                    !TokensLeft, !PS),
                (
                    RightResult = ok(RightTerm),
                    parser_get_term_context(!.PS, Context, TermContext),
                    Term = ok(term.functor(term.atom(TokenName), [RightTerm],
                        TermContext))
                ;
                    RightResult = error(_, _),
                    % Propagate error upwards.
                    Term = RightResult
                )
            else
                % TokenName is an operator, but not of a kind that
                % we should handle here.
                parse_simple_term(Token, Context, MaxPriority, Term,
                    !TokensLeft, !PS),
                OpPriority = 0
            )
        else
            % TokenName is not an operator.
            parse_simple_term(Token, Context, MaxPriority, Term,
                !TokensLeft, !PS),
            OpPriority = 0
        )
    ;
        !.TokensLeft = token_nil,
        Term = error("unexpected end-of-file at start of sub-term",
            !.TokensLeft),
        OpPriority = 0
    ).

:- pred parse_rest(int::in, term_kind::in, int::in, term(T)::in,
    parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_rest(MaxPriority, TermKind, LeftPriority, LeftTerm, Term,
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
                LeftAssoc0, RightAssoc0)
        then
            OpPriority = OpPriority0,
            LeftAssoc = LeftAssoc0,
            RightAssoc = RightAssoc0,
            parse_backquoted_operator(MaybeQualifier, Op, VariableTerms,
                !TokensLeft, !PS),
            !.TokensLeft = token_cons(name("`"), _Context, !:TokensLeft)
        else
            Op = Op0,
            VariableTerms = [],
            MaybeQualifier = no,
            OpTable = parser_state_get_ops_table(!.PS),
            ops.lookup_infix_op(OpTable, Op, OpPriority, LeftAssoc, RightAssoc)
        ),
        OpPriority =< MaxPriority,
        check_priority(LeftAssoc, OpPriority, LeftPriority)
    then
        adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
        do_parse_term(RightPriority, TermKind, RightTerm0, !TokensLeft, !PS),
        (
            RightTerm0 = ok(RightTerm),
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
            parse_rest(MaxPriority, TermKind, OpPriority, OpTerm, Term,
                !TokensLeft, !PS)
        ;
            RightTerm0 = error(_, _),
            % Propagate error upwards.
            Term = RightTerm0
        )
    else if
        % Postfix op.
        !.TokensLeft = token_cons(name(Op), Context, !:TokensLeft),
        OpTable = parser_state_get_ops_table(!.PS),
        ops.lookup_postfix_op(OpTable, Op, OpPriority, LeftAssoc),
        OpPriority =< MaxPriority,
        check_priority(LeftAssoc, OpPriority, LeftPriority)
    then
        parser_get_term_context(!.PS, Context, TermContext),
        OpTerm = term.functor(term.atom(Op), [LeftTerm], TermContext),
        parse_rest(MaxPriority, TermKind, OpPriority, OpTerm, Term,
            !TokensLeft, !PS)
    else
        Term = ok(LeftTerm)
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
    maybe(term(T))::in, maybe(term(T))::out, term.context::in, string::in,
    string::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_backquoted_operator_qualifier(MaybeQualifier0, MaybeQualifier, OpCtxt0,
        OpName0, OpName, !TokensLeft, !PS) :-
    ( if
        !.TokensLeft =
            token_cons(name(ModuleSeparator), SepContext, !:TokensLeft),
        ( ModuleSeparator = "."
        ; ModuleSeparator = ":"
        ),
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

    % term --> integer              % priority 0
    % term --> float                % priority 0
    % term --> implementation_defined % priority 0
    % term --> name("-") integer    % priority 0
    % term --> name("-") float      % priority 0
    % term --> atom(NonOp)          % priority 0
    % term --> atom(Op)             % priority `max_priority' + 1
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

:- pred parse_simple_term(token::in, token_context::in, int::in,
    parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_simple_term(Token, Context, Prec, TermParse, !TokensLeft, !PS) :-
    (
        Token = name(Atom),
        parser_get_term_context(!.PS, Context, TermContext),
        ( if !.TokensLeft = token_cons(open_ct, _Context, !:TokensLeft) then
            parse_args(ArgsParse, !TokensLeft, !PS),
            (
                ArgsParse = ok(Args),
                BaseTerm = functor(atom(Atom), Args, TermContext),
                BaseTermParse = ok(BaseTerm)
            ;
                ArgsParse = error(Message, Tokens),
                % Propagate error upwards, after changing type.
                BaseTermParse = error(Message, Tokens)
            )
        else
            OpTable = parser_state_get_ops_table(!.PS),
            ( if
                ops.lookup_op(OpTable, Atom),
                Prec =< ops.max_priority(OpTable)
            then
                parser_unexpected_tok(Token, Context,
                    "unexpected token at start of (sub)term",
                    BaseTermParse, !TokensLeft, !.PS)
            else
                BaseTerm = functor(atom(Atom), [], TermContext),
                BaseTermParse = ok(BaseTerm)
            )
        )
    ;
        Token = variable(VarName),
        add_var(VarName, Var, !PS),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = term.variable(Var, TermContext),
        BaseTermParse = ok(BaseTerm)
    ;
        Token = integer(LexerBase, Integer, LexerSignedness, LexerSize),
        Base = lexer_base_to_term_base(LexerBase),
        Signedness = lexer_signedness_to_term_signedness(LexerSignedness),
        Size = lexer_size_to_term_size(LexerSize),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(integer(Base, Integer, Signedness, Size), [],
            TermContext),
        BaseTermParse = ok(BaseTerm)
    ;
        Token = float(Float),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(float(Float), [], TermContext),
        BaseTermParse = ok(BaseTerm)
    ;
        Token = string(String),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(string(String), [], TermContext),
        BaseTermParse = ok(BaseTerm)
    ;
        Token = implementation_defined(Name),
        parser_get_term_context(!.PS, Context, TermContext),
        BaseTerm = functor(implementation_defined(Name), [], TermContext),
        BaseTermParse = ok(BaseTerm)
    ;
        ( Token = open
        ; Token = open_ct
        ),
        parse_term(SubTermParse, !TokensLeft, !PS),
        (
            SubTermParse = ok(_),
            ( if !.TokensLeft = token_cons(close, _Context, !:TokensLeft) then
                BaseTermParse = SubTermParse
            else
                parser_unexpected("expecting `)' or operator", BaseTermParse,
                    !TokensLeft, !.PS)
            )
        ;
            % Propagate error upwards.
            SubTermParse = error(_, _),
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
                SubTermParse = ok(SubTerm),
                conjunction_to_list(SubTerm, ArgTerms),
                ( if
                    !.TokensLeft = token_cons(close_curly, _Context,
                        !:TokensLeft)
                then
                    BaseTerm = functor(atom("{}"), ArgTerms, TermContext),
                    BaseTermParse = ok(BaseTerm)
                else
                    parser_unexpected("expecting `}' or operator",
                        BaseTermParse, !TokensLeft, !.PS)
                )
            ;
                SubTermParse = error(_, _),
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
        BaseTermParse = ok(BaseTermOpen),
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
    parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_higher_order_term_rest(BaseTerm, Context, TermParse, !TokensLeft, !PS) :-
    parser_get_term_context(!.PS, Context, TermContext),
    parse_args(ArgsParse, !TokensLeft, !PS),
    (
        ArgsParse = ok(Args),
        ApplyTerm = functor(atom(""), [BaseTerm | Args], TermContext),
        ( if
            !.TokensLeft = token_cons(open_ct, _OpenContext, !:TokensLeft)
        then
            parse_higher_order_term_rest(ApplyTerm, Context, TermParse,
                !TokensLeft, !PS)
        else
            TermParse = ok(ApplyTerm)
        )
    ;
        ArgsParse = error(Message, Tokens),
        % Propagate error upwards, after changing type.
        TermParse = error(Message, Tokens)
    ).

:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

conjunction_to_list(Term, ArgTerms) :-
    ( if Term = term.functor(term.atom(","), [LeftTerm, RightTerm], _) then
        conjunction_to_list(RightTerm, ArgTerms0),
        ArgTerms = [LeftTerm | ArgTerms0]
    else
        ArgTerms = [Term]
    ).

:- pred parse_special_atom(string::in, term.context::in,
    parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_special_atom(Atom, TermContext, Term, !TokensLeft, !PS) :-
    ( if !.TokensLeft = token_cons(open_ct, _Context, !:TokensLeft) then
        parse_args(Args0, !TokensLeft, !PS),
        (
            Args0 = ok(Args),
            Term = ok(term.functor(term.atom(Atom), Args, TermContext))
        ;
            % Propagate error upwards.
            Args0 = error(Message, Tokens),
            Term = error(Message, Tokens)
        )
    else
        Term = ok(term.functor(term.atom(Atom), [], TermContext))
    ).

:- pred parse_list(parse(term(T))::out, token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_list(List, !TokensLeft, !PS) :-
    parse_list_elem(Arg0, !TokensLeft, !PS),
    (
        Arg0 = ok(Arg),
        parse_list_tail(Arg, List, !TokensLeft, !PS)
    ;
        Arg0 = error(_, _),
        % Propagate error.
        List = Arg0
    ).

:- pred parse_list_tail(term(T)::in, parse(term(T))::out,
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
                Tail0 = ok(Tail),
                List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                    TermContext))
            ;
                Tail0 = error(_, _),
                % Propagate error.
                List = Tail0
            )
        else if Token = ht_sep then
            parse_arg(Tail0, !TokensLeft, !PS),
            (
                Tail0 = ok(Tail),
                ( if
                    !.TokensLeft = token_cons(close_list, _Context,
                        !:TokensLeft)
                then
                    List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                        TermContext))
                else
                    parser_unexpected("expecting ']' or operator", List,
                        !TokensLeft, !.PS)
                )
            ;
                Tail0 = error(_, _),
                % Propagate error.
                List = Tail0
            )
        else if Token = close_list then
            Tail = term.functor(term.atom("[]"), [], TermContext),
            List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                TermContext))
        else
            parser_unexpected_tok(Token, Context,
                "expected comma, `|', `]', or operator",
                List, !TokensLeft, !.PS)
        )
    ;
        !.TokensLeft = token_nil,
        % XXX The error message should state the line that the list started on.
        List = error("unexpected end-of-file in list", !.TokensLeft)
    ).

:- pred parse_args(parse(list(term(T)))::out,
    token_list::in, token_list::out,
    parser_state(Ops, T)::in, parser_state(Ops, T)::out) is det
    <= op_table(Ops).

parse_args(List, !TokensLeft, !PS) :-
    parse_arg(Arg0, !TokensLeft, !PS),
    (
        Arg0 = ok(Arg),
        (
            !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
            ( if Token = comma then
                parse_args(Tail0, !TokensLeft, !PS),
                (
                    Tail0 = ok(Tail),
                    List = ok([Arg|Tail])
                ;
                    Tail0 = error(_, _),
                    % Propagate error upwards.
                    List = Tail0
                )
            else if Token = close then
                List = ok([Arg])
            else
                parser_unexpected_tok(Token, Context,
                    "expected `,', `)', or operator", List, !TokensLeft, !.PS)
            )
        ;
            !.TokensLeft = token_nil,
            List = error("unexpected end-of-file in argument list",
                !.TokensLeft)
        )
    ;
        Arg0 = error(Message, Tokens),
        % Propagate error upwards.
        List = error(Message, Tokens)
    ).

%---------------------------------------------------------------------------%

    % We encountered an error. See if the next token was an infix or postfix
    % operator. If so, it would normally form part of the term, so the error
    % must have been an operator precedence error. Otherwise, it was some
    % other sort of error, so issue the usual error message.
    %
:- pred parser_unexpected(string::in, parse(U)::out,
    token_list::in, token_list::out, parser_state(Ops, T)::in) is det
    <= op_table(Ops).

parser_unexpected(UsualMessage, Error, !TokensLeft, PS) :-
    (
        !.TokensLeft = token_cons(Token, Context, !:TokensLeft),
        parser_unexpected_tok(Token, Context, UsualMessage, Error,
            !TokensLeft, PS)
    ;
        !.TokensLeft = token_nil,
        Error = error(UsualMessage, !.TokensLeft)
    ).

:- pred parser_unexpected_tok(token::in, token_context::in, string::in,
    parse(U)::out, token_list::in, token_list::out, parser_state(Ops, T)::in)
    is det <= op_table(Ops).

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
        Error = error("operator precedence error", !.TokensLeft)
    else
        Error = error(UsualMessage, !.TokensLeft)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred find_first_prefix_op(op_info::in, list(op_info)::in,
    ops.priority::out, ops.assoc::out) is semidet.

find_first_prefix_op(OpInfo, OtherOpInfos, OpPriority, RightAssoc) :-
    OpInfo = op_info(Class, Priority),
    ( if Class = prefix(RightAssocPrime) then
        OpPriority = Priority,
        RightAssoc = RightAssocPrime
    else
        OtherOpInfos = [HeadOpInfo | TailOpInfos],
        find_first_prefix_op(HeadOpInfo, TailOpInfos, OpPriority, RightAssoc)
    ).

:- pred find_first_binary_prefix_op(op_info::in, list(op_info)::in,
    ops.priority::out, ops.assoc::out, ops.assoc::out) is semidet.

find_first_binary_prefix_op(OpInfo, OtherOpInfos,
        OpPriority, RightAssoc, RightRightAssoc) :-
    OpInfo = op_info(Class, Priority),
    ( if Class = binary_prefix(RightAssocPrime, RightRightAssocPrime) then
        OpPriority = Priority,
        RightAssoc = RightAssocPrime,
        RightRightAssoc = RightRightAssocPrime
    else
        OtherOpInfos = [HeadOpInfo | TailOpInfos],
        find_first_binary_prefix_op(HeadOpInfo, TailOpInfos,
            OpPriority, RightAssoc, RightRightAssoc)
    ).

%---------------------------------------------------------------------------%

:- pred check_priority(ops.assoc::in, int::in, int::in) is semidet.

check_priority(y, MaxPriority, Priority) :-
    Priority =< MaxPriority.
check_priority(x, MaxPriority, Priority) :-
    Priority < MaxPriority.

:- pred parser_get_term_context(parser_state(Ops, T)::in, token_context::in,
    term.context::out) is det.

parser_get_term_context(ParserState, TokenContext, TermContext) :-
    FileName = parser_state_get_stream_name(ParserState),
    term.context_init(FileName, TokenContext, TermContext).

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

:- func lexer_base_to_term_base(lexer.integer_base) = term.integer_base.

lexer_base_to_term_base(base_2) = base_2.
lexer_base_to_term_base(base_8) = base_8.
lexer_base_to_term_base(base_10) = base_10.
lexer_base_to_term_base(base_16) = base_16.

:- func lexer_signedness_to_term_signedness(lexer.signedness)
    = term.signedness.

lexer_signedness_to_term_signedness(unsigned) = unsigned.
lexer_signedness_to_term_signedness(signed) = signed.

:- func lexer_size_to_term_size(lexer.integer_size) = term.integer_size.

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

:- pragma inline(parser_state_get_stream_name/1).
:- pragma inline(parser_state_get_ops_table/1).
:- pragma inline(parser_state_get_varset/1).
:- pragma inline(parser_state_get_var_names/1).

:- pragma inline(parser_state_set_varset/3).
:- pragma inline(parser_state_set_var_names/3).

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
%---------------------------------------------------------------------------%
