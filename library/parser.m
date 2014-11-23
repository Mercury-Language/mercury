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

    % read_term(Result):
    %
    % Reads a Mercury term from the current input stream.
    %
:- pred read_term(read_term(T)::out, io::di, io::uo) is det.

    % read_term_with_op_table(Result):
    %
    % Reads a term from the current input stream, using the given op_table
    % to interpret the operators.
    %
:- pred read_term_with_op_table(Ops::in, read_term(T)::out, io::di, io::uo)
    is det <= op_table(Ops).

    % read_term_filename(FileName, Result, !IO):
    %
    % Reads a term from the current input stream. The string is the filename
    % to use for the current input stream; this is used in constructing the
    % term.contexts in the read term. This interface is used to support
    % the `:- pragma source_file' directive.
    %
:- pred read_term_filename(string::in, read_term(T)::out, io::di, io::uo)
    is det.

    % read_term_filename_with_op_table(Ops, FileName, Result, !IO):
    %
    % As above but using the given op_table.
    %
:- pred read_term_filename_with_op_table(Ops::in, string::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).

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
    io.input_stream_name(FileName, !IO),
    read_term_filename_with_op_table(ops.init_mercury_op_table, FileName,
        Result, !IO).

read_term_with_op_table(Ops, Result, !IO) :-
    io.input_stream_name(FileName, !IO),
    read_term_filename_with_op_table(Ops, FileName, Result, !IO).

read_term_filename(FileName, Result, !IO) :-
    read_term_filename_with_op_table(ops.init_mercury_op_table, FileName,
        Result, !IO).

read_term_filename_with_op_table(Ops, FileName, Result, !IO) :-
    lexer.get_token_list(Tokens, !IO),
    parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

read_term_from_string(FileName, String, EndPos, Result) :-
    read_term_from_string_with_op_table(ops.init_mercury_op_table, FileName,
        String, EndPos, Result).

read_term_from_string_with_op_table(Ops, FileName, String, EndPos, Result) :-
    string.length(String, Len),
    StartPos = posn(1, 0, 0),
    read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result).

read_term_from_substring(FileName, String, Len, StartPos, EndPos, Result) :-
    read_term_from_substring_with_op_table(ops.init_mercury_op_table,
        FileName, String, Len, StartPos, EndPos, Result).

read_term_from_substring_with_op_table(Ops, FileName, String, Len,
        StartPos, EndPos, Result) :-
    lexer.string_get_token_list_max(String, Len, Tokens, StartPos, EndPos),
    parse_tokens_with_op_table(Ops, FileName, Tokens, Result).

%---------------------------------------------------------------------------%

parse_tokens(FileName, Tokens, Result) :-
    parse_tokens_with_op_table(ops.init_mercury_op_table, FileName, Tokens,
        Result).

parse_tokens_with_op_table(Ops, FileName, Tokens, Result) :-
    (
        Tokens = token_nil,
        Result = eof
    ;
        Tokens = token_cons(_, _, _),
        init_parser_state(Ops, FileName, Tokens, ParserState0),
        parse_whole_term(Term, ParserState0, ParserState),
        final_parser_state(ParserState, VarSet, LeftOverTokens),
        check_for_errors(Term, VarSet, Tokens, LeftOverTokens, Result)
    ).

:- pred check_for_errors(parse(term(T))::in, varset(T)::in,
    token_list::in, token_list::in, read_term(T)::out) is det.

check_for_errors(error(ErrorMessage, ErrorTokens), _VarSet, Tokens,
        _LeftOverTokens, Result) :-
    % Check if the error was caused by a bad token.
    ( check_for_bad_token(Tokens, BadTokenMessage, BadTokenLineNum) ->
        Message = BadTokenMessage,
        LineNum = BadTokenLineNum
    ;
        % Find the token that caused the error.
        (
            ErrorTokens = token_cons(ErrorTok, ErrorTokLineNum, _),
            lexer.token_to_string(ErrorTok, TokString),
            Message = "Syntax error at " ++ TokString ++ ": " ++ ErrorMessage,
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
    Result = error(Message, LineNum).

check_for_errors(ok(Term), VarSet, Tokens, LeftOverTokens, Result) :-
    ( check_for_bad_token(Tokens, Message, LineNum) ->
        Result = error(Message, LineNum)
    ;
        (
            LeftOverTokens = token_cons(Token, LineNum, _),
            lexer.token_to_string(Token, TokString),
            Message = "Syntax error: unexpected " ++ TokString,
            Result = error(Message, LineNum)
        ;
            LeftOverTokens = token_nil,
            Result = term(VarSet, Term)
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
        ; Token = integer(_)
        ; Token = big_integer(_)
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
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_whole_term(Term, !PS) :-
    parse_term(Term0, !PS),
    (
        Term0 = ok(_),
        ( parser_get_token(end, !PS) ->
            Term = Term0
        ;
            parser_unexpected("operator or `.' expected", Term, !PS)
        )
    ;
        % Propagate error upwards.
        Term0 = error(_, _),
        Term = Term0
    ).

:- pred parse_term(parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_term(Term, !PS) :-
    get_ops_table(!.PS, OpTable),
    parse_term_2(ops.max_priority(OpTable) + 1, ordinary_term, Term, !PS).

:- pred parse_arg(parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_arg(Term, !PS) :-
    get_ops_table(!.PS, OpTable),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse '::'/2 in arguments
    % the way we want to.  Perhaps a better solution would be to change the
    % priority of '::'/2, but we need to analyse the impact of that further.
    ArgPriority = ops.max_priority(OpTable) + 1,
    parse_term_2(ArgPriority, argument, Term, !PS).

:- pred parse_list_elem(parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_list_elem(Term, !PS) :-
    get_ops_table(!.PS, OpTable),
    % XXX We should do the following:
    %   ArgPriority = ops.arg_priority(OpTable),
    % but that would mean we can't, for example, parse promise_pure/0 in
    % foreign attribute lists.
    ArgPriority = ops.max_priority(OpTable) + 1,
    parse_term_2(ArgPriority, list_elem, Term, !PS).

:- pred parse_term_2(int::in, term_kind::in, parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_term_2(MaxPriority, TermKind, Term, !PS) :-
    parse_left_term(MaxPriority, TermKind, LeftPriority, LeftTerm0, !PS),
    (
        LeftTerm0 = ok(LeftTerm),
        parse_rest(MaxPriority, TermKind, LeftPriority, LeftTerm, Term, !PS)
    ;
        LeftTerm0 = error(_, _),
        % propagate error upwards
        Term = LeftTerm0
    ).

:- pred parse_left_term(int::in, term_kind::in, int::out, parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_left_term(MaxPriority, TermKind, OpPriority, Term, !PS) :-
    ( parser_get_token_context(Token, Context, !PS) ->
        (
            % Check for unary minus of integer.
            Token = name("-"),
            parser_get_token_context(IntToken, _IntContext, !PS),
            (
                IntToken = integer(X),
                NegX = 0 - X
            ;
                IntToken = big_integer(BigString),
                max_int_plus_1(int.bits_per_int, BigString),
                NegX = int.min_int
            )
        ->
            get_term_context(!.PS, Context, TermContext),
            Term = ok(term.functor(term.integer(NegX), [], TermContext)),
            OpPriority = 0
        ;
            % Check for unary minus of float.
            Token = name("-"),
            parser_get_token_context(float(F), _FloatContext, !PS)
        ->
            get_term_context(!.PS, Context, TermContext),
            NegF = 0.0 - F,
            Term = ok(term.functor(term.float(NegF), [], TermContext)),
            OpPriority = 0
        ;
            % Check for binary prefix op.
            Token = name(Op),
            \+ parser_peek_token(open_ct, !.PS, _),
            get_ops_table(!.PS, OpTable),
            ops.lookup_binary_prefix_op(OpTable, Op, BinOpPriority,
                RightAssoc, RightRightAssoc),
            BinOpPriority =< MaxPriority,
            parser_peek_token(NextToken, !PS),
            could_start_term(NextToken, yes)
        ->
            adjust_priority_for_assoc(BinOpPriority,
                RightAssoc, RightPriority),
            adjust_priority_for_assoc(BinOpPriority,
                RightRightAssoc, RightRightPriority),
            OpPriority = BinOpPriority,
            parse_term_2(RightPriority, TermKind, RightResult, !PS),
            (
                RightResult = ok(RightTerm),
                parse_term_2(RightRightPriority, TermKind, RightRightResult,
                    !PS),
                (
                    RightRightResult = ok(RightRightTerm),
                    get_term_context(!.PS, Context, TermContext),
                    Term = ok(term.functor(term.atom(Op),
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
        ;
            % Check for unary prefix op.
            Token = name(Op),
            \+ parser_peek_token(open_ct, !.PS, _),
            get_ops_table(!.PS, OpTable),
            ops.lookup_prefix_op(OpTable, Op, UnOpPriority, RightAssoc),
            UnOpPriority =< MaxPriority,
            parser_peek_token(NextToken, !PS),
            could_start_term(NextToken, yes)
        ->
            adjust_priority_for_assoc(UnOpPriority, RightAssoc,
                RightPriority),
            parse_term_2(RightPriority, TermKind, RightResult, !PS),
            OpPriority = UnOpPriority,
            (
                RightResult = ok(RightTerm),
                get_term_context(!.PS, Context, TermContext),
                Term = ok(term.functor(term.atom(Op), [RightTerm],
                    TermContext))
            ;
                RightResult = error(_, _),
                % Propagate error upwards.
                Term = RightResult
            )
        ;
            parse_simple_term(Token, Context, MaxPriority, Term, !PS),
            OpPriority = 0
        )
    ;
        Term = make_error(!.PS, "unexpected end-of-file at start of sub-term"),
        OpPriority = 0
    ).

:- pred parse_rest(int::in, term_kind::in, int::in, term(T)::in,
    parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_rest(MaxPriority, TermKind, LeftPriority, LeftTerm, Term, !PS) :-
    (
        % Infix op.
        parser_get_token_context(Token, Context, !PS),
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
        (
            % A token surrounded by backquotes is a prefix token being used
            % in an infix manner.
            Op0 = "`",
            get_ops_table(!.PS, OpTable),
            ops.lookup_operator_term(OpTable, OpPriority0,
                LeftAssoc0, RightAssoc0)
        ->
            OpPriority = OpPriority0,
            LeftAssoc = LeftAssoc0,
            RightAssoc = RightAssoc0,
            parse_backquoted_operator(Qualifier, Op, VariableTerm, !PS),
            parser_get_token(name("`"), !PS)
        ;
            Op = Op0,
            VariableTerm = [],
            Qualifier = no,
            get_ops_table(!.PS, OpTable),
            ops.lookup_infix_op(OpTable, Op, OpPriority,
                LeftAssoc, RightAssoc)
        ),
        OpPriority =< MaxPriority,
        check_priority(LeftAssoc, OpPriority, LeftPriority)
    ->
        adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
        parse_term_2(RightPriority, TermKind, RightTerm0, !PS),
        (
            RightTerm0 = ok(RightTerm),
            get_term_context(!.PS, Context, TermContext),
            OpTerm0 = term.functor(term.atom(Op),
                list.append(VariableTerm, [LeftTerm, RightTerm]),
                TermContext),
            (
                Qualifier = no,
                OpTerm = OpTerm0
            ;
                Qualifier = yes(QTerm),
                OpTerm = term.functor(term.atom("."), [QTerm, OpTerm0],
                    TermContext)
            ),
            parse_rest(MaxPriority, TermKind, OpPriority, OpTerm, Term, !PS)
        ;
            RightTerm0 = error(_, _),
            % Propagate error upwards.
            Term = RightTerm0
        )
    ;
        % Postfix op.
        parser_get_token_context(name(Op), Context, !PS),
        get_ops_table(!.PS, OpTable),
        ops.lookup_postfix_op(OpTable, Op, OpPriority, LeftAssoc),
        OpPriority =< MaxPriority,
        check_priority(LeftAssoc, OpPriority, LeftPriority)
    ->
        get_term_context(!.PS, Context, TermContext),
        OpTerm = term.functor(term.atom(Op), [LeftTerm], TermContext),
        parse_rest(MaxPriority, TermKind, OpPriority, OpTerm, Term, !PS)
    ;
        Term = ok(LeftTerm)
    ).

:- pred parse_backquoted_operator(maybe(term(T))::out, string::out,
    list(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is semidet <= op_table(Ops).

parse_backquoted_operator(Qualifier, OpName, VariableTerm, !PS) :-
    parser_get_token_context(Token, Context, !PS),
    get_term_context(!.PS, Context, TermContext),
    (
        Token = variable(VariableOp),
        Qualifier = no,
        OpName = "",
        add_var(VariableOp, Var, !PS),
        VariableTerm = [variable(Var, TermContext)]
    ;
        Token = name(OpName0),
        VariableTerm = [],
        parse_backquoted_operator_2(no, Qualifier, TermContext, OpName0, OpName,
            !PS)
    ).

:- pred parse_backquoted_operator_2(maybe(term(T))::in, maybe(term(T))::out,
    term.context::in, string::in, string::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_backquoted_operator_2(Qualifier0, Qualifier, OpCtxt0, OpName0, OpName,
        !PS) :-
    (
        parser_get_token_context(name(ModuleSeparator), SepContext, !PS),
        (
            ModuleSeparator = "."
        ;
            ModuleSeparator = ":"
        ),
        parser_get_token_context(name(OpName1), NameContext, !PS),
        OpName1 \= "`"
    ->
        get_term_context(!.PS, SepContext, SepCtxt),
        get_term_context(!.PS, NameContext, OpCtxt1),
        QTerm1 = term.functor(atom(OpName0), [], OpCtxt0),
        (
            Qualifier0 = no,
            Qualifier1 = yes(QTerm1)
        ;
            Qualifier0 = yes(QTerm0),
            Qualifier1 = yes(functor(atom("."), [QTerm0, QTerm1], SepCtxt))
        ),
        parse_backquoted_operator_2(Qualifier1, Qualifier, OpCtxt1,
            OpName1, OpName, !PS)
    ;
        Qualifier = Qualifier0,
        OpName = OpName0
    ).

%---------------------------------------------------------------------------%

:- pred parse_simple_term(token::in, token_context::in, int::in,
    parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_simple_term(Token, Context, Priority, Term, !PS) :-
    ( parse_simple_term_2(Token, Context, Priority, Term0, !PS) ->
        check_for_higher_order_term(Term0, Context, Term, !PS)
    ;
        parser_unexpected_tok(Token, Context,
            "unexpected token at start of (sub)term", Term, !PS)
    ).

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

:- pred parse_simple_term_2(token::in, token_context::in, int::in,
    parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is semidet <= op_table(Ops).

parse_simple_term_2(name(Atom), Context, Prec, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    ( parser_get_token(open_ct, !PS) ->
        parse_args(Args0, !PS),
        (
            Args0 = ok(Args),
            Term = ok(term.functor(term.atom(Atom), Args, TermContext))
        ;
            % Propagate error upwards.
            Args0 = error(Message, Tokens),
            Term = error(Message, Tokens)
        )
    ;
        get_ops_table(!.PS, OpTable),
        ( ops.lookup_op(OpTable, Atom) ->
            Prec > ops.max_priority(OpTable)
        ;
            true
        ),
        Term = ok(term.functor(term.atom(Atom), [], TermContext))
    ).

parse_simple_term_2(variable(VarName), Context, _, Term, !PS) :-
    add_var(VarName, Var, !PS),
    get_term_context(!.PS, Context, TermContext),
    Term = ok(term.variable(Var, TermContext)).

parse_simple_term_2(integer(Int), Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    Term = ok(term.functor(term.integer(Int), [], TermContext)).

parse_simple_term_2(big_integer(_), _Context, _, _Term, !PS) :-
    % The term type does not yet support big integers.
    fail.

parse_simple_term_2(float(Float), Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    Term = ok(term.functor(term.float(Float), [], TermContext)).

parse_simple_term_2(string(String), Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    Term = ok(term.functor(term.string(String), [], TermContext)).

parse_simple_term_2(implementation_defined(Name), Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    Term = ok(term.functor(term.implementation_defined(Name), [],
        TermContext)).

parse_simple_term_2(open, _, _, Term, !PS) :-
    parse_term(Term0, !PS),
    (
        Term0 = ok(_),
        ( parser_get_token(close, !PS) ->
            Term = Term0
        ;
            parser_unexpected("expecting `)' or operator", Term, !PS)
        )
    ;
        % Propagate error upwards.
        Term0 = error(_, _),
        Term = Term0
    ).

parse_simple_term_2(open_ct, Context, Prec, Term, !PS) :-
    parse_simple_term_2(open, Context, Prec, Term, !PS).

parse_simple_term_2(open_list, Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    ( parser_get_token(close_list, !PS) ->
        parse_special_atom("[]", TermContext, Term, !PS)
    ;
        parse_list(Term, !PS)
    ).

parse_simple_term_2(open_curly, Context, _, Term, !PS) :-
    get_term_context(!.PS, Context, TermContext),
    ( parser_get_token(close_curly, !PS) ->
        parse_special_atom("{}", TermContext, Term, !PS)
    ;
        % This is a slight departure from ISO Prolog syntax -- instead of
        % parsing "{1,2,3}" as "'{}'(','(1, ','(2, 3)))" we parse it as
        % "'{}'(1,2,3)". This makes the structure of tuple functors the same
        % as other functors.
        parse_term(SubTerm0, !PS),
        (
            SubTerm0 = ok(SubTerm),
            conjunction_to_list(SubTerm, ArgTerms),
            ( parser_get_token(close_curly, !PS) ->
                Term = ok(term.functor(term.atom("{}"), ArgTerms,
                    TermContext))
            ;
                parser_unexpected("expecting `}' or operator", Term, !PS)
            )
        ;
            SubTerm0 = error(_, _),
            % Propagate error upwards.
            Term = SubTerm0
        )
    ).

:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

conjunction_to_list(Term, ArgTerms) :-
    ( Term = term.functor(term.atom(","), [LeftTerm, RightTerm], _) ->
        conjunction_to_list(RightTerm, ArgTerms0),
        ArgTerms = [LeftTerm | ArgTerms0]
    ;
        ArgTerms = [Term]
    ).

:- pred check_for_higher_order_term(parse(term(T))::in,
    token_context::in, parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

check_for_higher_order_term(Term0, Context, Term, !PS) :-
    % As an extension to ISO Prolog syntax, we check for the syntax
    % "Term(Args)", and parse it as the term ''(Term, Args). The aim of this
    % extension is to provide a nicer syntax for higher-order stuff.
    (
        Term0 = ok(Term1),
        parser_get_token(open_ct, !PS)
    ->
        get_term_context(!.PS, Context, TermContext),
        parse_args(Args0, !PS),
        (
            Args0 = ok(Args),
            Term2 = ok(term.functor(term.atom(""), [Term1 | Args],
                TermContext)),
            check_for_higher_order_term(Term2, Context, Term, !PS)
        ;
            % Propagate error upwards.
            Args0 = error(Message, Tokens),
            Term = error(Message, Tokens)
        )
    ;
        Term = Term0
    ).

:- pred parse_special_atom(string::in, term.context::in,
    parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_special_atom(Atom, TermContext, Term, !PS) :-
    ( parser_get_token(open_ct, !PS) ->
        parse_args(Args0, !PS),
        (
            Args0 = ok(Args),
            Term = ok(term.functor(term.atom(Atom), Args, TermContext))
        ;
            % Propagate error upwards.
            Args0 = error(Message, Tokens),
            Term = error(Message, Tokens)
        )
    ;
        Term = ok(term.functor(term.atom(Atom), [], TermContext))
    ).

:- pred parse_list(parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_list(List, !PS) :-
    parse_list_elem(Arg0, !PS),
    (
        Arg0 = ok(Arg),
        parse_list_2(Arg, List, !PS)
    ;
        Arg0 = error(_, _),
        % Propagate error.
        List = Arg0
    ).

:- pred parse_list_2(term(T)::in, parse(term(T))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_list_2(Arg, List, !PS) :-
    ( parser_get_token_context(Token, Context, !PS) ->
        get_term_context(!.PS, Context, TermContext),
        ( Token = comma ->
            parse_list(Tail0, !PS),
            (
                Tail0 = ok(Tail),
                List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                    TermContext))
            ;
                Tail0 = error(_, _),
                % Propagate error.
                List = Tail0
            )
        ; Token = ht_sep ->
            parse_arg(Tail0, !PS),
            (
                Tail0 = ok(Tail),
                ( parser_get_token(close_list, !PS) ->
                    List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                        TermContext))
                ;
                    parser_unexpected("expecting ']' or operator", List, !PS)
                )
            ;
                Tail0 = error(_, _),
                % Propagate error.
                List = Tail0
            )
        ; Token = close_list ->
            Tail = term.functor(term.atom("[]"), [], TermContext),
            List = ok(term.functor(term.atom("[|]"), [Arg, Tail],
                TermContext))
        ;
            parser_unexpected_tok(Token, Context,
                "expected comma, `|', `]', or operator", List, !PS)
        )
    ;
        % XXX The error message should state the line that the list started on.
        List = make_error(!.PS, "unexpected end-of-file in list")
    ).

:- pred parse_args(parse(list(term(T)))::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parse_args(List, !PS) :-
    parse_arg(Arg0, !PS),
    (
        Arg0 = ok(Arg),
        ( parser_get_token_context(Token, Context, !PS) ->
            ( Token = comma ->
                parse_args(Tail0, !PS),
                (
                    Tail0 = ok(Tail),
                    List = ok([Arg|Tail])
                ;
                    Tail0 = error(_, _),
                    % Propagate error upwards.
                    List = Tail0
                )
            ; Token = close ->
                List = ok([Arg])
            ;
                parser_unexpected_tok(Token, Context,
                    "expected `,', `)', or operator", List, !PS)
            )
        ;
            List = make_error(!.PS, "unexpected end-of-file in argument list")
        )
    ;
        Arg0 = error(Message, Tokens),
        % Propagate error upwards.
        List = error(Message, Tokens)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%
% Routines that manipulate the parser state.

:- type state(Ops, T)   % <= op_table(Ops)
    --->    state(
                stream_name :: string,
                            % the name of the stream being parsed
                ops_table   :: Ops,
                            % the current set of operators
                varset      :: varset(T),
                            % the names of the variables in the
                            % term being parsed
                tokens_left :: token_list,
                            % the remaining tokens
                var_names   :: map(string, var(T))
                            % a map from variable name to variable
                            % so we know when to make a fresh var
            ).

:- func parser_state_get_stream_name(state(Ops, T)) = string.
:- func parser_state_get_ops_table(state(Ops, T)) = Ops.
:- func parser_state_get_varset(state(Ops, T)) = varset(T).
:- func parser_state_get_tokens_left(state(Ops, T)) = token_list.
:- func parser_state_get_var_names(state(Ops, T)) = map(string, var(T)).

:- func parser_state_set_varset(state(Ops, T), varset(T))
    = state(Ops, T).
:- func parser_state_set_tokens_left(state(Ops, T), token_list)
    = state(Ops, T).
:- func parser_state_set_var_names(state(Ops, T), map(string, var(T)))
    = state(Ops, T).

% If you want profiling to tell you the frequencies of these operations,
% change the inline pragmas to no_inline pragmas.

:- pragma inline(parser_state_get_stream_name/1).
:- pragma inline(parser_state_get_ops_table/1).
:- pragma inline(parser_state_get_varset/1).
:- pragma inline(parser_state_get_tokens_left/1).
:- pragma inline(parser_state_get_var_names/1).

:- pragma inline(parser_state_set_varset/2).
:- pragma inline(parser_state_set_tokens_left/2).
:- pragma inline(parser_state_set_var_names/2).

parser_state_get_stream_name(ParserState) = ParserState ^ stream_name.
parser_state_get_ops_table(ParserState) = ParserState ^ ops_table.
parser_state_get_varset(ParserState) = ParserState ^ varset.
parser_state_get_tokens_left(ParserState) = ParserState ^ tokens_left.
parser_state_get_var_names(ParserState) = ParserState ^ var_names.

parser_state_set_varset(ParserState0, VarSet) =
    ParserState0 ^ varset := VarSet.
parser_state_set_tokens_left(ParserState0, Tokens) =
    ParserState0 ^ tokens_left := Tokens.
parser_state_set_var_names(ParserState0, Names) =
    ParserState0 ^ var_names := Names.

%---------------------------------------------------------------------------%

    % We encountered an error. See if the next token was an infix or postfix
    % operator. If so, it would normally form part of the term, so the error
    % must have been an operator precedence error. Otherwise, it was some
    % other sort of error, so issue the usual error message.
    %
:- pred parser_unexpected(string::in, parse(U)::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parser_unexpected(UsualMessage, Error, !PS) :-
    ( parser_get_token_context(Token, Context, !PS) ->
        parser_unexpected_tok(Token, Context, UsualMessage, Error, !PS)
    ;
        Error = make_error(!.PS, UsualMessage)
    ).

:- pred parser_unexpected_tok(token::in, token_context::in, string::in,
    parse(U)::out,
    state(Ops, T)::in, state(Ops, T)::out) is det <= op_table(Ops).

parser_unexpected_tok(Token, Context, UsualMessage, Error, !PS) :-
    % Push the token back, so that the error message points at it
    % rather than at the following token.
    parser_unget_token(Token, Context, !PS),
    (
        ( Token = name(Op)
        ; Token = comma, Op = ","
        ),
        get_ops_table(!.PS, OpTable),
        ( ops.lookup_infix_op(OpTable, Op, _, _, _)
        ; ops.lookup_postfix_op(OpTable, Op, _, _)
        )
    ->
        Error = make_error(!.PS, "operator precedence error")
    ;
        Error = make_error(!.PS, UsualMessage)
    ).

%---------------------------------------------------------------------------%

:- func make_error(state(Ops, T), string) = parse(U).

make_error(ParserState, Message) = error(Message, Tokens) :-
    Tokens = parser_state_get_tokens_left(ParserState).

%---------------------------------------------------------------------------%

:- pred could_start_term(token::in, bool::out) is det.

could_start_term(name(_), yes).
could_start_term(variable(_), yes).
could_start_term(integer(_), yes).
could_start_term(big_integer(_), yes).
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

:- pred max_int_plus_1(int::in, string::in) is semidet.

max_int_plus_1(32, "2147483648").
max_int_plus_1(64, "9223372036854775808").

%---------------------------------------------------------------------------%

:- pred init_parser_state(Ops::in, string::in, token_list::in,
    state(Ops, T)::out) is det <= op_table(Ops).

init_parser_state(Ops, FileName, Tokens, ParserState) :-
    varset.init(VarSet),
    map.init(Names),
    ParserState = state(FileName, Ops, VarSet, Tokens, Names).

:- pred final_parser_state(state(Ops, T)::in, varset(T)::out,
    token_list::out) is det.

final_parser_state(ParserState, VarSet, TokenList) :-
    VarSet = parser_state_get_varset(ParserState),
    TokenList = parser_state_get_tokens_left(ParserState).

%---------------------------------------------------------------------------%

    % The following implied modes allow us to check that an expected token
    % matches the actual token before creating a new parser state.  This is
    % particularly for the call in `check_for_higher_order_term' which usually
    % fails.
    %
:- pred parser_get_token(token, state(Ops, T), state(Ops, T)).
:- mode parser_get_token(in, in, out) is semidet.
:- mode parser_get_token(out, in, out) is semidet.

parser_get_token(Token, !PS) :-
    parser_get_token_context(Token, _Context, !PS).

:- pred parser_get_token_context(token, token_context,
    state(Ops, T), state(Ops, T)).
:- mode parser_get_token_context(in, out, in, out) is semidet.
:- mode parser_get_token_context(out, out, in, out) is semidet.

parser_get_token_context(Token, Context, ParserState0, ParserState) :-
    Tokens0 = parser_state_get_tokens_left(ParserState0),
    Tokens0 = token_cons(Token, Context, Tokens),
    ParserState = parser_state_set_tokens_left(ParserState0, Tokens).

:- pred parser_unget_token(token::in, token_context::in,
    state(Ops, T)::in, state(Ops, T)::out) is det.

parser_unget_token(Token, Context, ParserState0, ParserState) :-
    Tokens0 = parser_state_get_tokens_left(ParserState0),
    Tokens = token_cons(Token, Context, Tokens0),
    ParserState = parser_state_set_tokens_left(ParserState0, Tokens).

:- pred parser_peek_token(token::out,
    state(Ops, T)::in, state(Ops, T)::out) is semidet.

parser_peek_token(Token, !PS) :-
    parser_peek_token_context(Token, _Context, !PS).

:- pred parser_peek_token_context(token::out, token_context::out,
    state(Ops, T)::in, state(Ops, T)::out) is semidet.

parser_peek_token_context(Token, Context, ParserState, ParserState) :-
    Tokens = parser_state_get_tokens_left(ParserState),
    Tokens = token_cons(Token, Context, _).

%---------------------------------------------------------------------------%

:- pred add_var(string::in, var(T)::out,
    state(Ops, T)::in, state(Ops, T)::out) is det.

add_var(VarName, Var, ParserState0, ParserState) :-
    ( VarName = "_" ->
        VarSet0 = parser_state_get_varset(ParserState0),
        varset.new_var(Var, VarSet0, VarSet),
        ParserState = parser_state_set_varset(ParserState0, VarSet)
    ;
        Names0 = parser_state_get_var_names(ParserState0),
        ( map.search(Names0, VarName, Var0) ->
            Var = Var0,
            ParserState = ParserState0
        ;
            VarSet0 = parser_state_get_varset(ParserState0),
            varset.new_named_var(VarName, Var, VarSet0, VarSet),
            map.det_insert(VarName, Var, Names0, Names),
            ParserState1 = parser_state_set_varset(ParserState0, VarSet),
            ParserState = parser_state_set_var_names(ParserState1, Names)
        )
    ).

:- pred get_ops_table(state(Ops, T)::in, Ops::out) is det
    <= op_table(Ops).

get_ops_table(ParserState, OpTable) :-
    OpTable = parser_state_get_ops_table(ParserState).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_priority(ops.assoc::in, int::in, int::in) is semidet.

check_priority(y, MaxPriority, Priority) :-
    Priority =< MaxPriority.
check_priority(x, MaxPriority, Priority) :-
    Priority < MaxPriority.

:- pred get_term_context(state(Ops, T)::in, token_context::in,
    term.context::out) is det.

get_term_context(ParserState, TokenContext, TermContext) :-
    FileName = parser_state_get_stream_name(ParserState),
    term.context_init(FileName, TokenContext, TermContext).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
