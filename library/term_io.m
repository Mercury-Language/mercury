%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: term_io.m.
% Main author: fjh.
% Stability: medium to high.
% 
% This file encapsulates all the term I/O.
% This exports predicates to read and write terms in the
% nice ground representation provided in term.m.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module term_io.
:- interface.

:- import_module char.
:- import_module io.
:- import_module ops.
:- import_module stream.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type read_term(T)
    --->    eof
    ;       error(string, int)
    ;       term(varset(T), term(T)).

:- type read_term   == read_term(generic).

    % term_io.read_term(Result, !IO):
    %
    % Read a term from standard input. Similar to NU-Prolog read_term/2,
    % except that resulting term is in the ground representation.
    % Binds Result to either `eof', `term(VarSet, Term)', or
    % `error(Message, LineNumber)'.
    %
:- pred term_io.read_term(read_term(T)::out, io::di, io::uo) is det.

    % As above, except uses the given operator table instead of
    % the standard Mercury operators.
    %
:- pred term_io.read_term_with_op_table(Ops::in, read_term(T)::out,
    io::di, io::uo) is det <= op_table(Ops).

    % Writes a term to standard output. Uses the variable names specified
    % by the varset. Writes _N for all unnamed variables, with N starting at 0.
    %
:- pred term_io.write_term(varset(T)::in, term(T)::in, io::di, io::uo) is det.

    % As above, except uses the given operator table instead of the
    % standard Mercury operators.
    %
:- pred term_io.write_term_with_op_table(Ops::in, varset(T)::in, term(T)::in,
    io::di, io::uo) is det <= op_table(Ops).

    % As above, except it appends a period and new-line.
    %
:- pred term_io.write_term_nl(varset(T)::in, term(T)::in, io::di, io::uo)
    is det.

    % As above, except it appends a period and new-line.
    %
:- pred term_io.write_term_nl_with_op_table(Ops::in, varset(T)::in,
    term(T)::in, io::di, io::uo) is det <= op_table(Ops).

    % Writes a constant (integer, float, string, or atom) to stdout.
    %
:- pred term_io.write_constant(const::in, io::di, io::uo) is det.

    % Like term_io.write_constant, but return the result in a string.
    %
:- func term_io.format_constant(const) = string.

    % Writes a variable to stdout.
    %
:- pred term_io.write_variable(var(T)::in, varset(T)::in, io::di, io::uo)
    is det.

    % As above, except uses the given operator table instead of the
    % standard Mercury operators.
    %
:- pred term_io.write_variable_with_op_table(Ops::in, var(T)::in,
    varset(T)::in, io::di, io::uo) is det <= op_table(Ops).

    % Given a string S, write S in double-quotes, with characters
    % escaped if necessary, to stdout.
    %
:- pred term_io.quote_string(string::in, io::di, io::uo) is det.

:- pred term_io.quote_string(Stream::in, string::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like term_io.quote_string, but return the result in a string.
    %
:- func term_io.quoted_string(string) = string.

    % Given an atom-name A, write A, enclosed in single-quotes if necessary,
    % with characters escaped if necessary, to stdout.
    %
:- pred term_io.quote_atom(string::in, io::di, io::uo) is det.

:- pred term_io.quote_atom(Stream::in, string::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like term_io.quote_atom, but return the result in a string.
    %
:- func term_io.quoted_atom(string) = string.

    % Given a character C, write C in single-quotes,
    % escaped if necessary, to stdout.
    %
:- pred term_io.quote_char(char::in, io::di, io::uo) is det.

:- pred term_io.quote_char(Stream::in, char::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like term_io.quote_char, but return the result in a string.
    %
:- func term_io.quoted_char(char) = string.

    % Given a character C, write C, escaped if necessary, to stdout.
    % The character is not enclosed in quotes.
    %
:- pred term_io.write_escaped_char(char::in, io::di, io::uo) is det.

:- pred term_io.write_escaped_char(Stream::in, char::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like term_io.write_escaped_char, but return the result in a string.
    %
:- func term_io.escaped_char(char) = string.

    % A reversible version of escaped_char.
    %
:- pred string_is_escaped_char(char, string).
:- mode string_is_escaped_char(in, out) is det.
:- mode string_is_escaped_char(out, in) is semidet.

    % Given a string S, write S, with characters escaped if necessary,
    % to stdout. The string is not enclosed in quotes.
    %
:- pred term_io.write_escaped_string(string::in, io::di, io::uo) is det.

:- pred term_io.write_escaped_string(Stream::in, string::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like term_io.write_escaped_char, but return the result in a string.
    %
:- func term_io.escaped_string(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%

:- interface.

    % Convert a character to the corresponding octal escape code.
    %
    % We use ISO-Prolog style octal escapes, which are of the form '\nnn\';
    % note that unlike C octal escapes, they are terminated with a backslash.
    %
    % XXX Using this predicate in the compiler may cause problems interfacing
    % with versions of the compiler that have been built in grades which use
    % different character representations.
    %
:- func mercury_escape_char(char) = string.

    % Succeed if the given character is a Mercury punctuation character.
    %
:- pred is_mercury_punctuation_char(char::in) is semidet.

    % encode_escaped_char(Char, Str):
    %
    % Succeed in one of two cases:
    %
    % - Char is 'x', and Str is "x", where x is a valid Mercury source
    %   character, or
    % - Char is '\x' and Str is "\x", where '\x' is a valid character
    %   escape sequence.
    %
:- pred encode_escaped_char(char, string).
:- mode encode_escaped_char(in, out) is semidet.
:- mode encode_escaped_char(out, in) is semidet.

    % for use by io.m.

:- type adjacent_to_graphic_token
    --->    maybe_adjacent_to_graphic_token
    ;       not_adjacent_to_graphic_token.

:- pred term_io.quote_atom_agt(string::in, adjacent_to_graphic_token::in,
    io::di, io::uo) is det.

:- pred term_io.quote_atom_agt(Stream::in, string::in,
    adjacent_to_graphic_token::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- func term_io.quoted_atom_agt(string, adjacent_to_graphic_token) = string.

:- pragma type_spec(term_io.quote_string/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(term_io.quote_atom/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(term_io.write_escaped_string/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(term_io.write_escaped_char/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(term_io.quote_char/4,
            (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(term_io.quote_atom_agt/5,
            (Stream = io.output_stream, State = io.state)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module lexer.
:- import_module list.
:- import_module parser.
:- import_module string.
:- import_module stream.string_writer.

%-----------------------------------------------------------------------------%

term_io.read_term(Result, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.read_term_with_op_table(Ops, Result, !IO).

term_io.read_term_with_op_table(Ops, Result, !IO) :-
    parser.read_term_with_op_table(Ops, Result, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % write a variable to standard output.
    %
    % There are two ways we could choose to write unnamed variables
    % (ie `_'):
    %   Convert the variable to an integer representation and write
    %   `_N' where N is that integer representation. This has the
    %   advantage that such variables get printed in a canonical
    %   way, so rearranging terms containing such variables will
    %   not effect the way they are numbered (this includes breaking
    %   up a term and printing the pieces separately).
    % or
    %   Number the unnamed variables from 0 and write `_N' where
    %   N is the number in the sequence of such variables. This has
    %   the advantage that such variables can be visually scanned
    %   rather more easily (for example in error messages).
    %
    % An ideal solution would be to provide both, and a flag to choose
    % between the two. At the moment we provide only the first, though
    % the infrastructure for the second is present in the code.

term_io.write_variable(Variable, VarSet, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_variable_with_op_table(Ops, Variable, VarSet, !IO).

term_io.write_variable_with_op_table(Ops, Variable, VarSet, !IO) :-
    term_io.write_variable_2(Ops, Variable, VarSet, _, 0, _, !IO).

:- pred term_io.write_variable_2(Ops::in, var(T)::in,
    varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

term_io.write_variable_2(Ops, Id, !VarSet, !N, !IO) :-
    ( varset.search_var(!.VarSet, Id, Val) ->
        term_io.write_term_2(Ops, Val, !VarSet, !N, !IO)
    ; varset.search_name(!.VarSet, Id, Name) ->
        io.write_string(Name, !IO)
    ;
        % XXX Problems with name clashes.

        term.var_to_int(Id, VarNum),
        string.int_to_string(VarNum, Num),
        string.append("_", Num, VarName),
        varset.name_var(Id, VarName, !VarSet),
        !:N = !.N + 1,
        io.write_string(VarName, !IO)
    ).

%-----------------------------------------------------------------------------%

term_io.write_term(VarSet, Term, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_term_with_op_table(Ops, VarSet, Term, !IO).

term_io.write_term_with_op_table(Ops, VarSet, Term, !IO) :-
    term_io.write_term_2(Ops, Term, VarSet, _, 0, _, !IO).

:- pred term_io.write_term_2(Ops::in, term(T)::in,
    varset(T)::in, varset(T)::out, int::in, int::out, io::di, io::uo) is det
    <= op_table(Ops).

term_io.write_term_2(Ops, Term, !VarSet, !N, !IO) :-
    term_io.write_term_3(Ops, Term, ops.max_priority(Ops) + 1,
        !VarSet, !N, !IO).

:- pred term_io.write_arg_term(Ops::in, term(T)::in,
    varset(T)::in, varset(T)::out, int::in, int::out, io::di, io::uo) is det
    <= op_table(Ops).

term_io.write_arg_term(Ops, Term, !VarSet, !N, !IO) :-
    term_io.write_term_3(Ops, Term, ops.arg_priority(Ops),
        !VarSet, !N, !IO).

:- pred term_io.write_term_3(Ops::in, term(T)::in, ops.priority::in,
    varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

term_io.write_term_3(Ops, term.variable(Id, _), _, !VarSet, !N, !IO) :-
    term_io.write_variable_2(Ops, Id, !VarSet, !N, !IO).
term_io.write_term_3(Ops, term.functor(Functor, Args, _), Priority,
        !VarSet, !N, !IO) :-
    (
        Functor = term.atom("[|]"),
        Args = [ListHead, ListTail]
    ->
        io.write_char('[', !IO),
        term_io.write_arg_term(Ops, ListHead, !VarSet, !N, !IO),
        term_io.write_list_tail(Ops, ListTail, !VarSet, !N, !IO),
        io.write_char(']', !IO)
    ;
        Functor = term.atom("[]"),
        Args = []
    ->
        io.write_string("[]", !IO)
    ;
        Functor = term.atom("{}"),
        Args = [BracedTerm]
    ->
        io.write_string("{ ", !IO),
        term_io.write_term_2(Ops, BracedTerm, !VarSet, !N, !IO),
        io.write_string(" }", !IO)
    ;
        Functor = term.atom("{}"),
        Args = [BracedHead | BracedTail]
    ->
        io.write_char('{', !IO),
        term_io.write_arg_term(Ops, BracedHead, !VarSet, !N, !IO),
        term_io.write_term_args(Ops, BracedTail, !VarSet, !N, !IO),
        io.write_char('}', !IO)
    ;
        % The empty functor '' is used for higher-order syntax: Var(Arg, ...)
        % gets parsed as ''(Var, Arg). When writing it out, we want to use
        % the nice syntax.
        Functor = term.atom(""),
        Args = [term.variable(Var, _), FirstArg | OtherArgs]
    ->
        term_io.write_variable_2(Ops, Var, !VarSet, !N, !IO),
        io.write_char('(', !IO),
        term_io.write_arg_term(Ops, FirstArg, !VarSet, !N, !IO),
        term_io.write_term_args(Ops, OtherArgs, !VarSet, !N, !IO),
        io.write_char(')', !IO)
    ;
        Args = [PrefixArg],
        Functor = term.atom(OpName),
        ops.lookup_prefix_op(Ops, OpName, OpPriority, OpAssoc)
    ->
        io.output_stream(Stream, !IO),
        maybe_write_paren(Stream, '(', Priority, OpPriority, !IO),
        term_io.write_constant(Functor, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
        term_io.write_term_3(Ops, PrefixArg, NewPriority, !VarSet, !N, !IO),
        maybe_write_paren(Stream, ')', Priority, OpPriority, !IO)
    ;
        Args = [PostfixArg],
        Functor = term.atom(OpName),
        ops.lookup_postfix_op(Ops, OpName, OpPriority, OpAssoc)
    ->
        io.output_stream(Stream, !IO),
        maybe_write_paren(Stream, '(', Priority, OpPriority, !IO),
        adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
        term_io.write_term_3(Ops, PostfixArg, NewPriority, !VarSet, !N, !IO),
        io.write_char(' ', !IO),
        term_io.write_constant(Functor, !IO),
        maybe_write_paren(Stream, ')', Priority, OpPriority, !IO)
    ;
        Args = [Arg1, Arg2],
        Functor = term.atom(OpName),
        ops.lookup_infix_op(Ops, OpName, OpPriority, LeftAssoc, RightAssoc)
    ->
        io.output_stream(Stream, !IO),
        maybe_write_paren(Stream, '(', Priority, OpPriority, !IO),
        adjust_priority_for_assoc(OpPriority, LeftAssoc, LeftPriority),
        term_io.write_term_3(Ops, Arg1, LeftPriority, !VarSet, !N, !IO),
        ( OpName = "," ->
            io.write_string(", ", !IO)
        ; OpName = "." ->
            % If the operator is '.'/2 then we must not put spaces around it
            % (or at the very least, we should not put spaces afterwards, which
            % would make it appear as the end-of-term token). However, we do
            % have to quote it if the right hand side can begin with a digit.
            ( starts_with_digit(Arg2) ->
                Dot = "'.'"
            ;
                Dot = "."
            ),
            io.write_string(Dot, !IO)
        ;
            io.write_char(' ', !IO),
            term_io.write_constant(Functor, !IO),
            io.write_char(' ', !IO)
        ),
        adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
        term_io.write_term_3(Ops, Arg2, RightPriority, !VarSet, !N, !IO),
        maybe_write_paren(Stream, ')', Priority, OpPriority, !IO)
    ;
        Args = [Arg1, Arg2],
        Functor = term.atom(OpName),
        ops.lookup_binary_prefix_op(Ops, OpName, OpPriority,
            FirstAssoc, SecondAssoc)
    ->
        io.output_stream(Stream, !IO),
        maybe_write_paren(Stream, '(', Priority, OpPriority, !IO),
        term_io.write_constant(Functor, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, FirstAssoc, FirstPriority),
        term_io.write_term_3(Ops, Arg1, FirstPriority, !VarSet, !N, !IO),
        io.write_char(' ', !IO),
        adjust_priority_for_assoc(OpPriority, SecondAssoc, SecondPriority),
        term_io.write_term_3(Ops, Arg2, SecondPriority, !VarSet, !N, !IO),
        maybe_write_paren(Stream, ')', Priority, OpPriority, !IO)
    ;
        (
            Args = [],
            Functor = term.atom(Op),
            ops.lookup_op(Ops, Op),
            Priority =< ops.max_priority(Ops)
        ->
            io.write_char('(', !IO),
            term_io.write_constant(Functor, !IO),
            io.write_char(')', !IO)
        ;
            term_io.write_constant(Functor,
                maybe_adjacent_to_graphic_token, !IO)
        ),
        (
            Args = [X | Xs],
            io.write_char('(', !IO),
            term_io.write_arg_term(Ops, X, !VarSet, !N, !IO),
            term_io.write_term_args(Ops, Xs, !VarSet, !N, !IO),
            io.write_char(')', !IO)
        ;
            Args = []
        )
    ).

:- pred term_io.write_list_tail(Ops::in, term(T)::in,
    varset(T)::in, varset(T)::out, int::in, int::out, io::di, io::uo) is det
    <= op_table(Ops).

term_io.write_list_tail(Ops, Term, !VarSet, !N, !IO) :-
    (
        Term = term.variable(Id, _),
        varset.search_var(!.VarSet, Id, Val)
    ->
        term_io.write_list_tail(Ops, Val, !VarSet, !N, !IO)
    ;
        Term = term.functor(term.atom("[|]"), [ListHead, ListTail], _)
    ->
        io.write_string(", ", !IO),
        term_io.write_arg_term(Ops, ListHead, !VarSet, !N, !IO),
        term_io.write_list_tail(Ops, ListTail, !VarSet, !N, !IO)
    ;
        Term = term.functor(term.atom("[]"), [], _)
    ->
        true
    ;
        io.write_string(" | ", !IO),
        term_io.write_term_2(Ops, Term, !VarSet, !N, !IO)
    ).

    % Succeeds iff outputting the given term would start with a digit.
    % (This is a safe, conservative approximation and is used to decide
    % whether or not to quote infix '.'/2.)
    %
:- pred starts_with_digit(term(T)::in) is semidet.

starts_with_digit(functor(integer(_), _, _)).
starts_with_digit(functor(float(_), _, _)).
starts_with_digit(functor(atom(Op), Args, _)) :-
    (
        Args = [Arg, _],
        ops.lookup_infix_op(ops.init_mercury_op_table, Op, _, _, _)
    ;
        Args = [Arg],
        ops.lookup_postfix_op(ops.init_mercury_op_table, Op, _, _)
    ),
    starts_with_digit(Arg).

%-----------------------------------------------------------------------------%

:- pred term_io.write_term_args(Ops::in, list(term(T))::in,
    varset(T)::in, varset(T)::out, int::in, int::out, io::di, io::uo) is det
    <= op_table(Ops).

    % write the remaining arguments
term_io.write_term_args(_, [], !VarSet, !N, !IO).
term_io.write_term_args(Ops, [X | Xs], !VarSet, !N, !IO) :-
    io.write_string(", ", !IO),
    term_io.write_arg_term(Ops, X, !VarSet, !N, !IO),
    term_io.write_term_args(Ops, Xs, !VarSet, !N, !IO).

%-----------------------------------------------------------------------------%

term_io.write_constant(Const, !IO) :-
    term_io.write_constant(Const, not_adjacent_to_graphic_token, !IO).

:- pred term_io.write_constant(const::in, adjacent_to_graphic_token::in,
    io::di, io::uo) is det.

term_io.write_constant(term.integer(I), _, !IO) :-
    io.write_int(I, !IO).
term_io.write_constant(term.float(F), _, !IO) :-
    io.write_float(F, !IO).
term_io.write_constant(term.atom(A), NextToGraphicToken, !IO) :-
    term_io.quote_atom_agt(A, NextToGraphicToken, !IO).
term_io.write_constant(term.string(S), _, !IO) :-
    term_io.quote_string(S, !IO).
term_io.write_constant(term.implementation_defined(N), _, !IO) :-
    io.write_char('$', !IO),
    io.write_string(N, !IO).

term_io.format_constant(Const) =
    term_io.format_constant_agt(Const, not_adjacent_to_graphic_token).

:- func term_io.format_constant_agt(const, adjacent_to_graphic_token) = string.

term_io.format_constant_agt(term.integer(I), _) =
    string.int_to_string(I).
term_io.format_constant_agt(term.float(F), _) =
    string.float_to_string(F).
term_io.format_constant_agt(term.atom(A), NextToGraphicToken) =
    term_io.quoted_atom_agt(A, NextToGraphicToken).
term_io.format_constant_agt(term.string(S), _) =
    term_io.quoted_string(S).
term_io.format_constant_agt(term.implementation_defined(N), _) =
    "$" ++ N.

%-----------------------------------------------------------------------------%

term_io.quote_char(C, !IO) :-
    io.write_string(term_io.quoted_char(C), !IO).

term_io.quote_char(Stream, C, !State) :-
    stream.put(Stream, term_io.quoted_char(C), !State).

term_io.quoted_char(C) =
    string.format("'%s'", [s(term_io.escaped_char(C))]).

term_io.quote_atom(S, !IO) :-
    term_io.quote_atom_agt(S, not_adjacent_to_graphic_token, !IO).

term_io.quote_atom(Stream, S, !State) :-
    term_io.quote_atom_agt(Stream, S, not_adjacent_to_graphic_token, !State).

term_io.quoted_atom(S) =
    term_io.quoted_atom_agt(S, not_adjacent_to_graphic_token).

term_io.quote_atom_agt(S, NextToGraphicToken, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.quote_atom_agt(Stream, S, NextToGraphicToken, !IO).

term_io.quote_atom_agt(Stream, S, NextToGraphicToken, !State) :-
    ShouldQuote = should_atom_be_quoted(S, NextToGraphicToken),
    (
        ShouldQuote = no,
        stream.put(Stream, S, !State)
    ;
        ShouldQuote = yes,
        stream.put(Stream, '''', !State),
        term_io.write_escaped_string(Stream, S, !State),
        stream.put(Stream, '''', !State)
    ).

term_io.quoted_atom_agt(S, NextToGraphicToken) = String :-
    ShouldQuote = should_atom_be_quoted(S, NextToGraphicToken),
    (
        ShouldQuote = no,
        String = S
    ;
        ShouldQuote = yes,
        ES = term_io.escaped_string(S),
        String = string.append_list(["'", ES, "'"])
    ).

:- func should_atom_be_quoted(string, adjacent_to_graphic_token) = bool.

should_atom_be_quoted(S, NextToGraphicToken) = ShouldQuote :-
    (
        % I didn't make these rules up: see ISO Prolog 6.3.1.3 and 6.4.2. -fjh
        (
            % Letter digit token (6.4.2)
            string.index(S, 0, FirstChar),
            char.is_lower(FirstChar),
            string.is_all_alnum_or_underscore(S)
        ;
            % Semicolon token (6.4.2)
            S = ";"
        ;
            % Cut token (6.4.2)
            S = "!"
        ;
            % Graphic token (6.4.2)
            string.all_match(lexer.graphic_token_char, S),
            S \= "",

            % We need to quote tokens starting with '#', because Mercury uses
            % '#' to start source line number indicators.
            not string.index(S, 0, '#'),

            % If the token could be the last token in a term, and the term
            % could be followed with ".\n", then we need to quote the token,
            % otherwise the "." would be considered part of the same graphic
            % token. We can only leave it unquoted if we're sure it won't be
            % adjacent to any graphic token.
            NextToGraphicToken = not_adjacent_to_graphic_token
        ;
            % 6.3.1.3: atom = open list, close list ;
            S = "[]"
        ;
            % 6.3.1.3: atom = open curly, close curly ;
            S = "{}"
        )
    ->
        ShouldQuote = no
    ;
        % Anything else must be output as a quoted token (6.4.2).
        ShouldQuote = yes
    ).

% Note: the code here is similar to code in compiler/mercury_to_mercury.m;
% any changes here may require similar changes there.

term_io.quote_string(S, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.quote_string(Stream, S, !IO).

term_io.quote_string(Stream, S, !State) :-
    stream.put(Stream, '"', !State),
    term_io.write_escaped_string(Stream, S, !State),
    stream.put(Stream, '"', !State).

term_io.quoted_string(S) =
    string.append_list(["""", term_io.escaped_string(S), """"]).

term_io.write_escaped_string(String, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.write_escaped_string(Stream, String, !IO).

term_io.write_escaped_string(Stream, String, !State) :-
    string.foldl(term_io.write_escaped_char(Stream), String, !State).

term_io.escaped_string(String) =
    string.append_list(
        reverse(string.foldl(term_io.add_escaped_char, String, []))).

:- func term_io.add_escaped_char(char, list(string)) = list(string).

term_io.add_escaped_char(Char, Strings0) = Strings :-
    ( mercury_escape_special_char(Char, QuoteChar) ->
        Strings = [from_char_list(['\\', QuoteChar]) | Strings0]
    ; is_mercury_source_char(Char) ->
        Strings = [string.char_to_string(Char) | Strings0]
    ;
        Strings = [mercury_escape_char(Char) | Strings0]
    ).

% Note: the code of add_escaped_char and write_escaped_char should be
% kept in sync. The code of both is similar to code in
% compiler/mercury_to_mercury.m; any changes here may require
% similar changes there.

term_io.write_escaped_char(Char, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.write_escaped_char(Stream, Char, !IO).

term_io.write_escaped_char(Stream, Char, !State) :-
    ( mercury_escape_special_char(Char, QuoteChar) ->
        stream.put(Stream, ('\\'), !State),
        stream.put(Stream, QuoteChar, !State)
    ; is_mercury_source_char(Char) ->
        stream.put(Stream, Char, !State)
    ;
        stream.put(Stream, mercury_escape_char(Char), !State)
    ).

term_io.escaped_char(Char) = String :-
    string_is_escaped_char(Char, String).

:- pragma promise_equivalent_clauses(string_is_escaped_char/2).

string_is_escaped_char(Char::in, String::out) :-
    ( mercury_escape_special_char(Char, QuoteChar) ->
        String = string.append("\\", string.char_to_string(QuoteChar))
    ; is_mercury_source_char(Char) ->
        String = string.char_to_string(Char)
    ;
        String = mercury_escape_char(Char)
    ).
string_is_escaped_char(Char::out, String::in) :-
    string.to_char_list(String, Chars),
    (
        Chars = [Char],
        (
            is_mercury_source_char(Char)
        ;
            mercury_escape_special_char(Char, _QuoteChar)
        )
    ;
        Chars = ['\\', QuoteChar],
        mercury_escape_special_char(Char, QuoteChar)
    ;
        Chars = ['\\', Char1, Char2, Char3],
        NumChars = [Char1, Char2, Char3],
        string.from_char_list(NumChars, NumString),
        string.base_string_to_int(8, NumString, Int),
        char.to_int(Char, Int)
    ).

mercury_escape_char(Char) = EscapeCode :-
    char.to_int(Char, Int),
    string.int_to_base_string(Int, 8, OctalString0),
    string.pad_left(OctalString0, '0', 3, OctalString),
    EscapeCode = "\\" ++ OctalString ++ "\\".

    % Succeed if Char is a character which is allowed in Mercury string
    % and character literals.
    %
    % Note: the code here is similar to code in compiler/mercury_to_mercury.m;
    % any changes here may require similar changes there.
    %
:- pred is_mercury_source_char(char::in) is semidet.

is_mercury_source_char(Char) :-
    ( char.is_alnum(Char)
    ; is_mercury_punctuation_char(Char)
    ; char.to_int(Char) >= 0x80
    ).

    % Currently we only allow the following characters.
    % XXX should we just use is_printable(Char) instead?
    %
    % Note: the code here is similar to code in runtime/mercury_trace_base.c;
    % any changes here may require similar changes there.

is_mercury_punctuation_char(' ').
is_mercury_punctuation_char('!').
is_mercury_punctuation_char('@').
is_mercury_punctuation_char('#').
is_mercury_punctuation_char('$').
is_mercury_punctuation_char('%').
is_mercury_punctuation_char('^').
is_mercury_punctuation_char('&').
is_mercury_punctuation_char('*').
is_mercury_punctuation_char('(').
is_mercury_punctuation_char(')').
is_mercury_punctuation_char('-').
is_mercury_punctuation_char('_').
is_mercury_punctuation_char('+').
is_mercury_punctuation_char('=').
is_mercury_punctuation_char('`').
is_mercury_punctuation_char('~').
is_mercury_punctuation_char('{').
is_mercury_punctuation_char('}').
is_mercury_punctuation_char('[').
is_mercury_punctuation_char(']').
is_mercury_punctuation_char(';').
is_mercury_punctuation_char(':').
is_mercury_punctuation_char('''').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char('<').
is_mercury_punctuation_char('>').
is_mercury_punctuation_char('.').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('/').
is_mercury_punctuation_char('?').
is_mercury_punctuation_char('\\').
is_mercury_punctuation_char('|').

%-----------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(encode_escaped_char/2).

encode_escaped_char(Char::in, Str::out) :-
    ( mercury_escape_special_char(Char, EscapeChar) ->
        string.from_char_list(['\\', EscapeChar], Str)
    ; is_mercury_source_char(Char) ->
        string.from_char_list([Char], Str)
    ;
        fail
    ).
encode_escaped_char(Char::out, Str::in) :-
    string.to_char_list(Str, Chars),
    (
        Chars = [Char]
    ;
        Chars = ['\\', EscapedChar],
        mercury_escape_special_char(Char, EscapedChar)
    ).

    % mercury_escape_special_char(Char, EscapeChar) is true iff Char
    % is character for which there is a special backslash-escape character
    % EscapeChar that can be used after a backslash in string literals or
    % atoms to represent Char.
    %
    % Note: the code here is similar to code in compiler/mercury_to_mercury.m;
    % any changes here may require similar changes there.
    %
:- pred mercury_escape_special_char(char, char).
:- mode mercury_escape_special_char(in, out) is semidet.
:- mode mercury_escape_special_char(out, in) is semidet.

mercury_escape_special_char('''', '''').
mercury_escape_special_char('"', '"').
mercury_escape_special_char('\\', '\\').
mercury_escape_special_char('\n', 'n').
mercury_escape_special_char('\t', 't').
mercury_escape_special_char('\b', 'b').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

term_io.write_term_nl(VarSet, Term, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_term_nl_with_op_table(Ops, VarSet, Term, !IO).

term_io.write_term_nl_with_op_table(Ops, VarSet, Term, !IO) :-
    term_io.write_term_with_op_table(Ops, VarSet, Term, !IO),
    io.write_string(".\n", !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
