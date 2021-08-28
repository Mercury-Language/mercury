%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2009, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module term_io.
:- interface.

:- import_module char.
:- import_module io.
:- import_module ops.
:- import_module stream.
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

    % Read a term from the current input stream or from the given input stream.
    %
    % Similar to NU-Prolog read_term/2, except that resulting term
    % is in the ground representation.
    %
    % Binds Result to either `eof', `term(VarSet, Term)', or
    % `error(Message, LineNumber)'.
    %
:- pred read_term(read_term(T)::out, io::di, io::uo) is det.
:- pred read_term(io.text_input_stream::in, read_term(T)::out,
    io::di, io::uo) is det.

    % As above, except uses the given operator table instead of
    % the standard Mercury operators.
    %
:- pred read_term_with_op_table(Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).
:- pred read_term_with_op_table(io.text_input_stream::in, Ops::in,
    read_term(T)::out, io::di, io::uo) is det <= op_table(Ops).

%---------------------------------------------------------------------------%

    % Writes a term to the current output stream or to the specified output
    % stream. Uses the variable names specified by the varset.
    % Writes _N for all unnamed variables, with N starting at 0.
    %
:- pred write_term(varset(T)::in, term(T)::in, io::di, io::uo) is det.
:- pred write_term(io.output_stream::in, varset(T)::in, term(T)::in,
    io::di, io::uo) is det.

    % As above, except uses the given operator table instead of the
    % standard Mercury operators.
    %
:- pred write_term_with_op_table(Ops::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(Ops).
:- pred write_term_with_op_table(io.text_output_stream::in, Ops::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(Ops).

    % As above, except it appends a period and new-line.
    %
:- pred write_term_nl(varset(T)::in, term(T)::in, io::di, io::uo) is det.
:- pred write_term_nl(io.text_output_stream::in, varset(T)::in, term(T)::in,
    io::di, io::uo) is det.

    % As above, except it appends a period and new-line.
    %
:- pred write_term_nl_with_op_table(Ops::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(Ops).
:- pred write_term_nl_with_op_table(io.text_output_stream::in, Ops::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(Ops).

%---------------------%

    % Writes a constant (integer, float, string, or atom) to
    % the current output stream, or to the specified output stream.
    %
:- pred write_constant(const::in, io::di, io::uo) is det.
:- pred write_constant(io.text_output_stream::in, const::in,
    io::di, io::uo) is det.

    % Like write_constant, but return the result in a string.
    %
:- func format_constant(const) = string.

%---------------------%

    % Writes a variable to the current output stream, or to the
    % specified output stream.
    %
:- pred write_variable(var(T)::in, varset(T)::in, io::di, io::uo) is det.
:- pred write_variable(io.text_output_stream::in, var(T)::in, varset(T)::in,
    io::di, io::uo) is det.

    % As above, except uses the given operator table instead of the
    % standard Mercury operators.
    %
:- pred write_variable_with_op_table(Ops::in,
    var(T)::in, varset(T)::in, io::di, io::uo) is det <= op_table(Ops).
:- pred write_variable_with_op_table(io.text_output_stream::in, Ops::in,
    var(T)::in, varset(T)::in, io::di, io::uo) is det <= op_table(Ops).

%---------------------%

    % Given a character C, write C in single-quotes,
    % escaped if necessary, to stdout.
    %
:- pred quote_char(char::in, io::di, io::uo) is det.
:- pred quote_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like quote_char, but return the result in a string.
    %
:- func quoted_char(char) = string.

    % Given a character C, write C, escaped if necessary, to stdout.
    % The character is not enclosed in quotes.
    %
:- pred write_escaped_char(char::in, io::di, io::uo) is det.
:- pred write_escaped_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like write_escaped_char, but return the result in a string.
    %
:- func escaped_char(char) = string.

    % A reversible version of escaped_char.
    %
:- pred string_is_escaped_char(char, string).
:- mode string_is_escaped_char(in, out) is det.
:- mode string_is_escaped_char(out, in) is semidet.

%---------------------%

    % Given a string S, write S in double-quotes, with characters
    % escaped if necessary, to the current output stream, or to the
    % specified output stream.
    %
:- pred quote_string(string::in, io::di, io::uo) is det.
:- pred quote_string(Stream::in, string::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like quote_string, but return the result in a string.
    %
:- func quoted_string(string) = string.

    % Given a string S, write S, with characters escaped if necessary,
    % to stdout. The string is not enclosed in quotes.
    %
:- pred write_escaped_string(string::in, io::di, io::uo) is det.
:- pred write_escaped_string(Stream::in, string::in,
    State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like write_escaped_string, but return the result in a string.
    %
:- func escaped_string(string) = string.

%---------------------%

    % Given an atom-name A, write A, enclosed in single-quotes if necessary,
    % with characters escaped if necessary, to stdout.
    %
:- pred quote_atom(string::in, io::di, io::uo) is det.
:- pred quote_atom(Stream::in, string::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % Like quote_atom, but return the result in a string.
    %
:- func quoted_atom(string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

%---------------------------------------------------------------------------%

:- interface.

%---------------------------------------------------------------------------%

    % for use by io.m.

:- type adjacent_to_graphic_token
    --->    maybe_adjacent_to_graphic_token
    ;       not_adjacent_to_graphic_token.

:- pred quote_atom_agt(string::in, adjacent_to_graphic_token::in,
    io::di, io::uo) is det.

:- pred quote_atom_agt(Stream::in, string::in,
    adjacent_to_graphic_token::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- func quoted_atom_agt(string, adjacent_to_graphic_token) = string.

:- pragma type_spec(pred(term_io.quote_string/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_atom/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.write_escaped_string/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.write_escaped_char/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_char/4),
    (Stream = io.output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_atom_agt/5),
    (Stream = io.output_stream, State = io.state)).

%---------------------------------------------------------------------------%

    % Convert `integer_base' constant to its numeric value.
    %
:- func integer_base_int(integer_base) = int.

    % Return the prefix for integer literals of the given base.
    %
:- func integer_base_prefix(integer_base) = string.

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module integer.
:- import_module lexer.
:- import_module list.
:- import_module parser.
:- import_module string.
:- import_module stream.string_writer.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

read_term(Result, !IO) :-
    io.input_stream(InStream, !IO),
    term_io.read_term(InStream, Result, !IO).

read_term(InStream, Result, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.read_term_with_op_table(InStream, Ops, Result, !IO).

read_term_with_op_table(Ops, Result, !IO) :-
    io.input_stream(InStream, !IO),
    term_io.read_term_with_op_table(InStream, Ops, Result, !IO).

read_term_with_op_table(InStream, Ops, Result, !IO) :-
    parser.read_term_with_op_table(InStream, Ops, Result, !IO).

%---------------------------------------------------------------------------%

write_term(VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term(OutStream, VarSet, Term, !IO).

write_term(OutStream, VarSet, Term, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_term_with_op_table(OutStream, Ops, VarSet, Term, !IO).

write_term_with_op_table(Ops, VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_with_op_table(OutStream, Ops, VarSet, Term, !IO).

write_term_with_op_table(OutStream, Ops, VarSet, Term, !IO) :-
    write_term_anon_vars(OutStream, Ops, Term, VarSet, _, 0, _, !IO).

%---------------------%

write_term_nl(VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_nl(OutStream, VarSet, Term, !IO).

write_term_nl(OutStream, VarSet, Term, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_term_nl_with_op_table(OutStream, Ops, VarSet, Term, !IO).

write_term_nl_with_op_table(Ops, VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_nl_with_op_table(OutStream, Ops, VarSet, Term, !IO).

write_term_nl_with_op_table(OutStream, Ops, VarSet, Term, !IO) :-
    term_io.write_term_with_op_table(OutStream, Ops, VarSet, Term, !IO),
    io.write_string(OutStream, ".\n", !IO).

%---------------------------------------------------------------------------%

:- pred write_term_anon_vars(io.text_output_stream::in, Ops::in,
    term(T)::in, varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

write_term_anon_vars(OutStream, Ops, Term, !VarSet, !N, !IO) :-
    write_term_prio_anon_vars(OutStream, Ops, Term, ops.max_priority(Ops) + 1,
        !VarSet, !N, !IO).

:- pred write_term_prio_anon_vars(io.text_output_stream::in, Ops::in,
    term(T)::in, ops.priority::in, varset(T)::in, varset(T)::out,
    int::in, int::out, io::di, io::uo) is det <= op_table(Ops).

write_term_prio_anon_vars(OutStream, Ops, Term, Priority, !VarSet, !N, !IO) :-
    (
        Term = term.variable(Var, _),
        write_variable_anon_vars(OutStream, Ops, Var, !VarSet, !N, !IO)
    ;
        Term = term.functor(Functor, Args, _),
        ( if
            Functor = term.atom("[|]"),
            Args = [ListHead, ListTail]
        then
            io.write_char(OutStream, '[', !IO),
            write_term_arg(OutStream, Ops, ListHead, !VarSet, !N, !IO),
            write_later_list_elements(OutStream, Ops, ListTail,
                !VarSet, !N, !IO),
            io.write_char(OutStream, ']', !IO)
        else if
            Functor = term.atom("[]"),
            Args = []
        then
            io.write_string(OutStream, "[]", !IO)
        else if
            Functor = term.atom("{}"),
            Args = [BracedTerm]
        then
            io.write_string(OutStream, "{ ", !IO),
            write_term_anon_vars(OutStream, Ops, BracedTerm,
                !VarSet, !N, !IO),
            io.write_string(OutStream, " }", !IO)
        else if
            Functor = term.atom("{}"),
            Args = [BracedHead | BracedTail]
        then
            io.write_char(OutStream, '{', !IO),
            write_term_arg(OutStream, Ops, BracedHead, !VarSet, !N, !IO),
            write_term_later_args(OutStream, Ops, BracedTail,
                !VarSet, !N, !IO),
            io.write_char(OutStream, '}', !IO)
        else if
            % The empty functor '' is used for higher-order syntax:
            % Var(Arg, ...) gets parsed as ''(Var, Arg). When writing it out,
            % we want to use the nice syntax.
            Functor = term.atom(""),
            Args = [term.variable(Var, _), FirstArg | OtherArgs]
        then
            write_variable_anon_vars(OutStream, Ops, Var, !VarSet, !N, !IO),
            io.write_char(OutStream, '(', !IO),
            write_term_arg(OutStream, Ops, FirstArg, !VarSet, !N, !IO),
            write_term_later_args(OutStream, Ops, OtherArgs, !VarSet, !N, !IO),
            io.write_char(OutStream, ')', !IO)
        else if
            Args = [PrefixArg],
            Functor = term.atom(OpName),
            ops.lookup_prefix_op(Ops, OpName, OpPriority, OpAssoc)
        then
            maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
            write_constant(OutStream, Functor, !IO),
            io.write_char(OutStream, ' ', !IO),
            adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
            write_term_prio_anon_vars(OutStream, Ops, PrefixArg, NewPriority,
                !VarSet, !N, !IO),
            maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
        else if
            Args = [PostfixArg],
            Functor = term.atom(OpName),
            ops.lookup_postfix_op(Ops, OpName, OpPriority, OpAssoc)
        then
            maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
            adjust_priority_for_assoc(OpPriority, OpAssoc, NewPriority),
            write_term_prio_anon_vars(OutStream, Ops, PostfixArg, NewPriority,
                !VarSet, !N, !IO),
            io.write_char(OutStream, ' ', !IO),
            write_constant(OutStream, Functor, !IO),
            maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
        else if
            Args = [Arg1, Arg2],
            Functor = term.atom(OpName),
            ops.lookup_infix_op(Ops, OpName, OpPriority, LeftAssoc, RightAssoc)
        then
            maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
            adjust_priority_for_assoc(OpPriority, LeftAssoc, LeftPriority),
            write_term_prio_anon_vars(OutStream, Ops, Arg1, LeftPriority,
                !VarSet, !N, !IO),
            ( if OpName = "," then
                io.write_string(OutStream, ", ", !IO)
            else if OpName = "." then
                % If the operator is '.'/2, then we must not put spaces
                % around it (or at the very least, we should not put spaces
                % afterwards) because that would make it appear as the
                % end-of-term token. However, we do have to quote it
                % if the right hand side can begin with a digit.
                ( if starts_with_digit(Arg2) then
                    Dot = "'.'"
                else
                    Dot = "."
                ),
                io.write_string(OutStream, Dot, !IO)
            else
                io.write_char(OutStream, ' ', !IO),
                write_constant(OutStream, Functor, !IO),
                io.write_char(OutStream, ' ', !IO)
            ),
            adjust_priority_for_assoc(OpPriority, RightAssoc, RightPriority),
            write_term_prio_anon_vars(OutStream, Ops, Arg2, RightPriority,
                !VarSet, !N, !IO),
            maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
        else if
            Args = [Arg1, Arg2],
            Functor = term.atom(OpName),
            ops.lookup_binary_prefix_op(Ops, OpName, OpPriority,
                FirstAssoc, SecondAssoc)
        then
            maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
            write_constant(OutStream, Functor, !IO),
            io.write_char(OutStream, ' ', !IO),
            adjust_priority_for_assoc(OpPriority, FirstAssoc, FirstPriority),
            write_term_prio_anon_vars(OutStream, Ops, Arg1, FirstPriority,
                !VarSet, !N, !IO),
            io.write_char(OutStream, ' ', !IO),
            adjust_priority_for_assoc(OpPriority, SecondAssoc, SecondPriority),
            write_term_prio_anon_vars(OutStream, Ops, Arg2, SecondPriority,
                !VarSet, !N, !IO),
            maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
        else
            ( if
                Args = [],
                Functor = term.atom(Op),
                ops.lookup_op(Ops, Op),
                Priority =< ops.max_priority(Ops)
            then
                io.write_char(OutStream, '(', !IO),
                write_constant(OutStream, Functor, !IO),
                io.write_char(OutStream, ')', !IO)
            else
                write_constant(OutStream, Functor,
                    maybe_adjacent_to_graphic_token, !IO)
            ),
            (
                Args = [X | Xs],
                io.write_char(OutStream, '(', !IO),
                write_term_arg(OutStream, Ops, X, !VarSet, !N, !IO),
                write_term_later_args(OutStream, Ops, Xs, !VarSet, !N, !IO),
                io.write_char(OutStream, ')', !IO)
            ;
                Args = []
            )
        )
    ).

:- pred write_term_arg(io.text_output_stream::in, Ops::in,
    term(T)::in, varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

write_term_arg(OutStream, Ops, Term, !VarSet, !N, !IO) :-
    write_term_prio_anon_vars(OutStream, Ops, Term, ops.arg_priority(Ops),
        !VarSet, !N, !IO).

    % Write the remaining arguments.
    %
:- pred write_term_later_args(io.text_output_stream::in, Ops::in,
    list(term(T))::in, varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

write_term_later_args(_, _, [], !VarSet, !N, !IO).
write_term_later_args(OutStream, Ops, [X | Xs], !VarSet, !N, !IO) :-
    io.write_string(OutStream, ", ", !IO),
    write_term_arg(OutStream, Ops, X, !VarSet, !N, !IO),
    write_term_later_args(OutStream, Ops, Xs, !VarSet, !N, !IO).

:- pred write_later_list_elements(io.text_output_stream::in, Ops::in,
    term(T)::in, varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

write_later_list_elements(OutStream, Ops, Term, !VarSet, !N, !IO) :-
    ( if
        Term = term.variable(Var, _),
        varset.search_var(!.VarSet, Var, Value)
    then
        write_later_list_elements(OutStream, Ops, Value, !VarSet, !N, !IO)
    else if
        Term = term.functor(term.atom("[|]"), [ListHead, ListTail], _)
    then
        io.write_string(OutStream, ", ", !IO),
        write_term_arg(OutStream, Ops, ListHead, !VarSet, !N, !IO),
        write_later_list_elements(OutStream, Ops, ListTail, !VarSet, !N, !IO)
    else if
        Term = term.functor(term.atom("[]"), [], _)
    then
        true
    else
        io.write_string(OutStream, " | ", !IO),
        write_term_anon_vars(OutStream, Ops, Term, !VarSet, !N, !IO)
    ).

    % Succeeds iff outputting the given term would start with a digit.
    % (This is a safe, conservative approximation and is used to decide
    % whether or not to quote infix '.'/2.)
    %
:- pred starts_with_digit(term(T)::in) is semidet.

starts_with_digit(functor(integer(_, _, _, _), _, _)).
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

%---------------------------------------------------------------------------%

write_constant(Const, !IO) :-
    io.output_stream(OutStream, !IO),
    write_constant(OutStream, Const, !IO).

write_constant(OutStream, Const, !IO) :-
    write_constant(OutStream, Const,
        not_adjacent_to_graphic_token, !IO).

:- pred write_constant(io.text_output_stream::in, const::in,
    adjacent_to_graphic_token::in, io::di, io::uo) is det.

write_constant(OutStream, Const, AdjacentToGraphicToken, !IO) :-
    (
        Const = term.integer(Base, I, Signedness, Size),
        Prefix = integer_base_prefix(Base),
        IntString = integer.to_base_string(I, integer_base_int(Base)),
        Suffix = integer_signedness_and_size_suffix(Signedness, Size),
        io.write_string(OutStream, Prefix, !IO),
        io.write_string(OutStream, IntString, !IO),
        io.write_string(OutStream, Suffix, !IO)
    ;
        Const = term.float(F),
        io.write_float(OutStream, F, !IO)
    ;
        Const = term.atom(A),
        term_io.quote_atom_agt(OutStream, A, AdjacentToGraphicToken, !IO)
    ;
        Const = term.string(S),
        term_io.quote_string(OutStream, S, !IO)
    ;
        Const = term.implementation_defined(N),
        io.write_char(OutStream, '$', !IO),
        io.write_string(OutStream, N, !IO)
    ).

format_constant(Const) =
    term_io.format_constant_agt(Const, not_adjacent_to_graphic_token).

:- func format_constant_agt(const, adjacent_to_graphic_token) = string.

format_constant_agt(Const, AdjacentToGraphicToken) = Str :-
    (
        Const = term.integer(Base, I, Signedness, Size),
        Str = integer_base_prefix(Base) ++
            to_base_string(I, integer_base_int(Base)) ++
            integer_signedness_and_size_suffix(Signedness, Size)
    ;
        Const = term.float(F),
        Str = string.float_to_string(F)
    ;
        Const = term.atom(A),
        Str = term_io.quoted_atom_agt(A, AdjacentToGraphicToken)
    ;
        Const = term.string(S),
        Str = term_io.quoted_string(S)
    ;
        Const = term.implementation_defined(N),
        Str = "$" ++ N
    ).

:- func integer_signedness_and_size_suffix(term.signedness,
    term.integer_size) = string.

integer_signedness_and_size_suffix(signed, size_word) = "".
integer_signedness_and_size_suffix(signed, size_8_bit) = "i8".
integer_signedness_and_size_suffix(signed, size_16_bit) = "i16".
integer_signedness_and_size_suffix(signed, size_32_bit) = "i32".
integer_signedness_and_size_suffix(signed, size_64_bit) = "i64".
integer_signedness_and_size_suffix(unsigned, size_word) = "u".
integer_signedness_and_size_suffix(unsigned, size_8_bit) = "u8".
integer_signedness_and_size_suffix(unsigned, size_16_bit) = "u16".
integer_signedness_and_size_suffix(unsigned, size_32_bit) = "u32".
integer_signedness_and_size_suffix(unsigned, size_64_bit) = "u64".

%---------------------------------------------------------------------------%

write_variable(Var, VarSet, !IO) :-
    io.output_stream(OutStream, !IO),
    write_variable(OutStream, Var, VarSet, !IO).

write_variable(OutStream, Var, VarSet, !IO) :-
    io.get_op_table(Ops, !IO),
    term_io.write_variable_with_op_table(OutStream, Ops, Var, VarSet, !IO).

write_variable_with_op_table(Ops, Var, VarSet, !IO) :-
    io.output_stream(OutStream, !IO),
    write_variable_with_op_table(OutStream, Ops, Var, VarSet, !IO).

write_variable_with_op_table(OutStream, Ops, Var, VarSet, !IO) :-
    write_variable_anon_vars(OutStream, Ops, Var, VarSet, _, 0, _, !IO).

    % Write a variable.
    %
    % There are two ways we could choose to write unnamed variables.
    %
    % 1 Convert the variable to the integer that represents it and write
    %   `_N' where N is that integer. This has the advantage that
    %    such variables get printed in a canonical way, so rearranging terms
    %    containing such variables will not effect the way they are numbered
    %    (this includes breaking up a term and printing the pieces separately).
    %
    % 2 Number the unnamed variables from 0 and write `_N' where
    %   N is the next number in the sequence of such variables.
    %   This has the advantage that such variables can be visually scanned
    %   rather more easily (for example in error messages).
    %
    % An ideal solution would be to provide both, and a flag to choose
    % between the two. At the moment we provide only the first, though
    % the infrastructure for the second is present in the code.
    % That infrastructure is the threading of the (as yet unused)
    % !N state variables through the code that writes terms.
    %
:- pred write_variable_anon_vars(io.text_output_stream::in, Ops::in,
    var(T)::in, varset(T)::in, varset(T)::out, int::in, int::out,
    io::di, io::uo) is det <= op_table(Ops).

write_variable_anon_vars(OutStream, Ops, Var, !VarSet, !N, !IO) :-
    ( if varset.search_var(!.VarSet, Var, Value) then
        write_term_anon_vars(OutStream, Ops, Value, !VarSet, !N, !IO)
    else if varset.search_name(!.VarSet, Var, Name) then
        io.write_string(OutStream, Name, !IO)
    else
        % XXX The names we generate here, with either approach,
        % *could* clash with the name of an explicit-named variable.

        % This code implements the first approach described above.
        term.var_to_int(Var, VarNum),

        % This code would implement the second approach described above.
        % VarNum = !.N,
        % !:N = !.N + 1,

        string.int_to_string(VarNum, VarNumStr),
        VarName = "_" ++ VarNumStr,

        % Recording the name we have given Var in !VarSet is needed
        % only with the second approach. The first would give the same
        % name to the same variable even without it, but since it would
        % allocate memory on *every* occurrence of the variable rather than
        % on just the first one, we record the name anyway.
        varset.name_var(Var, VarName, !VarSet),
        io.write_string(OutStream, VarName, !IO)
    ).

%---------------------------------------------------------------------------%

quote_char(C, !IO) :-
    io.output_stream(OutStream, !IO),
    io.write_string(OutStream, term_io.quoted_char(C), !IO).

quote_char(Stream, C, !State) :-
    stream.put(Stream, term_io.quoted_char(C), !State).

quoted_char(C) =
    string.format("'%s'", [s(term_io.escaped_char(C))]).

write_escaped_char(Char, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.write_escaped_char(Stream, Char, !IO).

write_escaped_char(Stream, Char, !State) :-
    % Note: the code of add_escaped_char and write_escaped_char
    % should be kept in sync. The code of both is similar to code in
    % compiler/parse_tree_out_pragma.m and MR_escape_string_quote
    % in runtime/mercury_string.c; any changes here may require similar
    % changes in those spots.
    ( if mercury_escape_special_char(Char, QuoteChar) then
        stream.put(Stream, ('\\'), !State),
        stream.put(Stream, QuoteChar, !State)
    else if is_mercury_source_char(Char) then
        stream.put(Stream, Char, !State)
    else
        stream.put(Stream, mercury_escape_char(Char), !State)
    ).

escaped_char(Char) = String :-
    string_is_escaped_char(Char, String).

:- pragma promise_equivalent_clauses(pred(string_is_escaped_char/2)).

string_is_escaped_char(Char::in, String::out) :-
    ( if mercury_escape_special_char(Char, QuoteChar) then
        String = string.append("\\", string.char_to_string(QuoteChar))
    else if is_mercury_source_char(Char) then
        String = string.char_to_string(Char)
    else
        String = mercury_escape_char(Char)
    ).
string_is_escaped_char(Char::out, String::in) :-
    % XXX ILSEQ Decide what to do with ill-formed sequences.
    string.to_char_list(String, Chars),
    (
        Chars = [Char],
        ( is_mercury_source_char(Char)
        ; mercury_escape_special_char(Char, _QuoteChar)
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

    % Succeed if Char is a character which is allowed in Mercury string
    % and character literals.
    %
    % Note: the code here is similar to code in the following spots:
    %
    %   - compiler/parse_tree_out_pragma.m.
    %   - runtime/mercury_trace_base.c
    %
    % Any changes here may require similar changes there.
    %
:- pred is_mercury_source_char(char::in) is semidet.

is_mercury_source_char(Char) :-
    ( char.is_alnum(Char)
    ; is_mercury_punctuation_char(Char)
    ; char.to_int(Char) >= 0xA0  % 0x7f - 0x9f are control characters.
    ).

%---------------------------------------------------------------------------%

% Note: the code here is similar to code in compiler/parse_tree_out_pragma.m;
% any changes here may require similar changes there.

quote_string(S, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.quote_string(Stream, S, !IO).

quote_string(Stream, S, !State) :-
    stream.put(Stream, '"', !State),
    term_io.write_escaped_string(Stream, S, !State),
    stream.put(Stream, '"', !State).

quoted_string(S) =
    string.append_list(["""", term_io.escaped_string(S), """"]).

write_escaped_string(String, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.write_escaped_string(Stream, String, !IO).

write_escaped_string(Stream, String, !State) :-
    % XXX ILSEQ Decide what to do with ill-formed sequences.
    string.foldl(term_io.write_escaped_char(Stream), String, !State).

escaped_string(String) =
    % XXX ILSEQ Decide what to do with ill-formed sequences.
    string.append_list(
        list.reverse(string.foldl(add_escaped_char, String, []))).

:- func add_escaped_char(char, list(string)) = list(string).

add_escaped_char(Char, Strings0) = Strings :-
    % Note: the code of add_escaped_char and write_escaped_char
    % should be kept in sync. The code of both is similar to code in
    % compiler/parse_tree_out_pragma.m; any changes here may require
    % similar changes there.
    ( if mercury_escape_special_char(Char, QuoteChar) then
        Strings = [from_char_list(['\\', QuoteChar]) | Strings0]
    else if is_mercury_source_char(Char) then
        Strings = [string.char_to_string(Char) | Strings0]
    else
        Strings = [mercury_escape_char(Char) | Strings0]
    ).

%---------------------------------------------------------------------------%

quote_atom(S, !IO) :-
    term_io.quote_atom_agt(S, not_adjacent_to_graphic_token, !IO).

quote_atom(Stream, S, !State) :-
    term_io.quote_atom_agt(Stream, S, not_adjacent_to_graphic_token, !State).

quoted_atom(S) =
    term_io.quoted_atom_agt(S, not_adjacent_to_graphic_token).

quote_atom_agt(S, AdjacentToGraphicToken, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.quote_atom_agt(Stream, S, AdjacentToGraphicToken, !IO).

quote_atom_agt(Stream, S, AdjacentToGraphicToken, !State) :-
    ShouldQuote = should_atom_be_quoted(S, AdjacentToGraphicToken),
    (
        ShouldQuote = no,
        stream.put(Stream, S, !State)
    ;
        ShouldQuote = yes,
        stream.put(Stream, '''', !State),
        term_io.write_escaped_string(Stream, S, !State),
        stream.put(Stream, '''', !State)
    ).

quoted_atom_agt(S, AdjacentToGraphicToken) = String :-
    ShouldQuote = should_atom_be_quoted(S, AdjacentToGraphicToken),
    (
        ShouldQuote = no,
        String = S
    ;
        ShouldQuote = yes,
        ES = term_io.escaped_string(S),
        String = string.append_list(["'", ES, "'"])
    ).

:- func should_atom_be_quoted(string, adjacent_to_graphic_token) = bool.

should_atom_be_quoted(S, AdjacentToGraphicToken) = ShouldQuote :-
    ( if
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
            AdjacentToGraphicToken = not_adjacent_to_graphic_token
        ;
            % 6.3.1.3: atom = open list, close list ;
            S = "[]"
        ;
            % 6.3.1.3: atom = open curly, close curly ;
            S = "{}"
        )
    then
        ShouldQuote = no
    else
        % Anything else must be output as a quoted token (6.4.2).
        ShouldQuote = yes
    ).

%---------------------------------------------------------------------------%

integer_base_int(base_2) = 2.
integer_base_int(base_8) = 8.
integer_base_int(base_10) = 10.
integer_base_int(base_16) = 16.

integer_base_prefix(base_2) = "0b".
integer_base_prefix(base_8) = "0o".
integer_base_prefix(base_10) = "".
integer_base_prefix(base_16) = "0x".

%---------------------------------------------------------------------------%

mercury_escape_char(Char) = EscapeCode :-
    char.to_int(Char, Int),
    string.int_to_base_string(Int, 8, OctalString0),
    string.pad_left(OctalString0, '0', 3, OctalString),
    EscapeCode = "\\" ++ OctalString ++ "\\".

%---------------------------------------------------------------------------%

    % Currently we only allow the following characters.
    % XXX should we just use is_printable(Char) instead?
    %
    % Note: the code here is similar to code in runtime/mercury_trace_base.c;
    % any changes here may require similar changes there.

% Codepoints in 0x20..0x2f.
is_mercury_punctuation_char(' ').
is_mercury_punctuation_char('!').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char('#').
is_mercury_punctuation_char('$').
is_mercury_punctuation_char('%').
is_mercury_punctuation_char('&').
is_mercury_punctuation_char('''').
is_mercury_punctuation_char('(').
is_mercury_punctuation_char(')').
is_mercury_punctuation_char('*').
is_mercury_punctuation_char('+').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('-').
is_mercury_punctuation_char('.').
is_mercury_punctuation_char('/').
% Codepoints in 0x3a..0x40.
is_mercury_punctuation_char(':').
is_mercury_punctuation_char(';').
is_mercury_punctuation_char('<').
is_mercury_punctuation_char('=').
is_mercury_punctuation_char('>').
is_mercury_punctuation_char('?').
is_mercury_punctuation_char('@').
% Codepoints in 0x5b..0x60.
is_mercury_punctuation_char('[').
is_mercury_punctuation_char('\\').
is_mercury_punctuation_char(']').
is_mercury_punctuation_char('^').
is_mercury_punctuation_char('_').
is_mercury_punctuation_char('`').
% Codpoints in 0x7b..0x7e.
is_mercury_punctuation_char('{').
is_mercury_punctuation_char('|').
is_mercury_punctuation_char('~').
is_mercury_punctuation_char('}').

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(encode_escaped_char/2)).

encode_escaped_char(Char::in, Str::out) :-
    ( if mercury_escape_special_char(Char, EscapeChar) then
        string.from_char_list(['\\', EscapeChar], Str)
    else if is_mercury_source_char(Char) then
        string.from_char_list([Char], Str)
    else
        fail
    ).
encode_escaped_char(Char::out, Str::in) :-
    % XXX ILSEQ Decide what to do with ill-formed sequences.
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
    % Likewise for the similar code in library/rtti_implementation.m.
    %
:- pred mercury_escape_special_char(char, char).
:- mode mercury_escape_special_char(in, out) is semidet.
:- mode mercury_escape_special_char(out, in) is semidet.

mercury_escape_special_char('\a', 'a').
mercury_escape_special_char('\b', 'b').
mercury_escape_special_char('\f', 'f').
mercury_escape_special_char('\n', 'n').
mercury_escape_special_char('\r', 'r').
mercury_escape_special_char('\t', 't').
mercury_escape_special_char('\v', 'v').
mercury_escape_special_char('\\', '\\').
mercury_escape_special_char('''', '''').
mercury_escape_special_char('"', '"').

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
