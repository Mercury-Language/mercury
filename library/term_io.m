%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2009, 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_io.m.
% Main author: fjh.
% Stability: medium to high.
%
% This module provides predicates to write out terms that use the ground
% representation defined in term.m.
%
% Predicates to read in such terms are available in mercury_term_parser.m.
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

    % Writes a term to the current output stream or to the specified output
    % stream. Uses the variable names specified by the varset.
    % Writes _N for all unnamed variables, with N starting at 0.
    %
:- pred write_term(varset(T)::in, term(T)::in, io::di, io::uo) is det.
:- pred write_term(io.text_output_stream::in, varset(T)::in, term(T)::in,
    io::di, io::uo) is det.

    % As above, except uses the given operator table instead of the
    % standard Mercury operators.
    %
:- pred write_term_with_op_table(OpTable::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(OpTable).
:- pred write_term_with_op_table(io.text_output_stream::in, OpTable::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(OpTable).

    % As write_term, except it appends a period and a newline.
    %
:- pred write_term_nl(varset(T)::in, term(T)::in, io::di, io::uo) is det.
:- pred write_term_nl(io.text_output_stream::in, varset(T)::in, term(T)::in,
    io::di, io::uo) is det.

    % As write_term_with_op_table above, except it appends a period
    % and a newline.
    %
:- pred write_term_nl_with_op_table(OpTable::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(OpTable).
:- pred write_term_nl_with_op_table(io.text_output_stream::in, OpTable::in,
    varset(T)::in, term(T)::in, io::di, io::uo) is det <= op_table(OpTable).

%---------------------%

    % Convert the given constant to a string.
    %
:- func format_constant(const) = string.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(func(format_constant/1), [constant_to_string/1]).
:- func constant_to_string(const) = string.

    % Writes a constant (integer, float, string, or atom) to
    % the current output stream, or to the specified output stream.
    %
:- pred write_constant(const::in, io::di, io::uo) is det.
:- pred write_constant(io.text_output_stream::in, const::in,
    io::di, io::uo) is det.
:- pred format_constant(Stream::in, const::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

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
:- pred write_variable_with_op_table(OpTable::in,
    var(T)::in, varset(T)::in, io::di, io::uo) is det <= op_table(OpTable).
:- pred write_variable_with_op_table(io.text_output_stream::in, OpTable::in,
    var(T)::in, varset(T)::in, io::di, io::uo) is det <= op_table(OpTable).

%---------------------%

    % Given a character C, return C, escaped if necessary, in single-quotes.
    %
:- func quoted_char(char) = string.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(func(quoted_char/1), [quoted_char_to_string/1]).
:- func quoted_char_to_string(char) = string.

    % Given a character C, write C, escaped if necessary, in single-quotes,
    % to the current output stream, or to the specified output stream.
    %
:- pred quote_char(char::in, io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_char/3), [write_quoted_char/3]).
:- pred write_quoted_char(char::in, io::di, io::uo) is det.
:- pred write_quoted_char(io.text_output_stream::in, char::in,
    io::di, io::uo) is det.
:- pred quote_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_char/4), [format_quoted_char/4]).
:- pred format_quoted_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

%---------------------%

    % Given a character C, return C, escaped if necessary.
    % Do not enclose it in single-quotes.
    %
:- func escaped_char(char) = string.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(func(escaped_char/1), [escaped_char_to_string/1]).
:- func escaped_char_to_string(char) = string.

    % Given a character C, write C, escaped if necessary,
    % and not enclosed in single-quotes, to the current output stream,
    % or to the specified output stream.
    %
:- pred write_escaped_char(char::in, io::di, io::uo) is det.
:- pred write_escaped_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(write_escaped_char/4), [format_escaped_char/4]).
:- pred format_escaped_char(Stream::in, char::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

    % A reversible version of escaped_char_to_string.
    %
:- pred string_is_escaped_char(char, string).
:- mode string_is_escaped_char(in, out) is det.
:- mode string_is_escaped_char(out, in) is semidet.

%---------------------%

    % Given a string S, return a version of S, with its characters escaped
    % if necessary, in double-quotes.
    %
:- func quoted_string(string) = string.

    % Given a string S, write a version of S, with its characters escaped
    % if necessary, in double-quotes, to the current output stream,
    % or to the specified output stream.
    %
:- pred quote_string(string::in, io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_string/3), [write_quoted_string/3]).
:- pred write_quoted_string(string::in, io::di, io::uo) is det.
:- pred write_quoted_string(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.
:- pred quote_string(Stream::in, string::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_string/4), [format_quoted_string/4]).
:- pred format_quoted_string(Stream::in, string::in, State::di, State::uo)
    is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

%---------------------%

    % Given a string S, return a version of S in which its characters
    % are escaped if necessary. Do not enclose the string in quotes.
    %
:- func escaped_string(string) = string.

    % Given a string S, write a version of S in which its characters
    % are escaped if necessary. Do not enclose the string in quotes.
    % Write it to the current output stream, or to the specified output stream.
    %
:- pred write_escaped_string(string::in, io::di, io::uo) is det.
:- pred write_escaped_string(Stream::in, string::in, State::di, State::uo)
    is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(write_escaped_string/4), [format_escaped_string/4]).
:- pred format_escaped_string(Stream::in, string::in, State::di, State::uo)
    is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

%---------------------%

    % Given a string S, return a version of S in which its characters
    % are escaped if necessary. Enclose the string in quotes.
    %
:- func quoted_atom(string) = string.

    % Given a string S, write a version of S in which its characters
    % are escaped if necessary. Enclose the string in quotes.
    % Write it to the current output stream, or to the specified output stream.
    %
:- pred quote_atom(string::in, io::di, io::uo) is det.
:- pred write_quoted_atom(string::in, io::di, io::uo) is det.
:- pred write_quoted_atom(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

:- pred quote_atom(Stream::in, string::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_atom/4), [format_quoted_atom/4]).
:- pred format_quoted_atom(Stream::in, string::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

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

:- func quoted_atom_agt(string, adjacent_to_graphic_token) = string.

:- pred quote_atom_agt(string::in, adjacent_to_graphic_token::in,
    io::di, io::uo) is det.
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_atom_agt/4), [write_quoted_atom_agt/4]).
:- pred write_quoted_atom_agt(string::in,
    adjacent_to_graphic_token::in, io::di, io::uo) is det.
:- pred write_quoted_atom_agt(io.text_output_stream::in, string::in,
    adjacent_to_graphic_token::in, io::di, io::uo) is det.

:- pred quote_atom_agt(Stream::in, string::in,
    adjacent_to_graphic_token::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
% NOTE_TO_IMPLEMENTORS OBS :- pragma obsolete(pred(quote_atom_agt/5), [format_quoted_atom_agt/5]).
:- pred format_quoted_atom_agt(Stream::in, string::in,
    adjacent_to_graphic_token::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

:- pragma type_spec(pred(term_io.quote_string/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_atom/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.write_escaped_string/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.write_escaped_char/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_char/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.format_quoted_char/4),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.quote_atom_agt/5),
    (Stream = io.text_output_stream, State = io.state)).
:- pragma type_spec(pred(term_io.format_quoted_atom_agt/5),
    (Stream = io.text_output_stream, State = io.state)).

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
:- import_module counter.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module mercury_term_lexer.
:- import_module require.
:- import_module string.
:- import_module string.builder.
:- import_module stream.string_writer.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

write_term(VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term(OutStream, VarSet, Term, !IO).

write_term(OutStream, VarSet, Term, !IO) :-
    OpTable = init_mercury_op_table,
    write_term_with_op_table(OutStream, OpTable, VarSet, Term, !IO).

write_term_with_op_table(OpTable, VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_with_op_table(OutStream, OpTable, VarSet, Term, !IO).

write_term_with_op_table(OutStream, OpTable, VarSet, Term, !IO) :-
    write_term_anon_vars(OutStream, OpTable, Term, VarSet, _,
        anon_var_to_int, _, !IO).

%---------------------%

write_term_nl(VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_nl(OutStream, VarSet, Term, !IO).

write_term_nl(OutStream, VarSet, Term, !IO) :-
    OpTable = init_mercury_op_table,
    write_term_nl_with_op_table(OutStream, OpTable, VarSet, Term, !IO).

write_term_nl_with_op_table(OpTable, VarSet, Term, !IO) :-
    io.output_stream(OutStream, !IO),
    write_term_nl_with_op_table(OutStream, OpTable, VarSet, Term, !IO).

write_term_nl_with_op_table(OutStream, OpTable, VarSet, Term, !IO) :-
    write_term_with_op_table(OutStream, OpTable, VarSet, Term, !IO),
    io.write_string(OutStream, ".\n", !IO).

%---------------------------------------------------------------------------%

:- pred write_term_anon_vars(io.text_output_stream::in, OpTable::in,
    term(T)::in, varset(T)::in, varset(T)::out,
    anon_var_info::in, anon_var_info::out, io::di, io::uo) is det
    <= op_table(OpTable).

write_term_anon_vars(OutStream, OpTable, Term, !VarSet, !Anon, !IO) :-
    write_term_prio_anon_vars(OutStream, OpTable, Term,
        ops.universal_priority(OpTable), !VarSet, !Anon, !IO).

:- pred write_term_prio_anon_vars(io.text_output_stream::in, OpTable::in,
    term(T)::in, ops.priority::in, varset(T)::in, varset(T)::out,
    anon_var_info::in, anon_var_info::out, io::di, io::uo) is det
    <= op_table(OpTable).

write_term_prio_anon_vars(OutStream, OpTable, Term, Priority,
        !VarSet, !Anon, !IO) :-
    (
        Term = term.variable(Var, _),
        write_variable_anon_vars(OutStream, OpTable, Var, !VarSet, !Anon, !IO)
    ;
        Term = term.functor(Functor, ArgTerms, _),
        (
            ( Functor = term.integer(_, _, _, _)
            ; Functor = term.string(_)
            ; Functor = term.float(_)
            ; Functor = term.implementation_defined(_)
            ),
            % Terms with these functors should NOT have arguments.
            (
                ArgTerms = [],
                format_constant_agt(OutStream, Functor,
                    maybe_adjacent_to_graphic_token, !IO)
            ;
                ArgTerms = [_ | _],
                unexpected($pred, "constant has arguments")
            )
        ;
            Functor = atom(Atom),
            write_atom_term_prio_anon_vars(OutStream, OpTable, Atom, ArgTerms,
                Priority, !VarSet, !Anon, !IO)
        )
    ).

:- pred write_atom_term_prio_anon_vars(io.text_output_stream::in,
    OpTable::in, string::in, list(term(T))::in, ops.priority::in,
    varset(T)::in, varset(T)::out, anon_var_info::in, anon_var_info::out,
    io::di, io::uo) is det <= op_table(OpTable).

write_atom_term_prio_anon_vars(OutStream, OpTable, Atom, ArgTerms,
        Priority, !VarSet, !Anon, !IO) :-
    % NOTE It would be nice to handle the first four conditions at once
    % with code such as
    %   ( if
    %       ( Atom = "[|]"
    %       ; Atom = "[]"
    %       ; Atom = "{}"
    %       ; Atom = ""
    %       )
    %   then
    %       ...
    %   else
    %       ...
    %   )
    %
    % Unfortunately, three things conspire together to prevent code
    % along those lines from being an improvement:
    %
    % - the fact that the special-case codes for these values of Atom 
    %   apply only if ArgTerms has the right shape,
    %
    % - the fact that these "right shapes" are different for these
    %   values of Atom, and
    %
    % - that the fallback code, which we should execute if ArgTerms
    %   does NOT have the right shape, is more complicated than a simple call
    %   to write_atom_term_prio_anon_vars_std, since it must also consider
    %   the possibility that Atom is an operator in a nonstandard OpTable.
    ( if
        Atom = "[|]",
        ArgTerms = [ListHead, ListTail]
    then
        io.write_char(OutStream, '[', !IO),
        write_term_arg(OutStream, OpTable, ListHead, !VarSet, !Anon, !IO),
        write_later_list_elements(OutStream, OpTable, ListTail,
            !VarSet, !Anon, !IO),
        io.write_char(OutStream, ']', !IO)
    else if
        Atom = "[]",
        ArgTerms = []
    then
        io.write_string(OutStream, "[]", !IO)
    else if
        Atom = "{}",
        ArgTerms = [BracedHeadTerm | BracedTailTerms]
    then
        (
            BracedTailTerms = [],
            % Add spaces around the one argument term.
            io.write_string(OutStream, "{ ", !IO),
            write_term_anon_vars(OutStream, OpTable, BracedHeadTerm,
                !VarSet, !Anon, !IO),
            io.write_string(OutStream, " }", !IO)
        ;
            BracedTailTerms = [_ | _],
            % Do not add spaces around the several argument terms.
            io.write_char(OutStream, '{', !IO),
            write_term_arg(OutStream, OpTable, BracedHeadTerm,
                !VarSet, !Anon, !IO),
            write_term_later_args(OutStream, OpTable, BracedTailTerms,
                !VarSet, !Anon, !IO),
            io.write_char(OutStream, '}', !IO)
        )
    else if
        % The empty functor '' is used for higher-order syntax:
        % Var(Arg, ...) gets parsed as ''(Var, Arg). When writing it out,
        % we want to use the nice syntax.
        Atom = "",
        ArgTerms = [term.variable(Var, _), FirstArg | OtherArgTerms]
    then
        write_variable_anon_vars(OutStream, OpTable, Var,
            !VarSet, !Anon, !IO),
        io.write_char(OutStream, '(', !IO),
        write_term_arg(OutStream, OpTable, FirstArg, !VarSet, !Anon, !IO),
        write_term_later_args(OutStream, OpTable, OtherArgTerms,
            !VarSet, !Anon, !IO),
        io.write_char(OutStream, ')', !IO)
    else if
        lookup_op_infos(OpTable, Atom, OpInfos)
    then
        OpInfos = op_infos(MaybeInfix, MaybeBinPrefix,
            MaybePrefix, MaybePostfix),
        (
            ( ArgTerms = []
            ; ArgTerms = [_, _, _ | _]
            ),
            write_atom_term_prio_anon_vars_std(OutStream, OpTable,
                Atom, ArgTerms, Priority, !VarSet, !Anon, !IO)
        ;
            ArgTerms = [ArgTerm1],
            ( if MaybePrefix = pre(OpPriority, OpGtOrGe) then
                maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
                format_constant_atom(OutStream, Atom, !IO),
                io.write_char(OutStream, ' ', !IO),
                NewPriority = min_priority_for_arg(OpPriority, OpGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm1,
                    NewPriority, !VarSet, !Anon, !IO),
                maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
            else if MaybePostfix = post(OpPriority, OpGtOrGe) then
                maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
                NewPriority = min_priority_for_arg(OpPriority, OpGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm1,
                    NewPriority, !VarSet, !Anon, !IO),
                io.write_char(OutStream, ' ', !IO),
                format_constant_atom(OutStream, Atom, !IO),
                maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
            else
                write_atom_term_prio_anon_vars_std(OutStream, OpTable,
                    Atom, ArgTerms, Priority, !VarSet, !Anon, !IO)
            )
        ;
            ArgTerms = [ArgTerm1, ArgTerm2],
            ( if
                MaybeInfix = in(OpPriority, LeftGtOrGe, RightGtOrGe)
            then
                maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
                LeftPriority = min_priority_for_arg(OpPriority, LeftGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm1,
                    LeftPriority, !VarSet, !Anon, !IO),
                ( if Atom = "," then
                    io.write_string(OutStream, ", ", !IO)
                else if Atom = "." then
                    % If the operator is '.'/2, then we must not put spaces
                    % around it (or at the very least, we should not put spaces
                    % afterwards) because that would make it appear as the
                    % end-of-term token. However, we do have to quote it
                    % if the right hand side can begin with a digit.
                    ( if starts_with_digit(ArgTerm2) then
                        Dot = "'.'"
                    else
                        Dot = "."
                    ),
                    io.write_string(OutStream, Dot, !IO)
                else
                    io.write_char(OutStream, ' ', !IO),
                    format_constant_atom(OutStream, Atom, !IO),
                    io.write_char(OutStream, ' ', !IO)
                ),
                RightPriority = min_priority_for_arg(OpPriority, RightGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm2,
                    RightPriority, !VarSet, !Anon, !IO),
                maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
            else if
                MaybeBinPrefix = bin_pre(OpPriority, LeftGtOrGe, RightGtOrGe)
            then
                maybe_write_paren(OutStream, '(', Priority, OpPriority, !IO),
                format_constant_atom(OutStream, Atom, !IO),
                io.write_char(OutStream, ' ', !IO),
                LeftPriority = min_priority_for_arg(OpPriority, LeftGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm1,
                    LeftPriority, !VarSet, !Anon, !IO),
                io.write_char(OutStream, ' ', !IO),
                RightPriority = min_priority_for_arg(OpPriority, RightGtOrGe),
                write_term_prio_anon_vars(OutStream, OpTable, ArgTerm2,
                    RightPriority, !VarSet, !Anon, !IO),
                maybe_write_paren(OutStream, ')', Priority, OpPriority, !IO)
            else
                write_atom_term_prio_anon_vars_std(OutStream, OpTable,
                    Atom, ArgTerms, Priority, !VarSet, !Anon, !IO)
            )
        )
    else
        write_atom_term_prio_anon_vars_std(OutStream, OpTable, Atom, ArgTerms,
            Priority, !VarSet, !Anon, !IO)
    ).

:- pred write_atom_term_prio_anon_vars_std(io.text_output_stream::in,
    OpTable::in, string::in, list(term(T))::in, ops.priority::in,
    varset(T)::in, varset(T)::out, anon_var_info::in, anon_var_info::out,
    io::di, io::uo) is det <= op_table(OpTable).

write_atom_term_prio_anon_vars_std(OutStream, OpTable, Atom, ArgTerms,
        Priority, !VarSet, !Anon, !IO) :-
    ( if
        ArgTerms = [],
        ops.is_op(OpTable, Atom),
        priority_ge(Priority, ops.loosest_op_priority(OpTable))
    then
        io.write_char(OutStream, '(', !IO),
        format_constant_atom(OutStream, Atom, !IO),
        io.write_char(OutStream, ')', !IO)
    else
        format_constant_atom(OutStream, Atom, !IO)
    ),
    (
        ArgTerms = [HeadArgTerm | TailArgTerms],
        io.write_char(OutStream, '(', !IO),
        write_term_arg(OutStream, OpTable, HeadArgTerm, !VarSet, !Anon, !IO),
        write_term_later_args(OutStream, OpTable, TailArgTerms,
            !VarSet, !Anon, !IO),
        io.write_char(OutStream, ')', !IO)
    ;
        ArgTerms = []
    ).

:- pred write_term_arg(io.text_output_stream::in, OpTable::in,
    term(T)::in, varset(T)::in, varset(T)::out, anon_var_info::in,
    anon_var_info::out,
    io::di, io::uo) is det <= op_table(OpTable).

write_term_arg(OutStream, OpTable, Term, !VarSet, !Anon, !IO) :-
    write_term_prio_anon_vars(OutStream, OpTable, Term,
        ops.arg_priority(OpTable), !VarSet, !Anon, !IO).

    % Write the remaining arguments.
    %
:- pred write_term_later_args(io.text_output_stream::in, OpTable::in,
    list(term(T))::in, varset(T)::in, varset(T)::out,
    anon_var_info::in, anon_var_info::out, io::di, io::uo) is det
    <= op_table(OpTable).

write_term_later_args(_, _, [], !VarSet, !Anon, !IO).
write_term_later_args(OutStream, OpTable, [X | Xs], !VarSet, !Anon, !IO) :-
    io.write_string(OutStream, ", ", !IO),
    write_term_arg(OutStream, OpTable, X, !VarSet, !Anon, !IO),
    write_term_later_args(OutStream, OpTable, Xs, !VarSet, !Anon, !IO).

:- pred write_later_list_elements(io.text_output_stream::in, OpTable::in,
    term(T)::in, varset(T)::in, varset(T)::out,
    anon_var_info::in, anon_var_info::out, io::di, io::uo) is det
    <= op_table(OpTable).

write_later_list_elements(OutStream, OpTable, Term, !VarSet, !Anon, !IO) :-
    ( if
        Term = term.variable(Var, _),
        varset.search_var(!.VarSet, Var, Value)
    then
        write_later_list_elements(OutStream, OpTable, Value,
            !VarSet, !Anon, !IO)
    else if
        Term = term.functor(term.atom("[|]"), [ListHead, ListTail], _)
    then
        io.write_string(OutStream, ", ", !IO),
        write_term_arg(OutStream, OpTable, ListHead, !VarSet, !Anon, !IO),
        write_later_list_elements(OutStream, OpTable, ListTail,
            !VarSet, !Anon, !IO)
    else if
        Term = term.functor(term.atom("[]"), [], _)
    then
        true
    else
        io.write_string(OutStream, " | ", !IO),
        write_term_anon_vars(OutStream, OpTable, Term, !VarSet, !Anon, !IO)
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

format_constant(Const) =
    constant_to_string(Const).

constant_to_string(Const) = Str :-
    AGT = not_adjacent_to_graphic_token,
    State0 = string.builder.init,
    format_constant_agt(string.builder.handle, Const, AGT, State0, State),
    Str = string.builder.to_string(State).

write_constant(Const, !IO) :-
    io.output_stream(OutStream, !IO),
    write_constant(OutStream, Const, !IO).

write_constant(OutStream, Const, !IO) :-
    AGT = not_adjacent_to_graphic_token,
    format_constant_agt(OutStream, Const, AGT, !IO).

format_constant(Stream, Const, !State) :-
    AGT = not_adjacent_to_graphic_token,
    format_constant_agt(Stream, Const, AGT, !State).

:- pred format_constant_agt(Stream::in, const::in,
    adjacent_to_graphic_token::in, State::di, State::uo) is det
    <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).

format_constant_agt(Stream, Const, AGT, !State) :-
    (
        Const = term.integer(Base, Int, Signedness, Size),
        Prefix = integer_base_prefix(Base),
        IntStr = integer.to_base_string(Int, integer_base_int(Base)),
        Suffix = integer_signedness_and_size_suffix(Signedness, Size),
        stream.put(Stream, Prefix, !State),
        stream.put(Stream, IntStr, !State),
        stream.put(Stream, Suffix, !State)
    ;
        Const = term.float(Float),
        put_float(Stream, Float, !State)
    ;
        Const = term.atom(Atom),
        term_io.format_quoted_atom_agt(Stream, Atom, AGT, !State)
    ;
        Const = term.string(Str),
        term_io.format_quoted_string(Stream, Str, !State)
    ;
        Const = term.implementation_defined(ImplDef),
        stream.put(Stream, '$', !State),
        put(Stream, ImplDef, !State)
    ).

    % This does the job of format_constant just for atom constants.
    %
:- pred format_constant_atom(Stream::in, string::in, State::di, State::uo)
    is det <= (stream.writer(Stream, string, State),
    stream.writer(Stream, char, State)).
:- pragma inline(pred(format_constant_atom/4)).

format_constant_atom(Stream, Atom, !IO) :-
    AGT = not_adjacent_to_graphic_token,
    term_io.format_quoted_atom_agt(Stream, Atom, AGT, !IO).

:- func integer_signedness_and_size_suffix(term.signedness,
    term.integer_size) = string.

integer_signedness_and_size_suffix(signed,   size_word) =   "".
integer_signedness_and_size_suffix(signed,   size_8_bit) =  "i8".
integer_signedness_and_size_suffix(signed,   size_16_bit) = "i16".
integer_signedness_and_size_suffix(signed,   size_32_bit) = "i32".
integer_signedness_and_size_suffix(signed,   size_64_bit) = "i64".
integer_signedness_and_size_suffix(unsigned, size_word) =   "u".
integer_signedness_and_size_suffix(unsigned, size_8_bit) =  "u8".
integer_signedness_and_size_suffix(unsigned, size_16_bit) = "u16".
integer_signedness_and_size_suffix(unsigned, size_32_bit) = "u32".
integer_signedness_and_size_suffix(unsigned, size_64_bit) = "u64".

%---------------------------------------------------------------------------%

write_variable(Var, VarSet, !IO) :-
    io.output_stream(OutStream, !IO),
    write_variable(OutStream, Var, VarSet, !IO).

write_variable(OutStream, Var, VarSet, !IO) :-
    OpTable = init_mercury_op_table,
    term_io.write_variable_with_op_table(OutStream, OpTable, Var, VarSet, !IO).

write_variable_with_op_table(OpTable, Var, VarSet, !IO) :-
    io.output_stream(OutStream, !IO),
    write_variable_with_op_table(OutStream, OpTable, Var, VarSet, !IO).

write_variable_with_op_table(OutStream, OpTable, Var, VarSet, !IO) :-
    write_variable_anon_vars(OutStream, OpTable, Var, VarSet, _,
        anon_var_to_int, _, !IO).

:- type anon_var_info
    --->    anon_var_to_int
            % The string we use to write out an anonymous variable
            % should be derived from its variable number, obtained
            % by calling var_to_int on it.
    ;       anon_occur_order(counter).
            % The string we use to write out an anonymous variable
            % should be based on whether it is the first, second, third etc
            % anonymous variable that our traversal of the whole term
            % has encountered. The counter tells us the number we should give
            % to the *next* one we encounter.

    % Write a variable.
    %
    % There are two ways we could choose to write unnamed variables.
    %
    % 1 Convert the variable to the integer that represents it and write
    %   `_N' where N is that integer. This has the advantage that
    %   such variables get printed in a canonical way, so rearranging terms
    %   containing such variables will not affect the way they are numbered
    %   (this includes breaking up a term and printing the pieces separately).
    %   We use this way if !.Anon is anon_var_to_int.
    %
    % 2 Number the unnamed variables from 0 and write `_N' where
    %   N is the next number in the sequence of such variables.
    %   This has the advantage that such variables can be visually scanned
    %   rather more easily (for example in error messages).
    %   We use this way if !.Anon is anon_occur_order.
    %
    % We provide full support for both approaches internally to term_io.m,
    % but expose only anon_var_to_int to users, for now.
    %
    % XXX The names we generate here, with either approach,
    % *could* clash with the name of an explicit-named variable.
    %
:- pred write_variable_anon_vars(io.text_output_stream::in, OpTable::in,
    var(T)::in, varset(T)::in, varset(T)::out,
    anon_var_info::in, anon_var_info::out, io::di, io::uo) is det
    <= op_table(OpTable).

write_variable_anon_vars(OutStream, OpTable, Var, !VarSet, !Anon, !IO) :-
    ( if varset.search_var(!.VarSet, Var, Value) then
        write_term_anon_vars(OutStream, OpTable, Value, !VarSet, !Anon, !IO)
    else if varset.search_name(!.VarSet, Var, Name) then
        io.write_string(OutStream, Name, !IO)
    else
        (
            !.Anon = anon_var_to_int,
            term.var_to_int(Var, VarNum)
        ;
            !.Anon = anon_occur_order(Counter0),
            counter.allocate(VarNum, Counter0, Counter),
            !:Anon = anon_occur_order(Counter)
        ),
        string.format("_%d", [i(VarNum)], VarName),

        % Recording the name we have given Var in !VarSet is needed only
        % with anon_occur_order; with anon_var_to_int, we would give the same
        % name to the same variable even without it. However, that would
        % require allocating memory for VarName on *every* use. The alternative
        % is recording the name in !VarSet here. Depending on the length
        % of VarName and the depth of the var name map in !.VarSet,
        % this recording could take as much time and memory as several
        % reconstructions of VarName. Since we don't know how many more
        % times we will need the name of Var, we cannot know for sure
        % which approach will be faster or use more memory. The difference
        % if that the memory usage of the "record the name" approach is
        % *bounded*, while the memory usage of the "rebuild the name each time"
        % approach is not. That is why we choose the former even with
        % anon_var_to_int.
        varset.name_var(Var, VarName, !VarSet),
        io.write_string(OutStream, VarName, !IO)
    ).

%---------------------------------------------------------------------------%

quoted_char(C) =
    quoted_char_to_string(C).

quoted_char_to_string(C) =
    string.format("'%s'", [s(term_io.escaped_char_to_string(C))]).

quote_char(C, !IO) :-
    write_quoted_char(C, !IO).

write_quoted_char(C, !IO) :-
    io.output_stream(OutStream, !IO),
    format_quoted_char(OutStream, C, !IO).

write_quoted_char(OutStream, C, !IO) :-
    format_quoted_char(OutStream, C, !IO).

quote_char(Stream, C, !State) :-
    format_quoted_char(Stream, C, !State).

format_quoted_char(Stream, C, !State) :-
    stream.put(Stream, term_io.quoted_char_to_string(C), !State).

%---------------------%

escaped_char(Char) =
    escaped_char_to_string(Char).

escaped_char_to_string(Char) = String :-
    string_is_escaped_char(Char, String).

write_escaped_char(Char, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.format_escaped_char(Stream, Char, !IO).

write_escaped_char(Stream, Char, !State) :-
    format_escaped_char(Stream, Char, !State).

format_escaped_char(Stream, Char, !State) :-
    % Note: the code of format_escaped_char is similar to code in
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

quoted_string(Str0) = Str :-
    State0 = string.builder.init,
    format_quoted_string(string.builder.handle, Str0, State0, State),
    Str = string.builder.to_string(State).

quote_string(Str, !IO) :-
    io.output_stream(OutStream, !IO),
    term_io.format_quoted_string(OutStream, Str, !IO).

write_quoted_string(Str, !IO) :-
    io.output_stream(OutStream, !IO),
    format_quoted_string(OutStream, Str, !IO).

write_quoted_string(OutStream, Str, !State) :-
    format_quoted_string(OutStream, Str, !State).

quote_string(Stream, Str, !State) :-
    format_quoted_string(Stream, Str, !State).

format_quoted_string(Stream, Str, !State) :-
    stream.put(Stream, '"', !State),
    term_io.format_escaped_string(Stream, Str, !State),
    stream.put(Stream, '"', !State).

%---------------------%

escaped_string(Str0) = Str :-
    State0 = string.builder.init,
    format_escaped_string(string.builder.handle, Str0, State0, State),
    Str = string.builder.to_string(State).

write_escaped_string(Str, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.format_escaped_string(Stream, Str, !IO).

write_escaped_string(Stream, Str, !State) :-
    format_escaped_string(Stream, Str, !State).

format_escaped_string(Stream, Str, !State) :-
    % XXX ILSEQ Decide what to do with ill-formed sequences.
    string.foldl(term_io.format_escaped_char(Stream), Str, !State).

%---------------------------------------------------------------------------%

quoted_atom(Str0) = Str :-
    State0 = string.builder.init,
    format_quoted_atom(string.builder.handle, Str0, State0, State),
    Str = string.builder.to_string(State).

quote_atom(Str, !IO) :-
    term_io.write_quoted_atom(Str, !IO).

write_quoted_atom(Str, !IO) :-
    AGT = not_adjacent_to_graphic_token,
    term_io.write_quoted_atom_agt(Str, AGT, !IO).

write_quoted_atom(OutStream, Str, !IO) :-
    AGT = not_adjacent_to_graphic_token,
    term_io.write_quoted_atom_agt(OutStream, Str, AGT, !IO).

quote_atom(Stream, Str, !State) :-
    format_quoted_atom(Stream, Str, !State).

format_quoted_atom(Stream, Str, !State) :-
    AGT = not_adjacent_to_graphic_token,
    term_io.format_quoted_atom_agt(Stream, Str, AGT, !State).

%---------------------%

quoted_atom_agt(Str0, AGT) = Str :-
    State0 = string.builder.init,
    format_quoted_atom_agt(string.builder.handle, Str0, AGT, State0, State),
    Str = string.builder.to_string(State).

quote_atom_agt(Str, AGT, !IO) :-
    io.output_stream(Stream, !IO),
    term_io.write_quoted_atom_agt(Stream, Str, AGT, !IO).

write_quoted_atom_agt(Str, AGT, !IO) :-
    io.output_stream(OutStream, !IO),
    write_quoted_atom_agt(OutStream, Str, AGT, !IO).

write_quoted_atom_agt(OutStream, Str, AGT, !IO) :-
    format_quoted_atom_agt(OutStream, Str, AGT, !IO).

quote_atom_agt(Stream, Str, AGT, !State) :-
    term_io.format_quoted_atom_agt(Stream, Str, AGT, !State).

format_quoted_atom_agt(Stream, Str, AGT, !State) :-
    ShouldQuote = should_atom_be_quoted(Str, AGT),
    (
        ShouldQuote = no,
        stream.put(Stream, Str, !State)
    ;
        ShouldQuote = yes,
        stream.put(Stream, '''', !State),
        term_io.format_escaped_string(Stream, Str, !State),
        stream.put(Stream, '''', !State)
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
            string.all_match(mercury_term_lexer.graphic_token_char, S),
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
% Codepoints in 0x7b..0x7e.
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
    % Note: the code here is similar (but not identical) to
    %
    % - escape_special_char in compiler/parse_tree_out_pragma.m, and
    % - quote_special_escape_char in library/rtti_implementation.m.
    %
    % Any changes here may require similar changes there.
    %
:- pred mercury_escape_special_char(char, char).
:- mode mercury_escape_special_char(in, out) is semidet.
:- mode mercury_escape_special_char(out, in) is semidet.

mercury_escape_special_char('\\', '\\').
mercury_escape_special_char('''', '''').
mercury_escape_special_char('"', '"').
mercury_escape_special_char('\a', 'a').
mercury_escape_special_char('\b', 'b').
mercury_escape_special_char('\f', 'f').
mercury_escape_special_char('\n', 'n').
mercury_escape_special_char('\r', 'r').
mercury_escape_special_char('\t', 't').
mercury_escape_special_char('\v', 'v').

%---------------------------------------------------------------------------%
:- end_module term_io.
%---------------------------------------------------------------------------%
