%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts terms in the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_term.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type needs_quotes
    --->    next_to_graphic_token
            % Needs quotes, if it is another graphic token.
    ;       not_next_to_graphic_token.
            % Doesn't need quotes.

%---------------------------------------------------------------------------%

    % Convert a Mercury variable into a Mercury variable name.
    % This is tricky because the compiler may introduce new variables
    % that either don't have names at all, or whose names end in
    % some sequence of primes (eg. Var''').
    %
:- pred mercury_convert_var_name(string::in, string::out) is det.

%---------------------------------------------------------------------------%

    % Output a single variable.
    % Variables that don't have names in the given varset
    % are given the name "V_<n>", where <n> is their variable number.
    % Variables whose name originally started with `V_' have their name changed
    % to start with `V_V_' to avoid name clashes.
    %
:- pred mercury_output_var(varset(T)::in, var_name_print::in, var(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_var_to_string(varset(T), var_name_print, var(T)) = string.
:- pred mercury_format_var(varset(T)::in, var_name_print::in, var(T)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

    % Output a comma-separated list of variables.
    %
:- pred mercury_output_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_vars_to_string(varset(T), var_name_print, list(var(T)))
    = string.
:- pred mercury_format_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, S::in, U::di, U::uo) is det <= output(S, U).

    % Output a variable or a list of variables with print_name_only.
    %
:- func mercury_var_to_name_only(varset(T), var(T)) = string.
:- func mercury_vars_to_name_only(varset(T), list(var(T))) = string.

%---------------------------------------------------------------------------%

:- func describe_error_term(varset(T), term(T)) = string.

%---------------------------------------------------------------------------%

    % Output a term.
    %
:- pred mercury_output_term(varset(T)::in, var_name_print::in, term(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_term_to_string(varset(T), var_name_print, term(T)) = string.
:- pred mercury_format_term(varset(T)::in, var_name_print::in, term(T)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

:- pred mercury_output_term_nq(varset(T)::in, var_name_print::in,
    needs_quotes::in, term(T)::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_term_nq_to_string(varset(T), var_name_print, needs_quotes,
    term(T)) = string.
:- pred mercury_format_term_nq(varset(T)::in, var_name_print::in,
    needs_quotes::in, term(T)::in, S::in, U::di, U::uo) is det <= output(S, U).

:- pred mercury_format_comma_separated_terms(varset(T)::in, var_name_print::in,
    term(T)::in, list(term(T))::in, S::in, U::di, U::uo) is det
    <= output(S, U).

:- pred mercury_output_limited_term(varset(T)::in, var_name_print::in, int::in,
    term(T)::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_limited_term_to_string(varset(T), var_name_print, int,
    term(T)) = string.

:- pred mercury_output_limited_term_nq(varset(T)::in, var_name_print::in,
    needs_quotes::in, int::in, term(T)::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_limited_term_nq_to_string(varset(T), var_name_print,
    needs_quotes, int, term(T)) = string.

%---------------------------------------------------------------------------%

:- func mercury_bracketed_atom_to_string(needs_quotes, string) = string.
:- pred mercury_format_bracketed_atom(needs_quotes::in, string::in, S::in,
    U::di, U::uo) is det <= output(S, U).

:- pred mercury_format_quoted_atom(needs_quotes::in, string::in, S::in,
    U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

    % Is this string a Mercury operator?
    %
:- pred mercury_op(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module lexer.
:- import_module ops.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

mercury_convert_var_name(Name, ConvertedName) :-
    % We have to be careful that every possible variable is mapped
    % to a distinct name. Variables without names are given names
    % starting with `V_' followed by a sequence of digits corresponding to
    % their variable id. To ensure that names generated this way don't clash
    % with any variables whose names originally started with `V_', we add
    % another `V_' prefix to those names.
    %
    % Compiler's internal name  Converted name
    % ------------------------  --------------
    % none                      V_[0-9]*
    % .*'+                      V_.*_[0-9]*
    % V_.*                      V_V_.*
    % anything else             same as original name
    %
    ( if string.remove_suffix(Name, "'", _) then
        strip_trailing_primes(Name, StrippedName, NumPrimes),
        ConvertedName = "V_" ++ StrippedName ++ "_" ++
            string.int_to_string(NumPrimes)
    else if string.prefix(Name, "V_") then
        ConvertedName = "V_" ++ Name
    else
        ConvertedName = Name
    ).

:- pred strip_trailing_primes(string::in, string::out, int::out) is det.

strip_trailing_primes(Name0, Name, Num) :-
    % XXX This implementation is O(N^2), but if N is not very small,
    % then something is very wrong.
    ( if string.remove_suffix(Name0, "'", Name1) then
        strip_trailing_primes(Name1, Name, Num0),
        Num = Num0 + 1
    else
        Num = 0,
        Name = Name0
    ).

%---------------------------------------------------------------------------%

mercury_output_var(VarSet, VarNamePrint, Var, Stream, !IO) :-
    mercury_format_var(VarSet, VarNamePrint, Var, Stream, !IO).

mercury_var_to_string(VarSet, VarNamePrint, Var) = String :-
    mercury_format_var(VarSet, VarNamePrint, Var, unit, "", String).

mercury_format_var(VarSet, VarNamePrint, Var, S, !U) :-
    ( if varset.search_name(VarSet, Var, Name) then
        (
            VarNamePrint = print_num_only,
            term.var_to_int(Var, VarNum),
            add_string("V_", S, !U),
            add_int(VarNum, S, !U)
        ;
            ( VarNamePrint = print_name_only
            ; VarNamePrint = print_name_and_num
            ),
            mercury_convert_var_name(Name, ConvertedName),
            add_string(ConvertedName, S, !U),
            (
                VarNamePrint = print_name_only
            ;
                VarNamePrint = print_name_and_num,
                term.var_to_int(Var, VarNum),
                add_string("_", S, !U),
                add_int(VarNum, S, !U)
            )
        )
    else
        term.var_to_int(Var, VarNum),
        add_string("V_", S, !U),
        add_int(VarNum, S, !U)
    ).

%---------------------%

mercury_output_vars(VarSet, VarNamePrint, Vars, Stream, !IO) :-
    mercury_format_vars(VarSet, VarNamePrint, Vars, Stream, !IO).

mercury_vars_to_string(VarSet, VarNamePrint, Vars) = String :-
    mercury_format_vars(VarSet, VarNamePrint, Vars, unit, "", String).

mercury_format_vars(VarSet, VarNamePrint, Vars, S, !U) :-
    add_list(mercury_format_var(VarSet, VarNamePrint), ", ", Vars, S, !U).

%---------------------%

mercury_var_to_name_only(VarSet, Var) =
    mercury_var_to_string(VarSet, print_name_only, Var).

mercury_vars_to_name_only(VarSet, Vars) =
    mercury_vars_to_string(VarSet, print_name_only, Vars).

%---------------------------------------------------------------------------%

describe_error_term(VarSet, Term) =
    % We should consider using the algorithms of term_io.write_term instead of
    % the ones now in mercury_limited_term_to_string to print terms; it adds
    % fewer redundant parentheses.
    mercury_limited_term_to_string(VarSet, print_name_only,
        max_term_string_size_in_syntax_error, Term).

    % The maximum size of the string representation of a term to print
    % at syntax errors.
    %
:- func max_term_string_size_in_syntax_error = int.

max_term_string_size_in_syntax_error = 80.

%---------------------------------------------------------------------------%

mercury_output_term(VarSet, VarNamePrint, Term, Stream, !IO) :-
    mercury_output_term_nq(VarSet, VarNamePrint, not_next_to_graphic_token,
        Term, Stream, !IO).

mercury_term_to_string(VarSet, VarNamePrint, Term) =
    mercury_term_nq_to_string(VarSet, VarNamePrint, not_next_to_graphic_token,
        Term).

mercury_format_term(VarSet, VarNamePrint, Term, S, !U) :-
    mercury_format_term_nq(VarSet, VarNamePrint, not_next_to_graphic_token,
        Term, S, !U).

%---------------------%

mercury_output_term_nq(VarSet, VarNamePrint, NextToGraphicToken, Term,
        Stream, !IO) :-
    mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken, Term,
        Stream, !IO).

mercury_term_nq_to_string(VarSet, VarNamePrint, NextToGraphicToken, Term)
        = String :-
    mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken, Term,
        unit, "", String).

mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken, Term, S, !U) :-
    (
        Term = term.variable(Var, _),
        mercury_format_var(VarSet, VarNamePrint, Var, S, !U)
    ;
        Term = term.functor(Functor, Args, _),
        ( if
            Functor = term.atom(""),
            Args = [F, X | Xs]
        then
            mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken,
                F, S, !U),
            add_string("(", S, !U),
            mercury_format_comma_separated_terms(VarSet, VarNamePrint,
                X, Xs, S, !U),
            add_string(")", S, !U)
        else if
            Functor = term.atom("[|]"),
            Args = [X, Xs]
        then
            add_string("[", S, !U),
            mercury_format_term(VarSet, VarNamePrint, X, S, !U),
            mercury_format_list_args(VarSet, VarNamePrint, Xs, S, !U),
            add_string("]", S, !U)
        else if
            Functor = term.atom("{}"),
            Args = [X]
        then
            % A unary tuple is usually a DCG escape,
            % so add some extra space.
            add_string("{ ", S, !U),
            mercury_format_term(VarSet, VarNamePrint, X, S, !U),
            add_string(" }", S, !U)
        else if
            Functor = term.atom("{}"),
            Args = [X | Xs]
        then
            add_string("{", S, !U),
            mercury_format_comma_separated_terms(VarSet, VarNamePrint,
                X, Xs, S, !U),
            add_string("}", S, !U)
        else if
            Args = [BinaryPrefixArg1, BinaryPrefixArg2],
            Functor = term.atom(FunctorName),
            mercury_binary_prefix_op(FunctorName)
        then
            add_string("(", S, !U),
            add_string(FunctorName, S, !U),
            add_string(" ", S, !U),
            mercury_format_term(VarSet, VarNamePrint, BinaryPrefixArg1, S, !U),
            add_string(" ", S, !U),
            mercury_format_term(VarSet, VarNamePrint, BinaryPrefixArg2, S, !U),
            add_string(")", S, !U)
        else if
            Args = [PrefixArg],
            Functor = term.atom(FunctorName),
            mercury_unary_prefix_op(FunctorName)
        then
            add_string("(", S, !U),
            add_string(FunctorName, S, !U),
            add_string(" ", S, !U),
            mercury_format_term(VarSet, VarNamePrint, PrefixArg, S, !U),
            add_string(")", S, !U)
        else if
            Args = [PostfixArg],
            Functor = term.atom(FunctorName),
            mercury_unary_postfix_op(FunctorName)
        then
            add_string("(", S, !U),
            mercury_format_term(VarSet, VarNamePrint, PostfixArg, S, !U),
            add_string(" ", S, !U),
            add_string(FunctorName, S, !U),
            add_string(")", S, !U)
        else if
            Args = [Arg1, Arg2],
            Functor = term.atom(FunctorName),
            mercury_infix_op(FunctorName)
        then
            ( if FunctorName = "." then
                mercury_format_term_nq(VarSet, VarNamePrint,
                    next_to_graphic_token, Arg1, S, !U),
                add_string(".", S, !U),
                mercury_format_term_nq(VarSet, VarNamePrint,
                    next_to_graphic_token, Arg2, S, !U)
            else
                add_string("(", S, !U),
                mercury_format_term_nq(VarSet, VarNamePrint,
                    not_next_to_graphic_token, Arg1, S, !U),
                add_string(" ", S, !U),
                add_string(FunctorName, S, !U),
                add_string(" ", S, !U),
                mercury_format_term_nq(VarSet, VarNamePrint,
                    not_next_to_graphic_token, Arg2, S, !U),
                add_string(")", S, !U)
            )
        else
            (
                Args = [Y | Ys],
                mercury_format_constant(NextToGraphicToken, Functor, S, !U),
                add_string("(", S, !U),
                mercury_format_comma_separated_terms(VarSet, VarNamePrint,
                    Y, Ys, S, !U),
                add_string(")", S, !U)
            ;
                Args = [],
                mercury_format_bracketed_constant_ngt(NextToGraphicToken,
                    Functor, S, !U)
            )
        )
    ).

:- pred mercury_format_list_args(varset(T)::in, var_name_print::in,
    term(T)::in, S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_list_args(VarSet, VarNamePrint, Term, S, !U) :-
    ( if
        Term = term.functor(term.atom("[|]"), Args, _),
        Args = [X, Xs]
    then
        add_string(", ", S, !U),
        mercury_format_term(VarSet, VarNamePrint, X, S, !U),
        mercury_format_list_args(VarSet, VarNamePrint, Xs, S, !U)
    else if
        Term = term.functor(term.atom("[]"), [], _)
    then
        true
    else
        add_string(" | ", S, !U),
        mercury_format_term(VarSet, VarNamePrint, Term, S, !U)
    ).

mercury_format_comma_separated_terms(VarSet, VarNamePrint,
        HeadTerm, TailTerms, S, !U) :-
    mercury_format_term(VarSet, VarNamePrint, HeadTerm, S, !U),
    mercury_format_remaining_terms(VarSet, VarNamePrint, TailTerms, S, !U).

:- pred mercury_format_remaining_terms(varset(T)::in, var_name_print::in,
    list(term(T))::in, S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_remaining_terms(_VarSet, _VarNamePrint, [], _S, !U).
mercury_format_remaining_terms(VarSet, VarNamePrint, [Term | Terms], S, !U) :-
    add_string(", ", S, !U),
    mercury_format_term(VarSet, VarNamePrint, Term, S, !U),
    mercury_format_remaining_terms(VarSet, VarNamePrint, Terms, S, !U).

:- pred mercury_format_bracketed_constant_ngt(needs_quotes::in, const::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_bracketed_constant_ngt(NextToGraphicToken, Const, S, !U) :-
    ( if
        Const = term.atom(Op),
        mercury_op(Op)
    then
        add_string("(", S, !U),
        add_quoted_atom(Op, S, !U),
        add_string(")", S, !U)
    else
        mercury_format_constant(NextToGraphicToken, Const, S, !U)
    ).

:- pred mercury_format_constant(needs_quotes::in, const::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_constant(NextToGraphicToken, Const, S, !U) :-
    ( if Const = term.atom(Atom) then
        mercury_format_quoted_atom(NextToGraphicToken, Atom, S, !U)
    else
        add_constant(Const, S, !U)
    ).

%---------------------%

mercury_output_limited_term(VarSet, VarNamePrint, Limit, Term, Stream, !IO) :-
    mercury_output_limited_term_nq(VarSet, VarNamePrint,
        not_next_to_graphic_token, Limit, Term, Stream, !IO).

mercury_limited_term_to_string(VarSet, VarNamePrint, Limit, Term) =
    mercury_limited_term_nq_to_string(VarSet, VarNamePrint,
        not_next_to_graphic_token, Limit, Term).

%---------------------%

mercury_output_limited_term_nq(VarSet, VarNamePrint, NextToGraphicToken,
        Limit, Term, Stream, !IO) :-
    Str = mercury_limited_term_nq_to_string(VarSet, VarNamePrint,
        NextToGraphicToken, Limit, Term),
    io.write_string(Stream, Str, !IO).

mercury_limited_term_nq_to_string(VarSet, VarNamePrint, NextToGraphicToken,
        Limit, Term) = String :-
    mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken, Term,
        unit, "", FullString),
    FullLen = string.count_codepoints(FullString),
    ( if FullLen =< Limit then
        String = FullString
    else
        (
            Term = term.variable(_, _),
            % We cannot reduce the length of the string.
            String = FullString
        ;
            Term = term.functor(Functor, Args, Context),
            NoArgTerm = term.functor(Functor, [], Context),
            mercury_format_term_nq(VarSet, VarNamePrint, NextToGraphicToken,
                NoArgTerm, unit, "", FunctorString),
            (
                Functor = term.atom(_),
                ArityStr = int_to_string(list.length(Args)),
                String = FunctorString ++ "/" ++ ArityStr
            ;
                ( Functor = term.integer(_, _, _, _)
                ; Functor = term.float(_)
                ; Functor = term.string(_)
                ; Functor = term.implementation_defined(_)
                ),
                String = FunctorString
            )
        )
    ).

%---------------------%

mercury_bracketed_atom_to_string(NextToGraphicToken, Name) = String :-
    mercury_format_bracketed_atom(NextToGraphicToken, Name, unit, "", String).

mercury_format_bracketed_atom(NextToGraphicToken, Name, S, !U) :-
    ( if mercury_op(Name) then
        add_string("(", S, !U),
        add_quoted_atom(Name, S, !U),
        add_string(")", S, !U)
    else
        mercury_format_quoted_atom(NextToGraphicToken, Name, S, !U)
    ).

mercury_format_quoted_atom(NextToGraphicToken, Name, S, !U) :-
    % If the symname is composed of only graphic token chars, then
    % term_io.quote_atom will not quote it; but if it is next another
    % graphic token, it needs to be quoted, otherwise the two would be
    % considered part of one symbol name (e.g. In "int:<", the ":<" parses
    % as one token, so when writing out the "<" after the ":" we need
    % to quote it.
    ( if
        NextToGraphicToken = next_to_graphic_token,
        string.to_char_list(Name, Chars),
        ( list.member(Char, Chars) => lexer.graphic_token_char(Char) )
    then
        add_string("'", S, !U),
        add_escaped_string(Name, S, !U),
        add_string("'", S, !U)
    else
        add_quoted_atom(Name, S, !U)
    ).

%---------------------------------------------------------------------------%
%
% Predicates to test whether a functor is a Mercury operator.
%

mercury_op(Op) :-
    ops.lookup_op(ops.init_mercury_op_table, Op).

:- pred mercury_binary_prefix_op(string::in) is semidet.

mercury_binary_prefix_op(Op) :-
    ops.lookup_binary_prefix_op(ops.init_mercury_op_table, Op, _, _, _).

:- pred mercury_infix_op(string::in) is semidet.

mercury_infix_op(Op) :-
    ops.lookup_infix_op(ops.init_mercury_op_table, Op, _, _, _).

:- pred mercury_unary_prefix_op(string::in) is semidet.

mercury_unary_prefix_op(Op) :-
    ops.lookup_prefix_op(ops.init_mercury_op_table, Op, _, _).

:- pred mercury_unary_postfix_op(string::in) is semidet.

mercury_unary_postfix_op(Op) :-
    ops.lookup_postfix_op(ops.init_mercury_op_table, Op, _, _).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_term.
%---------------------------------------------------------------------------%
