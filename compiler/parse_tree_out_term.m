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
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

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
:- func mercury_var_to_string(var_table, var_name_print, prog_var) = string.
:- pred mercury_output_var(var_table::in, var_name_print::in, prog_var::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_var(var_table::in, var_name_print::in, prog_var::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_var_to_string_vs(varset(T), var_name_print, var(T)) = string.
:- pred mercury_output_var_vs(varset(T)::in, var_name_print::in, var(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_var_vs(varset(T)::in, var_name_print::in, var(T)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_var_to_string_src(var_name_source, var_name_print, prog_var)
    = string.
:- pred mercury_output_var_src(var_name_source::in, var_name_print::in,
    prog_var::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_var_src(var_name_source::in, var_name_print::in,
    prog_var::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_var_raw_to_string(var_name_print, prog_var, string) = string.
:- pred mercury_format_var_raw(var_name_print::in, var(T)::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_var_num_only(var(T)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

    % Output a comma-separated list of variables.
    %
:- func mercury_vars_to_string(var_table, var_name_print, list(prog_var))
    = string.
:- pred mercury_output_vars(var_table::in, var_name_print::in,
    list(prog_var)::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_vars(var_table::in, var_name_print::in,
    list(prog_var)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_vars_to_string_vs(varset(T), var_name_print, list(var(T)))
    = string.
:- pred mercury_output_vars_vs(varset(T)::in, var_name_print::in,
    list(var(T))::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_vars_vs(varset(T)::in, var_name_print::in,
    list(var(T))::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_vars_to_string_src(var_name_source, var_name_print,
    list(prog_var)) = string.
:- pred mercury_output_vars_src(var_name_source::in, var_name_print::in,
    list(prog_var)::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_vars_src(var_name_source::in, var_name_print::in,
    list(prog_var)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

    % Output a variable or a list of variables with print_name_only.
    %
:- func mercury_var_to_name_only(var_table, prog_var) = string.
:- func mercury_vars_to_name_only(var_table, list(prog_var)) = string.
:- func mercury_var_to_name_only_vs(varset(T), var(T)) = string.
:- func mercury_vars_to_name_only_vs(varset(T), list(var(T))) = string.
:- func mercury_var_to_name_only_src(var_name_source, prog_var) = string.
:- func mercury_vars_to_name_only_src(var_name_source, list(prog_var))
    = string.

%---------------------------------------------------------------------------%

:- func describe_error_term(varset(T), term(T)) = string.

%---------------------------------------------------------------------------%

    % Output a term.
    %
:- func mercury_term_to_string_vs(varset(T), var_name_print, term(T)) = string.
:- pred mercury_output_term_vs(varset(T)::in, var_name_print::in, term(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_term_vs(varset(T)::in, var_name_print::in, term(T)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_term_to_string(var_table, var_name_print, prog_term)
    = string.
:- pred mercury_output_term(var_table::in, var_name_print::in,
    prog_term::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_term(var_table::in, var_name_print::in,
    prog_term::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_term_src(var_name_source::in, var_name_print::in,
    prog_term::in, io.text_output_stream::in, io::di, io::uo) is det.

:- func mercury_term_nq_to_string_vs(varset(T), var_name_print, needs_quotes,
    term(T)) = string.
:- pred mercury_output_term_nq_vs(varset(T)::in, var_name_print::in,
    needs_quotes::in, term(T)::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred mercury_format_term_nq_vs(varset(T)::in, var_name_print::in,
    needs_quotes::in, term(T)::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- func mercury_term_nq_to_string(var_table, var_name_print,
    needs_quotes, prog_term) = string.
:- pred mercury_output_term_nq(var_table::in, var_name_print::in,
    needs_quotes::in, prog_term::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred mercury_format_term_nq(var_table::in, var_name_print::in,
    needs_quotes::in, prog_term::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- func mercury_term_nq_to_string_src(var_name_source, var_name_print,
    needs_quotes, prog_term) = string.
:- pred mercury_output_term_nq_src(var_name_source::in, var_name_print::in,
    needs_quotes::in, prog_term::in, io.text_output_stream::in,
    io::di, io::uo) is det.

:- pred mercury_format_comma_separated_terms_vs(varset(T)::in,
    var_name_print::in, term(T)::in, list(term(T))::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_comma_separated_terms(var_table::in,
    var_name_print::in, prog_term::in, list(prog_term)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_limited_term_to_string_vs(varset(T), var_name_print, int,
    term(T)) = string.
:- pred mercury_output_limited_term_vs(varset(T)::in, var_name_print::in,
    int::in, term(T)::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_limited_term_vs(varset(T)::in, var_name_print::in,
    int::in, term(T)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_limited_term_nq_to_string_vs(varset(T), var_name_print,
    needs_quotes, int, term(T)) = string.
:- pred mercury_output_limited_term_nq_vs(varset(T)::in, var_name_print::in,
    needs_quotes::in, int::in, term(T)::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred mercury_format_limited_term_nq_vs(varset(T)::in, var_name_print::in,
    needs_quotes::in, int::in, term(T)::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func mercury_bracketed_atom_to_string(needs_quotes, string) = string.
:- pred mercury_format_bracketed_atom(needs_quotes::in, string::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_quoted_atom(needs_quotes::in, string::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- type graphic_char_in_string
    --->    no_graphic_chars
    ;       some_graphic_chars
    ;       all_graphic_chars.

:- func string_graphic_chars(string) = graphic_char_in_string.

%---------------------------------------------------------------------------%

    % Is this string a Mercury operator?
    %
:- pred mercury_op(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module mercury_term_lexer.
:- import_module ops.
:- import_module string.
:- import_module string.builder.

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

mercury_var_to_string(VarTable, VarNamePrint, Var) = Str :-
    State0 = string.builder.init,
    mercury_format_var(VarTable, VarNamePrint, Var,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_var(VarTable, VarNamePrint, Var, Stream, !IO) :-
    mercury_format_var(VarTable, VarNamePrint, Var, Stream, !IO).

mercury_format_var(VarTable, VarNamePrint, Var, S, !U) :-
    ( if var_table.search_var_name(VarTable, Var, Name) then
        mercury_format_var_raw(VarNamePrint, Var, Name, S, !U)
    else
        mercury_format_var_num_only(Var, S, !U)
    ).

%---------------------%

mercury_var_to_string_vs(VarSet, VarNamePrint, Var) = Str :-
    State0 = string.builder.init,
    mercury_format_var_vs(VarSet, VarNamePrint, Var,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_var_vs(VarSet, VarNamePrint, Var, Stream, !IO) :-
    mercury_format_var_vs(VarSet, VarNamePrint, Var, Stream, !IO).

mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U) :-
    ( if varset.search_name(VarSet, Var, Name) then
        mercury_format_var_raw(VarNamePrint, Var, Name, S, !U)
    else
        mercury_format_var_num_only(Var, S, !U)
    ).

%---------------------%

mercury_var_to_string_src(VarNameSrc, VarNamePrint, Var) = Str :-
    State0 = string.builder.init,
    mercury_format_var_src(VarNameSrc, VarNamePrint, Var,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO) :-
    mercury_format_var_src(VarNameSrc, VarNamePrint, Var, Stream, !IO).

mercury_format_var_src(VarNameSrc, VarNamePrint, Var, S, !U) :-
    ( if var_db.search_var_name_in_source(VarNameSrc, Var, Name) then
        mercury_format_var_raw(VarNamePrint, Var, Name, S, !U)
    else
        mercury_format_var_num_only(Var, S, !U)
    ).

%---------------------%

mercury_var_raw_to_string(VarNamePrint, Var, Name) = Str :-
    State0 = string.builder.init,
    mercury_format_var_raw(VarNamePrint, Var, Name,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_format_var_raw(VarNamePrint, Var, Name, S, !U) :-
    ( if Name = "" then
        % There is nothing else to print.
        mercury_format_var_num_only(Var, S, !U)
    else
        (
            VarNamePrint = print_num_only,
            mercury_format_var_num_only(Var, S, !U)
        ;
            VarNamePrint = print_name_only,
            mercury_convert_var_name(Name, ConvertedName),
            add_string(ConvertedName, S, !U)
        ;
            VarNamePrint = print_name_and_num,
            mercury_convert_var_name(Name, ConvertedName),
            add_string(ConvertedName, S, !U),
            term.var_to_int(Var, VarNum),
            add_string("_", S, !U),
            add_int(VarNum, S, !U)
        )
    ).

mercury_format_var_num_only(Var, S, !U) :-
    term.var_to_int(Var, VarNum),
    add_string("V_", S, !U),
    add_int(VarNum, S, !U).

%---------------------------------------------------------------------------%

mercury_vars_to_string(VarTable, VarNamePrint, Vars) = Str :-
    State0 = string.builder.init,
    mercury_format_vars(VarTable, VarNamePrint, Vars,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_vars(VarTable, VarNamePrint, Vars, Stream, !IO) :-
    mercury_format_vars(VarTable, VarNamePrint, Vars, Stream, !IO).

mercury_format_vars(VarTable, VarNamePrint, Vars, S, !U) :-
    add_list(mercury_format_var(VarTable, VarNamePrint), ", ", Vars, S, !U).

%---------------------%

mercury_vars_to_string_vs(VarSet, VarNamePrint, Vars) = Str :-
    State0 = string.builder.init,
    mercury_format_vars_vs(VarSet, VarNamePrint, Vars,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_vars_vs(VarTable, VarNamePrint, Vars, Stream, !IO) :-
    mercury_format_vars_vs(VarTable, VarNamePrint, Vars, Stream, !IO).

mercury_format_vars_vs(VarSet, VarNamePrint, Vars, S, !U) :-
    add_list(mercury_format_var_vs(VarSet, VarNamePrint), ", ", Vars, S, !U).

%---------------------%

mercury_vars_to_string_src(VarNameSrc, VarNamePrint, Vars) = Str :-
    State0 = string.builder.init,
    mercury_format_vars_src(VarNameSrc, VarNamePrint, Vars,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_vars_src(VarNameSrc, VarNamePrint, Vars, Stream, !IO) :-
    mercury_format_vars_src(VarNameSrc, VarNamePrint, Vars, Stream, !IO).

mercury_format_vars_src(VarNameSrc, VarNamePrint, Vars, S, !U) :-
    add_list(mercury_format_var_src(VarNameSrc, VarNamePrint), ", ", Vars,
        S, !U).

%---------------------------------------------------------------------------%

mercury_var_to_name_only(VarSet, Var) =
    mercury_var_to_string(VarSet, print_name_only, Var).

mercury_vars_to_name_only(VarSet, Vars) =
    mercury_vars_to_string(VarSet, print_name_only, Vars).

%---------------------%

mercury_var_to_name_only_vs(VarSet, Var) =
    mercury_var_to_string_vs(VarSet, print_name_only, Var).

mercury_vars_to_name_only_vs(VarSet, Vars) =
    mercury_vars_to_string_vs(VarSet, print_name_only, Vars).

%---------------------%

mercury_var_to_name_only_src(VarNameSrc, Var) =
    mercury_var_to_string_src(VarNameSrc, print_name_only, Var).

mercury_vars_to_name_only_src(VarNameSrc, Vars) =
    mercury_vars_to_string_src(VarNameSrc, print_name_only, Vars).

%---------------------------------------------------------------------------%

describe_error_term(VarSet, Term) =
    % We should consider using the algorithms of term_io.write_term instead of
    % the ones now in mercury_limited_term_to_string to print terms; it adds
    % fewer redundant parentheses.
    mercury_limited_term_to_string_vs(VarSet, print_name_only,
        max_term_string_size_in_syntax_error, Term).

    % The maximum size of the string representation of a term to print
    % at syntax errors.
    %
:- func max_term_string_size_in_syntax_error = int.

max_term_string_size_in_syntax_error = 80.

%---------------------------------------------------------------------------%

mercury_term_to_string_vs(VarSet, VarNamePrint, Term) =
    mercury_term_nq_to_string_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Term).

mercury_output_term_vs(VarSet, VarNamePrint, Term, Stream, !IO) :-
    mercury_output_term_nq_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Term, Stream, !IO).

mercury_format_term_vs(VarSet, VarNamePrint, Term, S, !U) :-
    mercury_format_term_nq_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Term, S, !U).

%---------------------%

mercury_term_to_string(VarTable, VarNamePrint, Term) =
    mercury_term_nq_to_string(VarTable, VarNamePrint,
        not_next_to_graphic_token, Term).

mercury_output_term(VarTable, VarNamePrint, Term, Stream, !IO) :-
    mercury_output_term_nq(VarTable, VarNamePrint,
        not_next_to_graphic_token, Term, Stream, !IO).

mercury_format_term(VarTable, VarNamePrint, Term, S, !U) :-
    mercury_format_term_nq(VarTable, VarNamePrint,
        not_next_to_graphic_token, Term, S, !U).

%---------------------%

mercury_output_term_src(VarNameSrc, VarNamePrint, Term, Stream, !IO) :-
    (
        VarNameSrc = vns_varset(VarSet),
        mercury_output_term_nq_vs(VarSet, VarNamePrint,
            not_next_to_graphic_token, Term, Stream, !IO)
    ;
        VarNameSrc = vns_var_table(VarTable),
        mercury_output_term_nq(VarTable, VarNamePrint,
            not_next_to_graphic_token, Term, Stream, !IO)
    ).

%---------------------%

mercury_term_nq_to_string_vs(VarSet, VarNamePrint, NextToGraphicToken, Term)
        = Str :-
    State0 = string.builder.init,
    mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken, Term,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken, Term,
        Stream, !IO) :-
    mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken, Term,
        Stream, !IO).

mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken, Term,
        S, !U) :-
    % Please keep in sync with mercury_format_term_nq.
    (
        Term = term.variable(Var, _),
        mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U)
    ;
        Term = term.functor(Functor, Args, _),
        ( if
            Functor = term.atom(""),
            Args = [F, X | Xs]
        then
            mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken,
                F, S, !U),
            add_string("(", S, !U),
            mercury_format_comma_separated_terms_vs(VarSet, VarNamePrint,
                X, Xs, S, !U),
            add_string(")", S, !U)
        else if
            Functor = term.atom("[|]"),
            Args = [X, Xs]
        then
            add_string("[", S, !U),
            mercury_format_term_vs(VarSet, VarNamePrint, X, S, !U),
            mercury_format_list_args_vs(VarSet, VarNamePrint, Xs, S, !U),
            add_string("]", S, !U)
        else if
            Functor = term.atom("{}"),
            Args = [X | Xs]
        then
            (
                Xs = [],
                % A unary tuple is usually a DCG escape,
                % so add some extra space.
                add_string("{ ", S, !U),
                mercury_format_term_vs(VarSet, VarNamePrint, X, S, !U),
                add_string(" }", S, !U)
            ;   
                Xs = [_ | _],
                add_string("{", S, !U),
                mercury_format_comma_separated_terms_vs(VarSet, VarNamePrint,
                    X, Xs, S, !U),
                add_string("}", S, !U)
            )
        else if 
            Functor = term.atom(FunctorName),
            mercury_op_table_search_op_infos(FunctorName, OpInfos)
        then
          (
                ( Args = []
                ; Args = [_, _, _ | _]
                ),
                mercury_format_plain_functor_args_nq_vs(VarSet,
                    VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
            ;
                Args = [ArgA],
                ( if OpInfos ^ oi_prefix = pre(_, _) then
                    add_string("(", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term_vs(VarSet, VarNamePrint, ArgA, S, !U),
                    add_string(")", S, !U)
                else if OpInfos ^ oi_postfix = post(_, _) then
                    add_string("(", S, !U),
                    mercury_format_term_vs(VarSet, VarNamePrint, ArgA, S, !U),
                    add_string(" ", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(")", S, !U)
                else
                    mercury_format_plain_functor_args_nq_vs(VarSet,
                        VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
                )
            ;
                Args = [ArgA, ArgB],
                ( if OpInfos ^ oi_binary_prefix = bin_pre(_, _, _) then
                    add_string("(", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term_vs(VarSet, VarNamePrint, ArgA, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term_vs(VarSet, VarNamePrint, ArgB, S, !U),
                    add_string(")", S, !U)
                else if OpInfos ^ oi_infix = in(_, _, _) then
                    ( if FunctorName = "." then
                        mercury_format_term_nq_vs(VarSet, VarNamePrint,
                            next_to_graphic_token, ArgA, S, !U),
                        add_string(".", S, !U),
                        mercury_format_term_nq_vs(VarSet, VarNamePrint,
                            next_to_graphic_token, ArgB, S, !U)
                    else
                        add_string("(", S, !U),
                        mercury_format_term_nq_vs(VarSet, VarNamePrint,
                            not_next_to_graphic_token, ArgA, S, !U),
                        add_string(" ", S, !U),
                        add_string(FunctorName, S, !U),
                        add_string(" ", S, !U),
                        mercury_format_term_nq_vs(VarSet, VarNamePrint,
                            not_next_to_graphic_token, ArgB, S, !U),
                        add_string(")", S, !U)
                    )
                else
                    mercury_format_plain_functor_args_nq_vs(VarSet,
                        VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
                )
            )
        else
            mercury_format_plain_functor_args_nq_vs(VarSet, VarNamePrint,
                NextToGraphicToken, Functor, Args, S, !U)
        )
    ).

:- pred mercury_format_plain_functor_args_nq_vs(varset(T)::in,
    var_name_print::in, needs_quotes::in, const::in, list(term(T))::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_plain_functor_args_nq_vs(VarSet, VarNamePrint,
        NextToGraphicToken, Functor, Args, S, !U) :-
    (
        Args = [],
        mercury_format_bracketed_constant_ngt(NextToGraphicToken,
            Functor, S, !U)
    ;
        Args = [HeadArg | TailArgs],
        mercury_format_constant(NextToGraphicToken, Functor, S, !U),
        add_string("(", S, !U),
        mercury_format_comma_separated_terms_vs(VarSet, VarNamePrint,
            HeadArg, TailArgs, S, !U),
        add_string(")", S, !U)
    ).

%---------------------%

mercury_term_nq_to_string(VarTable, VarNamePrint, NextToGraphicToken,
        Term) = Str :-
    State0 = string.builder.init,
    mercury_format_term_nq(VarTable, VarNamePrint, NextToGraphicToken, Term,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_term_nq(VarTable, VarNamePrint, NextToGraphicToken,
        Term, Stream, !IO) :-
    mercury_format_term_nq(VarTable, VarNamePrint, NextToGraphicToken,
        Term, Stream, !IO).

mercury_format_term_nq(VarTable, VarNamePrint, NextToGraphicToken, Term,
        S, !U) :-
    % Please keep in sync with mercury_format_term_nq_vs.
    (
        Term = term.variable(Var, _),
        mercury_format_var(VarTable, VarNamePrint, Var, S, !U)
    ;
        Term = term.functor(Functor, Args, _),
        ( if
            Functor = term.atom(""),
            Args = [F, X | Xs]
        then
            mercury_format_term_nq(VarTable, VarNamePrint, NextToGraphicToken,
                F, S, !U),
            add_string("(", S, !U),
            mercury_format_comma_separated_terms(VarTable, VarNamePrint,
                X, Xs, S, !U),
            add_string(")", S, !U)
        else if
            Functor = term.atom("[|]"),
            Args = [X, Xs]
        then
            add_string("[", S, !U),
            mercury_format_term(VarTable, VarNamePrint, X, S, !U),
            mercury_format_list_args(VarTable, VarNamePrint, Xs, S, !U),
            add_string("]", S, !U)
        else if
            Functor = term.atom("{}"),
            Args = [X | Xs]
        then
            (
                Xs = [],
                % A unary tuple is usually a DCG escape,
                % so add some extra space.
                add_string("{ ", S, !U),
                mercury_format_term(VarTable, VarNamePrint, X, S, !U),
                add_string(" }", S, !U)
            ;
                Xs = [_ | _],
                add_string("{", S, !U),
                mercury_format_comma_separated_terms(VarTable, VarNamePrint,
                    X, Xs, S, !U),
                add_string("}", S, !U)
            )
        else if
            Functor = term.atom(FunctorName),
            mercury_op_table_search_op_infos(FunctorName, OpInfos)
	    then
            (
                ( Args = []
                ; Args = [_, _, _ | _]
                ),
                mercury_format_plain_functor_args_nq(VarTable,
                    VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
            ;
                Args = [ArgA],
                ( if OpInfos ^ oi_prefix = pre(_, _) then
                    add_string("(", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term(VarTable, VarNamePrint, ArgA, S, !U),
                    add_string(")", S, !U)
                else if OpInfos ^ oi_postfix = post(_, _) then
                    add_string("(", S, !U),
                    mercury_format_term(VarTable, VarNamePrint, ArgA, S, !U),
                    add_string(" ", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(")", S, !U)
                else
                    mercury_format_plain_functor_args_nq(VarTable,
                        VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
                )
            ;
                Args = [ArgA, ArgB],
                ( if OpInfos ^ oi_binary_prefix = bin_pre(_, _, _) then
                    add_string("(", S, !U),
                    add_string(FunctorName, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term(VarTable, VarNamePrint, ArgA, S, !U),
                    add_string(" ", S, !U),
                    mercury_format_term(VarTable, VarNamePrint, ArgB, S, !U),
                    add_string(")", S, !U)
                else if OpInfos ^ oi_infix = in(_, _, _) then
                    ( if FunctorName = "." then
                        mercury_format_term_nq(VarTable, VarNamePrint,
                            next_to_graphic_token, ArgA, S, !U),
                        add_string(".", S, !U),
                        mercury_format_term_nq(VarTable, VarNamePrint,
                            next_to_graphic_token, ArgB, S, !U)
                    else
                        add_string("(", S, !U),
                        mercury_format_term_nq(VarTable, VarNamePrint,
                            not_next_to_graphic_token, ArgA, S, !U),
                        add_string(" ", S, !U),
                        add_string(FunctorName, S, !U),
                        add_string(" ", S, !U),
                        mercury_format_term_nq(VarTable, VarNamePrint,
                            not_next_to_graphic_token, ArgB, S, !U),
                        add_string(")", S, !U)
                    )
                else
                    mercury_format_plain_functor_args_nq(VarTable,
                        VarNamePrint, NextToGraphicToken, Functor, Args, S, !U)
                )
            )
        else
            mercury_format_plain_functor_args_nq(VarTable, VarNamePrint,
                NextToGraphicToken, Functor, Args, S, !U)
        )
    ).

:- pred mercury_format_plain_functor_args_nq(var_table::in,
    var_name_print::in, needs_quotes::in, const::in, list(prog_term)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_plain_functor_args_nq(VarTable, VarNamePrint,
        NextToGraphicToken, Functor, Args, S, !U) :-
    (
        Args = [],
        mercury_format_bracketed_constant_ngt(NextToGraphicToken,
            Functor, S, !U)
    ;
        Args = [HeadArg | TailArgs],
        mercury_format_constant(NextToGraphicToken, Functor, S, !U),
        add_string("(", S, !U),
        mercury_format_comma_separated_terms(VarTable, VarNamePrint,
            HeadArg, TailArgs, S, !U),
        add_string(")", S, !U)
    ).

%---------------------%

:- pred mercury_format_list_args_vs(varset(T)::in, var_name_print::in,
    term(T)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_list_args_vs(VarSet, VarNamePrint, Term, S, !U) :-
    % Please keep in sync with mercury_format_list_args_src.
    ( if
        Term = term.functor(term.atom("[|]"), Args, _),
        Args = [X, Xs]
    then
        add_string(", ", S, !U),
        mercury_format_term_vs(VarSet, VarNamePrint, X, S, !U),
        mercury_format_list_args_vs(VarSet, VarNamePrint, Xs, S, !U)
    else if
        Term = term.functor(term.atom("[]"), [], _)
    then
        true
    else
        add_string(" | ", S, !U),
        mercury_format_term_vs(VarSet, VarNamePrint, Term, S, !U)
    ).

:- pred mercury_format_list_args(var_table::in, var_name_print::in,
    prog_term::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_list_args(VarTable, VarNamePrint, Term, S, !U) :-
    % Please keep in sync with mercury_format_list_args.
    ( if
        Term = term.functor(term.atom("[|]"), Args, _),
        Args = [X, Xs]
    then
        add_string(", ", S, !U),
        mercury_format_term(VarTable, VarNamePrint, X, S, !U),
        mercury_format_list_args(VarTable, VarNamePrint, Xs, S, !U)
    else if
        Term = term.functor(term.atom("[]"), [], _)
    then
        true
    else
        add_string(" | ", S, !U),
        mercury_format_term(VarTable, VarNamePrint, Term, S, !U)
    ).

%---------------------%

mercury_term_nq_to_string_src(VarNameSrc, VarNamePrint, NextToGraphicToken,
        Term) = Str :-
    (
        VarNameSrc = vns_varset(VarSet),
        Str = mercury_term_nq_to_string_vs(VarSet, VarNamePrint,
            NextToGraphicToken, Term)
    ;
        VarNameSrc = vns_var_table(VarTable),
        Str = mercury_term_nq_to_string(VarTable, VarNamePrint,
            NextToGraphicToken, Term)
    ).

mercury_output_term_nq_src(VarNameSrc, VarNamePrint, NextToGraphicToken,
        Term, Stream, !IO) :-
    (
        VarNameSrc = vns_varset(VarSet),
        mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken,
            Term, Stream, !IO)
    ;
        VarNameSrc = vns_var_table(VarTable),
        mercury_format_term_nq(VarTable, VarNamePrint, NextToGraphicToken,
            Term, Stream, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_format_comma_separated_terms_vs(VarSet, VarNamePrint,
        HeadTerm, TailTerms, S, !U) :-
    mercury_format_term_vs(VarSet, VarNamePrint, HeadTerm, S, !U),
    mercury_format_remaining_terms_vs(VarSet, VarNamePrint, TailTerms, S, !U).

mercury_format_comma_separated_terms(VarTable, VarNamePrint,
        HeadTerm, TailTerms, S, !U) :-
    mercury_format_term(VarTable, VarNamePrint, HeadTerm, S, !U),
    mercury_format_remaining_terms(VarTable, VarNamePrint, TailTerms, S, !U).

:- pred mercury_format_remaining_terms_vs(varset(T)::in, var_name_print::in,
    list(term(T))::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_remaining_terms_vs(_VarSet, _VarNamePrint, [], _S, !U).
mercury_format_remaining_terms_vs(VarSet, VarNamePrint, [Term | Terms],
        S, !U) :-
    add_string(", ", S, !U),
    mercury_format_term_vs(VarSet, VarNamePrint, Term, S, !U),
    mercury_format_remaining_terms_vs(VarSet, VarNamePrint, Terms, S, !U).

:- pred mercury_format_remaining_terms(var_table::in, var_name_print::in,
    list(prog_term)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_remaining_terms(_VarTable, _VarNamePrint, [], _S, !U).
mercury_format_remaining_terms(VarTable, VarNamePrint, [Term | Terms],
        S, !U) :-
    add_string(", ", S, !U),
    mercury_format_term(VarTable, VarNamePrint, Term, S, !U),
    mercury_format_remaining_terms(VarTable, VarNamePrint, Terms, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_bracketed_constant_ngt(needs_quotes::in, const::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

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
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_constant(NextToGraphicToken, Const, S, !U) :-
    ( if Const = term.atom(Atom) then
        mercury_format_quoted_atom(NextToGraphicToken, Atom, S, !U)
    else
        add_constant(Const, S, !U)
    ).

%---------------------%

mercury_limited_term_to_string_vs(VarSet, VarNamePrint, Limit, Term) =
    mercury_limited_term_nq_to_string_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Limit, Term).

mercury_output_limited_term_vs(VarSet, VarNamePrint, Limit, Term,
        Stream, !IO) :-
    mercury_output_limited_term_nq_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Limit, Term, Stream, !IO).

mercury_format_limited_term_vs(VarSet, VarNamePrint, Limit, Term, S, !U) :-
    mercury_format_limited_term_nq_vs(VarSet, VarNamePrint,
        not_next_to_graphic_token, Limit, Term, S, !U).

%---------------------%

mercury_limited_term_nq_to_string_vs(VarSet, VarNamePrint, NextToGraphicToken,
        Limit, Term) = Str :-
    % Note that we *could* implement mercury_limited_term_nq_to_string_vs
    % in terms of mercury_format_limited_term_nq_vs, but the approach here
    % is simpler, because it makes explicit the "try one way, and if that
    % gives too long a string, try another way" approach.
    FullState0 = string.builder.init,
    mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken, Term,
        string.builder.handle, FullState0, FullState),
    ( if total_length_is_at_most(FullState, Limit) then
        Str = string.builder.to_string(FullState)
    else
        (
            Term = term.variable(_, _),
            % We cannot reduce the length of the string.
            Str = string.builder.to_string(FullState)
        ;
            Term = term.functor(Functor, Args, Context),
            NoArgTerm = term.functor(Functor, [], Context),
            FunctorState0 = string.builder.init,
            mercury_format_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken,
                NoArgTerm, string.builder.handle, FunctorState0, FunctorState),
            FunctorStr = string.builder.to_string(FunctorState),
            (
                Functor = term.atom(_),
                ArityStr = int_to_string(list.length(Args)),
                Str = FunctorStr ++ "/" ++ ArityStr
            ;
                ( Functor = term.integer(_, _, _, _)
                ; Functor = term.float(_)
                ; Functor = term.string(_)
                ; Functor = term.implementation_defined(_)
                ),
                Str = FunctorStr
            )
        )
    ).

mercury_output_limited_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken,
        Limit, Term, Stream, !IO) :-
    Str = mercury_limited_term_nq_to_string_vs(VarSet, VarNamePrint,
        NextToGraphicToken, Limit, Term),
    io.write_string(Stream, Str, !IO).

mercury_format_limited_term_nq_vs(VarSet, VarNamePrint, NextToGraphicToken,
        Limit, Term, S, !U) :-
    Str = mercury_limited_term_nq_to_string_vs(VarSet, VarNamePrint,
        NextToGraphicToken, Limit, Term),
    add_string(Str, S, !U).

%---------------------%

mercury_bracketed_atom_to_string(NextToGraphicToken, Name) = Str :-
    State0 = string.builder.init,
    mercury_format_bracketed_atom(NextToGraphicToken, Name,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_format_bracketed_atom(NextToGraphicToken, Name, S, !U) :-
    ( if mercury_op(Name) then
        add_string("(", S, !U),
        add_quoted_atom(Name, S, !U),
        add_string(")", S, !U)
    else
        mercury_format_quoted_atom(NextToGraphicToken, Name, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_quoted_atom(NextToGraphicToken, Name, S, !U) :-
    % If the symname is composed of only graphic token chars, then
    % term_io.quote_atom will not quote it; but if it is next to another
    % graphic token, it needs to be quoted, otherwise the two would be
    % considered part of one symbol name (e.g. In "int:<", the ":<" parses
    % as one token, so when writing out the "<" after the ":" we need
    % to quote it.
    ( if
        NextToGraphicToken = next_to_graphic_token,
        string.all_match(mercury_term_lexer.graphic_token_char, Name)
    then
        add_string("'", S, !U),
        add_escaped_string(Name, S, !U),
        add_string("'", S, !U)
    else
        add_quoted_atom(Name, S, !U)
    ).

%---------------------------------------------------------------------------%

:- type graphic_char
    --->    not_seen_graphic_char
    ;       seen_graphic_char.

:- type non_graphic_char
    --->    not_seen_non_graphic_char
    ;       seen_non_graphic_char.

string_graphic_chars(Str) = Result :-
    string.foldl2(string_graphic_chars_acc, Str,
        not_seen_graphic_char, Graphic, not_seen_non_graphic_char, NonGraphic),
    (
        Graphic = not_seen_graphic_char,
        Result = no_graphic_chars
    ;
        Graphic = seen_graphic_char,
        (
            NonGraphic = not_seen_non_graphic_char,
            Result = all_graphic_chars
        ;
            NonGraphic = seen_non_graphic_char,
            Result = some_graphic_chars
        )
    ).

:- pred string_graphic_chars_acc(char::in,
    graphic_char::in, graphic_char::out,
    non_graphic_char::in, non_graphic_char::out) is det.

string_graphic_chars_acc(Char, !Graphic, !NonGraphic) :-
    ( if mercury_term_lexer.graphic_token_char(Char) then
        !:Graphic = seen_graphic_char
    else
        !:NonGraphic = seen_non_graphic_char
    ).

%---------------------------------------------------------------------------%
%
% Predicates to test whether a functor is a Mercury operator.
%

mercury_op(Op) :-
    ops.mercury_op_table_is_op(Op).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_term.
%---------------------------------------------------------------------------%
