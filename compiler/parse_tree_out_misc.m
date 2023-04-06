%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_tree_out_misc.m.
% Main author: fjh.
%
% This module converts some of the simplest parts of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_misc.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred mercury_output_det(determinism::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- func mercury_det_to_string(determinism) = string.
:- pred mercury_format_det(determinism::in, S::in, U::di, U::uo) is det
    <= output(S, U).

%---------------------------------------------------------------------------%

    % Output an existential quantifier.
    %
:- pred mercury_output_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_quantifier_to_string(tvarset, var_name_print, existq_tvars)
    = string.
:- pred mercury_format_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%

    % Similar to mercury_output_vars/3, but prefixes each variable
    % with `!' to indicate that it is a state variable.
    %
:- pred mercury_output_state_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, io.text_output_stream::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_foreign_language_string(foreign_language::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- func mercury_foreign_language_to_string(foreign_language) = string.
:- pred mercury_format_foreign_language_string(foreign_language::in,
    S::in, U::di, U::uo) is det <= output(S, U).

%---------------------------------------------------------------------------%
%
% Write out indentation.
%

:- func indent_increment = int.

    % Write out the given indent level (indent_increment spaces per indent).
    % error_util.m
    %
:- pred write_indent(io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

    % Return the indent for the given level as a string.
    %
:- func indent_string(int) = string.

%---------------------------------------------------------------------------%

:- pred mercury_output_newline(int::in, io.text_output_stream::in,
    io::di, io::uo) is det.

:- pred mercury_format_tabs(int::in, S::in, U::di, U::uo) is det
    <= output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_term.

:- import_module int.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

mercury_output_det(Detism, Stream, !UI) :-
    mercury_format_det(Detism, Stream, !UI).

mercury_det_to_string(detism_det) = "det".
mercury_det_to_string(detism_semi) = "semidet".
mercury_det_to_string(detism_non) = "nondet".
mercury_det_to_string(detism_multi) = "multi".
mercury_det_to_string(detism_cc_multi) = "cc_multi".
mercury_det_to_string(detism_cc_non) = "cc_nondet".
mercury_det_to_string(detism_failure) = "failure".
mercury_det_to_string(detism_erroneous) = "erroneous".

mercury_format_det(Detism, S, !U) :-
    add_string(mercury_det_to_string(Detism), S, !U).

%---------------------------------------------------------------------------%

mercury_output_quantifier(TypeVarSet, VarNamePrint, ExistQVars, Stream, !IO) :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        Stream, !IO).

mercury_quantifier_to_string(TypeVarSet, VarNamePrint, ExistQVars) = String :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        unit, "", String).

mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, S, !U) :-
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        add_string("some [", S, !U),
        mercury_format_vars_vs(TypeVarSet, VarNamePrint, ExistQVars, S, !U),
        add_string("] ", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_state_vars(VarSet, VarNamePrint, StateVars, Stream, !IO) :-
    write_out_list(mercury_output_state_var(VarSet, VarNamePrint),
        ", ", StateVars, Stream, !IO).

:- pred mercury_output_state_var(varset(T)::in, var_name_print::in, var(T)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

mercury_output_state_var(VarSet, VarNamePrint, Var, Stream, !IO) :-
    io.write_string(Stream, "!", !IO),
    mercury_output_var_vs(VarSet, VarNamePrint, Var, Stream, !IO).

%---------------------------------------------------------------------------%

mercury_output_foreign_language_string(Lang, Stream, !IO) :-
    mercury_format_foreign_language_string(Lang, Stream, !IO).

mercury_foreign_language_to_string(Lang) = String :-
    mercury_format_foreign_language_string(Lang, unit, "", String).

mercury_format_foreign_language_string(Lang, S, !U) :-
    add_string("""" ++ foreign_language_string(Lang) ++ """", S, !U).

%---------------------------------------------------------------------------%

indent_increment = 2.

write_indent(Stream, Indent, !IO) :-
    Str = indent_string(Indent),
    io.write_string(Stream, Str, !IO).

indent_string(Indent) = Str :-
    % The code here is modelled after output_std_indent_levels in
    % library/pretty_printer.m, except we can, and do, assume that
    % Indent is never negative, and in our use case, deep indentation
    % is much rarer.
    ( if indent_str_09(Indent, Str0) then
        Str = Str0
    else
        indent_str_10(TenIndentStr),
        Str = TenIndentStr ++ indent_string(Indent - 10)
    ).

:- pred indent_str_09(int::in, string::out) is semidet.
:- pred indent_str_10(string::out) is det.

indent_str_09(0,  "").
indent_str_09(1,  "  ").
indent_str_09(2,  "    ").
indent_str_09(3,  "      ").
indent_str_09(4,  "        ").
indent_str_09(5,  "          ").
indent_str_09(6,  "            ").
indent_str_09(7,  "              ").
indent_str_09(8,  "                ").
indent_str_09(9,  "                  ").
indent_str_10(    "                    ").

%---------------------------------------------------------------------------%

mercury_output_newline(Indent, Stream, !IO) :-
    io.write_char(Stream, '\n', !IO),
    mercury_format_tabs(Indent, Stream, !IO).

mercury_format_tabs(Indent, S, !U) :-
    ( if Indent > 0 then
        add_string("\t", S, !U),
        mercury_format_tabs(Indent - 1, S, !U)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_misc.
%---------------------------------------------------------------------------%
