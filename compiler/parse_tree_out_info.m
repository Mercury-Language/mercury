%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module provides the parameters we use for printing out
% the parse tree of a Mercury module, in whole or in part.
% These parameters are represented by the merc_out_info type. Values of
% this type control those low-level aspects of how parse tree components
% are printed that may differ depending on *why* we want to print them,
% such as whether the goal is to generate valid Mercury code or to print
% as much detail as possible for debugging, even if those details are
% not expressible in Mercury syntax.

:- module parse_tree.parse_tree_out_info.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

:- type merc_out_info.

:- type maybe_qualified_item_names
    --->    unqualified_item_names
    ;       qualified_item_names.

:- type maybe_output_line_numbers
    --->    do_not_output_line_numbers
    ;       do_output_line_numbers.

:- type type_repn_for
    --->    type_repn_for_machines
    ;       type_repn_for_humans.

    % Are we generating output that has be able to be read back in as
    % valid Mercury, e.g. when the output goes to a .int* or .*opt file,
    % or are we generating output only for humans to read?
    %
    % XXX We should split output for humans into two: one for developers,
    % who won't mind, and will often need, variable numbers, and one
    % for ordinary users, who don't, and shouldn't have to, know about
    % the existence of variable numbers.
    %
    % XXX Since not all combinations of output_lang and var_name_print
    % make sense, we shouldn't pass values of the output_lang and
    % var_name_print types next to each other, as we now do in many places.
    % Instead, each alternative here should *contain* the var_name_print
    % value that we now pass next to it, but *only* if there is more than one
    % var_name_print value that makes sense for the value of output_lang.
    % This would require putting the two types next to each other.
    %
:- type output_lang
    --->    output_mercury
    ;       output_debug.

:- func init_debug_merc_out_info = merc_out_info.
:- func init_write_int_merc_out_info = merc_out_info.
:- func init_merc_out_info(globals, maybe_qualified_item_names, output_lang)
    = merc_out_info.
:- func merc_out_info_disable_line_numbers(merc_out_info) = merc_out_info.

:- func get_maybe_qualified_item_names(merc_out_info)
    = maybe_qualified_item_names.
:- func get_output_line_numbers(merc_out_info) = maybe_output_line_numbers.
:- func get_output_lang(merc_out_info) = output_lang.
:- func get_type_repn_for(merc_out_info) = type_repn_for.
:- func get_human_comma_sep(merc_out_info) = string.

:- pred maybe_format_line_number(merc_out_info::in, prog_context::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred maybe_unqualify_sym_name(merc_out_info::in,
    sym_name::in, sym_name::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.parse_tree_out_misc.

:- import_module bool.

%---------------------------------------------------------------------------%

:- type merc_out_info
    --->    merc_out_info(
                moi_qualify_item_names      :: maybe_qualified_item_names,
                moi_output_line_numbers     :: maybe_output_line_numbers,
                moi_output_lang             :: output_lang,

                % When writing out a comma in a type_repn, or some other
                % output that humans may want to look at, what should
                % we print to separate it from what follows?
                %
                % For humans, ",\n    "; for computers, just ", ".
                moi_type_repn_for           :: type_repn_for,
                moi_human_comma_sep         :: string
            ).

init_debug_merc_out_info = Info :-
    Info = merc_out_info(qualified_item_names, do_not_output_line_numbers,
        output_debug, type_repn_for_machines, ", ").

init_write_int_merc_out_info = Info :-
    Info = merc_out_info(qualified_item_names, do_not_output_line_numbers,
        output_mercury, type_repn_for_machines, ", ").

init_merc_out_info(Globals, MaybeQualifiedItemNames, Lang) = Info :-
    globals.lookup_bool_option(Globals, line_numbers, LineNumbersOpt),
    globals.lookup_bool_option(Globals, type_repns_for_humans,
        TypeRepnsForHumans),
    ( LineNumbersOpt = no, LineNumbers = do_not_output_line_numbers
    ; LineNumbersOpt = yes, LineNumbers = do_output_line_numbers
    ),
    (
        TypeRepnsForHumans = no,
        For = type_repn_for_machines,
        CommaSep = ", "
    ;
        TypeRepnsForHumans = yes,
        For = type_repn_for_humans,
        CommaSep = ",\n    "
    ),
    Info = merc_out_info(MaybeQualifiedItemNames, LineNumbers, Lang,
        For, CommaSep).

merc_out_info_disable_line_numbers(Info0) = Info :-
    Info = Info0 ^ moi_output_line_numbers := do_not_output_line_numbers.

get_maybe_qualified_item_names(Info) = Info ^ moi_qualify_item_names.
get_output_line_numbers(Info) = Info ^ moi_output_line_numbers.
get_output_lang(Info) = Info ^ moi_output_lang.
get_type_repn_for(Info) = Info ^ moi_type_repn_for.
get_human_comma_sep(Info) = Info ^ moi_human_comma_sep.

%---------------------------------------------------------------------------%

maybe_format_line_number(Info, Context, S, !U) :-
    LineNumbers = get_output_line_numbers(Info),
    (
        LineNumbers = do_output_line_numbers,
        add_string("\t% ", S, !U),
        parse_tree_out_misc.format_context(S, Context, !U),
        add_string("\n", S, !U)
    ;
        LineNumbers = do_not_output_line_numbers
    ).

maybe_unqualify_sym_name(Info, SymName, OutSymName) :-
    MaybeQualifiedItemNames = get_maybe_qualified_item_names(Info),
    (
        MaybeQualifiedItemNames = qualified_item_names,
        OutSymName = SymName
    ;
        MaybeQualifiedItemNames = unqualified_item_names,
        OutSymName = unqualified(unqualify_name(SymName))
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_info.
%---------------------------------------------------------------------------%
