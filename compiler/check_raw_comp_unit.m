%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2011 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_raw_comp_unit.m.
%
% Check whether the interface of a raw compilation unit exports anything.
%
%---------------------------------------------------------------------------%

:- module parse_tree.check_raw_comp_unit.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Given a raw compilation unit, check whether the module exports anything.
    % If it doesn't, and the option --warn-nothing-exported is set,
    % report a warning.
    %
:- pred check_for_no_exports(globals::in, raw_compilation_unit::in,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Given a raw compilation unit, which will be a module's interface,
    % check whether that interface exports anything. If it doesn't, and
    % --warn-nothing-exported is set, report a warning.
    %
    % Basically, it does the same job as check_for_no_exports, except
    % our caller has already done the task of computing the module's
    % interface, and given it to us.
    %
:- pred check_int_for_no_exports(raw_compilation_unit::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

check_for_no_exports(Globals, RawCompUnit, !Specs) :-
    globals.lookup_bool_option(Globals, warn_nothing_exported, ExportWarning),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        get_interface(dont_include_impl_types, RawCompUnit, IntRawCompUnit),
        check_int_for_no_exports(IntRawCompUnit, !Specs)
    ).

check_int_for_no_exports(IntRawCompUnit, !Specs) :-
    IntRawCompUnit = raw_compilation_unit(ModuleName, Context, RawItemBlocks),
    do_ms_interface_item_blocks_export_anything(RawItemBlocks, ExportAnything),
    (
        ExportAnything = yes
    ;
        ExportAnything = no,
        generate_no_exports_warning(ModuleName, Context, WarnSpec),
        !:Specs = [WarnSpec | !.Specs]
    ).

:- pred do_ms_interface_item_blocks_export_anything(list(raw_item_block)::in,
    bool::out) is det.

do_ms_interface_item_blocks_export_anything([], no).
do_ms_interface_item_blocks_export_anything([RawItemBlock | RawItemBlocks],
        ExportAnything) :-
    RawItemBlock = item_block(Section, _, Incls, _Avails, Items),
    ( if
        Section = ms_interface,
        % XXX ITEM_LIST Should we return "yes" for an item_block
        % that contains only ONE include_module declaration?
        ( Incls = [_ | _]
        ; do_ms_interface_items_export_anything(Items, yes)
        )
    then
        ExportAnything = yes
    else
        do_ms_interface_item_blocks_export_anything(RawItemBlocks,
            ExportAnything)
    ).

:- pred do_ms_interface_items_export_anything(list(item)::in,
    bool::out) is det.

do_ms_interface_items_export_anything([], no).
do_ms_interface_items_export_anything([Item | Items], ExportAnything) :-
    ( if Item = item_nothing(_) then
        % Item is not useful when exported; keep searching.
        do_ms_interface_items_export_anything(Items, ExportAnything)
    else
        % We found something useful being exported.
        ExportAnything = yes
    ).

:- pred generate_no_exports_warning(module_name::in,
    prog_context::in, error_spec::out) is det.

generate_no_exports_warning(ModuleName, Context, Spec) :-
    Severity = severity_conditional(warn_nothing_exported, yes,
        severity_warning, no),
    Component = option_is_set(warn_nothing_exported, yes,
        [always([invis_order_default_start(2),
            words("Warning: interface for module"), qual_sym_name(ModuleName),
            words("does not export anything."), nl]),
        verbose_only(verbose_always,
            [words("To be useful, a module should export something."),
            words("A file should contain at least one declaration"),
            words("other than"), decl("import_module"),
            words("in its interface section(s)."),
            words("This would normally be a"),
            decl("pred"), suffix(","), decl("func"), suffix(","),
            decl("type"), suffix(","), decl("inst"), words("or"),
            decl("mode"), words("declaration."), nl])
        ]),
    Msg = simple_msg(Context, [Component]),
    Spec = error_spec(Severity, phase_term_to_parse_tree, [Msg]).

%---------------------------------------------------------------------------%
:- end_module parse_tree.check_raw_comp_unit.
%---------------------------------------------------------------------------%
