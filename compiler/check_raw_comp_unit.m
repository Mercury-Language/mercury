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
% Check whether the interface of a module exports anything.
% XXX The name of this module is now misleading, since it does not operate
% on raw_compilation_units.
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

    % Given a module's source code, check whether its interface
    % exports anything. If it does not, and --warn-nothing-exported is set,
    % report a warning.
    %
:- pred check_module_interface_for_no_exports(globals::in,
    parse_tree_module_src::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module map.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

check_module_interface_for_no_exports(Globals, ParseTreeModuleSrc, !Specs) :-
    globals.lookup_bool_option(Globals, warn_nothing_exported, ExportWarning),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        ParseTreeModuleSrc = parse_tree_module_src(ModuleName,
            ModuleNameContext, IntIncls, _ImpIncls, _InclMap,
            _IntImports, _IntUses, _ImpImports, _ImpUses, _ImportUseMap,
            _IntFIMs, _ImpFIMs, _ImplicitFIMLangs,
            IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsFor,
            IntInstDefns, IntModeDefns,
            IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
            IntDeclPragmas, IntPromises,
            _IntBadClauses,
            _ImpTypeDefnsAbs, _ImpTypeDefnsMer, _ImpTypeDefnsFor,
            _ImpInstDefns, _ImpModeDefns,
            _ImpTypeClasses, _ImpInstances, _ImpPredDecls, _ImpModeDecls,
            _ImpClauses, _ImpForeignEnums, _ImpForeignExportEnums,
            _ImpDeclPragmas, _ImpImplPragmas, _ImpPromises,
            _ImpInitialises, _ImpFinalises, _ImpMutables),
        % XXX ITEM_LIST Should we return "yes" for an item_block
        % that contains only ONE include_module declaration?
        ( if
            map.is_empty(IntIncls),
            IntTypeDefnsAbs = [],
            IntTypeDefnsMer = [],
            IntTypeDefnsFor = [],
            IntInstDefns = [],
            IntModeDefns = [],
            IntTypeClasses = [],
            IntInstances = [],
            IntPredDecls = [],
            IntModeDecls = [],
            IntDeclPragmas = [],
            IntPromises = []
        then
            globals.lookup_bool_option(Globals, warn_nothing_exported, Warn),
            (
                Warn = no
                % It is a legitimate thing for libraries to export nothing.
            ;
                Warn =  yes,
                generate_no_exports_warning(ModuleName, ModuleNameContext,
                    !Specs)
            )
        else
            true
        )
    ).

:- pred generate_no_exports_warning(module_name::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_no_exports_warning(ModuleName, Context, !Specs) :-
    Msg = simple_msg(Context,
        [always([invis_order_default_start(2),
            words("Warning: the interface of module"),
            qual_sym_name(ModuleName),
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
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree,
        [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.check_raw_comp_unit.
%---------------------------------------------------------------------------%
