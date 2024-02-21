%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2011 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_module_interface.m.
%
% Check whether the interface of a module exports anything.
%
%---------------------------------------------------------------------------%

:- module parse_tree.check_module_interface.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.error_spec.
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
:- import_module parse_tree.item_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module map.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

check_module_interface_for_no_exports(Globals, ParseTreeModuleSrc, !Specs) :-
    globals.lookup_bool_option(Globals, warn_nothing_exported, ExportWarning),
    (
        ExportWarning = no
    ;
        ExportWarning = yes,
        ParseTreeModuleSrc = parse_tree_module_src(ModuleName,
            ModuleNameContext, InclMap, _ImportUseMap,
            _IntFIMs, _ImpFIMs, _IntSelfFIMLangs, _ImpSelfFIMLangs,

            TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
            TypeSpecs, InstModeSpecs,

            IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
            IntDeclPragmas, IntDeclMarkers, IntPromises, _IntBadClauses,

            _ImpTypeClasses, _ImpInstances, _ImpPredDecls, _ImpModeDecls,
            _ImpClauses, _ImpForeignProcs, _ImpForeignExportEnums,
            _ImpDeclPragmas, _ImpDeclMarkers, _ImpImplPragmas, _ImpImplMarkers,
            _ImpPromises, _ImpInitialises, _ImpFinalises, _ImpMutables),
        CountIncls =
            ( pred(_MN::in, InclInfo::in, IntCnt0::in, IntCnt::out,
                    ImpCnt0::in, ImpCnt::out) is det :-
                InclInfo = include_module_info(Section, _),
                (
                    Section = ms_interface,
                    IntCnt = IntCnt0 + 1,
                    ImpCnt = ImpCnt0
                ;
                    Section = ms_implementation,
                    IntCnt = IntCnt0,
                    ImpCnt = ImpCnt0 + 1
                )
            ),
        map.foldl2(CountIncls, InclMap, 0, NumIntIncls, 0, NumImpIncls),
        ( if
            (
                NumIntIncls = 0
            ;
                NumIntIncls = 1,
                % If a module interface contains nothing but a single
                % include_module declaration, then
                % - we should report "nothing exported" if there are no
                %   include_module declarations in the implementation either,
                % - but we should NOT report "nothing exported" if there are
                %   some include_module declarations in the implementation.
                %
                % Such packages are a useful way to establish a group of
                % modules that have access to each other's interfaces,
                % without those interfaces being accessible from the
                % rest of the program (with the obvious exception of the
                % module whose include_module declaration is exported.)
                NumImpIncls = 0
            ),
            type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
                IntTypeDefns, _, _),
            IntTypeDefns = [],
            inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
                IntInstDefns, _),
            IntInstDefns = [],
            mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
                IntModeDefns, _),
            IntModeDefns = [],

            % If some type, inst or mode definitions were invalid, then
            % there are two possibilities: either some of them are in
            % the interface section, or none of them are. Unfortunately,
            % we don't know which is the case, so must choose an algorithm
            % that works in both cases.
            %
            % - If some of the errors are in the interface section, then
            %   generating a "no exports" warning would be misleading.
            %
            % - If all of the errors are in the implementation section, then
            %   generating that warning would not be misleading, but
            %   it is also not quite needed. Due to those errors,
            %   the compilation will fail, with error messages that the
            %   programmer can and should fix, whether we generate
            %   a "no exports" warning or not. This means that the warning
            %   is not really needed *now*; it can be generated later,
            %   once the complained-about invalid definitions are fixed.
            TypeSpecs = [],
            InstModeSpecs = [],

            IntTypeClasses = [],
            IntInstances = [],
            IntPredDecls = [],
            % XXX We should delete the next three kinds of entities
            % from this test.
            %
            % Mode declarations, decl pragmas and decl markers all say
            % something about a predicate or function, and without
            % a declaration of that predicate or function ALSO in the
            % interface, they are not useful.
            IntModeDecls = [],          % we should delete this
            IntDeclPragmas = [],        % we should delete this
            IntDeclMarkers = [],        % we should delete this
            IntPromises = []
        then
            generate_no_exports_warning(ModuleName, ModuleNameContext,
                NumIntIncls, !Specs)
        else
            true
        )
    ).

:- inst num_int_incls for int/0
    --->    0
    ;       1.

:- pred generate_no_exports_warning(module_name::in, prog_context::in,
    int::in(num_int_incls),
    list(error_spec)::in, list(error_spec)::out) is det.

generate_no_exports_warning(ModuleName, Context, NumIntIncls, !Specs) :-
    MainMsg = simple_msg(Context,
        [always([invis_order_default_start(2, ""),
            words("Warning: the interface of module"),
            qual_sym_name(ModuleName),
            words("does not export anything."), nl]),
        % We don't list mode declarations because they don't make sense
        % without a corresponding pred or func declaration.
        % We don't list decl pragmas for the same reason.
        % We don't list promises because although they don't *have to be*
        % about predicates and functions defined in the same module,
        % they *should be*.
        verbose_only(verbose_always,
            [words("To be useful, a module should export something."),
            words("A file should contain at least one declaration"),
            words("other than"), decl("import_module"),
            words("in its interface section(s)."),
            words("This would normally be a"), decl("pred"), words("or"),
            decl("func"), words("declaration, or a"),
            decl("type"), suffix(","), decl("inst"), suffix(","),
            decl("mode"), suffix(","), decl("typeclass"), words("or"),
            decl("instance"), words("definition."), nl]
        )]),
    (
        NumIntIncls = 0,
        Msgs = [MainMsg]
    ;
        NumIntIncls = 1,
        InclMsg = simple_msg(Context,
            [verbose_only(verbose_always,
                [words("A module that includes a single submodule"),
                words("is not useful, because it can be replaced"),
                words("by that submodule."), nl])
            ]),
        Msgs = [MainMsg, InclMsg]
    ),
    Spec = error_spec($pred, severity_warning, phase_term_to_parse_tree, Msgs),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.check_module_interface.
%---------------------------------------------------------------------------%
