%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: get_dependencies.m.
%
% This module finds out what other things the contents of a given module
% depend on. These "things" can be
%
% - other Mercury modules that this module imports or uses,
% - files containing fact tables, or
% - foreign language source or header files.
%
% XXX ITEM_LIST Most of the work done in this module is now done
% more directly and simply in grab_modules.m. When we switch over to using
% the new code in modules.m exclusively, most or even all of this module
% shouldn't be needed anymore.
%
% XXX If some parts of this module survive this transition period,
% we should either factor out (if possible) or at least document
% the commonalities between the code here and in grab_modules.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.get_dependencies.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- pred get_explicit_and_implicit_avail_needs_in_parse_tree_plain_opt(
    parse_tree_plain_opt::in,
    set(module_name)::out, implicit_avail_needs::out) is det.

    % get_implicit_avail_needs_in_aug_compilation_unit(Globals, AugCompUnit,
    %   ImplicitlyUsedModules):
    %
    % Get the list of builtin modules (e.g. profiling_builtin.m,
    % table_builtin.m etc)_ that the given items may implicitly depend on.
    % and which should be automatically implicitly made available
    % as if via `:- use_module'.
    %
    % The module builtin.m should *always* be implicitly made available
    % as if via `:- import_module'.
    %
:- pred get_implicit_avail_needs_in_aug_compilation_unit(globals::in,
    aug_compilation_unit::in, set(module_name)::out) is det.

%---------------------------------------------------------------------------%

:- type maybe_need_tabling
    --->    dont_need_tabling
    ;       do_need_tabling.

:- type maybe_need_tabling_statistics
    --->    dont_need_tabling_statistics
    ;       do_need_tabling_statistics.

:- type maybe_need_stm
    --->    dont_need_stm
    ;       do_need_stm.

:- type maybe_need_exception
    --->    dont_need_exception
    ;       do_need_exception.

:- type maybe_need_string_format
    --->    dont_need_string_format
    ;       do_need_string_format.

:- type maybe_need_stream_format
    --->    dont_need_stream_format
    ;       do_need_stream_format.

:- type maybe_need_io
    --->    dont_need_io
    ;       do_need_io.

    % A representation of which builtin modules a set of items
    % implicitly needs imported or used.
    %
    % XXX We currently discover the need to import the modules needed
    % to compile away format strings by traversing all parts of all clauses,
    % and checking every predicate name and functor name to see whether
    % it could refer to any of the predicates recognized by the is_format_call
    % predicate. This is inefficient. It is also a bit unpredictable, since
    % it will lead us to implicitly import those modules even if a call
    % to unqualified("format") eventually turns out to call some other
    % predicate of that name.
    %
    % We should therefore consider ALWAYS implicitly importing the predicates
    % needed by format_call.m.
:- type implicit_avail_needs
    --->    implicit_avail_needs(
                ian_tabling             :: maybe_need_tabling,
                ian_tabling_statistics  :: maybe_need_tabling_statistics,
                ian_stm                 :: maybe_need_stm,
                ian_exception           :: maybe_need_exception,
                ian_string_format       :: maybe_need_string_format,
                ian_stream_format       :: maybe_need_stream_format,
                ian_io                  :: maybe_need_io
            ).

:- func init_implicit_avail_needs = implicit_avail_needs.

:- pred combine_implicit_needs(list(implicit_avail_needs)::in,
    implicit_avail_needs::out) is det.

%---------------------%

:- pred acc_implicit_avail_needs_in_instance_method(instance_method::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.
:- pred acc_implicit_avail_needs_in_mutable(item_mutable_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.
:- pred acc_implicit_avail_needs_in_clause(item_clause_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.
:- pred acc_implicit_avail_needs_in_goal(goal::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

%---------------------%

    % compute_implicit_avail_needs(Globals, ImplicitAvailNeeds,
    %    ImplicitlyUsedModules):
    %
    % Given ImplicitAvailNeeds, a data structure generated by
    % starting with init_implicit_avail_needs and then gathering
    % implicit import needs using calls to acc_implicit_avail_needs_in_*
    % return the set of modules that the items processed implicitly need used.
    % The set of modules that any set of items implicitly need imported
    % is *always* the singleton set containing only builtin.m.
    %
:- pred compute_implicit_avail_needs(globals::in, implicit_avail_needs::in,
    set(module_name)::out) is det.

    % extend_import_and_or_use_map_with_implicits(Globals,
    %   IntImplicitAvailNeeds, ImpImplicitAvailNeeds, !ImportUseMap):
    %
    % Given an ImplicitAvailNeeds structure for both the interface
    % and the implementation sections of a module, record the implicit
    % imports and/or uses they encode to !ImportUseMap.
    %
:- pred extend_import_and_or_use_map_with_implicits(globals::in,
    implicit_avail_needs::in, implicit_avail_needs::in,
    import_and_or_use_map::in, import_and_or_use_map::out) is det.

%---------------------------------------------------------------------------%

:- type item_contents
    --->    item_contents(
                ic_fifos                :: cord(foreign_include_file_info),
                ic_fact_tables          :: set(string),
                ic_langs                :: set(foreign_language),
                ic_foreign_export_langs :: set(foreign_language),
                ic_has_main             :: has_main
            ).

:- func init_item_contents = item_contents.

    % get_implicits_foreigns_fact_tables(IntItems, ImpItems,
    %   IntImplicitAvailNeeds, IntImpImplicitAvailNeeds, Contents):
    %
    % Given the interface and implementation items of a raw compilation unit,
    % compute and return
    %
    % - a representation of the implicit import needs of those items,
    %   i.e. of the set of modules that we need to implicitly import,
    %   in the interface and in the implementation;
    % - the foreign files they include;
    % - the foreign languages they use;
    % - the names of the files that contain their fact tables; and
    % - whether they export the program's entry point predicate.
    %
:- pred get_implicits_foreigns_fact_tables(list(item)::in, list(item)::in,
    implicit_avail_needs::out, implicit_avail_needs::out,
    item_contents::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.maybe_error.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

get_explicit_and_implicit_avail_needs_in_parse_tree_plain_opt(
        ParseTreePlainOpt, ExplicitModules, ImplicitAvailNeeds) :-
    ParseTreePlainOpt = parse_tree_plain_opt(_ModuleName, _ModuleNameContext,
        UseMap, _FIMSpecs, _TypeDefns, _ForeignEnums,
        _InstDefns, _ModeDefns, _TypeClasses, _Instances,
        _PredDecls, _ModeDecls, _Clauses, _ForeignProcs, _Promises,
        _MarkerPragmas, _TypeSpecs, _UnusedArgs, _TermInfos, _Term2Infos,
        _Exceptions, _Trailings, _MMTablings, _Sharings, _Reuses),
    map.keys_as_set(UseMap, ExplicitModules),
    acc_implicit_avail_needs_in_parse_tree_plain_opt(ParseTreePlainOpt,
        init_implicit_avail_needs, ImplicitAvailNeeds).

%---------------------------------------------------------------------------%

get_implicit_avail_needs_in_aug_compilation_unit(Globals, AugCompUnit,
        ImplicitlyUsedModules) :-
    ImplicitAvailNeeds0 = init_implicit_avail_needs,
    acc_implicit_avail_needs_in_aug_compilation_unit(AugCompUnit,
        ImplicitAvailNeeds0, ImplicitAvailNeeds),
    compute_implicit_avail_needs(Globals, ImplicitAvailNeeds,
        ImplicitlyUsedModules).

%---------------------------------------------------------------------------%

init_implicit_avail_needs = ImplicitAvailNeeds :-
    ImplicitAvailNeeds = implicit_avail_needs(dont_need_tabling,
        dont_need_tabling_statistics, dont_need_stm, dont_need_exception,
        dont_need_string_format, dont_need_stream_format, dont_need_io).

%---------------------------------------------------------------------------%

combine_implicit_needs(ImplicitNeedsList, ImplicitNeeds) :-
    combine_implicit_needs_acc(ImplicitNeedsList,
        dont_need_tabling, NeedTabling,
        dont_need_tabling_statistics, NeedTablingStatistics,
        dont_need_stm, NeedStm,
        dont_need_exception, NeedException,
        dont_need_string_format, NeedStringFormat,
        dont_need_stream_format, NeedStreamFormat,
        dont_need_io, NeedIO),
    ImplicitNeeds = implicit_avail_needs(NeedTabling, NeedTablingStatistics,
        NeedStm, NeedException, NeedStringFormat, NeedStreamFormat, NeedIO).

:- pred combine_implicit_needs_acc(list(implicit_avail_needs)::in,
    maybe_need_tabling::in, maybe_need_tabling::out,
    maybe_need_tabling_statistics::in, maybe_need_tabling_statistics::out,
    maybe_need_stm::in, maybe_need_stm::out,
    maybe_need_exception::in, maybe_need_exception::out,
    maybe_need_string_format::in, maybe_need_string_format::out,
    maybe_need_stream_format::in, maybe_need_stream_format::out,
    maybe_need_io::in, maybe_need_io::out) is det.

combine_implicit_needs_acc([], !NeedTabling,
        !NeedTablingStatistics, !NeedStm, !NeedException,
        !NeedStringFormat, !NeedStreamFormat, !NeedIO).
combine_implicit_needs_acc([Head | Tail], !NeedTabling,
        !NeedTablingStatistics, !NeedStm, !NeedException,
        !NeedStringFormat, !NeedStreamFormat, !NeedIO) :-
    Head = implicit_avail_needs(NeedTabling, NeedTablingStatistics,
        NeedStm, NeedException, NeedStringFormat, NeedStreamFormat, NeedIO),
    % XXX ARGPACK
    % If argument packing is enabled, it *should* be possible for the compiler
    % to replace this sequence of seven switches with a *single* bitwise OR
    % operation. Check whether this is the case.
    (
        NeedTabling = dont_need_tabling
    ;
        NeedTabling = do_need_tabling,
        !:NeedTabling = do_need_tabling
    ),
    (
        NeedTablingStatistics = dont_need_tabling_statistics
    ;
        NeedTablingStatistics = do_need_tabling_statistics,
        !:NeedTablingStatistics = do_need_tabling_statistics
    ),
    (
        NeedStm = dont_need_stm
    ;
        NeedStm = do_need_stm,
        !:NeedStm = do_need_stm
    ),
    (
        NeedException = dont_need_exception
    ;
        NeedException = do_need_exception,
        !:NeedException = do_need_exception
    ),
    (
        NeedStringFormat = dont_need_string_format
    ;
        NeedStringFormat = do_need_string_format,
        !:NeedStringFormat = do_need_string_format
    ),
    (
        NeedStreamFormat = dont_need_stream_format
    ;
        NeedStreamFormat = do_need_stream_format,
        !:NeedStreamFormat = do_need_stream_format
    ),
    (
        NeedIO = dont_need_io
    ;
        NeedIO = do_need_io,
        !:NeedIO = do_need_io
    ),
    combine_implicit_needs_acc(Tail, !NeedTabling,
        !NeedTablingStatistics, !NeedStm, !NeedException,
        !NeedStringFormat, !NeedStreamFormat, !NeedIO).

%---------------------------------------------------------------------------%

:- pred acc_implicit_avail_needs_in_aug_compilation_unit(
    aug_compilation_unit::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_aug_compilation_unit(AugCompUnit,
        !ImplicitAvailNeeds) :-
    AugCompUnit = aug_compilation_unit(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOpts, TransOpts, IntForOptSpecs),
    acc_implicit_avail_needs_in_parse_tree_module_src(ParseTreeModuleSrc,
        !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_ancestor_int_spec,
        AncestorIntSpecs, !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_direct_int_spec,
        DirectIntSpecs, !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_indirect_int_spec,
        IndirectIntSpecs, !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_int_for_opt_spec,
        IntForOptSpecs, !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_parse_tree_plain_opt,
        PlainOpts, !ImplicitAvailNeeds),
    map.foldl_values(acc_implicit_avail_needs_in_parse_tree_trans_opt,
        TransOpts, !ImplicitAvailNeeds).

%---------------------%

:- pred acc_implicit_avail_needs_in_parse_tree_module_src(
    parse_tree_module_src::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_module_src(ParseTreeModuleSrc,
        !ImplicitAvailNeeds) :-
    ParseTreeModuleSrc = parse_tree_module_src(_ModuleName, _ModuleNameContext,
        _IntInclMap, _ImpInclMap, _InclMap,
        _IntImportMap, _IntUseMap, _ImpImportMap, _ImpUseMap, _ImportUseMap,
        _IntFIMSpecMap, _ImpFIMSpecMap, _MaybeImplicitFIMLangs,

        _IntTypeDefnsAbs, IntTypeDefnsMer, _IntTypeDefnsForeign,
        _IntInstDefns, _IntModeDefns, _IntTypeClasses, IntInstances,
        _IntPredDecls, _IntModeDecls,
        _IntForeignExportEnums, _IntDeclPragmas, IntPromises, _IntBadPreds,

        _ImpTypeDefnsAbs, ImpTypeDefnsMer, _ImpTypeDefnsForeign,
        _ImpInstDefns, _ImpModeDefns, _ImpTypeClasses, ImpInstances,
        _ImpPredDecls, _ImpModeDecls, ImpClauses,
        _ImpForeignEnums, _ImpForeignExportEnums,
        _ImpDeclPragmas, ImpImplPragmas, ImpPromises,
        _ImpInitialises, _ImpFinalises, ImpMutables),

    list.foldl(acc_implicit_avail_needs_in_type_defn,
        IntTypeDefnsMer, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        IntInstances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_promise,
        IntPromises, !ImplicitAvailNeeds),

    list.foldl(acc_implicit_avail_needs_in_type_defn,
        ImpTypeDefnsMer, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        ImpInstances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_clause,
        ImpClauses, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_impl_pragma,
        ImpImplPragmas, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_promise,
        ImpPromises, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_mutable,
        ImpMutables, !ImplicitAvailNeeds).

%---------------------%

:- pred acc_implicit_avail_needs_in_ancestor_int_spec(ancestor_int_spec::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_ancestor_int_spec(AncestorIntSpec,
        !ImplicitAvailNeeds) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, _),
    acc_implicit_avail_needs_in_parse_tree_int0(ParseTreeInt0,
        !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_direct_int_spec(direct_int_spec::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_direct_int_spec(DirectIntSpec,
        !ImplicitAvailNeeds) :-
    (
        DirectIntSpec = direct_int1(ParseTreeInt1, _),
        acc_implicit_avail_needs_in_parse_tree_int1(ParseTreeInt1,
            !ImplicitAvailNeeds)
    ;
        DirectIntSpec = direct_int3(ParseTreeInt3, _),
        acc_implicit_avail_needs_in_parse_tree_int3(ParseTreeInt3,
            !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_indirect_int_spec(indirect_int_spec::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_indirect_int_spec(IndirectIntSpec,
        !ImplicitAvailNeeds) :-
    (
        IndirectIntSpec = indirect_int2(ParseTreeInt2, _),
        acc_implicit_avail_needs_in_parse_tree_int2(ParseTreeInt2,
            !ImplicitAvailNeeds)
    ;
        IndirectIntSpec = indirect_int3(ParseTreeInt3, _),
        acc_implicit_avail_needs_in_parse_tree_int3(ParseTreeInt3,
            !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_int_for_opt_spec(int_for_opt_spec::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_int_for_opt_spec(IntForOptIntSpec,
        !ImplicitAvailNeeds) :-
    (
        IntForOptIntSpec = for_opt_int0(ParseTreeInt0, _),
        acc_implicit_avail_needs_in_parse_tree_int0(ParseTreeInt0,
            !ImplicitAvailNeeds)
    ;
        IntForOptIntSpec = for_opt_int1(ParseTreeInt1, _),
        acc_implicit_avail_needs_in_parse_tree_int1(ParseTreeInt1,
            !ImplicitAvailNeeds)
    ;
        IntForOptIntSpec = for_opt_int2(ParseTreeInt2, _),
        acc_implicit_avail_needs_in_parse_tree_int2(ParseTreeInt2,
            !ImplicitAvailNeeds)
    ).

%---------------------%

:- pred acc_implicit_avail_needs_in_parse_tree_int0(parse_tree_int0::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_int0(ParseTreeInt0,
        !ImplicitAvailNeeds) :-
    ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntImportMap, _IntUsedMap, _ImpImportMap, _ImpUseMap, _ImportUseMap,
        _IntFIMSpecs, _ImpFIMSpecs,
        IntTypeDefnMap, _IntInstDefnMap, _IntModeDefnMap,
        _IntTypeClasses, IntInstances, _IntPredDecls, _IntModeDecls,
        _IntForeignEnumMap, _IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, _ImpInstDefnMap, _ImpModeDefnMap,
        _ImpTypeClasses, ImpInstances, _ImpPredDecls, _ImpModeDecls,
        _ImpForeignEnumMap, _ImpDeclPragmas, ImpPromises),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        IntTypeDefnMap, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        IntInstances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_promise,
        IntPromises, !ImplicitAvailNeeds),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        ImpTypeDefnMap, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        ImpInstances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_promise,
        ImpPromises, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_parse_tree_int1(parse_tree_int1::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_int1(ParseTreeInt1,
        !ImplicitAvailNeeds) :-
    ParseTreeInt1 = parse_tree_int1(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _ImpInclMap, _InclMap,
        _IntUseMap, _ImpUseMap, _ImportUseMap, _IntFIMSpecs, _ImpFIMSpecs,
        IntTypeDefnMap, _IntInstDefnMap, _IntModeDefnMap,
        _IntTypeClasses, IntInstances, _IntPredDecls, _IntModeDecls,
        _IntForeignEnumMap, _IntDeclPragmas, IntPromises, _IntTypeRepnMap,
        ImpTypeDefnMap, _ImpForeignEnumMap, _ImpTypeClasses),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        IntTypeDefnMap, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        IntInstances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_promise,
        IntPromises, !ImplicitAvailNeeds),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        ImpTypeDefnMap, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_parse_tree_int2(parse_tree_int2::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_int2(ParseTreeInt2,
        !ImplicitAvailNeeds) :-
    ParseTreeInt2 = parse_tree_int2(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _IntInclMap, _InclMap,
        _IntUseMap, _ImportUseMap, _IntFIMSpecs, _ImpFIMSpecs,
        IntTypeDefnMap, _IntInstDefnMap, _IntModeDefnMap,
        _IntTypeClasses, IntInstances, _IntTypeRepnMap,
        ImpTypeDefnMap),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        IntTypeDefnMap, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        IntInstances, !ImplicitAvailNeeds),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        ImpTypeDefnMap, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_parse_tree_int3(
    parse_tree_int3::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_int3(ParseTreeInt3,
        !ImplicitAvailNeeds) :-
    ParseTreeInt3 = parse_tree_int3(_ModuleName, _ModuleNameContext,
        _IntInclMap, _InclMap, _IntImportMap, _ImportUseMap,
        IntTypeDefnMap, _IntInstDefnMap, _IntModeDefnMap,
        _IntTypeClasses, IntInstances, _IntTypeRepnMap),

    map.foldl_values(acc_implicit_avail_needs_in_type_ctor_add_defns,
        IntTypeDefnMap, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        IntInstances, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_parse_tree_plain_opt(
    parse_tree_plain_opt::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_plain_opt(ParseTreePlainOpt,
        !ImplicitAvailNeeds) :-
    ParseTreePlainOpt = parse_tree_plain_opt(_ModuleName, _ModuleNameContext,
        _UsedModuleNames, _FIMSpecs, TypeDefns, _ForeignEnums,
        _InstDefns, _ModeDefns, _TypeClasses, Instances,
        _PredDecls, _ModeDecls, Clauses, _ForeignProcs, _Promises,
        _MarkerPragmas, _TypeSpecs, _UnusedArgs, _TermInfos, _Term2Infos,
        _Exceptions, _Trailings, _MMTablings, _Sharings, _Reuses),
    list.foldl(acc_implicit_avail_needs_in_type_defn,
        TypeDefns, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_instance,
        Instances, !ImplicitAvailNeeds),
    list.foldl(acc_implicit_avail_needs_in_clause,
        Clauses, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_parse_tree_trans_opt(
    parse_tree_trans_opt::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_parse_tree_trans_opt(ParseTreeTransOpt,
        !ImplicitAvailNeeds) :-
    ParseTreeTransOpt = parse_tree_trans_opt(_ModuleName, _ModuleNameContext,
        _TermInfos, _Term2Infos, _Exceptions, _Trailings, _MMTablings,
        _Sharings, _Reuses).
    % None of the item kinds in parse_tree_trans_opts can have any
    % implicit avail needs.

%---------------------%

:- pred acc_implicit_avail_needs_in_type_ctor_add_defns(
    type_ctor_all_defns::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_type_ctor_add_defns(AllDefns,
        !ImplicitAvailNeeds) :-
    AllDefns = type_ctor_all_defns(_SolverAbs, SolverNonAbs,
        _StdAbs, _StdEqv, _StdDu, _StdForeign),
    list.foldl(acc_implicit_avail_needs_in_type_defn_solver,
        SolverNonAbs, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_type_defn(item_type_defn_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_type_defn(ItemTypeDefn, !ImplicitAvailNeeds) :-
    ItemTypeDefn = item_type_defn_info(_TypeCtorName, _TypeParams,
        TypeDefn, _TVarSet, _Context, _SeqNum),
    (
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        )
    ;
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        acc_implicit_avail_needs_in_solver_details(DetailsSolver,
            !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_type_defn_solver(
    item_type_defn_info_solver::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_type_defn_solver(ItemTypeDefn,
        !ImplicitAvailNeeds) :-
    ItemTypeDefn = item_type_defn_info(_TypeCtorName, _TypeParams,
        DetailsSolver, _TVarSet, _Context, _SeqNum),
    acc_implicit_avail_needs_in_solver_details(DetailsSolver,
        !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_solver_details(type_details_solver::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_solver_details(DetailsSolver,
        !ImplicitAvailNeeds) :-
    DetailsSolver = type_details_solver(SolverTypeDetails,
        _MaybeUnifyComparePredNames),
    SolverTypeDetails = solver_type_details(_RepresentationType,
        _GroundInst, _AnyInst, MutableItems),
    list.foldl(acc_implicit_avail_needs_in_mutable, MutableItems,
        !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_instance(item_instance_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_instance(ItemInstance, !ImplicitAvailNeeds) :-
    ItemInstance = item_instance_info(_DerivingClass, _ClassName,
        _Types, _OriginalTypes, InstanceBody, _VarSet,
        _ModuleContainingInstance, _Context, _SeqNum),
    (
        InstanceBody = instance_body_abstract
    ;
        InstanceBody = instance_body_concrete(InstanceMethods),
        list.foldl(acc_implicit_avail_needs_in_instance_method,
            InstanceMethods, !ImplicitAvailNeeds)
    ).

acc_implicit_avail_needs_in_instance_method(InstanceMethod,
        !ImplicitAvailNeeds) :-
    InstanceMethod = instance_method(_PredOrFunc, _MethodName, ProcDef,
        _Arity, _Context),
    (
        ProcDef = instance_proc_def_name(_Name)
    ;
        ProcDef = instance_proc_def_clauses(ItemClauses),
        list.foldl(acc_implicit_avail_needs_in_clause, ItemClauses,
            !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_impl_pragma(item_impl_pragma_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_impl_pragma(ItemImplPragma,
        !ImplicitAvailNeeds) :-
    ItemImplPragma = item_pragma_info(ImplPragma, _Context, _SeqNum),
    (
        ImplPragma = impl_pragma_tabled(TableInfo),
        TableInfo = pragma_info_tabled(_, _, _, MaybeAttributes),
        !ImplicitAvailNeeds ^ ian_tabling := do_need_tabling,
        (
            MaybeAttributes = no
        ;
            MaybeAttributes = yes(Attributes),
            StatsAttr = Attributes ^ table_attr_statistics,
            (
                StatsAttr = table_gather_statistics,
                !ImplicitAvailNeeds ^ ian_tabling_statistics
                    := do_need_tabling_statistics
            ;
                StatsAttr = table_dont_gather_statistics
            )
        )
    ;
        ( ImplPragma = impl_pragma_foreign_decl(_)
        ; ImplPragma = impl_pragma_foreign_code(_)
        ; ImplPragma = impl_pragma_foreign_proc(_)
        ; ImplPragma = impl_pragma_foreign_proc_export(_)
        ; ImplPragma = impl_pragma_external_proc(_)
        ; ImplPragma = impl_pragma_inline(_)
        ; ImplPragma = impl_pragma_no_inline(_)
        ; ImplPragma = impl_pragma_consider_used(_)
        ; ImplPragma = impl_pragma_no_detism_warning(_)
        ; ImplPragma = impl_pragma_fact_table(_)
        ; ImplPragma = impl_pragma_promise_eqv_clauses(_)
        ; ImplPragma = impl_pragma_promise_pure(_)
        ; ImplPragma = impl_pragma_promise_semipure(_)
        ; ImplPragma = impl_pragma_mode_check_clauses(_)
        ; ImplPragma = impl_pragma_require_feature_set(_)
        ; ImplPragma = impl_pragma_require_tail_rec(_)
        )
    ).

acc_implicit_avail_needs_in_mutable(ItemMutableInfo,
        !ImplicitAvailNeeds) :-
    ItemMutableInfo = item_mutable_info(_Name,
        _OrigType, _Type, _OrigInst, _Inst, InitValue,
        _Attrs, _VarSet, _Context, _SeqNum),
    acc_implicit_avail_needs_in_term(InitValue, !ImplicitAvailNeeds).

acc_implicit_avail_needs_in_clause(ItemClause, !ImplicitAvailNeeds) :-
    ItemClause = item_clause_info(_PredName,_PredOrFunc, HeadTerms,
        _Origin, _VarSet, MaybeGoal, _Context, _SeqNum),
    acc_implicit_avail_needs_in_terms(HeadTerms, !ImplicitAvailNeeds),
    (
        MaybeGoal = ok2(Goal, _),
        acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds)
    ;
        MaybeGoal = error2(_)
    ).

:- pred acc_implicit_avail_needs_in_promise(item_promise_info::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_promise(ItemPromise, !ImplicitAvailNeeds) :-
    ItemPromise = item_promise_info(_PromiseType, Goal, _VarSet,
        _UnivQuantVars, _Context, _SeqNum),
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds).

acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds) :-
    (
        ( Goal = true_expr(_)
        ; Goal = fail_expr(_)
        )
        % Cannot contain anything that requires implicit imports.
    ;
        ( Goal = conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = par_conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = disj_expr(_, SubGoalA, SubGoalB)
        ; Goal = implies_expr(_, SubGoalA, SubGoalB)
        ; Goal = equivalent_expr(_, SubGoalA, SubGoalB)
        ),
        acc_implicit_avail_needs_in_goal(SubGoalA, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_goal(SubGoalB, !ImplicitAvailNeeds)
    ;
        ( Goal = not_expr(_, SubGoal)
        ; Goal = quant_expr(_, _, _, _Vars, SubGoal)
        ; Goal = promise_purity_expr(_, _Purity, SubGoal)
        ; Goal = promise_equivalent_solutions_expr(_, _OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; Goal = promise_equivalent_solution_sets_expr(_, _OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; Goal = promise_equivalent_solution_arbitrary_expr(_, _OrdVars,
            _StateVars, _DotVars, _ColonVars, SubGoal)
        ; Goal = require_detism_expr(_, _Detism, SubGoal)
        ; Goal = require_complete_switch_expr(_, _SwitchVar, SubGoal)
        ; Goal = require_switch_arms_detism_expr(_, _SwitchVar, _Detism,
            SubGoal)
        ; Goal = disable_warnings_expr(_, _HeadWarning, _TailWarnings, SubGoal)
        ),
        acc_implicit_avail_needs_in_goal(SubGoal, !ImplicitAvailNeeds)
    ;
        Goal = trace_expr(_, _CompCond, _RunCond, MaybeIO, _Mutables,
            SubGoal),
        (
            MaybeIO = yes(_),
            !ImplicitAvailNeeds ^ ian_io := do_need_io
        ;
            MaybeIO = no
        ),
        acc_implicit_avail_needs_in_goal(SubGoal, !ImplicitAvailNeeds)
    ;
        Goal = try_expr(_, _MaybeIO, SubGoal, Then, MaybeElse,
            Catches, MaybeCatchAny),
        !ImplicitAvailNeeds ^ ian_exception := do_need_exception,
        acc_implicit_avail_needs_in_goal(SubGoal, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_goal(Then, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_maybe_goal(MaybeElse,
            !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_catch_exprs(Catches,
            !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_maybe_catch_any_expr(MaybeCatchAny,
            !ImplicitAvailNeeds)
    ;
        Goal = if_then_else_expr(_, _Vars, _StateVars, Cond, Then, Else),
        acc_implicit_avail_needs_in_goal(Cond, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_goal(Then, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_goal(Else, !ImplicitAvailNeeds)
    ;
        Goal = atomic_expr(_, _Outer, _Inner, _OutputVars,
            MainGoal, OrElseGoals),
        !ImplicitAvailNeeds ^ ian_stm := do_need_stm,
        !ImplicitAvailNeeds ^ ian_exception := do_need_exception,
        acc_implicit_avail_needs_in_goal(MainGoal, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_goals(OrElseGoals, !ImplicitAvailNeeds)
    ;
        Goal = call_expr(_, CalleeSymName, Args, _Purity),
        ( if
            CalleeSymName = qualified(ModuleName, "format")
        then
            ( if
                ( ModuleName = unqualified("string")
                ; ModuleName = unqualified("io")
                )
            then
                % For io.format, we need to pull in the same modules
                % as for string.format.
                !ImplicitAvailNeeds ^ ian_string_format
                    := do_need_string_format
            else if
                ( ModuleName = unqualified("stream")
                ; ModuleName = unqualified("string_writer")
                ; ModuleName = qualified(unqualified("stream"),
                    "string_writer")
                )
            then
                % The replacement of calls to stream.string_writer.format
                % needs everything that the replacement of calls to
                % string.format or io.format needs.
                !ImplicitAvailNeeds ^ ian_string_format
                    := do_need_string_format,
                !ImplicitAvailNeeds ^ ian_stream_format
                    := do_need_stream_format
            else
                % The callee cannot be any of the predicates that
                % format_call.m is designed to optimize.
                true
            )
        else if
            CalleeSymName = unqualified("format")
        then
            % We don't know whether this will resolve to string.format,
            % io.format, or stream.string.writer.format. Ideally, we would
            % set ian_stream_format only if the current context contains
            % an import of stream.string_writer.m, but we don't have that
            % information here, or in our caller.
            !ImplicitAvailNeeds ^ ian_string_format := do_need_string_format,
            !ImplicitAvailNeeds ^ ian_stream_format := do_need_stream_format
        else
            true
        ),
        acc_implicit_avail_needs_in_terms(Args, !ImplicitAvailNeeds)
    ;
        Goal = event_expr(_, _EventName, EventArgs),
        acc_implicit_avail_needs_in_terms(EventArgs, !ImplicitAvailNeeds)
    ;
        Goal = unify_expr(_, TermA, TermB, _Purity),
        acc_implicit_avail_needs_in_term(TermA, !ImplicitAvailNeeds),
        acc_implicit_avail_needs_in_term(TermB, !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_goals(list(goal)::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_goals([], !ImplicitAvailNeeds).
acc_implicit_avail_needs_in_goals([Goal | Goals], !ImplicitAvailNeeds) :-
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds),
    acc_implicit_avail_needs_in_goals(Goals, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_maybe_goal(maybe(goal)::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_maybe_goal(no, !ImplicitAvailNeeds).
acc_implicit_avail_needs_in_maybe_goal(yes(Goal), !ImplicitAvailNeeds) :-
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_catch_exprs(list(catch_expr)::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_catch_exprs([], !ImplicitAvailNeeds).
acc_implicit_avail_needs_in_catch_exprs([CatchExpr | CatchExprs],
        !ImplicitAvailNeeds) :-
    CatchExpr = catch_expr(_Pattern, Goal),
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds),
    acc_implicit_avail_needs_in_catch_exprs(CatchExprs, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_maybe_catch_any_expr(
    maybe(catch_any_expr)::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_maybe_catch_any_expr(no, !ImplicitAvailNeeds).
acc_implicit_avail_needs_in_maybe_catch_any_expr(yes(CatchAnyExpr),
        !ImplicitAvailNeeds) :-
    CatchAnyExpr = catch_any_expr(_Var, Goal),
    acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds).

:- pred acc_implicit_avail_needs_in_term(prog_term::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_term(Term, !ImplicitAvailNeeds) :-
    (
        Term = variable(_Var, _Context)
    ;
        Term = functor(Const, ArgTerms, _Context),
        (
            Const = atom(Atom),
            ( if
                Atom = "format"
            then
                !ImplicitAvailNeeds ^ ian_string_format
                    := do_need_string_format,
                !ImplicitAvailNeeds ^ ian_stream_format
                    := do_need_stream_format
            else if
                ( Atom = "string.format"
                ; Atom = "string__format"
                ; Atom = "io.format"
                ; Atom = "io__format"
                )
            then
                !ImplicitAvailNeeds ^ ian_string_format
                    := do_need_string_format
            else if
                ( Atom = "stream.format"
                ; Atom = "stream__format"
                ; Atom = "string_writer.format"
                ; Atom = "string_writer__format"
                ; Atom = "stream.string_writer.format"
                ; Atom = "stream.string_writer__format"
                ; Atom = "stream__string_writer.format"
                ; Atom = "stream__string_writer__format"
                )
            then
                % The replacement of calls to stream.string_writer.format
                % needs everything that the replacement of calls to
                % string.format or io.format needs.
                !ImplicitAvailNeeds ^ ian_string_format
                    := do_need_string_format,
                !ImplicitAvailNeeds ^ ian_stream_format
                    := do_need_stream_format
            else
                true
            )
        ;
            ( Const = integer(_, _, _, _)
            ; Const = string(_)
            ; Const = float(_)
            ; Const = implementation_defined(_)
            )
        ),
        acc_implicit_avail_needs_in_terms(ArgTerms, !ImplicitAvailNeeds)
    ).

:- pred acc_implicit_avail_needs_in_terms(list(prog_term)::in,
    implicit_avail_needs::in, implicit_avail_needs::out) is det.

acc_implicit_avail_needs_in_terms([], !ImplicitAvailNeeds).
acc_implicit_avail_needs_in_terms([Term | Terms], !ImplicitAvailNeeds) :-
    acc_implicit_avail_needs_in_term(Term, !ImplicitAvailNeeds),
    acc_implicit_avail_needs_in_terms(Terms, !ImplicitAvailNeeds).

%---------------------------------------------------------------------------%

compute_implicit_avail_needs(Globals, ImplicitAvailNeeds,
        !:ImplicitlyUsedModules) :-
    !:ImplicitlyUsedModules =
        set.make_singleton_set(mercury_private_builtin_module),
    ImplicitAvailNeeds = implicit_avail_needs(
        ItemsNeedTabling, ItemsNeedTablingStatistics,
        ItemsNeedSTM, ItemsNeedException,
        ItemsNeedStringFormat, ItemsNeedStreamFormat, ItemsNeedIO),
    % We should include mercury_table_builtin_module if the Items contain
    % a tabling pragma, or if one of --use-minimal-model (either kind) and
    % --trace-table-io is specified. In the former case, we may also need
    % to import mercury_table_statistics_module.
    (
        ItemsNeedTabling = do_need_tabling,
        set.insert(mercury_table_builtin_module, !ImplicitlyUsedModules),
        (
            ItemsNeedTablingStatistics = do_need_tabling_statistics,
            set.insert(mercury_table_statistics_module, !ImplicitlyUsedModules)
        ;
            ItemsNeedTablingStatistics = dont_need_tabling_statistics
        )
    ;
        ItemsNeedTabling = dont_need_tabling,
        expect(unify(ItemsNeedTablingStatistics, dont_need_tabling_statistics),
            $pred, "tabling statistics without tabling"),
        ( if
            % These forms of tabling cannot ask for statistics.
            (
                globals.lookup_bool_option(Globals,
                    use_minimal_model_stack_copy, yes)
            ;
                globals.lookup_bool_option(Globals,
                    use_minimal_model_own_stacks, yes)
            ;
                globals.lookup_bool_option(Globals, trace_table_io, yes)
            )
        then
            set.insert(mercury_table_builtin_module, !ImplicitlyUsedModules)
        else
            true
        )
    ),
    (
        ItemsNeedSTM = do_need_stm,
        set.insert(mercury_stm_builtin_module, !ImplicitlyUsedModules),
        set.insert(mercury_exception_module, !ImplicitlyUsedModules),
        set.insert(mercury_univ_module, !ImplicitlyUsedModules)
    ;
        ItemsNeedSTM = dont_need_stm
    ),
    (
        ItemsNeedException = do_need_exception,
        set.insert(mercury_exception_module, !ImplicitlyUsedModules)
    ;
        ItemsNeedException = dont_need_exception
    ),
    (
        ItemsNeedStringFormat = do_need_string_format,
        set.insert(mercury_string_format_module, !ImplicitlyUsedModules),
        set.insert(mercury_string_parse_util_module, !ImplicitlyUsedModules)
    ;
        ItemsNeedStringFormat = dont_need_string_format
    ),
    (
        ItemsNeedStreamFormat = do_need_stream_format,
        set.insert(mercury_stream_module, !ImplicitlyUsedModules)
    ;
        ItemsNeedStreamFormat = dont_need_stream_format
    ),
    (
        ItemsNeedIO = do_need_io,
        set.insert(mercury_io_module, !ImplicitlyUsedModules)
    ;
        ItemsNeedIO = dont_need_io
    ),
    globals.lookup_bool_option(Globals, profile_deep, Deep),
    (
        Deep = yes,
        set.insert(mercury_profiling_builtin_module, !ImplicitlyUsedModules)
    ;
        Deep = no
    ),
    ( if
        (
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        )
    then
        set.insert(mercury_term_size_prof_builtin_module,
            !ImplicitlyUsedModules)
    else
        true
    ),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    ( if
        Target = target_c,
        HighLevelCode = no,
        Parallel = yes
    then
        set.insert(mercury_par_builtin_module, !ImplicitlyUsedModules)
    else
        true
    ),
    globals.lookup_bool_option(Globals, use_regions, UseRegions),
    (
        UseRegions = yes,
        set.insert(mercury_region_builtin_module, !ImplicitlyUsedModules)
    ;
        UseRegions = no
    ),
    globals.get_ssdb_trace_level(Globals, SSDBTraceLevel),
    (
        SSDBTraceLevel = ssdb_none
    ;
        ( SSDBTraceLevel = ssdb_shallow
        ; SSDBTraceLevel = ssdb_deep
        ),
        globals.lookup_bool_option(Globals, force_disable_ssdebug,
            DisableSSDB),
        (
            DisableSSDB = yes
        ;
            DisableSSDB = no,
            set.insert(mercury_ssdb_builtin_module, !ImplicitlyUsedModules)
        )
    ).

extend_import_and_or_use_map_with_implicits(Globals,
        IntImplicitAvailNeeds, ImpImplicitAvailNeeds, !ImportUseMap) :-
    compute_implicit_avail_needs(Globals, IntImplicitAvailNeeds,
        IntImplicitUses),
    compute_implicit_avail_needs(Globals, ImpImplicitAvailNeeds,
        ImpImplicitUses),
    PublicBuiltin = mercury_public_builtin_module,
    add_implicit_avail(implicit_int_import, PublicBuiltin, !ImportUseMap),
    set.foldl(add_implicit_avail(implicit_int_use), IntImplicitUses,
        !ImportUseMap),
    set.foldl(add_implicit_avail(implicit_imp_use), ImpImplicitUses,
        !ImportUseMap).

:- pred add_implicit_avail(implicit_import_or_use::in, module_name::in,
    import_and_or_use_map::in, import_and_or_use_map::out) is det.

add_implicit_avail(Implicit, ModuleName, !ImportUseMap) :-
    ( if map.search(!.ImportUseMap, ModuleName, OldEntry) then
        (
            OldEntry = explicit_avail(Explicit),
            NewEntry = implicit_avail(Implicit, yes(Explicit)),
            map.det_update(ModuleName, NewEntry, !ImportUseMap)
        ;
            OldEntry = implicit_avail(_, _)
            % Since we insert implicit avails into !ImportUseMap in order
            % from most general (int_import) to least general (imp_use),
            % if we find any existing implicit avail for this module,
            % keep it.
        )
    else
        NewEntry = implicit_avail(Implicit, no),
        map.det_insert(ModuleName, NewEntry, !ImportUseMap)
    ).

%---------------------------------------------------------------------------%

init_item_contents =
    item_contents(cord.init, set.init, set.init, set.init, no_main).

get_implicits_foreigns_fact_tables(IntItems, ImpItems,
        IntImplicitAvailNeeds, IntImpImplicitAvailNeeds, !:Contents) :-
    ImplicitAvailNeeds0 = init_implicit_avail_needs,
    !:Contents = init_item_contents,
    get_implicits_foreigns_fact_tables_acc(IntItems,
        ImplicitAvailNeeds0, IntImplicitAvailNeeds, !Contents),
    IntHasMain = !.Contents ^ ic_has_main,
    get_implicits_foreigns_fact_tables_acc(ImpItems,
        IntImplicitAvailNeeds, IntImpImplicitAvailNeeds, !Contents),
    % We ignore declarations of predicates named "main" in the
    % implementation section, because nonexported predicates cannot serve
    % as program entry points.
    !Contents ^ ic_has_main := IntHasMain.

:- pred get_implicits_foreigns_fact_tables_acc(list(item)::in,
    implicit_avail_needs::in, implicit_avail_needs::out,
    item_contents::in, item_contents::out) is det.

get_implicits_foreigns_fact_tables_acc([], !ImplicitAvailNeeds, !Contents).
get_implicits_foreigns_fact_tables_acc([Item | Items],
        !ImplicitAvailNeeds, !Contents) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(_TypeCtorName, _TypeParams,
            TypeDefn, _TVarSet, _Context, _SeqNum),
        (
            TypeDefn = parse_tree_solver_type(DetailsSolver),
            DetailsSolver = type_details_solver(SolverTypeDetails,
                _MaybeUnifyComparePredNames),
            SolverTypeDetails = solver_type_details(_RepresentationType,
                _GroundInst, _AnyInst, MutableItems),
            list.foldl(acc_implicit_avail_needs_in_mutable, MutableItems,
                !ImplicitAvailNeeds)
        ;
            TypeDefn = parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _),
            Langs0 = !.Contents ^ ic_langs,
            set.insert(foreign_type_language(ForeignType), Langs0, Langs),
            !Contents ^ ic_langs := Langs
        ;
            ( TypeDefn = parse_tree_du_type(_)
            ; TypeDefn = parse_tree_eqv_type(_)
            ; TypeDefn = parse_tree_abstract_type(_)
            )
        )
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        ItemForeignEnum = item_foreign_enum_info(Lang, _, _, _, _),
        Langs0 = !.Contents ^ ic_langs,
        set.insert(Lang, Langs0, Langs),
        !Contents ^ ic_langs := Langs
    ;
        Item = item_impl_pragma(ItemImplPragma),
        ItemImplPragma = item_pragma_info(ImplPragma, _, _),
        (
            (
                ImplPragma = impl_pragma_foreign_decl(FDInfo),
                FDInfo = pragma_info_foreign_decl(Lang, _, LiteralOrInclude)
            ;
                ImplPragma = impl_pragma_foreign_code(FCInfo),
                FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude)
            ),
            (
                LiteralOrInclude = floi_literal(_)
            ;
                LiteralOrInclude = floi_include_file(FileName),
                InclFile = foreign_include_file_info(Lang, FileName),
                FIFOs0 = !.Contents ^ ic_fifos,
                FIFOs = cord.snoc(FIFOs0, InclFile),
                !Contents ^ ic_fifos := FIFOs
            ),
            Langs0 = !.Contents ^ ic_langs,
            set.insert(Lang, Langs0, Langs),
            !Contents ^ ic_langs := Langs
        ;
            ImplPragma = impl_pragma_foreign_proc(FPInfo),
            FPInfo = pragma_info_foreign_proc(Attrs, _, _, _, _, _, _),
            Langs0 = !.Contents ^ ic_langs,
            set.insert(get_foreign_language(Attrs), Langs0, Langs),
            !Contents ^ ic_langs := Langs
        ;
            ImplPragma = impl_pragma_foreign_proc_export(FPEInfo),
            FPEInfo = pragma_info_foreign_proc_export(_, Lang, _, _),
            FELangs0 = !.Contents ^ ic_foreign_export_langs,
            Langs0 = !.Contents ^ ic_langs,
            set.insert(Lang, FELangs0, FELangs),
            set.insert(Lang, Langs0, Langs),
            !Contents ^ ic_foreign_export_langs := FELangs,
            !Contents ^ ic_langs := Langs
        ;
            ImplPragma = impl_pragma_fact_table(FactTableInfo),
            FactTableInfo = pragma_info_fact_table(_PredNameArity, FileName),
            FactTables0 = !.Contents ^ ic_fact_tables,
            set.insert(FileName, FactTables0, FactTables),
            !Contents ^ ic_fact_tables := FactTables
        ;
            ImplPragma = impl_pragma_tabled(TableInfo),
            TableInfo = pragma_info_tabled(_, _, _, MaybeAttributes),
            !ImplicitAvailNeeds ^ ian_tabling := do_need_tabling,
            (
                MaybeAttributes = no
            ;
                MaybeAttributes = yes(Attributes),
                StatsAttr = Attributes ^ table_attr_statistics,
                (
                    StatsAttr = table_gather_statistics,
                    !ImplicitAvailNeeds ^ ian_tabling_statistics
                        := do_need_tabling_statistics
                ;
                    StatsAttr = table_dont_gather_statistics
                )
            )
        ;
            ( ImplPragma = impl_pragma_external_proc(_)
            ; ImplPragma = impl_pragma_inline(_)
            ; ImplPragma = impl_pragma_no_inline(_)
            ; ImplPragma = impl_pragma_consider_used(_)
            ; ImplPragma = impl_pragma_no_detism_warning(_)
            ; ImplPragma = impl_pragma_require_tail_rec(_)
            ; ImplPragma = impl_pragma_promise_eqv_clauses(_)
            ; ImplPragma = impl_pragma_promise_pure(_)
            ; ImplPragma = impl_pragma_promise_semipure(_)
            ; ImplPragma = impl_pragma_mode_check_clauses(_)
            ; ImplPragma = impl_pragma_require_feature_set(_)
            )
        )
    ;
        Item = item_instance(ItemInstance),
        ItemInstance = item_instance_info(_DerivingClass, _ClassName,
            _Types, _OriginalTypes, InstanceBody, _VarSet,
            _ModuleContainingInstance, _Context, _SeqNum),
        (
            InstanceBody = instance_body_abstract
        ;
            InstanceBody = instance_body_concrete(InstanceMethods),
            list.foldl(acc_implicit_avail_needs_in_instance_method,
                InstanceMethods, !ImplicitAvailNeeds)
        )
    ;
        Item = item_clause(ItemClause),
        acc_implicit_avail_needs_in_clause(ItemClause, !ImplicitAvailNeeds)
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(_PromiseType, Goal, _VarSet,
            _UnivQuantVars, _Context, _SeqNum),
        acc_implicit_avail_needs_in_goal(Goal, !ImplicitAvailNeeds)
    ;
        Item = item_mutable(ItemMutable),
        Langs0 = !.Contents ^ ic_langs,
        set.insert_list(all_foreign_languages, Langs0, Langs),
        !Contents ^ ic_langs := Langs,
        acc_implicit_avail_needs_in_mutable(ItemMutable, !ImplicitAvailNeeds)
    ;
        ( Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        FELangs0 = !.Contents ^ ic_foreign_export_langs,
        set.insert_list(all_foreign_languages, FELangs0, FELangs),
        !Contents ^ ic_foreign_export_langs := FELangs
    ;
        Item = item_pred_decl(ItemPredDecl),
        ItemPredDecl = item_pred_decl_info(SymName, PorF, ArgTypes,
            _, WithType, _, _, _, _, _, _, _, _, _),
        ( if
            % XXX ITEM_LIST Could the predicate name be unqualified?
            (
                SymName = unqualified(_),
                unexpected($pred, "unqualified SymName")
            ;
                SymName = qualified(_, "main")
            ),
            PorF = pf_predicate,
            % XXX The comment below is obsolete, and was probably wrong
            % even before it became obsolete.
            % XXX We should allow `main/2' to be declared using `with_type`,
            % but equivalences haven't been expanded at this point.
            % The `has_main' field is only used for some special case handling
            % of the module containing main for the IL backend (we generate
            % a `.exe' file rather than a `.dll' file). This would arguably
            % be better done by generating a `.dll' file as normal, and a
            % separate `.exe' file containing initialization code and a call
            % to `main/2', as we do with the `_init.c' file in the C backend.
            ArgTypes = [_, _],
            WithType = no
        then
            !Contents ^ ic_has_main := has_main
        else
            true
        )
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_type_repn(_)
        )
    ),
    get_implicits_foreigns_fact_tables_acc(Items,
        !ImplicitAvailNeeds, !Contents).

%---------------------------------------------------------------------------%
:- end_module parse_tree.get_dependencies.
%---------------------------------------------------------------------------%
