%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_module.m.
% Main authors: conway, fjh.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_module.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

%---------------------------------------------------------------------------%

    % Print out an entire HLDS structure.
    %
:- pred write_hlds(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.
:- func hlds_to_string(module_info) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% XXX :- import_module hlds.pred_table.
% We actually use a type equivalence from pred_table.m (specifically,
% the fact that pred_id_table is a map), but we get an unused import warning
% for the above line anyway.
%
% The problem is as follows.
%
% - The format_preds predicate calls module_info_get_pred_id_table,
%   whose output has type pred_id_table. Therefore logically, the type
%   of the variable that holds this output is also pred_id_table,
%   which means that logically, this module *does* use *something*
%   exported by pred_table.m.
%
% - On the other hand, the equiv_type.m pass, which operates on the augmented
%   compilation unit, expands out equivalence types. The type of the output
%   argument of module_info_get_pred_id_table, pred_id_table, is thus
%   replaced with map(pred_id, pred_info). None of those types is defined
%   in pred_table.m, and this module uses nothing else exported from
%   pred_table.m either. This is why code generation can succeed
%   without importing pred_table.m, and this is also why we get the warning
%   about pred_table.m being unused if we *do* import it.
%
% I (zs) see three ways of fixing this problem.
%
% - We could have equiv_type.m record, in every item in which it expands out
%   a type equivalence (or, for the same reason, an inst or mode equivalence)
%   defined in a given module, record the name of that module in a new field
%   in that item. In this case, this would mean including the pred_table
%   module in this new field in the pred decl item for
%   module_info_get_pred_id_table. We would preserve the value of this field
%   in the HLDS, e.g. in pred_infos. Then, when the code of this module
%   references module_info_get_pred_id_table, the code generating unused
%   module warnings would consider that reference to use not just the module
%   that defines module_info_get_pred_id_table, but also all the modules
%   recorded in the new "modules that defined expanded equivalences" field
%   of its pred_info. And likewise for other entities that contain types,
%   insts and/or modes that can be expanded.
%
%   This approach would record this information on a per item basis
%   because we want to avoid false positives. If module A imports module B,
%   and module B contains a predicate p that refers to an equivalence type
%   in module C, this fact should make module C appear used in module A
%   if and only if module A actually *uses* predicate p.
%
% - We could add to every alternative in the mer_type, mer_inst and mer_mode
%   types a new field that records the set of modules that defined the
%   equivalence types, insts or modes in its construction. I mean that if
%   the programmer writes map(pred_id, pred_info), then this set would be
%   empty, but if the programmer writes pred_id_table, then, when replacing it
%   with map(pred_id, pred_info), equiv_type.m would include the pred_table
%   module in this set. Every compiler pass *but* unused imports would
%   of course ignore this extra argument.
%
%   A complication here is that unifications of mer_types, mer_insts and
%   mer_modes would have to be done using code that ignores the new fields.
%   Likewise, we wouldn't be able to use values of those types as keys
%   in maps without canonicalizing this field, probably by setting it to the
%   empty set.
%
% - We could simply NOT run equiv_type.m on the augmented compilation unit,
%   and instead expand type equivalences during type checking, and inst and
%   mode equivalences during mode checking. In both cases, we could record
%   the modules defining the types, insts and modes expanded out as being used.
%
% The second solution is probably the easiest to implement, but it also comes
% with a high and persistent memory overhead, which is why I don't think
% it is a good idea. The first solution is the next easiest. The best solution
% is, I think, the third, but it is the hardest to retrofit to our existing
% implementation. (Though it would have been much easier to implement when
% originally writing the type and mode checkers :-()

:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_out.hlds_out_inst_table.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_out.hlds_out_type_table.
:- import_module hlds.hlds_out.hlds_out_typeclass_table.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_name.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module digraph.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module string.builder.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out (selected parts of) the entire HLDS.
%

write_hlds(Stream, ModuleInfo, !IO) :-
    Str = hlds_to_string(ModuleInfo),
    io.write_string(Stream, Str, !IO).

hlds_to_string(ModuleInfo) = Str :-
    State0 = string.builder.init,
    format_hlds(ModuleInfo, State0, State),
    Str = string.builder.to_string(State).

:- pred format_hlds(module_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_hlds(ModuleInfo, !State) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_id,
        DumpPredIdStrs),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_name,
        DumpPredNames),
    globals.lookup_bool_option(Globals, dump_hlds_spec_preds, DumpSpecPreds0),
    globals.lookup_accumulating_option(Globals, dump_hlds_spec_preds_for,
        DumpSpecPredTypeNames),
    format_header(ModuleInfo, !State),
    Info = init_hlds_out_info(Globals, output_debug),
    Lang = output_debug,
    DumpOptions0 = Info ^ hoi_dump_hlds_options,
    (
        DumpSpecPredTypeNames = [],
        DumpSpecPreds = DumpSpecPreds0
    ;
        DumpSpecPredTypeNames = [_ | _],
        DumpSpecPreds = yes
    ),
    (
        DumpSpecPreds = no,
        DumpOptions = DumpOptions0
    ;
        DumpSpecPreds = yes,
        DumpOptions = DumpOptions0 ^ dump_unify_compare_preds := yes
    ),
    ( if
        % If the user specifically requested one or more predicates and/or
        % functions to be dumped, they won't be interested in the types,
        % insts etc.
        ( DumpPredIdStrs = [_ | _]
        ; DumpPredNames = [_ | _]
        ; DumpSpecPreds = yes
        )
    then
        true
    else
        DumpImports = DumpOptions ^ dump_imports,
        (
            DumpImports = yes,
            module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
            map.foldl(format_avail_entry, AvailModuleMap, !State)
        ;
            DumpImports = no
        ),

        DumpTypesClasses = DumpOptions ^ dump_type_typeclass_tables,
        (
            DumpTypesClasses = yes,
            LocalOnly = DumpOptions ^ dump_type_table_only_local,
            module_info_get_type_table(ModuleInfo, TypeTable),
            module_info_get_instance_table(ModuleInfo, InstanceTable),
            module_info_get_class_table(ModuleInfo, ClassTable),
            format_type_table(Info, LocalOnly, TypeTable, !State),
            format_classes(Info, ClassTable, !State),
            format_instances(Info, InstanceTable, !State)
        ;
            DumpTypesClasses = no
        ),

        DumpInstsModes = DumpOptions ^ dump_inst_mode_tables,
        (
            DumpInstsModes = yes,
            module_info_get_inst_table(ModuleInfo, InstTable),
            module_info_get_mode_table(ModuleInfo, ModeTable),
            DumpStructuredInsts = DumpOptions ^ dump_structured_insts,
            (
                DumpStructuredInsts = yes,
                MaybeUseErrorMsgInst = use_error_msg_inst(ModuleInfo)
            ;
                DumpStructuredInsts = no,
                MaybeUseErrorMsgInst = do_not_use_error_msg_inst
            ),
            globals.lookup_int_option(Globals, dump_hlds_inst_limit,
                InstNumLimit),
            globals.lookup_int_option(Globals, dump_hlds_inst_size_limit,
                InstSizeLimit),
            format_inst_table(Lang, MaybeUseErrorMsgInst,
                InstNumLimit, InstSizeLimit, InstTable, !State),
            format_mode_table(ModeTable, !State)
        ;
            DumpInstsModes = no
        ),

        DumpConsTable = DumpOptions ^ dump_cons_table,
        (
            DumpConsTable = yes,
            module_info_get_cons_table(ModuleInfo, ConsTable),
            format_cons_table(ConsTable, !State)
        ;
            DumpConsTable = no
        ),

        DumpCallAnswerTables = DumpOptions ^ dump_call_answer_tables,
        (
            DumpCallAnswerTables = yes,
            module_info_get_table_struct_map(ModuleInfo, TableStructMap),
            format_table_structs(ModuleInfo, TableStructMap, !State)
        ;
            DumpCallAnswerTables = no
        )
    ),

    DumpConstStructs = DumpOptions ^ dump_constant_structures,
    (
        DumpConstStructs = yes,
        module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
        format_const_struct_db(ConstStructDb, !State)
    ;
        DumpConstStructs = no
    ),

    DumpPreds = DumpOptions ^ dump_predicates,
    ( if
        ( DumpPreds = yes
        ; DumpSpecPreds = yes
        )
    then
        format_preds(Info, DumpSpecPreds, DumpSpecPredTypeNames, Lang,
            ModuleInfo, !State)
    else
        true
    ),

    DumpDepOrder = DumpOptions ^ dump_dependency_ordering,
    (
        DumpDepOrder = yes,
        module_info_get_maybe_dependency_info(ModuleInfo, MaybeDependencyInfo),
        (
            MaybeDependencyInfo = no,
            string.builder.append_string("% No dependency info\n\n", !State)
        ;
            MaybeDependencyInfo = yes(DependencyInfo),
            format_dependency_info(Info, ModuleInfo, DependencyInfo, !State)
        )
    ;
        DumpDepOrder = no
    ),
    format_footer(ModuleInfo, !State).

%---------------------------------------------------------------------------%

:- pred format_header(module_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_header(Module, !State) :-
    module_info_get_name(Module, ModuleName),
    string.builder.append_string("% vim: ts=2 sw=2 ft=mercury\n\n", !State),
    string.builder.format(":- module %s.\n\n",
        [s(escaped_sym_name_to_string(ModuleName))], !State).

:- pred format_footer(module_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_footer(Module, !State) :-
    module_info_get_name(Module, ModuleName),
    string.builder.format(":- end_module %s.\n",
        [s(escaped_sym_name_to_string(ModuleName))], !State).

%---------------------------------------------------------------------------%
%
% Write out the imports and uses.
%

:- pred format_avail_entry(module_name::in, avail_module_entry::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_avail_entry(ModuleName, Entry, !State) :-
    Entry = avail_module_entry(Section, ImportOrUse, Avails),
    (
        ImportOrUse = import_decl,
        ImportOrUseDecl = "import_module"
    ;
        ImportOrUse = use_decl,
        ImportOrUseDecl = "use_module"
    ),
    string.builder.format(":- %s %s.\n",
        [s(ImportOrUseDecl), s(escaped_sym_name_to_string(ModuleName))],
        !State),
    string.builder.format("%% %s, %s\n",
        [s(string.string(Section)), s(string.string(Avails))], !State).

%---------------------------------------------------------------------------%
%
% Write out constant structs defined in the module.
%

:- pred format_const_struct_db(const_struct_db::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_const_struct_db(ConstStructDb, !State) :-
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    string.builder.append_string(
        "%-------- Const structs --------\n\n", !State),
    list.foldl(format_const_struct, ConstStructs, !State),
    string.builder.append_string("\n", !State).

:- pred format_const_struct(pair(int, const_struct)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_const_struct(N - ConstStruct, !State) :-
    string.builder.format("\nconst_struct %d:\n", [i(N)], !State),
    ConstStruct = const_struct(ConsId, ConstArgs, Type, Inst, DefinedWhere),
    mercury_format_cons_id(output_debug, does_not_need_brackets, ConsId,
        string.builder.handle, !State),
    (
        ConstArgs = [],
        string.builder.append_string("\n", !State)
    ;
        ConstArgs = [HeadConstArg | TailConstArgs],
        string.builder.append_string("(\n", !State),
        format_const_struct_args(HeadConstArg, TailConstArgs, !State),
        string.builder.append_string(")\n", !State)
    ),
    string.builder.append_string("type: ", !State),
    mercury_format_type(varset.init, print_name_only, Type,
        string.builder.handle, !State),
    string.builder.append_string("\n", !State),
    string.builder.append_string("inst: ", !State),
    mercury_format_structured_inst(output_debug, varset.init, do_not_incl_addr,
        0u, "\n", Inst, string.builder.handle, !State),
    (
        DefinedWhere = defined_in_this_module,
        string.builder.append_string("defined_in_this_module\n", !State)
    ;
        DefinedWhere = defined_in_other_module,
        string.builder.append_string("defined_in_other_module\n", !State)
    ).

:- pred format_const_struct_args(const_struct_arg::in,
    list(const_struct_arg)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_const_struct_args(HeadConstArg, TailConstArgs, !State) :-
    string.builder.append_string("    ", !State),
    (
        HeadConstArg = csa_const_struct(N),
        string.builder.format("cs(%d)", [i(N)], !State)
    ;
        HeadConstArg = csa_constant(ConsId, Type),
        mercury_format_cons_id(output_debug, does_not_need_brackets, ConsId,
            string.builder.handle, !State),
        string.builder.append_string("\n        with type ", !State),
        mercury_format_type(varset.init, print_name_only, Type,
            string.builder.handle, !State)
    ),
    (
        TailConstArgs = [],
        string.builder.append_string("\n", !State)
    ;
        TailConstArgs = [HeadTailConstArg | TailTailConstArgs],
        string.builder.append_string(",\n", !State),
        format_const_struct_args(HeadTailConstArg, TailTailConstArgs, !State)
    ).

%---------------------------------------------------------------------------%
%
% Write out the cons_table.
%
% The tools/cons_table.awk script summarizes the cons tables we write out.
% When run on a file containing all the cons tables from a stage2 directory
% concatenated together, its results as of 2025 july 21 were:
%
% #names =  1729875, #ctors =     1729875, #ctors/name =      1.05, max = 24
% #ctors =  1811508, #synonyms =  1811508, #synonyms/ctor =   3.30, max =  9
%
% The cons_table entry that had the longest list of du_ctors
% for a given name was this one:
%
% DU_CTORS named error: 24
%   FQ_DU_CTOR dir.error/0 for type dir.make_single_directory_status/0: #synonyms = 1
%   FQ_DU_CTOR maybe.error/1 for type maybe.maybe_error/0: #synonyms = 3
%   FQ_DU_CTOR maybe.error/1 for type maybe.maybe_error/2: #synonyms = 3
%   FQ_DU_CTOR maybe.error/2 for type maybe.maybe_errors/2: #synonyms = 3
%   FQ_DU_CTOR io.error/1 for type io.maybe_incomplete_result/1: #synonyms = 3
%   FQ_DU_CTOR getopt.error/1 for type getopt.maybe_option_table/1: #synonyms = 3
%   FQ_DU_CTOR getopt_io.error/1 for type getopt_io.maybe_option_table/1: #synonyms = 3
%   FQ_DU_CTOR getopt.error/1 for type getopt.maybe_option_table_se/1: #synonyms = 3
%   FQ_DU_CTOR getopt_io.error/1 for type getopt_io.maybe_option_table_se/1: #synonyms = 3
%   FQ_DU_CTOR io.error/2 for type io.maybe_partial_res/1: #synonyms = 3
%   FQ_DU_CTOR stream.error/2 for type stream.maybe_partial_res/2: #synonyms = 3
%   FQ_DU_CTOR parsing_utils.error/3 for type parsing_utils.parse_result/1: #synonyms = 3
%   FQ_DU_CTOR io.error/2 for type io.read_result/1: #synonyms = 3
%   FQ_DU_CTOR mercury_term_parser.error/2 for type mercury_term_parser.read_term/1: #synonyms = 3
%   FQ_DU_CTOR io.error/1 for type io.res/0: #synonyms = 3
%   FQ_DU_CTOR io.error/1 for type io.res/1: #synonyms = 3
%   FQ_DU_CTOR stream.error/1 for type stream.res/1: #synonyms = 3
%   FQ_DU_CTOR stream.error/1 for type stream.res/2: #synonyms = 3
%   FQ_DU_CTOR io.error/1 for type io.result/0: #synonyms = 3
%   FQ_DU_CTOR io.error/1 for type io.result/1: #synonyms = 3
%   FQ_DU_CTOR stream.error/1 for type stream.result/1: #synonyms = 3
%   FQ_DU_CTOR stream.error/1 for type stream.result/2: #synonyms = 3
%   FQ_DU_CTOR term_conversion.error/1 for type term_conversion.term_to_type_result/2: #synonyms = 3
%   FQ_DU_CTOR mercury_term_lexer.error/1 for type mercury_term_lexer.token/0: #synonyms = 3
%
% Note that *all* of the names that had 18 or more du_ctors were either
% "error" or "ok".

:- pred format_cons_table(cons_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_cons_table(ConsTable, !State) :-
    get_cons_table_contents(ConsTable, NameMap, DuCtorMap),
    string.builder.append_string("%-------- Cons table --------\n", !State),
    map.foldl(format_cons_table_for_name(DuCtorMap), NameMap, !State),
    string.builder.append_string("\n", !State).

:- pred format_cons_table_for_name(map(du_ctor, list(du_ctor))::in,
    string::in, list(du_ctor)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_cons_table_for_name(DuCtorMap, Name, FqDuCtors, !State) :-
    list.length(FqDuCtors, NumFqDuCtors),
    string.builder.format("\n%% DU_CTORS named %s: %d\n",
        [s(Name), i(NumFqDuCtors)], !State),
    list.foldl(format_cons_table_for_fq_du_ctor(DuCtorMap), FqDuCtors, !State).

:- pred format_cons_table_for_fq_du_ctor(map(du_ctor, list(du_ctor))::in,
    du_ctor::in, string.builder.state::di, string.builder.state::uo) is det.

format_cons_table_for_fq_du_ctor(DuCtorMap, FqDuCtor, !State) :-
    map.lookup(DuCtorMap, FqDuCtor, OtherDuCtors),
    FqDuCtor = du_ctor(DuCtorSymName, DuCtorArity, TypeCtor),
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    DuCtorSymNameStr = sym_name_to_string(DuCtorSymName),
    TypeCtorSymNameStr = sym_name_to_string(TypeCtorSymName),
    list.length(OtherDuCtors, NumOtherDuCtors),
    string.builder.format(
        "%%   FQ_DU_CTOR %s/%d for type %s/%d: #synonyms = %d\n",
        [s(DuCtorSymNameStr), i(DuCtorArity),
        s(TypeCtorSymNameStr), i(TypeCtorArity), i(NumOtherDuCtors)], !State).

%---------------------------------------------------------------------------%
%
% Write out tabling structs defined in the module.
%

:- pred format_table_structs(module_info::in, table_struct_map::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_table_structs(ModuleInfo, TableStructMap, !State) :-
    map.to_assoc_list(TableStructMap, TableStructs),
    string.builder.append_string("%-------- Table structs --------\n", !State),
    list.foldl(format_table_struct_info(ModuleInfo), TableStructs, !State),
    string.builder.append_string("\n", !State).

:- pred format_table_struct_info(module_info::in,
    pair(pred_proc_id, table_struct_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_table_struct_info(ModuleInfo, PredProcId - TableStructInfo,
        !State) :-
    PredProcIdStr = pred_proc_id_to_dev_string(ModuleInfo, PredProcId),
    string.builder.format("\n%% table struct info for %s\n",
        [s(PredProcIdStr)], !State),
    TableStructInfo = table_struct_info(ProcTableStructInfo, Attributes),
    ProcTableStructInfo = proc_table_struct_info(_ProcLabel, TVarSet, _Context,
        NumInputs, NumOutputs, InputSteps, MaybeOutputSteps, ArgInfos,
        _EvalMethod),
    string.builder.format("%% #inputs: %d, #outputs: %d\n",
        [i(NumInputs), i(NumOutputs)], !State),
    string.builder.append_string("% input steps:", !State),
    list.foldl(format_space_and_table_trie_step(TVarSet),
        InputSteps, !State),
    string.builder.append_string("\n", !State),
    (
        MaybeOutputSteps = yes(OutputSteps),
        string.builder.append_string("% output steps:", !State),
        list.foldl(format_space_and_table_trie_step(TVarSet),
            OutputSteps, !State),
        string.builder.append_string("\n", !State)
    ;
        MaybeOutputSteps = no,
        string.builder.append_string("% no output steps", !State)
    ),
    format_table_arg_infos(TVarSet, ArgInfos, !State),

    Attributes = table_attributes(Strictness, SizeLimit, Stats, AllowReset,
        BackendWarning),
    (
        Strictness = cts_all_strict,
        string.builder.append_string("% all strict\n", !State)
    ;
        Strictness = cts_all_fast_loose,
        string.builder.append_string("% all fast_loose\n", !State)
    ;
        Strictness = cts_specified(ArgMethods, HiddenArgMethod),
        string.builder.append_string("% specified [", !State),
        format_arg_tabling_methods("", ArgMethods, !State),
        string.builder.append_string("]", !State),
        (
            HiddenArgMethod = table_hidden_arg_value,
            string.builder.append_string(", hidden args by value\n", !State)
        ;
            HiddenArgMethod = table_hidden_arg_addr,
            string.builder.append_string(", hidden args by addr\n", !State)
        )
    ),
    (
        SizeLimit = no,
        string.builder.append_string("% no size limit\n", !State)
    ;
        SizeLimit = yes(Limit),
        string.builder.format("%% size limit %d\n", [i(Limit)], !State)
    ),
    (
        Stats = table_gather_statistics,
        string.builder.append_string("% gather statistics\n", !State)
    ;
        Stats = table_do_not_gather_statistics,
        string.builder.append_string("% do not gather statistics\n", !State)
    ),
    (
        AllowReset = table_allow_reset,
        string.builder.append_string("% allow reset\n", !State)
    ;
        AllowReset = table_do_not_allow_reset,
        string.builder.append_string("% do not allow reset\n", !State)
    ),
    (
        BackendWarning = table_attr_ignore_with_warning,
        string.builder.append_string("% ignore only with warning\n", !State)
    ;
        BackendWarning = table_attr_ignore_without_warning,
        string.builder.append_string("% may ignore without warning\n", !State)
    ).

:- pred format_space_and_table_trie_step(tvarset::in, table_step_desc::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_space_and_table_trie_step(TVarSet, StepDesc, !State) :-
    StepDesc = table_step_desc(VarName, TrieStep),
    StepDescStr = table_trie_step_desc(TVarSet, TrieStep),
    string.builder.format(" %s: %s", [s(VarName), s(StepDescStr)], !State).

:- func table_trie_step_desc(tvarset, table_trie_step) = string.

table_trie_step_desc(TVarSet, Step) = Str :-
    (
        Step = table_trie_step_int(int_type_int),
        Str = "int"
    ;
        Step = table_trie_step_int(int_type_uint),
        Str = "uint"
    ;
        Step = table_trie_step_int(int_type_int8),
        Str = "int8"
    ;
        Step = table_trie_step_int(int_type_uint8),
        Str = "uint8"
    ;
        Step = table_trie_step_int(int_type_int16),
        Str = "int16"
    ;
        Step = table_trie_step_int(int_type_uint16),
        Str = "uint16"
    ;
        Step = table_trie_step_int(int_type_int32),
        Str = "int32"
    ;
        Step = table_trie_step_int(int_type_uint32),
        Str = "uint32"
    ;
        Step = table_trie_step_int(int_type_int64),
        Str = "int64"
    ;
        Step = table_trie_step_int(int_type_uint64),
        Str = "uint64"
    ;
        Step = table_trie_step_char,
        Str = "char"
    ;
        Step = table_trie_step_string,
        Str = "string"
    ;
        Step = table_trie_step_float,
        Str = "float"
    ;
        Step = table_trie_step_dummy,
        Str = "dummy"
    ;
        Step = table_trie_step_enum(N),
        Str = string.format("enum(%d)", [i(N)])
    ;
        Step = table_trie_step_foreign_enum,
        Str = "foreign_enum"
    ;
        Step = table_trie_step_general(Type, IsPoly, IsAddr),
        TypeStr = mercury_type_to_string(TVarSet, print_name_and_num, Type),
        (
            IsPoly = table_is_poly,
            IsPolyStr = "poly"
        ;
            IsPoly = table_is_mono,
            IsPolyStr = "mono"
        ),
        (
            IsAddr = table_value,
            IsAddrStr = "value"
        ;
            IsAddr = table_addr,
            IsAddrStr = "addr"
        ),
        Str = string.format("general(%s, %s, %s)",
            [s(TypeStr), s(IsPolyStr), s(IsAddrStr)])
    ;
        Step = table_trie_step_typeinfo,
        Str = "typeinfo"
    ;
        Step = table_trie_step_typeclassinfo,
        Str = "typeclassinfo"
    ;
        Step = table_trie_step_promise_implied,
        Str = "promise_implied"
    ).

:- pred format_arg_tabling_methods(string::in,
    list(maybe(arg_tabling_method))::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_arg_tabling_methods(_, [], !State).
format_arg_tabling_methods(Prefix, [MaybeMethod | MaybeMethods], !State) :-
    string.builder.append_string(Prefix, !State),
    (
        MaybeMethod = no,
        string.builder.append_string("output", !State)
    ;
        MaybeMethod = yes(arg_value),
        string.builder.append_string("value", !State)
    ;
        MaybeMethod = yes(arg_addr),
        string.builder.append_string("addr", !State)
    ;
        MaybeMethod = yes(arg_promise_implied),
        string.builder.append_string("promise_implied", !State)
    ),
    format_arg_tabling_methods(", ", MaybeMethods, !State).

%---------------------------------------------------------------------------%
%
% Write out the predicate table.
%

:- pred format_preds(hlds_out_info::in, bool::in, list(string)::in,
    output_lang::in, module_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_preds(Info, DumpSpecPreds, DumpSpecPredTypeNames, Lang,
        ModuleInfo, !State) :-
    string.builder.append_string("%-------- Predicates --------\n\n", !State),
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    map.to_assoc_list(PredIdTable, PredIdsInfos),
    (
        DumpSpecPreds = no,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, dump_hlds_pred_name_order,
            NameOrder),
        (
            NameOrder = no,
            PrintPredIdsInfos = PredIdsInfos
        ;
            NameOrder = yes,
            list.sort(compare_in_name_order, PredIdsInfos, PrintPredIdsInfos)
        )
    ;
        DumpSpecPreds = yes,
        map.init(SpecPredMap0),
        list.foldl(add_spec_preds_to_map(DumpSpecPredTypeNames), PredIdsInfos,
            SpecPredMap0, SpecPredMap),
        map.values(SpecPredMap, PrintPredIdsInfos)
    ),
    list.foldl(maybe_write_pred(Info, Lang, ModuleInfo),
        PrintPredIdsInfos, !State).

:- pred compare_in_name_order(
    pair(pred_id, pred_info)::in,
    pair(pred_id, pred_info)::in,
    comparison_result::out) is det.

compare_in_name_order(PredIdA - PredInfoA, PredIdB - PredInfoB, Result) :-
    pred_info_get_name(PredInfoA, PredNameA),
    pred_info_get_name(PredInfoB, PredNameB),
    compare(NameResult, PredNameA, PredNameB),
    (
        ( NameResult = (<)
        ; NameResult = (>)
        ),
        Result = NameResult
    ;
        NameResult = (=),
        compare(Result, PredIdA, PredIdB)
    ).

:- pred add_spec_preds_to_map(list(string)::in, pair(pred_id, pred_info)::in,
    map({type_ctor, special_pred_id}, pair(pred_id, pred_info))::in,
    map({type_ctor, special_pred_id}, pair(pred_id, pred_info))::out) is det.

add_spec_preds_to_map(DumpSpecPredTypeNames, PredIdInfo, !SpecPredMap) :-
    PredIdInfo = _PredId - PredInfo,
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_compiler(made_for_uci(SpecialPredId, TypeCtor)) then
        (
            DumpSpecPredTypeNames = [],
            map.det_insert({TypeCtor, SpecialPredId}, PredIdInfo, !SpecPredMap)
        ;
            DumpSpecPredTypeNames = [_ | _],
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            TypeCtorName = unqualify_name(TypeCtorSymName),
            ( if list.member(TypeCtorName, DumpSpecPredTypeNames) then
                map.det_insert({TypeCtor, SpecialPredId}, PredIdInfo,
                    !SpecPredMap)
            else
                true
            )
        )
    else
        true
    ).

:- pred maybe_write_pred(hlds_out_info::in, output_lang::in, module_info::in,
    pair(pred_id, pred_info)::in,
    string.builder.state::di, string.builder.state::uo) is det.

maybe_write_pred(Info, Lang, ModuleInfo, PredId - PredInfo, !State) :-
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpPredIdStrs = Info ^ hoi_dump_hlds_pred_ids,
    DumpPredNames = Info ^ hoi_dump_hlds_pred_names,
    pred_id_to_int(PredId, PredIdInt),
    ( if
        % If the user requested one or more predicates/functions to be dumped,
        % we dump them even if the condition of the nested if-then-else below
        % would say they shouldn't be dumped, and we don't dump anything else.
        ( DumpPredIdStrs = [_ | _]
        ; DumpPredNames = [_ | _]
        )
    then
        ( if
            (
                some [DumpPredIdStr, DumpPredId] (
                    list.member(DumpPredIdStr, DumpPredIdStrs),
                    string.to_int(DumpPredIdStr, DumpPredId),
                    PredIdInt = DumpPredId
                )
            ;
                PredName = pred_info_name(PredInfo),
                list.member(PredName, DumpPredNames)
            )
        then
            format_pred(Info, Lang, ModuleInfo, PredId, PredInfo, !State)
        else
            true
        )
    else
        DumpImports = DumpOptions ^ dump_imports,
        DumpUnifyCompare = DumpOptions ^ dump_unify_compare_preds,
        ( if
            (
                DumpImports = no,
                (
                    pred_info_is_imported(PredInfo)
                ;
                    % For pseudo-imported predicates (i.e. unification preds),
                    % only print them if we are using a local mode for them.
                    pred_info_is_pseudo_imported(PredInfo),
                    ProcIds = pred_info_all_procids(PredInfo),
                    hlds_pred.in_in_unification_proc_id(ProcId),
                    ProcIds = [ProcId]
                )
            ;
                DumpUnifyCompare = no,
                is_unify_index_or_compare_pred(PredInfo)
            )
        then
            true
        else
            format_pred(Info, Lang, ModuleInfo, PredId, PredInfo, !State)
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out dependency information.
%

:- pred format_dependency_info(hlds_out_info::in, module_info::in,
    hlds_dependency_info::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_dependency_info(_Info, ModuleInfo, DependencyInfo, !State) :-
    string.builder.append_string("% Dependency graph\n\n", !State),
    Graph = dependency_info_get_graph(DependencyInfo),
    digraph.traverse(Graph,
        format_dep_graph_node(ModuleInfo),
        format_dep_graph_edge(ModuleInfo), !State),

% If needed, this code can be used to check the raw behavior
% of digraph operations.
%
%   ( if tsort(Graph, TSort) then
%       format_indent(Indent, !State),
%       string.builder.append_string("\n% TSORT ordering\n\n", !State),
%       list.foldl(format_dependency_proc(Indent, "", ModuleInfo),
%           TSort, !State)
%   else
%       string.builder.append_string("\n% NO TSORT ordering\n\n", !State)
%   ),
%
%   format_indent(Indent, !State),
%   string.builder.append_string("\n% ATSORT ordering\n\n", !State),
%   AtSort = digraph.atsort(Graph),
%   list.foldl(format_dependency_scc(Indent, ModuleInfo), AtSort, !State),

    string.builder.append_string("\n% Bottom up dependency sccs\n\n", !State),
    Ordering = dependency_info_get_bottom_up_sccs(DependencyInfo),
    list.foldl(format_dependency_scc(ModuleInfo), Ordering, !State).

:- pred format_dep_graph_node(module_info::in, pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_dep_graph_node(ModuleInfo, Proc, !State) :-
    format_dependency_proc("calls from ", ModuleInfo, Proc, !State).

:- pred format_dep_graph_edge(module_info::in,
    pred_proc_id::in, pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_dep_graph_edge(ModuleInfo, _ParentProc, ChildProc, !State) :-
    format_dependency_proc("  to ", ModuleInfo, ChildProc, !State).

:- pred format_dependency_scc(module_info::in, scc::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_dependency_scc(ModuleInfo, SCC, !State) :-
    string.builder.append_string("% SCC\n", !State),
    set.foldl(format_dependency_proc("  ", ModuleInfo), SCC, !State).

:- pred format_dependency_proc(string::in, module_info::in, pred_proc_id::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_dependency_proc(Prefix, ModuleInfo, PredProcId, !State) :-
    PredProcId = proc(PredId, ProcId),
    Pieces = describe_unqual_proc_name(ModuleInfo, PredProcId),
    Desc = error_pieces_to_one_line_string(Pieces),

    string.builder.format("%% %spred %d proc %d, %s\n",
        [s(Prefix), i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId)),
        s(Desc)], !State).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_module.
%---------------------------------------------------------------------------%
