%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
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
% - The write_preds predicate calls module_info_get_pred_id_table,
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
%   of its pred_info. And likewide for other entities that contain types,
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
%   empty, but if the prgrammer writes pred_id_table, then, when replacing it
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
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module digraph.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out (selected parts of) the entire HLDS.
%

write_hlds(Stream, ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_id,
        DumpPredIdStrs),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_name,
        DumpPredNames),
    globals.lookup_bool_option(Globals, dump_hlds_spec_preds, DumpSpecPreds0),
    globals.lookup_accumulating_option(Globals, dump_hlds_spec_preds_for,
        DumpSpecPredTypeNames),
    write_header(Stream, ModuleInfo, !IO),
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
        % Print unify (and compare and index) predicates.
        DumpOptions = DumpOptions0 ++ "U"
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
        ( if string.contains_char(DumpOptions, 'I') then
            module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
            map.foldl(write_avail_entry(Stream), AvailModuleMap, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'T') then
            ( if string.contains_char(DumpOptions, 'L') then
                LocalOnly = yes
            else
                LocalOnly = no
            ),
            module_info_get_type_table(ModuleInfo, TypeTable),
            module_info_get_instance_table(ModuleInfo, InstanceTable),
            module_info_get_class_table(ModuleInfo, ClassTable),
            write_type_table(Info, Stream, LocalOnly, TypeTable, !IO),
            write_classes(Info, Stream, ClassTable, !IO),
            write_instances(Info, Stream, InstanceTable, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'M') then
            module_info_get_inst_table(ModuleInfo, InstTable),
            module_info_get_mode_table(ModuleInfo, ModeTable),
            globals.lookup_int_option(Globals, dump_hlds_inst_limit,
                InstLimit),
            write_inst_table(Stream, Lang, InstLimit, InstTable, !IO),
            write_mode_table(Stream, ModeTable, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'Z') then
            module_info_get_table_struct_map(ModuleInfo, TableStructMap),
            write_table_structs(Stream, ModuleInfo, TableStructMap, !IO)
        else
            true
        )
    ),
    ( if string.contains_char(DumpOptions, 'X') then
        module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
        write_const_struct_db(Stream, ConstStructDb, !IO)
    else
        true
    ),
    ( if
        ( string.contains_char(DumpOptions, 'x')
        ; DumpSpecPreds = yes
        )
    then
        write_preds(Info, Stream, DumpSpecPreds, DumpSpecPredTypeNames,
            Lang, ModuleInfo, !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'O') then
        module_info_get_maybe_dependency_info(ModuleInfo, MaybeDependencyInfo),
        (
            MaybeDependencyInfo = no,
            io.write_string(Stream, "% No dependency info\n\n", !IO)
        ;
            MaybeDependencyInfo = yes(DependencyInfo),
            write_dependency_info(Info, Stream, ModuleInfo,
                DependencyInfo, !IO)
        )
    else
        true
    ),
    write_footer(Stream, ModuleInfo, !IO).

%---------------------------------------------------------------------------%

:- pred write_header(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

write_header(Stream, Module, !IO) :-
    module_info_get_name(Module, ModuleName),
    io.write_string(Stream, "% vim: ts=2 sw=2 ft=mercury\n\n", !IO),
    io.format(Stream, ":- module %s.\n\n",
        [s(sym_name_to_escaped_string(ModuleName))], !IO).

:- pred write_footer(io.text_output_stream::in, module_info::in,
    io::di, io::uo) is det.

write_footer(Stream, Module, !IO) :-
    module_info_get_name(Module, ModuleName),
    io.format(Stream, ":- end_module %s.\n",
        [s(sym_name_to_escaped_string(ModuleName))], !IO).

%---------------------------------------------------------------------------%
%
% Write out the imports and uses.
%

:- pred write_avail_entry(io.text_output_stream::in, module_name::in,
    avail_module_entry::in, io::di, io::uo) is det.

write_avail_entry(Stream, ModuleName, Entry, !IO) :-
    Entry = avail_module_entry(Section, ImportOrUse, Avails),
    (
        ImportOrUse = import_decl,
        ImportOrUseDecl = "import_module"
    ;
        ImportOrUse = use_decl,
        ImportOrUseDecl = "use_module"
    ),
    io.format(Stream, ":- %s %s.\n",
        [s(ImportOrUseDecl), s(sym_name_to_escaped_string(ModuleName))], !IO),
    io.write_string(Stream, "% ", !IO),
    io.write(Stream, Section, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_line(Stream, Avails, !IO).

%---------------------------------------------------------------------------%
%
% Write out constant structs defined in the module.
%

:- pred write_const_struct_db(io.text_output_stream::in, const_struct_db::in,
    io::di, io::uo) is det.

write_const_struct_db(Stream, ConstStructDb, !IO) :-
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    io.write_string(Stream, "%-------- Const structs --------\n\n", !IO),
    list.foldl(write_const_struct(Stream), ConstStructs, !IO),
    io.nl(Stream, !IO).

:- pred write_const_struct(io.text_output_stream::in,
    pair(int, const_struct)::in, io::di, io::uo) is det.

write_const_struct(Stream, N - ConstStruct, !IO) :-
    io.format(Stream, "\nconst_struct %d:\n", [i(N)], !IO),
    ConstStruct = const_struct(ConsId, ConstArgs, Type, Inst, DefinedWhere),
    mercury_output_cons_id(output_debug, does_not_need_brackets, ConsId,
        Stream, !IO),
    (
        ConstArgs = [],
        io.nl(Stream, !IO)
    ;
        ConstArgs = [HeadConstArg | TailConstArgs],
        io.write_string(Stream, "(\n", !IO),
        write_const_struct_args(Stream, HeadConstArg, TailConstArgs, !IO),
        io.write_string(Stream, ")\n", !IO)
    ),
    io.write_string(Stream, "type: ", !IO),
    mercury_output_type(varset.init, print_name_only, Type, Stream, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "inst: ", !IO),
    mercury_output_structured_inst(Stream, Inst, 0, output_debug,
        do_not_incl_addr, varset.init, !IO),
    (
        DefinedWhere = defined_in_this_module,
        io.write_string(Stream, "defined_in_this_module\n", !IO)
    ;
        DefinedWhere = defined_in_other_module,
        io.write_string(Stream, "defined_in_other_module\n", !IO)
    ).

:- pred write_const_struct_args(io.text_output_stream::in,
    const_struct_arg::in, list(const_struct_arg)::in, io::di, io::uo) is det.

write_const_struct_args(Stream, HeadConstArg, TailConstArgs, !IO) :-
    io.write_string(Stream, "    ", !IO),
    (
        HeadConstArg = csa_const_struct(N),
        io.format(Stream, "cs(%d)", [i(N)], !IO)
    ;
        HeadConstArg = csa_constant(ConsId, Type),
        mercury_output_cons_id(output_debug, does_not_need_brackets, ConsId,
            Stream, !IO),
        io.write_string(Stream, "\n        with type ", !IO),
        mercury_output_type(varset.init, print_name_only, Type, Stream, !IO)
    ),
    (
        TailConstArgs = [],
        io.write_string(Stream, "\n", !IO)
    ;
        TailConstArgs = [HeadTailConstArg | TailTailConstArgs],
        io.write_string(Stream, ",\n", !IO),
        write_const_struct_args(Stream,
            HeadTailConstArg, TailTailConstArgs, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out tabling structs defined in the module.
%

:- pred write_table_structs(io.text_output_stream::in, module_info::in,
    table_struct_map::in, io::di, io::uo) is det.

write_table_structs(Stream, ModuleInfo, TableStructMap, !IO) :-
    map.to_assoc_list(TableStructMap, TableStructs),
    io.write_string(Stream, "%-------- Table structs --------\n", !IO),
    list.foldl(write_table_struct_info(Stream, ModuleInfo), TableStructs, !IO),
    io.nl(Stream, !IO).

:- pred write_table_struct_info(io.text_output_stream::in, module_info::in,
    pair(pred_proc_id, table_struct_info)::in, io::di, io::uo) is det.

write_table_struct_info(Stream, ModuleInfo, PredProcId - TableStructInfo,
        !IO) :-
    PredProcIdStr = pred_proc_id_to_string(ModuleInfo, PredProcId),
    io.format(Stream, "\n%% table struct info for %s\n",
        [s(PredProcIdStr)], !IO),
    TableStructInfo = table_struct_info(ProcTableStructInfo, Attributes),
    ProcTableStructInfo = proc_table_struct_info(_ProcLabel, TVarSet, _Context,
        NumInputs, NumOutputs, InputSteps, MaybeOutputSteps, ArgInfos,
        _EvalMethod),
    io.format(Stream, "%% #inputs: %d, #outputs: %d\n",
        [i(NumInputs), i(NumOutputs)], !IO),
    io.write_string(Stream, "% input steps:", !IO),
    list.foldl(write_space_and_table_trie_step(Stream, TVarSet),
        InputSteps, !IO),
    io.nl(Stream, !IO),
    (
        MaybeOutputSteps = yes(OutputSteps),
        io.write_string(Stream, "% output steps:", !IO),
        list.foldl(write_space_and_table_trie_step(Stream, TVarSet),
            OutputSteps, !IO),
        io.nl(Stream, !IO)
    ;
        MaybeOutputSteps = no,
        io.write_string(Stream, "% no output steps", !IO)
    ),
    write_table_arg_infos(Stream, TVarSet, ArgInfos, !IO),

    Attributes = table_attributes(Strictness, SizeLimit, Stats, AllowReset,
        BackendWarning),
    (
        Strictness = cts_all_strict,
        io.write_string(Stream, "% all strict\n", !IO)
    ;
        Strictness = cts_all_fast_loose,
        io.write_string(Stream, "% all fast_loose\n", !IO)
    ;
        Strictness = cts_specified(ArgMethods, HiddenArgMethod),
        io.write_string(Stream, "% specified [", !IO),
        write_arg_tabling_methods(Stream, "", ArgMethods, !IO),
        io.write_string(Stream, "]", !IO),
        (
            HiddenArgMethod = table_hidden_arg_value,
            io.write_string(Stream, ", hidden args by value\n", !IO)
        ;
            HiddenArgMethod = table_hidden_arg_addr,
            io.write_string(Stream, ", hidden args by addr\n", !IO)
        )
    ),
    (
        SizeLimit = no,
        io.write_string(Stream, "% no size limit\n", !IO)
    ;
        SizeLimit = yes(Limit),
        io.format(Stream, "%% size limit %d\n", [i(Limit)], !IO)
    ),
    (
        Stats = table_gather_statistics,
        io.write_string(Stream, "% gather statistics\n", !IO)
    ;
        Stats = table_dont_gather_statistics,
        io.write_string(Stream, "% do not gather statistics\n", !IO)
    ),
    (
        AllowReset = table_allow_reset,
        io.write_string(Stream, "% allow reset\n", !IO)
    ;
        AllowReset = table_dont_allow_reset,
        io.write_string(Stream, "% do not allow reset\n", !IO)
    ),
    (
        BackendWarning = table_attr_ignore_with_warning,
        io.write_string(Stream, "% ignore only with warning\n", !IO)
    ;
        BackendWarning = table_attr_ignore_without_warning,
        io.write_string(Stream, "% may ignore without warning\n", !IO)
    ).

:- pred write_space_and_table_trie_step(io.text_output_stream::in, tvarset::in,
    table_step_desc::in, io::di, io::uo) is det.

write_space_and_table_trie_step(Stream, TVarSet, StepDesc, !IO) :-
    StepDesc = table_step_desc(VarName, TrieStep),
    StepDescStr = table_trie_step_desc(TVarSet, TrieStep),
    io.format(Stream, " %s: %s", [s(VarName), s(StepDescStr)], !IO).

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

:- pred write_arg_tabling_methods(io.text_output_stream::in, string::in,
    list(maybe(arg_tabling_method))::in, io::di, io::uo) is det.

write_arg_tabling_methods(_, _, [], !IO).
write_arg_tabling_methods(Stream, Prefix, [MaybeMethod | MaybeMethods], !IO) :-
    io.write_string(Stream, Prefix, !IO),
    (
        MaybeMethod = no,
        io.write_string(Stream, "output", !IO)
    ;
        MaybeMethod = yes(arg_value),
        io.write_string(Stream, "value", !IO)
    ;
        MaybeMethod = yes(arg_addr),
        io.write_string(Stream, "addr", !IO)
    ;
        MaybeMethod = yes(arg_promise_implied),
        io.write_string(Stream, "promise_implied", !IO)
    ),
    write_arg_tabling_methods(Stream, ", ", MaybeMethods, !IO).

%---------------------------------------------------------------------------%
%
% Write out the predicate table.
%

:- pred write_preds(hlds_out_info::in, io.text_output_stream::in,
    bool::in, list(string)::in, output_lang::in, module_info::in,
    io::di, io::uo) is det.

write_preds(Info, Stream, DumpSpecPreds, DumpSpecPredTypeNames, Lang,
        ModuleInfo, !IO) :-
    io.write_string(Stream, "%-------- Predicates --------\n\n", !IO),
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
    list.foldl(maybe_write_pred(Info, Stream, Lang, ModuleInfo),
        PrintPredIdsInfos, !IO).

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
    ( if Origin = origin_special_pred(SpecialPredId, TypeCtor) then
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

:- pred maybe_write_pred(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, module_info::in, pair(pred_id, pred_info)::in,
    io::di, io::uo) is det.

maybe_write_pred(Info, Stream, Lang, ModuleInfo, PredId - PredInfo,
        !IO) :-
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
            write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO)
        else
            true
        )
    else
        ( if
            (
                not string.contains_char(DumpOptions, 'I'),
                pred_info_is_imported(PredInfo)
            ;
                % For pseudo-imported predicates (i.e. unification preds),
                % only print them if we are using a local mode for them.
                not string.contains_char(DumpOptions, 'I'),
                pred_info_is_pseudo_imported(PredInfo),
                ProcIds = pred_info_all_procids(PredInfo),
                hlds_pred.in_in_unification_proc_id(ProcId),
                ProcIds = [ProcId]
            ;
                % We dump unification and other compiler-generated special
                % predicates if suboption 'U' is on. We don't need that
                % information to understand how the program has been
                % transformed.
                not string.contains_char(DumpOptions, 'U'),
                is_unify_index_or_compare_pred(PredInfo)
            )
        then
            true
        else
            write_pred(Info, Stream, Lang, ModuleInfo, PredId, PredInfo, !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out dependency information.
%

:- pred write_dependency_info(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, hlds_dependency_info::in, io::di, io::uo) is det.

write_dependency_info(_Info, Stream, ModuleInfo, DependencyInfo, !IO) :-
    io.write_string(Stream, "% Dependency graph\n\n", !IO),
    Graph = dependency_info_get_graph(DependencyInfo),
    digraph.traverse(Graph,
        write_dep_graph_node(Stream, ModuleInfo),
        write_dep_graph_edge(Stream, ModuleInfo), !IO),

% If needed, this code can be used to check the raw behavior
% of digraph operations.
%
%   ( if tsort(Graph, TSort) then
%       write_indent(Stream, Indent, !IO),
%       io.write_string(Stream, "\n% TSORT ordering\n\n", !IO),
%       list.foldl(write_dependency_proc(Indent, "", ModuleInfo), TSort, !IO)
%   else
%       io.write_string(Stream, "\n% NO TSORT ordering\n\n", !IO)
%   ),
%
%   write_indent(Stream, Indent, !IO),
%   io.write_string(Stream, "\n% ATSORT ordering\n\n", !IO),
%   AtSort = digraph.atsort(Graph),
%   list.foldl(write_dependency_scc(Indent, ModuleInfo), AtSort, !IO),

    io.write_string(Stream, "\n% Bottom up dependency sccs\n\n", !IO),
    Ordering = dependency_info_get_bottom_up_sccs(DependencyInfo),
    list.foldl(write_dependency_scc(Stream, ModuleInfo), Ordering, !IO).

:- pred write_dep_graph_node(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, io::di, io::uo) is det.

write_dep_graph_node(Stream, ModuleInfo, Proc, !IO) :-
    write_dependency_proc(Stream, "calls from ", ModuleInfo, Proc, !IO).

:- pred write_dep_graph_edge(io.text_output_stream::in, module_info::in,
    pred_proc_id::in, pred_proc_id::in, io::di, io::uo) is det.

write_dep_graph_edge(Stream, ModuleInfo, _ParentProc, ChildProc, !IO) :-
    write_dependency_proc(Stream, "  to ", ModuleInfo, ChildProc, !IO).

:- pred write_dependency_scc(io.text_output_stream::in, module_info::in,
    scc::in, io::di, io::uo) is det.

write_dependency_scc(Stream, ModuleInfo, SCC, !IO) :-
    io.write_string(Stream, "% SCC\n", !IO),
    set.foldl(write_dependency_proc(Stream, "  ", ModuleInfo), SCC, !IO).

:- pred write_dependency_proc(io.text_output_stream::in, string::in,
    module_info::in, pred_proc_id::in, io::di, io::uo) is det.

write_dependency_proc(Stream, Prefix, ModuleInfo, PredProcId, !IO) :-
    PredProcId = proc(PredId, ProcId),
    Pieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, PredProcId),
    Desc = error_pieces_to_string(Pieces),

    io.format(Stream, "%% %spred %d proc %d, %s\n",
        [s(Prefix), i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId)),
        s(Desc)], !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_module.
%---------------------------------------------------------------------------%
