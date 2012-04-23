%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: hlds_code_util.m.
% 
% Various utilities routines for use during HLDS generation.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_code_util.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Find out how a function symbol (constructor) is represented
    % in the given type.
    %
:- func cons_id_to_tag(module_info, cons_id) = cons_tag.

    % Given a list of types, mangle the names so into a string which
    % identifies them. The types must all have their top level functor
    % bound, with any arguments free variables.
    %
:- pred make_instance_string(list(mer_type)::in, string::out) is det.

    % Succeeds iff this inst is one that can be used in a valid
    % mutable declaration.
    %
:- pred is_valid_mutable_inst(module_info::in, mer_inst::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

cons_id_to_tag(ModuleInfo, ConsId) = Tag:-
    (
        ConsId = int_const(Int),
        Tag = int_tag(Int)
    ;
        ConsId = float_const(Float),
        Tag = float_tag(Float)
    ;
        ConsId = char_const(Char),
        char.to_int(Char, CharCode),
        Tag = int_tag(CharCode)
    ;
        ConsId = string_const(String),
        Tag = string_tag(String)
    ;
        ConsId = impl_defined_const(_),
        unexpected($module, $pred, "implementation_defined_const")
    ;
        ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = closure_tag(PredId, ProcId, EvalMethod)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
        Tag = type_ctor_info_tag(ModuleName, TypeName, Arity)
    ;
        ConsId = base_typeclass_info_const(ModuleName, ClassName,
            _Instance, EncodedArgs),
        Tag = base_typeclass_info_tag(ModuleName, ClassName, EncodedArgs)
    ;
        ( ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ),
        Tag = unshared_tag(0)
    ;
        ConsId = tabling_info_const(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = tabling_info_tag(PredId, ProcId)
    ;
        ConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = deep_profiling_proc_layout_tag(PredId, ProcId)
    ;
        ConsId = table_io_decl(ShroudedPredProcId),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = table_io_decl_tag(PredId, ProcId)
    ;
        ConsId = tuple_cons(_Arity),
        % Tuples do not need a tag. Note that unary tuples are not treated
        % as no_tag types. There is no reason why they couldn't be, it is
        % just not worth the effort.
        Tag = single_functor_tag
    ;
        ConsId = cons(_Name, _Arity, TypeCtor),
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_du_type(_, ConsTagTable, _, _, _, _, _, _, _),
            map.lookup(ConsTagTable, ConsId, Tag)
        ;
            ( TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_foreign_type(_)
            ; TypeBody = hlds_solver_type(_, _)
            ; TypeBody = hlds_abstract_type(_)
            ),
            unexpected($module, $pred, "type is not d.u. type")
        )
    ).

%-----------------------------------------------------------------------------%

make_instance_string(InstanceTypes, InstanceString) :-
    % Note that for historical reasons, builtin types are treated as being
    % unqualified (`int') rather than being qualified (`builtin.int')
    % at this point.
    list.map(type_to_string, InstanceTypes, InstanceStrings),
    string.append_list(InstanceStrings, InstanceString).

:- pred type_to_string(mer_type::in, string::out) is det.

type_to_string(Type, String) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        TypeCtor = type_ctor(TypeName, TypeArity),
        TypeNameString = sym_name_to_string_sep(TypeName, "__"),
        string.int_to_string(TypeArity, TypeArityString),
        String = TypeNameString ++ "__arity" ++ TypeArityString ++ "__"
    ;
        unexpected($module, $pred, "invalid type")
    ).

%----------------------------------------------------------------------------%

is_valid_mutable_inst(ModuleInfo, Inst) :-
    set.init(Expansions),
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions).

:- pred is_valid_mutable_inst_2(module_info::in, mer_inst::in,
    set(inst_name)::in) is semidet.

is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions0) :-
    (
        ( Inst = any(Uniq, _)
        ; Inst = ground(Uniq, _)
        ),
        Uniq = shared
    ;
        Inst = bound(shared, _, BoundInsts),
        % XXX This looks wrong to me. It says that Inst is a valid mutable inst
        % if SOME ArgInst inside Inst is a valid mutable inst, whereas I think
        % ALL arguments of ALL functors should be required to be valid mutable
        % insts.
        list.member(bound_functor(_, ArgInsts), BoundInsts),
        list.member(ArgInst, ArgInsts),
        is_valid_mutable_inst_2(ModuleInfo, ArgInst, Expansions0)
    ;
        Inst = defined_inst(InstName),
        not set.member(InstName, Expansions0),
        set.insert(InstName, Expansions0, Expansions),
        inst_lookup(ModuleInfo, InstName, SubInst),
        is_valid_mutable_inst_2(ModuleInfo, SubInst, Expansions)
    ).

%----------------------------------------------------------------------------%
:- end_module hlds_code_util.
%----------------------------------------------------------------------------%
