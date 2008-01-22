%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2008 The University of Melbourne.
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

    % Are equivalence types fully expanded on this backend?
    %
:- pred are_equivalence_types_expanded(module_info::in) is semidet.

    % Find out how a function symbol (constructor) is represented
    % in the given type.
    %
:- func cons_id_to_tag(module_info, mer_type, cons_id) = cons_tag.

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
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module map.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

are_equivalence_types_expanded(ModuleInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    HighLevelData = yes,
    globals.get_target(Globals, Target),
    ( Target = target_il
    ; Target = target_java
    ).

%-----------------------------------------------------------------------------%

cons_id_to_tag(ModuleInfo, Type, ConsId) = Tag:-
    (
        ConsId = int_const(I),
        Tag = int_tag(I)
    ;
        ConsId = float_const(F),
        Tag = float_tag(F)
    ;
        ConsId = string_const(S),
        Tag = string_tag(S)
    ;
        ConsId = pred_const(ShroudedPredProcId, EvalMethod),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        Tag = pred_closure_tag(PredId, ProcId, EvalMethod)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeName, Arity),
        Tag = type_ctor_info_tag(ModuleName, TypeName, Arity)
    ;
        ConsId = base_typeclass_info_const(ModuleName, ClassName, _Instance,
            EncodedArgs),
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
        ConsId = cons(Name, Arity),
        (
            % Handle the `character' type specially.
            Type = builtin_type(builtin_type_character),
            Name = unqualified(ConsName),
            string.char_to_string(Char, ConsName)
        ->
            char.to_int(Char, CharCode),
            Tag = int_tag(CharCode)
        ;
            % Tuples do not need a tag. Note that unary tuples are not treated
            % as no_tag types. There's no reason why they couldn't be, it is
            % just not worth the effort.
            type_is_tuple(Type, _)
        ->
            Tag = single_functor_tag
        ;
            type_to_ctor_det(Type, TypeCtor),
            % Given the type_ctor, lookup up the constructor tag table
            % for that type.
            module_info_get_type_table(ModuleInfo, TypeTable),
            map.lookup(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            (
                TypeBody = hlds_du_type(_, ConsTagTable, _, _, _, _, _, _)
            ;
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_solver_type(_, _)
                ; TypeBody = hlds_abstract_type(_)
                ),
                unexpected(this_file, "cons_id_to_tag: type is not d.u. type?")
            ),

            % Finally look up the cons_id in the table.
            map.lookup(ConsTagTable, cons(Name, Arity), Tag)
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
        unexpected(this_file, "type_to_string: invalid type")
    ).

%----------------------------------------------------------------------------%

is_valid_mutable_inst(ModuleInfo, Inst) :-
    set.init(Expansions),
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions).

:- pred is_valid_mutable_inst_2(module_info::in, mer_inst::in,
    set(inst_name)::in) is semidet.

is_valid_mutable_inst_2(_, any(shared, _), _).
is_valid_mutable_inst_2(ModuleInfo, bound(shared, BoundInsts), Expansions) :-
    list.member(bound_functor(_, Insts), BoundInsts),
    list.member(Inst, Insts),
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions).
is_valid_mutable_inst_2(_, ground(shared, _), _).
is_valid_mutable_inst_2(ModuleInfo, defined_inst(InstName), Expansions0) :-
    not set.member(InstName, Expansions0),
    Expansions = set.insert(Expansions0, InstName),
    inst_lookup(ModuleInfo, InstName, Inst),
    is_valid_mutable_inst_2(ModuleInfo, Inst, Expansions).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_code_util.m".

%----------------------------------------------------------------------------%
:- end_module hlds_code_util.
%----------------------------------------------------------------------------%
