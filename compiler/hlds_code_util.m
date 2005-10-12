%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: hlds_code_util.m.
%
% Various utilities routines for use during hlds generation.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds__hlds_code_util.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module parse_tree__prog_data.

:- import_module list.

    % Are equivalence types fully expanded on this backend?
    %
:- pred are_equivalence_types_expanded(module_info::in) is semidet.

    % Find out how a function symbol (constructor) is represented
    % in the given type.
    %
:- func cons_id_to_tag(cons_id, type, module_info) = cons_tag.

    % Given a list of types, mangle the names so into a string which
    % identifies them. The types must all have their top level functor
    % bound, with any arguments free variables.
    %
:- pred make_instance_string(list(type)::in, string::out) is det.

:- implementation.

:- import_module check_hlds__type_util.
:- import_module hlds__hlds_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_type.

:- import_module bool.
:- import_module char.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

are_equivalence_types_expanded(ModuleInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals__lookup_bool_option(Globals, highlevel_data, HighLevelData),
    HighLevelData = yes,
    globals__get_target(Globals, Target),
    ( Target = il
    ; Target = java
    ).

%-----------------------------------------------------------------------------%

cons_id_to_tag(int_const(I), _, _) = int_constant(I).
cons_id_to_tag(float_const(F), _, _) = float_constant(F).
cons_id_to_tag(string_const(S), _, _) = string_constant(S).
cons_id_to_tag(pred_const(ShroudedPredProcId, EvalMethod), _, _) =
        pred_closure_tag(PredId, ProcId, EvalMethod) :-
    proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId).
cons_id_to_tag(type_ctor_info_const(M,T,A), _, _) =
        type_ctor_info_constant(M,T,A).
cons_id_to_tag(base_typeclass_info_const(M,C,_,N), _, _) =
        base_typeclass_info_constant(M,C,N).
cons_id_to_tag(type_info_cell_constructor(_), _, _) = unshared_tag(0).
cons_id_to_tag(typeclass_info_cell_constructor, _, _) = unshared_tag(0).
cons_id_to_tag(tabling_pointer_const(ShroudedPredProcId), _, _) =
        tabling_pointer_constant(PredId, ProcId) :-
    proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId).
cons_id_to_tag(deep_profiling_proc_layout(ShroudedPredProcId), _, _) =
        deep_profiling_proc_layout_tag(PredId, ProcId) :-
    proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId).
cons_id_to_tag(table_io_decl(ShroudedPredProcId), _, _) =
        table_io_decl_tag(PredId, ProcId) :-
    proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId).
cons_id_to_tag(cons(Name, Arity), Type, ModuleInfo) = Tag :-
    (
        % Handle the `character' type specially.
        Type = builtin(character),
        Name = unqualified(ConsName),
        string__char_to_string(Char, ConsName)
    ->
        char__to_int(Char, CharCode),
        Tag = int_constant(CharCode)
    ;
        % Tuples do not need a tag. Note that unary tuples are not treated
        % as no_tag types. There's no reason why they couldn't be, it's just
        % not worth the effort.
        type_is_tuple(Type, _)
    ->
        Tag = single_functor
    ;
        % Use the type to determine the type_ctor.
        ( type_to_ctor_and_args(Type, TypeCtor0, _) ->
            TypeCtor = TypeCtor0
        ;
            % The type-checker should ensure that this never happens.
            unexpected(this_file, "cons_id_to_tag: invalid type")
        ),

        % Given the type_ctor, lookup up the constructor tag table
        % for that type.
        module_info_get_type_table(ModuleInfo, TypeTable),
        map__lookup(TypeTable, TypeCtor, TypeDefn),
        hlds_data__get_type_defn_body(TypeDefn, TypeBody),
        ( ConsTable0 = TypeBody ^ du_type_cons_tag_values ->
            ConsTable = ConsTable0
        ;
            unexpected(this_file, "cons_id_to_tag: type is not d.u. type?")
        ),

        % Finally look up the cons_id in the table.
        map__lookup(ConsTable, cons(Name, Arity), Tag)
    ).

%-----------------------------------------------------------------------------%

make_instance_string(InstanceTypes, InstanceString) :-
    % Note that for historical reasons, builtin types are treated as being
    % unqualified (`int') rather than being qualified (`builtin.int')
    % at this point.
    list__map(type_to_string, InstanceTypes, InstanceStrings),
    string__append_list(InstanceStrings, InstanceString).

:- pred type_to_string((type)::in, string::out) is det.

type_to_string(Type, String) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        TypeCtor = TypeName - TypeArity,
        sym_name_to_string(TypeName, "__", TypeNameString),
        string__int_to_string(TypeArity, TypeArityString),
        String = TypeNameString ++ "__arity" ++ TypeArityString ++ "__"
    ;
        unexpected(this_file, "type_to_string: invalid type")
    ).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_code_util.m".

%----------------------------------------------------------------------------%
