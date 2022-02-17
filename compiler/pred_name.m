%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2014-2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pred_name.m.
%
% When we create new predicates (or functions), we need names for them.
% This module creates those names.
%
% - If the new predicate is a transformed version of an old predicate,
%   we create the name out of the name of the original predicate, and
%   the identity and parameters of the transformation.
%
% - If the new predicate is not a transformed version of an old predicate,
%   but is a predicate that implements a method for an instance, or
%   a unify, compare or index (uci) predicate for a type, then we create
%   its name out of the parameters of the instance and the method,
%   or the name of the type.
%
%---------------------------------------------------------------------------%

:- module parse_tree.pred_name.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % For use in cases where we create more than one predicate for the
    % same line, we also include a counter in the name of the transformed
    % predicate.
:- type line_number_and_counter
    --->    lnc(int, int).

:- type aux_tabling_pred_kind
    --->    atpk_statistics
    ;       atpk_reset.

:- type aux_tabling_maybe_single_proc
    --->    is_not_single_proc
    ;       is_single_proc.

    % With three exceptions, all the transform_names specify whether
    % the original predicate is a predicate or a function, because
    % our naming scheme includes this information in the transformed name.
    % (This is needed to avoid accidental collisions between the transformed
    % name of a predicate, and the transformed name of a function.)
    %
    % Two of the exceptions are transformations done by higher_order.m.
    % Although their transform_names include a pred_or_func argument,
    % the name we generate for them ignores this argument. I (zs) can see
    % no reason for this omission, but (a) it cannot be fixed without also
    % fixing name demangling code, and (b) there is not much point in doing
    % such fixes piecemeal.
    %
    % The third exception is tn_pragma_type_spec, which gets its information
    % not from a pred_info (which will *always* tell you whether it contains
    % a predicate or a function), but from a type_spec pragma (which *may*
    % specify predicate vs function, but, for now, it is allowed to stay silent
    % on that matter.
:- type transform_name
    --->    tn_higher_order(pred_or_func, int)
            % The higher order specialization specifies only a counter.

    ;       tn_higher_order_type_spec(pred_or_func, int, int)
            % The predicate calling make_transformed_pred_sym_name with this
            % transform should pass us *not* the name of the predicate
            % being transformed, but the name of the predicate *calling it*
            % at the call site being optimized. The second argument here
            % is the proc_id of this caller predicate. The third argument
            % is the version number of the higher order transformation
            % algorithm.
            %
            % XXX It would be nice to know what the relationship is
            % between tn_pragma_type_spec and tn_higher_order_type_spec.

    ;       tn_aux_tabling(pred_or_func, user_arity, aux_tabling_pred_kind,
                aux_tabling_maybe_single_proc, int)
            % The new predicate name will include the arity of the original
            % predicate, an indication of what kind of aux predicate this is,
            % and, if the procedure being transformed is not the only procedure
            % in its predicate (as indicated by the fourth argument), it will
            % also include the proc_id of the original procedure
            % (in its int form, as given by the fifth argument).

    ;       tn_accumulator(pred_or_func, line_number_and_counter)
    ;       tn_deforestation(pred_or_func, line_number_and_counter)
    ;       tn_lambda(pred_or_func, line_number_and_counter)
            % With the above transforms, the new predicate name includes
            % the line number and a unique counter value.

    ;       tn_loop_inv(pred_or_func, int, line_number_and_counter)
    ;       tn_tupling(pred_or_func, int, line_number_and_counter)
    ;       tn_untupling(pred_or_func, int, line_number_and_counter)
            % With the above transforms, the new predicate name,
            % besides the above, also includes (the integer form of)
            % the proc_id of the transformed procedure. This is because
            % (if relevant) we create a new pred_info for each procedure
            % we transform, even if they come from the same pred_info.
            %
            % XXX We cannot take the proc_ids as proc_ids, rather than ints,
            % as long as we call make_transformed_pred_sym_name from
            % parse_pragma.m, which is in the parse_tree package.
            %
            % XXX Arguably, that call in to make_transformed_pred_sym_name
            % in parse_pragma.m should be moved to add_pragma_type_spec.m,
            % which is inside the hlds package, and thus has access
            % to the proc_id type.

    ;       tn_last_call_modulo_cons(pred_or_func, int)
            % The new predicate names includes a sequentially allocated
            % variant number.
            % XXX A proc_id and a list of the argument numbers of the
            % arguments being passed by address would also work.

    ;       tn_ssdb_stdlib_proxy(pred_or_func)
            % The new predicate name includes only the pred_or_func indication.

    ;       tn_dep_par_conj(pred_or_func, int, list(int))
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument), as well as the list
            % of the argument positions that we transform into futures.

    ;       tn_par_distance_granularity(pred_or_func, int)
            % The new predicate name includes the distance parameter,
            % the main (actually only) parameter of the transformation.

    ;       tn_par_loop_control(pred_or_func, int)
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument).

    ;       tn_structure_reuse(pred_or_func, int, list(int))
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument), as well a list
            % of argument numbers. XXX It would be nice to know just
            % how those arguments were selected.

    ;       tn_pragma_type_spec(maybe(pred_or_func), tvarset, type_subst)
            % The new predicate name includes the type substitution
            % specified by the type_spec pragma, in the form of an assoc_list,
            % each element of which maps a type var to a type. The tvarset
            % argument is there to provide the names of the type vars
            % on both sides of those arrows.

    ;       tn_io_tabling(pred_or_func)
            % The new predicate name includes only the pred_or_func indication.

    ;       tn_minimal_model_generator(pred_or_func, int)
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument).

    ;       tn_stm_expanded(pred_or_func, stm_clone_kind, int, int, int)
            % The new predicate name includes the clone kind, the arity
            % of the original predicate (given by the third argument),
            % the pred_id of the original predicate (given by the fourth
            % argument), and a unique counter value (the last argument).

    ;       tn_unused_args(pred_or_func, int, list(int)).
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument), as well a list
            % of the argument numbers of the unused arguments.

:- type stm_clone_kind
    --->    stmck_top_level
    ;       stmck_rollback
    ;       stmck_wrapper
    ;       stmck_simple_wrapper
    ;       stmck_or_else.

    % make_transformed_pred_sym_name(ModuleName, OrigName, Transform,
    %   TransformedSymName):
    % make_transformed_pred_name(OrigName, Transform, TransformedName):
    %
    % Given the original name of a predicate or function, return the name
    % we want to give to a version of it that has been transformed by
    % Transform.
    %
    % The first version returns the transformed name as a sym_name
    % qualified with ModuleName, because some of our callers want the result
    % in this sym_name form.
    %
:- pred make_transformed_pred_sym_name(module_name::in, string::in,
    transform_name::in, sym_name::out) is det.
:- pred make_transformed_pred_name(string::in,
    transform_name::in, string::out) is det.

%---------------------%

    % Make the name of the introduced pred used to check a particular
    % instance of a particular class method.
    %
    % XXX This isn't quite perfect, I suspect.
    %
:- pred make_instance_method_pred_name(class_id::in,
    sym_name::in, user_arity::in, list(mer_type)::in, string::out) is det.

    % Given a list of types, mangle the names so into a string which
    % identifies them. The types must all have their top level functor
    % bound, with any arguments free variables.
    %
:- pred make_instance_string(list(mer_type)::in, string::out) is det.

%---------------------%

    % Return the predicate name we should use for the given uci (special) pred
    % for the given type_ctor.
    %
:- func uci_pred_name(special_pred_id, type_ctor) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_out.

:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

make_transformed_pred_sym_name(ModuleName, OrigName, Transform,
        TransformedSymName) :-
    make_transformed_pred_name(OrigName, Transform, TransformedName),
    TransformedSymName = qualified(ModuleName, TransformedName).

make_transformed_pred_name(OrigName, Transform, TransformedName) :-
    (
        Transform = tn_higher_order(_PredOrFunc, Counter),
        % XXX Ignoring _PredOrFunc seems to me to be a bug.
        % XXX The string we construct here does not fit into our current
        % naming scheme at all.
        string.format("%s__ho%d", [s(OrigName), i(Counter)], TransformedName)
    ;
        Transform = tn_higher_order_type_spec(_PredOrFunc, ProcNum, Version),
        % As documented above, for this transform, OrigName and ProcNum
        % both belong the caller at the call site being optimized, *not*
        % the procedure being transformed. _PredOrFunc belongs to the
        % predicate being transformed, but it is ignored ...
        % XXX Ignoring _PredOrFunc seems to me to be a bug.
        % XXX The string we construct here does not fit into our current
        % naming scheme at all.
        % XXX As is separating OrigName from the suffix with only a single '_'.
        string.format("%s_%d_%d", [s(OrigName), i(ProcNum), i(Version)],
            TransformedName)
    ;
        Transform = tn_aux_tabling(_PredOrFunc, UserArity, AuxTablingPredKind,
            SingleProc, ProcIdInt),
        % XXX Ignoring _PredOrFunc seems to me to be a bug.
        % XXX The string we construct here does not fit into our current
        % general naming scheme at all. However, the reference manual
        % does require us (in section 20.2) to generate the names that
        % we generate here.
        UserArity = user_arity(UserArityInt),
        ( AuxTablingPredKind = atpk_statistics, KindStr = "statistics"
        ; AuxTablingPredKind = atpk_reset,      KindStr = "reset"
        ),
        (
            SingleProc = is_single_proc,
            string.format("table_%s_for_%s_%d",
                [s(KindStr), s(OrigName), i(UserArityInt)], TransformedName)
        ;
            SingleProc = is_not_single_proc,
            string.format("table_%s_for_%s_%d_%d",
                [s(KindStr), s(OrigName), i(UserArityInt), i(ProcIdInt)],
                TransformedName)
        )
    ;
        Transform =
            tn_stm_expanded(_PredOrFunc, CloneKind, Arity, PredNum, Counter),
        ( CloneKind = stmck_top_level,      CloneKindStr = "top_level"
        ; CloneKind = stmck_rollback,       CloneKindStr = "rollback"
        ; CloneKind = stmck_wrapper,        CloneKindStr = "wrapper"
        ; CloneKind = stmck_simple_wrapper, CloneKindStr = "simple_wrapper"
        ; CloneKind = stmck_or_else,        CloneKindStr = "or_else"
        ),
        % XXX The string we construct here does not fit into our current
        % naming scheme at all, but while stm does not work, this does not
        % matter.
        string.format("StmExpanded_%s_%s_%d_%d_%d",
            [s(CloneKindStr), s(OrigName), i(Arity), i(PredNum), i(Counter)],
            TransformedName)
    ;
        ( Transform = tn_accumulator(_, _)
        ; Transform = tn_deforestation(_, _)
        ; Transform = tn_lambda(_, _)
        ; Transform = tn_loop_inv(_, _, _)
        ; Transform = tn_tupling(_, _, _)
        ; Transform = tn_untupling(_, _, _)
        ; Transform = tn_last_call_modulo_cons(_, _)
        ; Transform = tn_ssdb_stdlib_proxy(_)
        ; Transform = tn_dep_par_conj(_, _, _)
        ; Transform = tn_par_distance_granularity(_, _)
        ; Transform = tn_par_loop_control(_, _)
        ; Transform = tn_structure_reuse(_, _, _)
        ; Transform = tn_pragma_type_spec(_, _, _)
        ; Transform = tn_io_tabling(_)
        ; Transform = tn_minimal_model_generator(_, _)
        ; Transform = tn_unused_args(_, _, _)
        ),
        (
            (
                Transform = tn_accumulator(PredOrFunc, LNC),
                TransformId = "AccFrom"
            ;
                Transform = tn_deforestation(PredOrFunc, LNC),
                TransformId = "DeforestationIn"
            ;
                Transform = tn_lambda(PredOrFunc, LNC),
                TransformId = "IntroducedFrom"
            ),
            string.format("%s__%s",
                [s(TransformId), s(pred_or_func_to_str(PredOrFunc))], Prefix),
            LNC = lnc(Line, Counter),
            string.format("%d__%d", [i(Line), i(Counter)], Suffix)
        ;
            (
                Transform = tn_loop_inv(PredOrFunc, ProcNum, LNC),
                TransformId = "loop_inv"
            ;
                Transform = tn_tupling(PredOrFunc, ProcNum, LNC),
                TransformId = "tupling"
            ;
                Transform = tn_untupling(PredOrFunc, ProcNum, LNC),
                TransformId = "untupling"
            ),
            string.format("%s__%s",
                [s(TransformId), s(pred_or_func_to_str(PredOrFunc))], Prefix),
            LNC = lnc(Line, Counter),
            string.format("%d__%d_%d",
                [i(Line), i(Counter), i(ProcNum)], Suffix)
        ;
            Transform = tn_last_call_modulo_cons(PredOrFunc, VariantNum),
            string.format("LCMC__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            string.format("%i", [i(VariantNum)], Suffix)
        ;
            Transform = tn_ssdb_stdlib_proxy(PredOrFunc),
            string.format("SSDB__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            % XXX Having an empty Suffix leaves Name ending with
            % two consecutive underscores.
            Suffix = ""
        ;
            Transform = tn_dep_par_conj(PredOrFunc, ProcNum, ArgNums),
            string.format("Parallel__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            string.format("%s_%i",
                [s(ints_to_string(ArgNums)), i(ProcNum)], Suffix)
        ;
            Transform = tn_par_distance_granularity(PredOrFunc, Distance),
            string.format("DistanceGranularityFor__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            string.format("%i", [i(Distance)], Suffix)
        ;
            Transform = tn_par_loop_control(PredOrFunc, ProcNum),
            string.format("LoopControl__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            % XXX Starting Suffix with _ leaves Name containing
            % *three* consecutive underscores.
            string.format("_%i", [i(ProcNum)], Suffix)
        ;
            Transform = tn_structure_reuse(PredOrFunc, ProcNum, ArgNums),
            string.format("ctgc__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            % XXX All other Transform values that specify a ProcNum
            % put it at the *end* of the suffix.
            string.format("%i__%s",
                [i(ProcNum), s(ints_to_string(ArgNums))], Suffix)
        ;
            Transform =
                tn_pragma_type_spec(MaybePredOrFunc, VarSet, TypeSubst),
            (
                MaybePredOrFunc = yes(PredOrFunc),
                PredOrFuncStr = pred_or_func_to_str(PredOrFunc)
            ;
                MaybePredOrFunc = no,
                PredOrFuncStr = "pred_or_func"
            ),
            string.format("TypeSpecOf__%s", [s(PredOrFuncStr)], Prefix),
            Suffix = type_subst_to_string(VarSet, TypeSubst)
        ;
            Transform = tn_io_tabling(PredOrFunc),
            string.format("OutlinedForIOTablingFrom__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            % XXX Having an empty Suffix leaves Name ending with
            % two consecutive underscores.
            Suffix = ""
        ;
            Transform = tn_minimal_model_generator(PredOrFunc, ProcNum),
            string.format("GeneratorFor_%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            string.format("%i", [i(ProcNum)], Suffix)
        ;
            Transform = tn_unused_args(PredOrFunc, ProcNum, ArgNums),
            string.format("UnusedArgs__%s",
                [s(pred_or_func_to_str(PredOrFunc))], Prefix),
            string.format("%s_%i",
                [s(ints_to_string(ArgNums)), i(ProcNum)], Suffix)
        ),

        % XXX The format of Suffix depends on Prefix; therefore it should
        % immediately follow Prefix.
        string.format("%s__%s__%s",
            [s(Prefix), s(OrigName), s(Suffix)], TransformedName)
    ).

%---------------------------------------------------------------------------%

make_instance_method_pred_name(ClassId, MethodName, UserArity, InstanceTypes,
        PredName) :-
    ClassId = class_id(ClassName, _ClassArity),
    ClassNameStr = sym_name_to_string_sep(ClassName, "__"),
    MethodNameStr = sym_name_to_string_sep(MethodName, "__"),
    % Perhaps we should include the arity in this mangled string?
    make_instance_string(InstanceTypes, InstanceStr),
    UserArity = user_arity(UserArityInt),
    string.format("ClassMethod_for_%s____%s____%s_%d",
        [s(ClassNameStr), s(InstanceStr), s(MethodNameStr), i(UserArityInt)],
        PredName).

make_instance_string(InstanceTypes, InstanceStr) :-
    % Note that for historical reasons, builtin types are treated as being
    % unqualified (`int') rather than being qualified (`builtin.int')
    % at this point.
    list.map(instance_type_ctor_to_string, InstanceTypes, InstanceStrs),
    string.append_list(InstanceStrs, InstanceStr).

:- pred instance_type_ctor_to_string(mer_type::in, string::out) is det.

instance_type_ctor_to_string(Type, Str) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeName, TypeArity),
    TypeNameStr = sym_name_to_string_sep(TypeName, "__"),
    string.format("%s__arity%i__", [s(TypeNameStr), i(TypeArity)], Str).

%---------------------------------------------------------------------------%

uci_pred_name(SpecialPred, type_ctor(SymName, Arity)) = Name :-
    BaseName = get_special_pred_id_target_name(SpecialPred),
    % XXX The name demanglers don't yet understand predicate names for
    % uci preds that have the type name and arity appended, and the
    % hand-written unify/compare predicates for builtin types such as
    % typeinfo have the plain base names. Therefore replacing semidet_fail
    % with semidet_succeed here is useful only for debugging the compiler.
    ( if semidet_fail then
        Name = BaseName ++ sym_name_to_string(SymName)
            ++ "/" ++ int_to_string(Arity)
    else
        Name = BaseName
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func type_subst_to_string(tvarset, type_subst) = string.

type_subst_to_string(VarSet, TypeSubst) = Str :-
    TVarStrs = list.map(type_var_subst_to_string(VarSet), TypeSubst),
    % XXX The use of , and [] here *requires* mangling the names
    % we construct.
    TVarsStr = string.join_list(", ", TVarStrs),
    string.format("[%s]", [s(TVarsStr)], Str).

:- func type_var_subst_to_string(tvarset, pair(tvar, mer_type)) = string.

type_var_subst_to_string(VarSet, Var - Type) = Str :-
    varset.lookup_name(VarSet, Var, VarName),
    TypeStr = mercury_type_to_string(VarSet, print_name_only, Type),
    % XXX The use of = here *requires* mangling the names we construct.
    string.format("%s = %s", [s(VarName), s(TypeStr)], Str).

%---------------------------------------------------------------------------%

:- func ints_to_string(list(int)) = string.

ints_to_string(Ints) = Str :-
    IntStrs = list.map(string.int_to_string, Ints),
    % XXX The use of , and [] here *requires* mangling the names
    % we construct.
    IntsStr = string.join_list(", ", IntStrs),
    string.format("[%s]", [s(IntsStr)], Str).

%---------------------------------------------------------------------------%
:- end_module parse_tree.pred_name.
%---------------------------------------------------------------------------%
