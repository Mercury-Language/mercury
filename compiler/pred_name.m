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
% When we create transformed versions of predicates and functions, we need
% names for them. This module creates those names out of the names of the
% original predicate or function, and the identity and parameters of the
% transformation.
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

    % With one exception, all the transform_names specify whether
    % the original predicate is a predicate or a function, because
    % our naming scheme includes this information in the transformed name.
    % (This is needed to avoid accidental collisions between the transformed
    % name of a predicate, and the transformed name of a function.)
    %
    % The only exception is tn_type_spec, which gets its information
    % not from a pred_info (which will *always* tell you whether it contains
    % a predicate or a function), but from a type_spec pragma (which *may*
    % specify predicate vs function, but, for now, it is allowed to stay silent
    % on that matter.
:- type transform_name
    --->    tn_accumulator(pred_or_func, line_number_and_counter)
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
            % as long as we call make_pred_name from parse_pragma.m,
            % which is in the parse_tree package.
            %
            % XXX Arguably, that call in to make_pred_name in parse_pragma.m
            % should be moved to add_pragma_type_spec.m, which is inside
            % the hlds package, and thus has access to the proc_id type.

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

    ;       tn_type_spec(maybe(pred_or_func), tvarset, type_subst)
            % The new predicate name includes the type substitution
            % specified by the type_spec pragma, in the form of an assoc_list,
            % each element of which maps a type var to a type. The tvarset
            % argument is there to provide the names of the type vars
            % on both sides of those arrows.

    ;       tn_unused_args(pred_or_func, int, list(int)).
            % The new predicate name includes the proc_id of the original
            % predicate (given by the second argument), as well a list
            % of the argument numbers of the unused arguments.

    % make_pred_name(ModuleName, OrigName, Transform, TransformedSymName):
    %
    % Given the original name of a predicate or function, return the name
    % we want to give to a version of it that has been transformed by
    % Transform.
    %
    % We return the transformed name as a sym_name qualified with ModuleName,
    % because pretty much all our callers want the result in this sym_name
    % form.
    %
:- pred make_pred_name(module_name::in, string::in, transform_name::in,
    sym_name::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.

:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

make_pred_name(ModuleName, OrigName, Transform, TransformedSymName) :-
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
        string.format("%d__%d_%d", [i(Line), i(Counter), i(ProcNum)], Suffix)
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
        Transform = tn_type_spec(MaybePredOrFunc, VarSet, TypeSubst),
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
        Transform = tn_unused_args(PredOrFunc, ProcNum, ArgNums),
        string.format("UnusedArgs__%s",
            [s(pred_or_func_to_str(PredOrFunc))], Prefix),
        string.format("%s_%i",
            [s(ints_to_string(ArgNums)), i(ProcNum)], Suffix)
    ),

    % XXX The format of Suffix depends on Prefix; therefore it should
    % immediately follow Prefix.
    string.format("%s__%s__%s",
        [s(Prefix), s(OrigName), s(Suffix)], TransformedName),
    TransformedSymName = qualified(ModuleName, TransformedName).

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
