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
% This module contains two related but distinct parts.
%
% The first part contains types and predicates for constructing predicate
% names as strings. This is the approach mmc has traditionally used.
%
% The second part contains the pred_origin type, which records the
% "origin story" of each predicate. It is here because it is now quite close
% to being a *structured* representation of predicate names, and we intend
% to use it to eventually replace the plain string representation.
% So the first part is intended to eventually be deleted.
%
%---------------------------------------------------------------------------%

:- module hlds.pred_name.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.

%---------------------------------------------------------------------------%
%
% First part:
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

    % Return the predicate name we should use for promise of the given type
    % at the given location.
    %
:- func promise_pred_name(promise_type, string, int) = string.

%---------------------%

    % Return the predicate name we should use for the given uci (special) pred
    % for the given type_ctor.
    %
:- func uci_pred_name(special_pred_id, type_ctor) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Second part:
%

    % We should be able to use this type as a structured representation
    % of a name, to implement *structural* name mangling/unmangling, i.e.
    % to solve the problem of how to represent the structure of a predicate's
    % origins in a string, for inclusion in target language code and
    % eventually in executables, in a way that allows the structure
    % to be unambiguously recovered later. This is distinct from *lexical*
    % mangling/unmangling, which ensures that the string is acceptable
    % to the target language compiler as (usually) an identifier, which
    % requires mapping any characters that may not appear in identifiers
    % to characters that may appear there.
    %
    % The idea is that we should generate the names of the procedures
    % we output to target language source code *and* in the RTTI program
    % representations we generate for the debugger and the profilers
    % from the pred_origin field of the pred_info, not the name field.
    % The reason for this is that the mangling process can be considerably
    % simpler if it has access to a *structured* representation of all the
    % info that the target language name must include. Our old (and still
    % current) approach of encoding this info in a single string effectively
    % forced piecemeal development of the predicate name mangling scheme,
    % by requiring the developer who added a new kind of pred_origin to
    % pick its string representation *without* access to the string
    % representation of all the *other* pred origins, since those were
    % scattered throughout the rest of the compiler. This is why our current
    % undemangle algorithm does not have a convincing correctness argument,
    % which means that it is quite likely that in the presence of
    % unfortunately (or maliciously) chosen names for the base predicates,
    % it can generate incorrect output.
    %
    % On the other hand, an algorithm for converting a pred_origin
    % into a mangled target language name would have all the code
    % implementing the naming scheme in one place, which would
    %
    % - allow us to design and implement a simpler and more consistent
    %   naming scheme than the current one, and
    %
    % - allow us to develop the mangling algorithm using that scheme
    %   to be developed hand-in-hand with the corresponding updated unmangling
    %   algorithm, which *should* allow a correctness argument for the
    %   proposition that unmangle(mangle(PredOrigin) is always PredOrigin.
    %
    % The strings we use to represent the names and parameters of
    % program transformations and of compiler components that create
    % procedures should never include characters or substrings that
    % need to be lexically mangled, but they may include user provided names
    % (of predicates, functions, types classes, mutables etc) that
    % may require lexical mangling. Unlike our old approach, the  new approach
    % should allow this lexical mangling to be done just on the user-provided
    % parts of the string, without requiring the *whole* string to be
    % lexically mangled. And we can simply forego the lexical mangling
    % in situatins in which it is not required, e.g. in HLDS dumps.

:- type pred_origin
    --->    origin_user(pred_or_func, sym_name, user_arity)
            % The predicate is a normal user-written predicate or function.
            % The first argument says which, the second and third arguments
            % give its name and arity.

    ;       origin_special_pred(special_pred_id, type_ctor)
            % If the predicate is a unify, compare or index predicate,
            % specify which one, and for which type constructor.

    ;       origin_instance_method(sym_name, instance_method_constraints)
            % The predicate is a class method implementation. Record
            % the method name and extra information about the class
            % context to allow polymorphism.m to correctly set up the
            % extra type_info and typeclass_info arguments.

    ;       origin_class_method(class_id, pf_sym_name_arity)
            % The predicate is a class method implementation.

    ;       origin_deforestation(int, int)
            % The predicate was created by deforestation from two or more
            % goals, so that it would be incorrect to record it being
            % a transformed version of any one of them.
            %
            % The first integer gives the line number of one of the goals,
            % and the second is a deforestation action sequence number,
            % which will be different in different predicates created by
            % deforestation. (We don't record the filename part of the context,
            % because it will be the source file containing the module
            % except in the *extremely* rare cases where the module contains
            % a #line directive.

    ;       origin_assertion(string, int)
            % The predicate represents an assertion.

    ;       origin_lambda(string, int, int)
            % The predicate is a higher-order manifest constant.
            % The arguments specify its location in the source, as a
            % filename/line number pair, and a sequence number used to
            % distinguish multiple lambdas on the same line.

    ;       origin_solver_repn(type_ctor, solver_type_pred_kind)
            % The predicate is a representation change predicate
            % either to or from either ground or any, as indicated by
            % the solver_type_pred_kind, for the type constructor given by
            % the sym_name and arity.

    ;       origin_tabling(pf_sym_name_arity, tabling_aux_pred_kind)
            % The predicate is an auxiliary predicate of the indicated kind
            % for the tabled predicate identified by the pf_sym_name_arity.

    ;       origin_mutable(module_name, string, mutable_pred_kind)
            % The predicate is a predicate that operates on the mutable
            % with the given name in the given module. The last argument
            % says whether the predicate is an init, pre_init, get, set,
            % lock, or unlock predicate on that mutable.

    ;       origin_initialise
            % The predicate implements a standalone initialise declaration
            % (standalone means that it is NOT created to initialise
            % a mutable).

    ;       origin_finalise
            % The predicate implements a standalone finalise declaration.

    ;       origin_transformed(pred_transformation, pred_origin, pred_id).
            % The predicate is a transformed version of another predicate,
            % whose origin and identity are given by the second and third
            % arguments.

:- type pred_transformation
    --->    transform_higher_order_spec(
                % Sequence number among the higher order specializations
                % of the original predicate.
                int
            )
    ;       transform_higher_order_type_spec(
                % The proc id of the original procedure.
                proc_id
            )
    ;       transform_type_spec(
                % The substitution from type variables (represented by
                % the integers) to types (represented by the terms).
                one_or_more(pair(int, mer_type))
            )
    ;       transform_unused_args(
                % The proc id of the original procedure.
                proc_id,

                % The list of eliminated argument numbers.
                list(int)
            )
    ;       transform_accumulator(
                % The line number from the context of the original
                % procedure's code.
                int,

                % The list of the numbers of the variables in the original
                % predicate interface that have been converted to accumulators.
                list(int)
            )
    ;       transform_loop_inv(
                % The proc id of the original procedure.
                proc_id,

                % The line number from the context of the original
                % procedure's code.
                int,

                % A per-context-unique sequence number for this particular
                % loop invariant transformation. (It will be 1 unless there
                % is more than one such transformation at its context, in
                % which case they will have sequence numbers 1, 2 etc.)
                int
            )
    ;       transform_tuple(
                % The proc id of the original procedure.
                proc_id,

                % The line number from the context of the original
                % procedure's code.
                int,

                % A unique-in-the-module sequence number for this particular
                % tuple transformation.
                int
            )
    ;       transform_untuple(
                % The proc id of the original procedure.
                proc_id,

                % The line number from the context of the original
                % procedure's code.
                int,

                % A unique-in-the-module sequence number for this particular
                % untuple transformation.
                int
            )
    ;       transform_distance_granularity(
                % The distance parameter of the transformation.
                int
            )
    ;       transform_dep_par_conj(
                % The id of the procedure this predicate is derived from.
                proc_id,

                % The arguments in these positions are inside futures.
                list(int)
            )
    ;       transform_par_loop_ctrl(
                % The id of the procedure this predicate is derived from.
                proc_id
            )
    ;       transform_lcmc(
                % The id of the procedure this predicate is derived from.
                proc_id,

                % A unique-for-the-procedure sequence number for
                % this particular lcmc transformation.
                int,

                % The arguments in these positions are returned via pointer.
                list(int)
            )
    ;       transform_table_generator
    ;       transform_stm_expansion
    ;       transform_structure_reuse
    ;       transform_io_tabling
    ;       transform_ssdebug
    ;       transform_direct_arg_in_out.

    % Describes the class constraints on an instance method implementation.
    % This information is used by polymorphism.m to ensure that the
    % type_info and typeclass_info arguments are added in the order in
    % which they will be passed in by do_call_class_method.
    %
:- type instance_method_constraints
    --->    instance_method_constraints(
                class_id,

                % The types in the head of the instance declaration.
                list(mer_type),

                % The universal constraints on the instance declaration.
                list(prog_constraint),

                % The constraints on the method's type declaration in the
                % `:- typeclass' declaration.
                prog_constraints
            ).

%---------------------------------------------------------------------------%
%
% These two functions and this predicate are three different ways to convert
% a pred_origin into a string. They have different uses cases that impose
% different criteria about what characteristics the output should have.
% Some of their differences result from this, but some are simply caused
% by the fact that they were developed independently of each other,
% so even parts of the output that they *could* be consistent about
% they are not necessarily consistent about.
%

    % pred_info_id_to_string returns a string such as
    %       predicate `foo.bar/3'
    % or    function `foo.myfoo/5'
    % except in some special cases where the predicate name is mangled
    % and we can print a more meaningful identification of the predicate
    % in question.
    %
    % Moved here from hlds_util.m. Its output is used to identify predicates
    % in
    %
    % - expansions of $pred,
    % - compiler progress messages,
    % - error messages, and
    % - in parts of HLDS dumps other than the predicate's own entry.
    %
:- func pred_info_id_to_string(pred_info) = string.

    % Write out a predicate's origin in a format suitable to describe
    % a predicate's origin in that predicate's entry in HLDS dumps.
    % Moved here from hlds_pred.m.
    %
:- pred write_origin(io.text_output_stream::in, module_info::in,
    tvarset::in, var_name_print::in, pred_origin::in, io::di, io::uo) is det.

    % Generated an identification of the predicate with the given origin
    % and name to put into layout structures for the debugger and the deeep
    % profiler. Moved here from layout_out.m.
    %
:- func layout_origin_name(pred_origin, string) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.special_pred.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module string.
:- import_module term.
:- import_module varset.

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
                [s(bracketed_ints_to_string(ArgNums)), i(ProcNum)], Suffix)
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
                [i(ProcNum), s(bracketed_ints_to_string(ArgNums))], Suffix)
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
                [s(bracketed_ints_to_string(ArgNums)), i(ProcNum)], Suffix)
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

promise_pred_name(PromiseType, FileName, LineNumber) = Name :-
    % This naming scheme avoids naming conflicts only by luck.
    PromiseTypeStr = prog_out.promise_to_string(PromiseType),
    string.format("%s__%d__%s",
        [s(PromiseTypeStr), i(LineNumber), s(FileName)], Name).

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

pred_info_id_to_string(PredInfo) = Str :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    (
        Origin = origin_special_pred(SpecialId, TypeCtor),
        special_pred_description(SpecialId, Descr),
        TypeCtor = type_ctor(_TypeSymName, TypeArity),
        ( if TypeArity = 0 then
            ForStr = " for type "
        else
            ForStr = " for type constructor "
        ),
        Str = Descr ++ ForStr ++ type_name_to_string(TypeCtor)
    ;
        Origin = origin_instance_method(MethodName, MethodConstraints),
        MethodConstraints = instance_method_constraints(ClassId,
            InstanceTypes, _, _),
        MethodStr = pf_sym_name_orig_arity_to_string(PredOrFunc, MethodName,
            PredFormArity),
        ClassId = class_id(ClassName, _),
        ClassStr = sym_name_to_string(ClassName),
        TypeStrs = mercury_type_list_to_string(varset.init, InstanceTypes),
        string.format("instance method %s for `%s(%s)'",
            [s(MethodStr), s(ClassStr), s(TypeStrs)], Str)
    ;
        Origin = origin_class_method(ClassId, MethodId),
        ClassId = class_id(ClassSymName, ClassArity),
        MethodId = pf_sym_name_arity(MethodPredOrFunc,
            MethodSymName, MethodPredFormArity),
        user_arity_pred_form_arity(MethodPredOrFunc,
            MethodUserArity, MethodPredFormArity),
        MethodUserArity = user_arity(MethodUserArityInt),
        string.format("class method %s %s/%d for %s/%d",
            [s(pred_or_func_to_string(MethodPredOrFunc)),
            s(sym_name_to_string(MethodSymName)), i(MethodUserArityInt),
            s(sym_name_to_string(ClassSymName)), i(ClassArity)], Str)
    ;
        Origin = origin_assertion(FileName, LineNumber),
        ( if pred_info_is_promise(PredInfo, PromiseType) then
            Str = string.format("`%s' declaration (%s:%d)",
                [s(prog_out.promise_to_string(PromiseType)),
                s(FileName), i(LineNumber)])
        else
            SymName = qualified(Module, Name),
            Str = pf_sym_name_orig_arity_to_string(PredOrFunc, SymName,
                PredFormArity)
        )
    ;
        Origin = origin_tabling(BasePredId, TablingAuxPredKind),
        BasePredIdStr = pf_sym_name_orig_arity_to_string(BasePredId),
        (
            TablingAuxPredKind = tabling_aux_pred_stats,
            Str = "table statistics predicate for " ++ BasePredIdStr
        ;
            TablingAuxPredKind = tabling_aux_pred_reset,
            Str = "table reset predicate for " ++ BasePredIdStr
        )
    ;
        Origin = origin_solver_repn(TypeCtor, SolverAuxPredKind),
        TypeCtorStr = type_ctor_to_string(TypeCtor),
        (
            SolverAuxPredKind = solver_type_to_ground_pred,
            Str = "to ground representation predicate for " ++ TypeCtorStr
        ;
            SolverAuxPredKind = solver_type_to_any_pred,
            Str = "to any representation predicate for " ++ TypeCtorStr
        ;
            SolverAuxPredKind = solver_type_from_ground_pred,
            Str = "from ground representation predicate for " ++ TypeCtorStr
        ;
            SolverAuxPredKind = solver_type_from_any_pred,
            Str = "from any representation predicate for " ++ TypeCtorStr
        )
    ;
        ( Origin = origin_transformed(_, _, _)
        ; Origin = origin_deforestation(_, _)
        ; Origin = origin_mutable(_, _, _)
        ; Origin = origin_lambda(_, _, _)
        ; Origin = origin_initialise
        ; Origin = origin_finalise
        ; Origin = origin_user(_, _, _)
        ),
        SymName = qualified(Module, Name),
        Str = pf_sym_name_orig_arity_to_string(PredOrFunc, SymName,
            PredFormArity)
    ).

%---------------------------------------------------------------------------%

write_origin(Stream, ModuleInfo, TVarSet, VarNamePrint, Origin, !IO) :-
    (
        Origin = origin_special_pred(SpecialPredId, TypeCtor),
        (
            SpecialPredId = spec_pred_unify,
            SpecialPredIdStr = "unify"
        ;
            SpecialPredId = spec_pred_compare,
            SpecialPredIdStr = "compare"
        ;
            SpecialPredId = spec_pred_index,
            SpecialPredIdStr = "index"
        ),
        io.format(Stream, "%% special %s pred for %s\n",
            [s(SpecialPredIdStr), s(type_name_to_string(TypeCtor))], !IO)
    ;
        Origin = origin_instance_method(_, MethodConstraints),
        MethodConstraints = instance_method_constraints(ClassId,
            InstanceTypes, InstanceConstraints, ClassMethodConstraints),
        io.write_string(Stream, "% instance method\n", !IO),
        ClassId = class_id(ClassName, _),
        io.write_string(Stream,
            "% class name and instance type vector:\n", !IO),
        io.write_string(Stream, "%   ", !IO),
        mercury_output_constraint(TVarSet, VarNamePrint,
            constraint(ClassName, InstanceTypes), Stream, !IO),
        io.nl(Stream, !IO),
        write_origin_constraints(Stream, "instance constraints",
            TVarSet, VarNamePrint, InstanceConstraints, !IO),
        ClassMethodConstraints = constraints(MethodUnivConstraints,
            MethodExistConstraints),
        write_origin_constraints(Stream, "method universal constraints",
            TVarSet, VarNamePrint, MethodUnivConstraints, !IO),
        write_origin_constraints(Stream, "method existential constraints",
            TVarSet, VarNamePrint, MethodExistConstraints, !IO)
    ;
        Origin = origin_class_method(ClassId, MethodId),
        ClassId = class_id(ClassSymName, ClassArity),
        MethodId = pf_sym_name_arity(MethodPredOrFunc,
            MethodSymName, MethodPredFormArity),
        user_arity_pred_form_arity(MethodPredOrFunc,
            MethodUserArity, MethodPredFormArity),
        MethodUserArity = user_arity(MethodUserArityInt),
        io.format(Stream, "%% class method %s %s/%d for %s/%d\n",
            [s(pred_or_func_to_string(MethodPredOrFunc)),
            s(sym_name_to_string(MethodSymName)), i(MethodUserArityInt),
            s(sym_name_to_string(ClassSymName)), i(ClassArity)], !IO)
    ;
        Origin = origin_transformed(Transformation, _, OrigPredId),
        OrigPredIdNum = pred_id_to_int(OrigPredId),
        module_info_pred_info(ModuleInfo, OrigPredId, OrigPredInfo),
        io.format(Stream, "%% transformed from pred id %d\n",
            [i(OrigPredIdNum)], !IO),
        io.write_string(Stream, "% ", !IO),
        io.write_string(Stream, pred_info_id_to_string(OrigPredInfo), !IO),
        io.nl(Stream, !IO),
        io.write_string(Stream, "% transformation: ", !IO),
        io.write_line(Stream, Transformation, !IO)
    ;
        Origin = origin_deforestation(LineNum, SeqNum),
        io.format(Stream, "%% deforestation: line %d, seqnum %d\n",
            [i(LineNum), i(SeqNum)], !IO)
    ;
        Origin = origin_assertion(_, _),
        io.write_string(Stream, "% assertion\n", !IO)
    ;
        Origin = origin_solver_repn(TypeCtor, SolverAuxPredKind),
        TypeCtorStr = type_ctor_to_string(TypeCtor),
        (
            SolverAuxPredKind = solver_type_to_ground_pred,
            SolverAuxPredKindStr = "to ground conversion predicate"
        ;
            SolverAuxPredKind = solver_type_to_any_pred,
            SolverAuxPredKindStr = "to any conversion predicate"
        ;
            SolverAuxPredKind = solver_type_from_ground_pred,
            SolverAuxPredKindStr = "from ground conversion predicate"
        ;
            SolverAuxPredKind = solver_type_from_any_pred,
            SolverAuxPredKindStr = "from any conversion predicate"
        ),
        io.format(Stream, "%% %s for %s\n",
            [s(SolverAuxPredKindStr), s(TypeCtorStr)], !IO)
    ;
        Origin = origin_tabling(BasePredCallId, TablingAuxPredKind),
        BasePredStr = pf_sym_name_orig_arity_to_string(BasePredCallId),
        (
            TablingAuxPredKind = tabling_aux_pred_stats,
            TablingAuxPredKindStr = "table statistics predicate"
        ;
            TablingAuxPredKind = tabling_aux_pred_reset,
            TablingAuxPredKindStr = "table reset predicate"
        ),
        io.format(Stream, "%% %s for %s\n",
            [s(TablingAuxPredKindStr), s(BasePredStr)], !IO)
    ;
        Origin = origin_mutable(MutableModuleName, MutableName,
            MutablePredKind),
        MutableModuleNameStr = sym_name_to_string(MutableModuleName),
        (
            MutablePredKind = mutable_pred_std_get,
            MutablePredKindStr = "std get predicate"
        ;
            MutablePredKind = mutable_pred_std_set,
            MutablePredKindStr = "std set predicate"
        ;
            MutablePredKind = mutable_pred_io_get,
            MutablePredKindStr = "io get predicate"
        ;
            MutablePredKind = mutable_pred_io_set,
            MutablePredKindStr = "io set predicate"
        ;
            MutablePredKind = mutable_pred_unsafe_get,
            MutablePredKindStr = "unsafe get predicate"
        ;
            MutablePredKind = mutable_pred_unsafe_set,
            MutablePredKindStr = "unsafe set predicate"
        ;
            MutablePredKind = mutable_pred_constant_get,
            MutablePredKindStr = "constant get predicate"
        ;
            MutablePredKind = mutable_pred_constant_secret_set,
            MutablePredKindStr = "constant secret set predicate"
        ;
            MutablePredKind = mutable_pred_lock,
            MutablePredKindStr = "lock predicate"
        ;
            MutablePredKind = mutable_pred_unlock,
            MutablePredKindStr = "unlock predicate"
        ;
            MutablePredKind = mutable_pred_pre_init,
            MutablePredKindStr = "preinit predicate"
        ;
            MutablePredKind = mutable_pred_init,
            MutablePredKindStr = "init predicate"
        ),
        io.format(Stream, "%% %s for mutable %s in module %s\n",
            [s(MutablePredKindStr), s(MutableName),
            s(MutableModuleNameStr)], !IO)
    ;
        Origin = origin_initialise,
        io.write_string(Stream, "% initialise\n", !IO)
    ;
        Origin = origin_finalise,
        io.write_string(Stream, "% finalise\n", !IO)
    ;
        ( Origin = origin_lambda(_, _, _)
        ; Origin = origin_user(_, _, _)
        )
    ).

:- pred write_origin_constraints(io.text_output_stream::in, string::in,
    tvarset::in, var_name_print::in, list(prog_constraint)::in, io::di, io::uo)
    is det.

write_origin_constraints(Stream, Msg, TVarSet, VarNamePrint,
        Constraints, !IO) :-
    (
        Constraints = [],
        io.format(Stream, "%% %s: none\n", [s(Msg)], !IO)
    ;
        Constraints = [_ | _],
        io.format(Stream, "%% %s:\n", [s(Msg)], !IO),
        list.foldl(write_origin_constraint(Stream, TVarSet, VarNamePrint),
            Constraints, !IO)
    ).

:- pred write_origin_constraint(io.text_output_stream::in,
    tvarset::in, var_name_print::in, prog_constraint::in, io::di, io::uo)
    is det.

write_origin_constraint(Stream, TVarSet, VarNamePrint, Constraint, !IO) :-
    io.write_string(Stream, "%       ", !IO),
    mercury_output_constraint(TVarSet, VarNamePrint, Constraint, Stream, !IO),
    io.nl(Stream, !IO).

%---------------------------------------------------------------------------%

layout_origin_name(Origin, Name0) = Name :-
    (
        Origin = origin_lambda(FileName0, LineNum, SeqNo),
        ( if string.append("IntroducedFrom", _, Name0) then
            string.replace_all(FileName0, ".", "_", FileName),
            ( if SeqNo > 1 then
                string.format("lambda%d_%s_%d",
                    [i(SeqNo), s(FileName), i(LineNum)], Name)
            else
                string.format("lambda_%s_%d", [s(FileName), i(LineNum)], Name)
            )
        else
            % If the lambda pred has a meaningful name, use it.
            % This happens when the lambda is a partial application
            % that happens to supply zero arguments.
            Name = Name0
        )
    ;
        Origin = origin_special_pred(_SpecialPredId, _TypeCtor),
        Name = Name0
        % We can't use the following code until we have adapted the code
        % in the runtime and trace directories to handle the names
        % of special preds the same way as we do user-defined names.
%       (
%           SpecialPredId = unify,
%           SpecialName = "unify"
%       ;
%           SpecialPredId = compare,
%           SpecialName = "compare"
%       ;
%           SpecialPredId = index,
%           SpecialName = "index"
%       ),
%       TypeCtor = TypeSymName - TypeArity,
%       TypeName = sym_name_to_string(TypeSymName),
%       string.format("%s_for_%s_%d",
%           [s(SpecialName), s(TypeName), i(TypeArity)], Name)
    ;
        Origin = origin_transformed(Transform, OldOrigin, _),
        OldName = layout_origin_name(OldOrigin, ""),
        ( if
            ( OldName = ""
            ; Transform = transform_io_tabling      % preserves old behavior
            )
        then
            Name = Name0
        else
            Name = OldName ++ "_" ++ layout_pred_transform_name(Transform)
        )
    ;
        ( Origin = origin_user(_, _, _)
        ; Origin = origin_instance_method(_, _)
        ; Origin = origin_class_method(_, _)
        ; Origin = origin_deforestation(_, _)
        ; Origin = origin_assertion(_, _)
        ; Origin = origin_solver_repn(_, _)
        ; Origin = origin_tabling(_, _)
        ; Origin = origin_mutable(_, _, _)
        ; Origin = origin_initialise
        ; Origin = origin_finalise
        ),
        Name = Name0
    ).

:- func layout_pred_transform_name(pred_transformation) = string.

layout_pred_transform_name(transform_higher_order_spec(Seq)) =
    "ho" ++ int_to_string(Seq).
layout_pred_transform_name(transform_higher_order_type_spec(ProcId)) =
    "hoproc" ++ int_to_string(proc_id_to_int(ProcId)).
layout_pred_transform_name(transform_type_spec(Substs)) =
    string.join_list("_", list.map(subst_to_name,
        one_or_more_to_list(Substs))).
layout_pred_transform_name(transform_unused_args(_ProcId, Posns)) =
    "ua_" ++ string.join_list("_", list.map(int_to_string, Posns)).
layout_pred_transform_name(transform_accumulator(_LineNum, Posns)) = "acc_" ++
    string.join_list("_", list.map(int_to_string, Posns)).
layout_pred_transform_name(transform_loop_inv(ProcId, _LineNum, _SeqNum)) =
    "inv_" ++ int_to_string(proc_id_to_int(ProcId)).
layout_pred_transform_name(transform_tuple(ProcId, _LineNum, _SeqNum)) =
    "tup_" ++ int_to_string(proc_id_to_int(ProcId)).
layout_pred_transform_name(transform_untuple(ProcId, _LineNum, _SeqNum)) =
    "untup_" ++ int_to_string(proc_id_to_int(ProcId)).
layout_pred_transform_name(transform_distance_granularity(Distance)) =
    "distance_granularity_" ++ int_to_string(Distance).
layout_pred_transform_name(transform_dep_par_conj(_, _)) = "dep_par_conj_".
layout_pred_transform_name(transform_par_loop_ctrl(_)) = "par_lc".
layout_pred_transform_name(transform_lcmc(ProcId, _SeqNum, ArgPos)) =
    "retptr_" ++ int_to_string(proc_id_to_int(ProcId)) ++ "_args"
        ++ underscore_ints_to_string(ArgPos).
layout_pred_transform_name(transform_table_generator) = "table_gen".
layout_pred_transform_name(transform_stm_expansion) = "stm_expansion".
layout_pred_transform_name(transform_structure_reuse) = "structure_reuse".
layout_pred_transform_name(transform_io_tabling) = "io_tabling".
layout_pred_transform_name(transform_ssdebug) = "ssdebug".
layout_pred_transform_name(transform_direct_arg_in_out) = "daio".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func type_subst_to_string(tvarset, type_subst) = string.

type_subst_to_string(VarSet, TypeSubst) = Str :-
    TVarStrs = list.map(type_var_subst_to_string(VarSet),
        one_or_more_to_list(TypeSubst)),
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

:- func subst_to_name(pair(int, mer_type)) = string.

subst_to_name(TVar - Type) = Str :-
    TypeStr = mercury_type_to_string(varset.init, print_name_only, Type),
    Str = string.format("%d/%s", [i(TVar), s(TypeStr)]).

%---------------------------------------------------------------------------%

:- func underscore_ints_to_string(list(int)) = string.

underscore_ints_to_string([]) = "".
underscore_ints_to_string([N | Ns]) =
    "_" ++ int_to_string(N) ++ underscore_ints_to_string(Ns).

:- func bracketed_ints_to_string(list(int)) = string.

bracketed_ints_to_string(Ints) = Str :-
    IntStrs = list.map(string.int_to_string, Ints),
    % XXX The use of , and [] here *requires* mangling the names
    % we construct.
    IntsStr = string.join_list(", ", IntStrs),
    string.format("[%s]", [s(IntsStr)], Str).

%---------------------------------------------------------------------------%
:- end_module hlds.pred_name.
%---------------------------------------------------------------------------%
