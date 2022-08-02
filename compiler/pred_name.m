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

:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

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

    ;       tn_user_type_spec(pred_or_func, pred_id, proc_id, int)
            % The predicate calling make_transformed_pred_sym_name with this
            % transform should pass us *not* the name of the predicate
            % being transformed, but the name of the predicate *calling it*
            % at the call site being optimized. The second and third arguments
            % give the pred_id and proc_id of this caller. The last argument
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
    --->    origin_user(user_made)
    ;       origin_compiler(compiler_made)
    ;       origin_pred_transform(pred_transform, pred_origin, pred_id)
            % The predicate is a transformed version of another predicate,
            % whose origin and identity are given by the second and third
            % arguments.
    ;       origin_proc_transform(proc_transform, pred_origin,
                pred_id, proc_id).
            % The predicate is a transformed version of one procedure of
            % another predicate. The origin of that predicate is given
            % by the second argument, and the identity of the procedure
            % are given by the third and fourth arguments.

:- type user_made
    --->    user_made_pred(pred_or_func, sym_name, user_arity)
            % The predicate is a normal user-written predicate or function.
            % The first argument says which, the second and third arguments
            % give its name and arity.

    ;       user_made_lambda(string, int, int)
            % The predicate is a higher-order manifest constant.
            % The first two arguments specify its location in the source,
            % as a filename/line number pair, and the third argument is
            % a sequence number used to distinguish multiple lambdas
            % on the same line.

    ;       user_made_class_method(class_id, pred_pf_name_arity)
            % The predicate is a class method implementation.

    ;       user_made_instance_method(pred_pf_name_arity,
                instance_method_constraints)
            % The predicate is a class method implementation for the class
            % whose class_id is in the instance_method_constraints.
            % Record the method name and arity, and extra information about
            % the class context to allow polymorphism.m to correctly set up
            % the extra type_info and typeclass_info arguments.

    ;       user_made_assertion(promise_type, string, int).
            % The predicate represents an assertion of the given promise_type
            % at the given filename and line number.

:- type compiler_made
    --->    made_for_uci(special_pred_id, type_ctor)
            % If the predicate is a unify, compare or index predicate,
            % specify which one, and for which type constructor.

    ;       made_for_deforestation(int, int)
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

    ;       made_for_solver_repn(type_ctor, solver_type_pred_kind)
            % The predicate is a representation change predicate
            % either to or from either ground or any, as indicated by
            % the solver_type_pred_kind, for the type constructor given by
            % the sym_name and arity.

    ;       made_for_tabling(pred_pf_name_arity, tabling_aux_pred_kind)
            % The predicate is an auxiliary predicate of the indicated kind
            % for the tabled predicate identified by the pf_sym_name_arity.

    ;       made_for_mutable(module_name, string, mutable_pred_kind)
            % The predicate is a predicate that operates on the mutable
            % with the given name in the given module. The last argument
            % says whether the predicate is an init, pre_init, get, set,
            % lock, or unlock predicate on that mutable.

    ;       made_for_initialise(string, int)
            % The predicate implements a standalone initialise declaration
            % (standalone means that it is NOT created to initialise
            % a mutable). The arguments specify the declaration's context
            % as a filename and line number.

    ;       made_for_finalise(string, int).
            % The predicate implements a standalone finalise declaration.
            % The arguments specify the declaration's context as a
            % filename and line number.

:- type pred_transform
    --->    pred_transform_pragma_type_spec(
                % The predicate was created in response to a type_spec
                % pragma containing this substitution from type variables
                % (represented by the integers) to types.
                one_or_more(pair(int, mer_type))
            )
    ;       pred_transform_distance_granularity(
                % The distance parameter of the transformation.
                int
            )
    ;       pred_transform_table_generator
    ;       pred_transform_ssdebug(pred_or_func)
    ;       pred_transform_structure_reuse.
            % pred_transform_structure_reuse should probably be
            % proc_transform_structure_reuse, but until structure reuse
            % work reliably, it does not matter.

:- type proc_transform
    --->    proc_transform_user_type_spec(
                % User-directed type specialization.
                %
                % The pred and proc ids of the caller which caused
                % this new predicate to be created by the compiler.
                % XXX Why is this relevant?
                pred_id,
                proc_id
            )
    ;       proc_transform_higher_order_spec(
                % Non-user-directed type specialization.
                %
                % Sequence number among the higher order specializations
                % of the original predicate.
                % XXX Shouldn't this be "of the module"?
                int
            )
    ;       proc_transform_accumulator(
                % The line number from the context of the original
                % procedure's code.
                int,

                % The list of the numbers of the variables in the original
                % predicate interface that have been converted to accumulators.
                list(int)
            )
    ;       proc_transform_unused_args(
                % The list of eliminated argument numbers.
                list(int)
            )
    ;       proc_transform_loop_inv(
                % The line number from the context of the original
                % procedure's code.
                int,

                % A per-context-unique sequence number for this particular
                % loop invariant transformation. (It will be 1 unless there
                % is more than one such transformation at its context, in
                % which case they will have sequence numbers 1, 2 etc.)
                int
            )
    ;       proc_transform_tuple(
                % The line number from the context of the original
                % procedure's code.
                int,

                % A unique-in-the-module sequence number for this particular
                % tuple transformation.
                int
            )
    ;       proc_transform_untuple(
                % The line number from the context of the original
                % procedure's code.
                int,

                % A unique-in-the-module sequence number for this particular
                % untuple transformation.
                int
            )
    ;       proc_transform_dep_par_conj(
                % The arguments in these positions are inside futures.
                list(int)
            )
    ;       proc_transform_par_loop_ctrl
    ;       proc_transform_lcmc(
                % A unique-for-the-procedure sequence number for
                % this particular lcmc transformation.
                int,

                % The arguments in these positions are returned via pointer.
                list(int)
            )
    ;       proc_transform_stm_expansion
    ;       proc_transform_io_tabling
    ;       proc_transform_direct_arg_in_out.

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

    % pred_origin_to_user_string returns a string that is suitable to identify
    % a predicate to a user
    %
    % - in progress messages,
    % - in error messages, and
    % - as the expansion of $pred.
    %
    % For user written predicates, the result will look like this:
    %
    %       predicate `foo.bar'/3
    %       function `foo.myfoo'/5
    %
    % For predicates created by the compiler, the result be a description
    % such as
    %
    %       unification predicate for `map'/2
    %
    % For predicates that are the transformed version of another predicate,
    % the result will identify the original predicate at the start of the
    % transformation chain (which may contain more than one transformation)
    % and will say that a transformation happened, but will not say
    % how many transformations happened, or what the transformations were,
    % because
    %
    % - such details are not needed in progress messages, and
    % - they *should* not be needed for error messages, since we should
    %   not be reporting any errors for transformed predicates at all.
    %
    % The versions that also specify a proc_id do the same job, only
    % they also append the procedure's mode number.
    %
:- func pred_origin_to_user_string(pred_origin) = string.

    % pred_origin_to_dev_string returns a string that is suitable to identify
    % a predicate to a developer
    %
    % - in HLDS dumps (the parts other than the full pred provenance),
    % - in compiler output intended to debug the compiler itself, and
    % - in progress messages intended to help make sense of such output.
    %
    % The output will differ from pred_id_to_user_string in two main ways:
    %
    % - it will contain no quotes, because the unbalanced `' quote style
    %   we usually use screws up syntax highlighting in HLDS dumps, and
    %
    % - it will contain a description of each transformation applied to
    %   the base predicate.
    %
    % The versions that also specify a proc_id do the same job, only
    % they also append the procedure's mode number.
    %
:- func pred_origin_to_dev_string(pred_origin) = string.

    % Write out a predicate's origin in a format suitable to describe
    % a predicate's origin in that predicate's entry in HLDS dumps.
    % Moved here from hlds_pred.m.
    %
:- func dump_origin(tvarset, var_name_print, pred_origin) = string.

    % Generated an identification of the predicate with the given origin
    % and name to put into layout structures for the debugger and the deeep
    % profiler. Moved here from layout_out.m.
    %
:- func layout_origin_name(pred_origin, string) = string.

    % Return a representation of the pred_origin as a string that
    %
    % - can be put into layout structures in an executable,
    %   after any quote characters in it are escaped, and
    %
    % - can be used by debuggers and profilers to recover a representation
    %   of the original pred_origin that is sufficient both to uniquely
    %   identify the predicate, and to give information its origin
    %   that is sufficient for the purposes of the debugger or profiler.
    %
    % The string should be parseable from the front, meaning that
    % at no point should you have to go to the end of either the whole string,
    % or of a part of it, and scan backwards.
    %
:- func layout_origin_name_new(pred_origin) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.special_pred.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

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
        Transform = tn_user_type_spec(_PredOrFunc, _PredId, ProcId, Version),
        ProcNum = proc_id_to_int(ProcId),
        % As documented above, for this transform, OrigName and ProcId
        % both belong the caller at the call site being optimized, *not*
        % the procedure being transformed, which looks very weird.
        % It would probably also be a bug, if the name actually mattered.
        % Note that _PredOrFunc belongs to the predicate being transformed,
        % but it is ignored ...
        % XXX Ignoring _PredOrFunc seems to me (zs) to be a bug.
        % XXX As is separating OrigName from the suffix with only a single '_'.
        % XXX The string we construct here does not fit into our current
        % naming scheme at all.
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

pred_origin_to_user_string(Origin) = Str :-
    (
        Origin = origin_user(OriginUser),
        Str = origin_user_to_user_dev_string(user, OriginUser)
    ;
        Origin = origin_compiler(OriginCompiler),
        Str = origin_compiler_to_user_dev_string(user, OriginCompiler)
    ;
        ( Origin = origin_pred_transform(_, SubOrigin, _)
        ; Origin = origin_proc_transform(_, SubOrigin, _, _)
        ),
        BaseOrigin = get_base_origin(SubOrigin),
        BaseOriginStr = pred_origin_to_user_string(BaseOrigin),
        string.format("a compiler-transformed version of %s",
            [s(BaseOriginStr)], Str)
    ).

pred_origin_to_dev_string(Origin) = Str :-
    (
        Origin = origin_user(OriginUser),
        Str = origin_user_to_user_dev_string(dev, OriginUser)
    ;
        Origin = origin_compiler(OriginCompiler),
        Str = origin_compiler_to_user_dev_string(dev, OriginCompiler)
    ;
        (
            Origin = origin_pred_transform(PredTransform, SubOrigin, _),
            TransformStr = pred_transform_to_dev_string(PredTransform)
        ;
            Origin = origin_proc_transform(ProcTransform, SubOrigin, _, _),
            TransformStr = proc_transform_to_dev_string(ProcTransform)
        ),
        SubOriginStr = pred_origin_to_dev_string(SubOrigin),
        string.format("%s transformed by %s",
            [s(SubOriginStr), s(TransformStr)], Str)
    ).

:- type user_or_dev
    --->    user
    ;       dev.

:- inst user for user_or_dev/0
    --->    user.
:- inst dev for user_or_dev/0
    --->    dev.

:- func origin_user_to_user_dev_string(user_or_dev, user_made) = string.
:- mode origin_user_to_user_dev_string(in(user), in) = out is det.
:- mode origin_user_to_user_dev_string(in(dev), in) = out is det.

origin_user_to_user_dev_string(UserOrDev, OriginUser) = Str :-
    (
        OriginUser = user_made_pred(PredOrFunc, SymName, UserArity),
        UserArity = user_arity(UserArityInt),
        (
            UserOrDev = user,
            Str = pf_sym_name_user_arity_to_string(PredOrFunc,
                SymName, UserArityInt)
        ;
            UserOrDev = dev,
            Str = pf_sym_name_user_arity_to_unquoted_string(PredOrFunc,
                SymName, UserArityInt)
        )
    ;
        OriginUser = user_made_lambda(FileName, LineNumber, SeqNum),
        string.format("lambda expression #%d at %s/%d",
            [i(SeqNum), s(FileName), i(LineNumber)], Str)
    ;
        OriginUser = user_made_class_method(ClassId, MethodId),
        (
            UserOrDev = user,
            MethodIdStr = pf_sym_name_user_arity_to_string(MethodId)
        ;
            UserOrDev = dev,
            MethodIdStr = pf_sym_name_user_arity_to_unquoted_string(MethodId)
        ),
        ClassId = class_id(ClassSymName, ClassArity),
        ClassNameStr = sym_name_to_string(ClassSymName),
        string.format("class method %s for %s/%d",
            [s(MethodIdStr), s(ClassNameStr), i(ClassArity)], Str)
    ;
        OriginUser = user_made_instance_method(MethodId,
            MethodConstraints),
        (
            UserOrDev = user,
            MethodIdStr = pf_sym_name_user_arity_to_string(MethodId)
        ;
            UserOrDev = dev,
            MethodIdStr = pf_sym_name_user_arity_to_unquoted_string(MethodId)
        ),
        MethodConstraints = instance_method_constraints(ClassId,
            InstanceTypes, _, _),
        ClassId = class_id(ClassName, _),
        ClassStr = sym_name_to_string(ClassName),
        TypeStrs = mercury_type_list_to_string(varset.init, InstanceTypes),
        (
            UserOrDev = user,
            string.format("instance method %s for `%s(%s)'",
                [s(MethodIdStr), s(ClassStr), s(TypeStrs)], Str)
        ;
            UserOrDev = dev,
            string.format("instance method %s for %s(%s)",
                [s(MethodIdStr), s(ClassStr), s(TypeStrs)], Str)
        )
    ;
        OriginUser = user_made_assertion(PromiseType, FileName, LineNumber),
        PromiseTypeStr = prog_out.promise_to_string(PromiseType),
        string.format("%s declaration at %s:%d",
            [s(PromiseTypeStr), s(FileName), i(LineNumber)], Str)
    ).

:- func origin_compiler_to_user_dev_string(user_or_dev, compiler_made)
    = string.
:- mode origin_compiler_to_user_dev_string(in(user), in) = out is det.
:- mode origin_compiler_to_user_dev_string(in(dev), in) = out is det.

origin_compiler_to_user_dev_string(UserOrDev, OriginCompiler) = Str :-
    (
        OriginCompiler = made_for_uci(SpecialId, TypeCtor),
        special_pred_description(SpecialId, PredDescr),
        TypeCtor = type_ctor(TypeSymName, TypeArity),
        ( if TypeArity = 0 then
            string.format("%s for type %s",
                [s(PredDescr), s(sym_name_to_string(TypeSymName))], Str)
        else
            string.format("%s for type constructor %s/%d",
                [s(PredDescr), s(sym_name_to_string(TypeSymName)),
                i(TypeArity)], Str)
        )
    ;
        OriginCompiler = made_for_tabling(BasePredId, TablingAuxPredKind),
        (
            UserOrDev = user,
            BasePredIdStr = pf_sym_name_user_arity_to_string(BasePredId)
        ;
            UserOrDev = dev,
            BasePredIdStr =
                pf_sym_name_user_arity_to_unquoted_string(BasePredId)
        ),
        (
            TablingAuxPredKind = tabling_aux_pred_stats,
            Str = "table statistics predicate for " ++ BasePredIdStr
        ;
            TablingAuxPredKind = tabling_aux_pred_reset,
            Str = "table reset predicate for " ++ BasePredIdStr
        )
    ;
        OriginCompiler = made_for_solver_repn(TypeCtor, SolverAuxPredKind),
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
        OriginCompiler = made_for_deforestation(LineNumber, SeqNum),
        string.format("deforestation-created predicate #%d near line %d",
            [i(SeqNum), i(LineNumber)], Str)
    ;
        OriginCompiler = made_for_mutable(_ModuleName, Name, MutablePredKind),
        MutablePredKindStr = mutable_kind_to_user_dev_string(MutablePredKind),
        string.format("%s for mutable %s",
            [s(MutablePredKindStr), s(Name)], Str)
    ;
        OriginCompiler = made_for_initialise(FileName, LineNumber),
        string.format("initialise declaration at %s:%d",
            [s(FileName), i(LineNumber)], Str)
    ;
        OriginCompiler = made_for_finalise(FileName, LineNumber),
        string.format("finalise declaration at %s:%d",
            [s(FileName), i(LineNumber)], Str)
    ).

:- func mutable_kind_to_user_dev_string(mutable_pred_kind) = string.

mutable_kind_to_user_dev_string(MutablePredKind) = MutablePredKindStr :-
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
    ).

:- inst base_pred_origin for pred_origin/0
    --->    origin_user(ground)
    ;       origin_compiler(ground).

:- func get_base_origin(pred_origin) = pred_origin.
:- mode get_base_origin(in) = out(base_pred_origin) is det.

get_base_origin(Origin) = BaseOrigin :-
    (
        ( Origin = origin_user(_)
        ; Origin = origin_compiler(_)
        ),
        BaseOrigin = Origin
    ;
        ( Origin = origin_pred_transform(_, SubOrigin, _)
        ; Origin = origin_proc_transform(_, SubOrigin, _, _)
        ),
        BaseOrigin = get_base_origin(SubOrigin)
    ).

:- func pred_transform_to_dev_string(pred_transform) = string.

pred_transform_to_dev_string(PredTransform) = Str :-
    (
        PredTransform = pred_transform_pragma_type_spec(Substs),
        SubstStrs = list.map(dump_subst, one_or_more_to_list(Substs)),
        SubstsStr = string.join_list(", ", SubstStrs),
        string.format("type specialization %s", [s(SubstsStr)], Str)
    ;
        PredTransform = pred_transform_distance_granularity(Distance),
        string.format("distance granularity with distance %d",
            [i(Distance)], Str)
    ;
        PredTransform = pred_transform_table_generator,
        Str = "generator for own-stack minimal model tabling"
    ;
        PredTransform = pred_transform_structure_reuse,
        Str = "structure reuse"
    ;
        PredTransform = pred_transform_ssdebug(PredOrFunc),
        string.format("proxy for stdlib %s for source-to-source debugging",
            [s(pred_or_func_to_full_str(PredOrFunc))], Str)
    ).

:- func proc_transform_to_dev_string(proc_transform) = string.

proc_transform_to_dev_string(ProcTransform) = Str :-
    (
        ProcTransform = proc_transform_user_type_spec(CallerPredId,
            CallerProcId),
        CallerPredIdInt = pred_id_to_int(CallerPredId),
        CallerProcIdInt = proc_id_to_int(CallerProcId),
        string.format("user-direct type specialization for pred %d, proc %d",
            [i(CallerPredIdInt), i(CallerProcIdInt)], Str)
    ;
        ProcTransform = proc_transform_higher_order_spec(SeqNum),
        string.format("higher order specialization #%d", [i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_unused_args(Posns),
        PosnsStr = string.join_list(", ", list.map(int_to_string, Posns)),
        ( if Posns = [_, _ | _] then Plural = "s" else Plural = "" ),
        string.format("unused arg elimination for arg%s %s",
            [s(Plural), s(PosnsStr)], Str)
    ;
        ProcTransform = proc_transform_accumulator(_LineNum, Posns),
        PosnsStr = string.join_list(", ", list.map(int_to_string, Posns)),
        ( if Posns = [_, _ | _] then Plural = "s" else Plural = "" ),
        string.format("accumulator introduction on arg%s %s",
            [s(Plural), s(PosnsStr)], Str)
    ;
        ProcTransform = proc_transform_loop_inv(LineNum, SeqNum),
        string.format("loop invariant hoisting on line %d, #%d",
            [i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_tuple(LineNum, SeqNum),
        string.format("tupling on line %d, #%d",
            [i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_untuple(LineNum, SeqNum),
        string.format("untupling on line %d, #%d",
            [i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_dep_par_conj(_),
        Str = "dependent parallel conjunction transform"
    ;
        ProcTransform = proc_transform_par_loop_ctrl,
        Str = "parallel loop control transform"
    ;
        ProcTransform = proc_transform_lcmc(SeqNum, Posns),
        PosnsStr = string.join_list(", ", list.map(int_to_string, Posns)),
        ( if Posns = [_, _ | _] then Plural = "s" else Plural = "" ),
        string.format("last-call-modulo-construct on arg%s %s, #%d",
            [s(Plural), s(PosnsStr), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_stm_expansion,
        Str = "software transactional memory transform"
    ;
        ProcTransform = proc_transform_io_tabling,
        Str = "I/O tabling"
    ;
        ProcTransform = proc_transform_direct_arg_in_out,
        Str = "direct arg in out transform"
    ).

%---------------------------------------------------------------------------%

dump_origin(TVarSet, VarNamePrint, Origin) = Str :-
    Str = dump_origin(TVarSet, VarNamePrint, "% Origin:", Origin).

:- func dump_origin(tvarset, var_name_print, string, pred_origin) = string.

dump_origin(TVarSet, VarNamePrint, Prefix, Origin) = Str :-
    (
        Origin = origin_user(OriginUser),
        (
            OriginUser = user_made_pred(PredOrFunc, SymName, UserArity),
            UserArity = user_arity(UserArityInt),
            string.format("%s user defined %s %s/%d\n",
                [s(Prefix), s(pred_or_func_to_string(PredOrFunc)),
                s(sym_name_to_string(SymName)), i(UserArityInt)], Str)
        ;
            OriginUser = user_made_lambda(FileName, LineNumber, SeqNum),
            string.format("%s lambda expression file %s, line %d, seqnum %d\n",
                [s(Prefix), s(FileName), i(LineNumber), i(SeqNum)], Str)
        ;
            OriginUser = user_made_class_method(ClassId, MethodId),
            ClassId = class_id(ClassSymName, ClassArity),
            MethodId = pred_pf_name_arity(MethodPredOrFunc,
                MethodSymName, MethodUserArity),
            MethodUserArity = user_arity(MethodUserArityInt),
            string.format("%s class method %s %s/%d for %s/%d\n",
                [s(Prefix), s(pred_or_func_to_string(MethodPredOrFunc)),
                s(sym_name_to_string(MethodSymName)), i(MethodUserArityInt),
                s(sym_name_to_string(ClassSymName)), i(ClassArity)], Str)
        ;
            OriginUser = user_made_instance_method(MethodId,
                MethodConstraints),
            MethodId = pred_pf_name_arity(MethodPredOrFunc,
                MethodSymName, MethodUserArity),
            MethodUserArity = user_arity(MethodUserArityInt),
            MethodConstraints = instance_method_constraints(ClassId,
                InstanceTypes, InstanceConstraints, ClassMethodConstraints),
            ClassId = class_id(ClassSymName, ClassArity),
            ClassConstraint = constraint(ClassSymName, InstanceTypes),
            string.format("%s instance method %s %s/%d for class %s/%d\n",
                [s(Prefix), s(pred_or_func_to_str(MethodPredOrFunc)),
                s(sym_name_to_string(MethodSymName)), i(MethodUserArityInt),
                s(sym_name_to_string(ClassSymName)), i(ClassArity)], Line1),
            Line2 = "% instance type vector:\n",
            Line3 = mercury_constraint_to_string(TVarSet, VarNamePrint,
                ClassConstraint),
            Lines4 = dump_origin_constraints("instance constraints",
                TVarSet, VarNamePrint, InstanceConstraints),
            ClassMethodConstraints = constraints(MethodUnivConstraints,
                MethodExistConstraints),
            Lines5 = dump_origin_constraints("method universal constraints",
                TVarSet, VarNamePrint, MethodUnivConstraints),
            Lines6 = dump_origin_constraints("method existential constraints",
                TVarSet, VarNamePrint, MethodExistConstraints),
            Str = Line1 ++ Line2 ++ Line3 ++ Lines4 ++ Lines5 ++ Lines6
        ;
            OriginUser = user_made_assertion(PromiseType,
                FileName, LineNumber),
            PromiseTypeStr = prog_out.promise_to_string(PromiseType),
            string.format("%s %s declaration at %s:%d",
                [s(Prefix), s(PromiseTypeStr), s(FileName), i(LineNumber)],
                Str)
        )
    ;
        Origin = origin_compiler(OriginCompiler),
        (
            OriginCompiler = made_for_uci(SpecialPredId, TypeCtor),
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
            string.format("%s %s pred for %s\n",
                [s(Prefix), s(SpecialPredIdStr),
                s(type_name_to_string(TypeCtor))], Str)
        ;
            OriginCompiler = made_for_deforestation(LineNum, SeqNum),
            string.format("%s deforestation: line %d, seqnum %d\n",
                [s(Prefix), i(LineNum), i(SeqNum)], Str)
        ;
            OriginCompiler = made_for_solver_repn(TypeCtor, SolverAuxPredKind),
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
            string.format("%s %s for %s\n",
                [s(Prefix), s(SolverAuxPredKindStr), s(TypeCtorStr)], Str)
        ;
            OriginCompiler = made_for_tabling(BasePredCallId,
                TablingAuxPredKind),
            BasePredStr = pf_sym_name_user_arity_to_string(BasePredCallId),
            (
                TablingAuxPredKind = tabling_aux_pred_stats,
                TablingAuxPredKindStr = "table statistics predicate"
            ;
                TablingAuxPredKind = tabling_aux_pred_reset,
                TablingAuxPredKindStr = "table reset predicate"
            ),
            string.format("%s %s for %s\n",
                [s(Prefix), s(TablingAuxPredKindStr), s(BasePredStr)], Str)
        ;
            OriginCompiler = made_for_mutable(MutableModuleName, MutableName,
                MutablePredKind),
            MutableModuleNameStr = sym_name_to_string(MutableModuleName),
            MutablePredKindStr =
                mutable_kind_to_user_dev_string(MutablePredKind),
            string.format("%s %s for mutable %s in module %s\n",
                [s(Prefix), s(MutablePredKindStr), s(MutableName),
                s(MutableModuleNameStr)], Str)
        ;
            OriginCompiler = made_for_initialise(FileName, LineNumber),
            string.format("%s initialise predicate, file %s, line %d\n",
                [s(Prefix), s(FileName), i(LineNumber)], Str)
        ;
            OriginCompiler = made_for_finalise(FileName, LineNumber),
            string.format("%s finalise predicate, file %s, line %d\n",
                [s(Prefix), s(FileName), i(LineNumber)], Str)
        )
    ;
        ( Origin = origin_pred_transform(_, _, _)
        ; Origin = origin_proc_transform(_, _, _, _)
        ),
        dump_transformed_origin(TVarSet, VarNamePrint, Origin, _, Strs),
        string.append_list(Strs, Str)
    ).

:- pred dump_transformed_origin(tvarset::in, var_name_print::in,
    pred_origin::in, int::out, list(string)::out) is det.

dump_transformed_origin(TVarSet, VarNamePrint, Origin,
        TransformsPrinted, Strs) :-
    (
        ( Origin = origin_user(_)
        ; Origin = origin_compiler(_)
        ),
        TransformsPrinted = 0,
        Prefix = "% Origin base:",
        OriginStr = dump_origin(TVarSet, VarNamePrint, Prefix, Origin),
        Strs = [OriginStr]
    ;
        Origin = origin_pred_transform(PredTransform, SubOrigin, PredId),
        dump_transformed_origin(TVarSet, VarNamePrint, SubOrigin,
            SubTransformsPrinted, SubStrs),
        TransformsPrinted = SubTransformsPrinted + 1,
        PredIdInt = pred_id_to_int(PredId),
        TransformStr = pred_transform_to_dev_string(PredTransform),
        string.format("%% Transform %d on pred %d:\n%%  %s\n",
            [i(TransformsPrinted), i(PredIdInt), s(TransformStr)], MainStr),
        Strs = SubStrs ++ [MainStr]
    ;
        Origin = origin_proc_transform(ProcTransform, SubOrigin,
            PredId, ProcId),
        dump_transformed_origin(TVarSet, VarNamePrint, SubOrigin,
            SubTransformsPrinted, SubStrs),
        TransformsPrinted = SubTransformsPrinted + 1,
        PredIdInt = pred_id_to_int(PredId),
        ProcIdInt = proc_id_to_int(ProcId),
        TransformStr = proc_transform_to_dev_string(ProcTransform),
        string.format("%% Transform %d on pred %d, proc %d:\n%%  %s\n",
            [i(TransformsPrinted), i(PredIdInt), i(ProcIdInt),
            s(TransformStr)], MainStr),
        Strs = SubStrs ++ [MainStr]
    ).

:- func dump_origin_constraints(string, tvarset, var_name_print,
    list(prog_constraint)) = string.

dump_origin_constraints(Msg, TVarSet, VarNamePrint, Constraints) = Str :-
    (
        Constraints = [],
        string.format("%% %s: none\n", [s(Msg)], Str)
    ;
        Constraints = [_ | _],
        string.format("%% %s:\n", [s(Msg)], HeaderLine),
        ConstraintLines = list.map(
            dump_origin_constraint(TVarSet, VarNamePrint),
            Constraints),
        string.append_list([HeaderLine | ConstraintLines], Str)
    ).

:- func dump_origin_constraint(tvarset, var_name_print, prog_constraint)
    = string.

dump_origin_constraint(TVarSet, VarNamePrint, Constraint) = Str :-
    Str = string.format("%%       %s\n",
        [s(mercury_constraint_to_string(TVarSet, VarNamePrint, Constraint))]).

%---------------------------------------------------------------------------%

layout_origin_name(Origin, Name0) = Name :-
    (
        Origin = origin_user(OriginUser),
        (
            ( OriginUser = user_made_pred(_, _, _)
            ; OriginUser = user_made_class_method(_, _)
            ; OriginUser = user_made_instance_method(_, _)
            ; OriginUser = user_made_assertion(_, _, _)
            ),
            Name = Name0
        ;
            OriginUser = user_made_lambda(FileName0, LineNum, SeqNum),
            ( if string.append("IntroducedFrom", _, Name0) then
                string.replace_all(FileName0, ".", "_", FileName),
                ( if SeqNum > 1 then
                    string.format("lambda%d_%s_%d",
                        [i(SeqNum), s(FileName), i(LineNum)], Name)
                else
                    string.format("lambda_%s_%d",
                        [s(FileName), i(LineNum)], Name)
                )
            else
                % If the lambda pred has a meaningful name, use it.
                % This happens when the lambda is a partial application
                % that happens to supply zero arguments.
                Name = Name0
            )
        )
    ;
        Origin = origin_compiler(OriginCompiler),
        (
            OriginCompiler = made_for_uci(_SpecialPredId, _TypeCtor),
            Name = Name0
            % We can't use the following code until we have adapted the code
            % in the runtime and trace directories to handle the names
            % of special preds the same way as we do user-defined names.
%           (
%               SpecialPredId = unify,
%               SpecialName = "unify"
%           ;
%               SpecialPredId = compare,
%               SpecialName = "compare"
%           ;
%               SpecialPredId = index,
%               SpecialName = "index"
%           ),
%           TypeCtor = TypeSymName - TypeArity,
%           TypeName = sym_name_to_string(TypeSymName),
%           string.format("%s_for_%s_%d",
%               [s(SpecialName), s(TypeName), i(TypeArity)], Name)
        ;
            ( OriginCompiler = made_for_deforestation(_, _)
            ; OriginCompiler = made_for_solver_repn(_, _)
            ; OriginCompiler = made_for_tabling(_, _)
            ; OriginCompiler = made_for_mutable(_, _, _)
            ; OriginCompiler = made_for_initialise(_, _)
            ; OriginCompiler = made_for_finalise(_, _)
            ),
            Name = Name0
        )
    ;
        Origin = origin_pred_transform(PredTransform, OldOrigin, _),
        OldName = layout_origin_name(OldOrigin, ""),
        ( if OldName = "" then
            Name = Name0
        else
            Name = OldName ++ "_" ++ layout_pred_transform_name(PredTransform)
        )
    ;
        Origin = origin_proc_transform(ProcTransform, OldOrigin, _, ProcId),
        OldName = layout_origin_name(OldOrigin, ""),
        ( if
            % for io_tabling, this preserves old behavior
            ( OldName = ""
            ; ProcTransform = proc_transform_io_tabling
            )
        then
            Name = Name0
        else
            Name = OldName ++ "_" ++
                layout_proc_transform_name(ProcTransform, ProcId)
        )
    ).

:- func layout_pred_transform_name(pred_transform) = string.

layout_pred_transform_name(PredTransform) = Name :-
    (
        PredTransform = pred_transform_pragma_type_spec(Substs),
        Name = string.join_list("_", list.map(subst_to_name,
            one_or_more_to_list(Substs)))
    ;
        PredTransform = pred_transform_distance_granularity(Distance),
        Name = "distance_granularity_" ++ int_to_string(Distance)
    ;
        PredTransform = pred_transform_table_generator,
        Name = "table_gen"
    ;
        PredTransform = pred_transform_structure_reuse,
        Name = "structure_reuse"
    ;
        PredTransform = pred_transform_ssdebug(_),
        Name = "ssdebug"
    ).

:- func layout_proc_transform_name(proc_transform, proc_id) = string.

layout_proc_transform_name(ProcTransform, ProcId) = Name :-
    (
        ProcTransform = proc_transform_user_type_spec(_CallerPredId,
            CallerProcId),
        Name = "hoproc" ++ int_to_string(proc_id_to_int(CallerProcId))
    ;
        ProcTransform = proc_transform_higher_order_spec(SeqNum),
        Name = "ho" ++ int_to_string(SeqNum)
    ;
        ProcTransform = proc_transform_unused_args(Posns),
        Name = "ua_" ++ string.join_list("_", list.map(int_to_string, Posns))
    ;
        ProcTransform = proc_transform_accumulator(_LineNum, Posns),
        Name = "acc_" ++ string.join_list("_", list.map(int_to_string, Posns))
    ;
        ProcTransform = proc_transform_loop_inv(_LineNum, _SeqNum),
        Name = "inv_" ++ int_to_string(proc_id_to_int(ProcId))
    ;
        ProcTransform = proc_transform_tuple(_LineNum, _SeqNum),
        Name = "tup_" ++ int_to_string(proc_id_to_int(ProcId))
    ;
        ProcTransform = proc_transform_untuple(_LineNum, _SeqNum),
        Name = "untup_" ++ int_to_string(proc_id_to_int(ProcId))
    ;
        ProcTransform = proc_transform_dep_par_conj(_),
        Name = "dep_par_conj_"
    ;
        ProcTransform = proc_transform_par_loop_ctrl,
        Name = "par_lc"
    ;
        ProcTransform = proc_transform_lcmc(_SeqNum, ArgPos),
        Name = "retptr_" ++ int_to_string(proc_id_to_int(ProcId)) ++
            "_args" ++ underscore_ints_to_string(ArgPos)
    ;
        ProcTransform = proc_transform_stm_expansion,
        Name = "stm_expansion"
    ;
        ProcTransform = proc_transform_io_tabling,
        Name = "io_tabling"
    ;
        ProcTransform = proc_transform_direct_arg_in_out,
        Name = "daio"
    ).

%---------------------------------------------------------------------------%

layout_origin_name_new(Origin) = Str :-
    % The structure of the pred_origin will be indicated by occurrences
    % of the string "mrtq". The initial "mrt" stands for "Mercury runtime",
    % and the "q" is there to reduce the need for stuffing (see below).
    %
    % Each pred and proc transform starts with "mrtq_", and any string
    % we get from the user will have its end indicated by "_mrtq_".
    % To make this work, those user-provided strings will be "mrtq-stuffed",
    % which means that all occurrences of "mrtq" in them replaced by "mrtqq".
    % This prevents "mrtq_" from occurring in the stuffed strings.
    % Debuggers and profilers will have to "unstuff" these user-provided parts
    % of the stuffed strings once they have parsed the relevant part of
    % the string.
    %
    % Likewise, the parts of the predicate origins that deal with types
    % will have the structure of those types indicated by occurrences of "txq",
    % where the "t" stands for "type" and the "xq" is there to reduce the need
    % for "txq-stuffing".
    %
    % NOTE To make this code suitable for generating names for the functions/
    % methods that implement the predicates whose origins we are processing,
    % all the user-provided strings we handle would have to be converted
    % (in a reversible way) to use only characters that the target language
    % allows in identifiers. This means letters, numbers and underscores.
    % The way we construct Str, we guarantee that it starts with one of
    % "pred_", "func_" or "mrtq_", which guarantees that we don't fall foul
    % of limitations about what characters can *start* an identifier
    % in our target languages.
    (
        Origin = origin_user(OriginUser),
        (
            OriginUser = user_made_pred(PorF, SymName, UserArity),
            PorFStr = pred_or_func_to_str(PorF),
            StuffedNameStr = mrtq_stuff_sym_name(SymName),
            UserArity = user_arity(UserArityInt),
            % NOTE We could add an "mrtq_" prefix to Str. This should
            % not be needed in layout structures, but when we use
            % a generalized version of this code to generate target language
            % identifiers, having *all* procedure names start with "mrtq_"
            % could in theory allow us to stop adding a "mercury_" prefix
            % to those names. Unfortunately, this is not practical, because
            % the handwritten code of the runtime system uses the "mercury_"
            % prefix, and any switchover to using a "mrtq_" prefix instead
            % would require this code to be changed, or the callers of
            % predicates defined in the runtime would have to replace
            % the "mrtq_" prefix constructed here with "mercury_".
            % Neither is easy to do.
            string.format("%s_%s_mrtq_%d",
                [s(PorFStr), s(StuffedNameStr), i(UserArityInt)], Str)
        ;
            OriginUser = user_made_class_method(ClassId, PFSymNameArity),
            ClassId = class_id(ClassSymName, ClassArity),
            StuffedClassNameStr = mrtq_stuff_sym_name(ClassSymName),
            PFSymNameArity = pred_pf_name_arity(PorF, MethodSymName,
                MethodUserArity),
            PorFStr = pred_or_func_to_str(PorF),
            StuffedMethodNameStr = mrtq_stuff_sym_name(MethodSymName),
            MethodUserArity = user_arity(MethodUserArityInt),
            string.format("mrtq_class_%s_mrtq_%d_%s_%s_mrtq_%d",
                [s(StuffedClassNameStr), i(ClassArity),
                s(PorFStr), s(StuffedMethodNameStr), i(MethodUserArityInt)],
                Str)
        ;
            OriginUser = user_made_instance_method(PFSymNameArity,
                MethodConstraints),
            PFSymNameArity = pred_pf_name_arity(PorF, MethodSymName,
                MethodUserArity),
            PorFStr = pred_or_func_to_str(PorF),
            StuffedMethodNameStr = mrtq_stuff_sym_name(MethodSymName),
            MethodUserArity = user_arity(MethodUserArityInt),
            MethodConstraints = instance_method_constraints(ClassId,
                InstanceTypes, _, _),
            ClassId = class_id(ClassSymName, ClassArity),
            StuffedClassNameStr = mrtq_stuff_sym_name(ClassSymName),
            InstanceTypeStrs = list.map(type_to_txq_mrtq_stuffed_string,
                InstanceTypes),
            MoreInstanceTypeStrs = list.map((func(S) = "_mrtq_m_" ++ S),
                InstanceTypeStrs),
            MoreInstanceTypesStr = string.append_list(MoreInstanceTypeStrs),
            string.format("mrtq_instance_%s_mrtq_%d_%s_%s_mrtq_%d_%s_mrtq",
                [s(StuffedClassNameStr), i(ClassArity),
                s(PorFStr), s(StuffedMethodNameStr), i(MethodUserArityInt),
                s(MoreInstanceTypesStr)], Str)
        ;
            OriginUser = user_made_assertion(PromiseType, FileName0, LineNum),
            (
                PromiseType = promise_type_true,
                PromiseStr = "assert"
            ;
                PromiseType = promise_type_exclusive,
                PromiseStr = "excl"
            ;
                PromiseType = promise_type_exhaustive,
                PromiseStr = "exh"
            ;
                PromiseType = promise_type_exclusive_exhaustive,
                PromiseStr = "exclexh"
            ),
            string.replace_all(FileName0, ".", "_", FileName1),
            FileName = mrtq_stuff(FileName1),
            string.format("mrtq_%s_%s_mrtq_%d",
                [s(PromiseStr), s(FileName), i(LineNum)], Str)
        ;
            OriginUser = user_made_lambda(FileName0, LineNum, SeqNum),
            string.replace_all(FileName0, ".", "_", FileName1),
            FileName = mrtq_stuff(FileName1),
            string.format("mrtq_lambda_%d_%s_mrtq_%d",
                [i(SeqNum), s(FileName), i(LineNum)], Str)
        )
    ;
        Origin = origin_compiler(OriginCompiler),
        (
            OriginCompiler = made_for_uci(Kind, TypeCtor),
            ( Kind = spec_pred_unify,   UciName = "uni"
            ; Kind = spec_pred_compare, UciName = "cmp"
            ; Kind = spec_pred_index,   UciName = "idx"
            ),
            TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
            TypeCtorStr = sym_name_to_string(TypeCtorSymName),
            StuffedTypeCtorStr = mrtq_stuff(TypeCtorStr),
            string.format("mrtq_%s_%s_%d",
                [s(UciName), s(StuffedTypeCtorStr), i(TypeCtorArity)], Str)
        ;
            OriginCompiler = made_for_deforestation(LineNum, SeqNum),
            string.format("mrtq_deforst_%d_%d", [i(LineNum), i(SeqNum)], Str)
        ;
            OriginCompiler = made_for_solver_repn(TypeCtor, PredKind),
            TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
            TypeCtorStr = sym_name_to_string(TypeCtorSymName),
            StuffedTypeCtorStr = mrtq_stuff(TypeCtorStr),
            (
                PredKind = solver_type_to_ground_pred,
                PredKindStr = "tognd"
            ;
                PredKind = solver_type_to_any_pred,
                PredKindStr = "toany"
            ;
                PredKind = solver_type_from_ground_pred,
                PredKindStr = "fromgnd"
            ;
                PredKind = solver_type_from_any_pred,
                PredKindStr = "fromany"
            ),
            string.format("mrtq_solver_%s_%s_mrtq_%d",
                [s(PredKindStr), s(StuffedTypeCtorStr), i(TypeCtorArity)], Str)
        ;
            OriginCompiler = made_for_tabling(PFSymNameArity, Kind),
            PFSymNameArity = pred_pf_name_arity(PorF, SymName, UserArity),
            PorFStr = pred_or_func_to_str(PorF),
            StuffedNameStr = mrtq_stuff_sym_name(SymName),
            UserArity = user_arity(UserArityInt),
            ( Kind = tabling_aux_pred_stats, KindStr = "stats"
            ; Kind = tabling_aux_pred_reset, KindStr = "reset"
            ),
            string.format("mrtq_table_%s_%s_%s_mrtq_%d",
                [s(KindStr), s(PorFStr), s(StuffedNameStr), i(UserArityInt)],
                Str)
        ;
            OriginCompiler = made_for_mutable(_ModuleName, MutableName,
                Kind),
            StuffedNameStr = mrtq_stuff(MutableName),
            ( Kind = mutable_pred_std_get,              KindStr = "get"
            ; Kind = mutable_pred_std_set,              KindStr = "set"
            ; Kind = mutable_pred_io_get,               KindStr = "ioget"
            ; Kind = mutable_pred_io_set,               KindStr = "ioset"
            ; Kind = mutable_pred_unsafe_get,           KindStr = "uget"
            ; Kind = mutable_pred_unsafe_set,           KindStr = "uset"
            ; Kind = mutable_pred_constant_get,         KindStr = "cget"
            ; Kind = mutable_pred_constant_secret_set,  KindStr = "cset"
            ; Kind = mutable_pred_lock,                 KindStr = "lock"
            ; Kind = mutable_pred_unlock,               KindStr = "unlock"
            ; Kind = mutable_pred_pre_init,             KindStr = "preinit"
            ; Kind = mutable_pred_init,                 KindStr = "init"
            ),
            string.format("mrtq_mutable_%s_%s",
                [s(KindStr), s(StuffedNameStr)], Str)
        ;
            (
                OriginCompiler = made_for_initialise(FileName0, LineNum),
                KindStr = "initialise"
            ;
                OriginCompiler = made_for_finalise(FileName0, LineNum),
                KindStr = "finalise"
            ),
            string.replace_all(FileName0, ".", "_", FileName1),
            FileName = mrtq_stuff(FileName1),
            string.format("mrtq_%s_%s_mrtq_%d",
                [s(KindStr), s(FileName), i(LineNum)], Str)
        )
    ;
        Origin = origin_pred_transform(PredTransform, OldOrigin, _),
        OldStr = layout_origin_name_new(OldOrigin),
        PredTransformStr = layout_pred_transform_name_new(PredTransform),
        % "pts" and "pte" stand for "pred transform start/end".
        string.format("%s_mrtq_pts_%s_mrtq_pte",
            [s(OldStr), s(PredTransformStr)], Str)
    ;
        Origin = origin_proc_transform(ProcTransform, OldOrigin, _, ProcId),
        OldStr = layout_origin_name_new(OldOrigin),
        ProcTransformStr =
            layout_proc_transform_name_new(ProcTransform, ProcId),
        % "pmts" and "pmte" stand for "pred mode transform start/end".
        string.format("%s_mrtq_pmts_%s_mrtq_pmte",
            [s(OldStr), s(ProcTransformStr)], Str)
    ).

    % Return a representation of the pred_transform. The result string
    % will not start or end with an underscore, and will not contain
    % the string "mrtq_".
    %
:- func layout_pred_transform_name_new(pred_transform) = string.

layout_pred_transform_name_new(PredTransform) = Str :-
    (
        PredTransform = pred_transform_pragma_type_spec(Substs),
        SubstList = one_or_more_to_list(Substs),
        SubstStrs = list.map(subst_to_mrtq_stuffed_string, SubstList),
        SubstsStr = string.join_list("_txq_s_", SubstStrs),
        string.format("typespec_%s", [s(SubstsStr)], Str)
    ;
        PredTransform = pred_transform_distance_granularity(Distance),
        string.format("distgran_%d", [i(Distance)], Str)
    ;
        PredTransform = pred_transform_table_generator,
        Str = "tablegen"
    ;
        PredTransform = pred_transform_structure_reuse,
        Str = "structreuse"
    ;
        PredTransform = pred_transform_ssdebug(_),
        Str = "ssdb"
    ).

    % Return a representation of the proc_transform. The result string
    % will not start or end with an underscore, and will not contain
    % the string "mrtq_".
    %
:- func layout_proc_transform_name_new(proc_transform, proc_id) = string.

layout_proc_transform_name_new(ProcTransform, ProcId) = Str :-
    ProcIdInt = proc_id_to_int(ProcId),
    (
        ProcTransform = proc_transform_user_type_spec(_CallerPredId,
            _CallerProcId),
        % XXX This seems to be inadequate, because a predicate or function
        % may have more than one type_spec pragma for it with different type
        % substitutions, and this does not distinguish between them.
        % When constructing names using tn_user_type_spec, we include the
        % proc_id, but this does not address the issue.
        Str = "usertypespec"
    ;
        ProcTransform = proc_transform_higher_order_spec(SeqNum),
        string.format("ho_%d", [i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_unused_args(ArgPosns),
        % Since ArgPosns may not be empty, "ua" will be followed
        % by the underscore before the first argument position number.
        string.format("ua%s", [s(underscore_ints_to_string(ArgPosns))], Str)
    ;
        ProcTransform = proc_transform_accumulator(_LineNum, ArgPosns),
        % Since ArgPosns may not be empty, "acc" will be followed
        % by the underscore before the first argument position number.
        string.format("acc%s", [s(underscore_ints_to_string(ArgPosns))], Str)
    ;
        ProcTransform = proc_transform_loop_inv(LineNum, SeqNum),
        string.format("loopinv_%d_%d_%d",
            [i(ProcIdInt), i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_tuple(LineNum, SeqNum),
        string.format("tup_%d_%d_%d",
            [i(ProcIdInt), i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_untuple(LineNum, SeqNum),
        string.format("untup_%d_%d_%d",
            [i(ProcIdInt), i(LineNum), i(SeqNum)], Str)
    ;
        ProcTransform = proc_transform_dep_par_conj(ArgPosns),
        string.format("depparconj%s",
            [s(underscore_ints_to_string(ArgPosns))], Str)
    ;
        ProcTransform = proc_transform_par_loop_ctrl,
        Str = "parlc"
    ;
        ProcTransform = proc_transform_lcmc(_SeqNum, ArgPosns),
        string.format("lcmc %d%s",
            [i(ProcIdInt), s(underscore_ints_to_string(ArgPosns))], Str)
    ;
        ProcTransform = proc_transform_stm_expansion,
        Str = "stm"
    ;
        ProcTransform = proc_transform_io_tabling,
        Str = "iotabling"
    ;
        ProcTransform = proc_transform_direct_arg_in_out,
        Str = "daio"
    ).

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

:- func dump_subst(pair(int, mer_type)) = string.

dump_subst(TVar - Type) = Str :-
    TypeStr = mercury_type_to_string(varset.init, print_name_only, Type),
    Str = string.format("%d => %s", [i(TVar), s(TypeStr)]).

:- func subst_to_name(pair(int, mer_type)) = string.

subst_to_name(TVar - Type) = Str :-
    TypeStr = mercury_type_to_string(varset.init, print_name_only, Type),
    Str = string.format("%d/%s", [i(TVar), s(TypeStr)]).

:- func subst_to_mrtq_stuffed_string(pair(int, mer_type)) = string.

subst_to_mrtq_stuffed_string(TVar - Type) = Str :-
    TypeStr = type_to_txq_mrtq_stuffed_string(Type),
    string.format("_%d_%s_txq_sub", [i(TVar), s(TypeStr)], Str).

    % txq_X:
    %
    %   X = k:          type is kinded variable (reserved; not yet used)
    %   X = v:          type is type variable
    %   X = b:          type is builtin type
    %   X = d:          type is defined type
    %   X = t:          type is tuple type
    %   X = a:          type is apply_n type
    %   X = h:          type is higher order type
    %
    %   X = n:          end of name of type constructor
    %   X = m:          (at least) one more type in argument type list
    %   X = e:          end of argument type list
    %
:- func type_to_txq_mrtq_stuffed_string(mer_type) = string.

type_to_txq_mrtq_stuffed_string(Type) = Str :-
    (
        Type = kinded_type(SubType, _Kind),
        Str = type_to_txq_mrtq_stuffed_string(SubType)
    ;
        Type = type_variable(TVar, _),
        string.format("txq_v_%d", [i(var_to_int(TVar))], Str)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_to_string(BuiltinType, BuiltinTypeStr),
        string.format("txq_b_%s", [s(BuiltinTypeStr)], Str)
    ;
        (
            Type = defined_type(TypeCtorSymName, ArgTypes, _),
            StuffedTypeCtorStr = txq_mrtq_stuff_sym_name(TypeCtorSymName),
            string.format("txq_d_%s_txq_n", [s(StuffedTypeCtorStr)], StartStr)
        ;
            Type = tuple_type(ArgTypes, _),
            StartStr = "txq_t"
        ;
            Type = apply_n_type(TVar, ArgTypes, _),
            string.format("txq_a_%d", [i(var_to_int(TVar))], StartStr)
        ;
            Type = higher_order_type(PorF, ArgTypes, _HOInst, Purity, _Eval),
            ( PorF = pf_predicate, PorFStr = "p"
            ; PorF = pf_function,  PorFStr = "f"
            ),
            ( Purity = purity_pure,     PurityStr = "p"
            ; Purity = purity_semipure, PurityStr = "s"
            ; Purity = purity_impure,   PurityStr = "i"
            ),
            string.format("txq_h_%s%s", [s(PorFStr), s(PurityStr)], StartStr)
        ),
        ArgTypeStrs = list.map(type_to_txq_mrtq_stuffed_string, ArgTypes),
        MoreArgTypeStrs = list.map((func(S) = "_txq_m_" ++ S), ArgTypeStrs),
        MoreArgTypesStr = string.append_list(MoreArgTypeStrs),
        string.format("%s_%s_txq_e", [s(StartStr), s(MoreArgTypesStr)], Str)
    ).

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

:- func mrtq_stuff_sym_name(sym_name) = string.

mrtq_stuff_sym_name(SymName) = StuffedStr :- 
    NameStr = sym_name_to_string(SymName),
    StuffedStr = mrtq_stuff(NameStr).

:- func txq_mrtq_stuff_sym_name(sym_name) = string.

txq_mrtq_stuff_sym_name(SymName) = StuffedStr :- 
    NameStr = sym_name_to_string(SymName),
    StuffedStr = txq_mrtq_stuff(NameStr).

:- func mrtq_stuff(string) = string.

mrtq_stuff(Str) = StuffedStr :-
    ( if sub_string_search(Str, "mrtq", _StartIndex) then
        Pieces = split_at_string(Str, "mrtq"),
        StuffedStr = string.join_list("mrtqq", Pieces)
    else
        StuffedStr = Str
    ).

:- func txq_mrtq_stuff(string) = string.

txq_mrtq_stuff(Str) = DoubleStuffedStr :-
    ( if sub_string_search(Str, "txq", _StartIndex) then
        Pieces = split_at_string(Str, "txq"),
        StuffedStr = string.join_list("txqq", Pieces)
    else
        StuffedStr = Str
    ),
    DoubleStuffedStr = mrtq_stuff(StuffedStr).

%---------------------------------------------------------------------------%
:- end_module hlds.pred_name.
%---------------------------------------------------------------------------%
