%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with predicates
% and procedures.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_pred.
:- interface.

:- import_module check_hlds.mode_constraint_robdd.
:- import_module check_hlds.mode_errors.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_graph.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.term_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module string.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- interface.

    % A proc_id is the name of a mode within a particular predicate -
    % not to be confused with a mode_id, which is the name of a
    % user-defined mode.

:- type pred_id.
:- type proc_id.
:- type pred_proc_id
    --->    proc(pred_id, proc_id).

    % Predicate and procedure ids are abstract data types. One important
    % advantage of this arrangement is to make it harder to accidentally
    % use an integer in their place. However, you can convert between
    % integers and pred_ids/proc_ids with the following predicates and
    % functions.
    %
:- func shroud_pred_id(pred_id) = shrouded_pred_id.
:- func shroud_proc_id(proc_id) = shrouded_proc_id.
:- func shroud_pred_proc_id(pred_proc_id) = shrouded_pred_proc_id.

:- func unshroud_pred_id(shrouded_pred_id) = pred_id.
:- func unshroud_proc_id(shrouded_proc_id) = proc_id.
:- func unshroud_pred_proc_id(shrouded_pred_proc_id) = pred_proc_id.

:- pred pred_id_to_int(pred_id, int).
:- mode pred_id_to_int(in, out) is det.
:- mode pred_id_to_int(out, in) is det.
:- func pred_id_to_int(pred_id) = int.

:- pred proc_id_to_int(proc_id, int).
:- mode proc_id_to_int(in, out) is det.
:- mode proc_id_to_int(out, in) is det.
:- func proc_id_to_int(proc_id) = int.

    % Return the id of the first predicate in a module, and of the first
    % procedure in a predicate.
    %
:- func initial_pred_id = pred_id.
:- func initial_proc_id = proc_id.

    % Return an invalid predicate or procedure id. These are intended to be
    % used to initialize the relevant fields in in call(...) goals before
    % we do type- and mode-checks, or when those checks find that there was
    % no predicate matching the call.
    %
:- func invalid_pred_id = pred_id.
:- func invalid_proc_id = proc_id.

:- pred next_pred_id(pred_id::in, pred_id::out) is det.

    % For semidet complicated unifications with mode (in, in), these are
    % defined to have the same proc_id (0). This returns that proc_id.
    %
:- pred in_in_unification_proc_id(proc_id::out) is det.

:- type pred_info.
:- type proc_info.

:- type proc_table  ==  map(proc_id, proc_info).

:- pred next_mode_id(proc_table::in, proc_id::out) is det.

:- type call_id
    --->    plain_call_id(simple_call_id)
    ;       generic_call_id(generic_call_id).

:- type generic_call_id
    --->    gcid_higher_order(purity, pred_or_func, arity)
    ;       gcid_class_method(class_id, simple_call_id)
    ;       gcid_event_call(string)
    ;       gcid_cast(cast_kind).

:- type pred_proc_list  ==  list(pred_proc_id).

%-----------------------------------------------------------------------------%

:- type implementation_language
    --->    impl_lang_mercury
    ;       impl_lang_foreign(foreign_language).

    % The type of goals that have been given for a pred.

:- type goal_type
    --->    goal_type_clause
    ;       goal_type_foreign               % pragma foreign_proc(...)
    ;       goal_type_clause_and_foreign    % both clauses and foreign_procs
    ;       goal_type_promise(promise_type)
    ;       goal_type_none.

    % Note: `liveness' and `liveness_info' record liveness in the sense
    % used by code generation.  This is *not* the same thing as the notion
    % of liveness used by mode analysis!  See compiler/notes/glossary.html.

:- type liveness_info   ==  set(prog_var).  % The live variables

:- type liveness
    --->    live
    ;       dead.

:- type arg_info
    --->    arg_info(
                arg_loc,    % stored location
                arg_mode    % mode of top functor
            ).

    % The `arg_mode' specifies the mode of the top-level functor
    % (excluding `no_tag' functors, since those have no representation).
    % It is used by the code generators for determining how to
    % pass the argument.
    %
    % For the LLDS back-end, top_in arguments are passed in registers,
    % and top_out values are returned in registers; top_unused
    % values are not passed at all, but they are treated as if
    % they were top_out for the purpose of assigning arguments
    % to registers.  (So e.g. if a det procedure has three arguments
    % with arg_modes top_out, top_unused, and top_out respectively,
    % the last argument will be returned in register r3, not r2.)
    %
    % For the MLDS back-end, top_in values are passed as arguments;
    % top_out values are normally passed by reference, except that
    %   - if the procedure is model_nondet, and the --nondet-copy-out
    %     option is set, top_out values are passed by value to
    %     the continuation function
    %   - if the procedure is model_det or model_semi,
    %     and the --det-copy-out option is set,
    %     top_out arguments in the HLDS are mapped to (multiple)
    %     return values in the MLDS
    %   - if the HLDS function return value for a det function has
    %     mode `top_out', it is mapped to an MLDS return value.
    % top_unused arguments are not passed at all.
    %
:- type arg_mode
    --->    top_in
    ;       top_out
    ;       top_unused.

:- type arg_loc     ==  int.

    % The type `import_status' describes whether an entity (a predicate,
    % type, inst, or mode) is local to the current module, exported from
    % the current module, or imported from some other module.
    % Only predicates can have status pseudo_exported or pseudo_imported.
    % Only types can have status abstract_exported or abstract_imported.
    %
:- type import_status
    --->    status_external(import_status)
                % Declared `:- external'. This means that the implementation
                % for this procedure will be provided by some external source,
                % rather than via Mercury clauses (including `pragma
                % foreign_code' clauses). It can be through the use of another
                % language, or it could be through some other method we haven't
                % thought of yet.
    ;       status_imported(import_locn)
                % Defined in the interface of some other module.
    ;       status_opt_imported
                % Defined in the optimization interface of another module.
    ;       status_abstract_imported
                % Describes a type with only an abstract declaration imported,
                % maybe with the body of the type imported from a .opt file.
    ;       status_pseudo_imported
                % This is used for entities that are defined in the interface
                % of some other module but for which we may generate some code
                % in this module - in particular, this is used for unification
                % predicates (see comments in unify_proc.m).
    ;       status_exported
                % Defined in the interface of this module.
    ;       status_opt_exported
                % A local item for which the import-status has been changed
                % due to its presence in the .opt files
                % (intermod.adjust_pred_import_status).
    ;       status_abstract_exported
                % Describes a type with only an abstract declaration exported.
    ;       status_pseudo_exported
                % The converse of pseudo_imported; this means that only the
                % (in, in) mode of a unification is exported.
    ;       status_exported_to_submodules
                % Defined in the implementation of this module, and thus in
                % a sense local, but the module contains sub-modules, so the
                % entity needs to be exported to those sub-modules.
    ;       status_local.
                % Defined in the implementation of this module, and the module
                % does not contain any sub-modules.

    % Returns yes if the status indicates that the item was in any way exported
    % -- that is, if it could be used by any other module, or by sub-modules
    % of this module.
    %
:- func status_is_exported(import_status) = bool.

    % Returns yes if the status indicates that the item was exported
    % to importing modules (not just to sub-modules).
    %
:- func status_is_exported_to_non_submodules(import_status) = bool.

    % Returns yes if the status indicates that the item was in any way imported
    % -- that is, if it was defined in some other module, or in a sub-module
    % of this module. This is the opposite of status_defined_in_this_module.
    %
:- func status_is_imported(import_status) = bool.

    % Returns yes if the status indicates that the item was defined in this
    % module. This is the opposite of status_is_imported.
    %
:- func status_defined_in_this_module(import_status) = bool.

    % Returns yes if the import_status indicates the item came from
    % the implementation section.
    %
:- func status_defined_in_impl_section(import_status) = bool.

    % Are calls from a predicate with the given import_status always fully
    % qualified. For calls occurring in `.opt' files this will return
    % `is_fully_qualified', otherwise `may_be_partially_qualified'.
    %
:- func calls_are_fully_qualified(pred_markers) = is_fully_qualified.

    % Predicates can be marked with various boolean flags, called "markers".

    % An abstract set of markers.
:- type pred_markers.

:- type marker
    --->    marker_stub % The predicate has no clauses. typecheck.m will
                        % generate a body for the predicate which just throws
                        % an exception. This marker is used to tell purity
                        % analysis and determinism analysis not to issue
                        % warnings for these predicates.

    ;       marker_infer_type
                        % Requests type inference for the predicate.
                        % These markers are inserted by make_hlds
                        % for undeclared predicates.

    ;       marker_infer_modes
                        % Requests mode inference for the predicate.
                        % These markers are inserted by make_hlds
                        % for undeclared predicates.

    ;       marker_obsolete
                        % Requests warnings if this predicate is used.
                        % Used for pragma(obsolete).


    ;       marker_user_marked_inline
                        % The user requests that this be predicate should
                        % be inlined, even if it exceeds the usual size limits.
                        % Used for pragma(inline).
                        % Mutually exclusive with user_marked_no_inline.

    ;       marker_user_marked_no_inline
                        % The user requests that this be predicate should
                        % not be inlined. Used for pragma(no_inline).
                        % Mutually exclusive with user_marked_inline.

    ;       marker_heuristic_inline
                        % The compiler (meaning probably inlining.m) requests
                        % that this predicate be inlined. Does not override
                        % user_marked_no_inline.

    ;       marker_class_method
                        % Requests that this predicate be transformed into
                        % the appropriate call to a class method.

    ;       marker_class_instance_method
                        % This predicate was automatically generated for the
                        % implementation of a class method for an instance.

    ;       marker_named_class_instance_method
                        % This predicate was automatically generated for the
                        % implementation of a class method for an instance,
                        % and the instance was defined using the named syntax
                        % (e.g. "pred(...) is ...") rather than the clause
                        % syntax. (For such predicates, we output slightly
                        % different error messages.)

    ;       marker_is_impure
                        % Requests that no transformation that would be
                        % inappropriate for impure code be performed on calls
                        % to this predicate. This includes reordering calls
                        % to it relative to other goals (in both conjunctions
                        % and disjunctions), and removing redundant calls
                        % to it.

    ;       marker_is_semipure
                        % Requests that no transformation that would be
                        % inappropriate for semipure code be performed on
                        % calls to this predicate. This includes removing
                        % redundant calls to it on different sides of an
                        % impure goal.

    ;       marker_promised_pure
                        % Requests that calls to this predicate be transformed
                        % as usual, despite any impure or semipure markers
                        % present.

    ;       marker_promised_semipure
                        % Requests that calls to this predicate be treated as
                        % semipure, despite any impure calls in the body.

    ;       marker_promised_equivalent_clauses
                        % Promises that all modes of the predicate have
                        % equivalent semantics, event if they are defined by
                        % different sets of mode-specific clauses.

    % The terminates and does_not_terminate pragmas are kept as markers
    % to ensure that conflicting declarations are not made by the user.
    % Otherwise, the information could be added to the ProcInfos directly.

    ;       marker_terminates
                        % The user guarantees that this predicate will
                        % terminate for all (finite?) input.
    ;       marker_does_not_terminate
                        % States that this predicate does not terminate.
                        % This is useful for pragma foreign_code, which the
                        % compiler assumes to be terminating.
    ;       marker_check_termination
                        % The user requires the compiler to guarantee
                        % the termination of this predicate. If the compiler
                        % cannot guarantee termination then it must give an
                        % error message.

    ;       marker_calls_are_fully_qualified
                        % All calls in this predicate are fully qualified.
                        % This occurs for predicates read from `.opt' files
                        % and compiler-generated predicates.
    ;       marker_mode_check_clauses
                        % Each clause of the predicate should be modechecked
                        % separately. Used for predicates defined by lots of
                        % clauses (usually facts) for which the compiler's
                        % quadratic behavior during mode checking (in
                        % inst_match.bound_inst_list_contains_instname and
                        % instmap.merge) would be unacceptable.

    ;       marker_may_have_parallel_conj
                        % The predicate may contain parallel conjunctions.
                        % It should be run through the dependent parallel
                        % conjunction transformation.

    ;       marker_mutable_access_pred.
                        % This predicate is part of the machinery used to
                        % access mutables.  This marker is used to inform
                        % inlining that we should _always_ attempt to
                        % inline this predicate across module boundaries.

    % An abstract set of attributes.
:- type pred_attributes.

:- type attribute
    --->    custom(mer_type).
                        % A custom attribute, indended to be associated
                        % with this predicate in the underlying
                        % implementation.

:- type pred_transformation
    --->    transform_higher_order_specialization(
                int % Sequence number among the higher order
                    % specializations of the original predicate.
            )
    ;       transform_higher_order_type_specialization(
                int % The procedure number of the original procedure.
            )
    ;       transform_type_specialization(
                assoc_list(int, mer_type)
                    % The substitution from type variables (represented by
                    % the integers) to types (represented by the terms).
            )
    ;       transform_unused_argument_elimination(
                list(int) % The list of eliminated argument numbers.
            )
    ;       transform_accumulator(
                list(int)
                    % The list of the numbers of the variables in the original
                    % predicate interface that have been converted to
                    % accumulators.
            )
    ;       transform_loop_invariant(
                int % The procedure number of the original procedure.
            )
    ;       transform_tuple(
                int % The procedure number of the original procedure.
            )
    ;       transform_untuple(
                int % The procedure number of the original procedure.
            )
    ;       transform_dependent_parallel_conjunction
    ;       transform_return_via_ptr(
                proc_id,
                    % The id of the procedure this predicate is derived from.
                list(int)
                    % The arguments in these positions are returned via
                    % pointer.
            )
    ;       transform_table_generator
    ;       transform_dnf(
                int % This predicate was originally part of a predicate
                    % transformed into disjunctive normal form; this integer
                    % gives the part number.
            ).

:- type pred_creation
    --->    deforestation
                % I/O tabling will create a new predicate if the predicate
                % to be I/O tabled must not be inlined.
    ;       io_tabling.

:- type pred_origin
    --->    origin_special_pred(special_pred)
                % If the predicate is a unify, compare, index or initialisation
                % predicate, specify which one, and for which type constructor.
    ;       origin_instance_method(instance_method_constraints)
                % If this predicate is a class method implementation, record
                % extra information about the class context to allow
                % polymorphism.m to correctly set up the extra type_info
                % and typeclass_info arguments.
    ;       origin_transformed(pred_transformation, pred_origin, pred_id)
                % The predicate is a transformed version of another predicate,
                % whose origin and identity are given by the second and third
                % arguments.
    ;       origin_created(pred_creation)
                % The predicate was created by the compiler, and there is no
                % information available on where it came from.
    ;       origin_assertion(string, int)
                % The predicate represents an assertion.
    ;       origin_lambda(string, int, int)
                % The predicate is a higher-order manifest constant.
                % The arguments specify its location in the source, as a
                % filename/line number pair, and a sequence number used to
                % distinguish multiple lambdas on the same line.
    ;       origin_user(sym_name).
                % The predicate is a normal user-written predicate;
                % the string is its name.

    % pred_info_init(ModuleName, SymName, Arity, PredOrFunc, Context,
    %   Origin, Status, GoalType, Markers, ArgTypes, TypeVarSet,
    %   ExistQVars, ClassContext, ClassProofs, ClassConstraintMap,
    %   User, ClausesInfo, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults.
    %
:- pred pred_info_init(module_name::in, sym_name::in, arity::in,
    pred_or_func::in, prog_context::in, pred_origin::in, import_status::in,
    goal_type::in, pred_markers::in, list(mer_type)::in, tvarset::in,
    existq_tvars::in, prog_constraints::in, constraint_proof_map::in,
    constraint_map::in, clauses_info::in, pred_info::out) is det.

    % pred_info_create(ModuleName, SymName, PredOrFunc, Context, Origin,
    %   Status, Markers, TypeVarSet, ExistQVars, ArgTypes,
    %   ClassContext, Assertions, User, ProcInfo, ProcId, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults. The given
    % proc_info becomes the only procedure of the predicate (currently)
    % and its proc_id is returned as the second last argument.
    %
:- pred pred_info_create(module_name::in, sym_name::in, pred_or_func::in,
    prog_context::in, pred_origin::in, import_status::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in, prog_constraints::in,
    set(assert_id)::in, proc_info::in, proc_id::out, pred_info::out) is det.

    % define_new_pred(Origin, Goal, CallGoal, Args, ExtraArgs,
    %   InstMap, PredName, TVarSet, VarTypes, ClassContext,
    %   TVarMap, TCVarMap, VarSet, Markers, IsAddressTaken,
    %   !ModuleInfo, PredProcId)
    %
    % Create a new predicate for the given goal, returning a goal to
    % call the created predicate. ExtraArgs is the list of extra
    % type_infos and typeclass_infos required by typeinfo liveness
    % which were added to the front of the argument list.
    %
:- pred define_new_pred(pred_origin::in,
    hlds_goal::in, hlds_goal::out, list(prog_var)::in, list(prog_var)::out,
    instmap::in, string::in, tvarset::in, vartypes::in,
    prog_constraints::in, rtti_varmaps::in, prog_varset::in,
    inst_varset::in, pred_markers::in, is_address_taken::in,
    module_info::in, module_info::out, pred_proc_id::out) is det.

    % Various predicates for accessing the information stored in the
    % pred_id and pred_info data structures.
    %
:- type head_type_params == list(tvar).

:- func pred_info_module(pred_info) =  module_name.
:- func pred_info_name(pred_info) = string.

    % Pred_info_orig_arity returns the arity of the predicate
    % *not* counting inserted type_info arguments for polymorphic preds.
    %
:- func pred_info_orig_arity(pred_info) = arity.

    % N-ary functions are converted into N+1-ary predicates.
    % (Clauses are converted in make_hlds, but calls to functions
    % cannot be converted until after type-checking, once we have
    % resolved overloading. So we do that during mode analysis.)
    % The `is_pred_or_func' field of the pred_info records whether
    % a pred_info is really for a predicate or whether it is for
    % what was originally a function.
    %
:- func pred_info_is_pred_or_func(pred_info) = pred_or_func.

:- pred pred_info_context(pred_info::in, prog_context::out) is det.
:- pred pred_info_get_origin(pred_info::in, pred_origin::out) is det.
:- pred pred_info_get_import_status(pred_info::in, import_status::out) is det.
:- pred pred_info_get_goal_type(pred_info::in, goal_type::out) is det.
:- pred pred_info_get_markers(pred_info::in, pred_markers::out) is det.
:- pred pred_info_get_attributes(pred_info::in, pred_attributes::out) is det.
:- pred pred_info_get_arg_types(pred_info::in, list(mer_type)::out) is det.
:- pred pred_info_get_typevarset(pred_info::in, tvarset::out) is det.
:- pred pred_info_get_tvar_kinds(pred_info::in, tvar_kind_map::out) is det.
:- pred pred_info_get_exist_quant_tvars(pred_info::in, existq_tvars::out)
    is det.
:- pred pred_info_get_existq_tvar_binding(pred_info::in, tsubst::out) is det.
:- pred pred_info_get_head_type_params(pred_info::in, head_type_params::out)
    is det.
:- pred pred_info_get_class_context(pred_info::in, prog_constraints::out)
    is det.
:- pred pred_info_get_constraint_proofs(pred_info::in,
    constraint_proof_map::out) is det.
:- pred pred_info_get_constraint_map(pred_info::in,
    constraint_map::out) is det.
:- pred pred_info_get_unproven_body_constraints(pred_info::in,
    list(prog_constraint)::out) is det.
:- pred pred_info_get_assertions(pred_info::in, set(assert_id)::out) is det.
:- pred pred_info_clauses_info(pred_info::in, clauses_info::out) is det.
:- pred pred_info_get_procedures(pred_info::in, proc_table::out) is det.

    % Setting the name of a pred_info after its creation won't remove its name
    % from the indexes under its old name or insert into the indexes under its
    % new name. If is therefore safe to do this only after all the passes that
    % look up predicates by name.
    %
:- pred pred_info_set_name(string::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_set_origin(pred_origin::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_import_status(import_status::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_markers(pred_markers::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_attributes(pred_attributes::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_typevarset(tvarset::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_tvar_kinds(tvar_kind_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_existq_tvar_binding(tsubst::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_head_type_params(head_type_params::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_class_context(prog_constraints::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_proofs(constraint_proof_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_map(constraint_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_unproven_body_constraints(list(prog_constraint)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_assertions(set(assert_id)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_clauses_info(clauses_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_procedures(proc_table::in,
    pred_info::in, pred_info::out) is det.

:- func inst_graph_info(pred_info) = inst_graph_info.
:- func 'inst_graph_info :='(pred_info, inst_graph_info) = pred_info.

    % Mode information for the arguments of a procedure.
    % The first map gives the instantiation state on entry of the
    % node corresponding to the prog_var.  The second map gives
    % the instantation state on exit.
:- type arg_modes_map == pair(map(prog_var, bool)).

:- func modes(pred_info) = list(arg_modes_map).
:- func 'modes :='(pred_info, list(arg_modes_map)) = pred_info.

    % Return a list of all the proc_ids for the valid modes
    % of this predicate.  This does not include candidate modes
    % that were generated during mode inference but which mode
    % inference found were not valid modes.
:- func pred_info_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes
    % of this predicate, including invalid modes.
:- func pred_info_all_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes
    % of this predicate that are not imported.
:- func pred_info_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes
    % of this predicate that are not imported
    % (including invalid modes).
:- func pred_info_all_non_imported_procids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the valid modes
    % of this predicate that are exported.
:- func pred_info_exported_procids(pred_info) = list(proc_id).

    % Remove a procedure from the pred_info.
:- pred pred_info_remove_procid(proc_id::in, pred_info::in, pred_info::out)
    is det.

:- pred pred_info_get_arg_types(pred_info::in, tvarset::out, existq_tvars::out,
    list(mer_type)::out) is det.

:- pred pred_info_set_arg_types(tvarset::in, existq_tvars::in,
    list(mer_type)::in, pred_info::in, pred_info::out) is det.

:- pred pred_info_get_univ_quant_tvars(pred_info::in, existq_tvars::out)
    is det.

:- pred pred_info_proc_info(pred_info::in, proc_id::in, proc_info::out) is det.

:- pred pred_info_set_proc_info(proc_id::in, proc_info::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

    % pred_info_is_exported does *not* include predicates which are
    % exported_to_submodules or pseudo_exported
:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_opt_exported(pred_info::in) is semidet.

:- pred pred_info_is_exported_to_submodules(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

    % procedure_is_exported includes all modes of exported or
    % exported_to_submodules predicates, plus the in-in mode
    % for pseudo_exported unification predicates.
    %
:- pred procedure_is_exported(module_info::in, pred_info::in, proc_id::in)
    is semidet.

    % Set the import_status of the predicate to `imported'.
    % This is used for `:- external(foo/2).' declarations.
    %
:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

    % Do we have a clause goal type?
    % (this means either "clauses" or "clauses_and_pragmas")
    %
:- pred pred_info_clause_goal_type(pred_info::in) is semidet.

    % Do we have a pragma goal type?
    % (this means either "pragmas" or "clauses_and_pragmas")
    %
:- pred pred_info_pragma_goal_type(pred_info::in) is semidet.

:- pred pred_info_update_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.

    % Succeeds if there was a `:- pragma inline(...)' declaration
    % for this predicate. Note that the compiler may decide
    % to inline a predicate even if there was no pragma inline(...)
    % declaration for that predicate.
    %
:- pred pred_info_requested_inlining(pred_info::in) is semidet.

    % Succeeds if there was a `:- pragma no_inline(...)' declaration
    % for this predicate.
    %
:- pred pred_info_requested_no_inlining(pred_info::in) is semidet.

:- pred pred_info_get_purity(pred_info::in, purity::out) is det.

:- pred pred_info_get_promised_purity(pred_info::in, purity::out) is det.

:- pred pred_info_infer_modes(pred_info::in) is semidet.

:- pred purity_to_markers(purity::in, pred_markers::out) is det.

:- pred terminates_to_markers(proc_terminates::in, pred_markers::out) is det.

:- pred pred_info_get_call_id(pred_info::in, simple_call_id::out) is det.

    % Create an empty set of markers.
    %
:- pred init_markers(pred_markers::out) is det.

    % Check if a particular is in the set.
    %
:- pred check_marker(pred_markers::in, marker::in) is semidet.

    % Add a marker to the set.
    %
:- pred add_marker(marker::in, pred_markers::in, pred_markers::out) is det.

    % Remove a marker from the set.
    %
:- pred remove_marker(marker::in, pred_markers::in, pred_markers::out) is det.

    % Convert the set to a list.
    %
:- pred markers_to_marker_list(pred_markers::in, list(marker)::out) is det.

:- pred marker_list_to_markers(list(marker)::in, pred_markers::out) is det.

    % Create an empty set of attributes.
    %
:- pred init_attributes(pred_attributes::out) is det.

    % Check if a particular is in the set.
    %
:- pred check_attribute(pred_attributes::in, attribute::in) is semidet.

    % Add a attribute to the set.
    %
:- pred add_attribute(attribute::in, pred_attributes::in, pred_attributes::out)
    is det.

    % Remove a attribute from the set.
    %
:- pred remove_attribute(attribute::in,
    pred_attributes::in, pred_attributes::out) is det.

    % Convert the set to a list.
    %
:- pred attributes_to_attribute_list(pred_attributes::in,
    list(attribute)::out) is det.

:- pred attribute_list_to_attributes(list(attribute)::in,
    pred_attributes::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type pred_id     ==  int.
:- type proc_id     ==  int.

shroud_pred_id(PredId) = shrouded_pred_id(PredId).
shroud_proc_id(ProcId) = shrouded_proc_id(ProcId).
shroud_pred_proc_id(proc(PredId, ProcId)) =
    shrouded_pred_proc_id(PredId, ProcId).

unshroud_pred_id(shrouded_pred_id(PredId)) = PredId.
unshroud_proc_id(shrouded_proc_id(ProcId)) = ProcId.
unshroud_pred_proc_id(shrouded_pred_proc_id(PredId, ProcId)) =
    proc(PredId, ProcId).

pred_id_to_int(PredId, PredId).
pred_id_to_int(PredId) = PredId.

proc_id_to_int(ProcId, ProcId).
proc_id_to_int(ProcId) = ProcId.

initial_pred_id = 0.
initial_proc_id = 0.

invalid_pred_id = -1.
invalid_proc_id = -1.

next_pred_id(PredId, NextPredId) :-
    NextPredId = PredId + 1.

in_in_unification_proc_id(0).

    % We could store the next available ModeId rather than recomputing
    % it on demand, but it is probably more efficient this way.
    %
next_mode_id(Procs, ModeId) :-
    map.to_assoc_list(Procs, List),
    list.length(List, ModeInt),
    proc_id_to_int(ModeId, ModeInt).

status_is_exported(status_imported(_)) =             no.
status_is_exported(status_external(_)) =             no.
status_is_exported(status_abstract_imported) =       no.
status_is_exported(status_pseudo_imported) =         no.
status_is_exported(status_opt_imported) =            no.
status_is_exported(status_exported) =                yes.
status_is_exported(status_opt_exported) =            yes.
status_is_exported(status_abstract_exported) =       yes.
status_is_exported(status_pseudo_exported) =         yes.
status_is_exported(status_exported_to_submodules) =  yes.
status_is_exported(status_local) =                   no.

status_is_exported_to_non_submodules(Status) =
    (
        status_is_exported(Status) = yes,
        Status \= status_exported_to_submodules
    ->
        yes
    ;
        no
    ).

status_is_imported(Status) = bool.not(status_defined_in_this_module(Status)).

status_defined_in_this_module(status_imported(_)) =             no.
status_defined_in_this_module(status_external(_)) =             no.
status_defined_in_this_module(status_abstract_imported) =       no.
status_defined_in_this_module(status_pseudo_imported) =         no.
status_defined_in_this_module(status_opt_imported) =            no.
status_defined_in_this_module(status_exported) =                yes.
status_defined_in_this_module(status_opt_exported) =            yes.
status_defined_in_this_module(status_abstract_exported) =       yes.
status_defined_in_this_module(status_pseudo_exported) =         yes.
status_defined_in_this_module(status_exported_to_submodules) =  yes.
status_defined_in_this_module(status_local) =                   yes.

status_defined_in_impl_section(status_abstract_exported) =      yes.
status_defined_in_impl_section(status_exported_to_submodules) = yes.
status_defined_in_impl_section(status_local) =                  yes.
status_defined_in_impl_section(status_opt_imported) =           no.
status_defined_in_impl_section(status_abstract_imported) =      no.
status_defined_in_impl_section(status_pseudo_imported) =        no.
status_defined_in_impl_section(status_exported) =               no.
status_defined_in_impl_section(status_opt_exported) =           yes.
status_defined_in_impl_section(status_pseudo_exported) =        no.
status_defined_in_impl_section(status_external(Status)) =
    status_defined_in_impl_section(Status).
status_defined_in_impl_section(status_imported(ImportLocn)) =
    import_locn_defined_in_impl_section(ImportLocn).

:- func import_locn_defined_in_impl_section(import_locn) = bool.

import_locn_defined_in_impl_section(import_locn_implementation) = yes.
import_locn_defined_in_impl_section(import_locn_interface) = yes.
import_locn_defined_in_impl_section(import_locn_ancestor) = yes.
import_locn_defined_in_impl_section(import_locn_ancestor_private_interface)
    = yes.

calls_are_fully_qualified(Markers) =
    ( check_marker(Markers, marker_calls_are_fully_qualified) ->
        is_fully_qualified
    ;
        may_be_partially_qualified
    ).

%-----------------------------------------------------------------------------%

    % The information specific to a predicate, as opposed to a procedure.
    % (Functions count as predicates.)
    %
    % Note that it is an invariant that any type_info-related
    % variables in the arguments of a predicate must precede any
    % polymorphically-typed arguments whose type depends on the
    % values of those type_info-related variables;
    % accurate GC for the MLDS back-end relies on this.
:- type pred_info
    --->    pred_info(
                % Module in which pred occurs.
                module_name         :: module_name,

                % Predicate name.
                name                :: string,

                % The arity of the pred (*not* counting any inserted type_info
                % arguments).
                orig_arity          :: arity,

                % Whether this "predicate" is really a predicate or a function.
                is_pred_or_func     :: pred_or_func,

                % The location (line #) of the :- pred decl.
                context             :: prog_context,

                % Where did the predicate come from.
                pred_origin         :: pred_origin,

                import_status       :: import_status,

                % Whether the goals seen so far, if any, for this predicate
                % are clauses or foreign_code(...) pragmas.
                goal_type           :: goal_type,

                % Various boolean flags.
                markers             :: pred_markers,

                % Various attributes.
                attributes          :: pred_attributes,

                % Argument types.
                arg_types           :: list(mer_type),

                % Names of type vars in the predicate's type declaration.
                decl_typevarset     :: tvarset,

                % Names of type vars in the predicate's type declaration
                % or in the variable type assignments.
                typevarset          :: tvarset,

                % Kinds of the type vars.
                tvar_kinds          :: tvar_kind_map,

                % The set of existentially quantified type variables in the
                % predicate's type declaration.
                exist_quant_tvars   :: existq_tvars,

                % The statically known bindings of existentially quantified
                % type variables inside this predicate. This field is set
                % at the end of the polymorphism stage.
                existq_tvar_binding :: tsubst,

                % The set of type variables which the body of the predicate
                % can't bind, and whose type_infos are produced elsewhere.
                % This includes universally quantified head types (the
                % type_infos are passed in) plus existentially quantified types
                % in preds called from the body (the type_infos are returned
                % from the called predicates). Computed during type checking.
                head_type_params    :: head_type_params,

                % The class constraints on the type variables in the
                % predicate's type declaration.
                class_context       :: prog_constraints,

                % Explanations of how redundant constraints were eliminated.
                % These are needed by polymorphism.m to work out where to get
                % the typeclass_infos from. Computed during type checking.
                constraint_proofs   :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                % Computed during type checking.
                constraint_map      :: constraint_map,

                % Unproven class constraints on type variables in the
                % predicate's body, if any (if this remains non-empty after
                % type checking has finished, post_typecheck.m will report a
                % type error).
                unproven_body_constraints :: list(prog_constraint),

                % The predicate's inst graph, for constraint based
                % mode analysis.
                inst_graph_info     :: inst_graph_info,

                % Mode information extracted from constraint based
                % mode analysis.
                modes               :: list(arg_modes_map),

                % List of assertions which mention this predicate.
                assertions          :: set(assert_id),

                clauses_info        :: clauses_info,

                procedures          :: proc_table
            ).

pred_info_init(ModuleName, SymName, Arity, PredOrFunc, Context, Origin, Status,
        GoalType, Markers, ArgTypes, TypeVarSet, ExistQVars, ClassContext,
        ClassProofs, ClassConstraintMap, ClausesInfo, PredInfo) :-
    PredName = unqualify_name(SymName),
    sym_name_get_module_name(SymName, ModuleName, PredModuleName),
    type_vars_list(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    Attributes = [],
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(Kinds),
    map.init(ExistQVarBindings),
    UnprovenBodyConstraints = [],
    set.init(Assertions),
    map.init(Procs),
    PredInfo = pred_info(PredModuleName, PredName, Arity, PredOrFunc,
        Context, Origin, Status, GoalType, Markers, Attributes,
        ArgTypes, TypeVarSet, TypeVarSet, Kinds, ExistQVars, ExistQVarBindings,
        HeadTypeParams, ClassContext, ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, inst_graph_info_init, [],
        Assertions, ClausesInfo, Procs).

pred_info_create(ModuleName, SymName, PredOrFunc, Context, Origin, Status,
        Markers, ArgTypes, TypeVarSet, ExistQVars, ClassContext,
        Assertions, ProcInfo, ProcId, PredInfo) :-
    list.length(ArgTypes, Arity),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_headvars(ProcInfo, HeadVars),
    PredName = unqualify_name(SymName),
    Attributes = [],
    map.init(ClassProofs),
    map.init(ClassConstraintMap),
    type_vars_list(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(Kinds),
    map.init(ExistQVarBindings),
    UnprovenBodyConstraints = [],

    % The empty list of clauses is a little white lie.
    Clauses = init_clauses_rep,
    map.init(TVarNameMap),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    HasForeignClauses = no,
    ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
        HeadVars, Clauses, RttiVarMaps, HasForeignClauses),

    map.init(Procs0),
    next_mode_id(Procs0, ProcId),
    map.det_insert(Procs0, ProcId, ProcInfo, Procs),

    PredInfo = pred_info(ModuleName, PredName, Arity, PredOrFunc,
        Context, Origin, Status, goal_type_clause, Markers, Attributes,
        ArgTypes, TypeVarSet, TypeVarSet, Kinds, ExistQVars, ExistQVarBindings,
        HeadTypeParams, ClassContext, ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, inst_graph_info_init, [], Assertions,
        ClausesInfo, Procs).

define_new_pred(Origin, Goal0, Goal, ArgVars0, ExtraTypeInfos,
        InstMap0, PredName, TVarSet, VarTypes0, ClassContext,
        RttiVarMaps, VarSet0, InstVarSet, Markers,
        IsAddressTaken, ModuleInfo0, ModuleInfo, PredProcId) :-
    Goal0 = _GoalExpr - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

    % XXX The set of existentially quantified type variables
    % here might not be correct.
    ExistQVars = [],

    % If interface typeinfo liveness is set, all type_infos for the
    % arguments need to be passed in, not just the ones that are used.
    % Similarly if the address of a procedure of this predicate is taken,
    % so that we can copy the closure.
    module_info_get_globals(ModuleInfo0, Globals),
    ExportStatus = status_local,
    non_special_interface_should_use_typeinfo_liveness(ExportStatus,
        IsAddressTaken, Globals, TypeInfoLiveness),
    (
        TypeInfoLiveness = yes,
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        goal_util.extra_nonlocal_typeinfos(RttiVarMaps, VarTypes0,
            ExistQVars, NonLocals, ExtraTypeInfos0),
        set.delete_list(ExtraTypeInfos0, ArgVars0, ExtraTypeInfos1),
        set.to_sorted_list(ExtraTypeInfos1, ExtraTypeInfos),
        list.append(ExtraTypeInfos, ArgVars0, ArgVars)
    ;
        TypeInfoLiveness = no,
        ArgVars = ArgVars0,
        ExtraTypeInfos = []
    ),

    goal_info_get_context(GoalInfo, Context),
    goal_info_get_determinism(GoalInfo, Detism),
    compute_arg_types_modes(ArgVars, VarTypes0, InstMap0, InstMap,
        ArgTypes, ArgModes),

    module_info_get_name(ModuleInfo0, ModuleName),
    SymName = qualified(ModuleName, PredName),

        % Remove unneeded variables from the vartypes and varset.
    goal_util.goal_vars(Goal0, GoalVars0),
    set.insert_list(GoalVars0, ArgVars, GoalVars),
    map.select(VarTypes0, GoalVars, VarTypes),
    varset.select(VarSet0, GoalVars, VarSet),

        % Approximate the termination information for the new procedure.
    ( goal_cannot_loop(ModuleInfo0, Goal0) ->
        TermInfo = yes(cannot_loop(unit))
    ;
        TermInfo = no
    ),

    MaybeDeclaredDetism = no,
    proc_info_create(Context, VarSet, VarTypes, ArgVars, InstVarSet,
        ArgModes, MaybeDeclaredDetism, Detism, Goal0,
        RttiVarMaps, IsAddressTaken, ProcInfo0),
    proc_info_set_maybe_termination_info(TermInfo, ProcInfo0, ProcInfo),

    set.init(Assertions),

    pred_info_create(ModuleName, SymName, predicate, Context, Origin,
        ExportStatus, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Assertions, ProcInfo, ProcId, PredInfo),

    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(PredInfo, PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo),

    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
    Goal = GoalExpr - GoalInfo,
    PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(list(prog_var)::in, vartypes::in,
    instmap::in, instmap::in, list(mer_type)::out, list(mer_mode)::out) is det.

compute_arg_types_modes([], _, _, _, [], []).
compute_arg_types_modes([Var | Vars], VarTypes, InstMap0, InstMap,
        [Type | Types], [Mode | Modes]) :-
    map.lookup(VarTypes, Var, Type),
    instmap.lookup_var(InstMap0, Var, Inst0),
    instmap.lookup_var(InstMap, Var, Inst),
    Mode = (Inst0 -> Inst),
    compute_arg_types_modes(Vars, VarTypes, InstMap0, InstMap, Types, Modes).

%-----------------------------------------------------------------------------%

% The trivial access predicates.

pred_info_module(PI) = PI ^ module_name.
pred_info_name(PI) = PI ^ name.
pred_info_orig_arity(PI) = PI ^ orig_arity.

pred_info_is_pred_or_func(PI) = PI ^ is_pred_or_func.
pred_info_context(PI, PI ^ context).
pred_info_get_origin(PI, PI ^ pred_origin).
pred_info_get_import_status(PI, PI ^ import_status).
pred_info_get_goal_type(PI, PI ^ goal_type).
pred_info_get_markers(PI, PI ^ markers).
pred_info_get_attributes(PI, PI ^ attributes).
pred_info_get_arg_types(PI, PI ^ arg_types).
pred_info_get_typevarset(PI, PI ^ typevarset).
pred_info_get_tvar_kinds(PI, PI ^ tvar_kinds).
pred_info_get_exist_quant_tvars(PI, PI ^ exist_quant_tvars).
pred_info_get_existq_tvar_binding(PI, PI ^ existq_tvar_binding).
pred_info_get_head_type_params(PI, PI ^ head_type_params).
pred_info_get_class_context(PI, PI ^ class_context).
pred_info_get_constraint_proofs(PI, PI ^ constraint_proofs).
pred_info_get_constraint_map(PI, PI ^ constraint_map).
pred_info_get_unproven_body_constraints(PI, PI ^ unproven_body_constraints).
pred_info_get_assertions(PI, PI ^ assertions).
pred_info_clauses_info(PI, PI ^ clauses_info).
pred_info_get_procedures(PI, PI ^ procedures).

pred_info_set_name(X, PI, PI ^ name := X).
pred_info_set_origin(X, PI, PI ^ pred_origin := X).
pred_info_set_import_status(X, PI, PI ^ import_status := X).
pred_info_set_goal_type(X, PI, PI ^ goal_type := X).
pred_info_set_markers(X, PI, PI ^ markers := X).
pred_info_set_attributes(X, PI, PI ^ attributes := X).
pred_info_set_typevarset(X, PI, PI ^ typevarset := X).
pred_info_set_tvar_kinds(X, PI, PI ^ tvar_kinds := X).
pred_info_set_existq_tvar_binding(X, PI, PI ^ existq_tvar_binding := X).
pred_info_set_head_type_params(X, PI, PI ^ head_type_params := X).
pred_info_set_class_context(X, PI, PI ^ class_context := X).
pred_info_set_constraint_proofs(X, PI, PI ^ constraint_proofs := X).
pred_info_set_constraint_map(X, PI, PI ^ constraint_map := X).
pred_info_set_unproven_body_constraints(X, PI,
    PI ^ unproven_body_constraints := X).
pred_info_set_assertions(X, PI, PI ^ assertions := X).
pred_info_set_clauses_info(X, PI, PI ^ clauses_info := X).
pred_info_set_procedures(X, PI, PI ^ procedures := X).

%-----------------------------------------------------------------------------%

% The non-trivial access predicates.

pred_info_all_procids(PredInfo) = ProcIds :-
    ProcTable = PredInfo ^ procedures,
    map.keys(ProcTable, ProcIds).

pred_info_procids(PredInfo) = ValidProcIds :-
    AllProcIds = pred_info_all_procids(PredInfo),
    ProcTable = PredInfo ^ procedures,
    IsValid = (pred(ProcId::in) is semidet :-
        ProcInfo = map.lookup(ProcTable, ProcId),
        proc_info_is_valid_mode(ProcInfo)),
    list.filter(IsValid, AllProcIds, ValidProcIds).

pred_info_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ( ImportStatus = status_imported(_) ->
        ProcIds = []
    ; ImportStatus = status_external(_) ->
        ProcIds = []
    ; ImportStatus = status_pseudo_imported ->
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseduo_imported preds, procid 0 is imported
        list.delete_all(ProcIds0, 0, ProcIds)
    ;
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_all_non_imported_procids(PredInfo) = ProcIds :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ( ImportStatus = status_imported(_) ->
        ProcIds = []
    ; ImportStatus = status_external(_) ->
        ProcIds = []
    ; ImportStatus = status_pseudo_imported ->
        ProcIds0 = pred_info_procids(PredInfo),
        % for pseduo_imported preds, procid 0 is imported
        list.delete_all(ProcIds0, 0, ProcIds)
    ;
        ProcIds = pred_info_procids(PredInfo)
    ).

pred_info_exported_procids(PredInfo) = ProcIds :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    (
        ( ImportStatus = status_exported
        ; ImportStatus = status_opt_exported
        ; ImportStatus = status_exported_to_submodules
        )
    ->
        ProcIds = pred_info_procids(PredInfo)
    ;
        ImportStatus = status_pseudo_exported
    ->
        ProcIds = [0]
    ;
        ProcIds = []
    ).

pred_info_remove_procid(ProcId, !PredInfo) :-
    pred_info_get_procedures(!.PredInfo, Procs0),
    map.delete(Procs0, ProcId, Procs),
    pred_info_set_procedures(Procs, !PredInfo).

pred_info_get_arg_types(PredInfo, PredInfo ^ decl_typevarset,
        PredInfo ^ exist_quant_tvars, PredInfo ^ arg_types).

pred_info_set_arg_types(TypeVarSet, ExistQVars, ArgTypes, !PredInfo) :-
    !:PredInfo = !.PredInfo ^ decl_typevarset := TypeVarSet,
    !:PredInfo = !.PredInfo ^ exist_quant_tvars := ExistQVars,
    !:PredInfo = !.PredInfo ^ arg_types := ArgTypes.

pred_info_proc_info(PredInfo, ProcId, ProcInfo) :-
    ProcInfo = map.lookup(PredInfo ^ procedures, ProcId).

pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo) :-
    PredInfo = PredInfo0 ^ procedures :=
        map.set(PredInfo0 ^ procedures, ProcId, ProcInfo).

pred_info_is_imported(PredInfo) :-
    pred_info_get_import_status(PredInfo, Status),
    ( Status = status_imported(_)
    ; Status = status_external(_)
    ).

pred_info_is_pseudo_imported(PredInfo) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ImportStatus = status_pseudo_imported.

pred_info_is_exported(PredInfo) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ImportStatus = status_exported.

pred_info_is_opt_exported(PredInfo) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ImportStatus = status_opt_exported.

pred_info_is_exported_to_submodules(PredInfo) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ImportStatus = status_exported_to_submodules.

pred_info_is_pseudo_exported(PredInfo) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ImportStatus = status_pseudo_exported.

procedure_is_exported(ModuleInfo, PredInfo, ProcId) :-
    (
        pred_info_is_exported(PredInfo)
    ;
        pred_info_is_opt_exported(PredInfo)
    ;
        pred_info_is_exported_to_submodules(PredInfo)
    ;
        pred_info_is_pseudo_exported(PredInfo),
        in_in_unification_proc_id(ProcId)
    ;
        pred_info_get_import_status(PredInfo, ImportStatus),
        ImportStatus = status_external(ExternalImportStatus),
        status_is_exported(ExternalImportStatus) = yes
    ;
        pred_info_get_origin(PredInfo, origin_special_pred(SpecialPred)),
        SpecialPred = SpecialId - TypeCtor,
        module_info_get_type_table(ModuleInfo, TypeTable),
        % If the search fails, then TypeCtor must be a builtin type
        % constructor, such as the tuple constructor.
        map.search(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_in_exported_eqv(TypeDefn, yes),
        (
            SpecialId = spec_pred_unify,
            % The other proc_ids are module-specific.
            in_in_unification_proc_id(ProcId)
        ;
            SpecialId = spec_pred_compare
            % The declared modes are all global, and we don't
            % generate any modes for compare preds dynamically.
        ;
            SpecialId = spec_pred_index,
            % The index predicate is never called from anywhere
            % except the compare predicate.
            fail
        )
    ).

pred_info_mark_as_external(PredInfo0, PredInfo) :-
    PredInfo = PredInfo0 ^ import_status :=
        status_external(PredInfo0 ^ import_status).

pred_info_clause_goal_type(PredInfo) :-
    clause_goal_type(PredInfo ^ goal_type).

pred_info_pragma_goal_type(PredInfo) :-
    pragma_goal_type(PredInfo ^ goal_type).

:- pred clause_goal_type(goal_type::in) is semidet.

clause_goal_type(goal_type_clause).
clause_goal_type(goal_type_clause_and_foreign).

:- pred pragma_goal_type(goal_type::in) is semidet.

pragma_goal_type(goal_type_foreign).
pragma_goal_type(goal_type_clause_and_foreign).

pred_info_update_goal_type(GoalType1, !PredInfo) :-
    pred_info_get_goal_type(!.PredInfo, GoalType0),
    (
        GoalType0 = goal_type_none,
        GoalType = GoalType1
    ;
        GoalType0 = goal_type_foreign,
        ( clause_goal_type(GoalType1) ->
            GoalType = goal_type_clause_and_foreign
        ;
            GoalType = goal_type_foreign
        )
    ;
        GoalType0 = goal_type_clause,
        ( pragma_goal_type(GoalType1) ->
            GoalType = goal_type_clause_and_foreign
        ;
            GoalType = goal_type_clause
        )
    ;
        GoalType0 = goal_type_clause_and_foreign,
        GoalType = GoalType0
    ;
        GoalType0 = goal_type_promise(_),
        unexpected(this_file, "pred_info_update_goal_type")
    ),
    ( GoalType = GoalType0 ->
        % Avoid unnecessary memory allocation.
        true
    ;
        pred_info_set_goal_type(GoalType, !PredInfo)
    ).

pred_info_requested_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    (
        check_marker(Markers, marker_user_marked_inline)
    ;
        check_marker(Markers, marker_heuristic_inline)
    ).

pred_info_requested_no_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    check_marker(Markers, marker_user_marked_no_inline).

pred_info_get_purity(PredInfo0, Purity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, marker_is_impure) ->
        Purity = purity_impure
    ; check_marker(Markers, marker_is_semipure) ->
        Purity = purity_semipure
    ;
        Purity = purity_pure
    ).

pred_info_get_promised_purity(PredInfo0, PromisedPurity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( check_marker(Markers, marker_promised_pure) ->
        PromisedPurity = purity_pure
    ; check_marker(Markers, marker_promised_semipure) ->
        PromisedPurity = purity_semipure
    ;
        PromisedPurity = purity_impure
    ).

pred_info_infer_modes(PredInfo) :-
    pred_info_get_markers(PredInfo, Markers),
    check_marker(Markers, marker_infer_modes).

purity_to_markers(purity_pure, []).
purity_to_markers(purity_semipure, [marker_is_semipure]).
purity_to_markers(purity_impure, [marker_is_impure]).

terminates_to_markers(proc_terminates, [marker_terminates]).
terminates_to_markers(proc_does_not_terminate, [marker_does_not_terminate]).
terminates_to_markers(depends_on_mercury_calls, []).

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    type_vars_list(ArgTypes, ArgTypeVars0),
    list.sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    list.delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

%-----------------------------------------------------------------------------%

pred_info_get_call_id(PredInfo, SimpleCallId) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    SimpleCallId = simple_call_id(PredOrFunc, qualified(Module, Name), Arity).

%-----------------------------------------------------------------------------%

:- type pred_markers == list(marker).

init_markers([]).

check_marker(Markers, Marker) :-
    list.member(Marker, Markers).

add_marker(Marker, Markers, [Marker | Markers]).

remove_marker(Marker, Markers0, Markers) :-
    list.delete_all(Markers0, Marker, Markers).

markers_to_marker_list(Markers, Markers).

marker_list_to_markers(Markers, Markers).

:- type pred_attributes == list(attribute).

init_attributes([]).

check_attribute(Attributes, Attribute) :-
    list.member(Attribute, Attributes).

add_attribute(Attribute, Attributes, [Attribute | Attributes]).

remove_attribute(Attribute, Attributes0, Attributes) :-
    list.delete_all(Attributes0, Attribute, Attributes).

attributes_to_attribute_list(Attributes, Attributes).

attribute_list_to_attributes(Attributes, Attributes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Various predicates for accessing the proc_info data structure.

:- interface.

:- type is_address_taken
    --->    address_is_taken
    ;       address_is_not_taken.

:- type deep_profile_role
    --->    inner_proc(
                outer_proc  :: pred_proc_id
            )
    ;       outer_proc(
                inner_proc  :: pred_proc_id
            ).

:- type deep_recursion_info
    --->    deep_recursion_info(
                role        :: deep_profile_role,
                visible_scc :: list(visible_scc_data)
                            % If the procedure is not tail recursive, this list
                            % is empty. Otherwise, it contains outer-inner
                            % pairs of procedures in the visible SCC,
                            % including this procedure and its copy.
            ).

:- type visible_scc_data
    --->    visible_scc_data(
                vis_outer_proc  :: pred_proc_id,
                vis_inner_proc  :: pred_proc_id,
                rec_call_sites  :: list(int)
                                % A list of all the call site numbers that
                                % correspond to tail calls. (Call sites are
                                % numbered depth-first, left-to-right,
                                % from zero.)
            ).

:- type call_site_static_data           % defines MR_CallSiteStatic
    --->    normal_call(
                normal_callee           :: rtti_proc_label,
                normal_type_subst       :: string,
                normal_file_name        :: string,
                normal_line_number      :: int,
                normal_goal_path        :: goal_path
            )
    ;       special_call(
                special_file_name       :: string,
                special_line_number     :: int,
                special_goal_path       :: goal_path
            )
    ;       higher_order_call(
                higher_order_file_name  :: string,
                ho_line_number          :: int,
                ho_goal_path            :: goal_path
            )
    ;       method_call(
                method_file_name        :: string,
                method_line_number      :: int,
                method_goal_path        :: goal_path
            )
    ;       callback(
                callback_file_name      :: string,
                callback_line_number    :: int,
                callback_goal_path      :: goal_path
            ).

:- type hlds_proc_static
    --->    hlds_proc_static(   % defines part of MR_ProcStatic
                proc_static_file_name   :: string,
                proc_static_line_number :: int,
                proc_is_in_interface    :: bool,
                call_site_statics       :: list(call_site_static_data)
            ).

    % The hlds_deep_excp_vars gives the variables that hold the values returned
    % by the call port code, which are needed to let exception.throw perform
    % the work we need to do at the excp port.
:- type hlds_deep_excp_vars
    --->    hlds_deep_excp_vars(
                top_csd                 :: prog_var,
                middle_csd              :: prog_var,
                old_outermost           :: maybe(prog_var)
                                        % Needed only with the save/restore
                                        % approach, not the activation counting
                                        % approach.
            ).

:- type hlds_deep_layout
    --->    hlds_deep_layout(
                deep_layout_static      :: hlds_proc_static,
                deep_layout_excp        :: hlds_deep_excp_vars
            ).

:- type deep_profile_proc_info
    --->    deep_profile_proc_info(
                deep_rec        :: maybe(deep_recursion_info),
                deep_layout     :: maybe(hlds_deep_layout)
                                % The first field is set during the first,
                                % tail recursion part of the deep profiling
                                % transformation, if that is enabled.
                                % The deep_layout field is set during the
                                % second part; it will be bound to `no'
                                % before and during the first part, and
                                % to `yes' after the second. The contents
                                % of this field govern what will go into
                                % MR_ProcStatic structures.
            ).

:- type table_arg_info
    --->    table_arg_info(
                headvar         :: prog_var,
                slot_num        :: int,
                arg_type        :: mer_type
            ).

    % This type is analogous to llds:layout_locn, but it refers to slots in
    % the extended answer blocks used by I/O action tabling for declarative
    % debugging, not to lvals.
:- type table_locn
    --->    direct(int)
    ;       indirect(int, int).

    % This type differs from the type table_step_kind in
    % library/table_builtin.m in that (a) in gives more information about the
    % type of the corresponding argument (if this info is needed and
    % available), and (b) it doesn't have to handle dummy steps.
:- type table_trie_step
    --->    table_trie_step_int
    ;       table_trie_step_char
    ;       table_trie_step_string
    ;       table_trie_step_float
    ;       table_trie_step_dummy
    ;       table_trie_step_enum(int)
                % The int gives the number of alternatives in the enum type,
                % and thus the size of the corresponding trie node.
    ;       table_trie_step_user(mer_type)
    ;       table_trie_step_user_fast_loose(mer_type)
    ;       table_trie_step_poly
    ;       table_trie_step_poly_fast_loose
    ;       table_trie_step_typeinfo
    ;       table_trie_step_typeclassinfo
    ;       table_trie_step_promise_implied.

:- type table_arg_infos
    --->    table_arg_infos(
                list(table_arg_info),
                map(tvar, table_locn)
            ).

:- type proc_table_info
    --->    table_io_decl_info(
                % The information we need to display an I/O action to the user.
                %
                % The table_arg_type_infos correspond one to one to the
                % elements of the block saved for an I/O action. The first
                % element will be the pointer to the proc_layout of the
                % action's procedure.

                table_arg_infos
            )
    ;       table_gen_info(
                % The information we need to interpret the data structures
                % created by tabling for a procedure, except the information
                % (such as determinism) that is already available from
                % proc_layout structures.
                %
                % The table_arg_type_infos list first all the input arguments,
                % then all the output arguments.

                num_inputs          :: int,
                num_outputs         :: int,
                input_steps         :: list(table_trie_step),
                maybe_output_steps  :: maybe(list(table_trie_step)),
                gen_arg_infos       :: table_arg_infos
            ).

:- type untuple_proc_info
    --->    untuple_proc_info(
                map(prog_var, prog_vars)
            ).

:- pred proc_info_init(prog_context::in, arity::in, list(mer_type)::in,
    maybe(list(mer_mode))::in, list(mer_mode)::in, maybe(list(is_live))::in,
    maybe(determinism)::in, is_address_taken::in, proc_info::out) is det.

:- pred proc_info_set(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    maybe(list(is_live))::in, maybe(determinism)::in, determinism::in,
    hlds_goal::in, bool::in, rtti_varmaps::in,
    maybe(arg_size_info)::in, maybe(termination_info)::in,
    termination2_info::in, is_address_taken::in, stack_slots::in,
    maybe(list(arg_info))::in, liveness_info::in, proc_info::out) is det.

:- pred proc_info_create(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    determinism::in, hlds_goal::in, rtti_varmaps::in,
    is_address_taken::in, proc_info::out) is det.

:- pred proc_info_create(prog_context::in, prog_varset::in, vartypes::in,
    list(prog_var)::in, inst_varset::in, list(mer_mode)::in,
    maybe(determinism)::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, proc_info::out) is det.

:- pred proc_info_set_body(prog_varset::in, vartypes::in,
    list(prog_var)::in, hlds_goal::in, rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.

    % Predicates to get fields of proc_infos.

:- pred proc_info_get_context(proc_info::in, prog_context::out) is det.
:- pred proc_info_get_varset(proc_info::in, prog_varset::out) is det.
:- pred proc_info_get_vartypes(proc_info::in, vartypes::out) is det.
:- pred proc_info_get_headvars(proc_info::in, list(prog_var)::out) is det.
:- pred proc_info_get_inst_varset(proc_info::in, inst_varset::out) is det.
:- pred proc_info_get_maybe_declared_argmodes(proc_info::in,
    maybe(list(mer_mode))::out) is det.
:- pred proc_info_get_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_get_maybe_arglives(proc_info::in,
    maybe(list(is_live))::out) is det.
:- pred proc_info_get_declared_determinism(proc_info::in,
    maybe(determinism)::out) is det.
:- pred proc_info_get_inferred_determinism(proc_info::in, determinism::out)
    is det.
:- pred proc_info_get_goal(proc_info::in, hlds_goal::out) is det.
:- pred proc_info_get_can_process(proc_info::in, bool::out) is det.
:- pred proc_info_get_rtti_varmaps(proc_info::in, rtti_varmaps::out) is det.
:- pred proc_info_get_eval_method(proc_info::in, eval_method::out) is det.
:- pred proc_info_get_maybe_arg_size_info(proc_info::in,
    maybe(arg_size_info)::out) is det.
:- pred proc_info_get_maybe_termination_info(proc_info::in,
    maybe(termination_info)::out) is det.
:- pred proc_info_get_is_address_taken(proc_info::in,
    is_address_taken::out) is det.
:- pred proc_info_get_stack_slots(proc_info::in, stack_slots::out) is det.
:- pred proc_info_maybe_arg_info(proc_info::in,
    maybe(list(arg_info))::out) is det.
:- pred proc_info_get_liveness_info(proc_info::in, liveness_info::out) is det.
:- pred proc_info_get_need_maxfr_slot(proc_info::in, bool::out) is det.
:- pred proc_info_get_call_table_tip(proc_info::in,
    maybe(prog_var)::out) is det.
:- pred proc_info_get_maybe_proc_table_info(proc_info::in,
    maybe(proc_table_info)::out) is det.
:- pred proc_info_get_table_attributes(proc_info::in,
    maybe(table_attributes)::out) is det.
:- pred proc_info_get_maybe_deep_profile_info(proc_info::in,
    maybe(deep_profile_proc_info)::out) is det.
:- pred proc_info_get_maybe_untuple_info(proc_info::in,
    maybe(untuple_proc_info)::out) is det.

    % Predicates to set fields of proc_infos.

:- pred proc_info_set_varset(prog_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_vartypes(vartypes::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_headvars(list(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inst_varset(inst_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_declared_argmodes(maybe(list(mer_mode))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_argmodes(list(mer_mode)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arglives(maybe(list(is_live))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inferred_determinism(determinism::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_goal(hlds_goal::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_can_process(bool::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_rtti_varmaps(rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_eval_method(eval_method::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arg_size_info(maybe(arg_size_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_termination_info(maybe(termination_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_address_taken(is_address_taken::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_stack_slots(stack_slots::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_arg_info(list(arg_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_liveness_info(liveness_info::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_need_maxfr_slot(bool::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_call_table_tip(maybe(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_proc_table_info(maybe(proc_table_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_table_attributes(maybe(table_attributes)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_deep_profile_info(
    maybe(deep_profile_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_untuple_info(
    maybe(untuple_proc_info)::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_termination2_info(proc_info::in,
    termination2_info::out) is det.

:- pred proc_info_set_termination2_info(termination2_info::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_structure_sharing(proc_info::in,
    maybe(structure_sharing_domain)::out) is det.

:- pred proc_info_set_structure_sharing(structure_sharing_domain::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_imported_structure_sharing(proc_info::in,
    prog_vars::out, list(mer_type)::out, structure_sharing_domain::out)
    is semidet.

:- pred proc_info_set_imported_structure_sharing(prog_vars::in,
    list(mer_type)::in, structure_sharing_domain::in, proc_info::in,
    proc_info::out) is det.

:- pred proc_info_reset_imported_structure_sharing(proc_info::in,
    proc_info::out) is det.

:- pred proc_info_get_structure_reuse(proc_info::in,
    maybe(structure_reuse_domain)::out) is det.

:- pred proc_info_set_structure_reuse(structure_reuse_domain::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_get_imported_structure_reuse(proc_info::in,
    prog_vars::out, list(mer_type)::out, structure_reuse_domain::out)
    is semidet.

:- pred proc_info_set_imported_structure_reuse(prog_vars::in,
    list(mer_type)::in, structure_reuse_domain::in, proc_info::in,
    proc_info::out) is det.

:- pred proc_info_reset_imported_structure_reuse(proc_info::in,
    proc_info::out) is det.

:- pred proc_info_head_modes_constraint(proc_info::in, mode_constraint::out)
    is det.

:- pred proc_info_set_head_modes_constraint(mode_constraint::in,
    proc_info::in, proc_info::out) is det.

    % See also proc_info_interface_code_model in code_model.m.
:- pred proc_info_interface_determinism(proc_info::in, determinism::out)
    is det.

    % proc_info_never_succeeds(ProcInfo, Result):
    % return Result = yes if the procedure is known to never succeed
    % according to the declared determinism.
    %
:- pred proc_info_never_succeeds(proc_info::in, bool::out) is det.

:- pred proc_info_declared_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_arglives(proc_info::in, module_info::in,
    list(is_live)::out) is det.
:- pred proc_info_arg_info(proc_info::in, list(arg_info)::out) is det.
:- pred proc_info_get_initial_instmap(proc_info::in, module_info::in,
    instmap::out) is det.

:- pred proc_info_ensure_unique_names(proc_info::in, proc_info::out) is det.

    % Create a new variable of the given type to the procedure.
    %
:- pred proc_info_create_var_from_type(mer_type::in, maybe(string)::in,
    prog_var::out, proc_info::in, proc_info::out) is det.

    % Create a new variable for each element of the list of types.
    %
:- pred proc_info_create_vars_from_types(list(mer_type)::in,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % (further) instantiated by the procedure.
    %
:- pred proc_info_instantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % not (further) instantiated by the procedure.
    %
:- pred proc_info_uninstantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Return true if the interface of the given procedure must include
    % typeinfos for all the type variables in the types of the arguments.
    %
:- pred proc_interface_should_use_typeinfo_liveness(pred_info::in, proc_id::in,
    globals::in, bool::out) is det.

    % Return true if the interface of a procedure in a non-special
    % predicate with the given characteristics (import/export/local
    % status, address taken status) must include typeinfos for
    % all the type variables in the types of the arguments.
    % Note that only a few predicates in the builtin modules are special
    % in this sense, and that compiler-generated predicates are never
    % special.
    %
:- pred non_special_interface_should_use_typeinfo_liveness(import_status::in,
    is_address_taken::in, globals::in, bool::out) is det.

    % Return true if the body of a procedure from the given predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    % Note that body typeinfo liveness implies interface typeinfo liveness,
    % but not vice versa.
    %
:- pred body_should_use_typeinfo_liveness(pred_info::in, globals::in,
    bool::out) is det.

    % Return true if the body of a procedure in a non-special predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    %
:- pred non_special_body_should_use_typeinfo_liveness(globals::in,
    bool::out) is det.

    % If the procedure has a input/output pair of io.state arguments,
    % return the positions of those arguments in the argument list.
    % The positions are given as argument numbers, with the first argument
    % in proc_info_get_headvars being position 1, and so on. The first output
    % argument gives the position of the input state, the second the
    % position of the output state.
    %
    % Note that the automatically constructed unify, index and compare
    % procedures for the io:state type are not counted as having io:state
    % args, since they do not fall into the scheme of one input and one
    % output arg. Since they should never be called, this should not matter.
    %
:- pred proc_info_has_io_state_pair(module_info::in, proc_info::in,
    int::out, int::out) is semidet.

:- pred proc_info_has_io_state_pair_from_details(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, vartypes::in,
    int::out, int::out) is semidet.

    % Given a procedure table and the id of a procedure in that table,
    % return a procedure id to be attached to a clone of that procedure.
    % (The task of creating the clone proc_info and inserting into the
    % procedure table is the task of the caller.)
    %
:- pred clone_proc_id(proc_table::in, proc_id::in, proc_id::out) is det.

    % When mode inference is enabled, we record for each inferred mode
    % whether it is valid or not by keeping a list of error messages
    % in the proc_info. The mode is valid iff this list is empty.
    %
:- func mode_errors(proc_info) = list(mode_error_info).
:- func 'mode_errors :='(proc_info, list(mode_error_info)) = proc_info.
:- pred proc_info_is_valid_mode(proc_info::in) is semidet.

    % Make sure that all headvars are named. This can be useful e.g.
    % because the debugger ignores unnamed variables.
    %
:- pred ensure_all_headvars_are_named(proc_info::in, proc_info::out) is det.

    % Test whether the variable is of a dummy type, based on the vartypes.
    %
:- pred var_is_of_dummy_type(module_info::in, vartypes::in, prog_var::in)
    is semidet.

:- implementation.

:- import_module check_hlds.mode_errors.
:- import_module mdbcomp.program_representation.

:- type proc_info
    --->    proc_info(
                % The context of the `:- mode' decl (or the context of the
                % first clause, if there was no mode declaration).
                proc_context                :: prog_context,

                prog_varset                 :: prog_varset,
                var_types                   :: vartypes,
                head_vars                   :: list(prog_var),
                inst_varset                 :: inst_varset,

                % The declared modes of arguments.
                maybe_declared_head_modes   :: maybe(list(mer_mode)),

                actual_head_modes           :: list(mer_mode),
                maybe_head_modes_constraint :: maybe(mode_constraint),

                % Liveness (in the mode analysis sense) of the arguments
                % in the caller; says whether each argument may be used
                % after the call.
                head_var_caller_liveness    :: maybe(list(is_live)),

                % The _declared_ determinism of the procedure, or `no'
                % if there was no detism declaration.
                declared_detism             :: maybe(determinism),

                inferred_detism             :: determinism,
                body                        :: hlds_goal,

                % No if we must not process this procedure yet (used to delay
                % mode checking etc. for complicated modes of unification
                % predicates until the end of the unique_modes pass.)
                can_process                 :: bool,

                mode_errors                 :: list(mode_error_info),

                % Information about type_infos and typeclass_infos.
                proc_rtti_varmaps           :: rtti_varmaps,

                % How should the proc be evaluated.
                eval_method                 :: eval_method,

                proc_sub_info               :: proc_sub_info
            ).

:- type proc_sub_info
    --->    proc_sub_info(
                % Information about the relative sizes of the input and output
                % args of the procedure. Set by termination analysis.
                maybe_arg_sizes             :: maybe(arg_size_info),

                % The termination properties of the procedure.
                % Set by termination analysis.
                maybe_termination           :: maybe(termination_info),

                % Termination properties and argument size constraints for
                % the procedure. Set by termination2 analysis.
                termination2                :: termination2_info,

                % Is the address of this procedure taken? If yes, we will
                % need to use typeinfo liveness for them, so that deep_copy
                % and accurate gc have the RTTI they need for copying closures.
                %
                % Note that any non-local procedure must be considered
                % as having its address taken, since it is possible that
                % some other module may do so.
                is_address_taken            :: is_address_taken,

                % Allocation of variables to stack slots.
                stack_slots                 :: stack_slots,

                % The calling convention of each argument: information computed
                % by arg_info.m (based on the modes etc.) and used by code
                % generation to determine how each argument should be passed.
                arg_pass_info               :: maybe(list(arg_info)),

                % The initial liveness, for code generation.
                initial_liveness            :: liveness_info,

                % True iff tracing is enabled, this is a procedure that lives
                % on the det stack, and the code of this procedure may create
                % a frame on the det stack. (Only in these circumstances do we
                % need to reserve a stack slot to hold the value of maxfr
                % at the call, for use in implementing retry.) This slot
                % is used only with the LLDS backend XXX. Its value is set
                % during the live_vars pass; it is invalid before then.
                need_maxfr_slot             :: bool,

                % If the procedure's evaluation method is memo, loopcheck or
                % minimal, this slot identifies the variable that holds the tip
                % of the call table. Otherwise, this field will be set to `no'.
                %
                % Tabled procedures record, in the data structure identified
                % by this variable, that the call is active. When performing
                % a retry across such a procedure, we must reset the state
                % of the call; if we don't, the retried call will find the
                % active call and report an infinite loop error.
                %
                % Such resetting of course requires the debugger to know
                % whether the procedure has reached the call table tip yet.
                % Therefore when binding this variable, the code generator
                % of the relevant backend must record this fact in a place
                % accessible to the debugger, if debugging is enabled.
                call_table_tip              :: maybe(prog_var),

                % If set, it means that procedure has been subject to a tabling
                % transformation, either I/O tabling or the regular kind.
                % In the former case, the argument will contain all the
                % information we need to display I/O actions involving
                % this procedure; in the latter case, it will contain
                % all the information we need to display the call tables,
                % answer tables and answer blocks of the procedure.
                % XXX For now, the compiler fully supports only procedures
                % whose arguments are all either ints, floats or strings.
                % However, this is still sufficient for debugging most problems
                % in the tabling system.
                maybe_table_info            :: maybe(proc_table_info),

                table_attributes            :: maybe(table_attributes),

                maybe_deep_profile_proc_info :: maybe(deep_profile_proc_info),

                % If set, it means this procedure was created from another
                % procedure by the untupling transformation. This slot records
                % which of the procedure's arguments were derived from which
                % arguments in the original procedure.
                maybe_untuple_info          :: maybe(untuple_proc_info),

                % Structure sharing information as obtained by the structure
                % sharing analysis.
                structure_sharing           :: structure_sharing_info,

                % Structure reuse conditions obtained by the structure reuse
                % analysis (CTGC).
                structure_reuse             :: structure_reuse_info

        ).

:- type structure_sharing_info
    ---> structure_sharing_info(
            maybe_sharing           :: maybe(structure_sharing_domain),
            maybe_imported_sharing  :: maybe(imported_sharing)
                % Records the sharing information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed
                % at the beginning of structure sharing analysis. After that
                % this field is of no use.
        ).

    % Sharing information is expressed in terms of headvariables and the
    % type variables occurring in their types. In order to correctly process
    % (mainly renaming) this information, we need both the list of head
    % variables as well as their types. As this list of headvariables may
    % contain any compiler-added headvariables, the processing of imported
    % structure sharing information needs to be postponed until the actual
    % structure sharing analysis, which explains the need for the type
    % imported_sharing to temporarely store the imported sharing information.
    %
:- type imported_sharing
    --->    imported_sharing(
                % The list of headvars in which terms the imported sharing
                % is expressed.
                s_headvars        :: prog_vars,

                % The types of the headvars.
                s_types           :: list(mer_type),

                s_sharing         :: structure_sharing_domain
            ).

:- func structure_sharing_info_init = structure_sharing_info.

structure_sharing_info_init = structure_sharing_info(no, no).

:- type structure_reuse_info
    --->    structure_reuse_info(
                maybe_reuse           :: maybe(structure_reuse_domain),

                maybe_imported_reuse  :: maybe(imported_reuse)
                % Records the reuse information from any `.opt' or
                % `.trans_opt' file. This information needs to be processed
                % at the beginning of structure reuse analysis. After that
                % this field is of no use.
            ).

    % Same rationale as for imported_sharing.
    %
:- type imported_reuse
    --->    imported_reuse(
                % The list of headvars in which terms the imported reuse
                % information is expressed.
                r_headvars        :: prog_vars,

                % The types of the headvars.
                r_types           :: list(mer_type),

                r_reuse           :: structure_reuse_domain
            ).

:- func structure_reuse_info_init = structure_reuse_info.

structure_reuse_info_init = structure_reuse_info(no, no).

proc_info_init(MContext, Arity, Types, DeclaredModes, Modes, MaybeArgLives,
        MaybeDet, IsAddressTaken, ProcInfo) :-
    % Some parts of the procedure aren't known yet. We initialize them
    % to any old garbage which we will later throw away.

    % Inferred determinism gets initialized to `erroneous'.
    % This is what `det_analysis.m' wants. det_analysis.m
    % will later provide the correct inferred determinism for it.

    make_n_fresh_vars("HeadVar__", Arity, HeadVars, varset.init, BodyVarSet),
    varset.init(InstVarSet),
    map.from_corresponding_lists(HeadVars, Types, BodyTypes),
    ModeErrors = [],
    InferredDet = detism_erroneous,
    map.init(StackSlots),
    set.init(InitialLiveness),
    ArgInfo = no,
    goal_info_init(GoalInfo),
    ClauseBody = conj(plain_conj, []) - GoalInfo,
    CanProcess = yes,
    rtti_varmaps_init(RttiVarMaps),
    Term2Info = term_constr_main.term2_info_init,
    SharingInfo = structure_sharing_info_init,
    ReuseInfo = structure_reuse_info_init,
    ProcSubInfo = proc_sub_info(no, no, Term2Info, IsAddressTaken, StackSlots,
        ArgInfo, InitialLiveness, no, no, no, no, no, no,
        SharingInfo, ReuseInfo),
    ProcInfo = proc_info(MContext, BodyVarSet, BodyTypes, HeadVars, InstVarSet,
        DeclaredModes, Modes, no, MaybeArgLives, MaybeDet, InferredDet,
        ClauseBody, CanProcess, ModeErrors, RttiVarMaps, eval_normal,
        ProcSubInfo).

proc_info_set(Context, BodyVarSet, BodyTypes, HeadVars, InstVarSet, HeadModes,
        HeadLives, DeclaredDetism, InferredDetism, Goal, CanProcess,
        RttiVarMaps, ArgSizes, Termination, Termination2,
        IsAddressTaken, StackSlots, ArgInfo, Liveness, ProcInfo) :-
    ModeErrors = [],
    SharingInfo = structure_sharing_info_init,
    ReuseInfo = structure_reuse_info_init,
    ProcSubInfo = proc_sub_info(ArgSizes, Termination, Termination2,
        IsAddressTaken, StackSlots, ArgInfo, Liveness, no, no, no, no, no,
        no, SharingInfo, ReuseInfo),
    ProcInfo = proc_info(Context, BodyVarSet, BodyTypes, HeadVars,
        InstVarSet, no, HeadModes, no, HeadLives,
        DeclaredDetism, InferredDetism, Goal, CanProcess, ModeErrors,
        RttiVarMaps, eval_normal, ProcSubInfo).

proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet,
        HeadModes, Detism, Goal, RttiVarMaps, IsAddressTaken, ProcInfo) :-
    proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet,
        HeadModes, yes(Detism), Detism, Goal, RttiVarMaps,
        IsAddressTaken, ProcInfo).

proc_info_create(Context, VarSet, VarTypes, HeadVars, InstVarSet, HeadModes,
        MaybeDeclaredDetism, Detism, Goal, RttiVarMaps, IsAddressTaken,
        ProcInfo) :-
    map.init(StackSlots),
    set.init(Liveness),
    MaybeHeadLives = no,
    ModeErrors = [],
    Term2Info = term_constr_main.term2_info_init,
    SharingInfo = structure_sharing_info_init,
    ReuseInfo = structure_reuse_info_init,
    ProcSubInfo = proc_sub_info(no, no, Term2Info, IsAddressTaken,
        StackSlots, no, Liveness, no, no, no, no, no, no,
        SharingInfo, ReuseInfo),
    ProcInfo = proc_info(Context, VarSet, VarTypes, HeadVars,
        InstVarSet, no, HeadModes, no, MaybeHeadLives,
        MaybeDeclaredDetism, Detism, Goal, yes, ModeErrors,
        RttiVarMaps, eval_normal, ProcSubInfo).

proc_info_set_body(VarSet, VarTypes, HeadVars, Goal, RttiVarMaps,
        !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ prog_varset := VarSet,
    !:ProcInfo = !.ProcInfo ^ var_types := VarTypes,
    !:ProcInfo = !.ProcInfo ^ head_vars := HeadVars,
    !:ProcInfo = !.ProcInfo ^ body := Goal,
    !:ProcInfo = !.ProcInfo ^ proc_rtti_varmaps := RttiVarMaps.

proc_info_get_context(PI, PI ^ proc_context).
proc_info_get_varset(PI, PI ^ prog_varset).
proc_info_get_vartypes(PI, PI ^ var_types).
proc_info_get_headvars(PI, PI ^ head_vars).
proc_info_get_inst_varset(PI, PI ^ inst_varset).
proc_info_get_maybe_declared_argmodes(PI, PI ^ maybe_declared_head_modes).
proc_info_get_argmodes(PI, PI ^ actual_head_modes).
proc_info_get_maybe_arglives(PI, PI ^ head_var_caller_liveness).
proc_info_get_declared_determinism(PI, PI ^ declared_detism).
proc_info_get_inferred_determinism(PI, PI ^ inferred_detism).
proc_info_get_goal(PI, PI ^ body).
proc_info_get_can_process(PI, PI ^ can_process).
proc_info_get_rtti_varmaps(PI, PI ^ proc_rtti_varmaps).
proc_info_get_eval_method(PI, PI ^ eval_method).
proc_info_get_maybe_arg_size_info(PI, PI ^ proc_sub_info ^ maybe_arg_sizes).
proc_info_get_maybe_termination_info(PI,
    PI ^ proc_sub_info ^ maybe_termination).
proc_info_get_is_address_taken(PI, PI ^ proc_sub_info ^ is_address_taken).
proc_info_get_stack_slots(PI, PI ^ proc_sub_info ^ stack_slots).
proc_info_maybe_arg_info(PI, PI ^ proc_sub_info ^ arg_pass_info).
proc_info_get_liveness_info(PI, PI ^ proc_sub_info ^ initial_liveness).
proc_info_get_need_maxfr_slot(PI, PI ^ proc_sub_info ^ need_maxfr_slot).
proc_info_get_call_table_tip(PI, PI ^ proc_sub_info ^ call_table_tip).
proc_info_get_maybe_proc_table_info(PI, PI ^ proc_sub_info ^ maybe_table_info).
proc_info_get_table_attributes(PI, PI ^ proc_sub_info ^ table_attributes).
proc_info_get_maybe_deep_profile_info(PI,
    PI ^ proc_sub_info ^ maybe_deep_profile_proc_info).
proc_info_get_maybe_untuple_info(PI,
    PI ^ proc_sub_info ^ maybe_untuple_info).

proc_info_set_varset(VS, PI, PI ^ prog_varset := VS).
proc_info_set_vartypes(VT, PI, PI ^ var_types := VT).
proc_info_set_headvars(HV, PI, PI ^ head_vars := HV).
proc_info_set_inst_varset(IV, PI, PI ^ inst_varset := IV).
proc_info_set_maybe_declared_argmodes(AM, PI,
    PI ^ maybe_declared_head_modes := AM).
proc_info_set_argmodes(AM, PI, PI ^ actual_head_modes := AM).
proc_info_set_maybe_arglives(CL, PI, PI ^ head_var_caller_liveness := CL).
proc_info_set_inferred_determinism(ID, PI, PI ^ inferred_detism := ID).
proc_info_set_goal(G, PI, PI ^ body := G).
proc_info_set_can_process(CP, PI, PI ^ can_process := CP).
proc_info_set_rtti_varmaps(RI, PI, PI ^ proc_rtti_varmaps := RI).
proc_info_set_eval_method(EM, PI, PI ^ eval_method := EM).
proc_info_set_maybe_arg_size_info(MAS, PI,
    PI ^ proc_sub_info ^ maybe_arg_sizes := MAS).
proc_info_set_maybe_termination_info(MT, PI,
    PI ^ proc_sub_info ^ maybe_termination := MT).
proc_info_set_address_taken(AT, PI,
    PI ^ proc_sub_info ^ is_address_taken := AT).
proc_info_set_stack_slots(SS, PI, PI ^ proc_sub_info ^ stack_slots := SS).
proc_info_set_arg_info(AP, PI, PI ^ proc_sub_info ^ arg_pass_info := yes(AP)).
proc_info_set_liveness_info(IL, PI,
    PI ^ proc_sub_info ^ initial_liveness := IL).
proc_info_set_need_maxfr_slot(NMS, PI,
    PI ^ proc_sub_info ^ need_maxfr_slot := NMS).
proc_info_set_call_table_tip(CTT, PI,
    PI ^ proc_sub_info ^ call_table_tip := CTT).
proc_info_set_maybe_proc_table_info(MTI, PI,
    PI ^ proc_sub_info ^ maybe_table_info := MTI).
proc_info_set_table_attributes(TA, PI,
    PI ^ proc_sub_info ^ table_attributes := TA).
proc_info_set_maybe_deep_profile_info(DPI, PI,
    PI ^ proc_sub_info ^ maybe_deep_profile_proc_info := DPI).
proc_info_set_maybe_untuple_info(MUI, PI,
    PI ^ proc_sub_info ^ maybe_untuple_info := MUI).

proc_info_head_modes_constraint(ProcInfo, HeadModesConstraint) :-
    MaybeHeadModesConstraint = ProcInfo ^ maybe_head_modes_constraint,
    (
        MaybeHeadModesConstraint = yes(HeadModesConstraint)
    ;
        MaybeHeadModesConstraint = no,
        unexpected(this_file, "proc_info_head_modes_constraint: no constraint")
    ).

proc_info_set_head_modes_constraint(HMC, ProcInfo,
    ProcInfo ^ maybe_head_modes_constraint := yes(HMC)).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    mode_list_get_initial_insts(ModuleInfo, ArgModes, InitialInsts),
    assoc_list.from_corresponding_lists(HeadVars, InitialInsts, InstAL),
    instmap.from_assoc_list(InstAL, InstMap).

proc_info_declared_argmodes(ProcInfo, ArgModes) :-
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes1),
        ArgModes = ArgModes1
    ;
        MaybeArgModes = no,
        proc_info_get_argmodes(ProcInfo, ArgModes)
    ).

proc_info_interface_determinism(ProcInfo, Determinism) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDeterminism),
    (
        MaybeDeterminism = no,
        proc_info_get_inferred_determinism(ProcInfo, Determinism)
    ;
        MaybeDeterminism = yes(Determinism)
    ).

    % Return Result = yes if the called predicate is known to never succeed.
    %
proc_info_never_succeeds(ProcInfo, Result) :-
    proc_info_get_declared_determinism(ProcInfo, DeclaredDeterminism),
    (
        DeclaredDeterminism = no,
        Result = no
    ;
        DeclaredDeterminism = yes(Determinism),
        determinism_components(Determinism, _, HowMany),
        ( HowMany = at_most_zero ->
            Result = yes
        ;
            Result = no
        )
    ).

proc_info_arglives(ProcInfo, ModuleInfo, ArgLives) :-
    proc_info_get_maybe_arglives(ProcInfo, MaybeArgLives),
    (
        MaybeArgLives = yes(ArgLives0),
        ArgLives = ArgLives0
    ;
        MaybeArgLives = no,
        proc_info_get_argmodes(ProcInfo, Modes),
        get_arg_lives(ModuleInfo, Modes, ArgLives)
    ).

proc_info_is_valid_mode(ProcInfo) :-
    ProcInfo ^ mode_errors = [].

proc_info_arg_info(ProcInfo, ArgInfo) :-
    proc_info_maybe_arg_info(ProcInfo, MaybeArgInfo0),
    (
        MaybeArgInfo0 = yes(ArgInfo)
    ;
        MaybeArgInfo0 = no,
        unexpected(this_file, "proc_info_arg_info: arg_pass_info not set")
    ).

proc_info_get_termination2_info(ProcInfo, Termination2Info) :-
    Termination2Info = ProcInfo ^ proc_sub_info ^ termination2.

proc_info_set_termination2_info(Termination2Info, !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ termination2 :=
        Termination2Info.

proc_info_get_structure_sharing(ProcInfo, MaybeSharing) :-
    MaybeSharing = ProcInfo ^ proc_sub_info ^ structure_sharing
        ^ maybe_sharing.

proc_info_set_structure_sharing(Sharing, !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_sharing
        ^ maybe_sharing := yes(Sharing).

proc_info_get_imported_structure_sharing(ProcInfo, HeadVars, Types,
        Sharing) :-
    MaybeImportedSharing = ProcInfo ^ proc_sub_info ^ structure_sharing
        ^ maybe_imported_sharing,
    MaybeImportedSharing = yes(ImportedSharing),
    ImportedSharing = imported_sharing(HeadVars, Types, Sharing).

proc_info_set_imported_structure_sharing(HeadVars, Types, Sharing,
        !ProcInfo) :-
    ImportedSharing = imported_sharing(HeadVars, Types, Sharing),
    MaybeImportedSharing = yes(ImportedSharing),
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_sharing
        ^ maybe_imported_sharing := MaybeImportedSharing.

proc_info_reset_imported_structure_sharing(!ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_sharing
        ^ maybe_imported_sharing := no.

proc_info_get_structure_reuse(ProcInfo, MaybeReuse) :-
    MaybeReuse = ProcInfo ^ proc_sub_info ^ structure_reuse ^ maybe_reuse.

proc_info_set_structure_reuse(Reuse, !ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_reuse
        ^ maybe_reuse := yes(Reuse).

proc_info_get_imported_structure_reuse(ProcInfo, HeadVars, Types,
        Reuse) :-
    MaybeImportedReuse = ProcInfo ^ proc_sub_info ^ structure_reuse
        ^ maybe_imported_reuse,
    MaybeImportedReuse = yes(ImportedReuse),
    ImportedReuse = imported_reuse(HeadVars, Types, Reuse).

proc_info_set_imported_structure_reuse(HeadVars, Types, Reuse,
        !ProcInfo) :-
    ImportedReuse = imported_reuse(HeadVars, Types, Reuse),
    MaybeImportedReuse = yes(ImportedReuse),
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_reuse
        ^ maybe_imported_reuse := MaybeImportedReuse.

proc_info_reset_imported_structure_reuse(!ProcInfo) :-
    !:ProcInfo = !.ProcInfo ^ proc_sub_info ^ structure_reuse
        ^ maybe_imported_reuse := no.

proc_info_ensure_unique_names(!ProcInfo) :-
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    map.keys(VarTypes, AllVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    varset.ensure_unique_names(AllVars, "p", VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

proc_info_create_var_from_type(Type, MaybeName, NewVar, !ProcInfo) :-
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    varset.new_maybe_named_var(VarSet0, MaybeName, NewVar, VarSet),
    map.det_insert(VarTypes0, NewVar, Type, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_create_vars_from_types(Types, NewVars, !ProcInfo) :-
    list.length(Types, NumVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    varset.new_vars(VarSet0, NumVars, NewVars, VarSet),
    map.det_insert_from_corresponding_lists(VarTypes0, NewVars,
        Types, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

proc_info_instantiated_head_vars(ModuleInfo, ProcInfo, ChangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstChanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        map.lookup(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        \+ inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list.filter_map(IsInstChanged, HeadVarModes, ChangedInstHeadVars).

proc_info_uninstantiated_head_vars(ModuleInfo, ProcInfo,
        UnchangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstUnchanged = (pred(VarMode::in, Var::out) is semidet :-
        VarMode = Var - Mode,
        map.lookup(VarTypes, Var, Type),
        mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
        inst_matches_binding(Inst1, Inst2, Type, ModuleInfo)
    ),
    list.filter_map(IsInstUnchanged, HeadVarModes, UnchangedInstHeadVars).

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
        InterfaceTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( no_type_info_builtin(PredModule, PredName, PredArity) ->
        InterfaceTypeInfoLiveness = no
    ;
        pred_info_get_import_status(PredInfo, Status),
        pred_info_get_procedures(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_is_address_taken(ProcInfo, IsAddressTaken),
        non_special_interface_should_use_typeinfo_liveness(Status,
            IsAddressTaken, Globals, InterfaceTypeInfoLiveness)
    ).

non_special_interface_should_use_typeinfo_liveness(Status, IsAddressTaken,
        Globals, InterfaceTypeInfoLiveness) :-
    (
        (
            IsAddressTaken = address_is_taken
        ;
            % If the predicate is exported, its address may have
            % been taken elsewhere. If it is imported, then it
            % follows that it must be exported somewhere.
            Status \= status_local
        ;
            % If term size profiling (of either form) is enabled,
            % then we may need to access the typeinfo of any
            % variable bound to a heap cell argument. The only way
            % to ensure that this is possible is to preserve the
            % ability to access the typeinfo of any variable.
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        ;
            non_special_body_should_use_typeinfo_liveness(Globals, yes)
        )
    ->
        InterfaceTypeInfoLiveness = yes
    ;
        InterfaceTypeInfoLiveness = no
    ).

body_should_use_typeinfo_liveness(PredInfo, Globals, BodyTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( no_type_info_builtin(PredModule, PredName, PredArity) ->
        BodyTypeInfoLiveness = no
    ;
        non_special_body_should_use_typeinfo_liveness(Globals,
            BodyTypeInfoLiveness)
    ).

non_special_body_should_use_typeinfo_liveness(Globals, BodyTypeInfoLiveness) :-
    globals.lookup_bool_option(Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness).

proc_info_has_io_state_pair(ModuleInfo, ProcInfo, InArgNum, OutArgNum) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars,
        ArgModes, VarTypes, InArgNum, OutArgNum).

proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars, ArgModes,
        VarTypes, InArgNum, OutArgNum) :-
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarsModes),
    proc_info_has_io_state_pair_2(HeadVarsModes, ModuleInfo, VarTypes,
        1, no, MaybeIn, no, MaybeOut),
    (
        MaybeIn = yes(In),
        MaybeOut = yes(Out)
    ->
        InArgNum = In,
        OutArgNum = Out
    ;
        fail
    ).

:- pred proc_info_has_io_state_pair_2(assoc_list(prog_var, mer_mode)::in,
    module_info::in, vartypes::in, int::in,
    maybe(int)::in, maybe(int)::out, maybe(int)::in, maybe(int)::out)
    is semidet.

proc_info_has_io_state_pair_2([], _, _, _, !MaybeIn, !MaybeOut).
proc_info_has_io_state_pair_2([Var - Mode | VarModes], ModuleInfo, VarTypes,
        ArgNum, !MaybeIn, !MaybeOut) :-
    (
        map.lookup(VarTypes, Var, VarType),
        type_is_io_state(VarType)
    ->
        ( mode_is_fully_input(ModuleInfo, Mode) ->
            (
                !.MaybeIn = no,
                !:MaybeIn = yes(ArgNum)
            ;
                !.MaybeIn = yes(_),
                % Procedures with two input arguments of type io.state
                % (e.g. the automatically generated unification or comparison
                % procedure for the io.state type) do not fall into the
                % one input/one output pattern we are looking for.
                fail
            )
        ; mode_is_fully_output(ModuleInfo, Mode) ->
            (
                !.MaybeOut = no,
                !:MaybeOut = yes(ArgNum)
            ;
                !.MaybeOut = yes(_),
                % Procedures with two output arguments of type io.state
                % do not fall into the one input/one output pattern we are
                % looking for.
                fail
            )
        ;
            fail
        )
    ;
        true
    ),
    proc_info_has_io_state_pair_2(VarModes, ModuleInfo, VarTypes,
        ArgNum + 1, !MaybeIn, !MaybeOut).

clone_proc_id(ProcTable, _ProcId, CloneProcId) :-
    find_lowest_unused_proc_id(ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id(proc_table::in, proc_id::out) is det.

find_lowest_unused_proc_id(ProcTable, CloneProcId) :-
    find_lowest_unused_proc_id_2(0, ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id_2(proc_id::in, proc_table::in, proc_id::out)
    is det.

find_lowest_unused_proc_id_2(TrialProcId, ProcTable, CloneProcId) :-
    ( map.search(ProcTable, TrialProcId, _) ->
        find_lowest_unused_proc_id_2(TrialProcId + 1, ProcTable, CloneProcId)
    ;
        CloneProcId = TrialProcId
    ).

ensure_all_headvars_are_named(!ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    ensure_all_headvars_are_named_2(HeadVars, 1, VarSet0, VarSet),
    proc_info_set_varset(VarSet, !ProcInfo).

:- pred ensure_all_headvars_are_named_2(list(prog_var)::in, int::in,
    prog_varset::in, prog_varset::out) is det.

ensure_all_headvars_are_named_2([], _, !VarSet).
ensure_all_headvars_are_named_2([Var | Vars], SeqNum, !VarSet) :-
    ( varset.search_name(!.VarSet, Var, _Name) ->
        true
    ;
        Name = "HeadVar__" ++ int_to_string(SeqNum),
        varset.name_var(!.VarSet, Var, Name, !:VarSet)
    ),
    ensure_all_headvars_are_named_2(Vars, SeqNum + 1, !VarSet).

var_is_of_dummy_type(ModuleInfo, VarTypes, Var) :-
    map.lookup(VarTypes, Var, Type),
    is_dummy_argument_type(ModuleInfo, Type).

%-----------------------------------------------------------------------------%

    % Predicates to deal with record syntax.

:- interface.

    % field_extraction_function_args(Args, InputTermArg).
    % Work out which arguments of a field access correspond to the
    % field being extracted/set, and which are the container arguments.
    %
:- pred field_extraction_function_args(list(prog_var)::in, prog_var::out)
    is det.

    % field_update_function_args(Args, InputTermArg, FieldArg).
    %
:- pred field_update_function_args(list(prog_var)::in, prog_var::out,
    prog_var::out) is det.

    % field_access_function_name(AccessType, FieldName, FuncName).
    %
    % From the access type and the name of the field,
    % construct a function name.
    %
:- pred field_access_function_name(field_access_type::in, ctor_field_name::in,
    sym_name::out) is det.

    % is_field_access_function_name(ModuleInfo, FuncName, Arity,
    %   AccessType, FieldName).
    %
    % Inverse of the above.
    %
:- pred is_field_access_function_name(module_info::in, sym_name::in,
    arity::out, field_access_type::out, ctor_field_name::out) is semidet.

:- pred pred_info_is_field_access_function(module_info::in, pred_info::in)
    is semidet.

:- implementation.

field_extraction_function_args(Args, TermInputArg) :-
    ( Args = [TermInputArg0] ->
        TermInputArg = TermInputArg0
    ;
        unexpected(this_file, "field_extraction_function_args")
    ).

field_update_function_args(Args, TermInputArg, FieldArg) :-
    ( Args = [TermInputArg0, FieldArg0] ->
        FieldArg = FieldArg0,
        TermInputArg = TermInputArg0
    ;
        unexpected(this_file, "field_update_function_args")
    ).

field_access_function_name(get, FieldName, FieldName).
field_access_function_name(set, FieldName, FuncName) :-
    add_sym_name_suffix(FieldName, " :=", FuncName).

is_field_access_function_name(ModuleInfo, FuncName, Arity,
        AccessType, FieldName) :-
    ( remove_sym_name_suffix(FuncName, " :=", FieldName0) ->
        Arity = 2,
        AccessType = set,
        FieldName = FieldName0
    ;
        Arity = 1,
        AccessType = get,
        FieldName = FuncName
    ),
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.contains(CtorFieldTable, FieldName).

pred_info_is_field_access_function(ModuleInfo, PredInfo) :-
    pred_info_is_pred_or_func(PredInfo) = function,
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    adjust_func_arity(function, FuncArity, PredArity),
    is_field_access_function_name(ModuleInfo, qualified(Module, Name),
        FuncArity, _, _).

%-----------------------------------------------------------------------------%

    % Predicates to deal with builtins.

:- interface.

    % is_unify_or_compare_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler generated instance of a type-specific special_pred (i.e. one
    % of the unify, compare, or index predicates generated as a type-specific
    % instance of unify/2, index/2, or compare/3).
    %
:- pred is_unify_or_compare_pred(pred_info::in) is semidet.

    % Is the argument the pred_info for a builtin that can be generated inline?
    %
:- pred pred_info_is_builtin(pred_info::in) is semidet.

    % builtin_state(ModuleInfo, CallerPredId, PredId, ProcId, BuiltinState)
    %
    % Is the given procedure a builtin that should be generated inline
    % in the given caller?
    %
:- func builtin_state(module_info, pred_id, pred_id, proc_id) = builtin_state.

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.special_pred.

pred_info_is_builtin(PredInfo) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    ProcId = initial_proc_id,
    is_inline_builtin(ModuleName, PredName, ProcId, Arity).

builtin_state(ModuleInfo, CallerPredId, PredId, ProcId) = BuiltinState :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, allow_inlining, AllowInlining),
    globals.lookup_bool_option(Globals, inline_builtins, InlineBuiltins),
    (
        % The automatically generated "recursive" call in the
        % goal for each builtin must be generated inline, or
        % we would generate an infinite loop.
        (
            AllowInlining = yes,
            InlineBuiltins = yes
        ;
            CallerPredId = PredId
        ),
        is_inline_builtin(ModuleName, PredName, ProcId, Arity)
    ->
        BuiltinState = inline_builtin
    ;
        BuiltinState = not_builtin
    ).

:- pred is_inline_builtin(module_name::in, string::in, proc_id::in, arity::in)
    is semidet.

is_inline_builtin(ModuleName, PredName, ProcId, Arity) :-
    Arity =< 3,
    prog_varset_init(VarSet),
    varset.new_vars(VarSet, Arity, Args, _),
    builtin_ops.translate_builtin(ModuleName, PredName, ProcId, Args, _).

:- pred prog_varset_init(prog_varset::out) is det.

prog_varset_init(VarSet) :- varset.init(VarSet).

is_unify_or_compare_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, origin_special_pred(_)). % XXX bug

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

    % Check if the given evaluation method is allowed with
    % the given determinism.
    %
:- func valid_determinism_for_eval_method(eval_method, determinism) = bool.

    % Return true if the given evaluation method requires a
    % stratification check.
    %
:- func eval_method_needs_stratification(eval_method) = bool.

    % Return true if the given evaluation method uses a per-procedure
    % tabling pointer. If so, the back-end must generate a declaration
    % for the variable to hold the table.
    %
:- func eval_method_has_per_proc_tabling_pointer(eval_method) = bool.

    % Return true if the given evaluation method requires the body
    % of the procedure using it to be transformed by table_gen.m.
    %
:- func eval_method_requires_tabling_transform(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be ground.
    %
:- func eval_method_requires_ground_args(eval_method) = bool.

    % Return true if the given evaluation method requires the arguments
    % of the procedure using it to be non-unique.
    %
:- func eval_method_destroys_uniqueness(eval_method) = bool.

    % Return the change a given evaluation method can do to a given
    % determinism.
    %
:- func eval_method_change_determinism(eval_method, determinism) = determinism.

:- implementation.

valid_determinism_for_eval_method(eval_normal, _) = yes.
valid_determinism_for_eval_method(eval_loop_check, Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    ( MaxSoln = at_most_zero ->
        Valid = no
    ;
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_memo, Detism) = Valid :-
    determinism_components(Detism, _, MaxSoln),
    ( MaxSoln = at_most_zero ->
        Valid = no
    ;
        Valid = yes
    ).
valid_determinism_for_eval_method(eval_table_io(_, _), _) = _ :-
    unexpected(this_file,
        "valid_determinism_for_eval_method called after tabling phase").
valid_determinism_for_eval_method(eval_minimal(_), Detism) = Valid :-
    % The following reasons specify why a particular determinism is
    % incompatible with minimal model tabling.
    %
    % Reason 1:
    % Determinism analysis isn't yet smart enough to know whether
    % a cannot_fail execution path is guaranteed not to go through a call
    % to a predicate that is mutually recursive with this one, which (if this
    % predicate is minimal model) is the only way that the predicate can be
    % properly cannot_fail. The problem is that in in general, the mutually
    % recursive predicate may be in another module.
    %
    % Reason 2:
    % The transformation, as currently implemented, assumes that it is possible
    % to reach the call table tip, and generates HLDS that refers to the
    % introduced variable representing this tip. This variable however, will
    % be optimized away if the code cannot succeed, causing a code generator
    % abort.
    %
    % Reason 3:
    % Minimal model semantics requires computing to a fixpoint, and this is
    % incompatible with the notion of committed choice.
    %
    % Reason 4:
    % Doing the analysis required to ensure that a predicate can't have more
    % than one solution is much harder if the predicate concerned is
    % minimal_model. In theory, this analysis could be done, but it would
    % take a lot of programming, and since there is a simple workaround
    % (make the predicate nondet, and check the number of solutions at the
    % caller), this would not be cost-effective.
    (
        Detism = detism_det,
        Valid = no                  % Reason 1
    ;
        Detism = detism_semi,
        Valid = no                  % Reason 4
    ;
        Detism = detism_multi,      % Reason 1
        Valid = yes
    ;
        Detism = detism_non,
        Valid = yes
    ;
        Detism = detism_cc_multi,   % Reason 3
        Valid = no
    ;
        Detism = detism_cc_non,     % Reason 3
        Valid = no
    ;
        Detism = detism_erroneous,  % Reason 2
        Valid = no
    ;
        Detism = detism_failure,    % Reason 2
        Valid = no
    ).

eval_method_needs_stratification(eval_normal) = no.
eval_method_needs_stratification(eval_loop_check) = no.
eval_method_needs_stratification(eval_table_io(_, _)) = no.
eval_method_needs_stratification(eval_memo) = no.
eval_method_needs_stratification(eval_minimal(_)) = yes.

eval_method_has_per_proc_tabling_pointer(eval_normal) = no.
eval_method_has_per_proc_tabling_pointer(eval_loop_check) = yes.
eval_method_has_per_proc_tabling_pointer(eval_table_io(_, _)) = no.
eval_method_has_per_proc_tabling_pointer(eval_memo) = yes.
eval_method_has_per_proc_tabling_pointer(eval_minimal(_)) = yes.

eval_method_requires_tabling_transform(eval_normal) = no.
eval_method_requires_tabling_transform(eval_loop_check) = yes.
eval_method_requires_tabling_transform(eval_table_io(_, _)) = yes.
eval_method_requires_tabling_transform(eval_memo) = yes.
eval_method_requires_tabling_transform(eval_minimal(_)) = yes.

eval_method_requires_ground_args(eval_normal) = no.
eval_method_requires_ground_args(eval_loop_check) = yes.
eval_method_requires_ground_args(eval_table_io(_, _)) = yes.
eval_method_requires_ground_args(eval_memo) = yes.
eval_method_requires_ground_args(eval_minimal(_)) = yes.

eval_method_destroys_uniqueness(eval_normal) = no.
eval_method_destroys_uniqueness(eval_loop_check) = yes.
eval_method_destroys_uniqueness(eval_table_io(_, _)) = no.
eval_method_destroys_uniqueness(eval_memo) = yes.
eval_method_destroys_uniqueness(eval_minimal(_)) = yes.

eval_method_change_determinism(eval_normal, Detism) = Detism.
eval_method_change_determinism(eval_loop_check, Detism) = Detism.
eval_method_change_determinism(eval_table_io(_, _), Detism) = Detism.
eval_method_change_determinism(eval_memo, Detism) = Detism.
eval_method_change_determinism(eval_minimal(_), Detism0) = Detism :-
    det_conjunction_detism(detism_semi, Detism0, Detism).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_pred.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_pred.
%-----------------------------------------------------------------------------%
