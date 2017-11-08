%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_typeclass.m.
% Author: dgj, mark.
%
% This module checks conformance of instance declarations to the typeclass
% declaration. It takes various steps to do this.
%
% (1) In check_instance_declaration_types/4, we check that each type
% in the instance declaration is either a type with no arguments,
% or a polymorphic type whose arguments are all type variables.
% We also check that all of the types in exported instance declarations are
% in scope here. XXX The latter part should really be done earlier, but with
% the current implementation this is the most convenient spot.
%
% This step also checks that types in instance declarations are not abstract
% exported equivalence types defined in this module. Unfortunately, there is
% no way to check at compile time that it is not an abstract exported
% equivalence type defined in some *other* module.
%
% (2) In check_instance_decls/6, for every method of every instance we
% generate a new pred whose types and modes are as expected by the typeclass
% declaration, and whose body just calls the implementation provided by the
% instance declaration.
%
% For example, given the declarations:
%
% :- typeclass c(T) where [
%   pred m(T::in, T::out) is semidet
% ].
%
% :- instance c(int) where [
%   pred(m/2) is my_m
% ].
%
% we check the correctness of my_m/2 as an implementation of m/2
% by generating the following new predicate:
%
% :- pred 'implementation of m/2'(int::in, int::out) is semidet.
%
% 'implementation of m/2'(HeadVar_1, HeadVar_2) :-
%   my_m(HeadVar_1, HeadVar_2).
%
% By generating the new pred, we check the instance method for type, mode,
% determinism and uniqueness correctness since the generated pred is checked
% in each of those passes too. At this point, we add instance method pred/proc
% ids to the instance table of the HLDS. We also check that there are no
% missing, duplicate or bogus methods.
%
% In this pass we also call check_superclass_conformance/9, which checks that
% all superclass constraints are satisfied by the instance declaration. To do
% this, that predicate attempts to perform context reduction on the typeclass
% constraints, using the instance constraints as assumptions. At this point
% we fill in the super class proofs.
%
% (3) In check_for_cyclic_classes/4, we check for cycles in the typeclass
% hierarchy. A cycle occurs if we can start from any given typeclass
% declaration and follow the superclass constraints on classes to reach the
% same class that we started from. Only the class_id needs to be repeated;
% it doesn't need to have the parameters. Note that we follow the constraints
% on class declarations only, not those on instance declarations. While doing
% this, we fill in the fundeps_ancestors field in the class table.
%
% (4) In check_for_missing_concrete_instances/4, we check that each
% abstract instance has a corresponding concrete instance.
%
% (5) In check_functional_dependencies/4, all visible instances are
% checked for coverage and mutual consistency with respect to any functional
% dependencies. This doesn't necessarily catch all cases of inconsistent
% instances, however, since in general that cannot be done until link time.
% We try to catch as many cases as possible here, though, since we can give
% better error messages.
%
% (6) In check_typeclass_constraints/4, we check typeclass constraints on
% predicate and function declarations and on existentially typed data
% constructors for ambiguity, taking into consideration the information
% provided by functional dependencies. We also call check_constraint_quant/5
% to check that all type variables in constraints are universally quantified,
% or that they are all existentially quantified. We don't support constraints
% where some of the type variables are universal and some are existential.
%
%---------------------------------------------------------------------------%

:- module check_hlds.check_typeclass.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

:- pred check_typeclasses(module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typeclasses.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

check_typeclasses(!ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),

    trace [io(!IO1)] (
        maybe_write_string(Verbose,
            "% Checking instance declaration types...\n", !IO1)
    ),
    check_instance_declaration_types(!ModuleInfo, !Specs),

    % If we encounter any errors while checking that the types in an
    % instance declaration are valid then don't attempt the remaining
    % passes. Pass 2 cannot be run since the name mangling scheme we
    % use to generate the names of the method wrapper predicates may
    % abort if the types in an instance are not valid, e.g. if an
    % instance head contains a type variable that is not wrapped inside
    % a functor. Most of the other passes also depend upon information
    % that is calculated during pass 2.
    %
    % XXX it would be better to just remove the invalid instances at this
    % point and then continue on with the valid instances.
    %
    (
        !.Specs = [],
        trace [io(!IO2)] (
            maybe_write_string(Verbose,
                "% Checking typeclass instances...\n", !IO2)
        ),
        check_instance_decls(!ModuleInfo, !QualInfo, !Specs),

        trace [io(!IO3)] (
            maybe_write_string(Verbose,
                "% Checking for cyclic classes...\n", !IO3)
        ),
        check_for_cyclic_classes(!ModuleInfo, !Specs),

        trace [io(!IO4)] (
            maybe_write_string(Verbose,
                "% Checking for missing concrete instances...\n", !IO4)
        ),
        check_for_missing_concrete_instances(!.ModuleInfo, !Specs),

        trace [io(!IO5)] (
            maybe_write_string(Verbose,
                "% Checking functional dependencies on instances...\n", !IO5)
        ),
        check_functional_dependencies(!.ModuleInfo, !Specs),

        trace [io(!IO6)] (
            maybe_write_string(Verbose,
                "% Checking typeclass constraints...\n", !IO6)
        ),
        check_typeclass_constraints(!.ModuleInfo, !Specs)
    ;
        !.Specs = [_ | _]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % In check_instance_declaration_types/4, we check that each type
    % in the instance declaration must be either a type with no arguments,
    % or a polymorphic type whose arguments are all distinct type variables.
    %
:- pred check_instance_declaration_types(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_declaration_types(!ModuleInfo, !Specs) :-
    module_info_get_instance_table(!.ModuleInfo, InstanceTable),
    map.foldl(check_instance_declaration_types_for_class(!.ModuleInfo),
        InstanceTable, !Specs).

:- pred check_instance_declaration_types_for_class(module_info::in,
    class_id::in, list(hlds_instance_defn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_declaration_types_for_class(ModuleInfo, ClassId,
        InstanceDefns, !Specs) :-
    list.foldl(
        check_instance_declaration_types_for_instance(ModuleInfo, ClassId),
        InstanceDefns, !Specs).

:- pred check_instance_declaration_types_for_instance(module_info::in,
    class_id::in, hlds_instance_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_declaration_types_for_instance(ModuleInfo,
        ClassId, InstanceDefn, !Specs) :-
    OriginalTypes = InstanceDefn ^ instdefn_orig_types,
    list.foldl2(is_valid_instance_orig_type(ModuleInfo, ClassId, InstanceDefn),
        OriginalTypes, 1, _, !Specs),
    Types = InstanceDefn ^ instdefn_types,
    list.foldl3(is_valid_instance_type(ModuleInfo, ClassId, InstanceDefn),
        Types, 1, _, set.init, _, !Specs).

:- pred is_valid_instance_orig_type(module_info::in,
    class_id::in, hlds_instance_defn::in, mer_type::in,
    int::in, int::out, list(error_spec)::in, list(error_spec)::out) is det.

is_valid_instance_orig_type(ModuleInfo, ClassId, InstanceDefn, Type,
        N, N+1, !Specs) :-
    (
        Type = defined_type(_TypeName, _, _),
        ( if type_to_type_defn(ModuleInfo, Type, TypeDefn) then
            get_type_defn_body(TypeDefn, TypeBody),
            (
                TypeBody = hlds_eqv_type(_),
                get_type_defn_status(TypeDefn, TypeDefnStatus),
                ( if
                    TypeDefnStatus = type_status(status_abstract_exported),
                    % If the instance definition is itself abstract exported,
                    % we want to generate only one error message, instead of
                    % two error messages, one for the abstract and one for the
                    % concrete instance definition.
                    InstanceDefn ^ instdefn_body = instance_body_concrete(_)
                then
                    Spec = abstract_eqv_instance_type_msg(ClassId,
                        InstanceDefn, N),
                    !:Specs = [Spec | !.Specs]
                else
                    true
                )
            ;
                ( TypeBody = hlds_du_type(_, _, _, _, _, _, _, _, _)
                ; TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_solver_type(_)
                ; TypeBody = hlds_abstract_type(_)
                )
            )
        else
            % The type is either a builtin type or a type variable.
            true
        )
    ;
        ( Type = builtin_type(_)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = type_variable(_, _)
        ; Type = tuple_type(_, _)
        )
    ;
        Type = kinded_type(_, _),
        unexpected("check_typeclass", "kinded_type")
    ).

    % Each of these types in the instance declaration must be either
    % (a) a type with no arguments, or (b) a polymorphic type whose arguments
    % are all distinct type variables.
    %
:- pred is_valid_instance_type(module_info::in,
    class_id::in, hlds_instance_defn::in, mer_type::in,
    int::in, int::out, set(mer_type)::in, set(mer_type)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

is_valid_instance_type(ModuleInfo, ClassId, InstanceDefn, Type,
        N, N+1, !SeenTypes, !Specs) :-
    (
        Type = builtin_type(_)
    ;
        (
            Type = higher_order_type(_, _, _, _, _),
            EndPieces = [words("is a higher order type.")]
        ;
            Type = apply_n_type(_, _, _),
            EndPieces = [words("is an apply/N type.")]
        ;
            Type = type_variable(_, _),
            EndPieces = [words("is a type variable.")]
        ),
        Spec = bad_instance_type_msg(ClassId, InstanceDefn, N, EndPieces,
            badly_formed),
        !:Specs = [Spec | !.Specs]
    ;
        Type = tuple_type(Args, _),
        each_arg_is_a_type_variable(!.SeenTypes, Args, 1, Result),
        (
            Result = no_error,
            set.insert_list(Args, !SeenTypes)
        ;
            Result = arg_not_type_variable(_),
            Spec = badly_formed_instance_type_msg(ClassId, InstanceDefn,
                N, Result),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Type = kinded_type(_, _),
        unexpected("check_typeclass", "kinded_type")
    ;
        Type = defined_type(_TypeName, Args, _),
        each_arg_is_a_type_variable(!.SeenTypes, Args, 1, Result),
        (
            Result = no_error,
            set.insert_list(Args, !SeenTypes),
            ( if type_to_type_defn(ModuleInfo, Type, TypeDefn) then
                get_type_defn_body(TypeDefn, TypeBody),
                (
                    TypeBody = hlds_eqv_type(EqvType),
                    is_valid_instance_type(ModuleInfo, ClassId, InstanceDefn,
                        EqvType, N, _, !SeenTypes, !Specs)
                ;
                    ( TypeBody = hlds_du_type(_, _, _, _, _, _, _, _, _)
                    ; TypeBody = hlds_foreign_type(_)
                    ; TypeBody = hlds_solver_type(_)
                    ; TypeBody = hlds_abstract_type(_)
                    )
                )
            else
                % The type is either a builtin type or a type variable.
                true
            )
        ;
            Result = arg_not_type_variable(_),
            Spec = badly_formed_instance_type_msg(ClassId, InstanceDefn,
                N, Result),
            !:Specs = [Spec | !.Specs]
        )
    ).

:- type instance_arg_result
    --->    no_error
    ;       arg_not_type_variable(int).

:- inst instance_arg_result_error for instance_arg_result/0
    --->    arg_not_type_variable(ground).

:- pred each_arg_is_a_type_variable(set(mer_type)::in,
    list(mer_type)::in, int::in, instance_arg_result::out) is det.

each_arg_is_a_type_variable(_, [], _, no_error).
each_arg_is_a_type_variable(SeenTypes, [Type | Types], N, Result) :-
    (
        Type = type_variable(_, _),
        each_arg_is_a_type_variable(SeenTypes, Types, N + 1, Result)
    ;
        ( Type = defined_type(_, _, _)
        ; Type = builtin_type(_)
        ; Type = higher_order_type(_, _, _, _, _)
        ; Type = tuple_type(_, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        ),
        % XXX STATUS keep going, return other errors
        Result = arg_not_type_variable(N)
    ).

:- func badly_formed_instance_type_msg(class_id::in, hlds_instance_defn::in,
    int::in, instance_arg_result::in(instance_arg_result_error))
    = (error_spec::out) is det.

badly_formed_instance_type_msg(ClassId, InstanceDefn, N, Error) = Spec :-
    Error = arg_not_type_variable(ArgNum),
    EndPieces = [words("is a type whose"), nth_fixed(ArgNum),
        words("argument is not a variable.")],
    Spec = bad_instance_type_msg(ClassId, InstanceDefn, N, EndPieces,
        badly_formed).

:- func abstract_eqv_instance_type_msg(class_id, hlds_instance_defn, int) =
    error_spec.

abstract_eqv_instance_type_msg(ClassId, InstanceDefn, N) = Spec :-
    EndPieces = [words("is an abstract exported equivalence type.")],
    Spec = bad_instance_type_msg(ClassId, InstanceDefn, N, EndPieces,
        abstract_exported_eqv).

:- type bad_instance_type_kind
    --->    badly_formed
    ;       abstract_exported_eqv.

:- func bad_instance_type_msg(class_id, hlds_instance_defn, int,
    list(format_component), bad_instance_type_kind) = error_spec.

bad_instance_type_msg(ClassId, InstanceDefn, N, EndPieces, Kind) = Spec :-
    ClassId = class_id(ClassName, _),
    ClassNameString = sym_name_to_string(ClassName),

    InstanceVarSet = InstanceDefn ^ instdefn_tvarset,
    InstanceContext = InstanceDefn ^ instdefn_context,
    (
        Kind = badly_formed,
        % We are generating the error message because the type is badly formed
        % as expanded. The unexpanded version may be correctly formed.
        InstanceTypes = InstanceDefn ^ instdefn_types
    ;
        Kind = abstract_exported_eqv,
        % Messages about the expanded type being an equivalence type
        % would not make sense.
        InstanceTypes = InstanceDefn ^ instdefn_orig_types
    ),
    InstanceTypesString = mercury_type_list_to_string(InstanceVarSet,
        InstanceTypes),

    HeaderPieces = [words("In instance declaration for"),
        words_quote(ClassNameString ++ "(" ++ InstanceTypesString ++ ")"),
        suffix(":")],
    ArgNumPieces = [words("the"), nth_fixed(N), words("argument") | EndPieces]
        ++ [nl],
    (
        Kind = abstract_exported_eqv,
        HeadingMsg = simple_msg(InstanceContext,
            [always(HeaderPieces), always(ArgNumPieces)])
    ;
        Kind = badly_formed,
        VerbosePieces =
            [words("(Types in instance declarations must be functors " ++
                "with variables as arguments.)"), nl],
        HeadingMsg = simple_msg(InstanceContext,
            [always(HeaderPieces), always(ArgNumPieces),
            verbose_only(verbose_once, VerbosePieces)])
    ),
    Spec = error_spec(severity_error, phase_type_check, [HeadingMsg]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_instance_decls(module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_decls(!ModuleInfo, !QualInfo, !Specs) :-
    module_info_get_class_table(!.ModuleInfo, ClassTable),
    module_info_get_instance_table(!.ModuleInfo, InstanceTable0),
    map.map_foldl3(check_one_class(ClassTable), InstanceTable0, InstanceTable,
        !ModuleInfo, !QualInfo, [], NewSpecs),
    module_info_get_globals(!.ModuleInfo, Globals),
    Errors = contains_errors(Globals, NewSpecs),
    (
        Errors = no,
        module_info_set_instance_table(InstanceTable, !ModuleInfo)
    ;
        Errors = yes
    ),
    !:Specs = NewSpecs ++ !.Specs.

    % Check all the instances of one class.
    %
:- pred check_one_class(class_table::in, class_id::in,
    list(hlds_instance_defn)::in, list(hlds_instance_defn)::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_one_class(ClassTable, ClassId, InstanceDefns0, InstanceDefns,
        !ModuleInfo, !QualInfo, !Specs) :-
    map.lookup(ClassTable, ClassId, ClassDefn),
    ClassDefn = hlds_class_defn(TypeClassStatus, SuperClasses, _FunDeps,
        _Ancestors, ClassVars, _Kinds, Interface, ClassInterface,
        ClassVarSet, TermContext),
    ( if
        typeclass_status_defined_in_this_module(TypeClassStatus) = yes,
        Interface = class_interface_abstract
    then
        ClassId = class_id(ClassName, ClassArity),
        Pieces = [words("Error: no definition for typeclass"),
            unqual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
            suffix("."), nl],
        Msg = simple_msg(TermContext, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs],
        InstanceDefns = InstanceDefns0
    else
        ClassProcPredIds0 = list.map(
            pred_proc_id_project_pred_id, ClassInterface),
        list.sort_and_remove_dups(ClassProcPredIds0, ClassProcPredIds),
        list.map_foldl3(
            check_class_instance(ClassId, SuperClasses, ClassVars,
                ClassInterface, Interface, ClassVarSet, ClassProcPredIds),
            InstanceDefns0, InstanceDefns,
            !ModuleInfo, !QualInfo, !Specs)
    ).

    % Check one instance of one class.
    %
:- pred check_class_instance(class_id::in, list(prog_constraint)::in,
    list(tvar)::in, hlds_class_interface::in, class_interface::in,
    tvarset::in, list(pred_id)::in,
    hlds_instance_defn::in, hlds_instance_defn::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_class_instance(ClassId, SuperClasses, Vars, HLDSClassInterface,
        ClassInterface, ClassVarSet, ClassPredIds, !InstanceDefn,
        !ModuleInfo, !QualInfo, !Specs):-
    % Check conformance of the instance body.
    !.InstanceDefn = hlds_instance_defn(_, _, _, _, TermContext, _,
        InstanceBody, _, _, _),
    (
        InstanceBody = instance_body_abstract
    ;
        InstanceBody = instance_body_concrete(InstanceMethods),
        check_concrete_class_instance(ClassId, Vars,
            HLDSClassInterface, ClassInterface,
            ClassPredIds, TermContext, InstanceMethods,
            !InstanceDefn, !ModuleInfo, !QualInfo, !Specs)
    ),
    % Check that the superclass constraints are satisfied for the types
    % in this instance declaration.
    check_superclass_conformance(ClassId, SuperClasses, Vars, ClassVarSet,
        !.ModuleInfo, !InstanceDefn, !Specs).

:- pred check_concrete_class_instance(class_id::in, list(tvar)::in,
    hlds_class_interface::in, class_interface::in,
    list(pred_id)::in, term.context::in,
    list(instance_method)::in, hlds_instance_defn::in, hlds_instance_defn::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_concrete_class_instance(ClassId, Vars, HLDSClassInterface,
        ClassInterface, ClassPredIds, TermContext, InstanceMethods,
        !InstanceDefn, !ModuleInfo, !QualInfo, !Specs) :-
    (
        ClassInterface = class_interface_abstract,
        ClassId = class_id(ClassName, ClassArity),
        Pieces = [words("Error: instance declaration for abstract typeclass"),
            qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
            suffix("."), nl],
        Msg = simple_msg(TermContext, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        ClassInterface = class_interface_concrete(_),
        list.foldl5(check_instance_pred(ClassId, Vars, HLDSClassInterface),
            ClassPredIds, !InstanceDefn, [], RevInstanceMethods,
            !ModuleInfo, !QualInfo, !Specs),

        % We need to make sure that the MaybePredProcs field is set to yes(_)
        % after this pass. Normally that will be handled by
        % check_instance_pred, but we also need to handle it below,
        % in case the class has no methods.
        MaybePredProcs1 = !.InstanceDefn ^ instdefn_hlds_interface,
        (
            MaybePredProcs1 = yes(_),
            MaybePredProcs = MaybePredProcs1
        ;
            MaybePredProcs1 = no,
            MaybePredProcs = yes([])
        ),

        % Make sure the list of instance methods is in the same order
        % as the methods in the class definition. intermod.m relies on this.
        OrderedInstanceMethods = list.reverse(RevInstanceMethods),

        !InstanceDefn ^ instdefn_hlds_interface := MaybePredProcs,
        !InstanceDefn ^ instdefn_body :=
            instance_body_concrete(OrderedInstanceMethods),

        % Check if there are any instance methods left over, which did not
        % match any of the methods from the class interface.
        % XXX This is not a check for *left over* methods, since we don't
        % subtract the methods we have successfully processed from any
        % initial InstanceMethods.
        Context = !.InstanceDefn ^ instdefn_context,
        check_for_unknown_methods(InstanceMethods, ClassId, ClassPredIds,
            Context, !.ModuleInfo, !Specs)
    ).

    % Check if there are any instance methods left over, which did not match
    % any of the methods from the class interface. If so, add an appropriate
    % error message to the list of error messages.
    %
:- pred check_for_unknown_methods(list(instance_method)::in, class_id::in,
    list(pred_id)::in, prog_context::in, module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_unknown_methods(InstanceMethods, ClassId, ClassPredIds, Context,
        ModuleInfo, !Specs) :-
    module_info_get_predicate_table(ModuleInfo, PredTable),
    list.filter(method_is_known(PredTable, ClassPredIds), InstanceMethods,
        _KnownInstanceMethods, UnknownInstanceMethods),
    (
        UnknownInstanceMethods = []
    ;
        UnknownInstanceMethods = [HeadMethod | TailMethods],
        report_unknown_instance_methods(ClassId, HeadMethod, TailMethods,
            Context, !Specs)
    ).

:- pred method_is_known(predicate_table::in, list(pred_id)::in,
    instance_method::in) is semidet.

method_is_known(PredTable, ClassPredIds, Method) :-
    % Find this method definition's p/f, name, arity.
    Method = instance_method(MethodPredOrFunc, MethodName, _MethodDefn,
        MethodArity, _Context),
    % Search for pred_ids matching that p/f, name, arity, and succeed
    % if the method definition p/f, name, and arity matches at least one
    % of the methods from the class interface.
    adjust_func_arity(MethodPredOrFunc, MethodArity, MethodPredArity),
    predicate_table_lookup_pf_sym_arity(PredTable, is_fully_qualified,
        MethodPredOrFunc, MethodName, MethodPredArity, MatchingPredIds),
    some [PredId] (
        list.member(PredId, MatchingPredIds),
        list.member(PredId, ClassPredIds)
    ).

%----------------------------------------------------------------------------%

    % Check one pred in one instance of one class.
    %
:- pred check_instance_pred(class_id::in, list(tvar)::in,
    hlds_class_interface::in, pred_id::in,
    hlds_instance_defn::in, hlds_instance_defn::out,
    list(instance_method)::in, list(instance_method)::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_pred(ClassId, ClassVars, ClassInterface, PredId,
        InstanceDefn0, InstanceDefn, !RevOrderedMethods,
        !ModuleInfo, !QualInfo, !Specs) :-
    GetClassProcProcId =
        ( pred(ClassProc::in, ClassProcProcId::out) is semidet :-
            ClassProc = proc(PredId, ClassProcProcId)
        ),
    list.filter_map(GetClassProcProcId, ClassInterface, ClassProcProcIds0),
    list.sort_and_remove_dups(ClassProcProcIds0, ClassProcProcIds),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypeVars, ExistQVars, ArgTypes),
    pred_info_get_class_context(PredInfo, ClassContext0),
    pred_info_get_markers(PredInfo, Markers0),
    remove_marker(marker_class_method, Markers0, Markers),
    % The first constraint in the class context of a class method is always
    % the constraint for the class of which it is a member. Seeing that we are
    % checking an instance declaration, we don't check that constraint...
    % the instance declaration itself satisfies it!
    ( if ClassContext0 = constraints([_ | OtherUnivCs], ExistCs) then
        UnivCs = OtherUnivCs,
        ClassContext = constraints(UnivCs, ExistCs)
    else
        unexpected($module, $pred, "no constraint on class method")
    ),
    MethodName0 = pred_info_name(PredInfo),
    PredModule = pred_info_module(PredInfo),
    MethodName = qualified(PredModule, MethodName0),
    PredArity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    adjust_func_arity(PredOrFunc, Arity, PredArity),
    pred_info_get_proc_table(PredInfo, ProcTable),
    list.map(
        (pred(TheProcId::in, ModesAndDetism::out) is det :-
            map.lookup(ProcTable, TheProcId, ProcInfo),
            proc_info_get_argmodes(ProcInfo, Modes),
            % If the determinism declaration on the method was omitted,
            % then make_hlds will have already issued an error message,
            % so don't complain here.
            proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
            proc_info_get_inst_varset(ProcInfo, InstVarSet),
            ModesAndDetism = modes_and_detism(Modes, InstVarSet, MaybeDetism)
        ), ClassProcProcIds, ArgModes),

    InstanceDefn0 = hlds_instance_defn(_, InstanceTypes, _, InstanceStatus,
        _, _, _, _, _, _),

    % Work out the name of the predicate that we will generate
    % to check this instance method.
    make_introduced_pred_name(ClassId, MethodName, Arity,
        InstanceTypes, PredName),

    CheckInfo0 = check_instance_method_info(PredOrFunc, PredName, Arity,
        ExistQVars, ArgTypes, ClassContext, ArgModes, ArgTypeVars,
        InstanceStatus),
    check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
        CheckInfo0, InstanceDefn0, InstanceDefn, !RevOrderedMethods,
        !ModuleInfo, !QualInfo, !Specs).

%----------------------------------------------------------------------------%

    % This structure holds the information we need to check
    % a particular instance method.
:- type check_instance_method_info
    --->    check_instance_method_info(
                % Is the method pred or func?
                cimi_method_pred_or_func    :: pred_or_func,

                % Name that the introduced pred should be given.
                cimi_introduced_pred_name   :: sym_name,

                % Arity of the method. (For funcs, this is the original arity,
                % not the arity as a predicate.)
                cimi_method_arity           :: arity,

                % Existentially quantified type variables.
                cimi_existq_tvars           :: existq_tvars,

                % Expected types of arguments.
                cimi_expected_arg_types     :: list(mer_type),

                % Constraints from class method.
                cimi_method_constraints     :: prog_constraints,

                % Modes and determinisms of the required procs.
                cimi_modes_and_detism       :: list(modes_and_detism),

                cimi_tvarset                :: tvarset,

                % Import status of instance decl.
                cimi_import_status          :: instance_status
            ).

:- type modes_and_detism
    --->    modes_and_detism(
                list(mer_mode),
                inst_varset,
                maybe(determinism)
            ).

:- pred check_instance_pred_procs(class_id::in, list(tvar)::in, sym_name::in,
    pred_markers::in, check_instance_method_info::in,
    hlds_instance_defn::in, hlds_instance_defn::out,
    list(instance_method)::in, list(instance_method)::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_instance_pred_procs(ClassId, ClassVars, MethodName, Markers,
        CheckInfo, InstanceDefn0, InstanceDefn,
        !RevInstanceMethods, !ModuleInfo, !QualInfo, !Specs) :-
    InstanceDefn0 = hlds_instance_defn(InstanceModuleName,
        InstanceTypes, _OriginalTypes, _InstanceStatus, _InstanceContext,
        InstanceConstraints, InstanceBody, MaybeInstancePredProcs,
        InstanceVarSet, _InstanceProofs),
    PredOrFunc = CheckInfo ^ cimi_method_pred_or_func,
    Arity = CheckInfo ^ cimi_method_arity,
    get_matching_instance_defns(InstanceBody, PredOrFunc, MethodName, Arity,
        MatchingInstanceMethods),
    (
        MatchingInstanceMethods = [InstanceMethod],
        !:RevInstanceMethods = [InstanceMethod | !.RevInstanceMethods],
        InstanceMethod = instance_method(_, _, InstancePredDefn, _, Context),
        produce_auxiliary_procs(ClassId, ClassVars, MethodName, Markers,
            InstanceTypes, InstanceConstraints,
            InstanceVarSet, InstanceModuleName,
            InstancePredDefn, Context,
            InstancePredId, InstanceProcIds,
            CheckInfo, !ModuleInfo, !QualInfo, !Specs),
        MakeClassProc =
            ( pred(TheProcId::in, PredProcId::out) is det :-
                PredProcId = proc(InstancePredId, TheProcId)
            ),
        list.map(MakeClassProc, InstanceProcIds, InstancePredProcs1),
        (
            MaybeInstancePredProcs = yes(InstancePredProcs0),
            InstancePredProcs = InstancePredProcs0 ++ InstancePredProcs1
        ;
            MaybeInstancePredProcs = no,
            InstancePredProcs = InstancePredProcs1
        ),
        InstanceDefn = InstanceDefn0
            ^ instdefn_hlds_interface := yes(InstancePredProcs)
    ;
        MatchingInstanceMethods = [_, _ | _],
        InstanceDefn = InstanceDefn0,
        report_duplicate_method_defn(ClassId, InstanceDefn0, PredOrFunc,
            MethodName, Arity, MatchingInstanceMethods, !Specs)
    ;
        MatchingInstanceMethods = [],
        InstanceDefn = InstanceDefn0,
        report_undefined_method(ClassId, InstanceDefn0, PredOrFunc,
            MethodName, Arity, !Specs)
    ).

    % Get all the instance definitions which match the specified
    % predicate/function name/arity, with multiple clause definitions
    % being combined into a single definition.
    %
:- pred get_matching_instance_defns(instance_body::in, pred_or_func::in,
    sym_name::in, arity::in, list(instance_method)::out) is det.

get_matching_instance_defns(instance_body_abstract, _, _, _, []).
get_matching_instance_defns(instance_body_concrete(InstanceMethods),
        PredOrFunc, MethodName, MethodArity, ResultList) :-
    % First find the instance method definitions that match this
    % predicate/function's name and arity
    list.filter(
        ( pred(Method::in) is semidet :-
            Method = instance_method(PredOrFunc, MethodName, _MethodDefn,
                MethodArity, _Context)
        ),
        InstanceMethods, MatchingMethods),
    ( if
        MatchingMethods = [First, _Second | _],
        FirstContext = First ^ instance_method_decl_context,
        not (
            list.member(DefnViaName, MatchingMethods),
            DefnViaName = instance_method(_, _, InstanceProcDef, _, _),
            InstanceProcDef = DefnViaName ^ instance_method_proc_def,
            InstanceProcDef = instance_proc_def_name(_)
        )
    then
        % If all of the instance method definitions for this pred/func
        % are clauses, and there are more than one of them, then we must
        % combine them all into a single definition.
        MethodToClause = (pred(Method::in, Clauses::out) is semidet :-
            Method = instance_method(_, _, Defn, _, _),
            Defn = instance_proc_def_clauses(Clauses)
        ),
        list.filter_map(MethodToClause, MatchingMethods, ClausesList),
        list.condense(ClausesList, FlattenedClauses),
        CombinedMethod = instance_method(PredOrFunc, MethodName,
            instance_proc_def_clauses(FlattenedClauses), MethodArity,
            FirstContext),
        ResultList = [CombinedMethod]
    else
        % If there are less than two matching method definitions,
        % or if any of the instance method definitions is a method name,
        % then we're done.
        ResultList = MatchingMethods
    ).

:- pred produce_auxiliary_procs(class_id::in, list(tvar)::in, sym_name::in,
    pred_markers::in, list(mer_type)::in, list(prog_constraint)::in,
    tvarset::in, module_name::in, instance_proc_def::in, prog_context::in,
    pred_id::out, list(proc_id)::out,
    check_instance_method_info::in, module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

produce_auxiliary_procs(ClassId, ClassVars, MethodName, Markers0,
        InstanceTypes0, InstanceConstraints0, InstanceVarSet,
        InstanceModuleName, InstancePredDefn, Context, PredId, InstanceProcIds,
        CheckInfo0, !ModuleInfo, !QualInfo, !Specs) :-
    CheckInfo0 = check_instance_method_info(PredOrFunc, PredName, Arity,
        ExistQVars0, ArgTypes0, ClassMethodClassContext0, ArgModes,
        TVarSet0, InstanceStatus0),
    UnsubstArgTypes = ArgTypes0,

    % Rename the instance variables apart from the class variables.
    tvarset_merge_renaming(TVarSet0, InstanceVarSet, TVarSet1, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes1),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        InstanceConstraints0, InstanceConstraints1),

    % Work out what the type variables are bound to for this
    % instance, and update the class types appropriately.
    map.from_corresponding_lists(ClassVars, InstanceTypes1, TypeSubst),
    apply_subst_to_type_list(TypeSubst, ArgTypes0, ArgTypes1),
    apply_subst_to_prog_constraints(TypeSubst, ClassMethodClassContext0,
        ClassMethodClassContext1),

    % Calculate which type variables we need to keep. This includes all
    % type variables appearing in the arguments, the class method context and
    % the instance constraints. (Type variables in the existq_tvars must
    % occur either in the argument types or in the class method context;
    % type variables in the instance types must appear in the arguments.)
    type_vars_list(ArgTypes1, ArgTVars),
    prog_constraints_get_tvars(ClassMethodClassContext1, MethodContextTVars),
    constraint_list_get_tvars(InstanceConstraints1, InstanceTVars),
    list.condense([ArgTVars, MethodContextTVars, InstanceTVars], VarsToKeep0),
    list.sort_and_remove_dups(VarsToKeep0, VarsToKeep),

    % Project away the unwanted type variables.
    varset.squash(TVarSet1, VarsToKeep, TVarSet2, SquashSubst),
    apply_variable_renaming_to_type_list(SquashSubst, ArgTypes1, ArgTypes),
    apply_variable_renaming_to_prog_constraints(SquashSubst,
        ClassMethodClassContext1, ClassMethodClassContext),
    apply_partial_map_to_list(SquashSubst, ExistQVars0, ExistQVars),
    apply_variable_renaming_to_type_list(SquashSubst, InstanceTypes1,
        InstanceTypes),
    apply_variable_renaming_to_prog_constraint_list(SquashSubst,
        InstanceConstraints1, InstanceConstraints),

    % Add the constraints from the instance declaration to the constraints
    % from the class method. This allows an instance method to have constraints
    % on it which are not part of the instance declaration as a whole.
    ClassMethodClassContext = constraints(UnivConstraints1, ExistConstraints),
    list.append(InstanceConstraints, UnivConstraints1, UnivConstraints),
    ClassContext = constraints(UnivConstraints, ExistConstraints),

    % Introduce a new predicate which calls the implementation
    % given in the instance declaration.
    map.init(Proofs),
    map.init(ConstraintMap),
    add_marker(marker_class_instance_method, Markers0, Markers1),
    (
        InstancePredDefn = instance_proc_def_name(_),
        % For instance methods which are defined using the named syntax
        % (e.g. "pred(...) is ...") rather than the clauses syntax, we record
        % an additional marker; the only effect of this marker is that we
        % output slightly different error messages for such predicates.
        add_marker(marker_named_class_instance_method, Markers1, Markers)
    ;
        InstancePredDefn = instance_proc_def_clauses(_),
        Markers = Markers1
    ),

    IsImported = instance_status_is_imported(InstanceStatus0),
    (
        IsImported = yes,
        InstanceStatus = instance_status(status_opt_imported)
    ;
        IsImported = no,
        InstanceStatus = InstanceStatus0
    ),

    adjust_func_arity(PredOrFunc, Arity, PredArity),
    produce_instance_method_clauses(InstancePredDefn, PredOrFunc,
        PredArity, ArgTypes, Markers, Context, InstanceStatus, ClausesInfo,
        TVarSet2, TVarSet, !ModuleInfo, !QualInfo, !Specs),

    % Fill in some information in the pred_info which is used by polymorphism
    % to make sure the type-infos and typeclass-infos are added in the correct
    % order.
    MethodConstraints = instance_method_constraints(ClassId,
        InstanceTypes, InstanceConstraints, ClassMethodClassContext),
    PredOrigin = origin_instance_method(MethodName, MethodConstraints),
    map.init(VarNameRemap),
    % XXX STATUS
    InstanceStatus = instance_status(OldImportStatus),
    PredStatus = pred_status(OldImportStatus),
    CurUserDecl = maybe.no,
    pred_info_init(InstanceModuleName, PredName, PredArity, PredOrFunc,
        Context, PredOrigin, PredStatus, CurUserDecl, goal_type_none, Markers,
        ArgTypes, TVarSet, ExistQVars, ClassContext, Proofs, ConstraintMap,
        ClausesInfo, VarNameRemap, PredInfo0),
    pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo1),
    pred_info_set_instance_method_arg_types(UnsubstArgTypes,
        PredInfo1, PredInfo2),

    % Add procs with the expected modes and determinisms
    AddProc = (pred(ModeAndDet::in, NewProcId::out,
            OldPredInfo::in, NewPredInfo::out) is det :-
        ModeAndDet = modes_and_detism(Modes, InstVarSet, MaybeDet),
        ItemNumber = -1,
        % Before the simplification pass, HasParallelConj is not meaningful.
        HasParallelConj = has_no_parallel_conj,
        add_new_proc(Context, ItemNumber, PredArity,
            InstVarSet, Modes, yes(Modes), no, detism_decl_implicit, MaybeDet,
            address_is_taken, HasParallelConj,
            OldPredInfo, NewPredInfo, NewProcId)
    ),
    list.map_foldl(AddProc, ArgModes, InstanceProcIds, PredInfo2, PredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
    module_info_get_partial_qualifier_info(!.ModuleInfo, PQInfo),
    % XXX Why do we need to pass may_be_unqualified here, rather than passing
    % must_be_qualified or calling the predicate_table_insert/4 version?
    predicate_table_insert_qual(PredInfo, may_be_unqualified, PQInfo, PredId,
        PredicateTable1, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo).

%---------------------------------------------------------------------------%

    % Make the name of the introduced pred used to check a particular
    % instance of a particular class method
    %
    % XXX This isn't quite perfect, I suspect
    %
:- pred make_introduced_pred_name(class_id::in, sym_name::in, arity::in,
    list(mer_type)::in, sym_name::out) is det.

make_introduced_pred_name(ClassId, MethodName, Arity, InstanceTypes,
        PredName) :-
    ClassId = class_id(ClassName, _ClassArity),
    ClassNameString = sym_name_to_string_sep(ClassName, "__"),
    MethodNameString = sym_name_to_string_sep(MethodName, "__"),
    % Perhaps we should include the arity in this mangled string?
    string.int_to_string(Arity, ArityString),
    make_instance_string(InstanceTypes, InstanceString),
    string.append_list(
        [introduced_pred_name_prefix,
        ClassNameString, "____",
        InstanceString, "____",
        MethodNameString, "_",
        ArityString],
        PredNameString),
    PredName = unqualified(PredNameString).

    % The prefix added to the class method name for the predicate
    % used to call a class method for a specific instance.
    %
:- func introduced_pred_name_prefix = string.

introduced_pred_name_prefix = "ClassMethod_for_".

%---------------------------------------------------------------------------%

    % Check that the superclass constraints are satisfied for the
    % types in this instance declaration.
    %
:- pred check_superclass_conformance(class_id::in, list(prog_constraint)::in,
    list(tvar)::in, tvarset::in, module_info::in,
    hlds_instance_defn::in, hlds_instance_defn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_superclass_conformance(ClassId, ProgSuperClasses0, ClassVars0,
        ClassVarSet, ModuleInfo, InstanceDefn0, InstanceDefn, !Specs) :-
    InstanceDefn0 = hlds_instance_defn(ModuleName,
        InstanceTypes, OriginalTypes, Status, Context, InstanceProgConstraints,
        Body, Interface, InstanceVarSet0, Proofs0),
    tvarset_merge_renaming(InstanceVarSet0, ClassVarSet, InstanceVarSet1,
        Renaming),

    % Make the constraints in terms of the instance variables.
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        ProgSuperClasses0, ProgSuperClasses),

    % Now handle the class variables.
    apply_variable_renaming_to_tvar_list(Renaming, ClassVars0, ClassVars),

    % Calculate the bindings.
    map.from_corresponding_lists(ClassVars, InstanceTypes, TypeSubst),

    module_info_get_class_table(ModuleInfo, ClassTable),
    module_info_get_instance_table(ModuleInfo, InstanceTable),

    % Build a suitable constraint context for checking the instance.
    % To do this, we assume any constraints on the instance declaration
    % (that is, treat them as universal constraints on a predicate) and try
    % to prove the constraints on the class declaration (that is, treat them
    % as existential constraints on a predicate).
    %
    % We don't bother assigning ids to these constraints, since the resulting
    % constraint map is not used anyway.
    %
    init_hlds_constraint_list(ProgSuperClasses, SuperClasses),
    init_hlds_constraint_list(InstanceProgConstraints, InstanceConstraints),
    make_hlds_constraints(ClassTable, InstanceVarSet1, SuperClasses,
        InstanceConstraints, Constraints0),

    % Try to reduce the superclass constraints, using the declared instance
    % constraints and the usual context reduction rules.
    map.init(ConstraintMap0),
    typeclasses.reduce_context_by_rule_application(ClassTable, InstanceTable,
        ClassVars, TypeSubst, _, InstanceVarSet1, InstanceVarSet2,
        Proofs0, Proofs1, ConstraintMap0, _, Constraints0, Constraints),
    UnprovenConstraints = Constraints ^ hcs_unproven,

    (
        UnprovenConstraints = [],
        InstanceDefn = hlds_instance_defn(ModuleName,
            InstanceTypes, OriginalTypes, Status, Context,
            InstanceProgConstraints, Body, Interface, InstanceVarSet2, Proofs1)
    ;
        UnprovenConstraints = [_ | UnprovenConstraintsTail],
        ClassId = class_id(ClassName, _ClassArity),
        ClassNameString = sym_name_to_string(ClassName),
        InstanceTypesString = mercury_type_list_to_string(InstanceVarSet2,
            InstanceTypes),
        constraint_list_to_string(ClassVarSet, UnprovenConstraints,
            ConstraintsString),
        Pieces = [words("In instance declaration for"),
            words_quote(ClassNameString ++ "(" ++ InstanceTypesString ++ ")"),
            words(choose_number(UnprovenConstraintsTail,
                "superclass constraint", "superclass constraints")),
            words("not satisfied:"), words(ConstraintsString), suffix("."),
            nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs],
        InstanceDefn = InstanceDefn0
    ).

:- pred constraint_list_to_string(tvarset::in, list(hlds_constraint)::in,
    string::out) is det.

constraint_list_to_string(_, [], "").
constraint_list_to_string(VarSet, [C | Cs], String) :-
    retrieve_prog_constraint(C, P),
    String0 = mercury_constraint_to_string(VarSet, P),
    constraint_list_to_string_2(VarSet, Cs, String1),
    string.append_list(["`", String0, "'", String1], String).

:- pred constraint_list_to_string_2(tvarset::in, list(hlds_constraint)::in,
    string::out) is det.

constraint_list_to_string_2(_VarSet, [], "").
constraint_list_to_string_2(VarSet, [C | Cs], String) :-
    retrieve_prog_constraint(C, P),
    String0 = mercury_constraint_to_string(VarSet, P),
    constraint_list_to_string_2(VarSet, Cs, String1),
    string.append_list([", `", String0, "'", String1], String).

%---------------------------------------------------------------------------%

    % Check that every abstract instance in the module has a
    % corresponding concrete instance in the implementation.
    %
:- pred check_for_missing_concrete_instances(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_missing_concrete_instances(ModuleInfo, !Specs) :-
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    % Grab all the instance declarations that occur in this module
    % and partition them into two sets: abstract instance declarations
    % and concrete instance declarations.
    gather_abstract_and_concrete_instances(InstanceTable,
        AbstractInstances, ConcreteInstances),
    map.foldl(check_for_corresponding_instances(ConcreteInstances),
        AbstractInstances, !Specs).

    % Search the instance_table and create a table of abstract instances
    % that occur in the module and a table of concrete instances that
    % occur in the module. Imported instances are not included at all.
    %
:- pred gather_abstract_and_concrete_instances(instance_table::in,
    instance_table::out, instance_table::out) is det.

gather_abstract_and_concrete_instances(InstanceTable, Abstracts, Concretes) :-
    map.foldl2(partition_instances_for_class, InstanceTable,
        multi_map.init, Abstracts, multi_map.init, Concretes).

    % Partition all the non-imported instances for a particular class
    % into two groups, those that are abstract and those that are concrete.
    %
:- pred partition_instances_for_class(class_id::in,
    list(hlds_instance_defn)::in, instance_table::in, instance_table::out,
    instance_table::in, instance_table::out) is det.

partition_instances_for_class(ClassId, Instances, !Abstracts, !Concretes) :-
    list.foldl2(partition_instances_for_class_2(ClassId), Instances,
        !Abstracts, !Concretes).

:- pred partition_instances_for_class_2(class_id::in, hlds_instance_defn::in,
    instance_table::in, instance_table::out,
    instance_table::in, instance_table::out) is det.

partition_instances_for_class_2(ClassId, InstanceDefn,
        !Abstracts, !Concretes) :-
    InstanceStatus = InstanceDefn ^ instdefn_status,
    IsImported = instance_status_is_imported(InstanceStatus),
    (
        IsImported = no,
        Body = InstanceDefn ^ instdefn_body,
        (
            Body = instance_body_abstract,
            multi_map.add(ClassId, InstanceDefn, !Abstracts)
        ;
            Body = instance_body_concrete(_),
            multi_map.add(ClassId, InstanceDefn, !Concretes)
        )
    ;
        IsImported = yes
    ).

:- pred check_for_corresponding_instances(instance_table::in,
    class_id::in, list(hlds_instance_defn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_corresponding_instances(Concretes, ClassId, InstanceDefns, !Specs) :-
    list.foldl(check_for_corresponding_instances_2(Concretes, ClassId),
        InstanceDefns, !Specs).

:- pred check_for_corresponding_instances_2(instance_table::in, class_id::in,
    hlds_instance_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_corresponding_instances_2(Concretes, ClassId, AbstractInstance,
        !Specs) :-
    AbstractTypes = AbstractInstance ^ instdefn_types,
    ( if multi_map.search(Concretes, ClassId, ConcreteInstances) then
        ( if
            list.member(ConcreteInstance, ConcreteInstances),
            ConcreteTypes = ConcreteInstance ^ instdefn_types,
            ConcreteTypes = AbstractTypes
        then
            MissingConcreteError = no
        else
            % There were concrete instances for ClassId in the implementation
            % but none of them matches the abstract instance we have.
            MissingConcreteError = yes
        )
    else
        % There were no concrete instances for ClassId in the implementation.
        MissingConcreteError = yes
    ),
    (
        MissingConcreteError = yes,
        ClassId = class_id(ClassName, _),
        ClassNameString = sym_name_to_string(ClassName),
        AbstractTypesString = mercury_type_list_to_string(
            AbstractInstance ^ instdefn_tvarset, AbstractTypes),
        AbstractInstanceName = ClassNameString ++
            "(" ++ AbstractTypesString ++ ")",
        % XXX Should we mention any constraints on the instance declaration?
        Pieces = [words("Error: abstract instance declaration"),
            words("for"), quote(AbstractInstanceName),
            words("has no corresponding concrete"),
            words("instance in the implementation."), nl],
        AbstractInstanceContext = AbstractInstance ^ instdefn_context,
        Msg = simple_msg(AbstractInstanceContext, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        MissingConcreteError = no
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check for cyclic classes in the class table by traversing the class
    % hierarchy for each class. While we are doing this, calculate the set
    % of ancestors with functional dependencies for each class, and enter
    % this information in the class table.
    %
:- pred check_for_cyclic_classes(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_cyclic_classes(!ModuleInfo, !Specs) :-
    module_info_get_class_table(!.ModuleInfo, ClassTable0),
    ClassIds = map.keys(ClassTable0),
    list.foldl3(find_cycles([]), ClassIds, ClassTable0, ClassTable,
        set.init, _, [], Cycles),
    !:Specs = list.map(report_cyclic_classes(ClassTable), Cycles) ++ !.Specs,
    module_info_set_class_table(ClassTable, !ModuleInfo).

:- type class_path == list(class_id).

    % find_cycles(Path, ClassId, !ClassTable, !Visited, !Cycles)
    %
    % Perform a depth first traversal of the class hierarchy, starting
    % from ClassId. Path contains a list of nodes joining the current node
    % to the root. When we reach a node that has already been visited,
    % check whether there is a cycle in the Path.
    %
:- pred find_cycles(class_path::in, class_id::in,
    class_table::in, class_table::out, set(class_id)::in, set(class_id)::out,
    list(class_path)::in, list(class_path)::out) is det.

find_cycles(Path, ClassId, !ClassTable, !Visited, !Cycles) :-
    find_cycles_2(Path, ClassId, _, _, !ClassTable, !Visited, !Cycles).

    % As above, but also return this class's parameters and ancestor list.
    %
:- pred find_cycles_2(class_path::in, class_id::in, list(tvar)::out,
    list(prog_constraint)::out, class_table::in, class_table::out,
    set(class_id)::in, set(class_id)::out,
    list(class_path)::in, list(class_path)::out) is det.

find_cycles_2(Path, ClassId, Params, Ancestors, !ClassTable, !Visited,
        !Cycles) :-
    ClassDefn0 = map.lookup(!.ClassTable, ClassId),
    Params = ClassDefn0 ^ classdefn_vars,
    Kinds = ClassDefn0 ^ classdefn_kinds,
    ( if set.member(ClassId, !.Visited) then
        ( if find_cycle(ClassId, Path, [ClassId], Cycle) then
            !:Cycles = [Cycle | !.Cycles]
        else
            true
        ),
        Ancestors = ClassDefn0 ^ classdefn_fundep_ancestors
    else
        set.insert(ClassId, !Visited),

        % Make this class its own ancestor, but only if it has fundeps on it.
        FunDeps = ClassDefn0 ^ classdefn_fundeps,
        (
            FunDeps = [],
            Ancestors0 = []
        ;
            FunDeps = [_ | _],
            ClassId = class_id(ClassName, _),
            prog_type.var_list_to_type_list(Kinds, Params, Args),
            Ancestors0 = [constraint(ClassName, Args)]
        ),
        Superclasses = ClassDefn0 ^ classdefn_supers,
        list.foldl4(find_cycles_3([ClassId | Path]), Superclasses,
            !ClassTable, !Visited, !Cycles, Ancestors0, Ancestors),
        ClassDefn = ClassDefn0 ^ classdefn_fundep_ancestors := Ancestors,
        map.det_update(ClassId, ClassDefn, !ClassTable)
    ).

    % As we go, accumulate the ancestors from all the superclasses,
    % with the class parameters bound to the corresponding arguments.
    % Note that we don't need to merge varsets because typeclass
    % parameters are guaranteed to be distinct variables.
    %
:- pred find_cycles_3(class_path::in, prog_constraint::in,
    class_table::in, class_table::out,
    set(class_id)::in, set(class_id)::out,
    list(class_path)::in, list(class_path)::out,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

find_cycles_3(Path, Constraint, !ClassTable, !Visited, !Cycles, !Ancestors) :-
    Constraint = constraint(Name, Args),
    list.length(Args, Arity),
    ClassId = class_id(Name, Arity),
    find_cycles_2(Path, ClassId, Params, NewAncestors0, !ClassTable,
        !Visited, !Cycles),
    map.from_corresponding_lists(Params, Args, Binding),
    apply_subst_to_prog_constraint_list(Binding, NewAncestors0, NewAncestors),
    list.append(NewAncestors, !Ancestors).

    % find_cycle(ClassId, PathRemaining, PathSoFar, Cycle):
    %
    % Check if ClassId is present in PathRemaining, and if so then make
    % a cycle out of the front part of the path up to the point where
    % the ClassId is found. The part of the path checked so far is
    % accumulated in PathSoFar.
    %
:- pred find_cycle(class_id::in, class_path::in, class_path::in,
    class_path::out) is semidet.

find_cycle(ClassId, [Head | Tail], Path0, Cycle) :-
    Path = [Head | Path0],
    ( if ClassId = Head then
        Cycle = Path
    else
        find_cycle(ClassId, Tail, Path, Cycle)
    ).

    % The error message for cyclic classes is intended to look like this:
    %
    %   module.m:NNN: Error: cyclic superclass relation detected:
    %   module.m:NNN:   `foo/N' <= `bar/N' <= `baz/N' <= `foo/N'
    %
:- func report_cyclic_classes(class_table, class_path) = error_spec.

report_cyclic_classes(ClassTable, ClassPath) = Spec :-
    (
        ClassPath = [],
        unexpected($module, $pred, "empty cycle found.")
    ;
        ClassPath = [ClassId | Tail],
        Context = map.lookup(ClassTable, ClassId) ^ classdefn_context,
        ClassId = class_id(Name, Arity),
        StartPieces =
            [words("Error: cyclic superclass relation detected:"), nl,
            qual_sym_name_and_arity(sym_name_arity(Name, Arity)), nl],
        list.foldl(add_path_element, Tail, cord.init, LaterLinesCord),
        Pieces = StartPieces ++ cord.list(LaterLinesCord),
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg])
    ).

:- pred add_path_element(class_id::in,
    cord(format_component)::in, cord(format_component)::out) is det.

add_path_element(class_id(SymName, Arity), !LaterLines) :-
    Line = [words("<="),
        qual_sym_name_and_arity(sym_name_arity(SymName, Arity)), nl],
    !:LaterLines = !.LaterLines ++ cord.from_list(Line).

%---------------------------------------------------------------------------%

    % Check that all instances are range restricted with respect to the
    % functional dependencies. This means that, for each functional dependency,
    % the set of tvars in the range arguments must be a subset of the set
    % of tvars in the domain arguments. (Note that with the requirement of
    % distinct variables as arguments, this implies that all range arguments
    % must be ground. However, this code should work even if that requirement
    % is lifted in the future.)
    %
    % Also, check that all pairs of visible instances are mutually consistent
    % with respect to the functional dependencies. This is true iff the most
    % general unifier of corresponding domain arguments (if it exists) is
    % also a unifier of the corresponding range arguments.
    %
:- pred check_functional_dependencies(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_functional_dependencies(ModuleInfo, !Specs) :-
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.keys(InstanceTable, ClassIds),
    list.foldl(check_fundeps_for_class(ModuleInfo), ClassIds, !Specs).

:- pred check_fundeps_for_class(module_info::in, class_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_fundeps_for_class(ModuleInfo, ClassId, !Specs) :-
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(ClassTable, ClassId, ClassDefn),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    map.lookup(InstanceTable, ClassId, InstanceDefns),
    FunDeps = ClassDefn ^ classdefn_fundeps,
    check_coverage_for_instance_defns(ModuleInfo, ClassId, InstanceDefns,
        FunDeps, !Specs),
    module_info_get_globals(ModuleInfo, Globals),
    % Abstract definitions will always overlap with concrete definitions,
    % so we filter out the abstract definitions for this module. If
    % --intermodule-optimization is enabled then we strip out the imported
    % abstract definitions for all modules, since we will have the concrete
    % definitions for imported instances from the .opt files. If it is not
    % enabled, then we keep the abstract definitions for imported instances
    % since doing so may allow us to detect errors.
    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    (
        IntermodOpt = yes,
        list.filter(is_concrete_instance_defn, InstanceDefns,
            ConcreteInstanceDefns)
    ;
        IntermodOpt = no,
        list.filter(is_concrete_or_imported_instance_defn, InstanceDefns,
            ConcreteInstanceDefns)
    ),
    check_consistency(ClassId, ClassDefn, ConcreteInstanceDefns, FunDeps,
        !Specs).

:- pred is_concrete_instance_defn(hlds_instance_defn::in) is semidet.

is_concrete_instance_defn(InstanceDefn) :-
    InstanceDefn ^ instdefn_body = instance_body_concrete(_).

:- pred is_concrete_or_imported_instance_defn(hlds_instance_defn::in)
    is semidet.

is_concrete_or_imported_instance_defn(InstanceDefn) :-
    (
        is_concrete_instance_defn(InstanceDefn)
    ;
        instance_status_is_imported(InstanceDefn ^ instdefn_status) = yes
    ).

:- pred check_coverage_for_instance_defns(module_info::in, class_id::in,
    list(hlds_instance_defn)::in, hlds_class_fundeps::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_coverage_for_instance_defns(_, _, [], _, !Specs).
check_coverage_for_instance_defns(ModuleInfo, ClassId,
        [InstanceDefn | InstanceDefns], FunDeps, !Specs) :-
    list.foldl(
        check_coverage_for_instance_defn(ModuleInfo, ClassId, InstanceDefn),
        FunDeps, !Specs),
    check_coverage_for_instance_defns(ModuleInfo, ClassId,
        InstanceDefns, FunDeps, !Specs).

:- pred check_coverage_for_instance_defn(module_info::in, class_id::in,
    hlds_instance_defn::in, hlds_class_fundep::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_coverage_for_instance_defn(ModuleInfo, ClassId, InstanceDefn, FunDep,
        !Specs) :-
    TVarSet = InstanceDefn ^ instdefn_tvarset,
    Types = InstanceDefn ^ instdefn_types,
    FunDep = fundep(Domain, Range),
    DomainTypes = restrict_list_elements(Domain, Types),
    type_vars_list(DomainTypes, DomainVars),
    RangeTypes = restrict_list_elements(Range, Types),
    type_vars_list(RangeTypes, RangeVars),
    Constraints = InstanceDefn ^ instdefn_constraints,
    get_unbound_tvars(ModuleInfo, TVarSet, DomainVars, RangeVars,
        Constraints, UnboundVars),
    (
        UnboundVars = []
    ;
        UnboundVars = [_ | _],
        Spec = report_coverage_error(ClassId, InstanceDefn, UnboundVars),
        !:Specs = [Spec | !.Specs]
    ).

    % Check the consistency of each (unordered) pair of instances.
    %
:- pred check_consistency(class_id::in, hlds_class_defn::in,
    list(hlds_instance_defn)::in, hlds_class_fundeps::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_consistency(_, _, [], _, !Specs).
check_consistency(ClassId, ClassDefn, [Instance | Instances], FunDeps,
        !Specs) :-
    list.foldl(check_consistency_pair(ClassId, ClassDefn, FunDeps, Instance),
        Instances, !Specs),
    check_consistency(ClassId, ClassDefn, Instances, FunDeps,
        !Specs).

:- pred check_consistency_pair(class_id::in, hlds_class_defn::in,
    hlds_class_fundeps::in, hlds_instance_defn::in, hlds_instance_defn::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_consistency_pair(ClassId, ClassDefn, FunDeps, InstanceA, InstanceB,
        !Specs) :-
    % If both instances are imported from the same module then we don't need
    % to check the consistency, since this would have been checked when
    % compiling that module.
    ( if
        InstanceA ^ instdefn_module = InstanceB ^ instdefn_module,
        instance_status_is_imported(InstanceA ^ instdefn_status) = yes
    then
        true
    else
        list.foldl(
            check_consistency_pair_2(ClassId, ClassDefn, InstanceA, InstanceB),
            FunDeps, !Specs)
    ).

:- pred check_consistency_pair_2(class_id::in, hlds_class_defn::in,
    hlds_instance_defn::in, hlds_instance_defn::in, hlds_class_fundep::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_consistency_pair_2(ClassId, ClassDefn, InstanceA, InstanceB, FunDep,
        !Specs) :-
    TVarSetA = InstanceA ^ instdefn_tvarset,
    TVarSetB = InstanceB ^ instdefn_tvarset,
    tvarset_merge_renaming(TVarSetA, TVarSetB, _, Renaming),

    TypesA = InstanceA ^ instdefn_types,
    TypesB0 = InstanceB ^ instdefn_types,
    apply_variable_renaming_to_type_list(Renaming, TypesB0, TypesB),

    FunDep = fundep(Domain, Range),
    DomainA = restrict_list_elements(Domain, TypesA),
    DomainB = restrict_list_elements(Domain, TypesB),

    ( if type_unify_list(DomainA, DomainB, [], map.init, Subst) then
        RangeA0 = restrict_list_elements(Range, TypesA),
        RangeB0 = restrict_list_elements(Range, TypesB),
        apply_rec_subst_to_type_list(Subst, RangeA0, RangeA),
        apply_rec_subst_to_type_list(Subst, RangeB0, RangeB),
        ( if RangeA = RangeB then
            true
        else
            Spec = report_consistency_error(ClassId, ClassDefn,
                InstanceA, InstanceB, FunDep),
            !:Specs = [Spec | !.Specs]
        )
    else
        true
    ).

    % The coverage error message is intended to look like this:
    %
    % long_module_name:001: In instance for typeclass `long_class/2':
    % long_module_name:001:   functional dependency not satisfied: type
    % long_module_name:001:   variables T1, T2 and T3 occur in the range of a
    % long_module_name:001:   functional dependency, but are not determined
    % long_module_name:001:   by the domain.
    %
:- func report_coverage_error(class_id, hlds_instance_defn, list(tvar))
    = error_spec.

report_coverage_error(ClassId, InstanceDefn, Vars) = Spec :-
    ClassId = class_id(SymName, Arity),
    TVarSet = InstanceDefn ^ instdefn_tvarset,
    Context = InstanceDefn ^ instdefn_context,

    VarsStrs = list.map(mercury_var_to_name_only(TVarSet), Vars),
    Pieces = [words("In instance for typeclass"),
        qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        suffix(":"), nl,
        words("functional dependency not satisfied:"),
        words(choose_number(Vars, "type variable", "type variables"))]
        ++ list_to_quoted_pieces(VarsStrs) ++
        [words(choose_number(Vars, "occurs", "occur")),
        words("in the range of the functional dependency, but"),
        words(choose_number(Vars, "is", "are")),
        words("not determined by the domain."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- func report_consistency_error(class_id, hlds_class_defn,
    hlds_instance_defn, hlds_instance_defn, hlds_class_fundep) = error_spec.

report_consistency_error(ClassId, ClassDefn, InstanceA, InstanceB, FunDep)
        = Spec :-
    ClassId = class_id(SymName, Arity),
    Params = ClassDefn ^ classdefn_vars,
    TVarSet = ClassDefn ^ classdefn_tvarset,
    ContextA = InstanceA ^ instdefn_context,
    ContextB = InstanceB ^ instdefn_context,

    FunDep = fundep(Domain, Range),
    DomainParams = restrict_list_elements(Domain, Params),
    RangeParams = restrict_list_elements(Range, Params),
    Domains = mercury_vars_to_name_only(TVarSet, DomainParams),
    Ranges = mercury_vars_to_name_only(TVarSet, RangeParams),

    PiecesA = [words("Inconsistent instance declaration for typeclass"),
        qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        words("with functional dependency"),
        quote("(" ++ Domains ++ " -> " ++ Ranges ++ ")"),
        suffix("."), nl],
    PiecesB = [words("Here is the conflicting instance.")],

    MsgA = simple_msg(ContextA, [always(PiecesA)]),
    MsgB = error_msg(yes(ContextB), treat_as_first, 0, [always(PiecesB)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [MsgA, MsgB]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Look for pred or func declarations for which the type variables in
    % the constraints are not all determined by the type variables in the type
    % and the functional dependencies. Likewise look for constructors for which
    % the existential type variables in the constraints are not all determined
    % by the type variables in the constructor arguments and the functional
    % dependencies.
    %
:- pred check_typeclass_constraints(module_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_typeclass_constraints(ModuleInfo, !Specs) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.foldl(check_pred_constraints(ModuleInfo), PredIds, !Specs),
    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    list.foldl(check_ctor_constraints(ModuleInfo), TypeCtorsDefns, !Specs).

:- pred check_pred_constraints(module_info::in, pred_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pred_constraints(ModuleInfo, PredId, !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, Status),
    NeedsAmbiguityCheck = pred_needs_ambiguity_check(Status),
    (
        NeedsAmbiguityCheck = no
    ;
        NeedsAmbiguityCheck = yes,
        trace [io(!IO)] (
            write_pred_progress_message("% Checking typeclass constraints on ",
                PredId, ModuleInfo, !IO)
        ),
        check_pred_type_ambiguities(ModuleInfo, PredInfo, !Specs),
        check_constraint_quant(PredInfo, !Specs)
    ).

:- func pred_needs_ambiguity_check(pred_status) = bool.

pred_needs_ambiguity_check(pred_status(status_imported(_))) =             no.
pred_needs_ambiguity_check(pred_status(status_external(_))) =             yes.
pred_needs_ambiguity_check(pred_status(status_abstract_imported)) =       no.
pred_needs_ambiguity_check(pred_status(status_pseudo_imported)) =         no.
pred_needs_ambiguity_check(pred_status(status_opt_imported)) =            no.
pred_needs_ambiguity_check(pred_status(status_exported)) =                yes.
pred_needs_ambiguity_check(pred_status(status_opt_exported)) =            yes.
pred_needs_ambiguity_check(pred_status(status_abstract_exported)) =       yes.
pred_needs_ambiguity_check(pred_status(status_pseudo_exported)) =         yes.
pred_needs_ambiguity_check(pred_status(status_exported_to_submodules)) =  yes.
pred_needs_ambiguity_check(pred_status(status_local)) =                   yes.

:- pred check_pred_type_ambiguities(module_info::in, pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pred_type_ambiguities(ModuleInfo, PredInfo, !Specs) :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_class_context(PredInfo, Constraints),
    type_vars_list(ArgTypes, ArgTVars),
    prog_constraints_get_tvars(Constraints, ConstrainedTVars),
    Constraints = constraints(UnivCs, ExistCs),
    get_unbound_tvars(ModuleInfo, TVarSet, ArgTVars, ConstrainedTVars,
        UnivCs ++ ExistCs, UnboundTVars),
    (
        UnboundTVars = []
    ;
        UnboundTVars = [_ | _],
        Spec = report_unbound_tvars_in_pred_context(UnboundTVars, PredInfo),
        !:Specs = [Spec | !.Specs]
    ).

:- pred check_ctor_constraints(module_info::in,
    pair(type_ctor, hlds_type_defn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_ctor_constraints(ModuleInfo, TypeCtor - TypeDefn, !Specs) :-
    get_type_defn_body(TypeDefn, Body),
    (
        Body = hlds_du_type(Ctors, _, _, _, _, _, _, _, _),
        list.foldl(check_ctor_type_ambiguities(ModuleInfo, TypeCtor, TypeDefn),
            Ctors, !Specs)
    ;
        ( Body = hlds_eqv_type(_)
        ; Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_)
        ; Body = hlds_abstract_type(_)
        )
    ).

:- pred check_ctor_type_ambiguities(module_info::in, type_ctor::in,
    hlds_type_defn::in, constructor::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_ctor_type_ambiguities(ModuleInfo, TypeCtor, TypeDefn, Ctor, !Specs) :-
    Ctor = ctor(ExistQVars, Constraints, _, CtorArgs, _, _),
    ArgTypes = list.map(func(ctor_arg(_, T, _, _)) = T, CtorArgs),
    type_vars_list(ArgTypes, ArgTVars),
    list.filter((pred(V::in) is semidet :- list.member(V, ExistQVars)),
        ArgTVars, ExistQArgTVars),
    constraint_list_get_tvars(Constraints, ConstrainedTVars),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    get_unbound_tvars(ModuleInfo, TVarSet, ExistQArgTVars, ConstrainedTVars,
        Constraints, UnboundTVars),
    (
        UnboundTVars = []
    ;
        UnboundTVars = [_ | _],
        Spec = report_unbound_tvars_in_ctor_context(UnboundTVars, TypeCtor,
            TypeDefn),
        !:Specs = [Spec | !.Specs]
    ).

    % The error messages for ambiguous types are intended to look like this:
    %
    % long_module_name:001: In declaration for function `long_function/2':
    % long_module_name:001:   error in type class constraints: type variables
    % long_module_name:001:   T1, T2 and T3 occur in the constraints, but are
    % long_module_name:001:   not determined by the function's argument or
    % long_module_name:001:   result types.
    %
    % long_module_name:002: In declaration for predicate `long_predicate/3':
    % long_module_name:002:   error in type class constraints: type variable
    % long_module_name:002:   T occurs in the constraints, but is not
    % long_module_name:002:   determined by the predicate's argument types.
    %
    % long_module_name:002: In declaration for type `long_type/3':
    % long_module_name:002:   error in type class constraints: type variable
    % long_module_name:002:   T occurs in the constraints, but is not
    % long_module_name:002:   determined by the constructor's argument types.
    %
:- func report_unbound_tvars_in_pred_context(list(tvar), pred_info)
    = error_spec.

report_unbound_tvars_in_pred_context(Vars, PredInfo) = Spec :-
    pred_info_get_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, TVarSet, _, ArgTypes),
    PredName = pred_info_name(PredInfo),
    Module = pred_info_module(PredInfo),
    SymName = qualified(Module, PredName),
    Arity = length(ArgTypes),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),

    VarsStrs = list.map(mercury_var_to_name_only(TVarSet), Vars),

    Pieces0 = [words("In declaration for"),
        simple_call(simple_call_id(PredOrFunc, SymName, Arity)),
        suffix(":"), nl,
        words("error in type class constraints:"),
        words(choose_number(Vars, "type variable", "type variables"))]
        ++ list_to_quoted_pieces(VarsStrs) ++
        [words(choose_number(Vars, "occurs", "occur")),
        words("in the constraints, but"),
        words(choose_number(Vars, "is", "are")),
        words("not determined by the")],
    (
        PredOrFunc = pf_predicate,
        Pieces = Pieces0 ++ [words("predicate's argument types."), nl]
    ;
        PredOrFunc = pf_function,
        Pieces = Pieces0 ++ [words("function's argument or result types."), nl]
    ),
    Msg = simple_msg(Context,
        [always(Pieces),
        verbose_only(verbose_once, report_unbound_tvars_explanation)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- func report_unbound_tvars_in_ctor_context(list(tvar), type_ctor,
    hlds_type_defn) = error_spec.

report_unbound_tvars_in_ctor_context(Vars, TypeCtor, TypeDefn) = Spec :-
    get_type_defn_context(TypeDefn, Context),
    get_type_defn_tvarset(TypeDefn, TVarSet),
    TypeCtor = type_ctor(SymName, Arity),

    VarsStrs = list.map(mercury_var_to_name_only(TVarSet), Vars),

    Pieces = [words("In declaration for type"),
        qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
        suffix(":"), nl,
        words("error in type class constraints:"),
        words(choose_number(Vars, "type variable", "type variables"))]
        ++ list_to_quoted_pieces(VarsStrs) ++
        [words(choose_number(Vars, "occurs", "occur")),
        words("in the constraints, but"),
        words(choose_number(Vars, "is", "are")),
        words("not determined by the constructor's argument types."), nl],
    Msg = simple_msg(Context,
        [always(Pieces),
        verbose_only(verbose_once, report_unbound_tvars_explanation)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

:- func report_unbound_tvars_explanation = list(format_component).

report_unbound_tvars_explanation =
    [words("All types occurring in typeclass constraints"),
    words("must be fully determined."),
    words("A type is fully determined if one of the"),
    words("following holds:"),
    nl,
    words("1) All type variables occurring in the type"),
    words("are determined."),
    nl,
    words("2) The type occurs in a constraint argument,"),
    words("that argument is in the range of some"),
    words("functional dependency for that class, and"),
    words("the types in all of the domain arguments for"),
    words("that functional dependency are fully"),
    words("determined."),
    nl,
    words("A type variable is determined if one of the"),
    words("following holds:"),
    nl,
    words("1) The type variable occurs in the argument"),
    words("types of the predicate, function, or"),
    words("constructor which is constrained."),
    nl,
    words("2) The type variable occurs in a type which"),
    words("is fully determined."),
    nl,
    words("See the ""Functional dependencies"" section"),
    words("of the reference manual for details."), nl].

%---------------------------------------------------------------------------%

    % Check that all types appearing in universal (existential) constraints are
    % universally (existentially) quantified.
    %
:- pred check_constraint_quant(pred_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_constraint_quant(PredInfo, !Specs) :-
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    pred_info_get_class_context(PredInfo, Constraints),
    Constraints = constraints(UnivCs, ExistCs),
    prog_type.constraint_list_get_tvars(UnivCs, UnivTVars),
    solutions.solutions(
        (pred(V::out) is nondet :-
            list.member(V, UnivTVars),
            list.member(V, ExistQVars)
        ), BadUnivTVars),
    maybe_report_badly_quantified_vars(PredInfo, universal_constraint,
        BadUnivTVars, !Specs),
    prog_type.constraint_list_get_tvars(ExistCs, ExistTVars),
    list.delete_elems(ExistTVars, ExistQVars, BadExistTVars),
    maybe_report_badly_quantified_vars(PredInfo, existential_constraint,
        BadExistTVars, !Specs).

:- pred maybe_report_badly_quantified_vars(pred_info::in, quant_error_type::in,
    list(tvar)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_report_badly_quantified_vars(PredInfo, QuantErrorType, TVars, !Specs) :-
    (
        TVars = []
    ;
        TVars = [_ | _],
        Spec = report_badly_quantified_vars(PredInfo, QuantErrorType, TVars),
        !:Specs = [Spec | !.Specs]
    ).

:- type quant_error_type
    --->    universal_constraint
    ;       existential_constraint.

:- func report_badly_quantified_vars(pred_info, quant_error_type, list(tvar))
    = error_spec.

report_badly_quantified_vars(PredInfo, QuantErrorType, TVars) = Spec :-
    pred_info_get_typevarset(PredInfo, TVarSet),
    pred_info_get_context(PredInfo, Context),

    InDeclaration = [words("In declaration of")] ++
        describe_one_pred_info_name(should_module_qualify, PredInfo) ++
        [suffix(":")],
    TypeVariables = [words("type variable"),
        suffix(choose_number(TVars, "", "s"))],
    TVarsStrs = list.map(mercury_var_to_name_only(TVarSet), TVars),
    TVarsPart = list_to_quoted_pieces(TVarsStrs),
    Are = words(choose_number(TVars, "is", "are")),
    (
        QuantErrorType = universal_constraint,
        BlahConstrained = words("universally constrained"),
        BlahQuantified = words("existentially quantified")
    ;
        QuantErrorType = existential_constraint,
        BlahConstrained = words("existentially constrained"),
        BlahQuantified = words("universally quantified")
    ),
    Pieces = InDeclaration ++ TypeVariables ++ TVarsPart ++
        [Are, BlahConstrained, suffix(","), words("but"), Are,
        BlahQuantified, suffix("."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred get_unbound_tvars(module_info::in, tvarset::in, list(tvar)::in,
    list(tvar)::in, list(prog_constraint)::in, list(tvar)::out) is det.

get_unbound_tvars(ModuleInfo, TVarSet, RootTVars, AllTVars, Constraints,
        UnboundTVars) :-
    module_info_get_class_table(ModuleInfo, ClassTable),
    InducedFunDeps = induced_fundeps(ClassTable, TVarSet, Constraints),
    FunDepsClosure = fundeps_closure(InducedFunDeps, list_to_set(RootTVars)),
    UnboundTVarsSet = set.difference(list_to_set(AllTVars), FunDepsClosure),
    UnboundTVars = set.to_sorted_list(UnboundTVarsSet).

:- type induced_fundeps == list(induced_fundep).
:- type induced_fundep
    --->    fundep(
                domain      :: set(tvar),
                range       :: set(tvar)
            ).

:- func induced_fundeps(class_table, tvarset, list(prog_constraint))
    = induced_fundeps.

induced_fundeps(ClassTable, TVarSet, Constraints) = FunDeps :-
    list.foldl(induced_fundeps_2(ClassTable, TVarSet), Constraints,
        [], FunDeps).

:- pred induced_fundeps_2(class_table::in, tvarset::in, prog_constraint::in,
    induced_fundeps::in, induced_fundeps::out) is det.

induced_fundeps_2(ClassTable, TVarSet, Constraint, !FunDeps) :-
    Constraint = constraint(Name, Args),
    Arity = length(Args),
    ClassDefn = map.lookup(ClassTable, class_id(Name, Arity)),
    % The ancestors includes all superclasses of Constraint which have
    % functional dependencies on them (possibly including Constraint itself).
    ClassAncestors = ClassDefn ^ classdefn_fundep_ancestors,
    (
        % Optimize the common case.
        ClassAncestors = []
    ;
        ClassAncestors = [_ | _],
        ClassTVarSet = ClassDefn ^ classdefn_tvarset,
        ClassParams = ClassDefn ^ classdefn_vars,

        % We can ignore the resulting tvarset, since any new variables
        % will become bound when the arguments are bound. (This follows
        % from the fact that constraints on class declarations can only use
        % variables that appear in the head of the declaration.)

        tvarset_merge_renaming(TVarSet, ClassTVarSet, _, Renaming),
        apply_variable_renaming_to_prog_constraint_list(Renaming,
            ClassAncestors, RenamedAncestors),
        apply_variable_renaming_to_tvar_list(Renaming, ClassParams,
            RenamedParams),
        map.from_corresponding_lists(RenamedParams, Args, Subst),
        apply_subst_to_prog_constraint_list(Subst, RenamedAncestors,
            Ancestors),
        list.foldl(induced_fundeps_3(ClassTable), Ancestors, !FunDeps)
    ).

:- pred induced_fundeps_3(class_table::in, prog_constraint::in,
    induced_fundeps::in, induced_fundeps::out) is det.

induced_fundeps_3(ClassTable, Constraint, !FunDeps) :-
    Constraint = constraint(Name, Args),
    Arity = length(Args),
    ClassDefn = map.lookup(ClassTable, class_id(Name, Arity)),
    list.foldl(induced_fundep(Args), ClassDefn ^ classdefn_fundeps, !FunDeps).

:- pred induced_fundep(list(mer_type)::in, hlds_class_fundep::in,
    induced_fundeps::in, induced_fundeps::out) is det.

induced_fundep(Args, fundep(Domain0, Range0), !FunDeps) :-
    Domain = set.fold(induced_vars(Args), Domain0, set.init),
    Range = set.fold(induced_vars(Args), Range0, set.init),
    !:FunDeps = [fundep(Domain, Range) | !.FunDeps].

:- func induced_vars(list(mer_type), int, set(tvar)) = set(tvar).

induced_vars(Args, ArgNum, Vars) = union(Vars, NewVars) :-
    Arg = list.det_index1(Args, ArgNum),
    type_vars(Arg, ArgVars),
    NewVars = set.list_to_set(ArgVars).

:- func fundeps_closure(induced_fundeps, set(tvar)) = set(tvar).

fundeps_closure(FunDeps, TVars) = fundeps_closure_2(FunDeps, TVars, set.init).

:- func fundeps_closure_2(induced_fundeps, set(tvar), set(tvar)) = set(tvar).

fundeps_closure_2(FunDeps0, NewVars0, Result0) = Result :-
    ( if set.is_empty(NewVars0) then
        Result = Result0
    else
        Result1 = set.union(Result0, NewVars0),
        FunDeps1 = list.map(remove_vars(NewVars0), FunDeps0),
        list.foldl2(collect_determined_vars, FunDeps1, [], FunDeps,
            set.init, NewVars),
        Result = fundeps_closure_2(FunDeps, NewVars, Result1)
    ).

:- func remove_vars(set(tvar), induced_fundep) = induced_fundep.

remove_vars(Vars, fundep(Domain0, Range0)) = fundep(Domain, Range) :-
    Domain = set.difference(Domain0, Vars),
    Range = set.difference(Range0, Vars).

:- pred collect_determined_vars(induced_fundep::in, induced_fundeps::in,
    induced_fundeps::out, set(tvar)::in, set(tvar)::out) is det.

collect_determined_vars(FunDep @ fundep(Domain, Range), !FunDeps, !Vars) :-
    ( if set.is_empty(Domain) then
        !:Vars = set.union(Range, !.Vars)
    else
        !:FunDeps = [FunDep | !.FunDeps]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Error reporting.
%

    % Duplicate method definition error.
    %
:- pred report_duplicate_method_defn(class_id::in, hlds_instance_defn::in,
    pred_or_func::in, sym_name::in, arity::in, list(instance_method)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_method_defn(ClassId, InstanceDefn, PredOrFunc, MethodName,
        Arity, MatchingInstanceMethods, !Specs) :-
    InstanceVarSet = InstanceDefn ^ instdefn_tvarset,
    InstanceTypes = InstanceDefn ^ instdefn_types,
    InstanceContext = InstanceDefn ^ instdefn_context,
    ClassId = class_id(ClassName, _ClassArity),
    ClassNameString = sym_name_to_string(ClassName),
    InstanceTypesString = mercury_type_list_to_string(InstanceVarSet,
        InstanceTypes),
    HeaderPieces =
        [words("In instance declaration for"),
        words_quote(ClassNameString ++ "(" ++ InstanceTypesString ++ ")"),
        suffix(":"), words("multiple implementations of type class"),
        p_or_f(PredOrFunc), words("method"),
        unqual_sym_name_and_arity(sym_name_arity(MethodName, Arity)),
        suffix("."), nl],
    HeadingMsg = simple_msg(InstanceContext, [always(HeaderPieces)]),
    (
        MatchingInstanceMethods = [FirstInstance | LaterInstances]
    ;
        MatchingInstanceMethods = [],
        unexpected($module, $pred, "no matching instances")
    ),
    FirstInstanceContext = FirstInstance ^ instance_method_decl_context,
    FirstPieces = [words("First definition appears here."), nl],
    FirstMsg = simple_msg(FirstInstanceContext, [always(FirstPieces)]),
    DefnToMsg =
        (pred(Definition::in, Msg::out) is det :-
            TheContext = Definition ^ instance_method_decl_context,
            SubsequentPieces =
                [words("Subsequent definition appears here."), nl],
            Msg = simple_msg(TheContext, [always(SubsequentPieces)])
        ),
    list.map(DefnToMsg, LaterInstances, LaterMsgs),

    Spec = error_spec(severity_error, phase_type_check,
        [HeadingMsg, FirstMsg | LaterMsgs]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred report_undefined_method(class_id::in, hlds_instance_defn::in,
    pred_or_func::in, sym_name::in, arity::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_undefined_method(ClassId, InstanceDefn, PredOrFunc, MethodName, Arity,
        !Specs) :-
    InstanceVarSet = InstanceDefn ^ instdefn_tvarset,
    InstanceTypes = InstanceDefn ^ instdefn_types,
    InstanceContext = InstanceDefn ^ instdefn_context,
    ClassId = class_id(ClassName, _ClassArity),
    ClassNameString = sym_name_to_string(ClassName),
    InstanceTypesString = mercury_type_list_to_string(InstanceVarSet,
        InstanceTypes),

    Pieces = [words("In instance declaration for"),
        words_quote(ClassNameString ++ "(" ++ InstanceTypesString ++ ")"),
        suffix(":"),
        words("no implementation for type class"), p_or_f(PredOrFunc),
        words("method"),
        unqual_sym_name_and_arity(sym_name_arity(MethodName, Arity)),
        suffix("."), nl],
    Msg = simple_msg(InstanceContext, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred report_unknown_instance_methods(class_id::in,
    instance_method::in, list(instance_method)::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_instance_methods(ClassId, HeadMethod, TailMethods, Context,
        !Specs) :-
    ClassId = class_id(ClassName, ClassArity),
    (
        TailMethods = [],
        HeadMethod = instance_method(HeadPredOrFunc, HeadMethodName, _Defn,
            HeadArity, _Context),
        adjust_func_arity(HeadPredOrFunc, HeadArity, HeadPredArity),
        Pieces = [words("In instance declaration for"),
            qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
            suffix(":"), nl,
            words("the type class has no"),
            p_or_f(HeadPredOrFunc), words("method named"),
            unqual_sym_name_and_arity(
                sym_name_arity(HeadMethodName, HeadPredArity)),
            suffix("."), nl]
    ;
        TailMethods = [_ | _],
        Pieces1 = [words("In instance declaration for"),
            qual_sym_name_and_arity(sym_name_arity(ClassName, ClassArity)),
            suffix(":"),
            words("the type class has none of these methods:"), nl],
        format_method_names(HeadMethod, TailMethods, Pieces2),
        Pieces = Pieces1 ++ Pieces2
    ),

    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_type_check, [Msg]),
    !:Specs = [Spec | !.Specs].

:- pred format_method_names(instance_method::in, list(instance_method)::in,
    list(format_component)::out) is det.

format_method_names(HeadMethod, TailMethods, Pieces) :-
    HeadMethod = instance_method(PredOrFunc, Name, _Defn, Arity, _Context),
    adjust_func_arity(PredOrFunc, Arity, PredArity),
    (
        TailMethods = [],
        Pieces = [p_or_f(PredOrFunc),
            unqual_sym_name_and_arity(sym_name_arity(Name, PredArity)),
            suffix("."), nl]
    ;
        TailMethods = [HeadTailMethod | TailTailMethods],
        format_method_names(HeadTailMethod, TailTailMethods, TailPieces),
        Pieces = [p_or_f(PredOrFunc),
            unqual_sym_name_and_arity(sym_name_arity(Name, PredArity)),
            suffix(","), words("or"), nl | TailPieces]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.check_typeclass.
%---------------------------------------------------------------------------%
