%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2011 The University of Melbourne.
% Copyright (C) 2016-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_class.m.
% Main authors: dgj.
%
% This module handles the parsing of typeclass declarations.
% Perhaps some of this should go into parse_util.m?
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_class.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Parse a typeclass declaration.
    %
:- pred parse_typeclass_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

    % Parse an instance declaration.
    %
:- pred parse_instance_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

    % Parse constraints on a pred or func declaration, or on an existentially
    % quantified type definition. Currently all such constraints must be
    % simple.
    %
:- pred parse_class_constraints(module_name::in, varset::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

    % Parse a list of class and inst constraints.
    %
:- pred parse_class_and_inst_constraints(module_name::in, varset::in, term::in,
    maybe_class_and_inst_constraints::out) is det.

:- type maybe_class_and_inst_constraints ==
    maybe2(list(prog_constraint), inst_var_sub).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_item.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_util.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module term_int.

%---------------------------------------------------------------------------%

parse_typeclass_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [ArgTerm] then
        ( if
            ArgTerm = term.functor(term.atom("where"),
                [NameTerm, MethodsTerm], _)
        then
            parse_non_empty_class(ModuleName, VarSet, NameTerm, MethodsTerm,
                Context, SeqNum, MaybeItemTypeClassInfo)
        else
            parse_class_head(ModuleName, VarSet, ArgTerm, Context, SeqNum,
                MaybeItemTypeClassInfo)
        ),
        (
            MaybeItemTypeClassInfo = ok1(ItemTypeClassInfo),
            MaybeIOM = ok1(iom_item(item_typeclass(ItemTypeClassInfo)))
        ;
            MaybeItemTypeClassInfo = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces =
            [words("Error: a"), decl("typeclass"), words("declaration")] ++
            color_as_incorrect([words("should have the form")]) ++
            [nl_indent_delta(1)] ++
            color_as_correct([quote(":- typeclass tcname(T1, ... Tn)")]) ++
            [nl_indent_delta(-1),
            words("optionally followed by"), nl_indent_delta(1)] ++
            color_as_correct(
                [quote("where [method_signature_1, ... method_signature_m]"),
                suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_non_empty_class(module_name::in, varset::in, term::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_typeclass_info)::out)
    is det.

parse_non_empty_class(ModuleName, VarSet, NameTerm, MethodsTerm,
        Context, SeqNum, MaybeItemTypeClassInfo) :-
    parse_class_head(ModuleName, VarSet, NameTerm, Context, SeqNum,
        MaybeItemTypeClassInfo0),
    parse_class_decls(ModuleName, VarSet, MethodsTerm, MaybeClassDecls),
    ( if
        MaybeItemTypeClassInfo0 = ok1(ItemTypeClassInfo0),
        MaybeClassDecls = ok1(ClassDecls)
    then
        varset.coerce(VarSet, TVarSet),
        ItemTypeClassInfo = ((ItemTypeClassInfo0
            ^ tc_class_methods := class_interface_concrete(ClassDecls))
            ^ tc_varset := TVarSet),
        MaybeItemTypeClassInfo = ok1(ItemTypeClassInfo)
    else
        Specs = get_any_errors1(MaybeItemTypeClassInfo0) ++
            get_any_errors1(MaybeClassDecls),
        MaybeItemTypeClassInfo = error1(Specs)
    ).

:- pred parse_class_head(module_name::in, varset::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_typeclass_info)::out)
    is det.

parse_class_head(ModuleName, VarSet, ArgTerm, Context, SeqNum,
        MaybeItemTypeClassInfo) :-
    ( if
        ArgTerm = term.functor(term.atom("<="), [NameTerm, ConstraintsTerm], _)
    then
        parse_constrained_class(ModuleName, VarSet, NameTerm, ConstraintsTerm,
            Context, SeqNum, MaybeItemTypeClassInfo)
    else
        varset.coerce(VarSet, TVarSet),
        parse_unconstrained_class(ModuleName, TVarSet, ArgTerm,
            Context, SeqNum, MaybeItemTypeClassInfo)
    ).

:- pred parse_constrained_class(module_name::in, varset::in,
    term::in, term::in, prog_context::in, item_seq_num::in,
    maybe1(item_typeclass_info)::out) is det.

parse_constrained_class(ModuleName, VarSet, NameTerm, ConstraintsTerm,
        Context, SeqNum, MaybeItemTypeClass) :-
    varset.coerce(VarSet, TVarSet),
    parse_superclass_constraints(ModuleName, VarSet, ConstraintsTerm,
        MaybeParsedConstraints),
    (
        MaybeParsedConstraints = ok2(ConstraintList, FunDeps),
        parse_unconstrained_class(ModuleName, TVarSet, NameTerm,
            Context, SeqNum, MaybeItemTypeClass0),
        (
            MaybeItemTypeClass0 = error1(_),
            MaybeItemTypeClass = MaybeItemTypeClass0
        ;
            MaybeItemTypeClass0 = ok1(ItemTypeClass0),
            % Check for type variables in the constraints which do not
            % occur in the type class parameters.

            constraint_list_get_tvars(ConstraintList, ConstraintVars),
            list.sort_and_remove_dups(ConstraintVars, SortedConstraintVars),
            FunDepVars = tvars_in_fundeps(FunDeps),
            list.sort_and_remove_dups(FunDepVars, SortedFunDepVars),

            Params = ItemTypeClass0 ^ tc_class_params,
            list.filter(list.contains(Params), SortedConstraintVars,
                _ConstraintInParams, ConstraintNotInParams),
            list.filter(list.contains(Params), SortedFunDepVars,
                _FunDepInParams, FunDepNotInParams),
            (
                ConstraintNotInParams = [_ | _],
                ( if list.length(ConstraintList) = 1 then
                    ConstraintErrorContext =
                        [words("in the superclass constraint")]
                else
                    ConstraintErrorContext =
                        [words("in superclass constraints")]
                )
            ;
                ConstraintNotInParams = [],
                ConstraintErrorContext = []
            ),
            (
                FunDepNotInParams = [_ | _],
                ( if list.length(FunDeps) = 1 then
                    FunDepErrorContext =
                        [words("in the functional dependency")]
                else
                    FunDepErrorContext =
                        [words("in functional dependencies")]
                )
            ;
                FunDepNotInParams = [],
                FunDepErrorContext = []
            ),
            NotInParams = ConstraintNotInParams ++ FunDepNotInParams,
            (
                NotInParams = [],
                ItemTypeClass = ((ItemTypeClass0
                    ^ tc_superclasses := ConstraintList)
                    ^ tc_fundeps := FunDeps),
                MaybeItemTypeClass = ok1(ItemTypeClass)
            ;
                NotInParams = [_ | _],
                ClassName = ItemTypeClass0 ^ tc_class_name,
                pred_form_arity(ClassArity) = arg_list_arity(Params),
                ClassId = class_id(ClassName, ClassArity),
                ClassTVarSet = ItemTypeClass0 ^ tc_varset,
                ConstraintNotInParamsPieces =
                    list.map(var_to_quote_piece(ClassTVarSet),
                        ConstraintNotInParams),
                FunDepNotInParamsPieces =
                    list.map(var_to_quote_piece(ClassTVarSet),
                        FunDepNotInParams),
                ConstraintPieces =
                    piece_list_to_color_pieces(color_subject, "and", [],
                        ConstraintNotInParamsPieces),
                FunDepPieces =
                    piece_list_to_color_pieces(color_subject, "and", [],
                        FunDepNotInParamsPieces),
                ( if list.length(NotInParams) = 1 then
                    ErrorTypeVarsPieces = [words("Error: type variable")],
                    NotParameterPieces = [words("is")] ++
                        color_as_incorrect([words("not a parameter")])
                else
                    ErrorTypeVarsPieces = [words("Error: type variables")],
                    NotParameterPieces = [words("are")] ++
                        color_as_incorrect([words("not parameters")])
                ),
                (
                    ConstraintNotInParams = [],
                    FunDepNotInParams = [],
                    unexpected($pred, "no NotInParams")
                ;
                    ConstraintNotInParams = [],
                    FunDepNotInParams = [_ | _],
                    ConstrFunDepPieces = FunDepPieces ++ FunDepErrorContext
                ;
                    ConstraintNotInParams = [_ | _],
                    FunDepNotInParams = [],
                    ConstrFunDepPieces =
                        ConstraintPieces ++ ConstraintErrorContext
                ;
                    ConstraintNotInParams = [_ | _],
                    FunDepNotInParams = [_ | _],
                    ConstrFunDepPieces =
                        ConstraintPieces ++ ConstraintErrorContext
                        ++ [words("and")] ++
                        FunDepPieces ++ FunDepErrorContext
                ),
                Pieces = ErrorTypeVarsPieces ++ ConstrFunDepPieces ++
                    NotParameterPieces ++
                    [words("of type class")] ++
                    color_as_subject([unqual_class_id(ClassId),
                        suffix(".")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_t2pt,
                    Context, Pieces),
                MaybeItemTypeClass = error1([Spec])
            )
        )
    ;
        MaybeParsedConstraints = error2(Specs),
        MaybeItemTypeClass = error1(Specs)
    ).

:- func tvars_in_fundeps(list(prog_fundep)) = list(tvar).

tvars_in_fundeps(FunDeps) = list.condense(list.map(tvars_in_fundep, FunDeps)).

:- func tvars_in_fundep(prog_fundep) = list(tvar).

tvars_in_fundep(prog_fundep(Domain, Range)) =
    one_or_more_to_list(Domain) ++ one_or_more_to_list(Range).

:- pred parse_superclass_constraints(module_name::in, varset::in, term::in,
    maybe2(list(prog_constraint), list(prog_fundep))::out) is det.

parse_superclass_constraints(_ModuleName, VarSet, ConstraintsTerm, Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(one_or_more(HeadArbConstraint, TailArbConstraints)),
        ArbitraryConstraints = [HeadArbConstraint | TailArbConstraints],
        collect_superclass_constraints(VarSet, ArbitraryConstraints,
            SimpleConstraints, FunDeps, BadConstraintSpecs),
        (
            BadConstraintSpecs = [],
            Result = ok2(SimpleConstraints, FunDeps)
        ;
            BadConstraintSpecs = [_ | _],
            Result = error2(BadConstraintSpecs)
        )
    ;
        Result0 = error1(Specs),
        Result = error2(Specs)
    ).

:- pred collect_superclass_constraints(varset::in,
    list(arbitrary_constraint)::in,
    list(prog_constraint)::out, list(prog_fundep)::out,
    list(error_spec)::out) is det.

collect_superclass_constraints(_, [], [], [], []).
collect_superclass_constraints(VarSet, [Constraint | Constraints],
        !:SimpleConstraints, !:FunDeps, !:Specs) :-
    collect_superclass_constraints(VarSet, Constraints,
        !:SimpleConstraints, !:FunDeps, !:Specs),
    (
        Constraint = ac_type_constraint(TypeConstraint,
            _VoGTypes, NonVarNonGroundTypes, Context),
        (
            NonVarNonGroundTypes = [],
            !:SimpleConstraints = [TypeConstraint | !.SimpleConstraints]
        ;
            NonVarNonGroundTypes = [_ | _],
            varset.coerce(VarSet, TVarSet),
            TypeConstraint = constraint(SuperClassName, _),
            BadTypeStrs = list.map(
                mercury_type_to_string(TVarSet, print_name_only),
                NonVarNonGroundTypes),
            BadTypesPieces = quote_list_to_color_pieces(color_subject, "and",
                [], BadTypeStrs),
            (
                NonVarNonGroundTypes = [_],
                BadTypeMsgPieces = [words("The type")] ++ BadTypesPieces ++
                    color_as_incorrect([words("is neither.")])
            ;
                NonVarNonGroundTypes = [_, _ | _],
                BadTypeMsgPieces = [words("The types")] ++ BadTypesPieces ++
                    color_as_incorrect([words("are neither.")])
            ),
            Pieces = [words("Error: in a superclass constraint,"),
                words("all the argument types of the superclass,"),
                words("which in this case is")] ++
                color_as_subject([unqual_sym_name(SuperClassName),
                    suffix(",")]) ++
                [words("must be either type variables or ground types.")] ++
                BadTypeMsgPieces,
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ;
        Constraint = ac_inst_constraint(InstVar, Inst, Context),
        varset.coerce(VarSet, InstVarSet),
        InstConstraintStr = mercury_constrained_inst_vars_to_string(
            output_mercury, InstVarSet, set.make_singleton_set(InstVar), Inst),
        Pieces = [words("Error: a class declaration")] ++
            color_as_incorrect([words("may not contain")]) ++
            [words("an inst constraint such as")] ++
            color_as_subject([quote(InstConstraintStr), suffix(".")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        Constraint = ac_fundep(FunDep, _),
        !:FunDeps = [FunDep | !.FunDeps]
    ).

:- pred parse_unconstrained_class(module_name::in, tvarset::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_typeclass_info)::out)
    is det.

parse_unconstrained_class(ModuleName, TVarSet, NameTerm, Context, SeqNum,
        MaybeTypeClassInfo) :-
    ContextPieces = cord.singleton(words("In typeclass declaration:")),
    varset.coerce(TVarSet, VarSet),
    ( if is_the_name_a_variable(VarSet, vtk_class_decl, NameTerm, Spec) then
        MaybeTypeClassInfo = error1([Spec])
    else
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, NameTerm, MaybeClassName),
        (
            MaybeClassName = ok2(ClassName, ArgTerms0),
            list.map(term.coerce, ArgTerms0, ArgTerms),
            (
                ArgTerms = [],
                Pieces =
                    [words("Error: typeclass declarations")] ++
                    color_as_incorrect([words("require at least one"),
                        words("class parameter.")]) ++
                    [nl],
                Spec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(NameTerm), Pieces),
                MaybeTypeClassInfo = error1([Spec])
            ;
                ArgTerms = [_ | _],
                terms_to_distinct_vars(TVarSet, "a", "typeclass declaration",
                    ArgTerms, MaybeVars),
                (
                    MaybeVars = ok1(Vars),
                    % XXX Would this be a better context?
                    % Context = get_term_context(NameTerm),
                    TypeClassInfo = item_typeclass_info(ClassName, Vars,
                        [], [], class_interface_abstract, TVarSet,
                        Context, SeqNum),
                    MaybeTypeClassInfo = ok1(TypeClassInfo)
                ;
                    MaybeVars = error1(Specs),
                    MaybeTypeClassInfo = error1(Specs)
                )
            )
        ;
            MaybeClassName = error2(Specs),
            MaybeTypeClassInfo = error1(Specs)
        )
    ).

:- pred parse_class_decls(module_name::in, varset::in, term::in,
    maybe1(list(class_decl))::out) is det.

parse_class_decls(ModuleName, VarSet, DeclsTerm, MaybeClassDecls) :-
    ( if list_term_to_term_list(DeclsTerm, DeclTerms) then
        list.map(parse_class_decl(ModuleName, VarSet), DeclTerms, MaybeDecls),
        find_errors(MaybeDecls, MaybeClassDecls)
    else
        DeclsTermStr = describe_error_term(VarSet, DeclsTerm),
        Pieces = [words("Error: expected a")] ++
            color_as_correct([words("list of class methods,")]) ++
            [words("got")] ++
            color_as_incorrect([quote(DeclsTermStr), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(DeclsTerm), Pieces),
        MaybeClassDecls = error1([Spec])
    ).

    % From a list of maybe1s, search them for errors.
    % If some errors are found, return error1(their union).
    % If no error is found, return ok1(the original elements).
    %
:- pred find_errors(list(maybe1(T))::in, maybe1(list(T))::out) is det.

find_errors(Xs, Result) :-
    find_errors_loop(Xs, [], Results, [], Specs),
    (
        Specs = [],
        Result = ok1(Results)
    ;
        Specs = [_ | _],
        Result = error1(Specs)
    ).

:- pred find_errors_loop(list(maybe1(T))::in, list(T)::in, list(T)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

find_errors_loop([], !Results, !Specs).
find_errors_loop([X | Xs], !Results, !Specs) :-
    find_errors_loop(Xs, !Results, !Specs),
    (
        X = ok1(CurResult),
        !:Results = [CurResult | !.Results]
    ;
        X = error1(CurSpecs),
        !:Specs = CurSpecs ++ !.Specs
    ).

%---------------------------------------------------------------------------%

parse_instance_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [ArgTerm] then
        varset.coerce(VarSet, TVarSet),
        ( if
            ArgTerm = term.functor(term.atom("where"),
                [NameTerm, MethodsTerm], _)
        then
            parse_non_empty_instance(ModuleName, VarSet, TVarSet,
                NameTerm, MethodsTerm, Context, SeqNum, MaybeItemInstanceInfo)
        else
            parse_instance_name(ModuleName, TVarSet, ArgTerm,
                Context, SeqNum, MaybeItemInstanceInfo)
        ),
        (
            MaybeItemInstanceInfo = ok1(ItemInstanceInfo),
            MaybeIOM = ok1(iom_item(item_instance(ItemInstanceInfo)))
        ;
            MaybeItemInstanceInfo = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces =
            [words("Error: an"), decl("instance"), words("declaration")] ++
            color_as_incorrect([words("should have the form")]) ++
            [nl_indent_delta(1)] ++
            color_as_correct(
                [quote(":- instance tcname(type1, ... typen)")]) ++
            [nl_indent_delta(-1),
            words("optionally followed by"),
            nl_indent_delta(1)] ++
            color_as_correct(
                [quote("where [method_spec_1, ... method_spec_m]"),
                suffix(".")]) ++
            [nl_indent_delta(-1)],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_instance_name(module_name::in, tvarset::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_instance_info)::out)
    is det.

parse_instance_name(ModuleName, TVarSet, ArgTerm, Context, SeqNum,
        MaybeItemInstanceInfo) :-
    ( if
        ArgTerm = term.functor(term.atom("<="), [NameTerm, ConstraintsTerm], _)
    then
        parse_derived_instance(ModuleName, TVarSet, NameTerm, ConstraintsTerm,
            Context, SeqNum, MaybeItemInstanceInfo)
    else
        parse_underived_instance(ModuleName, TVarSet, ArgTerm,
            Context, SeqNum, MaybeItemInstanceInfo)
    ).

:- pred parse_derived_instance(module_name::in, tvarset::in,
    term::in, term::in, prog_context::in, item_seq_num::in,
    maybe1(item_instance_info)::out) is det.

parse_derived_instance(ModuleName, TVarSet, NameTerm, ConstraintsTerm,
        Context, SeqNum, MaybeItemInstanceInfo) :-
    varset.coerce(TVarSet, VarSet),
    parse_underived_instance(ModuleName, TVarSet, NameTerm,
        Context, SeqNum, MaybeItemInstanceInfo0),
    parse_instance_constraints(ModuleName, VarSet, ConstraintsTerm,
        MaybeInstanceConstraints),
    ( if
        MaybeItemInstanceInfo0 = ok1(ItemInstanceInfo0),
        MaybeInstanceConstraints = ok1(InstanceConstraints)
    then
        ItemInstanceInfo = ItemInstanceInfo0 ^ ci_deriving_class
            := InstanceConstraints,
        MaybeItemInstanceInfo = ok1(ItemInstanceInfo)
    else
        Specs = get_any_errors1(MaybeItemInstanceInfo0) ++
            get_any_errors1(MaybeInstanceConstraints),
        MaybeItemInstanceInfo = error1(Specs)
    ).

:- pred parse_instance_constraints(module_name::in, varset::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

parse_instance_constraints(ModuleName, VarSet, ConstraintsTerm, Result) :-
    NonSimplePieces = [words("Error: constraints on instance declarations")],
    parse_simple_class_constraints(ModuleName, VarSet, ConstraintsTerm,
        NonSimplePieces, Result).

:- pred parse_underived_instance(module_name::in, tvarset::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_instance_info)::out)
    is det.

parse_underived_instance(ModuleName, TVarSet, NameTerm, Context, SeqNum,
        MaybeItemInstanceInfo) :-
    % We don't give a default module name here since the instance declaration
    % could well be for a typeclass defined in another module.
    NameContextPieces = cord.singleton(words("In instance declaration:")),
    varset.coerce(TVarSet, VarSet),
    ( if is_the_name_a_variable(VarSet, vtk_instance_decl, NameTerm, Spec) then
        MaybeItemInstanceInfo = error1([Spec])
    else
        parse_sym_name_and_args(VarSet, NameContextPieces,
            NameTerm, MaybeClassName),
        (
            MaybeClassName = ok2(ClassName, TypeTerms),
            TypesContextPieces = NameContextPieces,
            parse_types(no_allow_ho_inst_info(wnhii_class_constraint),
                VarSet, TypesContextPieces, TypeTerms, MaybeTypes),
            (
                MaybeTypes = ok1(Types),
                ItemInstanceInfo = item_instance_info(ClassName, Types, Types,
                    [], instance_body_abstract, TVarSet, ModuleName,
                    Context, SeqNum),
                MaybeItemInstanceInfo = ok1(ItemInstanceInfo)
            ;
                MaybeTypes = error1(Specs),
                MaybeItemInstanceInfo = error1(Specs)
            )
        ;
            MaybeClassName = error2(Specs),
            MaybeItemInstanceInfo = error1(Specs)
        )
    ).

:- pred parse_non_empty_instance(module_name::in, varset::in, tvarset::in,
    term::in, term::in, prog_context::in, item_seq_num::in,
    maybe1(item_instance_info)::out) is det.

parse_non_empty_instance(ModuleName, VarSet, TVarSet, NameTerm, MethodsTerm,
        Context, SeqNum, MaybeItemInstanceInfo) :-
    parse_instance_name(ModuleName, TVarSet, NameTerm, Context, SeqNum,
        MaybeItemInstanceInfo0),
    parse_instance_methods(ModuleName, VarSet, MethodsTerm,
        MaybeInstanceMethods),
    ( if
        MaybeItemInstanceInfo0 = ok1(ItemInstanceInfo0),
        MaybeInstanceMethods = ok1(InstanceMethods)
    then
        ItemInstanceInfo = ((ItemInstanceInfo0
            ^ ci_method_instances := instance_body_concrete(InstanceMethods))
            ^ ci_varset := TVarSet),
        check_tvars_in_instance_constraint(ItemInstanceInfo, NameTerm,
            MaybeCheckSpec),
        (
            MaybeCheckSpec = yes(Spec),
            MaybeItemInstanceInfo = error1([Spec])
        ;
            MaybeCheckSpec = no,
            MaybeItemInstanceInfo = ok1(ItemInstanceInfo)
        )
    else
        Specs = get_any_errors1(MaybeItemInstanceInfo0) ++
            get_any_errors1(MaybeInstanceMethods),
        MaybeItemInstanceInfo = error1(Specs)
    ).

:- pred check_tvars_in_instance_constraint(item_instance_info::in,
    term::in, maybe(error_spec)::out) is det.

check_tvars_in_instance_constraint(ItemInstanceInfo, NameTerm, MaybeSpec) :-
    ItemInstanceInfo = item_instance_info(_Name, Types, _OriginalTypes,
        Constraints, _Methods, TVarSet, _ModName, _Context, _SeqNum),
    % Check that all of the type variables in the constraints on the instance
    % declaration also occur in the type class argument types in the instance
    % declaration.
    ( if
        constraint_list_get_tvars(Constraints, TVars),
        set_of_type_vars_in_types(Types, TypesVars),
        list.filter(set.contains(TypesVars), TVars, _BoundTVars, UnboundTVars),
        UnboundTVars = [_ | _]
    then
        UnboundTVarPieces =
            list.map(var_to_quote_piece(TVarSet), UnboundTVars),
        UnboundTVarsPieces = piece_list_to_color_pieces(color_subject, "and",
            [], UnboundTVarPieces),
        ( if list.length(UnboundTVars) = 1 then
            UnboundPieces = [words("unbound type variable")]
        else
            UnboundPieces = [words("unbound type variables")]
        ),
        Pieces = [words("Error:")] ++ color_as_incorrect(UnboundPieces) ++
            UnboundTVarsPieces ++
            [words("in constraints on instance declaration."), nl],
        % XXX Would _Context be better than get_term_context(NameTerm)?
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(NameTerm), Pieces),
        MaybeSpec = yes(Spec)
    else
        MaybeSpec = no
    ).

:- pred parse_instance_methods(module_name::in, varset::in, term::in,
    maybe1(list(instance_method))::out) is det.

parse_instance_methods(ModuleName, VarSet, MethodsTerm, Result) :-
    ( if list_term_to_term_list(MethodsTerm, MethodList) then
        list.map(term_to_instance_method(ModuleName, VarSet),
            MethodList, Interface),
        find_errors(Interface, Result)
    else
        MethodsTermStr = describe_error_term(VarSet, MethodsTerm),
        Pieces = [words("Error: expected a")] ++
            color_as_correct([words("list of instance methods.")]) ++
            [words("got")] ++
            color_as_incorrect([quote(MethodsTermStr), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(MethodsTerm), Pieces),
        Result = error1([Spec])
    ).

    % Turn the term into a method instance.
    %
:- pred term_to_instance_method(module_name::in, varset::in, term::in,
    maybe1(instance_method)::out) is det.

term_to_instance_method(_ModuleName, VarSet, MethodTerm,
        MaybeInstanceMethod) :-
    ( if
        MethodTerm = term.functor(term.atom("is"),
            [ClassMethodTerm, InstanceMethodTerm], TermContext)
    then
        % Note that the codes for 'pred(...)' and 'func(...)' are very similar.
        % Unfortunately, factoring out the common code would not really
        % simplify things.
        ( if
            ClassMethodTerm = term.functor(term.atom("pred"), [SlashTerm], _),
            SlashTerm = term.functor(term.atom("/"),
                [PredNameTerm, ArityTerm], _)
        then
            ( if
                try_parse_sym_name_and_no_args(PredNameTerm, PredSymName),
                term_int.decimal_term_to_int(ArityTerm, ArityInt),
                try_parse_sym_name_and_no_args(InstanceMethodTerm,
                    InstanceMethodName)
            then
                ProcDef = instance_proc_def_name(InstanceMethodName),
                MethodName = pred_pf_name_arity(pf_predicate, PredSymName,
                    user_arity(ArityInt)),
                InstanceMethod = instance_method(MethodName, ProcDef,
                    TermContext),
                MaybeInstanceMethod = ok1(InstanceMethod)
            else
                MethodTermStr = describe_error_term(VarSet, MethodTerm),
                Pieces = [words("Error: expected"),
                    nl_indent_delta(1)] ++
                    color_as_correct(
                        [quote("pred(<Name> / <Arity>) is <InstanceMethod>"),
                        suffix(",")]) ++
                    [nl_indent_delta(-1),
                    words("got"),
                    nl_indent_delta(1)] ++
                    color_as_incorrect([words(MethodTermStr), suffix(".")]) ++
                    [nl_indent_delta(-1)],
                Spec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(MethodTerm), Pieces),
                MaybeInstanceMethod = error1([Spec])
            )
        else if
            ClassMethodTerm = term.functor(term.atom("func"), [SlashTerm], _),
            SlashTerm = term.functor(term.atom("/"),
                [FuncNameTerm, ArityTerm], _)
        then
            ( if
                try_parse_sym_name_and_no_args(FuncNameTerm, FuncSymName),
                term_int.decimal_term_to_int(ArityTerm, ArityInt),
                try_parse_sym_name_and_no_args(InstanceMethodTerm,
                    InstanceMethodName)
            then
                ProcDef = instance_proc_def_name(InstanceMethodName),
                MethodName = pred_pf_name_arity(pf_function, FuncSymName,
                    user_arity(ArityInt)),
                InstanceMethod = instance_method(MethodName, ProcDef,
                    TermContext),
                MaybeInstanceMethod = ok1(InstanceMethod)
            else
                MethodTermStr = describe_error_term(VarSet, MethodTerm),
                Pieces = [words("Error: expected"),
                    nl_indent_delta(1)] ++
                    color_as_correct(
                        [quote("func(<Name> / <Arity>) is <InstanceMethod>"),
                        suffix(",")]) ++
                    [nl_indent_delta(-1),
                    words("got"),
                    nl_indent_delta(1)] ++
                    color_as_incorrect([words(MethodTermStr), suffix(".")]) ++
                    [nl_indent_delta(-1)],
                Spec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(MethodTerm), Pieces),
                MaybeInstanceMethod = error1([Spec])
            )
        else
            MethodTermStr = describe_error_term(VarSet, MethodTerm),
            Pieces = [words("Error: expected"),
                nl_indent_delta(1)] ++
                color_as_correct(
                    [quote("pred(<Name> / <Arity>) is <InstanceName>")]) ++
                [nl_indent_delta(-1),
                words("or"),
                nl_indent_delta(1)] ++
                color_as_correct(
                    [quote("func(<Name> / <Arity>) is <InstanceName>"),
                    suffix(",")]) ++
                [nl_indent_delta(-1),
                words("got"),
                nl_indent_delta(1)] ++
                color_as_incorrect([words(MethodTermStr), suffix(".")]) ++
                [nl_indent_delta(-1)],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(MethodTerm), Pieces),
            MaybeInstanceMethod = error1([Spec])
        )
    else
        ( if MethodTerm = term.functor(term.atom(":-"), [_], _) then
            Spec = report_unexpected_method_term(VarSet, MethodTerm),
            MaybeInstanceMethod = error1([Spec])
        else
            % For the clauses in an instance declaration, the default
            % module name for the clause heads is the module name of the class
            % that this is an instance declaration for, but we don't
            % necessarily know what module that is at this point,
            % since the class name hasn't been fully qualified yet.
            % So here we pass "no" in the first argument to indicate
            % the absence of a default module name. (If the module qualifiers
            % in the clauses don't match the module name of the class,
            % we will pick that up later, in check_typeclass.m.)

            parse_clause_term(no, VarSet, MethodTerm, item_no_seq_num,
                MaybeClause),
            (
                MaybeClause = ok1(ItemClause),
                ItemClause = item_clause_info(PredOrFunc, MethodSymName,
                    ArgTerms, _VarSet, _ClauseBody, Context, _SeqNum),
                PredFormArity = arg_list_arity(ArgTerms),
                user_arity_pred_form_arity(PredOrFunc,
                    UserArity, PredFormArity),
                ClauseCord = cord.singleton(ItemClause),
                ProcDef = instance_proc_def_clauses(ClauseCord),
                MethodName = pred_pf_name_arity(PredOrFunc, MethodSymName,
                    UserArity),
                InstanceMethod = instance_method(MethodName, ProcDef, Context),
                MaybeInstanceMethod = ok1(InstanceMethod)
            ;
                MaybeClause = error1(Specs),
                MaybeInstanceMethod = error1(Specs)
            )
        )
    ).

:- func report_unexpected_method_term(varset, term) = error_spec.

report_unexpected_method_term(VarSet, MethodTerm) = Spec :-
    MethodTermStr = describe_error_term(VarSet, MethodTerm),
    Pieces = [words("Error: expected clause or"), nl_indent_delta(1)] ++
        color_as_correct(
            [quote("pred(<Name> / <Arity>) is <InstanceName>")]) ++
        [nl_indent_delta(-1),
        words("or"),
        nl_indent_delta(1)] ++
        color_as_correct(
            [quote("func(<Name> / <Arity>) is <InstanceName>"),
            suffix(",")]) ++
        [words("got")] ++
        color_as_incorrect([words(MethodTermStr), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt,
        get_term_context(MethodTerm), Pieces).

%---------------------------------------------------------------------------%
%
% Predicates for parsing various kinds of constraints.
%

parse_class_constraints(ModuleName, VarSet, ConstraintsTerm, Result) :-
    NonSimplePieces = [words("Sorry, not implemented: constraints")],
    parse_simple_class_constraints(ModuleName, VarSet, ConstraintsTerm,
        NonSimplePieces, Result).

:- pred parse_simple_class_constraints(module_name::in, varset::in, term::in,
    list(format_piece)::in, maybe1(list(prog_constraint))::out) is det.

parse_simple_class_constraints(_ModuleName, VarSet, ConstraintsTerm,
        NonSimplePieces, Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(one_or_more(HeadArbConstraint, TailArbConstraints)),
        ( if
            % Fail if any of the constraints aren't simple.
            get_simple_constraint(HeadArbConstraint, HeadConstraint),
            list.map(get_simple_constraint,
                TailArbConstraints, TailConstraints)
        then
            % XXX ITEM_LIST Loosens representation; switching from one_or_more
            % to list allows an empty list.
            Result = ok1([HeadConstraint | TailConstraints])
        else
            Context = get_term_context(ConstraintsTerm),
            Pieces = NonSimplePieces ++
                color_as_incorrect([words("may only constrain"),
                    words("type variables and ground types.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            Result = error1([Spec])
        )
    ;
        Result0 = error1(Specs),
        Result = error1(Specs)
    ).

:- pred get_simple_constraint(arbitrary_constraint::in, prog_constraint::out)
    is semidet.

get_simple_constraint(ac_type_constraint(Constraint, _, [], _), Constraint).

parse_class_and_inst_constraints(_ModuleName, VarSet, ConstraintsTerm,
        Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(one_or_more(HeadArbConstraint, TailArbConstraints)),
        ArbitraryConstraints = [HeadArbConstraint | TailArbConstraints],
        collect_class_and_inst_constraints(ArbitraryConstraints,
            ProgConstraints, FunDeps, InstVarSub),
        (
            FunDeps = [],
            Result = ok2(ProgConstraints, InstVarSub)
        ;
            FunDeps = [_ | _],
            Pieces = [words("Error:")] ++
                color_as_subject([words("functional dependencies")]) ++
                [words("are only allowed")] ++
                color_as_incorrect([words("in typeclass declarations.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(ConstraintsTerm), Pieces),
            Result = error2([Spec])
        )
    ;
        Result0 = error1(Specs),
        Result = error2(Specs)
    ).

:- pred collect_class_and_inst_constraints(list(arbitrary_constraint)::in,
    list(prog_constraint)::out, list(prog_fundep)::out, inst_var_sub::out)
    is det.

collect_class_and_inst_constraints([], [], [], map.init).
collect_class_and_inst_constraints([Constraint | Constraints],
        !:ProgConstraints, !:FunDeps, !:InstVarSub) :-
    collect_class_and_inst_constraints(Constraints,
        !:ProgConstraints, !:FunDeps, !:InstVarSub),
    (
        Constraint = ac_type_constraint(ProgConstraint, _, _, _),
        !:ProgConstraints = [ProgConstraint | !.ProgConstraints]
    ;
        Constraint = ac_inst_constraint(InstVar, Inst, _),
        map.set(InstVar, Inst, !InstVarSub)
    ;
        Constraint = ac_fundep(FunDep, _),
        !:FunDeps = [FunDep | !.FunDeps]
    ).

:- type arbitrary_constraint
    --->    ac_type_constraint(prog_constraint, list(var_or_ground_type),
                list(mer_type), prog_context)
            % A constraint consisting of a typeclass name applied to one
            % or more types. The second argument lists the types that are
            % either type variables or ground types; the third argument lists
            % the types that are neither. (Superclass constraints, and the
            % constraints in type_spec_constrained_preds pragmas, may have
            % only type variables and ground types as arguments.)

    ;       ac_inst_constraint(inst_var, mer_inst, prog_context)
            % A constraint on an inst variable. Its principal functor is
            % '=<'/2.

    ;       ac_fundep(prog_fundep, prog_context).
            % A functional dependency. Its principal function symbol is '->'/2,
            % and both its argument terms contain one or more variables
            % separated by commas.

:- type arbitrary_constraints == one_or_more(arbitrary_constraint).

:- pred parse_arbitrary_constraints(varset::in, term::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result) :-
    conjunction_to_one_or_more(ConstraintsTerm,
        one_or_more(HeadConstraintTerm, TailConstraintTerms)),
    parse_arbitrary_constraint_list(VarSet,
        HeadConstraintTerm, TailConstraintTerms, Result).

:- pred parse_arbitrary_constraint_list(varset::in, term::in, list(term)::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraint_list(VarSet, HeadTerm, TailTerms, Result) :-
    parse_arbitrary_constraint(VarSet, HeadTerm, HeadResult),
    (
        TailTerms = [],
        (
            HeadResult = ok1(HeadConstraint),
            Result = ok1(one_or_more(HeadConstraint, []))
        ;
            HeadResult = error1(Specs),
            Result = error1(Specs)
        )
    ;
        TailTerms = [HeadTailTerm | TailTailTerms],
        parse_arbitrary_constraint_list(VarSet, HeadTailTerm, TailTailTerms,
            TailResult),
        ( if
            HeadResult = ok1(HeadConstraint),
            TailResult = ok1(TailConstraints)
        then
            Result = ok1(one_or_more.cons(HeadConstraint, TailConstraints))
        else
            Result = error1(get_any_errors1(HeadResult) ++
                get_any_errors1(TailResult))
        )
    ).

:- pred parse_arbitrary_constraint(varset::in, term::in,
    maybe1(arbitrary_constraint)::out) is det.

parse_arbitrary_constraint(VarSet, ConstraintTerm, Result) :-
    ( if
        ConstraintTerm =
            term.functor(term.atom("=<"), [LHSTerm, RHSTerm], Context)
    then
        (
            LHSTerm = term.variable(InstVar0, _),
            term.coerce_var(InstVar0, InstVar1),
            MaybeInstVar = ok1(InstVar1)
        ;
            LHSTerm = term.functor(_, _, LHSContext),
            LHSTermStr = describe_error_term(VarSet, LHSTerm),
            LHSPieces = [words("Error: a non-variable inst such as")] ++
                color_as_subject([quote(LHSTermStr)]) ++
                color_as_incorrect([words("may not be the subject"),
                    words("of an inst constraint.")]) ++
                [nl],
            LHSSpec = spec($pred, severity_error, phase_t2pt,
                LHSContext, LHSPieces),
            MaybeInstVar = error1([LHSSpec])
        ),
        ContextPieces = cord.from_list([words("In the constraining inst"),
            words("of an inst constraint:")]),
        parse_inst(no_allow_constrained_inst_var(wnciv_constraint_rhs),
            VarSet, ContextPieces, RHSTerm, MaybeInst),
        ( if
            MaybeInstVar = ok1(InstVar),
            MaybeInst = ok1(Inst)
        then
            Result = ok1(ac_inst_constraint(InstVar, Inst, Context))
        else
            Specs = get_any_errors1(MaybeInstVar)
                ++ get_any_errors1(MaybeInst),
            Result = error1(Specs)
        )
    else if
        parse_fundep(VarSet, ConstraintTerm, Result0)
    then
        Result = Result0
    else if
        try_parse_sym_name_and_args(ConstraintTerm, ClassName, ArgTerms0)
    then
        ArgsResultContextPieces =
            cord.singleton(words("In class constraint:")),
        parse_types(no_allow_ho_inst_info(wnhii_class_constraint),
            VarSet, ArgsResultContextPieces, ArgTerms0, ArgsResult),
        (
            ArgsResult = ok1(ArgTypes),
            varset.coerce(VarSet, TVarSet),
            Constraint = constraint(ClassName, ArgTypes),
            classify_types_as_var_ground_or_neither(TVarSet, ArgTypes,
                VoGTypes, NonVarNonGroundTypes),
            Context = get_term_context(ConstraintTerm),
            Result = ok1(ac_type_constraint(Constraint, VoGTypes,
                NonVarNonGroundTypes, Context))
        ;
            ArgsResult = error1(Specs),
            Result = error1(Specs)
        )
    else
        ConstraintTermStr = describe_error_term(VarSet, ConstraintTerm),
        Pieces = [words("Error: expected a")] ++
            color_as_correct([words("typeclass or inst constraint,")]) ++
            [words("got")] ++
            color_as_incorrect([quote(ConstraintTermStr)]) ++ [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ConstraintTerm), Pieces),
        Result = error1([Spec])
    ).

:- pred parse_fundep(varset::in, term::in,
    maybe1(arbitrary_constraint)::out) is semidet.

parse_fundep(VarSet, Term, Result) :-
    Term = term.functor(term.atom("->"), [DomainTerm, RangeTerm], Context),
    parse_fundep_side(VarSet, "a", "functional dependency domain",
        DomainTerm, MaybeDomain),
    parse_fundep_side(VarSet, "a", "functional dependency range",
        RangeTerm, MaybeRange),
    ( if
        MaybeDomain = ok1(Domain),
        MaybeRange = ok1(Range)
    then
        list_to_set(one_or_more_to_list(Domain), DomainSet),
        list_to_set(one_or_more_to_list(Range), RangeSet),
        set.intersect(DomainSet, RangeSet, CommonTypeVarSet),
        ( if set.is_non_empty(CommonTypeVarSet) then
            varset.coerce(VarSet, TVarSet),
            set.to_sorted_list(CommonTypeVarSet, CommonTypeVars),
            CommonTypeVarPieces = list.map(var_to_quote_piece(TVarSet),
                CommonTypeVars),
            CommonTypeVarsPieces = piece_list_to_color_pieces(color_subject,
                "and", [], CommonTypeVarPieces),
            Pieces = [
                words("Error: type"),
                words(choose_number(CommonTypeVars, "variable", "variables"))
            ] ++
            CommonTypeVarsPieces ++ [
                words(choose_number(CommonTypeVars, "occurs", "occur")),
                words("in")
            ] ++
            color_as_incorrect([
                words("both the domain and the range")
            ]) ++ [
                words(" of the same functional dependency."),
                nl
            ],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            Result = error1([Spec])
        else
            Result = ok1(ac_fundep(prog_fundep(Domain, Range), Context))
        )
    else
        Specs = get_any_errors1(MaybeDomain) ++ get_any_errors1(MaybeRange),
        Result = error1(Specs)
    ).

    % XXX ITEM_LIST Should return maybe1(one_or_more(tvar)).
    %
:- pred parse_fundep_side(varset::in, string::in, string::in, term::in,
    maybe1(one_or_more(tvar))::out) is det.

parse_fundep_side(VarSet0, AAn, Kind, TypesTerm0, MaybeTypeVars) :-
    VarSet = varset.coerce(VarSet0),
    TypesTerm = term.coerce(TypesTerm0),
    conjunction_to_list(TypesTerm, TypeTerms),
    terms_to_one_or_more_distinct_vars(VarSet, AAn, Kind, TypesTerm, TypeTerms,
        MaybeTypeVars).

:- pred classify_types_as_var_ground_or_neither(tvarset::in,
    list(mer_type)::in,
    list(var_or_ground_type)::out, list(mer_type)::out) is det.

classify_types_as_var_ground_or_neither(_, [], [], []).
classify_types_as_var_ground_or_neither(TVarSet, [Type0 | Types0],
        !:VarOrGroundTypes, !:NonVarNonGroundTypes) :-
    classify_types_as_var_ground_or_neither(TVarSet, Types0,
        !:VarOrGroundTypes, !:NonVarNonGroundTypes),
    Type1 = strip_kind_annotation(Type0),
    Type = coerce(Type1),
    ( if Type = type_variable(TVar, _Context) then
        varset.lookup_name(TVarSet, TVar, TVarName),
        !:VarOrGroundTypes =
            [type_var_name(TVar, TVarName) | !.VarOrGroundTypes]
    else if type_is_ground(Type, GroundType) then
        !:VarOrGroundTypes = [ground_type(GroundType) | !.VarOrGroundTypes]
    else
        !:NonVarNonGroundTypes = [Type | !.NonVarNonGroundTypes]
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_class.
%---------------------------------------------------------------------------%
