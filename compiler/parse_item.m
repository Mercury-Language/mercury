%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014, 2016-2019 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module handles the top level parsing of items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_item.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_item.

:- import_module term.
:- import_module varset.

    % parse_item_or_marker(ModuleName, VarSet, Term, SeqNum,
    %   MaybeItemOrMarker):
    %
    % Parse Term as either an item or sequence of items, or as a marker for
    % the start or end of a module, the start of a module section,
    % or a new source file.
    %
    % If Term represents an item (or more than one), bind MaybeItemOrMarker
    % to the parsed item(s), having qualified the appropriate parts of the item
    % with ModuleName as the module name. If Term represents a marker, include
    % its details in MaybeItemOrMarker. Include SeqNum as the sequence number
    % in both cases.
    %
    % If the parsing attempt is unsuccessful, bind MaybeItemOrMarker
    % to an appropriate error message.
    %
:- pred parse_item_or_marker(module_name::in, varset::in, term::in, int::in,
    maybe1(item_or_marker)::out) is det.

    % parse_class_decl(ModuleName, VarSet, Term, MaybeClassDecl):
    %
    % Parse Term as a declaration that may appear in the body of a
    % typeclass declaration. If successful, bind MaybeClassDecl to the
    % parsed item, otherwise bind it to an appropriate error message.
    % Qualify appropriate parts of the declaration with ModuleName
    % as the module name.
    %
    % Exported for use by parse_class.m.
    %
:- pred parse_class_decl(module_name::in, varset::in, term::in,
    maybe1(class_decl)::out) is det.

%---------------------------------------------------------------------------%

    % This type specifies whether the declaration we are attempting to parse
    % occurs inside a typeclass declaration or not.
    % XXX possibly we should also include the identity of the typeclass
    % involved in the case where parsing the class head succeeds.
    %
:- type decl_in_class
    --->    decl_is_in_class
    ;       decl_is_not_in_class.

%---------------------------------------------------------------------------%

:- type var_term_kind
    --->    vtk_type_decl_pred(decl_in_class)
    ;       vtk_type_decl_func(decl_in_class)
    ;       vtk_mode_decl_pred(decl_in_class)
    ;       vtk_mode_decl_func(decl_in_class)
    ;       vtk_class_decl
    ;       vtk_instance_decl
    ;       vtk_clause_pred
    ;       vtk_clause_func.

    % The term parser turns "X(a, b)" into "`'(X, a, b)".
    %
    % Check whether Term is the result of this transformation,
    % and if yes, return an error message that reflects what
    % the term was supposed to be.
    %
    % Exported for use by parse_class.m.
    %
:- pred is_the_name_a_variable(varset::in, var_term_kind::in, term::in,
    error_spec::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_class.
:- import_module parse_tree.parse_dcg_goal.
:- import_module parse_tree.parse_goal.
:- import_module parse_tree.parse_inst_mode_defn.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_mutable.
:- import_module parse_tree.parse_pragma.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_repn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module string.

%---------------------------------------------------------------------------%

parse_item_or_marker(ModuleName, VarSet, Term, SeqNum, MaybeIOM) :-
    ( if Term = term.functor(term.atom(":-"), [DeclTerm], _DeclContext) then
        parse_decl_term_item_or_marker(ModuleName, VarSet, DeclTerm,
            SeqNum, MaybeIOM)
    else
        parse_clause_term_item_or_marker(ModuleName, VarSet, Term,
            SeqNum, MaybeIOM)
    ).

%---------------------------------------------------------------------------%

:- pred parse_decl_term_item_or_marker(module_name::in, varset::in, term::in,
    int::in, maybe1(item_or_marker)::out) is det.
:- pragma inline(parse_decl_term_item_or_marker/5).

parse_decl_term_item_or_marker(ModuleName, VarSet, DeclTerm,
    SeqNum, MaybeIOM) :-
    ( if DeclTerm = term.functor(term.atom(Functor), ArgTerms, Context) then
        ( if
            parse_decl_item_or_marker(ModuleName, VarSet, Functor, ArgTerms,
                decl_is_not_in_class, Context, SeqNum, MaybeIOMPrime)
        then
            MaybeIOM = MaybeIOMPrime
        else
            Spec = decl_functor_is_not_valid(DeclTerm, Functor),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = decl_is_not_an_atom(VarSet, DeclTerm),
        MaybeIOM = error1([Spec])
    ).

:- func decl_is_not_an_atom(varset, term) = error_spec.

decl_is_not_an_atom(VarSet, Term) = Spec :-
    TermStr = mercury_term_to_string(VarSet, print_name_only, Term),
    Context = get_term_context(Term),
    Pieces = [words("Error:"), quote(TermStr),
        words("is not a valid declaration."), nl],
    Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
        Context, Pieces).

:- func decl_functor_is_not_valid(term, string) = error_spec.

decl_functor_is_not_valid(Term, Functor) = Spec :-
    Context = get_term_context(Term),
    Pieces = [words("Error:"), quote(Functor),
        words("is not a valid declaration type."), nl],
    Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
        Context, Pieces).

%---------------------------------------------------------------------------%

:- pred parse_decl_item_or_marker(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in,
    int::in, maybe1(item_or_marker)::out) is semidet.

parse_decl_item_or_marker(ModuleName, VarSet, Functor, ArgTerms,
        IsInClass, Context, SeqNum, MaybeIOM) :-
    require_switch_arms_det [Functor]
    (
        Functor = "module",
        parse_module_marker(ArgTerms, Context, SeqNum, MaybeIOM)
    ;
        Functor = "end_module",
        parse_end_module_marker(ArgTerms, Context, SeqNum, MaybeIOM)
    ;
        ( Functor = "interface", Section = ms_interface
        ; Functor = "implementation", Section = ms_implementation
        ),
        parse_section_marker(Functor, ArgTerms, Context, SeqNum,
            Section, MaybeIOM)
    ;
        ( Functor = "include_module", IIU = iiu_include_module
        ; Functor = "import_module", IIU = iiu_import_module
        ; Functor = "use_module", IIU = iiu_use_module
        ),
        parse_incl_imp_use_items(ModuleName, VarSet, Functor, ArgTerms,
            Context, SeqNum, IIU, MaybeIOM)
    ;
        Functor = "version_numbers",
        parse_version_numbers_marker(ModuleName, Functor, ArgTerms,
            Context, SeqNum, MaybeIOM)
    ;
        Functor = "type",
        parse_type_defn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            non_solver_type, MaybeIOM)
    ;
        Functor = "solver",
        parse_solver_type_defn_item(ModuleName, VarSet, ArgTerms,
            Context, SeqNum, MaybeIOM)
    ;
        Functor = "type_representation",
        parse_type_repn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        Functor = "inst",
        parse_inst_defn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        Functor = "abstract_inst",
        parse_abstract_inst_defn_item(ModuleName, VarSet, ArgTerms, Context,
            SeqNum, MaybeIOM)
    ;
        Functor = "mode",
        parse_mode_defn_or_decl_item(ModuleName, VarSet, ArgTerms,
            IsInClass, Context, SeqNum, allow_mode_decl_and_defn, [], MaybeIOM)
    ;
        Functor = "abstract_mode",
        parse_abstract_mode_defn_item(ModuleName, VarSet, ArgTerms, Context,
            SeqNum, MaybeIOM)
    ;
        ( Functor = "pred", PredOrFunc = pf_predicate
        ; Functor = "func", PredOrFunc = pf_function
        ),
        parse_pred_or_func_decl_item(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, PredOrFunc, [], [], MaybeIOM)
    ;
        ( Functor = "some", QuantType = quant_type_exist
        ; Functor = "all", QuantType = quant_type_univ
        ),
        parse_quant_attr(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, QuantType, cord.init, cord.init,
            MaybeIOM)
    ;
        ( Functor = "=>", QuantType = quant_type_exist
        ; Functor = "<=", QuantType = quant_type_univ
        ),
        parse_constraint_attr(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, QuantType, cord.init, cord.init,
            MaybeIOM)
    ;
        ( Functor = "impure", Purity = purity_impure
        ; Functor = "semipure", Purity = purity_semipure
        ),
        parse_purity_attr(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, Purity, cord.init, cord.init, MaybeIOM)
    ;
        Functor = "promise",
        parse_promise_item(VarSet, ArgTerms, Context, SeqNum, MaybeIOM)
    ;
        ( Functor = "promise_exclusive", PromiseType = promise_type_exclusive
        ; Functor = "promise_exhaustive", PromiseType = promise_type_exhaustive
        ; Functor = "promise_exclusive_exhaustive",
            PromiseType = promise_type_exclusive_exhaustive
        ),
        UnivQuantVars = [],
        parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
            PromiseType, UnivQuantVars, MaybeIOM)
    ;
        Functor = "typeclass",
        parse_typeclass_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        Functor = "instance",
        parse_instance_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        ( Functor = "initialise"
        ; Functor = "initialize"
        ),
        parse_initialise_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        ( Functor = "finalise"
        ; Functor = "finalize"
        ),
        parse_finalise_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        Functor = "mutable",
        parse_mutable_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeIOM)
    ;
        Functor = "pragma",
        parse_pragma(ModuleName, VarSet, ArgTerms, Context, SeqNum, MaybeIOM)
    ).

:- pred parse_attr_decl_item_or_marker(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in, int::in,
    cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is semidet.

parse_attr_decl_item_or_marker(ModuleName, VarSet, Functor, ArgTerms,
        IsInClass, Context, SeqNum, PurityAttrs0, QuantConstrAttrs0,
        MaybeIOM) :-
    % By coincidence, the kinds of items that may have purity,
    % quantification and/or constraint attributes on them, i.e.
    % the set item_pred_decl and item_mode_decl, is exactly the
    % set of items that may appear in class method specifications.
    %
    % A variant of the commented-out code below should help implement
    % quantification for these kinds of promise declarations, but enabling it
    % would break the above coincidence, requiring extra checks in
    % parse_class_decl.

    require_switch_arms_det [Functor]
    (
        Functor = "mode",
        parse_mode_defn_or_decl_item(ModuleName, VarSet, ArgTerms,
            IsInClass, Context, SeqNum, allow_mode_decl_only,
            cord.list(QuantConstrAttrs0), MaybeIOM0),
        ( if cord.is_empty(PurityAttrs0) then
            MaybeIOM = MaybeIOM0
        else
            Pieces = [words("Error: purity annotations"),
                words("are not allowed on mode declarations."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                Context, Pieces),
            (
                MaybeIOM0 = ok1(_),
                MaybeIOM = error1([Spec])
            ;
                MaybeIOM0 = error1(Specs0),
                MaybeIOM = error1([Spec | Specs0])
            )
        )
    ;
        ( Functor = "pred", PredOrFunc = pf_predicate
        ; Functor = "func", PredOrFunc = pf_function
        ),
        parse_pred_or_func_decl_item(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, PredOrFunc,
            cord.list(PurityAttrs0), cord.list(QuantConstrAttrs0), MaybeIOM)
    ;
        ( Functor = "some", QuantType = quant_type_exist
        ; Functor = "all", QuantType = quant_type_univ
        ),
        parse_quant_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
            Context, SeqNum, QuantType, PurityAttrs0, QuantConstrAttrs0,
            MaybeIOM)
    ;
        ( Functor = "=>", QuantType = quant_type_exist
        ; Functor = "<=", QuantType = quant_type_univ
        ),
        parse_constraint_attr(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, QuantType, PurityAttrs0,
            QuantConstrAttrs0, MaybeIOM)
    ;
        ( Functor = "impure", Purity = purity_impure
        ; Functor = "semipure", Purity = purity_semipure
        ),
        parse_purity_attr(ModuleName, VarSet, Functor, ArgTerms,
            IsInClass, Context, SeqNum, Purity, PurityAttrs0,
            QuantConstrAttrs0, MaybeIOM)
%   ;
%       ( Functor = "promise_exclusive", PromiseType = promise_type_exclusive
%       ; Functor = "promise_exhaustive", PromiseType = promise_type_exhaustive
%       ; Functor = "promise_exclusive_exhaustive",
%           PromiseType = promise_type_exclusive_exhaustive
%       ),
%       UnivQuantVars = [],
%       parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
%           PromiseType, UnivQuantVars, MaybeIOM)
    ).

%---------------------------------------------------------------------------%

:- pred parse_clause_term_item_or_marker(module_name::in, varset::in, term::in,
    int::in, maybe1(item_or_marker)::out) is det.
:- pragma inline(parse_clause_term_item_or_marker/5).

parse_clause_term_item_or_marker(ModuleName, VarSet, Term, SeqNum, MaybeIOM) :-
    ( if
        Term = term.functor(term.atom("-->"), [DCGHeadTerm, DCGBodyTerm],
            DCGContext)
    then
        % Term is a DCG clause.
        parse_dcg_clause(ModuleName, VarSet, DCGHeadTerm, DCGBodyTerm,
            DCGContext, SeqNum, MaybeIOM)
    else
        % Term is a clause; either a fact or a rule.
        ( if
            Term = term.functor(term.atom(":-"),
                [HeadTermPrime, BodyTermPrime], TermContext)
        then
            % Term is a rule.
            HeadTerm = HeadTermPrime,
            BodyTerm = BodyTermPrime,
            ClauseContext = TermContext
        else
            % Term is a fact.
            HeadTerm = Term,
            ClauseContext = get_term_context(HeadTerm),
            BodyTerm = term.functor(term.atom("true"), [], ClauseContext)
        ),
        parse_clause(ModuleName, VarSet, HeadTerm, BodyTerm,
            ClauseContext, SeqNum, MaybeIOM)
    ).

parse_class_decl(ModuleName, VarSet, Term, MaybeClassMethod) :-
    TermContext = get_term_context(Term),
    parse_attributed_decl(ModuleName, VarSet, Term, decl_is_in_class,
        TermContext, -1, cord.init, cord.init, MaybeIOM),
    (
        MaybeIOM = error1(Specs),
        MaybeClassMethod = error1(Specs)
    ;
        MaybeIOM = ok1(IOM),
        ( if IOM = iom_item(item_pred_decl(ItemPredDecl)) then
            ItemPredDecl = item_pred_decl_info(Name, PorF, ArgDecls,
                WithType, WithInst, MaybeDetism, _Origin,
                TypeVarSet, InstVarSet, ExistQVars, Purity,
                Constraints, Context, _SeqNum),
            PredOrFuncInfo = class_pred_or_func_info(Name, PorF, ArgDecls,
                WithType, WithInst, MaybeDetism, TypeVarSet, InstVarSet,
                ExistQVars, Purity, Constraints, Context),
            ClassDecl = class_decl_pred_or_func(PredOrFuncInfo),
            MaybeClassMethod = ok1(ClassDecl)
        else if IOM = iom_item(item_mode_decl(ItemModeDecl)) then
            ItemModeDecl = item_mode_decl_info(Name, MaybePorF, ArgModes,
                WithInst, MaybeDetism, InstVarSet, Context, _SeqNum),
            ModeInfo = class_mode_info(Name, MaybePorF, ArgModes,
                WithInst, MaybeDetism, InstVarSet, Context),
            ClassDecl = class_decl_mode(ModeInfo),
            MaybeClassMethod = ok1(ClassDecl)
        else
            Pieces = [words("Error: only pred, func and mode declarations"),
                words("are allowed in class interfaces."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                TermContext, Pieces),
            MaybeClassMethod = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------e

:- type purity_attr
    --->    purity_attr(purity).

:- type quantifier_type
    --->    quant_type_exist
    ;       quant_type_univ.

:- type quant_constr_attr
    --->    qca_quant_vars(quantifier_type, term)
    ;       qca_constraint(quantifier_type, term).

:- pred parse_quant_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in, int::in,
    quantifier_type::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_quant_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass, Context,
        SeqNum, QuantType, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM) :-
    ( if ArgTerms = [VarsTerm, SubTerm] then
        QuantAttr = qca_quant_vars(QuantType, VarsTerm),
        !:QuantConstrAttrs = cord.snoc(!.QuantConstrAttrs, QuantAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    else
        Pieces = [words("Error: the keyword"), quote(Functor),
            words("may appear in declarations"),
            words("only to denote the quantification"),
            words("of a list of variables."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_constraint_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in, int::in,
    quantifier_type::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_constraint_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
        Context, SeqNum, QuantType, !.PurityAttrs, !.QuantConstrAttrs,
        MaybeIOM) :-
    ( if ArgTerms = [SubTerm, ConstraintsTerm] then
        ConstrAttr = qca_constraint(QuantType, ConstraintsTerm),
        !:QuantConstrAttrs = cord.snoc(!.QuantConstrAttrs, ConstrAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    else
        Pieces = [words("Error: the symbol"), quote(Functor),
            words("may appear in declarations only to introduce"),
            words("a constraint or a conjunction of constraints."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_purity_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in, int::in,
    purity::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_purity_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
        Context, SeqNum, Purity, !.PurityAttrs, !.QuantConstrAttrs,
        MaybeIOM) :-
    ( if ArgTerms = [SubTerm] then
        PurityAttr = purity_attr(Purity),
        !:PurityAttrs = cord.snoc(!.PurityAttrs, PurityAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    else
        Pieces = [words("Error: the symbol"), quote(Functor),
            words("may appear only as an annotation"),
            words("in front of a predicate or function declaration."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_attributed_decl(module_name::in, varset::in, term::in,
    decl_in_class::in, prog_context::in, int::in,
    cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_attributed_decl(ModuleName, VarSet, Term, IsInClass, _Context, SeqNum,
        !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM) :-
    ( if Term = term.functor(term.atom(Functor), ArgTerms, FunctorContext) then
        ( if
            parse_attr_decl_item_or_marker(ModuleName, VarSet,
                Functor, ArgTerms, IsInClass, FunctorContext, SeqNum,
                !.PurityAttrs, !.QuantConstrAttrs, MaybeIOMPrime)
        then
            MaybeIOM = MaybeIOMPrime
        else
            Spec = decl_functor_is_not_valid(Term, Functor),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = decl_is_not_an_atom(VarSet, Term),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_module_marker(list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_module_marker(ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        ArgTerms = [ModuleNameTerm],
        try_parse_symbol_name(ModuleNameTerm, ModuleName)
    then
        Marker = iom_marker_module_start(ModuleName, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    else
        Pieces = [words("Error: a"), decl("module"), words("declaration"),
            words("should have just one argument,"),
            words("which should be a module name."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_end_module_marker(list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_end_module_marker(ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        ArgTerms = [ModuleNameTerm],
        try_parse_symbol_name(ModuleNameTerm, ModuleName)
    then
        Marker = iom_marker_module_end(ModuleName, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    else
        Pieces = [words("Error: an"), decl("end_module"), words("declaration"),
            words("should have just one argument,"),
            words("which should be a module name."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_section_marker(string::in, list(term)::in,
    prog_context::in, int::in, module_section::in,
    maybe1(item_or_marker)::out) is det.

parse_section_marker(Functor, ArgTerms, Context, SeqNum, Section, MaybeIOM) :-
    (
        ArgTerms = [],
        Marker = iom_marker_section(Section, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    ;
        ArgTerms = [_ | _],
        Pieces = [words("Error: an"), decl(Functor), words("declaration"),
            words("should have no arguments."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- type incl_imp_use
    --->    iiu_include_module
    ;       iiu_import_module
    ;       iiu_use_module.

:- pred parse_incl_imp_use_items(module_name::in, varset::in,
    string::in, list(term)::in, prog_context::in, int::in,
    incl_imp_use::in, maybe1(item_or_marker)::out) is det.

parse_incl_imp_use_items(ModuleName, VarSet, Functor, ArgTerms, Context,
        SeqNum, IIU, MaybeIOM) :-
    (
        IIU = iiu_include_module,
        Parser = parse_implicitly_qualified_module_name(ModuleName, VarSet)
    ;
        ( IIU = iiu_import_module
        ; IIU = iiu_use_module
        ),
        Parser = parse_module_name(VarSet)
    ),
    ( if ArgTerms = [ModuleNamesTerm] then
        parse_one_or_more(Parser, ModuleNamesTerm, MaybeModuleNames),
        (
            MaybeModuleNames = ok1(ModuleNames),
            ModuleNames = one_or_more(HeadModuleName, TailModuleNames),
            (
                IIU = iiu_include_module,
                make_item_include(Context, SeqNum,
                    HeadModuleName, HeadIncl),
                list.map(make_item_include(Context, SeqNum),
                    TailModuleNames, TailIncls),
                IOM = iom_marker_include(one_or_more(HeadIncl, TailIncls))
            ;
                IIU = iiu_import_module,
                make_item_avail_import(Context, SeqNum,
                    HeadModuleName, HeadImport),
                list.map(make_item_avail_import(Context, SeqNum),
                    TailModuleNames, TailImports),
                IOM = iom_marker_avail(one_or_more(HeadImport, TailImports))
            ;
                IIU = iiu_use_module,
                make_item_avail_use(Context, SeqNum,
                    HeadModuleName, HeadUse),
                list.map(make_item_avail_use(Context, SeqNum),
                    TailModuleNames, TailUses),
                IOM = iom_marker_avail(one_or_more(HeadUse, TailUses))
            ),
            MaybeIOM = ok1(IOM)
        ;
            MaybeModuleNames = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        (
            ( IIU = iiu_include_module
            ; IIU = iiu_import_module
            ),
            Article = "an"
        ;
            IIU = iiu_use_module,
            Article = "a"
        ),
        Pieces = [words("Error:"), words(Article), decl(Functor),
            words("declaration"), words("should have just one argument,"),
            words("which should be a list of one or more module names."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred make_item_include(prog_context::in, int::in, module_name::in,
    item_include::out) is det.

make_item_include(Context, SeqNum, ModuleName, Incl) :-
    Incl = item_include(ModuleName, Context, SeqNum).

:- pred make_item_avail_import(prog_context::in, int::in,
    module_name::in, item_avail::out) is det.

make_item_avail_import(Context, SeqNum, ModuleName, Avail) :-
    AvailImportInfo = avail_import_info(ModuleName, Context, SeqNum),
    Avail = avail_import(AvailImportInfo).

:- pred make_item_avail_use(prog_context::in, int::in,
    module_name::in, item_avail::out) is det.

make_item_avail_use(Context, SeqNum, ModuleName, Avail) :-
    AvailUseInfo = avail_use_info(ModuleName, Context, SeqNum),
    Avail = avail_use(AvailUseInfo).

%---------------------------------------------------------------------------%

:- type maybe_allow_mode_defn
    --->    allow_mode_decl_and_defn
    ;       allow_mode_decl_only.

:- pred parse_mode_defn_or_decl_item(module_name::in, varset::in,
    list(term)::in, decl_in_class::in,
    prog_context::in, int::in, maybe_allow_mode_defn::in,
    list(quant_constr_attr)::in, maybe1(item_or_marker)::out) is det.

parse_mode_defn_or_decl_item(ModuleName, VarSet, ArgTerms, IsInClass, Context,
        SeqNum, AllowModeDefn, QuantConstrAttrs, MaybeIOM) :-
    ( if ArgTerms = [SubTerm] then
        ( if
            SubTerm = term.functor(term.atom("=="), [HeadTerm, BodyTerm], _),
            % If AllowModeDefn = allow_mode_decl_only, then we expect SubTerm
            % to a mode declaration, so we have to parse it that way,
            % even if that yields nothing but error messages.
            AllowModeDefn = allow_mode_decl_and_defn
        then
            % This is the definition of a mode.
            parse_mode_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
                Context, SeqNum, MaybeIOM)
        else
            % This is the declaration of one mode of a predicate or function.
            parse_mode_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
                SeqNum, QuantConstrAttrs, MaybeIOM)
        )
    else
        Pieces = [words("Error: a"), decl("mode"), words("declaration"),
            words("should have just one argument,"),
            words("which should be either the definition of a mode,"),
            words("or the declaration of one mode"),
            words("of a predicate or function."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_version_numbers_marker(module_name::in,
    string::in, list(term)::in, prog_context::in, int::in,
    maybe1(item_or_marker)::out) is det.

parse_version_numbers_marker(ModuleName, Functor, ArgTerms,
        Context, _SeqNum, MaybeIOM) :-
    ( if
        ArgTerms = [VersionNumberTerm, ModuleNameTerm, VersionNumbersTerm]
    then
        ( if decimal_term_to_int(VersionNumberTerm, VersionNumber) then
            ( if VersionNumber = version_numbers_version_number then
                ( if try_parse_symbol_name(ModuleNameTerm, ModuleName) then
                    recompilation.version.parse_version_numbers(
                        VersionNumbersTerm, MaybeVersionNumbers),
                    (
                        MaybeVersionNumbers = ok1(VersionNumbers),
                        IOM = iom_marker_version_numbers(VersionNumbers),
                        MaybeIOM = ok1(IOM)
                    ;
                        MaybeVersionNumbers = error1(Specs),
                        MaybeIOM = error1(Specs)
                    )
                else
                    Pieces = [words("Error: invalid module name in"),
                        decl("version_numbers"), suffix("."), nl],
                    Spec = simplest_spec(severity_error,
                        phase_term_to_parse_tree,
                        get_term_context(ModuleNameTerm), Pieces),
                    MaybeIOM = error1([Spec])
                )
            else
                Pieces = [words("Error: the interface file"),
                    words("was created by an obsolete compiler,"),
                    words("so it must be rebuilt."), nl],
                Severity = severity_conditional(warn_smart_recompilation,
                    yes, severity_error, no),
                Spec = error_spec(Severity, phase_term_to_parse_tree,
                    [simple_msg(Context,
                        [option_is_set(warn_smart_recompilation, yes,
                            [always(Pieces)])])]),
                MaybeIOM = ok1(iom_handled([Spec]))
            )
        else
            Pieces = [words("Error: invalid version number in"),
                decl("version_numbers"), suffix("."), nl],
            VersionNumberContext = get_term_context(VersionNumbersTerm),
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                VersionNumberContext, Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Pieces = [words("Error: a"), decl(Functor), words("declaration"),
            words("should have exactly three arguments,"),
            words("which should be a version number,"),
            words("a module name, and a tuple containing maps"),
            words("from item ids to timestamps."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred parse_clause(module_name::in, varset::in, term::in, term::in,
    term.context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_clause(ModuleName, VarSet0, HeadTerm, BodyTerm0, Context, SeqNum,
        MaybeIOM) :-
    varset.coerce(VarSet0, ProgVarSet0),
    GoalContextPieces = cord.init,
    parse_goal(BodyTerm0, GoalContextPieces, MaybeBodyGoal,
        ProgVarSet0, ProgVarSet),
    varset.coerce(ProgVarSet, VarSet),
    ( if
        HeadTerm = term.functor(term.atom("="),
            [FuncHeadTerm0, FuncResultTerm0], _),
        FuncHeadTerm = desugar_field_access(FuncHeadTerm0)
    then
        MaybeFuncResultTerm = yes(FuncResultTerm0),
        ( if
            is_the_name_a_variable(VarSet0, vtk_clause_func, FuncHeadTerm,
                Spec)
        then
            MaybeFunctor = error2([Spec])
        else
            HeadContextPieces = cord.singleton(words("In equation head:")),
            parse_implicitly_qualified_sym_name_and_args(ModuleName,
                FuncHeadTerm, VarSet, HeadContextPieces, MaybeFunctor)
        )
    else
        MaybeFuncResultTerm = no,
        ( if
            is_the_name_a_variable(VarSet0, vtk_clause_pred, HeadTerm, Spec)
        then
            MaybeFunctor = error2([Spec])
        else
            HeadContextPieces = cord.singleton(words("In clause head:")),
            parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
                VarSet, HeadContextPieces, MaybeFunctor)
        )
    ),

    (
        MaybeFunctor = ok2(Name, ArgTerms0),
        (
            MaybeFuncResultTerm = yes(FuncResultTerm),
            PredOrFunc = pf_function,
            ArgTerms = ArgTerms0 ++ [FuncResultTerm]
        ;
            MaybeFuncResultTerm = no,
            PredOrFunc = pf_predicate,
            ArgTerms = ArgTerms0
        ),
        list.map(term.coerce, ArgTerms, ProgArgTerms),
        ItemClause = item_clause_info(Name, PredOrFunc, ProgArgTerms,
            item_origin_user, ProgVarSet, MaybeBodyGoal, Context, SeqNum),
        Item = item_clause(ItemClause),
        MaybeIOM = ok1(iom_item(Item))
    ;
        MaybeFunctor = error2(FunctorSpecs),
        Specs = FunctorSpecs ++ get_any_errors1(MaybeBodyGoal),
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%
%
% Parsing ":- pred" and ":- func" declarations.
%

    % parse_pred_or_func_decl parses a predicate or function declaration.
    %
:- pred parse_pred_or_func_decl_item(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in, int::in,
    pred_or_func::in, list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_or_func_decl_item(ModuleName, VarSet, Functor, ArgTerms,
        IsInClass, Context, SeqNum, PredOrFunc, PurityAttrs, QuantConstrAttrs,
        MaybeIOM) :-
    ( if ArgTerms = [Term] then
        (
            IsInClass = decl_is_in_class,
            PredOrFuncDeclPieces = [words("type class"), p_or_f(PredOrFunc),
                words("method declaration:")]
        ;
            IsInClass = decl_is_not_in_class,
            PredOrFuncDeclPieces = [p_or_f(PredOrFunc), words("declaration:")]
        ),
        DetismContextPieces =
            cord.from_list([words("In")] ++ PredOrFuncDeclPieces),
        parse_determinism_suffix(VarSet, DetismContextPieces, Term,
            BeforeDetismTerm, MaybeMaybeDetism),
        WithInstContextPieces = cord.from_list([
            words("In the"), quote("with_inst"), words("annotation of a")] ++
            PredOrFuncDeclPieces),
        parse_with_inst_suffix(VarSet, WithInstContextPieces,
            BeforeDetismTerm, BeforeWithInstTerm, MaybeWithInst),
        parse_with_type_suffix(VarSet, BeforeWithInstTerm, BeforeWithTypeTerm,
            MaybeWithType),
        BaseTerm = BeforeWithTypeTerm,
        ( if
            MaybeMaybeDetism = ok1(MaybeDetism),
            MaybeWithInst = ok1(WithInst),
            MaybeWithType = ok1(WithType)
        then
            ( if
                WithInst = yes(_),
                MaybeDetism = yes(_)
            then
                Pieces = [words("Error:"), quote("with_inst"),
                    words("and determinism both specified."), nl],
                Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                    get_term_context(BaseTerm), Pieces),
                MaybeIOM = error1([Spec])
            else if
                WithInst = yes(_),
                WithType = no
            then
                Pieces = [words("Error:"), quote("with_inst"),
                    words("specified"), words("without"),
                    quote("with_type"), suffix("."), nl],
                Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                    get_term_context(BaseTerm), Pieces),
                MaybeIOM = error1([Spec])
            else
                ( if
                    % Function declarations with `with_type` annotations
                    % have the same form as predicate declarations.
                    PredOrFunc = pf_function,
                    WithType = no
                then
                    parse_func_decl_base(ModuleName, VarSet,
                        BaseTerm, MaybeDetism, IsInClass,
                        Context, SeqNum, PurityAttrs, QuantConstrAttrs,
                        MaybeIOM)
                else
                    parse_pred_decl_base(PredOrFunc, ModuleName, VarSet,
                        BaseTerm, WithType, WithInst, MaybeDetism,
                        IsInClass, Context, SeqNum, PurityAttrs,
                        QuantConstrAttrs, MaybeIOM)
                )
            )
        else
            Specs = get_any_errors1(MaybeMaybeDetism)
                ++ get_any_errors1(MaybeWithInst)
                ++ get_any_errors1(MaybeWithType),
            MaybeIOM = error1(Specs)
        )
    else
        % Should we mention the determinism? It is allowed only
        % in predicate declarations that specify the modes, so the
        % wording required would probably be more confusing than helpful.
        Pieces = [words("Error: a"), decl(Functor), words("declaration"),
            words("should have just one argument,"),
            words("which should specify the types and maybe the modes"),
            words("of the arguments of a"), words(Functor), suffix("."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

    % parse a `:- pred p(...)' declaration or a
    % `:- func f(...) `with_type` t' declaration
    %
:- pred parse_pred_decl_base(pred_or_func::in, module_name::in, varset::in,
    term::in, maybe(mer_type)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, decl_in_class::in, prog_context::in, int::in,
    list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, PredTypeTerm,
        WithType, WithInst, MaybeDet, IsInClass, Context, SeqNum,
        PurityAttrs, QuantConstrAttrs, MaybeIOM) :-
    ContextPieces = cord.singleton(words("In")) ++
        cord.from_list(pred_or_func_decl_pieces(PredOrFunc)) ++
        cord.singleton(suffix(":")),
    get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, MaybeExistClassInstContext),
    get_purity_from_attrs(Context, PurityAttrs, MaybePurity),
    ( if
        MaybeExistClassInstContext =
            ok3(ExistQVars, Constraints, InstConstraints),
        MaybePurity = ok1(Purity)
    then
        % The term parser turns "X(a, b)" into "`'(X, a, b)".
        ( if
            is_the_name_a_variable(VarSet, vtk_type_decl_pred(IsInClass),
                PredTypeTerm, Spec)
        then
            MaybeIOM = error1([Spec])
        else
            parse_implicitly_qualified_sym_name_and_args(ModuleName,
                PredTypeTerm, VarSet, ContextPieces, MaybePredNameAndArgs),
            (
                MaybePredNameAndArgs = error2(Specs),
                MaybeIOM = error1(Specs)
            ;
                MaybePredNameAndArgs = ok2(Functor, ArgTerms),
                ArgContextFunc =
                    ( func(ArgNum) = ContextPieces ++
                        cord.from_list([words("in the"), nth_fixed(ArgNum),
                        words("argument:"), nl])
                    ),
                parse_type_and_modes(constrain_some_inst_vars(InstConstraints),
                    dont_require_tm_mode, wnhii_pred_arg, VarSet,
                    ArgContextFunc, ArgTerms, 1, TypesAndModes, [], TMSpecs),
                check_type_and_mode_list_is_consistent(TypesAndModes, no,
                    get_term_context(PredTypeTerm), MaybeTypeModeListKind),
                ( if
                    TMSpecs = [],
                    MaybeTypeModeListKind = ok1(_)
                then
                    ( if
                        WithInst = yes(_),
                        TypesAndModes = [type_only(_) | _]
                    then
                        Pieces = [words("Error:"), quote("with_inst"),
                            words("specified without argument modes."), nl],
                        Spec = simplest_spec(severity_error,
                            phase_term_to_parse_tree,
                            get_term_context(PredTypeTerm), Pieces),
                        MaybeIOM = error1([Spec])
                    else if
                        WithInst = no,
                        WithType = yes(_),
                        TypesAndModes = [type_and_mode(_, _) | _]
                    then
                        Pieces = [words("Error: arguments have modes but"),
                            quote("with_inst"), words("not specified."), nl],
                        Spec = simplest_spec(severity_error,
                            phase_term_to_parse_tree,
                            get_term_context(PredTypeTerm), Pieces),
                        MaybeIOM = error1([Spec])
                    else
                        varset.coerce(VarSet, TypeVarSet),
                        varset.coerce(VarSet, InstVarSet),
                        inconsistent_constrained_inst_vars_in_type_and_modes(
                            TypesAndModes, InconsistentVars),
                        report_inconsistent_constrained_inst_vars(
                            in_pred_or_func_decl_desc(PredOrFunc),
                            get_term_context(PredTypeTerm),
                            InstVarSet, InconsistentVars,
                            MaybeInconsistentSpec),
                        (
                            MaybeInconsistentSpec = no,
                            Origin = item_origin_user,
                            ItemPredDecl = item_pred_decl_info(Functor,
                                PredOrFunc, TypesAndModes, WithType, WithInst,
                                MaybeDet, Origin, TypeVarSet, InstVarSet,
                                ExistQVars, Purity, Constraints,
                                Context, SeqNum),
                            Item = item_pred_decl(ItemPredDecl),
                            MaybeIOM = ok1(iom_item(Item))
                        ;
                            MaybeInconsistentSpec = yes(Spec),
                            MaybeIOM = error1([Spec])
                        )
                    )
                else
                    Specs = TMSpecs ++ get_any_errors1(MaybeTypeModeListKind),
                    MaybeIOM = error1(Specs)
                )
            )
        )
    else
        Specs = get_any_errors1(MaybePurity) ++
            get_any_errors3(MaybeExistClassInstContext),
        MaybeIOM = error1(Specs)
    ).

    % Parse a `:- func p(...)' declaration *without* a with_type clause.
    %
:- pred parse_func_decl_base(module_name::in, varset::in, term::in,
    maybe(determinism)::in, decl_in_class::in, prog_context::in, int::in,
    list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_func_decl_base(ModuleName, VarSet, Term, MaybeDet, IsInClass, Context,
        SeqNum, PurityAttrs, QuantConstrAttrs, MaybeIOM) :-
    ContextPieces = cord.from_list([words("In"), decl("func"),
        words("declaration:"), nl]),
    get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, MaybeContext),
    (
        MaybeContext = error3(Specs),
        MaybeIOM = error1(Specs)
    ;
        MaybeContext = ok3(ExistQVars, Constraints, InstConstraints),
        ( if
            Term = term.functor(term.atom("="),
                [MaybeSugaredFuncTerm, ReturnTerm], _)
        then
            % The term parser turns "X(a, b)" into "`'(X, a, b)".
            ( if
                is_the_name_a_variable(VarSet, vtk_type_decl_func(IsInClass),
                    MaybeSugaredFuncTerm, Spec)
            then
                MaybeIOM = error1([Spec])
            else
                FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
                parse_implicitly_qualified_sym_name_and_args(ModuleName,
                    FuncTerm, VarSet, ContextPieces, MaybeFuncNameAndArgs),
                (
                    MaybeFuncNameAndArgs = error2(Specs),
                    MaybeIOM = error1(Specs)
                ;
                    MaybeFuncNameAndArgs = ok2(FuncName, ArgTerms),
                    ArgContextFunc = (func(ArgNum) = ContextPieces ++
                            cord.from_list([words("in the"), nth_fixed(ArgNum),
                            words("argument:"), nl])),
                    parse_type_and_modes(
                        constrain_some_inst_vars(InstConstraints),
                        dont_require_tm_mode, wnhii_func_arg,
                        VarSet, ArgContextFunc, ArgTerms, 1,
                        ArgTypesAndModes, [], ArgTMSpecs),
                    RetContextPieces = ContextPieces ++
                        cord.from_list([words("in the return value"), nl]),
                    parse_type_and_mode(
                        constrain_some_inst_vars(InstConstraints),
                        dont_require_tm_mode, wnhii_func_return_arg,
                        VarSet, RetContextPieces, ReturnTerm,
                        MaybeRetTypeAndMode),
                    ( if
                        ArgTMSpecs = [],
                        MaybeRetTypeAndMode = ok1(RetTypeAndMode)
                    then
                        % We use an auxiliary predicate because the code is
                        % just too deeply indented here.
                        parse_func_decl_base_2(FuncName,
                            ArgTypesAndModes, RetTypeAndMode,
                            FuncTerm, Term, VarSet, MaybeDet,
                            ExistQVars, Constraints, Context, SeqNum,
                            PurityAttrs, MaybeIOM)
                    else
                        Specs =
                            ArgTMSpecs ++ get_any_errors1(MaybeRetTypeAndMode),
                        MaybeIOM = error1(Specs)
                    )
                )
            )
        else
            Pieces = [words("Error:"), quote("="), words("expected in"),
                decl("func"), words("declaration."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                get_term_context(Term), Pieces),
            MaybeIOM = error1([Spec])
        )
    ).

:- pred parse_func_decl_base_2(sym_name::in, list(type_and_mode)::in,
    type_and_mode::in, term::in, term::in, varset::in, maybe(determinism)::in,
    existq_tvars::in, prog_constraints::in, prog_context::in, int::in,
    list(purity_attr)::in, maybe1(item_or_marker)::out) is det.

parse_func_decl_base_2(FuncName, Args, ReturnArg, FuncTerm, Term,
        VarSet, MaybeDetism, ExistQVars, Constraints, Context, SeqNum,
        PurityAttrs, MaybeIOM) :-
    check_type_and_mode_list_is_consistent(Args, yes(ReturnArg),
        get_term_context(FuncTerm), MaybeTypeModeListKind),
    get_purity_from_attrs(Context, PurityAttrs, MaybePurity),
    ( if
        MaybeTypeModeListKind = ok1(_),
        MaybePurity = ok1(Purity)
    then
        varset.coerce(VarSet, TVarSet),
        varset.coerce(VarSet, IVarSet),
        AllArgs = Args ++ [ReturnArg],
        inconsistent_constrained_inst_vars_in_type_and_modes(AllArgs,
            InconsistentVars),
        report_inconsistent_constrained_inst_vars("in function declaration",
            get_term_context(Term),
            IVarSet, InconsistentVars, MaybeInconsistentSpec),
        (
            MaybeInconsistentSpec = no,
            Origin = item_origin_user,
            ItemPredDecl = item_pred_decl_info(FuncName, pf_function, AllArgs,
                no, no, MaybeDetism, Origin, TVarSet, IVarSet, ExistQVars,
                Purity, Constraints, Context, SeqNum),
            Item = item_pred_decl(ItemPredDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeInconsistentSpec = yes(Spec),
            MaybeIOM = error1([Spec])
        )
    else
        Specs = get_any_errors1(MaybeTypeModeListKind)
            ++ get_any_errors1(MaybePurity),
        MaybeIOM = error1(Specs)
    ).

:- type type_mode_list_kind
    --->    tml_no_arguments
            % There are zero arguments.

    ;       tml_all_types_have_modes
            % There are some arguments, and they all have modes.

    ;       tml_no_types_have_modes.
            % There are some arguments, and none have modes.

    % Verify that among the arguments of a :- pred or :- func declaration,
    % either all arguments specify a mode or none of them do. If some do
    % and some don't, return an error message that identifies the argument
    % positions that are missing modes. (If some argument positions have
    % modes, then the programmer probably intended for all of them to have
    % modes.)
    %
:- pred check_type_and_mode_list_is_consistent(list(type_and_mode)::in,
    maybe(type_and_mode)::in, term.context::in,
    maybe1(type_mode_list_kind)::out) is det.

check_type_and_mode_list_is_consistent(TypesAndModes, MaybeRetTypeAndMode,
        Context, MaybeKind) :-
    classify_type_and_mode_list(1, TypesAndModes,
        WithModeArgNums0, WithoutModeArgNums0),
    (
        MaybeRetTypeAndMode = no,
        WithModeArgNums = WithModeArgNums0,
        WithoutModeArgNums = WithoutModeArgNums0
    ;
        MaybeRetTypeAndMode = yes(RetTypeAndMode),
        (
            RetTypeAndMode = type_only(_),
            WithModeArgNums = WithModeArgNums0,
            WithoutModeArgNums = WithoutModeArgNums0 ++ [-1]
        ;
            RetTypeAndMode = type_and_mode(_, _),
            WithModeArgNums = WithModeArgNums0 ++ [-1],
            WithoutModeArgNums = WithoutModeArgNums0
        )
    ),
    (
        WithModeArgNums = [],
        WithoutModeArgNums = [],
        % No arguments; no possibility of inconsistency.
        MaybeKind = ok1(tml_no_arguments)
    ;
        WithModeArgNums = [],
        WithoutModeArgNums = [_ | _],
        % No arguments have modes; no inconsistency.
        MaybeKind = ok1(tml_no_types_have_modes)
    ;
        WithModeArgNums = [_ | _],
        WithoutModeArgNums = [],
        % All arguments have modes; no inconsistency.
        MaybeKind = ok1(tml_all_types_have_modes)
    ;
        WithModeArgNums = [_ | _],
        WithoutModeArgNums = [FirstWithout | RestWithout],
        % Some arguments have modes and some don't, which is inconsistent.
        (
            RestWithout = [],
            IdPieces = [words("The argument without a mode is the"),
                wrap_nth(dont_add_the_prefix, FirstWithout), suffix("."), nl]
        ;
            RestWithout = [_ | _],
            % If the return value is one of the arguments without a mode,
            % then the "the" prefix before "return value" will come *after*
            % at least one argument number, to give a message such as
            % "The arguments without modes are the second and the return
            % value.".
            WithoutArgNumPieces =
                list.map(wrap_nth(add_the_prefix), WithoutModeArgNums),
            WithoutArgNumsPieces =
                component_list_to_pieces("and", WithoutArgNumPieces),
            IdPieces = [words("The arguments without modes are the") |
                WithoutArgNumsPieces] ++ [suffix("."), nl]
        ),
        Pieces = [words("Error: some but not all arguments have modes."), nl
            | IdPieces],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeKind = error1([Spec])
    ).

:- pred classify_type_and_mode_list(int::in, list(type_and_mode)::in,
    list(int)::out, list(int)::out) is det.

classify_type_and_mode_list(_, [], [], []).
classify_type_and_mode_list(ArgNum, [Head | Tail],
        WithModeArgNums, WithoutModeArgNums) :-
    classify_type_and_mode_list(ArgNum + 1, Tail,
        WithModeArgNums0, WithoutModeArgNums0),
    (
        Head = type_only(_),
        WithModeArgNums = WithModeArgNums0,
        WithoutModeArgNums = [ArgNum | WithoutModeArgNums0]
    ;
        Head = type_and_mode(_, _),
        WithModeArgNums = [ArgNum | WithModeArgNums0],
        WithoutModeArgNums = WithoutModeArgNums0
    ).

:- type maybe_add_the_prefix
    --->    dont_add_the_prefix
    ;       add_the_prefix.

:- func wrap_nth(maybe_add_the_prefix, int) = format_component.

wrap_nth(MaybeAddPredix, ArgNum) = Component :-
    ( if ArgNum < 0 then
        (
            MaybeAddPredix = dont_add_the_prefix,
            Component = words("return value")
        ;
            MaybeAddPredix = add_the_prefix,
            Component = words("the return value")
        )
    else
        Component = nth_fixed(ArgNum)
    ).

%---------------------------------------------------------------------------%
%
% Parsing mode declarations for predicates and functions.
%

:- pred parse_mode_decl(module_name::in, varset::in, term::in,
    decl_in_class::in, prog_context::in, int::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_mode_decl(ModuleName, VarSet, Term, IsInClass, Context, SeqNum,
        QuantConstrAttrs, MaybeIOM) :-
    (
        IsInClass = decl_is_in_class,
        DeclWords = words("type class method mode")
    ;
        IsInClass = decl_is_not_in_class,
        DeclWords = words("mode")
    ),
    DetismContextPieces = cord.from_list([
        words("In"), DeclWords, words("declaration:")
    ]),
    parse_determinism_suffix(VarSet, DetismContextPieces, Term,
        BeforeDetismTerm, MaybeMaybeDetism),
    WithInstContextPieces = cord.from_list([
        words("In the"), quote("with_inst"), words("annotation of a"),
        DeclWords, words("declaration:")
    ]),
    parse_with_inst_suffix(VarSet, WithInstContextPieces, BeforeDetismTerm,
        BeforeWithInstTerm, MaybeWithInst),
    BaseTerm = BeforeWithInstTerm,
    ( if
        MaybeMaybeDetism = ok1(MaybeDetism),
        MaybeWithInst = ok1(WithInst)
    then
        ( if
            MaybeDetism = yes(_),
            WithInst = yes(_)
        then
            Pieces = [words("Error:"), quote("with_inst"),
                words("and determinism both specified."), nl],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                get_term_context(Term), Pieces),
            MaybeIOM = error1([Spec])
        else
            parse_mode_decl_base(ModuleName, VarSet, BaseTerm, IsInClass,
                Context, SeqNum, WithInst, MaybeDetism, QuantConstrAttrs,
                MaybeIOM)
        )
    else
        Specs = get_any_errors1(MaybeMaybeDetism)
            ++ get_any_errors1(MaybeWithInst),
        MaybeIOM = error1(Specs)
    ).

:- pred parse_mode_decl_base(module_name::in, varset::in, term::in,
    decl_in_class::in, prog_context::in, int::in, maybe(mer_inst)::in,
    maybe(determinism)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_mode_decl_base(ModuleName, VarSet, Term, IsInClass, Context, SeqNum,
        WithInst, MaybeDet, QuantConstrAttrs, MaybeIOM) :-
    ( if
        WithInst = no,
        Term = term.functor(term.atom("="),
            [MaybeSugaredFuncTerm, ReturnTypeTerm], _)
    then
        % The term parser turns "X(a, b)" into "`'(X, a, b)".
        ( if
            is_the_name_a_variable(VarSet, vtk_mode_decl_func(IsInClass),
                MaybeSugaredFuncTerm, Spec)
        then
            MaybeIOM = error1([Spec])
        else
            FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
            ContextPieces = cord.from_list([words("In function"), decl("mode"),
                words("declaration")]),
            parse_implicitly_qualified_sym_name_and_args(ModuleName, FuncTerm,
                VarSet, ContextPieces, MaybeFunctorArgs),
            (
                MaybeFunctorArgs = error2(Specs),
                MaybeIOM = error1(Specs)
            ;
                MaybeFunctorArgs = ok2(Functor, ArgTerms),
                parse_func_mode_decl(Functor, ArgTerms, ModuleName,
                    ReturnTypeTerm, Term, VarSet, MaybeDet, Context, SeqNum,
                    QuantConstrAttrs, MaybeIOM)
            )
        )
    else
        % The term parser turns "X(a, b)" into "`'(X, a, b)".
        ( if
            is_the_name_a_variable(VarSet, vtk_mode_decl_pred(IsInClass),
                Term, Spec)
        then
            MaybeIOM = error1([Spec])
        else
            ContextPieces = cord.from_list([words("In"), decl("mode"),
                words("declaration")]),
            parse_implicitly_qualified_sym_name_and_args(ModuleName, Term,
                VarSet, ContextPieces, MaybeFunctorArgs),
            (
                MaybeFunctorArgs = error2(Specs),
                MaybeIOM = error1(Specs)
            ;
                MaybeFunctorArgs = ok2(Functor, ArgTerms),
                parse_pred_mode_decl(Functor, ArgTerms, ModuleName, Term,
                    VarSet, WithInst, MaybeDet,
                    Context, SeqNum, QuantConstrAttrs, MaybeIOM)
            )
        )
    ).

:- pred parse_pred_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, varset::in, maybe(mer_inst)::in, maybe(determinism)::in,
    prog_context::in, int::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_mode_decl(Functor, ArgTerms, ModuleName, PredModeTerm, VarSet,
        WithInst, MaybeDet, Context, SeqNum, QuantConstrAttrs, MaybeIOM) :-
    ArgContextPieces = cord.from_list(
        [words("In the mode declaration of the predicate"),
        unqual_sym_name(Functor), suffix(":")]),
    parse_modes(allow_constrained_inst_var, VarSet, ArgContextPieces,
        ArgTerms, MaybeArgModes0),
    ContextPieces = cord.from_list([words("In predicate"), decl("mode"),
        words("declaration")]),
    get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, MaybeConstraints),
    ( if
        MaybeArgModes0 = ok1(ArgModes0),
        MaybeConstraints = ok3(_, _, InstConstraints)
    then
        list.map(constrain_inst_vars_in_mode_sub(InstConstraints),
            ArgModes0, ArgModes),
        varset.coerce(VarSet, InstVarSet),
        inconsistent_constrained_inst_vars_in_modes(ArgModes,
            InconsistentVars),
        report_inconsistent_constrained_inst_vars(
            "in predicate mode declaration", get_term_context(PredModeTerm),
            InstVarSet, InconsistentVars, MaybeInconsistentSpec),
        (
            MaybeInconsistentSpec = no,
            (
                WithInst = no,
                MaybePredOrFunc = yes(pf_predicate)
            ;
                WithInst = yes(_),
                % We don't know whether it's a predicate or a function
                % until we expand out the inst.
                MaybePredOrFunc = no
            ),
            ItemModeDecl = item_mode_decl_info(Functor, MaybePredOrFunc,
                ArgModes, WithInst, MaybeDet, InstVarSet,
                Context, SeqNum),
            Item = item_mode_decl(ItemModeDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeInconsistentSpec = yes(Spec),
            MaybeIOM = error1([Spec])
        )
    else
        Specs = get_any_errors1(MaybeArgModes0)
            ++ get_any_errors3(MaybeConstraints),
        MaybeIOM = error1(Specs)
    ).

:- pred parse_func_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, term::in, varset::in, maybe(determinism)::in,
    prog_context::in, int::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_func_mode_decl(Functor, ArgTerms, ModuleName, RetModeTerm, FullTerm,
        VarSet, MaybeDetism, Context, SeqNum, QuantConstrAttrs, MaybeIOM) :-
    ArgContextPieces = cord.from_list(
        [words("In the mode declaration of the function"),
        unqual_sym_name(Functor), suffix(":")]),
    parse_modes(allow_constrained_inst_var, VarSet, ArgContextPieces,
        ArgTerms, MaybeArgModes0),
    RetContextPieces = cord.from_list([words("In the return value"),
        words("of the mode declaration of the function"),
        unqual_sym_name(Functor), suffix(":")]),
    parse_mode(allow_constrained_inst_var, VarSet, RetContextPieces,
        RetModeTerm, MaybeRetMode0),
    QuantContextPieces = cord.from_list([words("In function"), decl("mode"),
        words("declaration")]),
    get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, QuantContextPieces, MaybeConstraints),
    ( if
        MaybeArgModes0 = ok1(ArgModes0),
        MaybeRetMode0 = ok1(RetMode0),
        MaybeConstraints = ok3(_, _, InstConstraints)
    then
        list.map(constrain_inst_vars_in_mode_sub(InstConstraints),
            ArgModes0, ArgModes),
        constrain_inst_vars_in_mode_sub(InstConstraints,
            RetMode0, RetMode),
        varset.coerce(VarSet, InstVarSet),
        ArgReturnModes = ArgModes ++ [RetMode],
        inconsistent_constrained_inst_vars_in_modes(ArgReturnModes,
            InconsistentVars),
        report_inconsistent_constrained_inst_vars(
            "in function mode declaration", get_term_context(FullTerm),
            InstVarSet, InconsistentVars, MaybeInconsistentSpec),
        (
            MaybeInconsistentSpec = no,
            ItemModeDecl = item_mode_decl_info(Functor,
                yes(pf_function), ArgReturnModes, no, MaybeDetism,
                InstVarSet, Context, SeqNum),
            Item = item_mode_decl(ItemModeDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeInconsistentSpec = yes(Spec),
            MaybeIOM = error1([Spec])
        )
    else
        Specs = get_any_errors1(MaybeArgModes0)
            ++ get_any_errors1(MaybeRetMode0)
            ++ get_any_errors3(MaybeConstraints),
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%

:- pred get_purity_from_attrs(prog_context::in, list(purity_attr)::in,
    maybe1(purity)::out) is det.

get_purity_from_attrs(_Context, [], ok1(purity_pure)).
get_purity_from_attrs(Context, [PurityAttr | PurityAttrs], MaybePurity) :-
    PurityAttr = purity_attr(Purity),
    (
        PurityAttrs = [],
        MaybePurity = ok1(Purity)
    ;
        PurityAttrs = [_ | _],
        Pieces = [words("Error: duplicate purity annotations"),
            words("are not allowed."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybePurity = error1([Spec])
    ).

%---------------------------------------------------------------------------%

    % We could perhaps get rid of some code duplication between here and
    % parse_class.m?

    % XXX This documentation is out of date.
    % get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
    %   QuantConstrAttrs, ContextPieces, MaybeExistClassInstContext):
    %
    % Parse type quantifiers, type class constraints and inst constraints
    % from the attributes in QuantConstrAttrs.
    %
    % In the absence of any errors, return MaybeExistClassInstContext
    % as a triple of ExistQVars, ClassConstraints, and InstConstraints,
    % with ExistQVars listing the existentially quantified variables,
    % ClassConstraints listing both the universal and existential type
    % class constraints, and InstConstraints mapping each inst variable
    % to the (smallest) constraint containing them.
    % XXX The "smallest" part of that is almost certainly bug.
    %
:- pred get_class_context_and_inst_constraints_from_attrs(module_name::in,
    varset::in, list(quant_constr_attr)::in, cord(format_component)::in,
    maybe3(existq_tvars, prog_constraints, inst_var_sub)::out) is det.

get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, MaybeExistClassInstContext) :-
    % When we reach here, QuantConstrAttrs contains declaration attributes
    % in the outermost to innermost order.
    %
    % Constraints and quantifiers should occur in the following order,
    % outermost to innermost:
    %
    %                               operator        precedence
    %                               --------        ----------
    %   1. universal quantifiers    all             950
    %   2. existential quantifiers  some            950
    %   3. universal constraints    <=              920
    %   4. existential constraints  =>              920 [*]
    %   5. the decl itself          pred or func    800
    %
    % [*] Note that the semantic meaning of `=>' is not quite the same
    % as implication; logically speaking it's more like conjunction.
    % Oh well, at least it has the right precedence.
    %
    % In theory it could make sense to allow the order of 2 & 3 to be
    % swapped, or (in the case of multiple constraints & multiple
    % quantifiers) to allow arbitrary interleaving of 2 & 3, but in
    % practice it seems there would be little benefit in allowing that
    % flexibility, so we don't.
    %
    % NOTE We do NOT check that the order above is actually followed.
    %
    % Universal quantification is the default, so we just ignore
    % universal quantifiers. (XXX It might be a good idea to check that
    % any universally quantified type variables do actually occur SOMEWHERE
    % in the type declaration, and are not also existentially quantified,
    % and if not, issue a warning or error message.)

    get_class_context_and_inst_constraints_loop(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, [], Specs,
        cord.init, _UnivQVarsCord, cord.init, ExistQVarsCord,
        cord.init, UnivClassConstraints, map.init, UnivInstConstraints,
        cord.init, ExistClassConstraints, map.init, ExistInstConstraints),

    ExistQVars0 = cord.list(ExistQVarsCord),
    list.map(term.coerce_var, ExistQVars0, ExistQVars),
    (
        Specs = [],
        ClassConstraints = constraints(
            cord.list(UnivClassConstraints),
            cord.list(ExistClassConstraints)),
        InstConstraints =
            map.old_merge(UnivInstConstraints, ExistInstConstraints),
        MaybeExistClassInstContext = ok3(ExistQVars, ClassConstraints,
            InstConstraints)
    ;
        Specs = [_ | _],
        MaybeExistClassInstContext = error3(Specs)
    ).

:- pred get_class_context_and_inst_constraints_loop(module_name::in,
    varset::in, list(quant_constr_attr)::in, cord(format_component)::in,
    list(error_spec)::in, list(error_spec)::out,
    cord(var)::in, cord(var)::out, cord(var)::in, cord(var)::out,
    cord(prog_constraint)::in, cord(prog_constraint)::out,
    inst_var_sub::in, inst_var_sub::out,
    cord(prog_constraint)::in, cord(prog_constraint)::out,
    inst_var_sub::in, inst_var_sub::out) is det.

get_class_context_and_inst_constraints_loop(_ModuleName, _VarSet,
        [], _ContextPieces, !Specs, !UnivQVars, !ExistQVars,
        !UnivClassConstraints, !UnivInstConstraints,
        !ExistvClassConstraints, !ExistvInstConstraints).
get_class_context_and_inst_constraints_loop(ModuleName, VarSet,
        [QuantConstrAttr | QuantConstrAttrs], ContextPieces, !Specs,
        !UnivQVars, !ExistQVars,
        !UnivClassConstraints, !UnivInstConstraints,
        !ExistClassConstraints, !ExistInstConstraints) :-
    (
        QuantConstrAttr = qca_quant_vars(QuantType, VarsTerm),
        % Both versions of ContextPieces should be statically allocated terms.
        (
            QuantType = quant_type_exist,
            TailContextPieces = [words("in first argument of"),
                quote("some"), suffix(":")]
        ;
            QuantType = quant_type_univ,
            TailContextPieces = [words("in first argument of"),
                quote("all"), suffix(":")]
        ),
        VarsContextPieces = ContextPieces ++ cord.from_list(TailContextPieces),
        parse_possibly_repeated_vars(VarsTerm, VarSet, VarsContextPieces,
            MaybeVars),
        (
            MaybeVars = error1(VarsSpecs),
            !:Specs = VarsSpecs ++ !.Specs
        ;
            MaybeVars = ok1(Vars),
            (
                QuantType = quant_type_exist,
                !:ExistQVars = !.ExistQVars ++ cord.from_list(Vars)
            ;
                QuantType = quant_type_univ,
                !:UnivQVars = !.UnivQVars ++ cord.from_list(Vars)
            )
        )
    ;
        QuantConstrAttr = qca_constraint(QuantType, ConstraintsTerm),
        parse_class_and_inst_constraints(ModuleName, VarSet, ConstraintsTerm,
            MaybeConstraints),
        (
            MaybeConstraints = error2(ConstraintSpecs),
            !:Specs = ConstraintSpecs ++ !.Specs
        ;
            MaybeConstraints = ok2(ClassConstraints, InstConstraint),
            (
                QuantType = quant_type_exist,
                !:ExistClassConstraints = !.ExistClassConstraints ++
                    cord.from_list(ClassConstraints),
                !:ExistInstConstraints =
                    map.old_merge(!.ExistInstConstraints, InstConstraint)
            ;
                QuantType = quant_type_univ,
                !:UnivClassConstraints = !.UnivClassConstraints ++
                    cord.from_list(ClassConstraints),
                !:UnivInstConstraints =
                    map.old_merge(!.UnivInstConstraints, InstConstraint)
            )
        )

    ),
    get_class_context_and_inst_constraints_loop(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, !Specs, !UnivQVars, !ExistQVars,
        !UnivClassConstraints, !UnivInstConstraints,
        !ExistClassConstraints, !ExistInstConstraints).

%---------------------------------------------------------------------------%

:- pred parse_promise_item(varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_or_marker)::out) is det.

parse_promise_item(VarSet, ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if ArgTerms = [Term] then
        varset.coerce(VarSet, ProgVarSet0),
        ContextPieces = cord.init,
        parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
        (
            MaybeGoal0 = ok1(Goal0),
            PromiseType = promise_type_true,
            ( if
                Goal0 = quant_expr(quant_all, quant_ordinary_vars, _,
                    UnivVars0, AllGoal)
            then
                UnivVars0 = UnivVars,
                Goal = AllGoal
            else
                UnivVars = [],
                Goal = Goal0
            ),
            ItemPromise = item_promise_info(PromiseType, Goal, ProgVarSet,
                UnivVars, Context, SeqNum),
            Item = item_promise(ItemPromise),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeGoal0 = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), decl("promise"), words("declaration"),
            words("should have just one argument,"),
            words("which should be a goal."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_promise_ex_item(varset::in, string::in, list(term)::in,
    prog_context::in, int::in, promise_type::in, list(term)::in,
    maybe1(item_or_marker)::out) is det.

parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
        PromiseType, _UnivVarTerms, MaybeIOM) :-
    ( if ArgTerms = [Term] then
        varset.coerce(VarSet, ProgVarSet0),
        ContextPieces = cord.init,
        parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
        (
            MaybeGoal0 = ok1(Goal),
            % Get universally quantified variables.
            % XXX We used to try to get a list of universally quantified
            % variables from attributes, using this code:
            % get_quant_vars(quant_type_univ, ModuleName, [], _,
            %     [], UnivVars0),
            % list.map(term.coerce_var, UnivVars0, UnivVars),
            % However, passing [] as the list of attributes,
            % instead of a list of attributes passed to us by our caller,
            % guaranteed that the value of UnivVars would ALWAYS be [].
            %
            % We should allow our caller to process "all [<vars>]" prefixes
            % before the promise_ex declaration, and give us the terms
            % containing lists of variables for us to parse.
            UnivVars = [],
            ItemPromise = item_promise_info(PromiseType, Goal, ProgVarSet,
                UnivVars, Context, SeqNum),
            Item = item_promise(ItemPromise),
            MaybeIOM = ok1(iom_item(Item))
        ;
            MaybeGoal0 = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a"), decl(Functor), words("declaration"),
            words("should have just one argument,"),
            words("which should be a goal."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

    % parse_determinism_suffix(VarSet, ContextPieces, BodyTerm,
    %   BeforeDetismTerm, MaybeMaybeDetism):
    %
    % Look for a suffix of the form "is <detism>" in Term. If we find one,
    % bind MaybeMaybeDetism to ok1(yes()) wrapped around the determinism,
    % and bind BeforeDetismTerm to the other part of Term. If we don't
    % find, one, then bind MaybeMaybeDetism to ok1(no).
    %
:- pred parse_determinism_suffix(varset::in, cord(format_component)::in,
    term::in, term::out, maybe1(maybe(determinism))::out) is det.

parse_determinism_suffix(VarSet, ContextPieces, Term, BeforeDetismTerm,
        MaybeMaybeDetism) :-
    ( if
        Term = term.functor(term.atom("is"), Args, _),
        Args = [BeforeDetismTermPrime, DetismTerm]
    then
        BeforeDetismTerm = BeforeDetismTermPrime,
        ( if
            DetismTerm = term.functor(term.atom(DetismFunctor), [], _),
            standard_det(DetismFunctor, Detism)
        then
            MaybeMaybeDetism = ok1(yes(Detism))
        else
            DetismTermStr = describe_error_term(VarSet, DetismTerm),
            Pieces = cord.list(ContextPieces) ++ [
                lower_case_next_if_not_first,
                words("Error: invalid determinism category"),
                quote(DetismTermStr), suffix("."), nl
            ],
            Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
                get_term_context(DetismTerm), Pieces),
            MaybeMaybeDetism = error1([Spec])
        )
    else
        BeforeDetismTerm = Term,
        MaybeMaybeDetism = ok1(no)
    ).

    % Process the `with_type type` suffix part of a declaration.
    %
:- pred parse_with_type_suffix(varset::in, term::in,
    term::out, maybe1(maybe(mer_type))::out) is det.

parse_with_type_suffix(VarSet, Term, BeforeWithTypeTerm, MaybeWithType) :-
    ( if
        Term = term.functor(TypeQualifier,
            [BeforeWithTypeTermPrime, TypeTerm], _),
        ( TypeQualifier = term.atom("with_type")
        ; TypeQualifier = term.atom(":")
        )
    then
        BeforeWithTypeTerm = BeforeWithTypeTermPrime,
        ContextPieces = cord.from_list([words("In"), quote("with_type"),
            words("annotation:")]),
        parse_type(no_allow_ho_inst_info(wnhii_type_qual),
            VarSet, ContextPieces, TypeTerm, MaybeType),
        (
            MaybeType = ok1(Type),
            MaybeWithType = ok1(yes(Type))
        ;
            MaybeType = error1(Specs),
            MaybeWithType = error1(Specs)
        )
    else
        BeforeWithTypeTerm = Term,
        MaybeWithType = ok1(no)
    ).

    % Process the `with_inst inst` suffix part of a declaration.
    %
:- pred parse_with_inst_suffix(varset::in, cord(format_component)::in,
    term::in, term::out, maybe1(maybe(mer_inst))::out) is det.

parse_with_inst_suffix(VarSet, ContextPieces, Term,
        BeforeWithInstTerm, MaybeWithInst) :-
    ( if
        Term = term.functor(term.atom("with_inst"),
            [BeforeWithInstTermPrime, InstTerm], _)
    then
        BeforeWithInstTerm = BeforeWithInstTermPrime,
        parse_inst(allow_constrained_inst_var, VarSet, ContextPieces,
            InstTerm, MaybeInst),
        (
            MaybeInst = ok1(Inst),
            MaybeWithInst = ok1(yes(Inst))
        ;
            MaybeInst = error1(Specs),
            MaybeWithInst = error1(Specs)
        )
    else
        BeforeWithInstTerm = Term,
        MaybeWithInst = ok1(no)
    ).

%---------------------------------------------------------------------------%

    % Perform one of the following field-access syntax rewrites if possible:
    %
    %   A ^ f(B, ...)           --->    f(B, ..., A)
    %   (A ^ f(B, ...) := X)    --->    'f :='(B, ..., A, X)
    %
:- func desugar_field_access(term) = term.

desugar_field_access(Term) = DesugaredTerm :-
    ( if
        Term = functor(atom("^"), [A, RHS], Context),
        RHS  = functor(atom(FieldName), Bs, _)
    then
        DesugaredTerm = functor(atom(FieldName), Bs ++ [A], Context)
    else if
        Term = functor(atom(":="), [LHS, X], _),
        LHS  = functor(atom("^"), [A, RHS], Context),
        RHS  = functor(atom(FieldName), Bs, _)
    then
        FunctionName = FieldName ++ " :=",
        DesugaredTerm = functor(atom(FunctionName), Bs ++ [A, X], Context)
    else
        DesugaredTerm = Term
    ).

%---------------------------------------------------------------------------%

    % A ModuleName is just an sym_name.
    %
:- pred parse_module_name(varset::in, term::in,
    maybe1(module_name)::out) is det.

parse_module_name(VarSet, Term, MaybeModuleName) :-
    parse_symbol_name(VarSet, Term, MaybeModuleName).

    % A ModuleName is an implicitly-quantified sym_name.
    %
    % We check for module names starting with capital letters as a special
    % case, so that we can report a better error message for that case.
    %
:- pred parse_implicitly_qualified_module_name(module_name::in,
    varset::in, term::in, maybe1(module_name)::out) is det.

parse_implicitly_qualified_module_name(DefaultModuleName, VarSet, Term,
        MaybeModule) :-
    (
        Term = term.variable(_, Context),
        Pieces = [words("Error: module names starting with capital letters"),
            words("must be quoted using single quotes"),
            words("(e.g. "":- module 'Foo'."")."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeModule = error1([Spec])
    ;
        Term = term.functor(_, _, _),
        parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet,
            Term, MaybeModule)
    ).

%---------------------------------------------------------------------------%

is_the_name_a_variable(VarSet, Kind, Term, Spec) :-
    ( if Term = term.functor(term.atom(""), ArgTerms, TermContext) then
        ( if
            ArgTerms = [ArgTerm1 | _],
            ArgTerm1 = term.variable(_, _)
        then
            VarStr = describe_error_term(VarSet, ArgTerm1),
            VarPieces = [words("such as"), quote(VarStr)]
        else
            VarPieces = []
        ),
        require_complete_switch [Kind]
        (
            Kind = vtk_type_decl_pred(IsInClass),
            (
                IsInClass = decl_is_not_in_class,
                WhatPieces = [words("a predicate")]
            ;
                IsInClass = decl_is_in_class,
                WhatPieces = [words("a type class predicate method")]
            )
        ;
            Kind = vtk_type_decl_func(IsInClass),
            (
                IsInClass = decl_is_not_in_class,
                WhatPieces = [words("a function")]
            ;
                IsInClass = decl_is_in_class,
                WhatPieces = [words("a type class function method")]
            )
        ;
            Kind = vtk_mode_decl_pred(IsInClass),
            (
                IsInClass = decl_is_not_in_class,
                WhatPieces = [words("a mode for a predicate")]
            ;
                IsInClass = decl_is_in_class,
                WhatPieces = [words("a mode for a type class predicate method")]
            )
        ;
            Kind = vtk_mode_decl_func(IsInClass),
            (
                IsInClass = decl_is_not_in_class,
                WhatPieces = [words("a mode for a function")]
            ;
                IsInClass = decl_is_in_class,
                WhatPieces = [words("a mode for a type class function method")]
            )
        ;
            Kind = vtk_class_decl,
            WhatPieces = [words("a type class")]
        ;
            Kind = vtk_instance_decl,
            WhatPieces = [words("an instance for a type class")]
        ;
            Kind = vtk_clause_pred,
            WhatPieces = [words("a clause for a predicate")]
        ;
            Kind = vtk_clause_func,
            WhatPieces = [words("a clause for a function")]
        ),
        Pieces = [words("Error: you cannot declare")] ++ WhatPieces ++
            [words("whose name is a variable")] ++ VarPieces ++
            [suffix("."), nl],
        Spec = simplest_spec(severity_error, phase_term_to_parse_tree,
            TermContext, Pieces)
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- func in_pred_or_func_decl_desc(pred_or_func) = string.

in_pred_or_func_decl_desc(pf_function) = "in function declaration".
in_pred_or_func_decl_desc(pf_predicate) = "in predicate declaration".

:- func pred_or_func_decl_pieces(pred_or_func) = list(format_component).

pred_or_func_decl_pieces(pf_function) =
    [decl("func"), words("declaration")].
pred_or_func_decl_pieces(pf_predicate) =
    [decl("pred"), words("declaration")].

%---------------------------------------------------------------------------%
%
% You can uncomment this section for debugging.
%

% :- interface.
%
% :- pred write_item_to_stream(io.output_stream::in, item::in, io::di, io::uo)
%     is det.
%
% :- pred write_item_to_stdout(item::in, io::di, io::uo) is det.
%
% :- pred write_items_to_file(string::in, list(item)::in, io::di, io::uo)
%     is det.
%
% :- implementation.
%
% :- import_module pretty_printer.
%
% write_item_to_stream(Stream, Item, !IO) :-
%     write_doc(Stream, format(Item), !IO),
%     io.nl(Stream, !IO).
%
% write_item_to_stdout(Item, !IO) :-
%     write_item_to_stream(io.stdout_stream, Item, !IO).
%
% write_items_to_file(FileName, Items, !IO) :-
%     io.open_output(FileName, Result, !IO),
%     (
%         Result = ok(Stream),
%         list.foldl(write_item_to_stream(Stream), Items, !IO)
%     ;
%         Result = error(_)
%     ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_item.
%---------------------------------------------------------------------------%
