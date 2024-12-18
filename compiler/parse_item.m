%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014, 2016-2024 The Mercury team.
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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module maybe.
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
    % to an error1() wrapped around an appropriate set of error messages.
    %
:- pred parse_item_or_marker(module_name::in, varset::in, term::in,
    item_seq_num::in, maybe1(item_or_marker)::out) is det.

    % parse_clause_term(MaybeDefaultModuleName, VarSet, Term, SeqNum,
    %   MaybeItemOrMarker):
    %
    % The part of parse_item_or_marker that parses clauses. Implicit
    % qualification happens only if the caller passes a module name
    % in the first argument.
    %
    % Exported for use by parse_class.m.
    %
:- pred parse_clause_term(maybe(module_name)::in, varset::in, term::in,
    item_seq_num::in, maybe1(item_clause_info)::out) is det.

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
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_defn.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_type_repn.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_parse_tree.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module pretty_printer.
:- import_module string.
:- import_module term_int.

%---------------------------------------------------------------------------%

parse_item_or_marker(ModuleName, VarSet, Term, SeqNum, MaybeIOM) :-
    ( if Term = term.functor(term.atom(":-"), [DeclTerm], _DeclContext) then
        parse_decl_term_item_or_marker(ModuleName, VarSet, DeclTerm,
            SeqNum, MaybeIOM)
    else
        parse_clause_term(yes(ModuleName), VarSet, Term, SeqNum, MaybeClause),
        (
            MaybeClause = ok1(ItemClause),
            MaybeIOM = ok1(iom_item(item_clause(ItemClause)))
        ;
            MaybeClause = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ).

%---------------------------------------------------------------------------%

:- pred parse_decl_term_item_or_marker(module_name::in, varset::in, term::in,
    item_seq_num::in, maybe1(item_or_marker)::out) is det.
:- pragma inline(pred(parse_decl_term_item_or_marker/5)).

parse_decl_term_item_or_marker(ModuleName, VarSet, DeclTerm,
        SeqNum, MaybeIOM) :-
    ( if DeclTerm = term.functor(term.atom(Functor), ArgTerms, Context) then
        ( if
            parse_decl_item_or_marker(ModuleName, VarSet, Functor, ArgTerms,
                decl_is_not_in_class, Context, SeqNum, MaybeIOMPrime)
        then
            MaybeIOM = MaybeIOMPrime
        else
            Spec = decl_functor_is_not_valid(Functor, Context),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = decl_is_not_an_atom(VarSet, DeclTerm),
        MaybeIOM = error1([Spec])
    ).

:- func decl_is_not_an_atom(varset, term) = error_spec.

decl_is_not_an_atom(VarSet, Term) = Spec :-
    TermStr = mercury_term_to_string_vs(VarSet, print_name_only, Term),
    Context = get_term_context(Term),
    Pieces = [words("Error:")] ++
        color_as_subject([quote(TermStr)]) ++
        [words("is")] ++
        color_as_incorrect([words("not a valid declaration.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

:- func decl_functor_is_not_valid(string, prog_context) = error_spec.

decl_functor_is_not_valid(Functor, Context) = Spec :-
    Pieces = [words("Error:")] ++
        color_as_subject([quote(Functor)]) ++
        [words("is")] ++
        color_as_incorrect([words("not a valid declaration type.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%---------------------------------------------------------------------------%

:- pred parse_decl_item_or_marker(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in, prog_context::in,
    item_seq_num::in, maybe1(item_or_marker)::out) is semidet.

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
        parse_quant_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
            Context, SeqNum, QuantType, cord.init, cord.init, MaybeIOM)
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
% The supported form of promise_ex declarations is
% ":- all [Vars] promise_ex Goal", and it is parsed by
% parse_attr_decl_item_or_marker.
%
%   ;
%       ( Functor = "promise_exclusive", PromiseType = promise_type_exclusive
%       ; Functor = "promise_exhaustive", PromiseType = promise_type_exhaustive
%       ; Functor = "promise_exclusive_exhaustive",
%           PromiseType = promise_type_exclusive_exhaustive
%       ),
%       UnivQuantVars = [],
%       parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
%           PromiseType, UnivQuantVars, MaybeIOM)
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
    string::in, list(term)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
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
            Pieces = [words("Error:")] ++
                color_as_subject([words("purity annotations")]) ++
                [words("are")] ++
                color_as_incorrect([words("not allowed")]) ++
                [words("on mode declarations."), nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
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
    ;
        ( Functor = "promise_exclusive", PromiseType = promise_type_exclusive
        ; Functor = "promise_exhaustive", PromiseType = promise_type_exhaustive
        ; Functor = "promise_exclusive_exhaustive",
            PromiseType = promise_type_exclusive_exhaustive
        ),
        parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
            PromiseType, PurityAttrs0, QuantConstrAttrs0, MaybeIOM)
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(parse_clause_term/5)).

parse_clause_term(MaybeModuleName, VarSet, Term, SeqNum, MaybeClause) :-
    ( if
        Term = term.functor(term.atom("-->"), [DCGHeadTerm, DCGBodyTerm],
            DCGContext)
    then
        % Term is a DCG clause.
        parse_dcg_clause(MaybeModuleName, VarSet, DCGHeadTerm, DCGBodyTerm,
            DCGContext, SeqNum, MaybeClause)
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
        parse_clause(MaybeModuleName, VarSet, HeadTerm, BodyTerm,
            ClauseContext, SeqNum, MaybeClause)
    ).

parse_class_decl(ModuleName, VarSet, Term, MaybeClassMethod) :-
    TermContext = get_term_context(Term),
    parse_attributed_decl(ModuleName, VarSet, Term, decl_is_in_class,
        TermContext, item_no_seq_num, cord.init, cord.init, MaybeIOM),
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
            Pieces = [words("Error: only")] ++
                color_as_correct(
                    [words("pred, func and mode declarations")]) ++
                [words("are")] ++
                color_as_incorrect([words("allowed")]) ++
                [words("in class interfaces."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                TermContext, Pieces),
            MaybeClassMethod = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------e

:- type purity_attr
    --->    purity_attr(purity).

:- type quant_constr_attr
    --->    qca_quant_vars(quantifier_type, term)
    ;       qca_constraint(quantifier_type, term).

:- pred parse_quant_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    quantifier_type::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_quant_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass, Context,
        SeqNum, QuantType, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM) :-
    (
        ArgTerms = [VarsTerm, SubTerm],
        QuantAttr = qca_quant_vars(QuantType, VarsTerm),
        !:QuantConstrAttrs = cord.snoc(!.QuantConstrAttrs, QuantAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: the keyword")] ++
            color_as_subject([quote(Functor)]) ++
            [words("may appear in declarations")] ++
            color_as_incorrect([words("only")]) ++
            [words("to denote the quantification"),
            words("of a list of variables."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_constraint_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    quantifier_type::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_constraint_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
        Context, SeqNum, QuantType, !.PurityAttrs, !.QuantConstrAttrs,
        MaybeIOM) :-
    (
        ArgTerms = [SubTerm, ConstraintsTerm],
        ConstrAttr = qca_constraint(QuantType, ConstraintsTerm),
        !:QuantConstrAttrs = cord.snoc(!.QuantConstrAttrs, ConstrAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: the symbol")] ++
            color_as_subject([quote(Functor)]) ++
            [words("may appear in declarations")] ++
            color_as_incorrect([words("only")]) ++
            [words("to introduce a constraint or"),
            words("a conjunction of constraints."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_purity_attr(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    purity::in, cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_purity_attr(ModuleName, VarSet, Functor, ArgTerms, IsInClass,
        Context, SeqNum, Purity, !.PurityAttrs, !.QuantConstrAttrs,
        MaybeIOM) :-
    (
        ArgTerms = [SubTerm],
        PurityAttr = purity_attr(Purity),
        !:PurityAttrs = cord.snoc(!.PurityAttrs, PurityAttr),
        parse_attributed_decl(ModuleName, VarSet, SubTerm, IsInClass, Context,
            SeqNum, !.PurityAttrs, !.QuantConstrAttrs, MaybeIOM)
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        Pieces = [words("Error: the symbol")] ++
            color_as_subject([quote(Functor)]) ++
            [words("may appear")] ++
            color_as_incorrect([words("only")]) ++
            [words("as an annotation in front of"),
            words("a predicate or function declaration."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_attributed_decl(module_name::in, varset::in, term::in,
    decl_in_class::in, prog_context::in, item_seq_num::in,
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
            Spec = decl_functor_is_not_valid(Functor, FunctorContext),
            MaybeIOM = error1([Spec])
        )
    else
        Spec = decl_is_not_an_atom(VarSet, Term),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_module_marker(list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_module_marker(ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        ArgTerms = [ModuleNameTerm],
        try_parse_sym_name(ModuleNameTerm, ModuleName)
    then
        Marker = iom_marker_module_start(ModuleName, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    else
        Pieces = [words("Error: a")] ++
            color_as_subject([decl("module"), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,"),
                words("which should be a module name.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_end_module_marker(list(term)::in, prog_context::in,
    item_seq_num::in, maybe1(item_or_marker)::out) is det.

parse_end_module_marker(ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if
        ArgTerms = [ModuleNameTerm],
        try_parse_sym_name(ModuleNameTerm, ModuleName)
    then
        Marker = iom_marker_module_end(ModuleName, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    else
        Pieces = [words("Error: an")] ++
            color_as_subject([decl("end_module"), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,"),
                words("which should be a module name.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_section_marker(string::in, list(term)::in,
    prog_context::in, item_seq_num::in, module_section::in,
    maybe1(item_or_marker)::out) is det.

parse_section_marker(Functor, ArgTerms, Context, SeqNum, Section, MaybeIOM) :-
    (
        ArgTerms = [],
        Marker = iom_marker_section(Section, Context, SeqNum),
        MaybeIOM = ok1(Marker)
    ;
        ArgTerms = [_ | _],
        Pieces = [words("Error: an")] ++
            color_as_subject([decl(Functor), words("declaration")]) ++
            color_as_incorrect([words("should have no arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- type incl_imp_use
    --->    iiu_include_module
    ;       iiu_import_module
    ;       iiu_use_module.

:- pred parse_incl_imp_use_items(module_name::in, varset::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
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
    (
        ArgTerms = [ModuleNamesTerm],
        parse_comma_separated_one_or_more(Parser, ModuleNamesTerm,
            MaybeModuleNames),
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
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        (
            ( IIU = iiu_include_module
            ; IIU = iiu_import_module
            ),
            Article = "an"
        ;
            IIU = iiu_use_module,
            Article = "a"
        ),
        Pieces = [words("Error:"), words(Article)] ++
            color_as_subject([decl(Functor), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,"),
                words("which should be a list of"),
                words("one or more module names.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred make_item_include(prog_context::in, item_seq_num::in, module_name::in,
    item_include::out) is det.

make_item_include(Context, SeqNum, ModuleName, Incl) :-
    Incl = item_include(ModuleName, Context, SeqNum).

:- pred make_item_avail_import(prog_context::in, item_seq_num::in,
    module_name::in, item_avail::out) is det.

make_item_avail_import(Context, SeqNum, ModuleName, Avail) :-
    AvailImportInfo = avail_import_info(ModuleName, Context, SeqNum),
    Avail = avail_import(AvailImportInfo).

:- pred make_item_avail_use(prog_context::in, item_seq_num::in,
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
    prog_context::in, item_seq_num::in, maybe_allow_mode_defn::in,
    list(quant_constr_attr)::in, maybe1(item_or_marker)::out) is det.

parse_mode_defn_or_decl_item(ModuleName, VarSet, ArgTerms, IsInClass, Context,
        SeqNum, AllowModeDefn, QuantConstrAttrs, MaybeIOM) :-
    (
        ArgTerms = [SubTerm],
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
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        Pieces = [words("Error: a")] ++
            color_as_subject([decl("mode"), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,")]) ++
            [words("which should be either the definition of a mode,"),
            words("or the declaration of one mode"),
            words("of a predicate or function."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_version_numbers_marker(module_name::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_version_numbers_marker(ModuleName, Functor, ArgTerms,
        Context, _SeqNum, MaybeIOM) :-
    % Currently we use color only in the diagnostics that users
    % can be expected to see in the absence of compiler bugs.
    (
        ArgTerms = [VNTerm, ModuleNameTerm, VersionNumbersTerm],
        ( if term_int.decimal_term_to_int(VNTerm, VN) then
            ( if VN = module_item_version_numbers_version_number then
                ( if try_parse_sym_name(ModuleNameTerm, ModuleName) then
                    recompilation.version.parse_module_item_version_numbers(
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
                    Spec = spec($pred, severity_error, phase_t2pt,
                        get_term_context(ModuleNameTerm), Pieces),
                    MaybeIOM = error1([Spec])
                )
            else
                Pieces = [words("Error: this interface file"),
                    words("was created by an obsolete compiler,")] ++
                    color_as_incorrect([words("so it must be rebuilt.")]) ++
                    [nl],
                Spec = conditional_spec($pred, warn_smart_recompilation, yes,
                    severity_error, phase_t2pt, [msg(Context, Pieces)]),
                MaybeIOM = ok1(iom_handled_error([Spec]))
            )
        else
            Pieces = [words("Error: invalid version number in"),
                decl("version_numbers"), suffix("."), nl],
            VersionNumberContext = get_term_context(VersionNumbersTerm),
            Spec = spec($pred, severity_error, phase_t2pt,
                VersionNumberContext, Pieces),
            MaybeIOM = error1([Spec])
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_]
        ; ArgTerms = [_, _]
        ; ArgTerms = [_, _, _, _ | _]
        ),
        Pieces = [words("Error: a"), decl(Functor), words("declaration"),
            words("should have exactly three arguments,"),
            words("which should be a version number,"),
            words("a module name, and a tuple containing maps"),
            words("from item ids to timestamps."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred parse_clause(maybe(module_name)::in, varset::in, term::in, term::in,
    term.context::in, item_seq_num::in, maybe1(item_clause_info)::out) is det.

parse_clause(MaybeModuleName, VarSet0, HeadTerm, BodyTerm0, Context, SeqNum,
        MaybeClause) :-
    varset.coerce(VarSet0, ProgVarSet0),
    GoalContextPieces = cord.init,
    trace [compile_time(flag("print_parse_goal_input")),
        runtime(env("PRINT_PARSE_GOAL_INPUT")), io(!IO)]
    (
        io.stderr_stream(StdErr, !IO),
        io.nl(StdErr, !IO),
        write_doc(StdErr, pretty_printer.format(BodyTerm0), !IO),
        io.nl(StdErr, !IO)
    ),
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
            HeadContextPieces =
                cord.from_list([words("In equation head:"), nl]),
            (
                MaybeModuleName = no,
                parse_sym_name_and_args(VarSet,
                    HeadContextPieces, FuncHeadTerm, MaybeFunctor)
            ;
                MaybeModuleName = yes(ModuleName),
                parse_implicitly_qualified_sym_name_and_args(ModuleName,
                    VarSet, HeadContextPieces, FuncHeadTerm, MaybeFunctor)
            )
        )
    else
        MaybeFuncResultTerm = no,
        ( if
            is_the_name_a_variable(VarSet0, vtk_clause_pred, HeadTerm, Spec)
        then
            MaybeFunctor = error2([Spec])
        else
            HeadContextPieces =
                cord.from_list([words("In clause head:"), nl]),
            (
                MaybeModuleName = no,
                parse_sym_name_and_args(VarSet,
                    HeadContextPieces, HeadTerm, MaybeFunctor)
            ;
                MaybeModuleName = yes(ModuleName),
                parse_implicitly_qualified_sym_name_and_args(ModuleName,
                    VarSet, HeadContextPieces, HeadTerm, MaybeFunctor)
            )
        )
    ),
    (
        MaybeFunctor = ok2(SymName, ArgTerms0),
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
        trace [compile_time(flag("print_parse_goal_output")),
            runtime(env("PRINT_PARSE_GOAL_OUTPUT")), io(!IO)]
        (
            io.stderr_stream(StdErr, !IO),
            ( if
                unqualify_name(SymName) = "pred_you_want_to_debug"
            then
                (
                    MaybeBodyGoal = ok2(Goal, _),
                    io.nl(StdErr, !IO),
                    io.format(StdErr, "parsed %s/%d:\n",
                        [s(sym_name_to_string(SymName)),
                        i(list.length(ProgArgTerms))], !IO),
                    mercury_format_goal(StdErr, ProgVarSet, 0u, Goal, !IO),
                    io.nl(StdErr, !IO)
                ;
                    MaybeBodyGoal = error2(_),
                    io.format(StdErr, "parsing %s/%d failed\n",
                        [s(sym_name_to_string(SymName)),
                        i(list.length(ProgArgTerms))], !IO)
                )
            else
                true
            )
        ),
        ItemClause = item_clause_info(PredOrFunc, SymName, ProgArgTerms,
            ProgVarSet, MaybeBodyGoal, Context, SeqNum),
        MaybeClause = ok1(ItemClause)
    ;
        MaybeFunctor = error2(FunctorSpecs),
        Specs = FunctorSpecs ++ get_any_errors_warnings2(MaybeBodyGoal),
        MaybeClause = error1(Specs)
    ).

%---------------------------------------------------------------------------%
%
% Parsing ":- pred" and ":- func" declarations.
%

    % parse_pred_or_func_decl parses a predicate or function declaration.
    %
:- pred parse_pred_or_func_decl_item(module_name::in, varset::in,
    string::in, list(term)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    pred_or_func::in, list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_or_func_decl_item(ModuleName, VarSet, Functor, ArgTerms,
        IsInClass, Context, SeqNum, PredOrFunc, PurityAttrs, QuantConstrAttrs,
        MaybeIOM) :-
    (
        ArgTerms = [Term],
        (
            IsInClass = decl_is_in_class,
            PredOrFuncDeclPieces = [words("type class"), p_or_f(PredOrFunc),
                words("method declaration:"), nl]
        ;
            IsInClass = decl_is_not_in_class,
            PredOrFuncDeclPieces =
                [p_or_f(PredOrFunc), words("declaration:"), nl]
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
                Spec = report_with_inst_and_detism(p_or_f(PredOrFunc),
                    BaseTerm),
                MaybeIOM = error1([Spec])
            else if
                WithInst = yes(_),
                WithType = no
            then
                Spec = report_with_inst_no_with_type(PredOrFunc, BaseTerm),
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
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        % Should we mention the determinism? It is allowed only
        % in predicate declarations that specify the modes, so the
        % wording required would probably be more confusing than helpful.
        Pieces = [words("Error: a")] ++
            color_as_subject([decl(Functor), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,")]) ++
            [words("which should specify the types and maybe the modes"),
            words("of the arguments of a"), words(Functor), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

    % parse a `:- pred p(...)' declaration or a
    % `:- func f(...) `with_type` t' declaration
    %
:- pred parse_pred_decl_base(pred_or_func::in, module_name::in, varset::in,
    term::in, maybe(mer_type)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, PredTypeTerm,
        WithType, WithInst, MaybeDetism, IsInClass, Context, SeqNum,
        PurityAttrs, QuantConstrAttrs, MaybeIOM) :-
    ContextPieces = cord.singleton(words("In")) ++
        cord.from_list(pred_or_func_decl_pieces(PredOrFunc)) ++
        cord.from_list([suffix(":"), nl]),
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
                VarSet, ContextPieces, PredTypeTerm, MaybePredNameAndArgs),
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
                parse_types_and_maybe_modes(
                    constrain_some_inst_vars(InstConstraints),
                    do_not_require_tm_mode, wnhii_pred_arg, VarSet,
                    ArgContextFunc, ArgTerms, 1, TypeAndMaybeModeList,
                    [], TMSpecs),
                PredTypeContext = get_term_context(PredTypeTerm),
                check_type_and_maybe_mode_list_is_consistent(
                    TypeAndMaybeModeList, no, PredTypeContext,
                    MaybeTypesAndMaybeModes),
                ( if
                    TMSpecs = [],
                    MaybeTypesAndMaybeModes = ok1(TypesAndMaybeModes)
                then
                    ( if
                        WithInst = yes(_),
                        TypesAndMaybeModes = types_only([_ | _])
                    then
                        Spec = report_with_inst_no_arg_modes(PredOrFunc,
                            PredTypeTerm),
                        MaybeIOM = error1([Spec])
                    else if
                        WithInst = no,
                        WithType = yes(_),
                        TypesAndMaybeModes = types_and_modes([_ | _])
                    then
                        Spec = report_with_type_no_with_inst(PredOrFunc,
                            PredTypeTerm),
                        MaybeIOM = error1([Spec])
                    else
                        varset.coerce(VarSet, TypeVarSet),
                        varset.coerce(VarSet, InstVarSet),
                        (
                            ( TypesAndMaybeModes = no_types_arity_zero
                            ; TypesAndMaybeModes = types_only(_)
                            ),
                            InconsistentVars = []
                        ;
                            TypesAndMaybeModes =
                                types_and_modes(TypesAndModes),
                            inconsistent_constrained_inst_vars_in_tms(
                                TypesAndModes, InconsistentVars)
                        ),
                        (
                            InconsistentVars = [],
                            Origin = item_origin_user,
                            ItemPredDecl = item_pred_decl_info(Functor,
                                PredOrFunc, TypesAndMaybeModes,
                                WithType, WithInst, MaybeDetism, Origin,
                                TypeVarSet, InstVarSet, ExistQVars, Purity,
                                Constraints, Context, SeqNum),
                            Item = item_pred_decl(ItemPredDecl),
                            MaybeIOM = ok1(iom_item(Item))
                        ;
                            InconsistentVars =
                                [HeadInconsistentVar | TailInconsistentVars],
                            report_inconsistent_constrained_inst_vars(
                                in_pred_or_func_decl_desc(PredOrFunc),
                                get_term_context(PredTypeTerm), InstVarSet,
                                HeadInconsistentVar, TailInconsistentVars,
                                Spec),
                            MaybeIOM = error1([Spec])
                        )
                    )
                else
                    Specs = TMSpecs ++
                        get_any_errors1(MaybeTypesAndMaybeModes),
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
    maybe(determinism)::in, decl_in_class::in,
    prog_context::in, item_seq_num::in,
    list(purity_attr)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_func_decl_base(ModuleName, VarSet, Term, MaybeDetism, IsInClass, Context,
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
                    VarSet, ContextPieces, FuncTerm, MaybeFuncNameAndArgs),
                (
                    MaybeFuncNameAndArgs = error2(Specs),
                    MaybeIOM = error1(Specs)
                ;
                    MaybeFuncNameAndArgs = ok2(FuncName, ArgTerms),
                    ArgContextFunc =
                        ( func(ArgNum) = ContextPieces ++
                            cord.from_list([words("in the"), nth_fixed(ArgNum),
                            words("argument:"), nl])
                        ),
                    parse_types_and_maybe_modes(
                        constrain_some_inst_vars(InstConstraints),
                        do_not_require_tm_mode, wnhii_func_arg,
                        VarSet, ArgContextFunc, ArgTerms, 1,
                        ArgTypesAndMaybeModes, [], ArgTMSpecs),
                    RetContextPieces = ContextPieces ++
                        cord.from_list([words("in the return value:"), nl]),
                    parse_type_and_maybe_mode(
                        constrain_some_inst_vars(InstConstraints),
                        do_not_require_tm_mode, wnhii_func_return_arg,
                        VarSet, RetContextPieces, ReturnTerm,
                        MaybeRetTypeAndMaybeMode),
                    ( if
                        ArgTMSpecs = [],
                        MaybeRetTypeAndMaybeMode = ok1(RetTypeAndMaybeMode)
                    then
                        % We use an auxiliary predicate because the code is
                        % just too deeply indented here.
                        parse_func_decl_base_2(FuncName,
                            ArgTypesAndMaybeModes, RetTypeAndMaybeMode,
                            FuncTerm, Term, VarSet, MaybeDetism,
                            ExistQVars, Constraints, Context, SeqNum,
                            PurityAttrs, MaybeIOM)
                    else
                        Specs = ArgTMSpecs ++
                            get_any_errors1(MaybeRetTypeAndMaybeMode),
                        MaybeIOM = error1(Specs)
                    )
                )
            )
        else
            Pieces = [words("Error: this"), decl("func"),
                words("declaration"), words("is")] ++
                color_as_incorrect([words("missing")]) ++
                [words("the")] ++
                color_as_subject([quote("="), words("sign")]) ++
                [words("that should separate the argument types"),
                words("from the return type."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), Pieces),
            MaybeIOM = error1([Spec])
        )
    ).

:- pred parse_func_decl_base_2(sym_name::in, list(type_and_maybe_mode)::in,
    type_and_maybe_mode::in, term::in, term::in, varset::in,
    maybe(determinism)::in, existq_tvars::in, univ_exist_constraints::in,
    prog_context::in, item_seq_num::in,
    list(purity_attr)::in, maybe1(item_or_marker)::out) is det.

parse_func_decl_base_2(FuncName, Args, ReturnArg, FuncTerm, Term,
        VarSet, MaybeDetism, ExistQVars, Constraints, Context, SeqNum,
        PurityAttrs, MaybeIOM) :-
    check_type_and_maybe_mode_list_is_consistent(Args, yes(ReturnArg),
        get_term_context(FuncTerm), MaybeTypesAndMaybeModes),
    get_purity_from_attrs(Context, PurityAttrs, MaybePurity),
    ( if
        MaybeTypesAndMaybeModes = ok1(TypesAndMaybeModes),
        MaybePurity = ok1(Purity)
    then
        varset.coerce(VarSet, TVarSet),
        varset.coerce(VarSet, IVarSet),
        (
            ( TypesAndMaybeModes = no_types_arity_zero
            ; TypesAndMaybeModes = types_only(_)
            ),
            InconsistentVars = []
        ;
            TypesAndMaybeModes = types_and_modes(TypesAndModes),
            inconsistent_constrained_inst_vars_in_tms(TypesAndModes,
                InconsistentVars)
        ),
        (
            InconsistentVars = [],
            Origin = item_origin_user,
            ItemPredDecl = item_pred_decl_info(FuncName, pf_function,
                TypesAndMaybeModes, no, no, MaybeDetism, Origin,
                TVarSet, IVarSet, ExistQVars, Purity, Constraints,
                Context, SeqNum),
            Item = item_pred_decl(ItemPredDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            InconsistentVars = [HeadInconsistentVar | TailInconsistentVars],
            Where = "in function declaration",
            report_inconsistent_constrained_inst_vars(Where,
                get_term_context(Term), IVarSet,
                HeadInconsistentVar, TailInconsistentVars, Spec),
            MaybeIOM = error1([Spec])
        )
    else
        Specs = get_any_errors1(MaybeTypesAndMaybeModes)
            ++ get_any_errors1(MaybePurity),
        MaybeIOM = error1(Specs)
    ).

    % Verify that among the arguments of a :- pred or :- func declaration,
    % either all arguments specify a mode or none of them do. If some do
    % and some don't, return an error message that identifies the argument
    % positions that are missing modes. (If some argument positions have
    % modes, then the programmer probably intended for all of them to have
    % modes.)
    %
:- pred check_type_and_maybe_mode_list_is_consistent(
    list(type_and_maybe_mode)::in, maybe(type_and_maybe_mode)::in,
    term.context::in, maybe1(types_and_maybe_modes)::out) is det.

check_type_and_maybe_mode_list_is_consistent(TypesAndMaybeModes,
        MaybeRetTypeAndMaybeMode, Context, MaybeResult) :-
    classify_type_and_maybe_mode_list(1, TypesAndMaybeModes,
        WithModeArgs0, WithoutModeArgs0),
    (
        MaybeRetTypeAndMaybeMode = no,
        WithModeArgs = WithModeArgs0,
        WithoutModeArgs = WithoutModeArgs0
    ;
        MaybeRetTypeAndMaybeMode = yes(RetTypeAndMaybeMode),
        (
            RetTypeAndMaybeMode = type_only(RetType),
            WithModeArgs = WithModeArgs0,
            WithoutModeArgs = WithoutModeArgs0 ++ [-1 - RetType]
        ;
            RetTypeAndMaybeMode = type_and_mode(RetType, RetMode),
            RetTM = type_and_mode(RetType, RetMode),
            WithModeArgs = WithModeArgs0 ++ [-1 - RetTM],
            WithoutModeArgs = WithoutModeArgs0
        )
    ),
    (
        WithModeArgs = [],
        WithoutModeArgs = [],
        MaybeResult = ok1(no_types_arity_zero)
    ;
        WithModeArgs = [],
        WithoutModeArgs = [_ | _],
        % No arguments have modes; no inconsistency.
        assoc_list.values(WithoutModeArgs, Types),
        MaybeResult = ok1(types_only(Types))
    ;
        WithModeArgs = [_ | _],
        WithoutModeArgs = [],
        % All arguments have modes; no inconsistency.
        assoc_list.values(WithModeArgs, TypesAndModes),
        MaybeResult = ok1(types_and_modes(TypesAndModes))
    ;
        WithModeArgs = [_ | _],
        WithoutModeArgs = [FirstWithout | RestWithout],
        % Some arguments have modes and some don't, which is inconsistent.
        (
            RestWithout = [],
            IdPieces =
                [words("The argument without a mode is the")] ++
                nth_arg_only(FirstWithout) ++
                [nl]
        ;
        RestWithout = [_ | _],
            ( if
                list.last(WithoutModeArgs, LastWithoutModeArgNum - _),
                LastWithoutModeArgNum < 0
            then
                % Separate the last non-return-value arg from the return value
                % with "and the", yielding e.g. "second and the return value".
                And = "and the"
            else
                % Separate the last two args with just "and",
                % yielding e.g. "second and third".
                And = "and"
            ),
            WithoutArgNumPieces = list.map(nth_arg, WithoutModeArgs),
            WithoutArgNumsPieces = piece_list_to_color_pieces(color_hint,
                And, [suffix(".")], WithoutArgNumPieces),
            IdPieces =
                [words("The arguments without modes are the")] ++
                WithoutArgNumsPieces ++
                [nl]
        ),
        Pieces = [words("Error:")] ++
            color_as_incorrect([words("some but not all")]) ++
            [words("arguments have modes."), nl
            | IdPieces],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeResult = error1([Spec])
    ).

:- pred classify_type_and_maybe_mode_list(int::in,
    list(type_and_maybe_mode)::in,
    assoc_list(int, type_and_mode)::out,
    assoc_list(int, mer_type)::out) is det.

classify_type_and_maybe_mode_list(_, [], [], []).
classify_type_and_maybe_mode_list(ArgNum, [Head | Tail],
        WithModeArgs, WithoutModeArgs) :-
    classify_type_and_maybe_mode_list(ArgNum + 1, Tail,
        WithModeArgs0, WithoutModeArgs0),
    (
        Head = type_only(Type),
        WithModeArgs = WithModeArgs0,
        WithoutModeArgs = [ArgNum - Type| WithoutModeArgs0]
    ;
        Head = type_and_mode(Type, Mode),
        WithModeArgs = [ArgNum - type_and_mode(Type, Mode) | WithModeArgs0],
        WithoutModeArgs = WithoutModeArgs0
    ).

:- func nth_arg_only(pair(int, _)) = list(format_piece).

nth_arg_only(Arg) = Pieces :-
    Piece0 = nth_arg(Arg),
    Pieces = color_pieces(color_hint, [Piece0, suffix(".")]).

:- func nth_arg(pair(int, _)) = format_piece.

nth_arg(ArgNum - _) = Piece :-
    ( if ArgNum < 0 then
        Piece = words("return value")
    else
        Piece = nth_fixed(ArgNum)
    ).

%---------------------------------------------------------------------------%
%
% Parsing mode declarations for predicates and functions.
%

:- pred parse_mode_decl(module_name::in, varset::in, term::in,
    decl_in_class::in, prog_context::in, item_seq_num::in,
    list(quant_constr_attr)::in, maybe1(item_or_marker)::out) is det.

parse_mode_decl(ModuleName, VarSet, Term, IsInClass, Context, SeqNum,
        QuantConstrAttrs, MaybeIOM) :-
    (
        IsInClass = decl_is_in_class,
        DeclWords = words("type class method mode")
    ;
        IsInClass = decl_is_not_in_class,
        DeclWords = words("mode")
    ),
    DetismContextPieces =
        cord.from_list([words("In"), DeclWords, words("declaration:")]),
    parse_determinism_suffix(VarSet, DetismContextPieces, Term,
        BeforeDetismTerm, MaybeMaybeDetism),
    WithInstContextPieces = cord.from_list([
        words("In the"), quote("with_inst"), words("annotation of a"),
        DeclWords, words("declaration:")]),
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
            Spec = report_with_inst_and_detism(words("mode"), Term),
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
    decl_in_class::in, prog_context::in, item_seq_num::in,
    maybe(mer_inst)::in, maybe(determinism)::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_mode_decl_base(ModuleName, VarSet, Term, IsInClass, Context, SeqNum,
        WithInst, MaybeDetism, QuantConstrAttrs, MaybeIOM) :-
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
                words("declaration:"), nl]),
            parse_implicitly_qualified_sym_name_and_args(ModuleName,
                VarSet, ContextPieces, FuncTerm, MaybeFunctorArgs),
            (
                MaybeFunctorArgs = error2(Specs),
                MaybeIOM = error1(Specs)
            ;
                MaybeFunctorArgs = ok2(Functor, ArgTerms),
                parse_func_mode_decl(Functor, ArgTerms, ModuleName,
                    ReturnTypeTerm, Term, VarSet, MaybeDetism, Context, SeqNum,
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
                words("declaration:"), nl]),
            parse_implicitly_qualified_sym_name_and_args(ModuleName,
                VarSet, ContextPieces, Term, MaybeFunctorArgs),
            (
                MaybeFunctorArgs = error2(Specs),
                MaybeIOM = error1(Specs)
            ;
                MaybeFunctorArgs = ok2(Functor, ArgTerms),
                parse_pred_mode_decl(Functor, ArgTerms, ModuleName, Term,
                    VarSet, WithInst, MaybeDetism,
                    Context, SeqNum, QuantConstrAttrs, MaybeIOM)
            )
        )
    ).

:- pred parse_pred_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, varset::in, maybe(mer_inst)::in, maybe(determinism)::in,
    prog_context::in, item_seq_num::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_pred_mode_decl(Functor, ArgTerms, ModuleName, PredModeTerm, VarSet,
        WithInst, MaybeDetism, Context, SeqNum, QuantConstrAttrs, MaybeIOM) :-
    ArgContextPieces = cord.from_list(
        [words("In the mode declaration of the predicate"),
        unqual_sym_name(Functor), suffix(":"), nl]),
    parse_modes(allow_constrained_inst_var, VarSet, ArgContextPieces,
        ArgTerms, MaybeArgModes0),
    ContextPieces = cord.from_list([words("In predicate"), decl("mode"),
        words("declaration:"), nl]),
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
        (
            InconsistentVars = [],
            (
                WithInst = no,
                MaybePredOrFunc = yes(pf_predicate)
            ;
                WithInst = yes(_),
                % We don't know whether it is a predicate or a function
                % until we expand out the inst.
                MaybePredOrFunc = no
            ),
            ItemModeDecl = item_mode_decl_info(Functor, MaybePredOrFunc,
                ArgModes, WithInst, MaybeDetism, InstVarSet, Context, SeqNum),
            Item = item_mode_decl(ItemModeDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            InconsistentVars = [HeadInconsistentVar | TailInconsistentVars],
            Where = "in predicate mode declaration",
            report_inconsistent_constrained_inst_vars(Where,
                get_term_context(PredModeTerm), InstVarSet,
                HeadInconsistentVar, TailInconsistentVars, Spec),
            MaybeIOM = error1([Spec])
        )
    else
        Specs = get_any_errors1(MaybeArgModes0)
            ++ get_any_errors3(MaybeConstraints),
        MaybeIOM = error1(Specs)
    ).

:- pred parse_func_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, term::in, varset::in, maybe(determinism)::in,
    prog_context::in, item_seq_num::in, list(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_func_mode_decl(Functor, ArgTerms, ModuleName, RetModeTerm, FullTerm,
        VarSet, MaybeDetism, Context, SeqNum, QuantConstrAttrs, MaybeIOM) :-
    ArgContextPieces = cord.from_list(
        [words("In the mode declaration of the function"),
        unqual_sym_name(Functor), suffix(":"), nl]),
    parse_modes(allow_constrained_inst_var, VarSet, ArgContextPieces,
        ArgTerms, MaybeArgModes0),
    RetContextPieces = cord.from_list([words("In the return value"),
        words("of the mode declaration of the function"),
        unqual_sym_name(Functor), suffix(":"), nl]),
    parse_mode(allow_constrained_inst_var, VarSet, RetContextPieces,
        RetModeTerm, MaybeRetMode0),
    QuantContextPieces = cord.from_list([words("In function"), decl("mode"),
        words("declaration:"), nl]),
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
        (
            InconsistentVars = [],
            ItemModeDecl = item_mode_decl_info(Functor,
                yes(pf_function), ArgReturnModes, no, MaybeDetism,
                InstVarSet, Context, SeqNum),
            Item = item_mode_decl(ItemModeDecl),
            MaybeIOM = ok1(iom_item(Item))
        ;
            InconsistentVars = [HeadInconsistentVar | TailInconsistentVars],
            report_inconsistent_constrained_inst_vars(
                "in function mode declaration", get_term_context(FullTerm),
                InstVarSet, HeadInconsistentVar, TailInconsistentVars, Spec),
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
        Pieces = [words("Error:")] ++
            color_as_subject([words("duplicate purity annotations")]) ++
            color_as_incorrect([words("are not allowed.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
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
    varset::in, list(quant_constr_attr)::in, cord(format_piece)::in,
    maybe3(existq_tvars, univ_exist_constraints, inst_var_sub)::out) is det.

get_class_context_and_inst_constraints_from_attrs(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, MaybeExistClassInstContext) :-
    % When we reach here, QuantConstrAttrs contains declaration attributes
    % in outermost to innermost order.
    %
    % Constraints and quantifiers should occur in the following order,
    % outermost to innermost:
    %
    %                               operator        precedence
    %                               --------        ----------
    %   1. universal quantifiers    all             550
    %   2. existential quantifiers  some            550
    %   3. universal constraints    <=              580
    %   4. existential constraints  =>              580 [*]
    %   5. the decl itself          pred or func    700
    %
    % [*] Note that the semantic meaning of `=>' is not quite the same
    % as implication; logically speaking, it is more like conjunction.
    % Oh well, at least it has the right precedence.
    %
    % In theory, it could make sense to allow the order of 2 & 3 to be
    % swapped, or (in the case of multiple constraints and multiple
    % quantifiers) to allow arbitrary interleaving of 2 & 3, but in practice
    % it seems there would be little benefit in allowing that flexibility,
    % so we don't.
    %
    % NOTE We do NOT check that the order above is actually followed.
    %
    % Universal quantification is the default, so we just ignore
    % universal quantifiers. (XXX It might be a good idea to check that
    % any universally quantified type variables (a) do actually occur SOMEWHERE
    % in the type declaration, and (2) are not also existentially quantified.
    % If either of these checks fail, we should issue an error message.)

    get_class_context_and_inst_constraints_loop(ModuleName, VarSet,
        QuantConstrAttrs, ContextPieces, [], Specs,
        cord.init, _UnivQVarsCord, cord.init, ExistQVarsCord,
        cord.init, UnivClassConstraints, map.init, UnivInstConstraints,
        cord.init, ExistClassConstraints, map.init, ExistInstConstraints),

    ExistQVars0 = cord.list(ExistQVarsCord),
    list.map(term.coerce_var, ExistQVars0, ExistQVars),
    (
        Specs = [],
        ClassConstraints = univ_exist_constraints(
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
    varset::in, list(quant_constr_attr)::in, cord(format_piece)::in,
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
        parse_and_check_quant_vars(QuantType, type_var, ContextPieces,
            VarSet, VarsTerm, MaybeVars),
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
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

parse_promise_item(VarSet, ArgTerms, Context, SeqNum, MaybeIOM) :-
    ( if ArgTerms = [Term] then
        varset.coerce(VarSet, ProgVarSet0),
        ContextPieces = cord.init,
        parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
        (
            MaybeGoal0 = ok2(Goal0, GoalWarningSpecs),
            (
                GoalWarningSpecs = [],
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
                ItemPromise = item_promise_info(promise_type_true, Goal,
                    ProgVarSet, UnivVars, Context, SeqNum),
                Item = item_promise(ItemPromise),
                MaybeIOM = ok1(iom_item(Item))
            ;
                GoalWarningSpecs = [_ | _],
                % We *could* try to preserve any warnings for code
                % inside Goal0, and add the promise to the parse tree
                % for later addition to the HLDS even in the presence
                % of such warnings, but there doesn't seem to be any point
                % in doing that, because at the moment, the only kind
                % of construct that generates warning_specs is a
                % disable_warnings scope, and those should NOT be appearing
                % in any promise.
                MaybeIOM = error1(GoalWarningSpecs)
            )
        ;
            MaybeGoal0 = error2(Specs),
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a")] ++
            color_as_subject([decl("promise"), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,"),
                words("which should be a goal.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_promise_ex_item(varset::in, string::in, list(term)::in,
    prog_context::in, item_seq_num::in, promise_type::in,
    cord(purity_attr)::in, cord(quant_constr_attr)::in,
    maybe1(item_or_marker)::out) is det.

parse_promise_ex_item(VarSet, Functor, ArgTerms, Context, SeqNum,
        PromiseType, PurityAttrCord, QuantConstrAttrCord, MaybeIOM) :-
    ( if ArgTerms = [Term] then
        PurityAttrs = cord.list(PurityAttrCord),
        (
            PurityAttrs = [],
            PuritySpecs = []
        ;
            PurityAttrs = [_ | _],
            PurityPieces =
                [words("Error: a")] ++
                color_as_subject([decl(Functor), words("declaration")]) ++
                color_as_incorrect([words("may not have")]) ++
                [words("a purity attribute."), nl],
            PuritySpec = spec($pred, severity_error, phase_t2pt,
                Context, PurityPieces),
            PuritySpecs = [PuritySpec]
        ),
        QuantConstrAttrs = cord.list(QuantConstrAttrCord),
        ContextPieces = cord.from_list([words("In"), words(Functor),
            words("declaration:"), nl]),
        ( if
            QuantConstrAttrs = [QuantConstrAttr],
            QuantConstrAttr = qca_quant_vars(quant_type_univ, VarsTerm)
        then
            parse_and_check_quant_vars(quant_type_univ, ordinary_var,
                ContextPieces, VarSet, VarsTerm, MaybeUnivVars)
        else
            Form = ":- all [<vars>] " ++ Functor ++ " ( <disjunction> )",
            UnivVarsPieces =
                [words("Error: a")] ++
                color_as_subject([decl(Functor), words("declaration")]) ++
                color_as_incorrect([words("must have the form")]) ++
                [nl_indent_delta(1)] ++
                color_as_correct([quote(Form), suffix(".")]) ++
                [nl_indent_delta(-1)],
            UnivVarsSpec = spec($pred, severity_error, phase_t2pt,
                Context, UnivVarsPieces),
            MaybeUnivVars = error1([UnivVarsSpec])
        ),
        varset.coerce(VarSet, ProgVarSet0),
        parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
        ( if
            PuritySpecs = [],
            MaybeUnivVars = ok1(UnivVars),
            MaybeGoal0 = ok2(Goal, GoalWarningSpecs),
            % We *could* try to preserve any warnings for code inside Goal,
            % and add the promise to the parse tree for later addition
            % to the HLDS even in the presence of such warnings, but
            % there doesn't seem to be any point in doing that, because
            % at the moment, the only kind of construct that generates
            % warning_specs is a disable_warnings scope, and those
            % should NOT be appearing in any promise.
            GoalWarningSpecs = []
        then
            UnivProgVars = list.map(term.coerce_var, UnivVars),
            ItemPromise = item_promise_info(PromiseType, Goal, ProgVarSet,
                UnivProgVars, Context, SeqNum),
            Item = item_promise(ItemPromise),
            MaybeIOM = ok1(iom_item(Item))
        else
            ( MaybeGoal0 = ok2(_, GoalSpecs)
            ; MaybeGoal0 = error2(GoalSpecs)
            ),
            Specs = PuritySpecs ++ get_any_errors1(MaybeUnivVars) ++ GoalSpecs,
            MaybeIOM = error1(Specs)
        )
    else
        Pieces = [words("Error: a")] ++
            color_as_subject([decl(Functor), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,"),
                words("which should be a goal.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
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
:- pred parse_determinism_suffix(varset::in, cord(format_piece)::in,
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
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected a")] ++
                color_as_correct([words("determinism,")]) ++
                [words("got")] ++
                color_as_incorrect([quote(DetismTermStr), suffix(".")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt,
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
            words("annotation:"), nl]),
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
:- pred parse_with_inst_suffix(varset::in, cord(format_piece)::in,
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
    parse_sym_name(VarSet, Term, MaybeModuleName).

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
        % XXX Should we add a warning about such module names
        % being a really bad idea?
        Pieces = [words("Error:")] ++
            color_as_subject([words("module names starting with"),
                words("capital letters")]) ++
            color_as_incorrect(
                [words("must be quoted using single quotes")]) ++
            [words("(e.g. "":- module 'Foo'."")."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeModule = error1([Spec])
    ;
        Term = term.functor(_, _, _),
        parse_implicitly_qualified_sym_name(DefaultModuleName, VarSet,
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
            VarDotPieces = [words("such as")] ++
                color_as_subject([quote(VarStr), suffix(".")])
        else
            VarDotPieces = [suffix(".")]
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
                WhatPieces =
                    [words("a mode for a type class predicate method")]
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
            [words("whose name is a")] ++
            color_as_incorrect([words("variable")]) ++
            VarDotPieces ++ [nl],
        Spec = spec($pred, severity_error, phase_t2pt, TermContext, Pieces)
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- func report_with_inst_and_detism(format_piece, term(T)) = error_spec.

report_with_inst_and_detism(DeclKindPiece, Term) = Spec :-
    Pieces = [words("Error: a"), DeclKindPiece, words("declaration"),
        words("that specifies a")] ++
        color_as_inconsistent([words("determinism")]) ++
        color_as_incorrect([words("may not also specify")]) ++
        [words("a")] ++
        color_as_inconsistent([quote("with_inst"), words("annotation.")]) ++
        [words("This is because the"), quote("with_inst"),
        words("annotation itself also specifies a determinism."), nl],
    Context = get_term_context(Term),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

:- func report_with_inst_no_arg_modes(pred_or_func, term(T)) = error_spec.

report_with_inst_no_arg_modes(PredOrFunc, Term) = Spec :-
    Pieces = [words("Error: a"), p_or_f(PredOrFunc), words("declaration"),
        words("that does not specify")] ++
        color_as_inconsistent([words("argument modes")]) ++
        color_as_incorrect([words("may not specify")]) ++
        [words("a")] ++
        color_as_inconsistent([quote("with_inst"), words("annotation.")]),
    Context = get_term_context(Term),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

:- func report_with_inst_no_with_type(pred_or_func, term(T)) = error_spec.

report_with_inst_no_with_type(PredOrFunc, Term) = Spec :-
    % Keep as similar to report_with_type_no_with_inst as possible.
    Pieces = [words("Error: a"), p_or_f(PredOrFunc), words("declaration"),
        words("that specifies a")] ++
        color_as_inconsistent([quote("with_inst"), words("annotation")]) ++
        color_as_incorrect([words("must also specify")]) ++
        [words("a")] ++
        color_as_inconsistent([quote("with_type"), words("annotation")]) ++
        [words("to specify the types of the arguments it adds."), nl],
    Context = get_term_context(Term),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

:- func report_with_type_no_with_inst(pred_or_func, term(T)) = error_spec.

report_with_type_no_with_inst(PredOrFunc, Term) = Spec :-
    % Keep as similar to report_with_inst_no_with_type as possible.
    Pieces = [words("Error: a"), p_or_f(PredOrFunc), words("declaration"),
        words("that specifies argument modes and also has a")] ++
        color_as_inconsistent([quote("with_type"), words("annotation")]) ++
        color_as_incorrect([words("must also include")]) ++
        [words("a")] ++
        color_as_inconsistent([quote("with_inst"), words("annotation")]) ++
        [words("to specify the modes of the arguments it adds."), nl],
    Context = get_term_context(Term),
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%---------------------------------------------------------------------------%

:- func in_pred_or_func_decl_desc(pred_or_func) = string.

in_pred_or_func_decl_desc(pf_function) = "in function declaration".
in_pred_or_func_decl_desc(pf_predicate) = "in predicate declaration".

:- func pred_or_func_decl_pieces(pred_or_func) = list(format_piece).

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
% :- pred write_item_to_stream(io.text_output_stream::in, item::in,
%     io::di, io::uo) is det.
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
