%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that parse the names of modes, insts
% and determinisms.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_io_inst_mode_name.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%

:- type allow_constrained_inst_var
    --->    allow_constrained_inst_var
    ;       no_allow_constrained_inst_var.

    % XXX These predicates should take a ContextPieces argument, be det,
    % and return a maybe1(...) result.
    %
:- pred convert_mode_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_mode)::out) is semidet.
:- pred convert_mode(allow_constrained_inst_var::in, term::in, mer_mode::out)
    is semidet.

:- pred is_known_mode_name(string::in) is semidet.

%---------------------------------------------------------------------------%

:- pred convert_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_inst)::out) is semidet.

:- pred convert_inst(allow_constrained_inst_var::in, term::in, mer_inst::out)
    is semidet.

:- pred is_known_inst_name(string::in) is semidet.

%---------------------------------------------------------------------------%

:- pred standard_det(string::in, determinism::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module set.

%---------------------------------------------------------------------------%

convert_mode_list(_, [], []).
convert_mode_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_mode(AllowConstrainedInstVar, H0, H),
    convert_mode_list(AllowConstrainedInstVar, T0, T).

convert_mode(AllowConstrainedInstVar, Term, Mode) :-
    Term = term.functor(TermFunctor, TermArgs, _),
    % The is_known_mode_name predicate should succeed for exactly
    % the set of functor atoms recognized here.
    ( if
        TermFunctor = term.atom(">>")
    then
        TermArgs = [InstTermA, InstTermB],
        convert_inst(AllowConstrainedInstVar, InstTermA, InstA),
        convert_inst(AllowConstrainedInstVar, InstTermB, InstB),
        Mode = (InstA -> InstB)
    else if
        TermFunctor = term.atom("is")
    then
        TermArgs = [BeforeIsTerm, DetTerm],
        convert_higher_order_mode(AllowConstrainedInstVar,
            BeforeIsTerm, DetTerm, Mode)
    else
        % If the sym_name_and_args fails, we should report the error
        % (we would need to call parse_qualified_term instead).
        try_parse_sym_name_and_args_from_f_args(TermFunctor, TermArgs,
            Name, Args),
        convert_inst_list(AllowConstrainedInstVar, Args, ConvertedArgs),
        Mode = user_defined_mode(Name, ConvertedArgs)
    ).

is_known_mode_name(">>").
is_known_mode_name("is").

:- pred convert_higher_order_mode(allow_constrained_inst_var::in,
    term::in, term::in, mer_mode::out) is semidet.

convert_higher_order_mode(AllowConstrainedInstVar, BeforeIsTerm, DetTerm,
        Mode) :-
    BeforeIsTerm =
        term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
    (
        ( BeforeIsFunctor = "pred"
        ; BeforeIsFunctor = "any_pred"
        ),
        % XXX We should improve switch detection to make this duplication
        % unnecessary.
        (
            BeforeIsFunctor = "pred",
            % Handle higher-order predicate modes:
            % a mode of the form
            %   pred(<Mode1>, <Mode2>, ...) is <Det>
            % is an abbreviation for the inst mapping
            %   (  pred(<Mode1>, <Mode2>, ...) is <Det>
            %   -> pred(<Mode1>, <Mode2>, ...) is <Det>
            %   )
            IsAny = no
        ;
            BeforeIsFunctor = "any_pred",
            % Handle higher-order predicate modes:
            % a mode of the form
            %   any_pred(<Mode1>, <Mode2>, ...) is <Det>
            % is an abbreviation for the inst mapping
            %   (  any_pred(<Mode1>, <Mode2>, ...) is <Det>
            %   -> any_pred(<Mode1>, <Mode2>, ...) is <Det>
            %   )
            IsAny = yes
        ),
        convert_mode_list(AllowConstrainedInstVar, BeforeIsArgTerms, ArgModes),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        PredInstInfo = pred_inst_info(pf_predicate, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(PredInstInfo))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(PredInstInfo))
        ),
        Mode = (Inst -> Inst)
    ;
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncTerm, RetModeTerm],
        FuncTerm = term.functor(term.atom(FuncTermFunctor), ArgModesTerms, _),
        (
            FuncTermFunctor = "func",
            % Handle higher-order function modes:
            % a mode of the form
            %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            % is an abbreviation for the inst mapping
            %   (  func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   -> func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   )
            IsAny = no
        ;
            FuncTermFunctor = "any_func",
            % Handle higher-order function modes:
            % a mode of the form
            %   any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            % is an abbreviation for the inst mapping
            %   (  any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   -> any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   )
            IsAny = yes
        ),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list.append(ArgModes0, [RetMode], ArgModes),
        FuncInstInfo = pred_inst_info(pf_function, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(FuncInstInfo))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(FuncInstInfo))
        ),
        Mode = (Inst -> Inst)
    ).

%---------------------------------------------------------------------------%

convert_inst_list(_, [], []).
convert_inst_list(AllowConstrainedInstVar, [Term | Terms], [Inst | Insts]) :-
    convert_inst(AllowConstrainedInstVar, Term, Inst),
    convert_inst_list(AllowConstrainedInstVar, Terms, Insts).

convert_inst(AllowConstrainedInstVar, Term, Inst) :-
    (
        Term = term.variable(V0, _),
        term.coerce_var(V0, V),
        Inst = inst_var(V)
    ;
        Term = term.functor(Functor, Args0, _Context),
        Functor = term.atom(Name),
        % XXX It would be nice if we could merge the switch on Name inside
        % is_simple_builtin_inst with the explicit switch below, *without*
        % duplicating the code of is_simple_builtin_inst.
        ( if is_known_inst_name_args(Name, Args0, KnownInstKind) then
            (
                KnownInstKind = known_inst_bad_arity,
                fail
            ;
                KnownInstKind = known_inst_simple(Inst)
            ;
                KnownInstKind = known_inst_compound(CompoundInstKind),
                (
                    CompoundInstKind = kcik_is(BeforeIsTerm, DetTerm),
                    convert_higher_order_inst(AllowConstrainedInstVar,
                        BeforeIsTerm, DetTerm, Inst)
                ;
                    CompoundInstKind = kcik_bound(DisjTerm),
                    parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                        shared, Inst)
                ;
                    CompoundInstKind = kcik_unique(DisjTerm),
                    parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                        unique, Inst)
                ;
                    CompoundInstKind = kcik_mostly_unique(DisjTerm),
                    parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                        mostly_unique, Inst)
                ;
                    CompoundInstKind = kcik_constrained(VarTerm, SubInstTerm),
                    AllowConstrainedInstVar = allow_constrained_inst_var,
                    VarTerm = term.variable(Var, _),
                    % Do not allow nested constrained_inst_vars.
                    convert_inst(no_allow_constrained_inst_var, SubInstTerm,
                        SubInst),
                    Inst = constrained_inst_vars(set.make_singleton_set(
                        term.coerce_var(Var)), SubInst)
                )
            )
        else
            % Anything else must be a user-defined inst.
            try_parse_sym_name_and_args_from_f_args(Functor, Args0,
                QualifiedName, Args1),
            ( if
                BuiltinModule = mercury_public_builtin_module,
                sym_name_get_module_name_default(QualifiedName, unqualified(""),
                    BuiltinModule),
                % If the term is qualified with the `builtin' module,
                % then it may be one of the simple builtin insts.
                UnqualifiedName = unqualify_name(QualifiedName),
                is_known_inst_name_args(UnqualifiedName, Args1, KnownInstKind),
                KnownInstKind = known_inst_simple(InstPrime),

                % However, if the inst is a user_inst defined inside
                % the `builtin' module, then we need to make sure it is
                % properly module-qualified.
                InstPrime \= defined_inst(user_inst(_, _))
            then
                Inst = InstPrime
            else
                convert_inst_list(AllowConstrainedInstVar, Args1, Args),
                Inst = defined_inst(user_inst(QualifiedName, Args))
            )
        )
    ).

:- type known_compound_inst_kind(T)
    --->    kcik_is(T, T)
    ;       kcik_constrained(T, T)
    ;       kcik_bound(T)
    ;       kcik_unique(T)
    ;       kcik_mostly_unique(T).

:- type known_inst_kind(T)
    --->    known_inst_simple(mer_inst)
    ;       known_inst_compound(known_compound_inst_kind(T))
    ;       known_inst_bad_arity.

is_known_inst_name(Name) :-
    is_known_inst_name_args(Name, [] : list(mer_inst), _).

    % is_known_inst_name_args(InstName, InstArgs, KnownInstKind):
    %
    % If Name is a known inst name and Name(InstArgs) is a valid inst
    % structure, then return its kind in KnownInstKind.
    %
    % If Name is a known inst name but Name(InstArgs) is NOT a valid inst
    % structure, then return known_inst_bad_arity in KnownInstKind.
    %
    % If Name is not a known inst name, fail.
    %
:- pred is_known_inst_name_args(string::in, list(T)::in,
    known_inst_kind(T)::out) is semidet.

is_known_inst_name_args(Name, Args, KnownInst) :-
    (
        % Known insts which are always simple.
        (
            Name = "free",
            SimpleInst = free
        ;
            Name = "ground",
            SimpleInst = ground(shared, none_or_default_func)
        ;
            Name = "clobbered",
            SimpleInst = ground(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered",
            SimpleInst = ground(mostly_clobbered, none_or_default_func)
        ;
            Name = "any",
            SimpleInst = any(shared, none_or_default_func)
        ;
            Name = "unique_any",
            SimpleInst = any(unique, none_or_default_func)
        ;
            Name = "mostly_unique_any",
            SimpleInst = any(mostly_unique, none_or_default_func)
        ;
            Name = "clobbered_any",
            SimpleInst = any(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered_any",
            SimpleInst = any(mostly_clobbered, none_or_default_func)
        ;
            Name = "not_reached",
            SimpleInst = not_reached
        ),
        (
            Args = [],
            KnownInst = known_inst_simple(SimpleInst)
        ;
            Args = [_ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "unique",
        (
            Args = [],
            KnownInst = known_inst_simple(ground(unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "mostly_unique",
        (
            Args = [],
            KnownInst = known_inst_simple(
                ground(mostly_unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_mostly_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "is",
        (
            ( Args = []
            ; Args = [_]
            ),
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_is(Arg1, Arg2))
        ;
            Args = [_, _, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "=<",
        (
            ( Args = []
            ; Args = [_]
            ),
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_constrained(Arg1, Arg2))
        ;
            Args = [_, _, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "bound",
        (
            Args = [],
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_bound(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "bound_unique",
        % `bound_unique' is for backwards compatibility - use `unique'
        % instead.
        (
            Args = [],
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ).

:- pred convert_higher_order_inst(allow_constrained_inst_var::in,
    term::in, term::in, mer_inst::out) is semidet.

convert_higher_order_inst(AllowConstrainedInstVar, BeforeIsTerm, DetTerm,
        Inst) :-
    BeforeIsTerm =
        term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
    (
        ( BeforeIsFunctor = "pred"
        ; BeforeIsFunctor = "any_pred"
        ),
        (
            BeforeIsFunctor = "pred",
            % The syntax for a ground higher-order pred inst is
            %
            %   pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = no
        ;
            BeforeIsFunctor = "any_pred",
            % The syntax for an `any' higher-order pred inst is
            %
            %   any_pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = yes
        ),
        convert_mode_list(AllowConstrainedInstVar, BeforeIsArgTerms, ArgModes),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        PredInst = pred_inst_info(pf_predicate, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(PredInst))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(PredInst))
        )
    ;
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncTerm, RetModeTerm],
        FuncTerm = term.functor(term.atom(FuncTermFunctor), ArgModesTerms, _),
        (
            FuncTermFunctor = "func",
            % The syntax for a ground higher-order func inst is
            %
            %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = no
        ;
            FuncTermFunctor = "any_func",
            % The syntax for an `any' higher-order func inst is
            %
            %   any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = yes
        ),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list.append(ArgModes0, [RetMode], ArgModes),
        FuncInst = pred_inst_info(pf_function, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(FuncInst))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(FuncInst))
        )
    ).

:- pred parse_bound_inst_list(allow_constrained_inst_var::in, term::in,
    uniqueness::in, mer_inst::out) is semidet.

parse_bound_inst_list(AllowConstrainedInstVar, Disj, Uniqueness, Inst) :-
    disjunction_to_list(Disj, Disjuncts),
    convert_bound_inst_list(AllowConstrainedInstVar, Disjuncts, Functors0),
    list.sort(Functors0, Functors),
    % Check that the list doesn't specify the same functor twice.
    not (
        list.append(_, SubList, Functors),
        SubList = [F1, F2 | _],
        F1 = bound_functor(ConsId, _),
        F2 = bound_functor(ConsId, _)
    ),
    Inst = bound(Uniqueness, inst_test_no_results, Functors).

:- pred convert_bound_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(bound_inst)::out) is semidet.

convert_bound_inst_list(_, [], []).
convert_bound_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_bound_inst(AllowConstrainedInstVar, H0, H),
    convert_bound_inst_list(AllowConstrainedInstVar, T0, T).

:- pred convert_bound_inst(allow_constrained_inst_var::in, term::in,
    bound_inst::out) is semidet.

convert_bound_inst(AllowConstrainedInstVar, InstTerm, BoundInst) :-
    InstTerm = term.functor(Functor, Args0, _),
    require_complete_switch [Functor]
    (
        Functor = term.atom(_),
        try_parse_sym_name_and_args_from_f_args(Functor, Args0,
            SymName, Args1),
        list.length(Args1, Arity),
        ConsId = cons(SymName, Arity, cons_id_dummy_type_ctor)
    ;
        Functor = term.implementation_defined(_),
        % Implementation-defined literals should not appear in inst
        % definitions.
        fail
    ;
        ( Functor = term.integer(_)
        ; Functor = term.big_integer(_, _)
        ; Functor = term.float(_)
        ; Functor = term.string(_)
        ),
        Args1 = Args0,
        list.length(Args1, Arity),
        make_functor_cons_id(Functor, Arity, ConsId)
    ),
    convert_inst_list(AllowConstrainedInstVar, Args1, Args),
    BoundInst = bound_functor(ConsId, Args).

%---------------------------------------------------------------------------%

standard_det("det",       detism_det).
standard_det("cc_nondet", detism_cc_non).
standard_det("cc_multi",  detism_cc_multi).
standard_det("nondet",    detism_non).
standard_det("multi",     detism_multi).
standard_det("multidet",  detism_multi).
standard_det("semidet",   detism_semi).
standard_det("erroneous", detism_erroneous).
standard_det("failure",   detism_failure).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_inst_mode_name.
%---------------------------------------------------------------------------%
