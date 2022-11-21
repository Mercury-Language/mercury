%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_type_assign.m.
%
% This file contains code both to print out type_assigns/arg_type_assigns,
% and to transform them into other data structures than can help diagnose
% type errors.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_type_assign.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred type_assign_set_msg_to_verbose_component(typecheck_info::in,
    prog_varset::in, type_assign_set::in, error_msg_component::out) is det.

:- pred arg_type_assign_set_msg_to_verbose_component(typecheck_info::in,
    prog_varset::in, args_type_assign_set::in, error_msg_component::out)
    is det.

%---------------------------------------------------------------------------%

:- type type_stuff
    --->    type_stuff(
                type_stuff_base_type            :: mer_type,
                type_stuff_tvarset              :: tvarset,
                type_stuff_binding              :: tsubst,
                type_stuff_existq_tvars         :: list(tvar)
            ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable, removing all duplicates.
    % The check for duplicates makes this algorithm O(N^2), which can be
    % a problem. In addition, the equality test unifications done by
    % list.member compare type_stuffs starting with the base_type field,
    % which (in the extremely limited deep profiling sample consisting
    % of just one run that motivated this comment) always compares equal,
    % unlike the second and third fields of type_stuff.
    %
:- pred get_all_type_stuffs_remove_dups(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.

    % Given a type assignment set and a variable, return the result of
    % applying the given function to the list of the possible different types
    % for the variable. The returned list may contain duplicates.
    %
    % We *could* eliminate duplicates here piecemeal as they are generated,
    % as get_all_type_stuffs_remove_dups does, but that is an quadratic
    % algorithm, and our callers typically call list.sort_and_remove_dups
    % on the result, which removes duplicates at a linear cost over the
    % usually O(N log N) cost of the sorting itself.
    %
    % However, the much bigger win is that each result is typically
    % smaller than the type_stuff it is derived from, because
    %
    % - the tvarsets in type_stuffs are often big, while
    % - the result is a type in some form, whose size is typcally small.
    %
    % And if the results are smaller than the type_stuffs, then comparing
    % should be faster as well.
    %
:- pred get_all_transformed_type_stuffs((func(type_stuff) = T)::in,
    type_assign_set::in, prog_var::in, list(T)::out) is det.

:- type arg_type_stuff
    --->    arg_type_stuff(
                arg_type_stuff_var_type             :: mer_type,
                arg_type_stuff_source               :: args_type_assign_source,
                arg_type_stuff_arg_type             :: mer_type,
                arg_type_stuff_tvarset              :: tvarset,
                arg_type_stuff_existq_tvars         :: list(tvar)
            ).

    % Given a variable and an arg type assignment set, return the list of
    % the possible different types for the variable and the argument.
    %
:- pred get_arg_type_stuffs(int::in, prog_var::in, args_type_assign_set::in,
    list(arg_type_stuff)::out) is det.

%---------------------------------------------------------------------------%

:- func typestuff_to_type(type_stuff) = mer_type.

:- func typestuff_to_pieces(maybe_add_quotes, inst_varset, type_stuff)
    = list(format_piece).

%---------------------------------------------------------------------------%

:- type actual_expected_types
    --->    actual_expected_types(
                % We put the pieces before the types, so that sorting lists
                % of actual_expected_types structures will *look* sorted,
                % provided the structure of the lists involved is similar.
                % (They should be, since they are generated by the same code.)
                actual_type_pieces      :: list(format_piece),
                actual_type             :: mer_type,
                expected_type_piece     :: list(format_piece),
                expected_type           :: mer_type,
                existq_tvars            :: list(tvar),
                expectation_source      :: maybe(args_type_assign_source)
            ).

:- func type_stuff_to_actual_expected(maybe_add_quotes, inst_varset,
    mer_type, type_stuff) = actual_expected_types.

:- func arg_type_stuff_to_actual_expected(maybe_add_quotes, inst_varset,
    arg_type_stuff) = actual_expected_types.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_error_util.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

type_assign_set_msg_to_verbose_component(Info, VarSet, TypeAssignSet,
        VerboseComponent) :-
    % Converting a type assign set to a part of an error_spec can take
    % a *very* long time if the type assign set is very big, which it can be
    % in large predicates with many ambiguously typed variables.
    % Yet in the common cases handled by the two predicates below,
    % the result of the conversion is needed only if verbose_errors is set.
    % The code below ensures that we incur the cost of the conversion
    % only if we have to.
    %
    % We *always* include a verbose_only component in the result,
    % because this ensures that the compiler output includes the line
    %   For more information, recompile with `-E'.
    % at the end.
    typecheck_info_get_verbose_errors(Info, VerboseErrors),
    (
        VerboseErrors = no,
        VerbosePieces = []
    ;
        VerboseErrors = yes,
        typecheck_info_get_module_info(Info, ModuleInfo),
        VerbosePieces = type_assign_set_msg_to_pieces(ModuleInfo, VarSet,
            TypeAssignSet)
    ),
    VerboseComponent = verbose_only(verbose_always, VerbosePieces).

arg_type_assign_set_msg_to_verbose_component(Info, VarSet,
        ArgTypeAssignSet, VerboseComponent) :-
    typecheck_info_get_verbose_errors(Info, VerboseErrors),
    (
        VerboseErrors = no,
        VerbosePieces = []
    ;
        VerboseErrors = yes,
        typecheck_info_get_module_info(Info, ModuleInfo),
        VerbosePieces = args_type_assign_set_msg_to_pieces(ModuleInfo, VarSet,
            ArgTypeAssignSet)
    ),
    VerboseComponent = verbose_only(verbose_always, VerbosePieces).

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func type_assign_set_msg_to_pieces(module_info, prog_varset,
    type_assign_set) = list(format_piece).

type_assign_set_msg_to_pieces(ModuleInfo, VarSet, TypeAssignSet) = Pieces :-
    ( if TypeAssignSet = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(TypeAssignSet, SortedTypeAssignSet),
    LaterPieces = type_assign_set_to_pieces(ModuleInfo, VarSet,
        SortedTypeAssignSet, MaybeSeq),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

    % Return a description of the current set of type assignments.
    %
    % Since this description can be very large and unwieldy, containing
    % much irrelevant information as well as (hopefully) one or two useful
    % pieces of information, it is intended to be used only with
    % --verbose-errors.
    %
:- func args_type_assign_set_msg_to_pieces(module_info, prog_varset,
    args_type_assign_set) = list(format_piece).

args_type_assign_set_msg_to_pieces(ModuleInfo, VarSet, ArgTypeAssignSet)
        = Pieces :-
    ( if ArgTypeAssignSet = [_] then
        FirstWords = "The partial type assignment was:",
        MaybeSeq = no
    else
        FirstWords = "The possible partial type assignments were:",
        MaybeSeq = yes(1)
    ),
    list.sort(ArgTypeAssignSet, SortedArgTypeAssignSet),
    LaterPieces = args_type_assign_set_to_pieces(ModuleInfo, VarSet,
        SortedArgTypeAssignSet, MaybeSeq),
    Pieces = [words(FirstWords), nl_indent_delta(1) | LaterPieces] ++
        [nl_indent_delta(-1)].

%---------------------------------------------------------------------------%

:- func type_assign_set_to_pieces(module_info, prog_varset, type_assign_set,
    maybe(int)) = list(format_piece).

type_assign_set_to_pieces(_, _, [], _) = [].
type_assign_set_to_pieces(ModuleInfo, VarSet, [TypeAssign | TypeAssigns],
        MaybeSeq) =
    type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, no, MaybeSeq) ++
    type_assign_set_to_pieces(ModuleInfo, VarSet, TypeAssigns,
        inc_maybe_seq(MaybeSeq)).

:- func args_type_assign_set_to_pieces(module_info, prog_varset,
    args_type_assign_set, maybe(int)) = list(format_piece).

args_type_assign_set_to_pieces(_, _, [], _) = [].
args_type_assign_set_to_pieces(ModuleInfo, VarSet,
        [ArgTypeAssign | ArgTypeAssigns], MaybeSeq) = Pieces :-
    % XXX Why does this simply pick the TypeAssign part of the ArgTypeAssign,
    % instead of invoking convert_args_type_assign?
    ArgTypeAssign = args_type_assign(TypeAssign, _ArgTypes, _Constraints,
        Source),
    Pieces =
        type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, yes(Source),
            MaybeSeq) ++
        args_type_assign_set_to_pieces(ModuleInfo, VarSet, ArgTypeAssigns,
            inc_maybe_seq(MaybeSeq)).

%---------------------%

:- func type_assign_to_pieces(module_info, prog_varset, type_assign,
    maybe(args_type_assign_source), maybe(int)) = list(format_piece).

type_assign_to_pieces(ModuleInfo, VarSet, TypeAssign, MaybeSource, MaybeSeq)
        = Pieces :-
    (
        MaybeSeq = yes(N),
        ( if
            MaybeSource = yes(Source),
            SourcePieces0 =
                describe_args_type_assign_source(ModuleInfo, Source),
            SourcePieces0 = [_ | _]
        then
            SourcePieces = [suffix(","), words("derived from") | SourcePieces0]
        else
            SourcePieces = []
        ),
        SeqPieces0 = [words("Type assignment"), int_fixed(N)] ++
            SourcePieces ++ [suffix(":"), nl],
        ( if N > 1 then
            SeqPieces = [blank_line | SeqPieces0]
        else
            SeqPieces = SeqPieces0
        )
    ;
        MaybeSeq = no,
        SeqPieces = []
    ),
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    type_assign_get_var_types(TypeAssign, VarTypes),
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TypeVarSet),
    vartypes_vars(VarTypes, Vars),
    (
        ExistQTVars = [],
        HeadPieces = []
    ;
        ExistQTVars = [_ | _],
        VarsStr = mercury_vars_to_string_vs(TypeVarSet, varnums, ExistQTVars),
        HeadPieces = [words("some [" ++ VarsStr ++ "]"), nl]
    ),
    TypePieces = type_assign_types_to_pieces(VarSet, VarTypes, TypeVarSet,
        TypeBindings, no, Vars),
    ConstraintPieces = type_assign_hlds_constraints_to_pieces(Constraints,
        TypeBindings, TypeVarSet),
    Pieces = SeqPieces ++ HeadPieces ++ TypePieces ++ ConstraintPieces ++ [nl].

:- func type_assign_types_to_pieces(prog_varset, vartypes, tvarset,
    tsubst, bool, list(prog_var)) = list(format_piece).

type_assign_types_to_pieces(_, _, _, _, FoundOne, []) = Pieces :-
    (
        FoundOne = no,
        Pieces = [words("(No variables were assigned a type)")]
    ;
        FoundOne = yes,
        Pieces = []
    ).
type_assign_types_to_pieces(VarSet, VarTypes, TypeVarSet, TypeBindings,
        FoundOne, [Var | Vars]) = Pieces :-
    ( if search_var_type(VarTypes, Var, Type) then
        (
            FoundOne = yes,
            PrefixPieces = [nl]
        ;
            FoundOne = no,
            PrefixPieces = []
        ),
        VarStr = mercury_var_to_string_vs(VarSet, varnums, Var),
        TypeStr = type_with_bindings_to_string(Type, TypeVarSet, TypeBindings),
        AssignPieces = [fixed(VarStr), suffix(":"), words(TypeStr)],
        TailPieces = type_assign_types_to_pieces(VarSet, VarTypes,
            TypeVarSet, TypeBindings, yes, Vars),
        Pieces = PrefixPieces ++ AssignPieces ++ TailPieces
    else
        Pieces = type_assign_types_to_pieces(VarSet, VarTypes,
            TypeVarSet, TypeBindings, FoundOne, Vars)
    ).

:- func type_with_bindings_to_string(mer_type, tvarset, tsubst) = string.

type_with_bindings_to_string(Type0, TypeVarSet, TypeBindings) = Str :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_module_names_from_type(strip_builtin_module_name, Type1, Type),
    Str = mercury_type_to_string(TypeVarSet, print_name_only, Type).

:- func type_assign_hlds_constraints_to_pieces(hlds_constraints,
    tsubst, tvarset) = list(format_piece).

type_assign_hlds_constraints_to_pieces(Constraints, TypeBindings, TypeVarSet)
        = Pieces1 ++ Pieces2 :-
    Constraints =
        hlds_constraints(ConstraintsToProve, AssumedConstraints, _, _),
    PiecesList1 = type_assign_constraints_to_pieces_list("&",
        AssumedConstraints, TypeBindings, TypeVarSet, no),
    PiecesList2 = type_assign_constraints_to_pieces_list("<=",
        ConstraintsToProve, TypeBindings, TypeVarSet, no),
    Pieces1 = component_list_to_line_pieces(PiecesList1, [nl]),
    Pieces2 = component_list_to_line_pieces(PiecesList2, [nl]).

:- func type_assign_constraints_to_pieces_list(string, list(hlds_constraint),
    tsubst, tvarset, bool) = list(list(format_piece)).

type_assign_constraints_to_pieces_list(_, [], _, _, _) = [].
type_assign_constraints_to_pieces_list(Operator, [Constraint | Constraints],
        TypeBindings, TypeVarSet, FoundOne) = [ThisPieces] ++ TailPieceLists :-
    (
        FoundOne = no,
        Prefix = Operator ++ " "
    ;
        FoundOne = yes,
        Prefix = "   "
    ),
    apply_rec_subst_to_constraint(TypeBindings, Constraint, BoundConstraint),
    retrieve_prog_constraint(BoundConstraint, ProgConstraint),
    ThisPieces = [fixed(Prefix ++
        mercury_constraint_to_string(TypeVarSet, print_name_only,
            ProgConstraint))],
    TailPieceLists = type_assign_constraints_to_pieces_list(Operator,
        Constraints, TypeBindings, TypeVarSet, yes).

:- func inc_maybe_seq(maybe(int)) = maybe(int).

inc_maybe_seq(no) = no.
inc_maybe_seq(yes(N)) = yes(N + 1).

:- func varnums = var_name_print.

varnums = print_name_and_num.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_all_type_stuffs_remove_dups([], _Var, []).
get_all_type_stuffs_remove_dups([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
    get_all_type_stuffs_remove_dups(TypeAssigns, Var, TailTypeStuffs),
    get_type_stuff(TypeAssign, Var, TypeStuff),
    ( if list.member(TypeStuff, TailTypeStuffs) then
        TypeStuffs = TailTypeStuffs
    else
        TypeStuffs = [TypeStuff | TailTypeStuffs]
    ).

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable. The returned list may contain
    % duplicates.
    %
:- pred get_all_type_stuffs(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.
% See the comments on get_all_type_stuffs_remove_dups above
% for some scenarios in which this code could be needed.
:- pragma consider_used(pred(get_all_type_stuffs/3)).

get_all_type_stuffs([], _Var, []).
get_all_type_stuffs([TypeAssign | TypeAssigns], Var,
        [TypeStuff | TypeStuffs]) :-
    get_type_stuff(TypeAssign, Var, TypeStuff),
    get_all_type_stuffs(TypeAssigns, Var, TypeStuffs).

get_all_transformed_type_stuffs(_TransformFunc, [], _Var, []).
get_all_transformed_type_stuffs(TransformFunc, [TypeAssign | TypeAssigns], Var,
        [Result | Results]) :-
    get_type_stuff(TypeAssign, Var, TypeStuff),
    Result = TransformFunc(TypeStuff),
    get_all_transformed_type_stuffs(TransformFunc, TypeAssigns, Var, Results).

    % Given a type assignment and a variable, return information about
    % the type of that variable in that type assignment.
    %
:- pred get_type_stuff(type_assign::in, prog_var::in, type_stuff::out) is det.

get_type_stuff(TypeAssign, Var, TypeStuff) :-
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, Type0) then
        Type = Type0
    else
        % This shouldn't happen - how can a variable which has not yet been
        % assigned a type variable fail to have the correct type?
        Type = defined_type(unqualified("<any>"), [], kind_star)
    ),
    TypeStuff = type_stuff(Type, TVarSet, TypeBindings, ExistQTVars).

%---------------------------------------------------------------------------%

get_arg_type_stuffs(_ArgNum, _Var, [], []).
get_arg_type_stuffs(ArgNum, Var, [ArgTypeAssign | ArgTypeAssigns],
        ArgTypeStuffs) :-
    get_arg_type_stuffs(ArgNum, Var, ArgTypeAssigns, TailArgTypeStuffs),
    get_arg_type_stuff(ArgNum, Var, ArgTypeAssign,
        TailArgTypeStuffs, ArgTypeStuffs).

:- pred get_arg_type_stuff(int::in, prog_var::in, args_type_assign::in,
    list(arg_type_stuff)::in, list(arg_type_stuff)::out) is det.
:- pragma inline(pred(get_arg_type_stuff/5)).

get_arg_type_stuff(ArgNum, Var, ArgTypeAssign,
        TailArgTypeStuffs, ArgTypeStuffs) :-
    ArgTypeAssign = args_type_assign(TypeAssign, ArgTypes, _, Source),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( if search_var_type(VarTypes, Var, VarType0) then
        VarType = VarType0
    else
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        VarType = defined_type(unqualified("<any>"), [], kind_star)
    ),
    list.det_index1(ArgTypes, ArgNum, ArgType),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    apply_rec_subst_to_type(TypeBindings, VarType, RecSubstVarType),
    apply_rec_subst_to_type(TypeBindings, ArgType, RecSubstArgType),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_existq_tvars(TypeAssign, ExistQTVars),
    ArgTypeStuff = arg_type_stuff(RecSubstVarType, Source, RecSubstArgType,
        TVarSet, ExistQTVars),
    ( if list.member(ArgTypeStuff, TailArgTypeStuffs) then
        ArgTypeStuffs = TailArgTypeStuffs
    else
        ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typestuff_to_type(TypeStuff) = Type :-
    TypeStuff = type_stuff(Type0, _TypeVarSet, TypeBindings, _ExistQTVars),
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_module_names_from_type(strip_builtin_module_name, Type1, Type).

typestuff_to_pieces(AddQuotes, InstVarSet, TypeStuff) = Pieces :-
    Type = typestuff_to_type(TypeStuff),
    TypeStuff = type_stuff(_Type0, TypeVarSet, _TypeBindings, ExistQTVars),
    Pieces = type_to_pieces(TypeVarSet, InstVarSet, print_name_only, AddQuotes,
        ExistQTVars, Type).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

type_stuff_to_actual_expected(AddQuotes, InstVarSet, ExpectedType,
        VarTypeStuff) = ActualExpected :-
    VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding, ExistQTVars),
    strip_module_names_from_type(strip_builtin_module_name,
        VarType, StrippedVarType),
    strip_module_names_from_type(strip_builtin_module_name,
        ExpectedType, StrippedExpectedType),
    ActualPieces0 = bound_type_to_pieces(TVarSet, InstVarSet,
        print_name_only, AddQuotes, TypeBinding, ExistQTVars,
        StrippedVarType),
    ExpectedPieces0 = bound_type_to_pieces(TVarSet, InstVarSet,
        print_name_only, AddQuotes, TypeBinding, ExistQTVars,
        StrippedExpectedType),
    ( if ActualPieces0 = ExpectedPieces0 then
        ActualPieces = bound_type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, TypeBinding, ExistQTVars,
            VarType),
        ExpectedPieces = bound_type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, TypeBinding, ExistQTVars,
            ExpectedType),
        ActualExpected = actual_expected_types(ActualPieces, VarType,
            ExpectedPieces, ExpectedType, ExistQTVars, no)
    else
        ActualExpected = actual_expected_types(ActualPieces0, VarType,
            ExpectedPieces0, ExpectedType, ExistQTVars, no)
    ).

arg_type_stuff_to_actual_expected(AddQuotes, InstVarSet, ArgTypeStuff)
        = ActualExpected :-
    ArgTypeStuff = arg_type_stuff(VarType, Source, ExpectedType,
        TVarSet, ExistQTVars),
    strip_module_names_from_type(strip_builtin_module_name,
        VarType, StrippedVarType),
    strip_module_names_from_type(strip_builtin_module_name,
        ExpectedType, StrippedExpectedType),
    ActualPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
        AddQuotes, ExistQTVars, StrippedVarType),
    ExpectedPieces0 = type_to_pieces(TVarSet, InstVarSet, print_name_only,
        AddQuotes, ExistQTVars, StrippedExpectedType),
    ( if ActualPieces0 = ExpectedPieces0 then
        ActualPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, ExistQTVars, VarType),
        ExpectedPieces = type_to_pieces(TVarSet, InstVarSet,
            print_name_and_num, AddQuotes, ExistQTVars, ExpectedType),
        ActualExpected = actual_expected_types(ActualPieces, VarType,
            ExpectedPieces, ExpectedType, ExistQTVars, yes(Source))
    else
        ActualExpected = actual_expected_types(ActualPieces0, VarType,
            ExpectedPieces0, ExpectedType, ExistQTVars, yes(Source))
    ).

:- func bound_type_to_pieces(tvarset, inst_varset, var_name_print,
    maybe_add_quotes, tsubst, list(tvar), mer_type) = list(format_piece).

bound_type_to_pieces(TVarSet, InstVarSet, VarNamePrint, AddQuotes,
        TypeBindings, ExistQTVars, Type0) = Pieces :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type),
    Pieces = type_to_pieces(TVarSet, InstVarSet, VarNamePrint, AddQuotes,
        ExistQTVars, Type).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_type_assign.
%---------------------------------------------------------------------------%
