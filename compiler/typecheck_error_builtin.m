%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_builtin.m.
%
% This module provides utility predicates dealing with builtin types and
% operations to the code that constructs diagnostics for type errors.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_builtin.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % Is the given function symbol the name of a function that is defined
    % (or could/should be defined) in each of int.m, uint.m,
    % and intN.m/uintN.m for N = {8,16,32,64}?
    %
:- pred is_int_func_op(cons_id::in) is semidet.

    % Is the given string the name of a predicate that is defined
    % in each of int.m, uint.m, intN.m and uintN.m for N = {8,16,32,64}?
    %
:- pred is_int_pred_op(sym_name::in, pred_form_arity::in) is semidet.

%---------------------------------------------------------------------------%

:- pred acc_builtin_types_of_var(list(type_assign)::in, prog_var::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

:- pred acc_builtin_types_in_cons_type_infos(list(cons_type_info)::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

:- pred acc_builtin_type(mer_type::in,
    set(builtin_type)::in, set(builtin_type)::out) is det.

%---------------------------------------------------------------------------%

:- pred type_needs_int_constant_suffix(mer_type::in) is semidet.

:- func nosuffix_integer_pieces = list(format_piece).

%---------------------------------------------------------------------------%

:- func report_any_invisible_int_types(type_error_clause_context,
    set(builtin_type)) = list(format_piece).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_error_type_assign.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.

:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

is_int_func_op(ConsId) :-
    ConsId = du_data_ctor(DuCtor),
    DuCtor = du_ctor(SymName, Arity, _TypeCtor),
    % We ignore the module name part of SymName, since it is very likely
    % that the error is precisely the fact that the module name is wrong.
    Name = unqualify_name(SymName),
    % This table lists the user arities of the named functions.
    ( Name = "abs",                     Arity = 1
    ; Name = "unchecked_abs",           Arity = 1
    ; Name = "nabs",                    Arity = 1

    ; Name = "max",                     Arity = 2
    ; Name = "min",                     Arity = 2

    ; Name = "+",                       (Arity = 1; Arity = 2)
    ; Name = "plus",                    Arity = 2
    ; Name = "-",                       (Arity = 1; Arity = 2)
    ; Name = "minus",                   Arity = 2
    ; Name = "*",                       Arity = 2
    ; Name = "times",                   Arity = 2
    ; Name = "/",                       Arity = 2
    ; Name = "//",                      Arity = 2
    ; Name = "div",                     Arity = 2
    ; Name = "unchecked_quotient",      Arity = 2
    ; Name = "mod",                     Arity = 2
    ; Name = "rem",                     Arity = 2
    ; Name = "unchecked_rem",           Arity = 2

    ; Name = "pow",                     Arity = 2
    ; Name = "log2",                    Arity = 1

    ; Name = "<<",                      Arity = 2
    ; Name = "unchecked_left_shift",    Arity = 2
    ; Name = ">>",                      Arity = 2
    ; Name = "unchecked_right_shift",   Arity = 2

    ; Name = "\\",                      Arity = 1
    ; Name = "/\\",                     Arity = 2
    ; Name = "\\/",                     Arity = 2
    ; Name = "xor",                     Arity = 2
    ).

is_int_pred_op(SymName, PredFormArity) :-
    % We ignore the module name part of SymName, since it is very likely
    % that the error is precisely the fact that the module name is wrong.
    Name = unqualify_name(SymName),
    PredFormArity = pred_form_arity(Arity),
    ( Name = "<",                       Arity = 2
    ; Name = ">",                       Arity = 2
    ; Name = "=<",                      Arity = 2
    ; Name = ">=",                      Arity = 2

    ; Name = "abs",                     Arity = 2

    ; Name = "max",                     Arity = 3
    ; Name = "min",                     Arity = 3

    ; Name = "pow",                     Arity = 3
    ; Name = "log2",                    Arity = 2
    ).

%---------------------------------------------------------------------------%

acc_builtin_types_of_var(TypeAssignSet, Var, !BuiltinTypes) :-
    get_all_type_stuffs_remove_dups(TypeAssignSet, Var, VarTypeStuffs),
    TypesOfVar = list.map(typestuff_to_type, VarTypeStuffs),
    list.foldl(acc_builtin_type, TypesOfVar, !BuiltinTypes).

acc_builtin_types_in_cons_type_infos([], !BuiltinTypes).
acc_builtin_types_in_cons_type_infos([ConsTypeInfo | ConsTypeInfos],
        !BuiltinTypes) :-
    ConsTypeInfo = cons_type_info(_VarSet, _ExistQTVars, ResultType, ArgTypes,
        _Constraints, _Source),
    acc_builtin_type(ResultType, !BuiltinTypes),
    list.foldl(acc_builtin_type, ArgTypes, !BuiltinTypes),
    acc_builtin_types_in_cons_type_infos(ConsTypeInfos, !BuiltinTypes).

acc_builtin_type(Type, !BuiltinTypes) :-
    (
        Type = builtin_type(BuiltinType),
        set.insert(BuiltinType, !BuiltinTypes)
    ;
        ( Type = type_variable(_, _)
        ; Type = defined_type(_, _, _)
        ; Type = tuple_type(_, _)
        ; Type = higher_order_type(_, _, _, _)
        ; Type = apply_n_type(_, _, _)
        ; Type = kinded_type(_, _)
        )
    ).

%---------------------------------------------------------------------------%

type_needs_int_constant_suffix(Type) :-
    Type = builtin_type(BuiltinType),
    BuiltinType = builtin_type_int(BuiltinTypeInt),
    BuiltinTypeInt \= int_type_int.

nosuffix_integer_pieces = Pieces :-
    Pieces = [words("A integer constant that consists only of digits is")] ++
        color_as_hint([words("always of type"), quote("int"), suffix(".")]) ++
        [words("Unsigned integer constants of the default size"),
        words("should have the suffix"), quote("u"), suffix(";"),
        words("constants of sized integer types should have"),
        words("an"), quote("i8"), suffix(","), quote("i16"), suffix(","),
        quote("i32"), words("or"), quote("i64"), words("suffix"),
        words("if they are signed, and"),
        words("an"), quote("u8"), suffix(","), quote("u16"), suffix(","),
        quote("u32"), words("or"), quote("u64"), words("suffix"),
        words("if they are unsigned."), nl].

%---------------------------------------------------------------------------%

report_any_invisible_int_types(ClauseContext, BuiltinTypes) = Pieces :-
    set.filter_map(
        (pred(builtin_type_int(IntType)::in, IntType::out) is semidet),
        BuiltinTypes, IntTypes),
    ( if
        set.is_non_empty(IntTypes),
        ModuleInfo = ClauseContext ^ tecc_module_info,
        module_info_get_visible_modules(ModuleInfo, VisModules),
        set.filter_map(is_int_n_module, VisModules, VisIntTypes),
        set.difference(IntTypes, VisIntTypes, InvisIntTypes),
        set.to_sorted_list(InvisIntTypes, InvisIntTypesList),
        % Is there at least one integer type whose module we did not import?
        InvisIntTypesList = [HeadInvisIntType | TailInvisIntTypes]
    then
        % XXX The pieces we return denote the names of both types and modules.
        % Should we quote the type names? The module names? Currently,
        % we quote both, which is consistent, but that very consistency
        % makes it harder for readers to differentiate the two roles
        % of each name.
        IntTypeToModulePiece =
            ( func(IT) = quote(Str) :- int_type_module_name(IT, Str) ),
        HeadInvisIntTypePiece = IntTypeToModulePiece(HeadInvisIntType),
        TailInvisIntTypePieces =
            list.map(IntTypeToModulePiece, TailInvisIntTypes),
        (
            TailInvisIntTypePieces = [],
            Pieces = [words("Note that operations on values of type"),
                HeadInvisIntTypePiece, words("are available"),
                words("only if module")] ++
                color_as_hint([HeadInvisIntTypePiece,
                    words("is imported.")]) ++
                [nl]
        ;
            TailInvisIntTypePieces = [_ | _],
            InvisIntTypePieces =
                [HeadInvisIntTypePiece | TailInvisIntTypePieces],
            InvisIntTypeListAreImportedtPieces =
                piece_list_to_color_pieces(color_hint, "and",
                    [words("are imported.")], InvisIntTypePieces),
            Pieces = [words("Note that operations on values of types") |
                InvisIntTypePieces] ++ [words("are available"),
                words("only if modules")] ++
                InvisIntTypeListAreImportedtPieces ++ [nl]
        )
    else
        Pieces = []
    ).

:- pred is_int_n_module(module_name::in, int_type::out) is semidet.

is_int_n_module(ModuleSymName, IntType) :-
    ModuleSymName = unqualified(ModuleName),
    int_type_module_name(IntType, ModuleName).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_builtin.
%---------------------------------------------------------------------------%
