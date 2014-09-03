%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: builtin_lib_types.m.
%
% The function and predicates of this module handle return information about
% the types and type constructors built into Mercury and defined in modules of
% the standard Mercury library, as well as the function symbols of those types.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.builtin_lib_types.
:- interface.

:- import_module parse_tree.prog_data.

%-----------------------------------------------------------------------------%
%
% Types.
%

:- func int_type = mer_type.
:- func float_type = mer_type.
:- func string_type = mer_type.
:- func char_type = mer_type.
:- func void_type = mer_type.
:- func c_pointer_type = mer_type.
:- func heap_pointer_type = mer_type.
:- func float_box_type = mer_type.
:- func sample_type_info_type = mer_type.
:- func sample_typeclass_info_type = mer_type.
:- func type_info_type = mer_type.
:- func type_ctor_info_type = mer_type.
:- func type_desc_type = mer_type.
:- func pseudo_type_desc_type = mer_type.
:- func type_ctor_desc_type = mer_type.
:- func comparison_result_type = mer_type.
:- func io_state_type = mer_type.
:- func io_io_type = mer_type.
:- func univ_type = mer_type.
:- func exception_result_type(mer_type) = mer_type.
:- func stm_atomic_type = mer_type.
:- func stm_state_type = mer_type.
:- func stm_valid_result_type = mer_type.
:- func stm_rollback_exception_type = mer_type.
:- func stm_dummy_output_type = mer_type.
:- func region_type = mer_type.
:- func future_type(mer_type) = mer_type.

%-----------------------------------------------------------------------------%
%
% Type constructors.
%

:- func int_type_ctor = type_ctor.
:- func float_type_ctor = type_ctor.
:- func char_type_ctor = type_ctor.
:- func string_type_ctor = type_ctor.

:- func poly_type_type_ctor = type_ctor.

:- func list_type_ctor = type_ctor.

:- func exception_result_type_ctor = type_ctor.

:- func stm_valid_result_type_ctor = type_ctor.
:- func stm_rollback_exception_type_ctor = type_ctor.
:- func stm_dummy_output_type_ctor = type_ctor.

%-----------------------------------------------------------------------------%

    % Succeed iff the given variable is of region_type.
    %
:- pred is_region_var(vartypes::in, prog_var::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Functors.
%

    % The functors of type exception_result_type_ctor.
    %
:- func exception_succeeded_functor = cons_id.
:- func exception_failed_functor = cons_id.
:- func exception_exception_functor = cons_id.

    % The functors of type stm_valid_result_type_ctor.
    %
:- func stm_validres_valid_functor = cons_id.
:- func stm_validres_invalid_functor = cons_id.

    % The functors of type stm_rollback_exception_type_ctor.
    %
:- func stm_rollback_exception_functor = cons_id.
:- func stm_rollback_retry_functor = cons_id.

    % The functors of type stm_dummy_output_type_ctor.
    %
:- func stm_dummy_output_functor = cons_id.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.

:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

int_type = builtin_type(builtin_type_int).

float_type = builtin_type(builtin_type_float).

string_type = builtin_type(builtin_type_string).

char_type = builtin_type(builtin_type_char).

void_type = defined_type(unqualified("void"), [], kind_star).

c_pointer_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_public_builtin_module,
    Name = qualified(BuiltinModule, "c_pointer").

heap_pointer_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "heap_pointer").

float_box_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "float_box").

sample_type_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "sample_type_info").

sample_typeclass_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "sample_typeclass_info").

type_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "type_info").

type_ctor_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "type_ctor_info").

type_desc_type = defined_type(Name, [], kind_star) :-
    Module = mercury_std_lib_module_name(unqualified("type_desc")),
    Name = qualified(Module, "type_desc").

pseudo_type_desc_type = defined_type(Name, [], kind_star) :-
    Module = mercury_std_lib_module_name(unqualified("type_desc")),
    Name = qualified(Module, "pseudo_type_desc").

type_ctor_desc_type = defined_type(Name, [], kind_star) :-
    Module = mercury_std_lib_module_name(unqualified("type_desc")),
    Name = qualified(Module, "type_ctor_desc").

comparison_result_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_public_builtin_module,
    Name = qualified(BuiltinModule, "comparison_result").

io_state_type = defined_type(Name, [], kind_star) :-
    Module = mercury_io_module,
    Name = qualified(Module, "state").

io_io_type = defined_type(Name, [], kind_star) :-
    Module = mercury_io_module,
    Name = qualified(Module, "io").

univ_type = defined_type(Name, [], kind_star) :-
    Module = mercury_univ_module,
    Name = qualified(Module, "univ").

exception_result_type(SubType) = defined_type(Name, [SubType], kind_star) :-
    Module = mercury_exception_module,
    Name = qualified(Module, "exception_result").

stm_atomic_type = defined_type(Name, [], kind_star) :-
    Module = mercury_std_lib_module_name(unqualified("stm_builtin")),
    Name = qualified(Module, "stm").

stm_state_type = defined_type(Name, [], kind_star) :-
    Module = mercury_stm_builtin_module,
    Name = qualified(Module, "stm").

stm_valid_result_type = defined_type(Name, [], kind_star) :-
    Module = mercury_stm_builtin_module,
    Name = qualified(Module, "stm_validation_result").

stm_rollback_exception_type = defined_type(Name, [], kind_star) :-
    Module = mercury_stm_builtin_module,
    Name = qualified(Module, "rollback_exception").

stm_dummy_output_type = defined_type(Name, [], kind_star) :-
    Module = mercury_stm_builtin_module,
    Name = qualified(Module, "stm_dummy_output").

region_type = defined_type(Name, [], kind_star) :-
    Module = mercury_region_builtin_module,
    Name = qualified(Module, "region").

future_type(ValueType) = defined_type(Name, [ValueType], kind_star) :-
    Module = mercury_par_builtin_module,
    Name = qualified(Module, "future").

%-----------------------------------------------------------------------------%

int_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int").
float_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("float").
char_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("character").
string_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("string").

poly_type_type_ctor = type_ctor(Name, 0) :-
    Name = qualified(mercury_string_module, "poly_type").

list_type_ctor = type_ctor(Name, 1) :-
    Name = qualified(mercury_list_module, "list").

exception_result_type_ctor = type_ctor(Name, 1) :-
    Name = qualified(mercury_exception_module, "exception_result").

stm_valid_result_type_ctor = type_ctor(Name, 0) :-
    Name = qualified(mercury_stm_builtin_module, "stm_validation_result").
stm_rollback_exception_type_ctor = type_ctor(Name, 0) :-
    Name = qualified(mercury_stm_builtin_module, "rollback_exception").
stm_dummy_output_type_ctor = type_ctor(Name, 0) :-
    Name = qualified(mercury_stm_builtin_module, "stm_dummy_output").

%-----------------------------------------------------------------------------%

is_region_var(VarTypes, Var)  :-
    lookup_var_type(VarTypes, Var, Type),
    Type = region_type.

%-----------------------------------------------------------------------------%

exception_succeeded_functor = cons(Name, 1, TypeCtor) :-
    Name = qualified(mercury_exception_module, "succeeded"),
    TypeCtor = exception_result_type_ctor.
exception_failed_functor = cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_exception_module, "failed"),
    TypeCtor = exception_result_type_ctor.
exception_exception_functor = cons(Name, 1, TypeCtor) :-
    Name = qualified(mercury_exception_module, "exception"),
    TypeCtor = exception_result_type_ctor.

stm_validres_valid_functor = cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_stm_builtin_module, "stm_transaction_valid"),
    TypeCtor = stm_valid_result_type_ctor.
stm_validres_invalid_functor =  cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_stm_builtin_module, "stm_transaction_invalid"),
    TypeCtor = stm_valid_result_type_ctor.

stm_rollback_exception_functor = cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_stm_builtin_module,
        "rollback_invalid_transaction"),
    TypeCtor = stm_rollback_exception_type_ctor.
stm_rollback_retry_functor = cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_stm_builtin_module, "rollback_retry"),
    TypeCtor = stm_rollback_exception_type_ctor.

stm_dummy_output_functor = cons(Name, 0, TypeCtor) :-
    Name = qualified(mercury_stm_builtin_module, "stm_dummy_output"),
    TypeCtor = stm_dummy_output_type_ctor.

%-----------------------------------------------------------------------------%
:- end_module parse_tree.builtin_lib_types.
%-----------------------------------------------------------------------------%
