%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009, 2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: builtin_lib_types.m.
%
% The function and predicates of this module handle return information about
% the types and type constructors built into Mercury and defined in modules of
% the standard Mercury library, as well as the function symbols of those types.
%
%---------------------------------------------------------------------------%

:- module parse_tree.builtin_lib_types.
:- interface.

:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%
%
% Types.
%

:- func int_type = mer_type.
:- func uint_type = mer_type.
:- func int8_type = mer_type.
:- func uint8_type = mer_type.
:- func int16_type = mer_type.
:- func uint16_type = mer_type.
:- func int32_type = mer_type.
:- func uint32_type = mer_type.
:- func int64_type = mer_type.
:- func uint64_type = mer_type.
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

%---------------------%

    % Construct the type of the type_info for the given type.
    %
:- func build_type_info_type(mer_type) = mer_type.

   % Build the type describing the typeclass_info for the
    % given prog_constraint.
    %
:- func build_typeclass_info_type = mer_type.

    % Check whether a type is either the `type_info' type or the
    % `type_ctor_info' type introduced by this pass.
    %
:- pred type_is_type_info_or_ctor_type(mer_type::in) is semidet.

    % Check whether a type is the `typeclass_info' type.
    %
:- pred type_is_typeclass_info(mer_type::in) is semidet.

%---------------------------------------------------------------------------%
%
% Type constructors.
%

:- func int_type_ctor = type_ctor.
:- func uint_type_ctor = type_ctor.
:- func int8_type_ctor = type_ctor.
:- func uint8_type_ctor = type_ctor.
:- func int16_type_ctor = type_ctor.
:- func uint16_type_ctor = type_ctor.
:- func int32_type_ctor = type_ctor.
:- func uint32_type_ctor = type_ctor.
:- func int64_type_ctor = type_ctor.
:- func uint64_type_ctor = type_ctor.
:- func float_type_ctor = type_ctor.
:- func char_type_ctor = type_ctor.
:- func string_type_ctor = type_ctor.

:- func poly_type_type_ctor = type_ctor.

:- func list_type_ctor = type_ctor.

:- func exception_result_type_ctor = type_ctor.

:- func stm_valid_result_type_ctor = type_ctor.
:- func stm_rollback_exception_type_ctor = type_ctor.
:- func stm_dummy_output_type_ctor = type_ctor.

%---------------------------------------------------------------------------%
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module list.

%---------------------------------------------------------------------------%

int_type = builtin_type(builtin_type_int(int_type_int)).

uint_type = builtin_type(builtin_type_int(int_type_uint)).

int8_type = builtin_type(builtin_type_int(int_type_int8)).

uint8_type = builtin_type(builtin_type_int(int_type_uint8)).

int16_type = builtin_type(builtin_type_int(int_type_int16)).

uint16_type = builtin_type(builtin_type_int(int_type_uint16)).

int32_type = builtin_type(builtin_type_int(int_type_int32)).

uint32_type = builtin_type(builtin_type_int(int_type_uint32)).

int64_type = builtin_type(builtin_type_int(int_type_int64)).

uint64_type = builtin_type(builtin_type_int(int_type_uint64)).

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

%---------------------%

build_type_info_type(Type) = TypeInfoType :-
    % XXX TypeInfoType = type_ctor_info_type.
    ( if type_has_variable_arity_ctor(Type, _, _) then
        % We cannot use a plain type_ctor_info because we need to
        % record the arity.
        TypeInfoType = type_info_type
    else if type_to_ctor_and_args(Type, _Ctor, Args) then
        (
            Args = [],
            TypeInfoType = type_ctor_info_type
        ;
            Args = [_ | _],
            TypeInfoType = type_info_type
        )
    else
        % The type is variable, which means we have a type_info for it.
        % That type_info may actually be a type_ctor_info, but the code
        % of the current predicate won't treat it as such.
        TypeInfoType = type_info_type
    ).

build_typeclass_info_type = TypeClassInfoType :-
    PrivateBuiltin = mercury_private_builtin_module,
    TypeclassInfoTypeName = qualified(PrivateBuiltin, "typeclass_info"),
    TypeClassInfoType = defined_type(TypeclassInfoTypeName, [], kind_star).

type_is_type_info_or_ctor_type(TypeInfoType) :-
    type_to_ctor_and_args(TypeInfoType, TypeCtor, []),
    TypeCtor = type_ctor(qualified(ModuleName, TypeName), 0),
    ModuleName = mercury_private_builtin_module,
    ( TypeName = "type_info"
    ; TypeName = "type_ctor_info"
    ).

type_is_typeclass_info(TypeClassInfoType) :-
    type_to_ctor(TypeClassInfoType, TypeCtor),
    TypeCtor = type_ctor(qualified(ModuleName, "typeclass_info"), 0),
    ModuleName = mercury_private_builtin_module.

%---------------------------------------------------------------------------%

int_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int").
uint_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("uint").
int8_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int8").
uint8_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("uint8").
int16_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int16").
uint16_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("uint16").
int32_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int32").
uint32_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("uint32").
int64_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("int64").
uint64_type_ctor = type_ctor(Name, 0) :-
    Name = unqualified("uint64").
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
:- end_module parse_tree.builtin_lib_types.
%---------------------------------------------------------------------------%
