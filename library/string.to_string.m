%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: string.to_string.m.
%
% This module implements the related predicates
%
% - string.string
% - string.string_ops
% - string.string_ops_noncanon
%
% which each convert a value of an arbitrary type to a string.
%
%---------------------------------------------------------------------------%

:- module string.to_string.
:- interface.

:- import_module deconstruct.
:- import_module ops.

%---------------------------------------------------------------------------%

:- func string_impl(T) = string.

:- func string_ops_impl(ops.table, T) = string.

:- pred string_ops_noncanon_impl(noncanon_handling, ops.table, T, string).
:- mode string_ops_noncanon_impl(in(do_not_allow), in, in, out) is det.
:- mode string_ops_noncanon_impl(in(canonicalize), in, in, out) is det.
:- mode string_ops_noncanon_impl(in(include_details_cc), in, in, out)
    is cc_multi.
:- mode string_ops_noncanon_impl(in, in, in, out) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module int.
:- import_module type_desc.
:- import_module univ.
:- import_module version_array.

:- use_module rtti_implementation.
:- use_module term_io.

%---------------------------------------------------------------------------%

string_impl(Univ) = String :-
    string_ops_noncanon_impl(canonicalize, ops.init_mercury_op_table,
        Univ, String).

string_ops_impl(OpTable, Univ) = String :-
    string_ops_noncanon_impl(canonicalize, OpTable, Univ, String).

string_ops_noncanon_impl(NonCanon, OpTable, X, String) :-
    value_to_revstrings(NonCanon, OpTable, X, [], RevStrings),
    String = string.append_list(list.reverse(RevStrings)).

%---------------------------------------------------------------------------%

    % For efficiency, these predicates collect a list of strings which,
    % when concatenated in reverse order, produce the final output.
    %
:- type revstrings == list(string).

    % Utility predicate.
    %
:- pred add_revstring(string::in, revstrings::in, revstrings::out) is det.

add_revstring(String, RevStrings, [String | RevStrings]).

%---------------------------------------------------------------------------%

:- pred value_to_revstrings(noncanon_handling, ops.table, T,
    revstrings, revstrings).
:- mode value_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode value_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode value_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode value_to_revstrings(in, in, in, in, out) is cc_multi.

value_to_revstrings(NonCanon, OpTable, X, !Rs) :-
    Priority = ops.universal_priority(OpTable),
    value_to_revstrings_prio(NonCanon, OpTable, Priority, X, !Rs).

:- pred value_to_revstrings_prio(noncanon_handling, ops.table, ops.priority, T,
    revstrings, revstrings).
:- mode value_to_revstrings_prio(in(do_not_allow), in, in, in, in, out) is det.
:- mode value_to_revstrings_prio(in(canonicalize), in, in, in, in, out) is det.
:- mode value_to_revstrings_prio(in(include_details_cc), in, in, in, in, out)
    is cc_multi.
:- mode value_to_revstrings_prio(in, in, in, in, in, out) is cc_multi.

value_to_revstrings_prio(NonCanon, OpTable, Priority, X, !Rs) :-
    % We need to special-case the builtin types:
    %   int, uint, int8, uint8, int16, uint16, int32, uint32
    %   char, float, string
    %   type_info, univ, c_pointer, array
    %   and private_builtin.type_info

    ( if dynamic_cast(X, String) then
        add_revstring(term_io.quoted_string(String), !Rs)
    else if dynamic_cast(X, Char) then
        add_revstring(term_io.quoted_char(Char), !Rs)
    else if dynamic_cast(X, Int) then
        add_revstring(string.int_to_string(Int), !Rs)
    else if dynamic_cast(X, UInt) then
        add_revstring(string.uint_to_string(UInt) ++ "u", !Rs)
    else if dynamic_cast(X, Int8) then
        add_revstring(string.int8_to_string(Int8) ++ "i8", !Rs)
    else if dynamic_cast(X, UInt8) then
        add_revstring(string.uint8_to_string(UInt8) ++ "u8", !Rs)
    else if dynamic_cast(X, Int16) then
        add_revstring(string.int16_to_string(Int16) ++ "i16", !Rs)
    else if dynamic_cast(X, UInt16) then
        add_revstring(string.uint16_to_string(UInt16) ++ "u16", !Rs)
    else if dynamic_cast(X, Int32) then
        add_revstring(string.int32_to_string(Int32) ++ "i32", !Rs)
    else if dynamic_cast(X, UInt32) then
        add_revstring(string.uint32_to_string(UInt32) ++ "u32", !Rs)
    else if dynamic_cast(X, Float) then
        add_revstring(string.float_to_string(Float), !Rs)
    else if dynamic_cast(X, Bitmap) then
        add_revstring(term_io.quoted_string(bitmap.to_string(Bitmap)), !Rs)
    else if dynamic_cast(X, TypeDesc) then
        type_desc_to_revstrings(TypeDesc, !Rs)
    else if dynamic_cast(X, TypeCtorDesc) then
        type_ctor_desc_to_revstrings(TypeCtorDesc, !Rs)
    else if dynamic_cast(X, C_Pointer) then
        add_revstring(c_pointer_to_string(C_Pointer), !Rs)
    else if
        % Check if the type is array.array/1. We cannot just use dynamic_cast
        % here since array.array/1 is a polymorphic type.
        %
        % The calls to type_ctor_name and type_ctor_module_name are not really
        % necessary -- we could use dynamic_cast in the condition instead of
        % det_dynamic_cast in the body. However, this way of doing things
        % is probably more efficient in the common case when the thing
        % being printed is *not* of type array.array/1.
        %
        % The ordering of the tests here (arity, then name, then module name,
        % rather than the reverse) is also chosen for efficiency, to find
        % failure cheaply in the common cases, rather than for readability.
        %
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "array",
        type_ctor_module_name(TypeCtor) = "array"
    then
        % Now that we know the element type, we can constrain the type of
        % the variable Array so that we can use det_dynamic_cast.
        %
        has_type(Elem, ElemType),
        same_array_elem_type(Array, Elem),
        det_dynamic_cast(X, Array),
        array_to_revstrings(NonCanon, OpTable, Array, !Rs)
    else if
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "version_array",
        type_ctor_module_name(TypeCtor) = "version_array"
    then
        has_type(Elem, ElemType),
        same_version_array_elem_type(VersionArray, Elem),
        det_dynamic_cast(X, VersionArray),
        version_array_to_revstrings(NonCanon, OpTable, VersionArray, !Rs)
    else if
        % Check if the type is private_builtin.type_info/1.
        % See the comments above for array.array/1.
        %
        type_ctor_and_args(type_of(X), TypeCtor, ArgTypes),
        ArgTypes = [ElemType],
        type_ctor_name(TypeCtor) = "type_info",
        type_ctor_module_name(TypeCtor) = "private_builtin"
    then
        has_type(Elem, ElemType),
        same_private_builtin_type(PrivateBuiltinTypeInfo, Elem),
        det_dynamic_cast(X, PrivateBuiltinTypeInfo),
        private_builtin_type_info_to_revstrings(PrivateBuiltinTypeInfo, !Rs)
    else
        ordinary_term_to_revstrings(NonCanon, OpTable, Priority, X, !Rs)
    ).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

:- pred same_version_array_elem_type(version_array(T)::unused, T::unused)
    is det.

same_version_array_elem_type(_, _).

:- pred same_private_builtin_type(private_builtin.type_info::unused,
    T::unused) is det.

same_private_builtin_type(_, _).

:- pred ordinary_term_to_revstrings(noncanon_handling, ops.table,
    ops.priority, T, revstrings, revstrings).
:- mode ordinary_term_to_revstrings(in(do_not_allow),
    in, in, in, in, out) is det.
:- mode ordinary_term_to_revstrings(in(canonicalize),
    in, in, in, in, out) is det.
:- mode ordinary_term_to_revstrings(in(include_details_cc),
    in, in, in, in, out) is cc_multi.
:- mode ordinary_term_to_revstrings(in,
    in, in, in, in, out) is cc_multi.

ordinary_term_to_revstrings(NonCanon, OpTable, MinTermPrio, X, !Rs) :-
    % NOTE: The code of this predicate should be kept in sync with
    % the code of write_ordinary_term in stream.string_writer.m.
    % XXX The code for handling tuples is currently NOT in sync.
    deconstruct(X, NonCanon, Functor, _Arity, Args),
    ( if
        (
            Functor = "[|]",
            Args = [ListHead, ListTail],
            add_revstring("[", !Rs),
            arg_to_revstrings(NonCanon, OpTable, ListHead, !Rs),
            univ_list_tail_to_revstrings(NonCanon, OpTable, ListTail, !Rs),
            add_revstring("]", !Rs)
        ;
            Functor = "[]",
            Args = [],
            add_revstring("[]", !Rs)
        ;
            Functor = "{}",
            (
                Args = [],
                add_revstring("{}", !Rs)
            ;
                Args = [BracedTerm],
                add_revstring("{ ", !Rs),
                value_to_revstrings(NonCanon, OpTable, univ_value(BracedTerm),
                    !Rs),
                add_revstring(" }", !Rs)
            ;
                Args = [BracedHead | BracedTail],
                BracedTail = [_ | _],
                % If we add padding after { and before } for tuples
                % containing one term, why do we not also do so for tuples
                % containing more than one term?
                %
                % (compiler/parse_tree_out_term.m says it is because non-DCG
                % goals in DCG clauses look like one-argument tuples, and
                % by tradition, they have spaces between the goal and
                % the { and }.) However, that is not an argument for
                % doing this for *all* uses of {}.
                add_revstring("{", !Rs),
                arg_to_revstrings(NonCanon, OpTable, BracedHead, !Rs),
                term_args_to_revstrings(NonCanon, OpTable, BracedTail, !Rs),
                add_revstring("}", !Rs)
            )
        )
    then
        % Return the value of !:R computed above.
        true
    else
        (
            Args = [ArgA],
            ( if ops.lookup_op_infos(OpTable, Functor, OpInfos) then
                ( if
                    OpInfos ^ oi_prefix = pre(OpPrio, GtOrGeA)
                then
                    maybe_add_revstring("(", MinTermPrio, OpPrio, !Rs),
                    add_revstring(term_io.quoted_atom(Functor), !Rs),
                    add_revstring(" ", !Rs),
                    MinPrioA = min_priority_for_arg(OpPrio, GtOrGeA),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioA,
                        univ_value(ArgA), !Rs),
                    maybe_add_revstring(")", MinTermPrio, OpPrio, !Rs)
                else if
                    OpInfos ^ oi_postfix = post(OpPrio, GtOrGeA)
                then
                    maybe_add_revstring("(", MinTermPrio, OpPrio, !Rs),
                    MinPrioA = min_priority_for_arg(OpPrio, GtOrGeA),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioA,
                        univ_value(ArgA), !Rs),
                    add_revstring(" ", !Rs),
                    add_revstring(term_io.quoted_atom(Functor), !Rs),
                    maybe_add_revstring(")", MinTermPrio, OpPrio, !Rs)
                else
                    plain_term_to_revstrings(NonCanon, OpTable, MinTermPrio,
                        Functor, Args, !Rs)
                )
            else
                plain_term_to_revstrings(NonCanon, OpTable, MinTermPrio,
                    Functor, Args, !Rs)
            )
        ;
            Args = [ArgA, ArgB],
            ( if ops.lookup_op_infos(OpTable, Functor, OpInfos) then
                ( if
                    OpInfos ^ oi_infix = in(OpPrio, GtOrGeA, GtOrGeB)
                then
                    MinPrioA = min_priority_for_arg(OpPrio, GtOrGeA),
                    MinPrioB = min_priority_for_arg(OpPrio, GtOrGeB),
                    maybe_add_revstring("(", MinTermPrio, OpPrio, !Rs),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioA,
                        univ_value(ArgA), !Rs),
                    ( if Functor = "," then
                        add_revstring(", ", !Rs)
                    else
                        add_revstring(" ", !Rs),
                        add_revstring(term_io.quoted_atom(Functor), !Rs),
                        add_revstring(" ", !Rs)
                    ),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioB,
                        univ_value(ArgB), !Rs),
                    maybe_add_revstring(")", MinTermPrio, OpPrio, !Rs)
                else if
                    OpInfos ^ oi_binary_prefix =
                        bin_pre(OpPrio, GtOrGeA, GtOrGeB)
                then
                    MinPrioA = min_priority_for_arg(OpPrio, GtOrGeA),
                    MinPrioB = min_priority_for_arg(OpPrio, GtOrGeB),
                    maybe_add_revstring("(", MinTermPrio, OpPrio, !Rs),
                    add_revstring(term_io.quoted_atom(Functor), !Rs),
                    add_revstring(" ", !Rs),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioA,
                        univ_value(ArgA), !Rs),
                    add_revstring(" ", !Rs),
                    value_to_revstrings_prio(NonCanon, OpTable, MinPrioB,
                        univ_value(ArgB), !Rs),
                    maybe_add_revstring(")", MinTermPrio, OpPrio, !Rs)
                else
                    plain_term_to_revstrings(NonCanon, OpTable, MinTermPrio,
                        Functor, Args, !Rs)
                )
            else
                plain_term_to_revstrings(NonCanon, OpTable, MinTermPrio,
                    Functor, Args, !Rs)
            )
        ;
            ( Args = []
            ; Args = [_, _, _ | _]
            ),
            plain_term_to_revstrings(NonCanon, OpTable, MinTermPrio,
                Functor, Args, !Rs)
        )
    ).

:- pred plain_term_to_revstrings(noncanon_handling, ops.table,
    ops.priority, string, list(univ), revstrings, revstrings).
:- mode plain_term_to_revstrings(in(do_not_allow), in, in, in, in, in, out)
    is det.
:- mode plain_term_to_revstrings(in(canonicalize), in, in, in, in, in, out)
    is det.
:- mode plain_term_to_revstrings(in(include_details_cc), in, in, in, in,
    in, out) is cc_multi.
:- mode plain_term_to_revstrings(in, in, in, in, in, in, out)
    is cc_multi.

plain_term_to_revstrings(NonCanon, OpTable, Priority, Functor, Args, !Rs) :-
    ( if
        Args = [],
        ops.is_op(OpTable, Functor),
        priority_ge(Priority, ops.loosest_op_priority(OpTable))
    then
        add_revstring("(", !Rs),
        add_revstring(term_io.quoted_atom(Functor), !Rs),
        add_revstring(")", !Rs)
    else
        add_revstring(
            term_io.quoted_atom_agt(Functor,
                term_io.maybe_adjacent_to_graphic_token),
            !Rs
        )
    ),
    (
        Args = [Y | Ys],
        add_revstring("(", !Rs),
        arg_to_revstrings(NonCanon, OpTable, Y, !Rs),
        term_args_to_revstrings(NonCanon, OpTable, Ys, !Rs),
        add_revstring(")", !Rs)
    ;
        Args = []
    ).

:- pred maybe_add_revstring(string::in, ops.priority::in, ops.priority::in,
    revstrings::in, revstrings::out) is det.

maybe_add_revstring(String, Priority, OpPrio, !Rs) :-
    ( if priority_lt(OpPrio, Priority) then
        add_revstring(String, !Rs)
    else
        true
    ).

:- pred univ_list_tail_to_revstrings(noncanon_handling, ops.table, univ,
    revstrings, revstrings).
:- mode univ_list_tail_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode univ_list_tail_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode univ_list_tail_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode univ_list_tail_to_revstrings(in, in, in, in, out) is cc_multi.

univ_list_tail_to_revstrings(NonCanon, OpTable, Univ, !Rs) :-
    deconstruct(univ_value(Univ), NonCanon, Functor, _Arity, Args),
    ( if
        Functor = "[|]",
        Args = [ListHead, ListTail]
    then
        add_revstring(", ", !Rs),
        arg_to_revstrings(NonCanon, OpTable, ListHead, !Rs),
        univ_list_tail_to_revstrings(NonCanon, OpTable, ListTail, !Rs)
    else if
        Functor = "[]",
        Args = []
    then
        true
    else
        add_revstring(" | ", !Rs),
        value_to_revstrings(NonCanon, OpTable, univ_value(Univ), !Rs)
    ).

    % Write the remaining arguments.
    %
:- pred term_args_to_revstrings(noncanon_handling, ops.table, list(univ),
    revstrings, revstrings).
:- mode term_args_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode term_args_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode term_args_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode term_args_to_revstrings(in, in, in, in, out) is cc_multi.

term_args_to_revstrings(_, _, [], !Rs).
term_args_to_revstrings(NonCanon, OpTable, [X | Xs], !Rs) :-
    add_revstring(", ", !Rs),
    arg_to_revstrings(NonCanon, OpTable, X, !Rs),
    term_args_to_revstrings(NonCanon, OpTable, Xs, !Rs).

:- pred arg_to_revstrings(noncanon_handling,
    ops.table, univ, revstrings, revstrings).
:- mode arg_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode arg_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode arg_to_revstrings(in(include_details_cc), in, in, in, out) is cc_multi.
:- mode arg_to_revstrings(in, in, in, in, out) is cc_multi.

arg_to_revstrings(NonCanon, OpTable, X, !Rs) :-
    Priority = comma_priority(OpTable),
    value_to_revstrings_prio(NonCanon, OpTable, Priority, univ_value(X), !Rs).

:- pred array_to_revstrings(noncanon_handling, ops.table, array(T),
    revstrings, revstrings).
:- mode array_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode array_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode array_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode array_to_revstrings(in, in, in, in, out) is cc_multi.

array_to_revstrings(NonCanon, OpTable, Array, !Rs) :-
    add_revstring("array(", !Rs),
    value_to_revstrings(NonCanon, OpTable,
        array.to_list(Array) `with_type` list(T), !Rs),
    add_revstring(")", !Rs).

:- pred version_array_to_revstrings(noncanon_handling, ops.table,
    version_array(T), revstrings, revstrings).
:- mode version_array_to_revstrings(in(do_not_allow), in, in, in, out) is det.
:- mode version_array_to_revstrings(in(canonicalize), in, in, in, out) is det.
:- mode version_array_to_revstrings(in(include_details_cc), in, in, in, out)
    is cc_multi.
:- mode version_array_to_revstrings(in, in, in, in, out) is cc_multi.

version_array_to_revstrings(NonCanon, OpTable, Array, !Rs) :-
    add_revstring("version_array(", !Rs),
    value_to_revstrings(NonCanon, OpTable,
        version_array.to_list(Array) `with_type` list(T), !Rs),
    add_revstring(")", !Rs).

:- pred type_desc_to_revstrings(type_desc::in,
    revstrings::in, revstrings::out) is det.

type_desc_to_revstrings(TypeDesc, !Rs) :-
    add_revstring(term_io.quoted_atom(type_name(TypeDesc)), !Rs).

:- pred type_ctor_desc_to_revstrings(type_ctor_desc::in,
    revstrings::in, revstrings::out) is det.

type_ctor_desc_to_revstrings(TypeCtorDesc, !Rs) :-
    type_desc.type_ctor_name_and_arity(TypeCtorDesc, ModuleName,
        Name0, Arity0),
    Name = term_io.quoted_atom(Name0),
    ( if
        ModuleName = "builtin",
        Name = "func"
    then
        % The type ctor that we call `builtin.func/N' takes N + 1 type
        % parameters: N arguments plus one return value. So we need to subtract
        % one from the arity here.
        Arity = Arity0 - 1
    else
        Arity = Arity0
    ),
    ( if ModuleName = "builtin" then
        String = string.format("%s/%d", [s(Name), i(Arity)])
    else
        String = string.format("%s.%s/%d", [s(ModuleName), s(Name), i(Arity)])
    ),
    add_revstring(String, !Rs).

:- pred private_builtin_type_info_to_revstrings(
    private_builtin.type_info::in, revstrings::in, revstrings::out) is det.

private_builtin_type_info_to_revstrings(PrivateBuiltinTypeInfo, !Rs) :-
    private_builtin.unsafe_type_cast(PrivateBuiltinTypeInfo, TypeInfo),
    type_desc.type_info_to_type_desc(TypeInfo, TypeDesc),
    type_desc_to_revstrings(TypeDesc, !Rs).

:- pred det_dynamic_cast(T1::in, T2::out) is det.

det_dynamic_cast(X, Y) :-
    det_univ_to_type(univ(X), Y).

%---------------------------------------------------------------------------%
:- end_module string.to_string.
%---------------------------------------------------------------------------%
