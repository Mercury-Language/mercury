%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2018, 2025 The Mercury team.
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
    % XXX This *should* be a string builder, but *cannot* be a string builder
    % without the operations on string builders having <in,out> modes besides
    % the current <di,uo> modes. This is because in several places in this
    % module, we have code in the conditions of if-then-elses that both
    %
    % - tests whether the input has one of several known shapes, and
    % - converts inputs of each shape to a string *using more than one append*.
    %
    % If each append has a <di,uo> mode, then this whole code cannot be
    % in the condition, since a failure in the condition after an append
    % would require resurrecting an already-destroyed value.
    %
    % We could avoid this by delaying the appends to the string builder
    % to the then-part of the if-then-else, but this would require
    % more complex and slower code. This makes the whole exercise unattractive.
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
    % We need to special-case the builtin types.
    % XXX Are there any that should special-case but don't?
    % There is at least one: univ.
    type_ctor_and_args(type_of(X), TypeCtorDesc, ArgTypeDescs),
    TypeCtorModuleName = type_ctor_module_name(TypeCtorDesc),
    TypeCtorName = type_ctor_name(TypeCtorDesc),
    ( if
        (
            TypeCtorModuleName = "builtin",
            (
                TypeCtorName = "string",
                det_dynamic_cast(X, Str),
                ToAddStr = term_io.quoted_string(Str)
            ;
                TypeCtorName = "character",
                det_dynamic_cast(X, Char),
                ToAddStr = term_io.quoted_char_to_string(Char)
            ;
                TypeCtorName = "float",
                det_dynamic_cast(X, Float),
                ToAddStr = string.float_to_string(Float)
            ;
                TypeCtorName = "int",
                det_dynamic_cast(X, I),
                ToAddStr = string.int_to_string(I)
            ;
                TypeCtorName = "int8",
                det_dynamic_cast(X, I8),
                ToAddStr = string.int8_to_string(I8) ++ "i8"
            ;
                TypeCtorName = "int16",
                det_dynamic_cast(X, I16),
                ToAddStr = string.int16_to_string(I16) ++ "i16"
            ;
                TypeCtorName = "int32",
                det_dynamic_cast(X, I32),
                ToAddStr = string.int32_to_string(I32) ++ "i32"
            ;
                TypeCtorName = "int64",
                det_dynamic_cast(X, I64),
                ToAddStr = string.int64_to_string(I64) ++ "i64"
            ;
                TypeCtorName = "uint",
                det_dynamic_cast(X, U),
                ToAddStr = string.uint_to_string(U) ++ "u"
            ;
                TypeCtorName = "uint8",
                det_dynamic_cast(X, U8),
                ToAddStr = string.uint8_to_string(U8) ++ "u8"
            ;
                TypeCtorName = "uint16",
                det_dynamic_cast(X, U16),
                ToAddStr = string.uint16_to_string(U16) ++ "u16"
            ;
                TypeCtorName = "uint32",
                det_dynamic_cast(X, U32),
                ToAddStr = string.uint32_to_string(U32) ++ "u32"
            ;
                TypeCtorName = "uint64",
                det_dynamic_cast(X, U64),
                ToAddStr = string.uint64_to_string(U64) ++ "u64"
            ;
                TypeCtorName = "c_pointer",
                det_dynamic_cast(X, CPtr),
                ToAddStr = c_pointer_to_string(CPtr)
            )
        ;
            TypeCtorModuleName = "bitmap",
            TypeCtorName = "bitmap",
            det_dynamic_cast(X, Bitmap),
            ToAddStr = term_io.quoted_string(bitmap.to_string(Bitmap))
        ;
            TypeCtorModuleName = "type_desc",
            (
                TypeCtorName = "type_desc",
                det_dynamic_cast(X, TypeDesc),
                ToAddStr = term_io.quoted_atom(type_name(TypeDesc))
            ;
                TypeCtorName = "type_ctor_desc",
                det_dynamic_cast(X, TypeCtorDesc),
                ToAddStr = type_ctor_desc_to_string(TypeCtorDesc)
            )
        ;
            TypeCtorModuleName = "private_builtin",
            TypeCtorName = "type_info",
            ArgTypeDescs = [ElemType],
            % The following code used to be split across two predicates.
            % I (zs) think that this may explain its (probably) unnecessary
            % complexity.
            has_type(Elem, ElemType),
            same_private_builtin_type(PITypeInfo, Elem),
            det_dynamic_cast(X, PITypeInfo),
            private_builtin.unsafe_type_cast(PITypeInfo, TypeInfo),
            type_desc.type_info_to_type_desc(TypeInfo, TypeDesc),
            ToAddStr = term_io.quoted_atom(type_name(TypeDesc))
        )
    then
        add_revstring(ToAddStr, !Rs)
    else if
         (
            TypeCtorModuleName = "array",
            TypeCtorName = "array",
            ArgTypeDescs = [ElemType],
            % Now that we know the element type, we can constrain the type of
            % the variable Array so that we can use det_dynamic_cast.
            has_type(Elem, ElemType),
            same_array_elem_type(Array, Elem),
            det_dynamic_cast(X, Array),
            array_to_revstrings(NonCanon, OpTable, Array, !Rs)
        ;
            TypeCtorModuleName = "version_array",
            TypeCtorName = "version_array",
            ArgTypeDescs = [ElemType],
            has_type(Elem, ElemType),
            same_version_array_elem_type(VersionArray, Elem),
            det_dynamic_cast(X, VersionArray),
            version_array_to_revstrings(NonCanon, OpTable, VersionArray, !Rs)
        )
    then
        % The condition has already done the updates to !Rs.
        true
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

:- func type_ctor_desc_to_string(type_ctor_desc) = string.

type_ctor_desc_to_string(TypeCtorDesc) = Str :-
    type_desc.type_ctor_name_and_arity(TypeCtorDesc, ModuleName,
        Name0, Arity0),
    Name = term_io.quoted_atom(Name0),
    ( if
        ModuleName = "builtin",
        Name = "func"
    then
        % The type ctor that we call `builtin.func/N' takes N + 1 type
        % parameters: N arguments and one return value. So we need to
        % subtract one from the arity here.
        Arity = Arity0 - 1
    else
        Arity = Arity0
    ),
    ( if ModuleName = "builtin" then
        Str = string.format("%s/%d", [s(Name), i(Arity)])
    else
        Str = string.format("%s.%s/%d", [s(ModuleName), s(Name), i(Arity)])
    ).

:- pred det_dynamic_cast(T1::in, T2::out) is det.

det_dynamic_cast(A, B) :-
    ( if dynamic_cast(A, BPrime) then
        B = BPrime
    else
        unexpected($pred, "dynamic_cast failed")
    ).

%---------------------------------------------------------------------------%
:- end_module string.to_string.
%---------------------------------------------------------------------------%
