%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_data.m.
% Main authors: fjh, conway.
%
% This module defines the part of the HLDS that deals with issues related
% to data types, and the representation of values of various types.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_data.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.

:- implementation.

:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The representation of functors.
%

:- interface.

    % A `cons_tag' specifies how a functor and its arguments (if any) are
    % represented. Currently all values are represented as a single word;
    % values which do not fit into a word are represented by a (possibly
    % tagged) pointer to memory on the heap.
    %
    % XXX TYPE_REPN
    % We should consider a scheme that consolidates the following cons_tags
    % under these categories:
    %
    % - user_const_tag (int, float, string, foreign, dummy, shared_local)
    % - system_const_tag (type_info, ... tabling)
    % - ground_term_const_tag
    % - local_args_tag
    % - remote_args_tag
    % - no_or_direct_arg_tag
    % - closure_tag
    %
:- type cons_tag
    % The kinds of constants that may appear in user code.
    --->    int_tag(int_tag)
            % This means the constant is represented as a word containing
            % the specified integer value. This is used for enumerations and
            % character constants, as well as for integer constants of every
            % possible size and signedness.

    ;       float_tag(float)
            % Floats are represented using the MR_float_to_word(),
            % MR_word_to_float(), and MR_float_const() macros. The default
            % implementation of these is to use boxed double-precision floats.

    ;       string_tag(string)
            % Strings are represented using the MR_string_const() macro;
            % in the current implementation, Mercury strings are represented
            % just as C null-terminated strings.

    ;       foreign_tag(foreign_language, string)
            % This means the constant is represented by the string, which is
            % embedded directly in the target language. This is used for
            % foreign enumerations, i.e. those enumeration types that are
            % the subject of a foreign_enum pragma.

    ;       dummy_tag
            % This is for constants that are the only function symbol in their
            % type. Such function symbols contain no information, and thus
            % do not need to be represented at all.

    ;       shared_local_tag_no_args(ptag, local_sectag, lsectag_mask)
            % This is for constants in types that also have non-constants.
            % We allocate a primary tag value to be shared by all the
            % constants, and distinguish the constants from each other
            % using a secondary tag stored in the rest of the word,
            % immediately after the primary tag bits. The second field
            % says how big the secondary tag is. The third field says
            % whether this primary tag values is also shared with
            % some non-constants whose cons_id is shared_local_tag_with_args.
            % If it is, the third field will be lsectag_must_be_masked,
            % otherwise, it will be lsectag_always_rest_of_word. (See below.)
            %
            % Note that the name may sometimes be misleading. While most uses
            % of shared_local_tag_no_args (and shared_local_tag_with_args)
            % *do* share the given primary tag value with another function
            % symbol, it is possible for this not to be the case. In that case,
            % the local secondary tag will occupy zero bits.

    % The kinds of constants that cannot appear in user code,
    % being generated only inside the compiler.

    ;       ground_term_const_tag(int, cons_tag)

    ;       type_info_const_tag(int)
    ;       typeclass_info_const_tag(int)

    ;       type_ctor_info_tag(module_name, string, arity)
            % This is how we refer to type_ctor_info structures represented
            % as global data. The args are the name of the module the type
            % is defined in, and the name of the type, and its arity.

    ;       base_typeclass_info_tag(module_name, class_id, string)
            % This is how we refer to base_typeclass_info structures
            % represented as global data. The first argument is the name
            % of the module containing the instance declaration, the second
            % is the class name and arity, while the third is the string which
            % uniquely identifies the instance declaration (it is made from
            % the type of the arguments to the instance decl).

    ;       deep_profiling_proc_layout_tag(pred_id, proc_id)
            % This is for constants representing procedure descriptions for
            % deep profiling.

    ;       tabling_info_tag(pred_id, proc_id)
            % This is how we refer to the global structures containing
            % tabling pointer variables and related data. The word just
            % contains the address of the global struct.

    ;       table_io_entry_tag(pred_id, proc_id)
            % This is for constants representing the structure that allows us
            % to decode the contents of the answer block containing the
            % headvars of I/O primitives.

    % The kinds of non-constants that may appear in user code.

    ;       remote_args_tag(remote_args_tag_info)

    ;       local_args_tag(local_args_tag_info)
            % This cons_id is a variant of shared_local_tag_no_args that is
            % intended for function symbols that *do* have arguments,
            % arguments that fit into a single word *after* the primary
            % and the local secondary tag (if there is a sectag).
            % If a primary tag value has any such cons_ids allocated for it,
            % then the bits in a word after the primary tag may include
            % these arguments, so accessing the secondary tag (if any)
            % requires masking off all the non-sectag bits.

    ;       no_tag
            % This is for types with a single functor of arity one. In this
            % case, we don't need to store the functor, and instead we store
            % the argument directly.

    ;       direct_arg_tag(ptag)
            % This is for functors which can be distinguished with just a
            % primary tag. The primary tag says which of the type's functors
            % (which must have arity 1) this word represents. However, the
            % body of the word is not a pointer to a cell holding the argument;
            % it IS the value of that argument, which must be an untagged
            % pointer to a cell.

    % The kinds of non-constants that cannot appear in user code,
    % being generated only inside the compiler.

    ;       closure_tag(pred_id, proc_id, lambda_eval_method).
            % Higher-order pred closures tags. These are represented as
            % a pointer to an argument vector. For closures with
            % lambda_eval_method `normal', the first two words of the argument
            % vector hold the number of args and the address of the procedure
            % respectively. The remaining words hold the arguments.

:- type int_tag
    --->    int_tag_int(int)
            % This means the constant is represented just as a word containing
            % the specified integer value. This is used for enumerations and
            % character constants as well as for int constants.

    ;       int_tag_uint(uint)
            % This means the constant is represented just as a word containing
            % the specified unsigned integer value. This is used for uint
            % constants.

    ;       int_tag_int8(int8)
    ;       int_tag_uint8(uint8)
    ;       int_tag_int16(int16)
    ;       int_tag_uint16(uint16)
    ;       int_tag_int32(int32)
    ;       int_tag_uint32(uint32)
    ;       int_tag_int64(int64)
    ;       int_tag_uint64(uint64).

    % The type `ptag' holds a primary tag value.
    % It consists of 2 bits on 32 bit machines and 3 bits on 64 bit machines.
    % If we are not using primary tags to distinguish function symbols
    % from each other, the ptag will always be 0u8.
    %
:- type ptag
    --->    ptag(uint8).

:- type local_args_tag_info
    --->    local_args_only_functor
            % There are no other function symbols in the type.
            % The ptag is implicitly zero, and there is no local sectag.
    ;       local_args_not_only_functor(ptag, local_sectag).
            % There are other function symbols in the type.
            % The arguments specify the ptag and the local sectag (if any).

:- type local_sectag
    --->    local_sectag(
                lsectag_value       :: uint,

                % The ptag and the local sectag together.
                lsectag_prim_sec    :: uint,

                % The size and mask of the sectag. If the number of bits
                % is zero, then there is no local sectag.
                lsectag_bits        :: sectag_bits
            ).

:- type lsectag_mask
    --->    lsectag_always_rest_of_word
            % All the local secondary tags in this type are the "traditional"
            % kind of local tags, which occupy the whole of the word
            % after the primary tag. In other words, there is never any
            % argument packed after the primary and secondary tag bits.
            % Therefore computing the secondary tag needs only the primary tag
            % bits to be masked off.
    ;       lsectag_must_be_masked.
            % At least one of the functors of this type is represented
            % by a local secondary tag that *is* followed by packed arguments.
            % Therefore computing the secondary tag needs not only
            % the primary tag bits to be masked off, but the arguments as well.
            % The sectag bits argument gives the mask to apply to the
            % post-primary-tag part of the word.

:- type remote_args_tag_info
    --->    remote_args_only_functor
            % This is for functors in types that have only a single functor.
            % For these types, we don't need any tags, primary or secondary,
            % to distinguish between function symbols. However, we do have
            % to decide what to put into bottom two or three bits (on 32-
            % and 64-bit systems respectively) of the representation of
            % every term, the area reserved for the primary tag. For these
            % functors, we put zeroes there, which is equivalent to having
            % zero as a ptag. The rest of the word is a pointer to the
            % argument vector, which may start with zero or more type_infos
            % and/or typeclass_infos added by the polymorphism transformation.
            %
            % This kind of tag is used by both the low level and the
            % the high level data representation.

    ;       remote_args_unshared(ptag)
            % This is for non-constants functors which can be distinguished
            % from other functors in their type with just the primary tag.
            % Terms whose functor has a cons_tag using this value are
            % represented by a word whose bottom two or three bits contain
            % the given ptag, with the rest of the word being a pointer to the
            % argument vector, which may start with zero or more type_infos
            % and/or typeclass_infos added by the polymorphism transformation.
            %
            % This kind of tag is used only by the low level data
            % representation.

    ;       remote_args_shared(ptag, remote_sectag)
            % This is for non-constants functors which cannot be distinguished
            % from other functors in their type with just the primary tag,
            % but need a remote secondary tag as well. Terms whose functor
            % has a cons_tag using this value are represented by a word
            % whose bottom two or three bits contain the given ptag, with
            % the rest of the word being a pointer to the tagword, the word
            % containing the remote secondary tag. The second argument gives
            % both the value and the size of the remote sectag.
            %
            % If the size is rsectag_word, the remote sectag will occupy
            % the whole tagword, which may then be followed by zero or more
            % type_infos and/or typeclass_infos added by the polymorphism
            % transformation, and then the arguments themselves.
            %
            % If the size is rsectag_bits, the remote sectag will occupy
            % the bottom sectag_num_bits bits of the  tagword, with the
            % rest of the word containing an initial subsequence of zero or
            % more subword-sized arguments.
            %
            % If the number of subword-size arguments packed in the tagword
            % is zero, then the tagword may be followed by type_infos and/or
            % typeclass_infos added by the polymorphism transformation,
            % followed by the arguments themselves, as usual. However,
            % if the number of subword-size arguments packed in the tagword
            % is *not*, zero, then these must be followed immediately by
            % the rest of the arguments (if any); they may *not* be followed by
            % any such type_infos and/or typeclass_infos added by polymorphism.
            % (The implementation uses this implication in reverse: if we
            % *would* need to add type_infos and/or typeclass_infos, then
            % du_type_layout will choose not to pack any arguments next
            % to the remote sectag, even if it otherwise could do so.)
            %
            % This restriction preserves the old invariant that the
            % arguments added by polymorphism go before all user-visible
            % arguments. Loosening that invariant would require substantial
            % changes to polymorphism.m.
            %
            % Note that all the functors sharing a given ptag value must agree
            % on the exact size of the remote sectag (i.e. whether it is
            % a whole word or not, and if not, how many bits it has),
            % since tests of the form X = f(...) need to know how many
            % of the bits of the remote tagword to look at.
            %
            % This kind of tag is used only by the low level data
            % representation.

    ;       remote_args_ctor(uint).
            % The high level data representation does not use either primary
            % or secondary tags. Instead, the various function symbols
            % of the type are distinguished by an integer stored in a field
            % named "data" of the base class (representing terms of the type),
            % which is inherited by each of the subclasses (each of which
            % represents terms whose top function symbol is a given functor).
            % The argument gives the value of "data" for this function symbol.
            %
            % This kind of tag is used only by the high level data
            % representation.
            %
            % XXX ARG_PACK Maybe we should include include MaybeCtorName,
            % the output of the first few lines of the predicate
            % ml_generate_dynamic_construct_compound, in both
            % remote_args_tag_infos that may be used by the high level
            % representation, so that the constructor name, if any,
            % is computed just once for each function symbol.
            % This would require separating the low and high level data
            % uses of remote_args_only_functor.

:- type remote_sectag
    --->    remote_sectag(
                rsectag_value       :: uint,
                rsectag_size        :: rsectag_size
            ).

:- type rsectag_size
    --->    rsectag_word
    ;       rsectag_subword(sectag_bits).

:- type sectag_bits
    --->    sectag_bits(
                % A local secondary tag is always next to the primary tag.
                % A remote secondary tag is always at the start of the word
                % at offset 0 in the memory cell.
                sectag_num_bits     :: uint8,
                sectag_mask         :: uint
            ).

    % Return the primary tag, if any, for a cons_tag.
    % A return value of `no' means the primary tag is unknown.
    % A return value of `yes(N)' means the primary tag is N.
    % (`yes(0)' also corresponds to the case where there no primary tag.)
    %
:- func get_maybe_primary_tag(cons_tag) = maybe(ptag).

    % Return the secondary tag, if any, for a cons_tag.
    % A return value of `no' means there is no secondary tag.
    %
:- func get_maybe_secondary_tag(cons_tag) = maybe(int).

%---------------------%

    % A cons_id together with its tag.
    %
:- type tagged_cons_id
    --->    tagged_cons_id(cons_id, cons_tag).

    % Return the tag inside a tagged_cons_id.
    %
:- func project_tagged_cons_id_tag(tagged_cons_id) = cons_tag.

%---------------------------------------------------------------------------%

:- implementation.

get_maybe_primary_tag(Tag) = MaybePtag :-
    (
        % In some of the cases where we return `no' here,
        % it would probably be OK to return `yes(0)'.
        % But it's safe to be conservative...
        ( Tag = int_tag(_)
        ; Tag = float_tag(_)
        ; Tag = string_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = no_tag
        ; Tag = dummy_tag
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ),
        MaybePtag = no
    ;
        Tag = ground_term_const_tag(_, SubTag),
        MaybePtag = get_maybe_primary_tag(SubTag)
    ;
        ( Tag = direct_arg_tag(Ptag)
        ; Tag = shared_local_tag_no_args(Ptag, _, _)
        ),
        MaybePtag = yes(Ptag)
    ;
        Tag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            MaybePtag = yes(ptag(0u8))
        ;
            ( RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ; RemoteArgsTagInfo = remote_args_shared(Ptag, _)
            ),
            MaybePtag = yes(Ptag)
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % XXX This is a lie; the high level data representation does not
            % use primary tags. Our caller should never call us with this
            % value of RemoteArgsTagInfo.
            MaybePtag = yes(ptag(0u8))
        )
    ;
        Tag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            Ptag = ptag(0u8)
        ;
            LocalArgsTagInfo = local_args_not_only_functor(Ptag, _LocalSectag)
        ),
        MaybePtag = yes(Ptag)
    ).

get_maybe_secondary_tag(Tag) = MaybeSectag :-
    % XXX Return a uint?
    (
        ( Tag = int_tag(_)
        ; Tag = float_tag(_)
        ; Tag = string_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = type_info_const_tag(_)
        ; Tag = typeclass_info_const_tag(_)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_entry_tag(_, _)
        ; Tag = no_tag
        ; Tag = dummy_tag
        ; Tag = direct_arg_tag(_PrimaryTag)
        ),
        MaybeSectag = no
    ;
        Tag = ground_term_const_tag(_, SubTag),
        MaybeSectag = get_maybe_secondary_tag(SubTag)
    ;
        Tag = shared_local_tag_no_args(_Ptag, LocalSectag, _),
        LocalSectag = local_sectag(SectagUint, _, _),
        Sectag = uint.cast_to_int(SectagUint),
        MaybeSectag = yes(Sectag)
    ;
        Tag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            Sectag = 0
        ;
            LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag),
            LocalSectag = local_sectag(SectagUint, _, _),
            Sectag = uint.cast_to_int(SectagUint)
        ),
        MaybeSectag = yes(Sectag)
    ;
        Tag = remote_args_tag(RemoteArgsTagInfo),
        (
            ( RemoteArgsTagInfo = remote_args_only_functor
            ; RemoteArgsTagInfo = remote_args_unshared(_)
            ),
            MaybeSectag = no
        ;
            RemoteArgsTagInfo = remote_args_shared(_Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, _),
            Sectag = uint.cast_to_int(SectagUint),
            MaybeSectag = yes(Sectag)
        ;
            RemoteArgsTagInfo = remote_args_ctor(Data),
            % XXX This is a sort-of lie; the high level data representation
            % does not use secondary tags the same way as the low level
            % data representation does. Our caller should never call us
            % with this value of RemoteArgsTagInfo.
            MaybeSectag = yes(uint.cast_to_int(Data))
        )
    ).

project_tagged_cons_id_tag(TaggedConsId) = Tag :-
    TaggedConsId = tagged_cons_id(_, Tag).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The type table.
%

:- interface.

    % The symbol table for types. Conceptually, it is a map from type_ctors
    % to hlds_type_defns, but the implementation may be different for
    % efficiency.
    %
:- type type_table.

:- func init_type_table = type_table.

:- pred add_type_ctor_defn(type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out) is det.

:- pred replace_type_ctor_defn(type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out) is det.

:- pred search_type_ctor_defn(type_table::in, type_ctor::in,
    hlds_type_defn::out) is semidet.
:- pred lookup_type_ctor_defn(type_table::in, type_ctor::in,
    hlds_type_defn::out) is det.

:- pred get_all_type_ctor_defns(type_table::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

:- pred set_all_type_ctor_defns(assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out, type_table::out) is det.

:- pred foldl_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T)::
        in(pred(in, in, in, out) is det),
    type_table::in, T::in, T::out) is det.

:- pred foldl2_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T, U, U)::
        in(pred(in, in, in, out, in, out) is det),
    type_table::in, T::in, T::out, U::in, U::out) is det.

:- pred foldl3_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, T, T, U, U, V, V)::
        in(pred(in, in, in, out, in, out, in, out) is det),
    type_table::in, T::in, T::out, U::in, U::out, V::in, V::out) is det.

:- pred map_foldl_over_type_ctor_defns(
    pred(type_ctor, hlds_type_defn, hlds_type_defn, T, T)::
        in(pred(in, in, out, in, out) is det),
    type_table::in, type_table::out, T::in, T::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type type_table == map(string, type_ctor_table).

:- type type_ctor_table == map(type_ctor, hlds_type_defn).

init_type_table = map.init.

add_type_ctor_defn(TypeCtor, TypeDefn, !TypeTable) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    ( if map.search(!.TypeTable, Name, TypeCtorTable0) then
        map.det_insert(TypeCtor, TypeDefn, TypeCtorTable0, TypeCtorTable),
        map.det_update(Name, TypeCtorTable, !TypeTable)
    else
        TypeCtorTable = map.singleton(TypeCtor, TypeDefn),
        map.det_insert(Name, TypeCtorTable, !TypeTable)
    ).

replace_type_ctor_defn(TypeCtor, TypeDefn, !TypeTable) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.lookup(!.TypeTable, Name, TypeCtorTable0),
    map.det_update(TypeCtor, TypeDefn, TypeCtorTable0, TypeCtorTable),
    map.det_update(Name, TypeCtorTable, !TypeTable).

search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.search(TypeTable, Name, TypeCtorTable),
    map.search(TypeCtorTable, TypeCtor, TypeDefn).

lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) :-
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    map.lookup(TypeTable, Name, TypeCtorTable),
    map.lookup(TypeCtorTable, TypeCtor, TypeDefn).

%---------------------%

get_all_type_ctor_defns(TypeTable, TypeCtorsDefns) :-
    map.foldl_values(get_all_type_ctor_defns_2, TypeTable, [], TypeCtorsDefns).

:- pred get_all_type_ctor_defns_2(type_ctor_table::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

get_all_type_ctor_defns_2(TypeCtorTable, !TypeCtorsDefns) :-
    map.to_assoc_list(TypeCtorTable, NameTypeCtorsDefns),
    !:TypeCtorsDefns = NameTypeCtorsDefns ++ !.TypeCtorsDefns.

%---------------------%

set_all_type_ctor_defns(TypeCtorsDefns, SortedTypeCtorsDefns, TypeTable) :-
    list.sort(compare_type_ctor_defns_by_name,
        TypeCtorsDefns, SortedTypeCtorsDefns),
    gather_type_ctors_by_name(SortedTypeCtorsDefns,
        [], RevTypeCtorsDefnsByName),
    map.from_rev_sorted_assoc_list(RevTypeCtorsDefnsByName, TypeTable).

:- pred compare_type_ctor_defns_by_name(
    pair(type_ctor, hlds_type_defn)::in, pair(type_ctor, hlds_type_defn)::in,
    comparison_result::out) is det.

compare_type_ctor_defns_by_name(PairA, PairB, Result) :-
    PairA = TypeCtorA - _TypeDefnA,
    PairB = TypeCtorB - _TypeDefnB,
    TypeCtorA = type_ctor(SymNameA, _ArityA),
    TypeCtorB = type_ctor(SymNameB, _ArityB),
    NameA = unqualify_name(SymNameA),
    NameB = unqualify_name(SymNameB),
    compare(Result, NameA, NameB).

:- pred gather_type_ctors_by_name(assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(string, type_ctor_table)::in,
    assoc_list(string, type_ctor_table)::out) is det.

gather_type_ctors_by_name([], !RevTypeCtorsDefnsByName).
gather_type_ctors_by_name([TypeCtorTypeDefn | TypeCtorsTypeDefns0],
        !RevTypeCtorsDefnsByName) :-
    TypeCtorTypeDefn = TypeCtor - TypeDefn,
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    TypeCtorTable0 = map.singleton(TypeCtor, TypeDefn),
    gather_type_ctors_this_name(Name, TypeCtorTable0, TypeCtorTable,
        TypeCtorsTypeDefns0, TypeCtorsTypeDefns),
    !:RevTypeCtorsDefnsByName =
        [Name - TypeCtorTable | !.RevTypeCtorsDefnsByName],
    gather_type_ctors_by_name(TypeCtorsTypeDefns, !RevTypeCtorsDefnsByName).

:- pred gather_type_ctors_this_name(string::in,
    type_ctor_table::in, type_ctor_table::out,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

gather_type_ctors_this_name(_, !TypeCtorTable, [], []).
gather_type_ctors_this_name(ThisName, !TypeCtorTable, 
        [TypeCtorTypeDefn | TypeCtorsTypeDefns], LeftOverTypeCtorsTypeDefns) :-
    TypeCtorTypeDefn = TypeCtor - TypeDefn,
    TypeCtor = type_ctor(SymName, _Arity),
    Name = unqualify_name(SymName),
    ( if Name = ThisName then
        map.det_insert(TypeCtor, TypeDefn, !TypeCtorTable),
        gather_type_ctors_this_name(ThisName, !TypeCtorTable, 
            TypeCtorsTypeDefns, LeftOverTypeCtorsTypeDefns)
    else
        LeftOverTypeCtorsTypeDefns = [TypeCtorTypeDefn | TypeCtorsTypeDefns]
    ).

%---------------------%

foldl_over_type_ctor_defns(Pred, TypeTable, !Acc) :-
    map.foldl_values(foldl_over_type_ctor_defns_2(Pred), TypeTable, !Acc).

:- pred foldl_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T)::
        in(pred(in, in, in, out) is det),
    type_ctor_table::in, T::in, T::out) is det.

foldl_over_type_ctor_defns_2(Pred, TypeCtorTable, !Acc) :-
    map.foldl(Pred, TypeCtorTable, !Acc).

foldl2_over_type_ctor_defns(Pred, TypeTable, !AccA, !AccB) :-
    map.foldl2_values(foldl2_over_type_ctor_defns_2(Pred), TypeTable,
        !AccA, !AccB).

:- pred foldl2_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T, U, U)::
        in(pred(in, in, in, out, in, out) is det),
    type_ctor_table::in, T::in, T::out, U::in, U::out) is det.

foldl2_over_type_ctor_defns_2(Pred, TypeCtorTable, !AccA, !AccB) :-
    map.foldl2(Pred, TypeCtorTable, !AccA, !AccB).

foldl3_over_type_ctor_defns(Pred, TypeTable, !AccA, !AccB, !AccC) :-
    map.foldl3_values(foldl3_over_type_ctor_defns_2(Pred), TypeTable,
        !AccA, !AccB, !AccC).

:- pred foldl3_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, T, T, U, U, V, V)::
        in(pred(in, in, in, out, in, out, in, out) is det),
    type_ctor_table::in, T::in, T::out, U::in, U::out, V::in, V::out) is det.

foldl3_over_type_ctor_defns_2(Pred, TypeCtorTable, !AccA, !AccB, !AccC) :-
    map.foldl3(Pred, TypeCtorTable, !AccA, !AccB, !AccC).

map_foldl_over_type_ctor_defns(Pred, !TypeTable, !Acc) :-
    map.map_foldl(map_foldl_over_type_ctor_defns_2(Pred), !TypeTable, !Acc).

:- pred map_foldl_over_type_ctor_defns_2(
    pred(type_ctor, hlds_type_defn, hlds_type_defn, T, T)::
        in(pred(in, in, out, in, out) is det),
    string::in, type_ctor_table::in, type_ctor_table::out,
    T::in, T::out) is det.

map_foldl_over_type_ctor_defns_2(Pred, _Name, !TypeCtorTable, !Acc) :-
    map.map_foldl(Pred, !TypeCtorTable, !Acc).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Type definitions.
%

:- interface.

    % Have we reported an error for this type definition yet?
:- type type_defn_prev_errors
    --->    type_defn_no_prev_errors
    ;       type_defn_prev_errors.

    % An `hlds_type_body' holds the body of a type definition:
    % du = discriminated union, eqv_type = equivalence type (a type defined
    % to be equivalent to some other type), and solver_type.
    %
:- type hlds_type_body
    --->    hlds_du_type(
                % The ctors for this type.
                du_type_ctors               :: one_or_more(constructor),

                % Does this type have user-defined equality and comparison
                % predicates?
                du_type_canonical           :: maybe_canonical,

                % Information about the representation of the type.
                % This field is filled in (i.e. it is set to yes(...))
                % during the decide_type_repns pass.
                du_type_repn                :: maybe(du_type_repn),

                % Are there `:- pragma foreign' type declarations for
                % this type? We need to know when we are generating e.g.
                % optimization interface files, because we want to make
                % such files valid in all grades, not just the grade that is
                % currently selected grade. In this case, it is possible
                % for this field to be a yes(...) wrapped around a foreign
                % type body that applies to the current grade, which means
                % that if we were generating code for this module,
                % the representation we would use for this type would be
                % the *foreign* type, not the Mercury type.
                %
                % If we are generating code, this field will be yes(...)
                % *only* if the foreign type definitions do not apply
                % to the current target language, so the type representation
                % we want to use for this type is the one in the du_type_repn
                % field. If there *is* a foreign type definition that is valid
                % for the current target language for this type, then
                % add_type.m will set the body of this type to
                % hlds_foreign_type, not hlds_du_type.
                du_type_is_foreign_type     :: maybe(foreign_type_body)
            )
    ;       hlds_eqv_type(mer_type)
    ;       hlds_foreign_type(foreign_type_body)
    ;       hlds_solver_type(type_details_solver)
    ;       hlds_abstract_type(type_details_abstract).

    % This is how type, modes and constructors are represented. The parts that
    % are not defined here (i.e. type_param, constructor, type, inst and mode)
    % are represented in the same way as in parse tree, and are defined there.
    %
    % An hlds_type_defn holds the information about a type definition.
:- type hlds_type_defn.

:- pred create_hlds_type_defn(tvarset::in, list(type_param)::in,
    tvar_kind_map::in, hlds_type_body::in, bool::in,
    type_status::in, need_qualifier::in, type_defn_prev_errors::in,
    prog_context::in, hlds_type_defn::out) is det.

:- pred get_type_defn_tvarset(hlds_type_defn::in, tvarset::out) is det.
:- pred get_type_defn_tparams(hlds_type_defn::in, list(type_param)::out)
    is det.
:- pred get_type_defn_kind_map(hlds_type_defn::in, tvar_kind_map::out) is det.
:- pred get_type_defn_body(hlds_type_defn::in, hlds_type_body::out) is det.
:- pred get_type_defn_status(hlds_type_defn::in, type_status::out) is det.
:- pred get_type_defn_in_exported_eqv(hlds_type_defn::in, bool::out) is det.
:- pred get_type_defn_ctors_need_qualifier(hlds_type_defn::in,
    need_qualifier::out) is det.
:- pred get_type_defn_prev_errors(hlds_type_defn::in,
    type_defn_prev_errors::out) is det.
:- pred get_type_defn_context(hlds_type_defn::in, prog_context::out) is det.

:- pred set_type_defn_body(hlds_type_body::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_tvarset(tvarset::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_status(type_status::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_in_exported_eqv(bool::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.
:- pred set_type_defn_prev_errors(type_defn_prev_errors::in,
    hlds_type_defn::in, hlds_type_defn::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type hlds_type_defn
    --->    hlds_type_defn(
                % Note that the first three of these fields are duplicated
                % in the hlds_cons_defns of the data constructors of the type
                % (if any).

                % Names of the type variables, if any.
                type_defn_tvarset           :: tvarset,

                % Formal type parameters.
                type_defn_params            :: list(type_param),

                % The kinds of the formal parameters.
                type_defn_kinds             :: tvar_kind_map,

                % The definition of the type.
                type_defn_body              :: hlds_type_body,

                % Does the type constructor appear on the right hand side
                % of a type equivalence defining a type that is visible from
                % outside this module? If yes, equiv_type_hlds may generate
                % references to this type constructor's unify and compare preds
                % from other modules even if the type is otherwise local
                % to the module, so we can't make their implementations private
                % to the module.
                %
                % Meaningful only after the equiv_type_hlds pass.
                type_defn_in_exported_eqv   :: bool,

                % Is the type defined in this module, and if yes,
                % is it exported.
                type_defn_status            :: type_status,

                % Do uses of the type's constructors need to be qualified?
                type_defn_ctors_need_qualifier :: need_qualifier,

                % Have we reported an error for this type definition yet?
                % If yes, then don't emit any more errors for it, since they
                % are very likely to be due to the compiler's incomplete
                % recovery from the previous error.
                type_defn_prev_errors       :: type_defn_prev_errors,

                % The location of this type definition in the original
                % source code.
                type_defn_context           :: prog_context
            ).

create_hlds_type_defn(Tvarset, Params, Kinds, TypeBody, InExportedEqv,
        TypeStatus, NeedQual, PrevErrors, Context, Defn) :-
    Defn = hlds_type_defn(Tvarset, Params, Kinds, TypeBody, InExportedEqv,
        TypeStatus, NeedQual, PrevErrors, Context).

get_type_defn_tvarset(Defn, X) :-
    X = Defn ^ type_defn_tvarset.
get_type_defn_tparams(Defn, X) :-
    X = Defn ^ type_defn_params.
get_type_defn_kind_map(Defn, X) :-
    X = Defn ^ type_defn_kinds.
get_type_defn_body(Defn, X) :-
    X = Defn ^ type_defn_body.
get_type_defn_status(Defn, X) :-
    X = Defn ^ type_defn_status.
get_type_defn_in_exported_eqv(Defn, X) :-
    X = Defn ^ type_defn_in_exported_eqv.
get_type_defn_ctors_need_qualifier(Defn, X) :-
    X = Defn ^ type_defn_ctors_need_qualifier.
get_type_defn_prev_errors(Defn, X) :-
    X = Defn ^ type_defn_prev_errors.
get_type_defn_context(Defn, X) :-
    X = Defn ^ type_defn_context.

set_type_defn_body(X, !Defn) :-
    !Defn ^ type_defn_body := X.
set_type_defn_tvarset(X, !Defn) :-
    !Defn ^ type_defn_tvarset := X.
set_type_defn_status(X, !Defn) :-
    !Defn ^ type_defn_status := X.
set_type_defn_in_exported_eqv(X, !Defn) :-
    !Defn ^ type_defn_in_exported_eqv := X.
set_type_defn_prev_errors(X, !Defn) :-
    !Defn ^ type_defn_prev_errors := X.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The representation of du (discriminated union) types.
%

:- interface.

:- type du_type_repn
    --->    du_type_repn(
                % This field contains the same constructors as the
                % du_type_ctors field of the hlds_du_type functor,
                % but in a form in which has representation information
                % for both the constructors and their arguments.
                dur_ctor_repns              :: list(constructor_repn),

                % The cons_ctor_map field maps the name of each constructor
                % of the type to
                %
                % - either the one constructor with that name in the type ctor
                %   (the usual case), or to
                % - the list of two or more constructors that share the name
                %   in the type, which must all have different arities.
                %
                % This allows the same lookups as a map from cons_id to
                % constructor, but is better because the comparisons
                % at each node in the map are cheaper.
                %
                % It is an invariant that the every constructor_repn in the
                % dur_ctor_repns field occurs as a value in this map exactly
                % once, and vice versa.
                dur_ctor_map                :: ctor_name_to_repn_map,

                dur_cheaper_tag_test        :: maybe_cheaper_tag_test,

                % Is this type an enumeration or a dummy type?
                dur_kind                    :: du_type_kind,

                % Direct argument functors.
                % XXX TYPE_REPN Include this information in the
                % constructor_repns in the dur_ctor_repns and dur_ctor_map
                % fields.
                % The maybe() wrapper looks to be unnecessary, but we
                % currently use it to allow the representation of
                % "where direct_arg is []" annotations on types,
                % such as in tests/invalid/where_direct_arg.m.
                dur_direct_arg_ctors        :: maybe(list(sym_name_and_arity))
            ).

:- type constructor_repn
    --->    ctor_repn(
                % The ordinal number of the functor. The first functor
                % in a type definition has ordinal number 0.
                cr_ordinal          :: uint32,

                % Existential constraints, if any.
                % It is an invariant that this will be no_exist_constraints
                % if the list of arguments is empty.
                cr_maybe_exist      :: maybe_cons_exist_constraints,

                % The cons_id should be cons(SymName, Arity, TypeCtor)
                % for user-defined types, and tuple_cons(Arity) for the
                % system-defined tuple types.
                cr_name             :: sym_name,

                cr_tag              :: cons_tag,

                cr_args             :: list(constructor_arg_repn),

                % We precompute the number of arguments once, to save having
                % to recompute it many times later.
                cr_num_args         :: int,

                cr_context          :: prog_context
            ).

:- type constructor_arg_repn
    --->    ctor_arg_repn(
                car_field_name      :: maybe(ctor_field_name),
                car_type            :: mer_type,
                car_pos_width       :: arg_pos_width,
                car_context         :: prog_context
            ).

:- type du_type_kind
    --->    du_type_kind_mercury_enum
    ;       du_type_kind_foreign_enum(
                dtkfe_language      :: foreign_language
            )
    ;       du_type_kind_direct_dummy
            % This du type has one function symbol with no arguments.
            % We call such types *direct* dummy types to distinguish them
            % from notag types that become dummy from having their
            % argument type being a dummy type.
    ;       du_type_kind_notag(
                % A notag type is a dummy type if and only if the type it wraps
                % is a dummy type.
                dtkn_functor_name   :: sym_name,
                dtkn_arg_type       :: mer_type,
                dtkn_maybe_arg_name :: maybe(string)
            )
    ;       du_type_kind_general.

:- type maybe_cheaper_tag_test
    --->    no_cheaper_tag_test
    ;       cheaper_tag_test(
                more_expensive_cons_id  :: cons_id,
                more_expensive_cons_tag :: cons_tag,
                less_expensive_cons_id  :: cons_id,
                less_expensive_cons_tag :: cons_tag
            ).

:- func get_maybe_cheaper_tag_test(hlds_type_body) = maybe_cheaper_tag_test.

    % The ctor_name_to_repn_map type maps each constructor in a
    % discriminated union type to the information that describes how
    % terms with that constructor are represented. The representation
    % information includes not just the constructor's cons_tag,
    % but also information about the representation of its arguments.
    %
    % The map is from the name of the constructor to the (usually one,
    % sometimes two or more) constructors with that name; after the lookup,
    % code should search the one_or_more to look for the constructor
    % with the right arity.
    %
:- type ctor_name_to_repn_map == map(string, one_or_more(constructor_repn)).

:- pred insert_ctor_repn_into_map(constructor_repn::in,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

    % Return the cons_ids for the given data constructors of the give type
    % constructor, in a sorted order.
    %
:- func constructor_cons_ids(type_ctor, list(constructor)) = list(cons_id).

%---------------------------------------------------------------------------%

:- implementation.

get_maybe_cheaper_tag_test(TypeBody) = CheaperTagTest :-
    (
        TypeBody = hlds_du_type(_, _, MaybeRepn, _),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn),
            CheaperTagTest = Repn ^ dur_cheaper_tag_test
        )
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(_)
        ),
        CheaperTagTest = no_cheaper_tag_test
    ).

%---------------------%

insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap) :-
    SymName = CtorRepn ^ cr_name,
    Name = unqualify_name(SymName),
    ( if map.search(!.CtorRepnMap, Name, OldCtorRepns) then
        OldCtorRepns = one_or_more(FirstOldCtorRepn, LaterOldCtorRepns),
        CtorRepns = one_or_more(CtorRepn,
            [FirstOldCtorRepn | LaterOldCtorRepns]),
        map.det_update(Name, CtorRepns, !CtorRepnMap)
    else
        map.det_insert(Name, one_or_more(CtorRepn, []), !CtorRepnMap)
    ).

%---------------------%

constructor_cons_ids(TypeCtor, Ctors) = SortedConsIds :-
    gather_constructor_cons_ids(TypeCtor, Ctors, [], ConsIds),
    list.sort(ConsIds, SortedConsIds).

:- pred gather_constructor_cons_ids(type_ctor::in, list(constructor)::in,
    list(cons_id)::in, list(cons_id)::out) is det.

gather_constructor_cons_ids(_TypeCtor, [], !ConsIds).
gather_constructor_cons_ids(TypeCtor, [Ctor | Ctors], !ConsIds) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor),
    !:ConsIds = [ConsId | !.ConsIds],
    gather_constructor_cons_ids(TypeCtor, Ctors, !ConsIds).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The representation of foreign types.
%

:- interface.

:- type foreign_type_body
    --->    foreign_type_body(
                c       :: foreign_type_lang_body(c_foreign_type),
                java    :: foreign_type_lang_body(java_foreign_type),
                csharp  :: foreign_type_lang_body(csharp_foreign_type),
                erlang  :: foreign_type_lang_body(erlang_foreign_type)
            ).

:- type foreign_type_lang_body(T) == maybe(type_details_foreign(T)).

    % Check asserted properties of a foreign type.
    %
:- pred asserted_can_pass_as_mercury_type(foreign_type_assertions::in)
    is semidet.
:- pred asserted_stable(foreign_type_assertions::in) is semidet.
:- pred asserted_word_aligned_pointer(foreign_type_assertions::in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

asserted_can_pass_as_mercury_type(foreign_type_assertions(Set)) :-
    (
        set.contains(Set, foreign_type_can_pass_as_mercury_type)
    ;
        set.contains(Set, foreign_type_word_aligned_pointer)
    ).

asserted_stable(Assertions) :-
    Assertions = foreign_type_assertions(Set),
    set.contains(Set, foreign_type_stable),
    asserted_can_pass_as_mercury_type(Assertions).

asserted_word_aligned_pointer(foreign_type_assertions(Set)) :-
    set.contains(Set, foreign_type_word_aligned_pointer).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The notag type table.
%

:- interface.

    % The type definitions for no_tag types have information mirrored in a
    % separate table for faster lookups. mode_util.mode_to_top_functor_mode
    % makes heavy use of type_util.type_is_no_tag_type.
    %
:- type no_tag_type
    --->    no_tag_type(
                list(type_param),   % Formal type parameters.
                sym_name,           % Constructor name.
                mer_type            % Argument type.
            ).

    % A type_ctor essentially contains three components. The raw name
    % of the type constructor, its module qualification, and its arity.
    % I (zs) tried replacing this table with a two-stage map (from raw name
    % to a subtable that itself mapped the full type_ctor to no_tag_type,
    % in an attempt to make the main part of the looked use cheaper
    % comparisons, on just raw strings. However, this change effectively led
    % to no change in performance.
:- type no_tag_type_table == map(type_ctor, no_tag_type).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% This does not belong here, but it does not seem to belong anywhere else
% either.
%

:- interface.

    % The atomic variants of the Boehm gc allocator calls (e.g.
    % GC_malloc_atomic instead of GC_malloc) may yield slightly faster code
    % since atomic blocks are not scanned for included pointers. However,
    % this makes them safe to use *only* if the block allocated this way
    % can never contain any pointer the Boehm collector would be interested
    % in tracing. In particular, even if the cell initially contains no
    % pointers, we must still use may_not_use_atomic_alloc for it if the cell
    % could possibly be reused later by compile-time garbage collection.
    %
:- type may_use_atomic_alloc
    --->    may_use_atomic_alloc
    ;       may_not_use_atomic_alloc.

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_data.
%---------------------------------------------------------------------------%
