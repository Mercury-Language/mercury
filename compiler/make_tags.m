%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1996, 1998-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: make_tags.m.
% Main author: fjh.
% 
% This module is where we determine the representation for
% discriminated union types.  Each d.u. type is represented as
% a word.  In the case of functors with arguments, we allocate
% the arguments on the heap, and the word contains a pointer to
% those arguments.
%
% For types which are just enumerations (all the constructors
% are constants), we just assign a different value for each
% constructor.
%
% For types which have only one functor of arity one, there is
% no need to store the functor, and we just store the argument
% value directly; construction and deconstruction unifications
% on these type are no-ops.
%
% For other types, we use a couple of bits of the word as a
% tag.  We split the constructors into constants and functors,
% and assign tag zero to the constants (if any).  If there is
% more than one constant, we distinguish between the different
% constants by the value of the rest of the word.  Then we
% assign one tag bit each to the first few functors.  The
% remaining functors all get the last remaining two-bit tag.
% These functors are distinguished by a secondary tag which is
% the first word of the argument vector for those functors.
%
% If there are no tag bits available, then we try using reserved
% addresses (e.g. NULL, (void *)1, (void *)2, etc.) instead.
% We split the constructors into constants and functors,
% and assign numerical reserved addresses to the first constants,
% up to the limit set by --num-reserved-addresses.
% After that, for the MLDS back-end, we assign symbolic reserved
% addresses to the remaining constants, up to the limit set by
% --num-reserved-objects; these symbolic reserved addresses
% are the addresses of global variables that we generate specially
% for this purpose.  Finally, the functors and any remaining
% constants are distinguished by a secondary tag, if there are more
% than one of them.
%
% If there is a `pragma reserve_tag' declaration for the type,
% or if the `--reserve-tag' option is set,
% then we reserve the first primary tag (for representing
% unbound variables).  This is used by HAL, for Herbrand constraints
% (i.e. Prolog-style logic variables).
% This also disables enumerations and no_tag types.
% 
%-----------------------------------------------------------------------------%

:- module hlds.make_tags.
:- interface.

:- import_module hlds.hlds_data.
:- import_module libs.globals.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

    % assign_constructor_tags(Constructors, MaybeUserEq, TypeCtor,
    %   ReservedTagPragma, Globals, TagValues, IsEnum):
    %
    % Assign a constructor tag to each constructor for a discriminated union
    % type, and determine whether the type is an enumeration type or not.
    % (`Globals' is passed because exact way in which this is done is
    % dependent on a compilation option.)
    %
:- pred assign_constructor_tags(list(constructor)::in,
    maybe(unify_compare)::in, type_ctor::in, bool::in,
    globals::in, cons_tag_values::out, enum_or_dummy::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module svmap.

%-----------------------------------------------------------------------------%

assign_constructor_tags(Ctors, UserEqCmp, TypeCtor, ReservedTagPragma, Globals,
        CtorTags, EnumDummy) :-

    % Work out how many tag bits and reserved addresses we've got to play with.
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
    globals.lookup_int_option(Globals, num_reserved_addresses,
        NumReservedAddresses),
    globals.lookup_int_option(Globals, num_reserved_objects,
        NumReservedObjects),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

    % Determine if we need to reserve a tag for use by HAL's Herbrand
    % constraint solver. (This also disables enumerations and no_tag types.)
    globals.lookup_bool_option(Globals, reserve_tag, GlobalReserveTag),
    ReserveTag = GlobalReserveTag `or` ReservedTagPragma,

    (
        ReserveTag = yes,
        InitTag = 1
    ;
        ReserveTag = no,
        InitTag = 0
    ),

    % Now assign them.
    map.init(CtorTags0),
    (
        % Try representing the type as an enumeration: all the constructors
        % must be constant, and we must be allowed to make unboxed enums.
        globals.lookup_bool_option(Globals, unboxed_enums, yes),
        ctors_are_all_constants(Ctors),
        ReserveTag = no
    ->
        ( Ctors = [_] ->
            EnumDummy = is_dummy
        ;
            EnumDummy = is_enum
        ),
        assign_enum_constants(Ctors, InitTag, CtorTags0, CtorTags)
    ;
        EnumDummy = not_enum_or_dummy,
        (
            % Try representing it as a no-tag type.
            type_with_constructors_should_be_no_tag(Globals, TypeCtor,
                ReserveTag, Ctors, UserEqCmp, SingleFunc, SingleArg, _)
        ->
            SingleConsId = make_cons_id_from_qualified_sym_name(SingleFunc,
                [SingleArg]),
            map.set(CtorTags0, SingleConsId, no_tag, CtorTags)
        ;
            NumTagBits = 0
        ->
            (
                ReserveTag = yes,
                % XXX Need to fix this.
                % This occurs for the .NET and Java backends.
                sorry("make_tags", "--reserve-tag with num_tag_bits = 0")
            ;
                ReserveTag = no
            ),
            % Assign reserved addresses to the constants, if possible.
            separate_out_constants(Ctors, Constants, Functors),
            assign_reserved_numeric_addresses(Constants, LeftOverConstants0,
                CtorTags0, CtorTags1, 0, NumReservedAddresses),
            (
                HighLevelCode = yes,
                assign_reserved_symbolic_addresses(
                    LeftOverConstants0, LeftOverConstants, TypeCtor,
                    CtorTags1, CtorTags2, 0, NumReservedObjects)
            ;
                HighLevelCode = no,
                % Reserved symbolic addresses are not supported for the
                % LLDS back-end.
                LeftOverConstants = LeftOverConstants0,
                CtorTags2 = CtorTags1
            ),
            % Assign shared_with_reserved_address(...) representations
            % for the remaining constructors.
            RemainingCtors = LeftOverConstants ++ Functors,
            ReservedAddresses = list.filter_map(
                (func(reserved_address_tag(RA)) = RA is semidet),
                map.values(CtorTags2)),
            assign_unshared_tags(RemainingCtors, 0, 0, ReservedAddresses,
                CtorTags2, CtorTags)
        ;
            MaxTag = max_num_tags(NumTagBits) - 1,
            separate_out_constants(Ctors, Constants, Functors),
            assign_constant_tags(Constants, CtorTags0, CtorTags1,
                InitTag, NextTag),
            assign_unshared_tags(Functors, NextTag, MaxTag, [],
                CtorTags1, CtorTags)
        )
    ).

:- pred assign_enum_constants(list(constructor)::in, int::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_enum_constants([], _, !CtorTags).
assign_enum_constants([Ctor | Rest], Val, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    Tag = int_tag(Val),
    svmap.set(ConsId, Tag, !CtorTags),
    assign_enum_constants(Rest, Val + 1, !CtorTags).

    % Assign the representations null_pointer, small_pointer(1),
    % small_pointer(2), ..., small_pointer(N) to the constructors,
    % until N >= NumReservedAddresses.
    %
:- pred assign_reserved_numeric_addresses(
    list(constructor)::in, list(constructor)::out,
    cons_tag_values::in, cons_tag_values::out, int::in, int::in) is det.

assign_reserved_numeric_addresses([], [], !CtorTags, _, _).
assign_reserved_numeric_addresses([Ctor | Rest], LeftOverConstants,
        !CtorTags, Address, NumReservedAddresses) :-
    ( Address >= NumReservedAddresses ->
        LeftOverConstants = [Ctor | Rest]
    ;
        Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
        ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
        ( Address = 0 ->
            Tag = reserved_address_tag(null_pointer)
        ;
            Tag = reserved_address_tag(small_pointer(Address))
        ),
        svmap.set(ConsId, Tag, !CtorTags),
        assign_reserved_numeric_addresses(Rest, LeftOverConstants,
            !CtorTags, Address + 1, NumReservedAddresses)
    ).

    % Assign reserved_object(CtorName, CtorArity) representations
    % to the specified constructors.
    %
:- pred assign_reserved_symbolic_addresses(
    list(constructor)::in, list(constructor)::out, type_ctor::in,
    cons_tag_values::in, cons_tag_values::out, int::in, int::in) is det.

assign_reserved_symbolic_addresses([], [], _, !CtorTags, _, _).
assign_reserved_symbolic_addresses([Ctor | Ctors], LeftOverConstants, TypeCtor,
        !CtorTags, Num, Max) :-
    ( Num >= Max ->
        LeftOverConstants = [Ctor | Ctors]
    ;
        Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
        Arity = list.length(Args),
        Tag = reserved_address_tag(reserved_object(TypeCtor, Name, Arity)),
        ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
        svmap.set(ConsId, Tag, !CtorTags),
        assign_reserved_symbolic_addresses(Ctors, LeftOverConstants,
            TypeCtor, !CtorTags, Num + 1, Max)
    ).

:- pred assign_constant_tags(list(constructor)::in, cons_tag_values::in,
    cons_tag_values::out, int::in, int::out) is det.

    % If there's no constants, don't do anything. Otherwise, allocate the
    % first tag for the constants, and give them all shared local tags
    % with that tag as the primary tag, and different secondary tags
    % starting from zero.
    %
    % Note that if there's a single constant, we still give it a
    % shared_local_tag rather than a unshared_tag. That's because
    % deconstruction of the shared_local_tag is more efficient.
    %
assign_constant_tags(Constants, !CtorTags, InitTag, NextTag) :-
    (
        Constants = [],
        NextTag = InitTag
    ;
        Constants = [_ | _],
        NextTag = InitTag + 1,
        assign_shared_local_tags(Constants, InitTag, 0, !CtorTags)
    ).

:- pred assign_unshared_tags(list(constructor)::in, int::in, int::in,
    list(reserved_address)::in, cons_tag_values::in, cons_tag_values::out)
    is det.

assign_unshared_tags([], _, _, _, !CtorTags).
assign_unshared_tags([Ctor | Rest], Val, MaxTag, ReservedAddresses,
        !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    % If there's only one functor,
    % give it the "single_functor" (untagged)
    % representation, rather than giving it unshared_tag(0).
    (
        Val = 0,
        Rest = []
    ->
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            single_functor_tag),
        svmap.set(ConsId, Tag, !CtorTags)
    ;
        % If we're about to run out of unshared tags, start assigning
        % shared remote tags instead.
        Val = MaxTag,
        Rest = [_ | _]
    ->
        assign_shared_remote_tags([Ctor | Rest], MaxTag, 0, ReservedAddresses,
            !CtorTags)
    ;
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            unshared_tag(Val)),
        svmap.set(ConsId, Tag, !CtorTags),
        assign_unshared_tags(Rest, Val + 1, MaxTag, ReservedAddresses,
            !CtorTags)
    ).

:- pred assign_shared_remote_tags(list(constructor)::in, int::in, int::in,
    list(reserved_address)::in, cons_tag_values::in, cons_tag_values::out)
    is det.

assign_shared_remote_tags([], _, _, _, !CtorTags).
assign_shared_remote_tags([Ctor | Rest], PrimaryVal, SecondaryVal,
        ReservedAddresses, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    Tag = maybe_add_reserved_addresses(ReservedAddresses,
        shared_remote_tag(PrimaryVal, SecondaryVal)),
    svmap.set(ConsId, Tag, !CtorTags),
    SecondaryVal1 = SecondaryVal + 1,
    assign_shared_remote_tags(Rest, PrimaryVal, SecondaryVal1,
        ReservedAddresses, !CtorTags).

:- pred assign_shared_local_tags(list(constructor)::in, int::in, int::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_shared_local_tags([], _, _, !CtorTags).
assign_shared_local_tags([Ctor | Rest], PrimaryVal, SecondaryVal, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = make_cons_id_from_qualified_sym_name(Name, Args),
    Tag = shared_local_tag(PrimaryVal, SecondaryVal),
    svmap.set(ConsId, Tag, !CtorTags),
    SecondaryVal1 = SecondaryVal + 1,
    assign_shared_local_tags(Rest, PrimaryVal, SecondaryVal1, !CtorTags).

:- func maybe_add_reserved_addresses(list(reserved_address), cons_tag) =
    cons_tag.

maybe_add_reserved_addresses(ReservedAddresses, Tag0) = Tag :-
    (
        ReservedAddresses = [],
        Tag = Tag0
    ;
        ReservedAddresses = [_ | _],
        Tag = shared_with_reserved_addresses_tag(ReservedAddresses, Tag0)
    ).

%-----------------------------------------------------------------------------%

:- func max_num_tags(int) = int.

max_num_tags(NumTagBits) = MaxTags :-
    int.pow(2, NumTagBits, MaxTags).

%-----------------------------------------------------------------------------%

:- pred ctors_are_all_constants(list(constructor)::in) is semidet.

ctors_are_all_constants([]).
ctors_are_all_constants([Ctor | Rest]) :-
    Ctor = ctor(_ExistQVars, _Constraints, _Name, Args, _Ctxt),
    Args = [],
    ctors_are_all_constants(Rest).

%-----------------------------------------------------------------------------%

:- pred separate_out_constants(list(constructor)::in,
    list(constructor)::out, list(constructor)::out) is det.

separate_out_constants([], [], []).
separate_out_constants([Ctor | Ctors], Constants, Functors) :-
    separate_out_constants(Ctors, Constants0, Functors0),
    Args = Ctor ^ cons_args,
    (
        Args = [],
        Constants = [Ctor | Constants0],
        Functors = Functors0
    ;
        Args = [_ | _],
        Constants = Constants0,
        Functors = [Ctor | Functors0]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
