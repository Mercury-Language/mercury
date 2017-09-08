%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1996, 1998-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make_tags.m.
% Main author: fjh.
%
% This module is where we determine the representation for discriminated union
% types. Each d.u. type is represented as a word. In the case of functors
% with arguments, we allocate the arguments on the heap, and the word contains
% a pointer to those arguments.
%
% For types which are just enumerations (all the constructors are constants),
% we just assign a different value for each constructor.
%
% For types which have only one functor of arity one, there is no need to store
% the functor, and we just store the argument value directly; construction and
% deconstruction unifications on these type are no-ops.
%
% For other types, we use a couple of bits of the word as a tag. We split the
% constructors into constants and functors, and assign tag zero to the
% constants (if any). If there is more than one constant, we distinguish
% between the different constants by the value of the rest of the word. Then
% we assign one tag bit each to the first few functors. The remaining functors
% all get the last remaining two-bit tag. These functors are distinguished by
% a secondary tag which is the first word of the argument vector for those
% functors.
%
% If there are no tag bits available, then we try using reserved addresses
% (e.g. NULL, (void *) 1, (void *) 2, etc.) instead. We split the constructors
% into constants and functors, and assign numerical reserved addresses to the
% first constants, up to the limit set by --num-reserved-addresses. After
% that, for the MLDS back-end, we assign symbolic reserved addresses to the
% remaining constants, up to the limit set by --num-reserved-objects; these
% symbolic reserved addresses are the addresses of global variables that we
% generate specially for this purpose. Finally, the functors and any remaining
% constants are distinguished by a secondary tag, if there are more than one of
% them.
% XXX As of 2017 aug 8, we don't support --num-reserved-objects.
%
% If there is a `pragma reserve_tag' declaration for the type, or if the
% `--reserve-tag' option is set, then we reserve the first primary tag (for
% representing unbound variables). This is used by HAL, for Herbrand
% constraints (i.e. Prolog-style logic variables). This also disables
% enumerations and no_tag types.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_tags.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % assign_constructor_tags(Constructors, MaybeUserEq, TypeCtor,
    %   ReservedTagPragma, Globals, TagValues, IsEnum):
    %
    % Assign a constructor tag to each constructor for a discriminated union
    % type, and determine whether (a) the type representation uses reserved
    % addresses, and (b) the type is an enumeration or dummy type.
    % (`Globals' is passed because exact way in which this is done is
    % dependent on a compilation option.)
    %
:- pred assign_constructor_tags(list(constructor)::in,
    maybe(unify_compare)::in, type_ctor::in, uses_reserved_tag::in,
    globals::in, cons_tag_values::out,
    uses_reserved_address::out, du_type_kind::out) is det.

    % For data types with exactly two alternatives, one of which is a constant,
    % we can test against the constant (negating the result of the test, if
    % needed), since a test against a constant is cheaper than a tag test.
    %
    % The type must not use reserved tags or reserved addresses.
    %
:- pred compute_cheaper_tag_test(cons_tag_values::in,
    maybe_cheaper_tag_test::out) is det.

    % Look for general du type definitions that can be converted into
    % direct arg type definitions.
    %
:- pred post_process_type_defns(module_info::in, module_info::out,
    list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.status.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

assign_constructor_tags(Ctors, UserEqCmp, TypeCtor, ReservedTagPragma, Globals,
        !:CtorTags, ReservedAddr, DuTypeKind) :-
    % Work out how many tag bits and reserved addresses we have available.
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
    globals.lookup_int_option(Globals, num_reserved_addresses,
        NumReservedAddresses),
    globals.lookup_int_option(Globals, num_reserved_objects,
        NumReservedObjects),
    % As of 2017 aug 8, there is no longer any way to specify any value
    % for num_reserved_objects other than the default, which is zero.
    expect(unify(NumReservedObjects, 0), $pred, "NumReservedObjects != 0"),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),

    % Determine if we need to reserve a tag for use by HAL's Herbrand
    % constraint solver. (This also disables enumerations and no_tag types.)
    (
        ReservedTagPragma = uses_reserved_tag,
        InitTag = 1
    ;
        ReservedTagPragma = does_not_use_reserved_tag,
        InitTag = 0
    ),

    % Now assign them.
    map.init(!:CtorTags),
    ( if
        % Try representing the type as an enumeration: all the constructors
        % must be constant, and we must be allowed to make unboxed enums.
        globals.lookup_bool_option(Globals, unboxed_enums, yes),
        ctors_are_all_constants(Ctors),
        ReservedTagPragma = does_not_use_reserved_tag
    then
        ( if Ctors = [_] then
            DuTypeKind = du_type_kind_direct_dummy
        else
            DuTypeKind = du_type_kind_mercury_enum
        ),
        assign_enum_constants(TypeCtor, Ctors, InitTag, !CtorTags),
        ReservedAddr = does_not_use_reserved_address
    else if
        % Try representing it as a no-tag type.
        type_ctor_should_be_notag(Globals, TypeCtor, ReservedTagPragma,
            Ctors, UserEqCmp, SingleFunctorName, SingleArgType,
            MaybeSingleArgName)
    then
        SingleConsId = cons(SingleFunctorName, 1, TypeCtor),
        map.det_insert(SingleConsId, no_tag, !CtorTags),
        % XXX What if SingleArgType uses reserved addresses?
        ReservedAddr = does_not_use_reserved_address,
        DuTypeKind = du_type_kind_notag(SingleFunctorName, SingleArgType,
            MaybeSingleArgName)
    else
        DuTypeKind = du_type_kind_general,
        ( if NumTagBits = 0 then
            (
                ReservedTagPragma = uses_reserved_tag,
                % XXX Need to fix this.
                % This occurs for the .NET and Java backends.
                sorry("make_tags", "--reserve-tag with num_tag_bits = 0")
            ;
                ReservedTagPragma = does_not_use_reserved_tag
            ),
            % Assign reserved addresses to the constants, if possible.
            separate_out_constants(Ctors, Constants, Functors),
            assign_reserved_numeric_addresses(TypeCtor, Constants,
                LeftOverConstants0, !CtorTags, 0, NumReservedAddresses,
                does_not_use_reserved_address, ReservedAddr1),
            (
                HighLevelCode = yes,
                assign_reserved_symbolic_addresses(TypeCtor,
                    LeftOverConstants0, LeftOverConstants,
                    !CtorTags, 0, NumReservedObjects,
                    ReservedAddr1, ReservedAddr)
            ;
                HighLevelCode = no,
                % Reserved symbolic addresses are not supported for the
                % LLDS back-end.
                LeftOverConstants = LeftOverConstants0,
                ReservedAddr = ReservedAddr1
            ),
            % Assign shared_with_reserved_address(...) representations
            % for the remaining constructors.
            RemainingCtors = LeftOverConstants ++ Functors,
            list.filter_map(is_reserved_address_tag,
                map.values(!.CtorTags), ReservedAddresses),
            assign_unshared_tags(TypeCtor, RemainingCtors, 0, 0,
                ReservedAddresses, !CtorTags)
        else
            MaxTag = max_num_tags(NumTagBits) - 1,
            separate_out_constants(Ctors, Constants, Functors),
            assign_constant_tags(TypeCtor, Constants, InitTag, NextTag,
                !CtorTags),
            assign_unshared_tags(TypeCtor, Functors, NextTag, MaxTag,
                [], !CtorTags),
            ReservedAddr = does_not_use_reserved_address
        )
    ).

:- pred is_reserved_address_tag(cons_tag::in, reserved_address::out)
    is semidet.

is_reserved_address_tag(reserved_address_tag(RA), RA).

:- pred assign_enum_constants(type_ctor::in, list(constructor)::in, int::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_enum_constants(_, [], _, !CtorTags).
assign_enum_constants(TypeCtor, [Ctor | Ctors], Val, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    Tag = int_tag(int_tag_int(Val)),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    assign_enum_constants(TypeCtor, Ctors, Val + 1, !CtorTags).

    % Assign the representations null_pointer, small_pointer(1),
    % small_pointer(2), ..., small_pointer(N) to the constructors,
    % until N >= NumReservedAddresses.
    %
:- pred assign_reserved_numeric_addresses(type_ctor::in,
    list(constructor)::in, list(constructor)::out,
    cons_tag_values::in, cons_tag_values::out, int::in, int::in,
    uses_reserved_address::in, uses_reserved_address::out) is det.

assign_reserved_numeric_addresses(_, [], [], !CtorTags, _, _, !ReservedAddr).
assign_reserved_numeric_addresses(TypeCtor, [Ctor | Ctors], LeftOverConstants,
        !CtorTags, Address, NumReservedAddresses, !ReservedAddr) :-
    ( if Address >= NumReservedAddresses then
        LeftOverConstants = [Ctor | Ctors]
    else
        Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
        ConsId = cons(Name, Arity, TypeCtor),
        ( if Address = 0 then
            Tag = reserved_address_tag(null_pointer)
        else
            Tag = reserved_address_tag(small_pointer(Address))
        ),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        !:ReservedAddr = uses_reserved_address,
        assign_reserved_numeric_addresses(TypeCtor, Ctors, LeftOverConstants,
            !CtorTags, Address + 1, NumReservedAddresses, !ReservedAddr)
    ).

    % Assign reserved_object(CtorName, CtorArity) representations
    % to the specified constructors.
    %
:- pred assign_reserved_symbolic_addresses(type_ctor::in,
    list(constructor)::in, list(constructor)::out,
    cons_tag_values::in, cons_tag_values::out, int::in, int::in,
    uses_reserved_address::in, uses_reserved_address::out) is det.

assign_reserved_symbolic_addresses(_, [], [], !CtorTags, _, _, !ReservedAddr).
assign_reserved_symbolic_addresses(TypeCtor, [Ctor | Ctors], LeftOverConstants,
        !CtorTags, Num, Max, !ReservedAddr) :-
    ( if Num >= Max then
        LeftOverConstants = [Ctor | Ctors]
    else
        Ctor = ctor(_ExistQVars, _Constraints, Name, Args, Arity, _Ctxt),
        Tag = reserved_address_tag(reserved_object(TypeCtor, Name, Arity)),
        ConsId = cons(Name, list.length(Args), TypeCtor),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        !:ReservedAddr = uses_reserved_address,
        assign_reserved_symbolic_addresses(TypeCtor, Ctors, LeftOverConstants,
            !CtorTags, Num + 1, Max, !ReservedAddr)
    ).

:- pred assign_constant_tags(type_ctor::in, list(constructor)::in,
    int::in, int::out, cons_tag_values::in, cons_tag_values::out) is det.

assign_constant_tags(TypeCtor, Constants, InitTag, NextTag, !CtorTags) :-
    % If there are no constants, don't do anything. Otherwise, allocate the
    % first tag for the constants, and give them all shared local tags
    % with that tag as the primary tag, and different secondary tags
    % starting from zero.
    %
    % Note that if there is a single constant, we still give it a
    % shared_local_tag rather than a unshared_tag. That is because
    % deconstruction of the shared_local_tag is more efficient.
    (
        Constants = [],
        NextTag = InitTag
    ;
        Constants = [_ | _],
        NextTag = InitTag + 1,
        assign_shared_local_tags(TypeCtor, Constants, InitTag, 0, !CtorTags)
    ).

:- pred assign_unshared_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in, list(reserved_address)::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_unshared_tags(_, [], _, _, _, !CtorTags).
assign_unshared_tags(TypeCtor, [Ctor | Ctors], Val, MaxTag, ReservedAddresses,
        !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    ( if
        % If there is only one functor, give it the "single_functor" (untagged)
        % representation, rather than giving it unshared_tag(0).
        Val = 0,
        Ctors = []
    then
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            single_functor_tag),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags)
    else if
        % If we are about to run out of unshared tags, start assigning
        % shared remote tags instead.
        Val = MaxTag,
        Ctors = [_ | _]
    then
        assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], MaxTag, 0,
            ReservedAddresses, !CtorTags)
    else if
        Val =< MaxTag
    then
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            unshared_tag(Val)),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        assign_unshared_tags(TypeCtor, Ctors, Val + 1, MaxTag,
            ReservedAddresses, !CtorTags)
    else
        unexpected($module, $pred, "exceeded max tag")
    ).

:- pred assign_shared_remote_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in, list(reserved_address)::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_shared_remote_tags(_, [], _, _, _, !CtorTags).
assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], PrimaryVal, SecondaryVal,
        ReservedAddresses, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    Tag = maybe_add_reserved_addresses(ReservedAddresses,
        shared_remote_tag(PrimaryVal, SecondaryVal)),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    SecondaryVal1 = SecondaryVal + 1,
    assign_shared_remote_tags(TypeCtor, Ctors, PrimaryVal, SecondaryVal1,
        ReservedAddresses, !CtorTags).

:- pred assign_shared_local_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in, cons_tag_values::in, cons_tag_values::out) is det.

assign_shared_local_tags(_, [], _, _, !CtorTags).
assign_shared_local_tags(TypeCtor, [Ctor | Ctors], PrimaryVal, SecondaryVal,
        !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    Tag = shared_local_tag(PrimaryVal, SecondaryVal),
    % We call set instead of det_insert because we don't want types
    % that erroneously contain more than one copy of a cons_id to crash
    % the compiler.
    map.set(ConsId, Tag, !CtorTags),
    assign_shared_local_tags(TypeCtor, Ctors, PrimaryVal, SecondaryVal + 1,
        !CtorTags).

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

compute_cheaper_tag_test(CtorTagMap, CheaperTagTest) :-
    ( if
        map.to_assoc_list(CtorTagMap, CtorTagList),
        CtorTagList = [ConsIdA - ConsTagA, ConsIdB - ConsTagB],
        ConsIdA = cons(_, ArityA, _),
        ConsIdB = cons(_, ArityB, _)
    then
        ( if
            ArityB = 0,
            ArityA > 0
        then
            CheaperTagTest = cheaper_tag_test(ConsIdA, ConsTagA,
                ConsIdB, ConsTagB)
        else if
            ArityA = 0,
            ArityB > 0
        then
            CheaperTagTest = cheaper_tag_test(ConsIdB, ConsTagB,
                ConsIdA, ConsTagA)
        else
            CheaperTagTest = no_cheaper_tag_test
        )
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

%-----------------------------------------------------------------------------%

post_process_type_defns(!HLDS, Specs) :-
    module_info_get_globals(!.HLDS, Globals),
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, record_term_sizes_as_words,
            TermSizeWords),
        globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
            TermSizeCells),
        ( if
            TermSizeWords = no,
            TermSizeCells = no
        then
            module_info_get_type_table(!.HLDS, TypeTable0),
            module_info_get_name(!.HLDS, ModuleName),
            get_all_type_ctor_defns(TypeTable0, TypeCtorsDefns),
            globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
            globals.lookup_bool_option(Globals, debug_type_rep, DebugTypeRep),
            MaxTag = max_num_tags(NumTagBits) - 1,
            convert_direct_arg_functors(Target, ModuleName, DebugTypeRep,
                MaxTag, TypeCtorsDefns, TypeTable0, TypeTable, [], Specs),
            module_info_set_type_table(TypeTable, !HLDS)
        else
            % We cannot use direct arg functors in term size grades.
            Specs = []
        )
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        % Direct arg functors have not (yet) been implemented on these targets.
        Specs = []
    ).

:- pred convert_direct_arg_functors(compilation_target::in, module_name::in,
    bool::in, int::in, assoc_list(type_ctor, hlds_type_defn)::in,
    type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors(_, _, _, _, [], !TypeTable, !Specs).
convert_direct_arg_functors(Target, ModuleName, DebugTypeRep, MaxTag,
        [TypeCtorDefn | TypeCtorsDefns], !TypeTable, !Specs) :-
    TypeCtorDefn = TypeCtor - TypeDefn,
    convert_direct_arg_functors_if_suitable(Target, ModuleName, DebugTypeRep,
        MaxTag, TypeCtor, TypeDefn, !TypeTable, !Specs),
    convert_direct_arg_functors(Target, ModuleName, DebugTypeRep, MaxTag,
        TypeCtorsDefns, !TypeTable, !Specs).

:- pred convert_direct_arg_functors_if_suitable(compilation_target::in,
    module_name::in, bool::in, int::in, type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors_if_suitable(Target, ModuleName, DebugTypeRep,
        MaxTag, TypeCtor, TypeDefn, !TypeTable, !Specs) :-
    get_type_defn_body(TypeDefn, Body),
    (
        Body = hlds_du_type(Ctors, _ConsTagValues, _MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, MaybeAssertedDirectArgCtors,
            ReservedTag, ReservedAddr, MaybeForeign),
        ( if
            Ctors = [_, _ | _],
            DuKind = du_type_kind_general,
            ReservedTag = does_not_use_reserved_tag,
            ReservedAddr = does_not_use_reserved_address,
            MaybeForeign = no,
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            sym_name_get_module_name(TypeCtorSymName, TypeCtorModule)
        then
            get_type_defn_status(TypeDefn, TypeStatus),
            (
                MaybeAssertedDirectArgCtors = yes(AssertedDirectArgFunctors)
            ;
                MaybeAssertedDirectArgCtors = no,
                AssertedDirectArgFunctors = []
            ),
            separate_out_constants(Ctors, Constants, Functors),
            list.filter(
                is_direct_arg_ctor(!.TypeTable, Target, TypeCtorModule,
                    TypeStatus, AssertedDirectArgFunctors),
                Functors, DirectArgFunctors, NonDirectArgFunctors),
            (
                DirectArgFunctors = []
                % We cannot use the direct argument representation for any
                % functors.
            ;
                DirectArgFunctors = [_ | _],
                some [!NextTag, !CtorTags] (
                    !:NextTag = 0,
                    map.init(!:CtorTags),
                    assign_constant_tags(TypeCtor, Constants,
                        !NextTag, !CtorTags),
                    % We prefer to allocate primary tags to direct argument
                    % functors.
                    (
                        NonDirectArgFunctors = [],
                        MaxTagForDirect = MaxTag
                    ;
                        NonDirectArgFunctors = [_ | _],
                        MaxTagForDirect = MaxTag - 1
                    ),
                    assign_direct_arg_tags(TypeCtor, DirectArgFunctors,
                        !NextTag, MaxTagForDirect, LeftOverDirectArgFunctors,
                        !CtorTags),
                    assign_unshared_tags(TypeCtor,
                        LeftOverDirectArgFunctors ++ NonDirectArgFunctors,
                        !.NextTag, MaxTag, [], !CtorTags),
                    DirectArgConsTagValues = !.CtorTags
                ),
                compute_cheaper_tag_test(DirectArgConsTagValues,
                    MaybeCheaperTagTest),
                DirectArgFunctorNames =
                    list.map(constructor_to_sym_name_and_arity,
                    DirectArgFunctors),
                (
                    DebugTypeRep = yes,
                    trace [io(!IO)] (
                        output_direct_arg_functor_summary(ModuleName, TypeCtor,
                            DirectArgFunctorNames, !IO)
                    )
                ;
                    DebugTypeRep = no
                ),
                DirectArgBody = hlds_du_type(Ctors, DirectArgConsTagValues,
                    MaybeCheaperTagTest, DuKind, MaybeUserEqComp,
                    yes(DirectArgFunctorNames), ReservedTag, ReservedAddr,
                    MaybeForeign),
                set_type_defn_body(DirectArgBody, TypeDefn, DirectArgTypeDefn),
                replace_type_ctor_defn(TypeCtor, DirectArgTypeDefn, !TypeTable)
            ),
            check_incorrect_direct_arg_assertions(AssertedDirectArgFunctors,
                NonDirectArgFunctors, !Specs)
        else
            % We cannot use the direct argument representation for any
            % functors.
            true
        )
    ;
        ( Body = hlds_eqv_type(_)
        ; Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_)
        ; Body = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred is_direct_arg_ctor(type_table::in, compilation_target::in,
    module_name::in, type_status::in, list(sym_name_and_arity)::in,
    constructor::in) is semidet.

is_direct_arg_ctor(TypeTable, Target, TypeCtorModule, TypeStatus,
        AssertedDirectArgCtors, Ctor) :-
    Ctor = ctor(ExistQTVars, ExistConstraints, ConsName, ConsArgs, Arity,
        _CtorContext),
    ExistQTVars = [],
    ExistConstraints = [],
    ConsArgs = [ConsArg],
    expect(unify(Arity, 1), $module, $pred, "Arity != 1"),
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgWidth, _ArgContext),
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeCtorArgTypes),

    ( if
        % Trust the `direct_arg' attribute of an imported type.
        type_status_is_imported(TypeStatus) = yes,
        list.contains(AssertedDirectArgCtors, sym_name_arity(ConsName, Arity))
    then
        ArgCond = direct_arg_asserted
    else if
        % Tuples are always acceptable argument types as they are represented
        % by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet) so are not acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    then
        ArgCond = arg_type_is_word_aligned_pointer
    else
        search_type_ctor_defn(TypeTable, ArgTypeCtor, ArgTypeDefn),
        get_type_defn_body(ArgTypeDefn, ArgBody),
        ( if is_foreign_type_for_target(ArgBody, Target, Assertions) then
            % Foreign types are acceptable arguments if asserted that their
            % values are word-aligned pointers.
            asserted_word_aligned_pointer(Assertions),
            ArgCond = arg_type_is_word_aligned_pointer
        else
            % The argument type is not a foreign type.

            ArgTypeCtorArgTypes = [],
            % XXX We could let this be a subset of the type params, but that
            % would require the runtime system to be able to handle variables
            % in the argument type, during unification and comparison
            % (mercury_unify_compare_body.h) during deconstruction
            % (mercury_ml_expand_body.h), during deep copying
            % (mercury_deep_copy_body.h), and maybe during some other
            % operations.

            ArgBody = hlds_du_type(ArgCtors, ArgConsTagValues,
                ArgMaybeCheaperTagTest, ArgDuKind, _ArgMaybeUserEqComp,
                ArgDirectArgCtors, ArgReservedTag, ArgReservedAddr,
                ArgMaybeForeign),
            ArgCtors = [_],
            ArgMaybeCheaperTagTest = no_cheaper_tag_test,
            ArgDuKind = du_type_kind_general,
            ArgDirectArgCtors = no,
            ArgReservedTag = does_not_use_reserved_tag,
            ArgReservedAddr = does_not_use_reserved_address,
            ArgMaybeForeign = no,

            map.to_assoc_list(ArgConsTagValues, ArgConsTagValueList),
            ArgConsTagValueList = [ArgConsTagValue],
            ArgConsTagValue = _ConsId - single_functor_tag,

            ( if
                type_status_defined_in_this_module(TypeStatus) = yes,
                list.contains(AssertedDirectArgCtors,
                    sym_name_arity(ConsName, Arity))
            then
                ArgCond = direct_arg_asserted
            else
                ArgTypeCtor = type_ctor(ArgTypeCtorSymName, _ArgTypeCtorArity),
                sym_name_get_module_name(ArgTypeCtorSymName,
                    ArgTypeCtorModule),
                ( if TypeCtorModule = ArgTypeCtorModule then
                    get_type_defn_status(ArgTypeDefn, ArgTypeStatus),
                    ArgCond = arg_type_defined_in_same_module(ArgTypeStatus)
                else
                    ArgCond = arg_type_defined_in_different_module
                )
            )
        )
    ),
    module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = yes.

:- type direct_arg_cond
    --->    direct_arg_asserted
            % The constructor being checked has a single argument, and a
            % `where direct_arg' attribute asserts that the direct arg
            % representation may be used for the constructor.

    ;       arg_type_is_word_aligned_pointer
            % The constructor being checked has a single argument, and either
            % the argument has a builtin type that is represented with a
            % word-aligned pointer, or the argument has a foreign type with the
            % `word_aligned_pointer' assertion.

    ;       arg_type_defined_in_same_module(type_status)
            % The constructor being checked has a single argument, and the
            % argument type is defined in the same module as the constructor.
            % The argument type has the given import status.

    ;       arg_type_defined_in_different_module.
            % The constructor being checked has a single argument, and the
            % argument type is defined in a different module from the
            % constructor.

    % When this predicate is called, we should have checked that
    % the constructor has a single argument, and the argument has a type
    % such that the direct arg functor representation may apply
    % to the constructor. We still need to check that other modules
    % would infer the same type representation for the same constructor,
    % given that they may not have the same knowledge of the constructor's type
    % or the argument type.
    %
    % TypeStatus is import status of the type of the constructor being checked.
    %
:- func module_visibilities_allow_direct_arg(type_status, direct_arg_cond)
    = bool.

module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = AllowDirectArg :-
    % XXX STATUS
    TypeStatus = type_status(OldImportStatus),
    (
        % If the outer type _definition_ is not exported from this module,
        % then the direct arg representation may be used. In the absence of
        % intermodule optimisation, only this module can [de]construct values
        % of this type.
        ( OldImportStatus = status_local
        ; OldImportStatus = status_abstract_exported
        ),
        AllowDirectArg = yes
    ;
        % If the outer type is opt-exported, another module may opt-import this
        % type, but abstract-import the argument type. It could not then infer
        % if the direct arg representation is required for any functors of the
        % outer type. The problem is overcome by adding `where direct_arg'
        % attributes to the opt-exported type definition in .opt files,
        % which state the functors that require the direct arg representation.
        OldImportStatus = status_opt_exported,
        AllowDirectArg = yes
    ;
        % If the outer type is exported from this module, then the direct arg
        % representation may be used, so long as any importing modules will
        % infer the same thing.
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ArgTypeStatus = type_status(ArgOldTypeStatus),
            ( if
                (
                    OldImportStatus = status_exported,
                    ArgOldTypeStatus = status_exported
                ;
                    % If the wrapper type is exported to submodules only, then
                    % the only modules whose access to the argument type
                    % matters is those submodules. Each of these
                    % ArgOldTypeStatus values allows these submodules access
                    % to the argument type's definition. The fact that some
                    % of them also allow other modules access doesn't matter,
                    % because (due to their lack of visibility to the wrapper
                    % type) the question of the wrapper's type representation
                    % won't come up in them.
                    OldImportStatus = status_exported_to_submodules,
                    ( ArgOldTypeStatus = status_exported
                    ; ArgOldTypeStatus = status_exported_to_submodules
                    ; ArgOldTypeStatus = status_abstract_exported
                    )
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % The direct arg representation is required if the type of the
        % constructor being checked is imported, and:
        % - if a `where direct_arg' attribute says so
        % - if the argument value is a word-aligned pointer
        % - if the argument type is imported from the same module
        OldImportStatus = status_imported(TypeImportLocn),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ( if
                ArgTypeStatus = type_status(status_imported(ArgImportLocn)),
                % If the argument type is only exported by an ancestor to its
                % submodules (of which we are one), the outer type must also
                % only be exported to submodules. Otherwise submodules and
                % non-submodules would infer different things.
                (
                    ArgImportLocn =
                        import_locn_ancestor_private_interface_proper
                =>
                    TypeImportLocn =
                        import_locn_ancestor_private_interface_proper
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % If the outer type is opt-imported, there will always be a
        % `where direct_arg' attribute on the type definition which states
        % if the direct argument representation must be used.
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ),
        (
            ArgCond = direct_arg_asserted,
            AllowDirectArg = yes
        ;
            ( ArgCond = arg_type_is_word_aligned_pointer
            ; ArgCond = arg_type_defined_in_same_module(_)
            ; ArgCond = arg_type_defined_in_different_module
            ),
            AllowDirectArg = no
        )
    ;
        ( OldImportStatus = status_external(_)
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_pseudo_imported
        ),
        unexpected($module, $pred, "inappropriate status for type")
    ).

:- pred is_foreign_type_for_target(hlds_type_body::in, compilation_target::in,
    foreign_type_assertions::out) is semidet.

is_foreign_type_for_target(TypeBody, Target, Assertions) :-
    (
        TypeBody = hlds_du_type(_, _, _, _, _, _, _, _, MaybeForeignType),
        MaybeForeignType = yes(ForeignType)
    ;
        TypeBody = hlds_foreign_type(ForeignType)
    ),
    (
        Target = target_c,
        ForeignType ^ c = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_java,
        ForeignType ^ java = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_csharp,
        ForeignType ^ csharp = yes(foreign_type_lang_data(_, _, Assertions))
    ;
        Target = target_erlang,
        ForeignType ^ erlang = yes(foreign_type_lang_data(_, _, Assertions))
    ).

:- pred assign_direct_arg_tags(type_ctor::in, list(constructor)::in,
    int::in, int::out, int::in, list(constructor)::out,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_direct_arg_tags(_, [], !Val, _, [], !CtorTags).
assign_direct_arg_tags(TypeCtor, [Ctor | Ctors], !Val, MaxTag, LeftOverCtors,
        !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),
    ( if
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.Val = MaxTag,
        Ctors = [_ | _]
    then
        LeftOverCtors = [Ctor | Ctors]
    else
        Tag = direct_arg_tag(!.Val),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        !:Val = !.Val + 1,
        assign_direct_arg_tags(TypeCtor, Ctors, !Val, MaxTag, LeftOverCtors,
            !CtorTags)
    ).

:- pred check_incorrect_direct_arg_assertions(list(sym_name_and_arity)::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_incorrect_direct_arg_assertions(_AssertedDirectArgCtors, [], !Specs).
check_incorrect_direct_arg_assertions(AssertedDirectArgCtors, [Ctor | Ctors],
        !Specs) :-
    ( if
        Ctor = ctor(_, _, SymName, _Args, Arity, Context),
        list.contains(AssertedDirectArgCtors, sym_name_arity(SymName, Arity))
    then
        Pieces = [words("Error:"),
            unqual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("cannot be represented as a direct pointer to its"),
            words("sole argument."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    check_incorrect_direct_arg_assertions(AssertedDirectArgCtors, Ctors,
        !Specs).

:- func constructor_to_sym_name_and_arity(constructor) = sym_name_and_arity.

constructor_to_sym_name_and_arity(ctor(_, _, Name, _Args, Arity, _)) =
    sym_name_arity(Name, Arity).

:- pred output_direct_arg_functor_summary(module_name::in, type_ctor::in,
    list(sym_name_and_arity)::in, io::di, io::uo) is det.

output_direct_arg_functor_summary(ModuleName, TypeCtor, DirectArgFunctorNames,
        !IO) :-
    write_sym_name(ModuleName, !IO),
    io.write_string(" : ", !IO),
    write_type_ctor(TypeCtor, !IO),
    io.write_string(" : ", !IO),
    io.write_list(DirectArgFunctorNames, ", ", write_sym_name_and_arity, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
%
% Auxiliary functions and predicates.
%

:- func max_num_tags(int) = int.

max_num_tags(NumTagBits) = MaxTags :-
    int.pow(2, NumTagBits, MaxTags).

:- pred ctors_are_all_constants(list(constructor)::in) is semidet.

ctors_are_all_constants([]).
ctors_are_all_constants([Ctor | Rest]) :-
    Ctor = ctor(_ExistQVars, _Constraints, _Name, Args, _, _Ctxt),
    Args = [],
    ctors_are_all_constants(Rest).

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
:- end_module hlds.make_tags.
%-----------------------------------------------------------------------------%
