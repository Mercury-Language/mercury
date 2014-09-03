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
% types.  Each d.u. type is represented as a word.  In the case of functors
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
% For other types, we use a couple of bits of the word as a tag.  We split the
% constructors into constants and functors, and assign tag zero to the
% constants (if any).  If there is more than one constant, we distinguish
% between the different constants by the value of the rest of the word.  Then
% we assign one tag bit each to the first few functors.  The remaining functors
% all get the last remaining two-bit tag.  These functors are distinguished by
% a secondary tag which is the first word of the argument vector for those
% functors.
%
% If there are no tag bits available, then we try using reserved addresses
% (e.g. NULL, (void *)1, (void *)2, etc.) instead.  We split the constructors
% into constants and functors, and assign numerical reserved addresses to the
% first constants, up to the limit set by --num-reserved-addresses.  After
% that, for the MLDS back-end, we assign symbolic reserved addresses to the
% remaining constants, up to the limit set by --num-reserved-objects; these
% symbolic reserved addresses are the addresses of global variables that we
% generate specially for this purpose.  Finally, the functors and any remaining
% constants are distinguished by a secondary tag, if there are more than one of
% them.
%
% If there is a `pragma reserve_tag' declaration for the type, or if the
% `--reserve-tag' option is set, then we reserve the first primary tag (for
% representing unbound variables).  This is used by HAL, for Herbrand
% constraints (i.e. Prolog-style logic variables).  This also disables
% enumerations and no_tag types.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_tags.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
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

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
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
    % Work out how many tag bits and reserved addresses we've got to play with.
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
    globals.lookup_int_option(Globals, num_reserved_addresses,
        NumReservedAddresses),
    globals.lookup_int_option(Globals, num_reserved_objects,
        NumReservedObjects),
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
    (
        % Try representing the type as an enumeration: all the constructors
        % must be constant, and we must be allowed to make unboxed enums.
        globals.lookup_bool_option(Globals, unboxed_enums, yes),
        ctors_are_all_constants(Ctors),
        ReservedTagPragma = does_not_use_reserved_tag
    ->
        ( Ctors = [_] ->
            DuTypeKind = du_type_kind_direct_dummy
        ;
            DuTypeKind = du_type_kind_mercury_enum
        ),
        assign_enum_constants(TypeCtor, Ctors, InitTag, !CtorTags),
        ReservedAddr = does_not_use_reserved_address
    ;
        (
            % Try representing it as a no-tag type.
            type_ctor_should_be_notag(Globals, TypeCtor, ReservedTagPragma,
                Ctors, UserEqCmp, SingleFunctorName, SingleArgType,
                MaybeSingleArgName)
        ->
            SingleConsId = cons(SingleFunctorName, 1, TypeCtor),
            map.det_insert(SingleConsId, no_tag, !CtorTags),
            % XXX What if SingleArgType uses reserved addresses?
            ReservedAddr = does_not_use_reserved_address,
            DuTypeKind = du_type_kind_notag(SingleFunctorName, SingleArgType,
                MaybeSingleArgName)
        ;
            DuTypeKind = du_type_kind_general,
            ( NumTagBits = 0 ->
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
                GetRA = (func(reserved_address_tag(RA)) = RA is semidet),
                ReservedAddresses = list.filter_map(GetRA,
                    map.values(!.CtorTags)),
                assign_unshared_tags(TypeCtor, RemainingCtors, 0, 0,
                    ReservedAddresses, !CtorTags)
            ;
                MaxTag = max_num_tags(NumTagBits) - 1,
                separate_out_constants(Ctors, Constants, Functors),
                assign_constant_tags(TypeCtor, Constants, InitTag, NextTag,
                    !CtorTags),
                assign_unshared_tags(TypeCtor, Functors, NextTag, MaxTag,
                    [], !CtorTags),
                ReservedAddr = does_not_use_reserved_address
            )
        )
    ).

:- pred assign_enum_constants(type_ctor::in, list(constructor)::in, int::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_enum_constants(_, [], _, !CtorTags).
assign_enum_constants(TypeCtor, [Ctor | Ctors], Val, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = cons(Name, list.length(Args), TypeCtor),
    Tag = int_tag(Val),
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
    ( Address >= NumReservedAddresses ->
        LeftOverConstants = [Ctor | Ctors]
    ;
        Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
        ConsId = cons(Name, list.length(Args), TypeCtor),
        ( Address = 0 ->
            Tag = reserved_address_tag(null_pointer)
        ;
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
    ( Num >= Max ->
        LeftOverConstants = [Ctor | Ctors]
    ;
        Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
        Arity = list.length(Args),
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
    % If there's no constants, don't do anything. Otherwise, allocate the
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
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = cons(Name, list.length(Args), TypeCtor),
    % If there's only one functor, give it the "single_functor" (untagged)
    % representation, rather than giving it unshared_tag(0).
    (
        Val = 0,
        Ctors = []
    ->
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            single_functor_tag),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags)
    ;
        % If we're about to run out of unshared tags, start assigning
        % shared remote tags instead.
        Val = MaxTag,
        Ctors = [_ | _]
    ->
        assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], MaxTag, 0,
            ReservedAddresses, !CtorTags)
    ;
        Val =< MaxTag
    ->
        Tag = maybe_add_reserved_addresses(ReservedAddresses,
            unshared_tag(Val)),
        % We call set instead of det_insert because we don't want types
        % that erroneously contain more than one copy of a cons_id to crash
        % the compiler.
        map.set(ConsId, Tag, !CtorTags),
        assign_unshared_tags(TypeCtor, Ctors, Val + 1, MaxTag,
            ReservedAddresses, !CtorTags)
    ;
        unexpected($module, $pred, "exceeded max tag")
    ).

:- pred assign_shared_remote_tags(type_ctor::in, list(constructor)::in,
    int::in, int::in, list(reserved_address)::in,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_shared_remote_tags(_, [], _, _, _, !CtorTags).
assign_shared_remote_tags(TypeCtor, [Ctor | Ctors], PrimaryVal, SecondaryVal,
        ReservedAddresses, !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = cons(Name, list.length(Args), TypeCtor),
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
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = cons(Name, list.length(Args), TypeCtor),
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
    (
        map.to_assoc_list(CtorTagMap, CtorTagList),
        CtorTagList = [ConsIdA - ConsTagA, ConsIdB - ConsTagB],
        ConsIdA = cons(_, ArityA, _),
        ConsIdB = cons(_, ArityB, _)
    ->
        (
            ArityB = 0,
            ArityA > 0
        ->
            CheaperTagTest = cheaper_tag_test(ConsIdA, ConsTagA,
                ConsIdB, ConsTagB)
        ;
            ArityA = 0,
            ArityB > 0
        ->
            CheaperTagTest = cheaper_tag_test(ConsIdB, ConsTagB,
                ConsIdA, ConsTagA)
        ;
            CheaperTagTest = no_cheaper_tag_test
        )
    ;
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
        (
            TermSizeWords = no,
            TermSizeCells = no
        ->
            module_info_get_type_table(!.HLDS, TypeTable0),
            module_info_get_name(!.HLDS, ModuleName),
            get_all_type_ctor_defns(TypeTable0, TypeCtorsDefns),
            globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
            globals.lookup_bool_option(Globals, debug_type_rep, DebugTypeRep),
            MaxTag = max_num_tags(NumTagBits) - 1,
            convert_direct_arg_functors(ModuleName, DebugTypeRep, MaxTag,
                TypeCtorsDefns, TypeTable0, TypeTable, [], Specs),
            module_info_set_type_table(TypeTable, !HLDS)
        ;
            % We cannot use direct arg functors in term size grades.
            Specs = []
        )
    ;
        ( Target = target_il
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        % Direct arg functors have not (yet) been implemented on these targets.
        Specs = []
    ).

:- pred convert_direct_arg_functors(module_name::in, bool::in, int::in,
    assoc_list(type_ctor, hlds_type_defn)::in, type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors(_, _, _, [], !TypeTable, !Specs).
convert_direct_arg_functors(ModuleName, DebugTypeRep, MaxTag,
        [TypeCtorDefn | TypeCtorsDefns], !TypeTable, !Specs) :-
    TypeCtorDefn = TypeCtor - TypeDefn,
    convert_direct_arg_functors_if_suitable(ModuleName, DebugTypeRep, MaxTag,
        TypeCtor, TypeDefn, !TypeTable, !Specs),
    convert_direct_arg_functors(ModuleName, DebugTypeRep, MaxTag,
        TypeCtorsDefns, !TypeTable, !Specs).

:- pred convert_direct_arg_functors_if_suitable(module_name::in, bool::in,
    int::in, type_ctor::in, hlds_type_defn::in,
    type_table::in, type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

convert_direct_arg_functors_if_suitable(ModuleName, DebugTypeRep, MaxTag,
        TypeCtor, TypeDefn, !TypeTable, !Specs) :-
    get_type_defn_body(TypeDefn, Body),
    (
        Body = hlds_du_type(Ctors, _ConsTagValues, _MaybeCheaperTagTest,
            DuKind, MaybeUserEqComp, MaybeAssertedDirectArgCtors,
            ReservedTag, ReservedAddr, MaybeForeign),
        (
            Ctors = [_, _ | _],
            DuKind = du_type_kind_general,
            ReservedTag = does_not_use_reserved_tag,
            ReservedAddr = does_not_use_reserved_address,
            MaybeForeign = no,
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            sym_name_get_module_name(TypeCtorSymName, TypeCtorModule)
        ->
            get_type_defn_status(TypeDefn, TypeStatus),
            (
                MaybeAssertedDirectArgCtors = yes(AssertedDirectArgFunctors)
            ;
                MaybeAssertedDirectArgCtors = no,
                AssertedDirectArgFunctors = []
            ),
            separate_out_constants(Ctors, Constants, Functors),
            list.filter(is_direct_arg_ctor(!.TypeTable, TypeCtorModule,
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
        ;
            % We cannot use the direct argument representation for any
            % functors.
            true
        )
    ;
        ( Body = hlds_eqv_type(_)
        ; Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_, _)
        ; Body = hlds_abstract_type(_)
        )
        % Leave these types alone.
    ).

:- pred is_direct_arg_ctor(type_table::in, module_name::in, import_status::in,
    list(sym_name_and_arity)::in, constructor::in) is semidet.

is_direct_arg_ctor(TypeTable, TypeCtorModule, TypeStatus,
        AssertedDirectArgCtors, Ctor) :-
    Ctor = ctor(ExistQTVars, ExistConstraints, ConsName, ConsArgs,
        _CtorContext),
    ExistQTVars = [],
    ExistConstraints = [],
    ConsArgs = [ConsArg],
    Arity = 1,
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgWidth, _ArgContext),
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeCtorArgTypes),

    (
        % Trust the `direct_arg' attribute of an imported type.
        status_is_imported(TypeStatus) = yes,
        list.contains(AssertedDirectArgCtors, ConsName / Arity)
    ->
        ArgCond = direct_arg_asserted
    ;
        % Tuples are always acceptable argument types as they are represented
        % by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet) so are not acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    ->
        ArgCond = direct_arg_builtin_type
    ;
        ArgTypeCtorArgTypes = [],
        % XXX We could let this be a subset of the type params, but that would
        % require the runtime system to be able to handle variables in the
        % argument type, during unification and comparison
        % (mercury_unify_compare_body.h) during deconstruction
        % (mercury_ml_expand_body.h), during deep copying
        % (mercury_deep_copy_body.h), and maybe during some other operations.

        search_type_ctor_defn(TypeTable, ArgTypeCtor, ArgTypeDefn),
        get_type_defn_body(ArgTypeDefn, ArgBody),
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

        (
            status_defined_in_this_module(TypeStatus) = yes,
            list.contains(AssertedDirectArgCtors, ConsName / Arity)
        ->
            ArgCond = direct_arg_asserted
        ;
            ArgTypeCtor = type_ctor(ArgTypeCtorSymName, _ArgTypeCtorArity),
            sym_name_get_module_name(ArgTypeCtorSymName, ArgTypeCtorModule),
            ( TypeCtorModule = ArgTypeCtorModule ->
                get_type_defn_status(ArgTypeDefn, ArgTypeStatus),
                ArgCond = direct_arg_same_module(ArgTypeStatus)
            ;
                ArgCond = direct_arg_different_module
            )
        )
    ),
    check_direct_arg_cond(TypeStatus, ArgCond).

:- type direct_arg_cond
    --->    direct_arg_builtin_type
            % The argument is of a builtin type that is represented with an
            % untagged pointer.

    ;       direct_arg_asserted
            % A `where direct_arg' attribute asserts that the direct arg
            % representation may be used for the constructor.

    ;       direct_arg_same_module(import_status)
            % The argument type is defined in the same module as the outer
            % type, and has the given import status.

    ;       direct_arg_different_module.
            % The argument type is defined in a different module to the outer
            % type.

:- pred check_direct_arg_cond(import_status::in, direct_arg_cond::in)
    is semidet.

check_direct_arg_cond(TypeStatus, ArgCond) :-
    require_complete_switch [TypeStatus]
    (
        % If the outer type _definition_ is not exported from this module then
        % the direct arg representation may be used.  In the absence of
        % intermodule optimisation, only this module can [de]construct values
        % of this type.
        ( TypeStatus = status_local
        ; TypeStatus = status_abstract_exported
        )
    ;
        % If the outer type is opt-exported, another module may opt-import this
        % type, but abstract-import the argument type.  It could not then infer
        % if the direct arg representation is required for any functors of the
        % outer type.  The problem is overcome by adding `where direct_arg'
        % attributes to the opt-exported type definition in .opt files,
        % which state the functors that require the direct arg representation.
        TypeStatus = status_opt_exported
    ;
        % If the outer type is exported from this module, then the direct arg
        % representation may be used, so long as any importing modules will
        % infer the same thing.
        ( TypeStatus = status_exported
        ; TypeStatus = status_exported_to_submodules
        ),
        ( ArgCond = direct_arg_builtin_type
        ; ArgCond = direct_arg_asserted
        ; ArgCond = direct_arg_same_module(status_exported)
        )
    ;
        % If the outer type is exported to sub-modules only, the argument
        % type only needs to be exported to sub-modules as well.
        TypeStatus = status_exported_to_submodules,
        ( ArgCond = direct_arg_same_module(status_exported_to_submodules)
        ; ArgCond = direct_arg_same_module(status_abstract_exported)
        )
    ;
        % The direct arg representation is required if the outer type is
        % imported, and:
        % - if the argument type is an acceptable builtin type
        % - a `where direct_arg' attribute says so
        % - if the argument type is imported from the same module
        TypeStatus = status_imported(TypeImportLocn),
        (
            ArgCond = direct_arg_builtin_type
        ;
            ArgCond = direct_arg_asserted
        ;
            ArgCond = direct_arg_same_module(status_imported(ArgImportLocn)),
            % If the argument type is only exported by an ancestor to its
            % sub-modules (of which we are one), the outer type must also only
            % be exported to sub-modules. Otherwise sub-modules and
            % non-sub-modules would infer different things.
            (
                ArgImportLocn = import_locn_ancestor_private_interface_proper
            =>
                TypeImportLocn = import_locn_ancestor_private_interface_proper
            )
        )
    ;
        % If the outer type is opt-imported, there will always be a
        % `where direct_arg' attribute on the type definition which states
        % if the direct argument representation must be used.
        ( TypeStatus = status_opt_imported
        ; TypeStatus = status_abstract_imported
        ),
        ArgCond = direct_arg_asserted
    ;
        ( TypeStatus = status_external(_)
        ; TypeStatus = status_pseudo_exported
        ; TypeStatus = status_pseudo_imported
        ),
        unexpected($module, $pred, "inappropriate status for type")
    ).

:- pred assign_direct_arg_tags(type_ctor::in, list(constructor)::in,
    int::in, int::out, int::in, list(constructor)::out,
    cons_tag_values::in, cons_tag_values::out) is det.

assign_direct_arg_tags(_, [], !Val, _, [], !CtorTags).
assign_direct_arg_tags(TypeCtor, [Ctor | Ctors], !Val, MaxTag, LeftOverCtors,
        !CtorTags) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ConsId = cons(Name, list.length(Args), TypeCtor),
    (
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.Val = MaxTag,
        Ctors = [_ | _]
    ->
        LeftOverCtors = [Ctor | Ctors]
    ;
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
    (
        Ctor = ctor(_, _, SymName, Args, Context),
        list.length(Args, Arity),
        list.contains(AssertedDirectArgCtors, SymName / Arity)
    ->
        Pieces = [words("Error:"), sym_name_and_arity(SymName / Arity),
            words("cannot be represented as a direct pointer to its"),
            words("sole argument."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_type_check, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ),
    check_incorrect_direct_arg_assertions(AssertedDirectArgCtors, Ctors,
        !Specs).

:- func constructor_to_sym_name_and_arity(constructor) = sym_name_and_arity.

constructor_to_sym_name_and_arity(ctor(_, _, Name, Args, _)) =
    Name / list.length(Args).

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
    Ctor = ctor(_ExistQVars, _Constraints, _Name, Args, _Ctxt),
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
