%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: tag_switch_util.m.
% Authors: zs.
%
% This module defines stuff for generating tag switches that is shared
% between the MLDS and LLDS back-ends.
%
%---------------------------------------------------------------------------%

:- module backend_libs.tag_switch_util.
:- interface.

:- import_module backend_libs.rtti.         % for sectag_locn
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.

%---------------------------------------------------------------------------%

% XXX This comment seems to have been misplaced. -zs 2022 feb 10.
% Map secondary tag values (-1 stands for none) to information about their
% switch arm. This "information about the switch arm" is polymorphic, because
% in the presence of switch arms that correspond to more than one cons_id,
% cons_ids whose tags may not all use the same primary tag, we will need to
% duplicate this information, with at least one copy per primary tag.
%
% In the LLDS backend, we can (and do) give a label to each goal. The
% predicates in this module will duplicate only the label, and our caller
% has the responsibility of ensuring that each label/goal pair is defined
% only once.
%
% With the MLDS, we don't (yet) do this, because some MLDS backends (e.g. Java)
% don't support labels. Instead, if need be we duplicate the HLDS goal, which
% means we will generate MLDS code for it more than once.

    % Note that it is not an error for a primary tag to have no case.
    % This can happen when
    %
    % - the in semidet switches, or in det switches where the
    % initial inst of the switch variable is a bound(...) inst representing
    % a subtype.
    %
:- type ptag_case_group(CaseRep)
    --->    one_or_more_whole_ptags(whole_ptags_info(CaseRep))
    ;       one_shared_ptag(shared_ptag_info(CaseRep)).

    % It is possible for two or more primary tag values
    % to have exactly the same action, if those ptags represent
    % cons_ids that share the same arm of the switch.
:- type whole_ptags_info(CaseRep)
    --->    whole_ptags_info(
                % The first and any later ptag values that have this code.
                % For all of them, the following invariant holds:
                %
                % - either the ptag is unshared (i.e. its sectag_locn
                %   is either sectag_none or sectag_none_direct_arg,
                %
                % - or the ptag is shared between two or more function symbols,
                %   meaning its sectag_locn is one of the alternatives
                %   in shared_sectag_locn, but the selected switch arm
                %   is CaseRep for *all* the possible sectag values.
                wpi_head_ptag           :: ptag,
                wpi_tail_ptags          :: list(ptag),

                % The number of function symbols represented by this group.
                wpi_num_functors        :: uint,

                % A representation of the code for this primary tag.
                wpi_case_rep            :: CaseRep
            ).

:- type shared_ptag_info(CaseRep)
    --->    shared_ptag_info(
                % This ptag is shared by more than one function symbol,
                % and the selected switch arm is NOT the same for all of them.
                % (If the selected switch arm *were* the same for all of them,
                % this ptag value would occur in a whole_ptags_info.)

                spi_ptag                :: ptag,
                spi_sectag_locn         :: shared_sectag_locn,

                % MaxSectag, the maximum secondary tag value for this ptag.
                % The number of function symbols that share this ptag
                % will be MaxSectag + 1 (since sectag values start at 0u).
                spi_max_sectag          :: uint,

                % Does the switch on the secondary tag cover all possible
                % sectag values?
                spi_complete            :: is_switch_complete,

                % The number of function symbols represented by this group.
                spi_num_functors        :: uint,

                % The next two fields contain the same information in
                % slightly different form. The spi_sectag_to_goal_map field
                % contains this info in the form preferred as the input
                % for the jump table and binary search methods of switching
                % on the secondary tag, while the spi_goal_to_sectags_map
                % contains it in the form preferred by the try_chain and
                % try_me_else chain methods.

                % This field maps each secondary tag value to the case arm
                % it selects.
                spi_sectag_to_goal_map  :: sectag_goal_map(CaseRep),

                % This field contains the reverse map, and groups together
                % the sectag values that all select the same switch arm.
                %
                % As explained above, we use shared_ptag_infos only when
                % the switch requires at least two different actions for
                % some of the different sectag values sharing this ptag.
                % However, it is still possible for this map to have
                % just one entry, if
                %
                % - the function symbols using this shared ptag and the
                %   sectag values listed as the value of that one entry select
                %   the switch arm listed in the key of that entry, and
                %
                % - some of the other possible sectag values for this shared
                %   ptag have *no* selected arm in this switch, either because
                %   the switched-on variable cannot be bound to the
                %   corresponding function symbol when the switch is entered,
                %   or because the switch should fail if the variable *is*
                %   bound to that function symbol.
                %
                % However, this map cannot be empty.
                spi_goal_to_sectags_map :: sectag_case_map(CaseRep)
            ).

:- type shared_sectag_locn =< sectag_locn
    --->    sectag_local_rest_of_word
    ;       sectag_local_bits(uint8, uint)              % #bits, mask
    ;       sectag_remote_word
    ;       sectag_remote_bits(uint8, uint).            % #bits, mask

    % Does the switch (which in our case is on a secondary tag)
    % cover all the possible values in the switched-on value's range?
    %
    % Note that this notion is subtly different from the notion
    % "can this switch fail?". That is because it is possible for
    % an incomplete_switch switch to be cannot_fail, if the values
    % which are not covered by the switch's cases cannot occur as the
    % switched-on value.
:- type is_switch_complete
    --->    incomplete_switch
    ;       complete_switch.

    % Map each secondary tag value to the representation of the associated
    % code. A negative secondary tag "value" means "no secondary tag".
    %
    % It is of course possible that there is more than one secondary tag value
    % that maps to the same code. Exploiting such sharing is up to
    % backend-specific code.
    %
:- type sectag_goal_map(CaseRep)  == map(uint, CaseRep).
:- type sectag_goal_list(CaseRep) == assoc_list(uint, CaseRep).

:- type sectag_case_map(CaseRep)  == map(CaseRep, one_or_more(uint)).
:- type sectag_case(CaseRep)      == pair(CaseRep, one_or_more(uint)).
:- type sectag_case_list(CaseRep) == assoc_list(CaseRep, one_or_more(uint)).

    % group_cases_by_ptag(ModuleInfo, VarType, TaggedCases, RepresentCase,
    %   !StateA, !StateB, !StateC, !StateD,
    %   PtagGroups, NumPtagsUsed, MaxPtagUint8):
    %
    % Group together all the cases that depend on the given variable
    % having the same primary tag value.
    %
:- pred group_cases_by_ptag(module_info::in, mer_type::in,
    list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    list(ptag_case_group(CaseRep))::out, int::out, uint8::out) is det.

    % Order the given groups based on the number of function symbols they
    % represent, putting the ones with the most function symbols first.
    % Break ties by using the order of the first ptag in each group.
    %
:- pred order_ptag_groups_by_count(list(ptag_case_group(CaseRep))::in,
    list(ptag_case_group(CaseRep))::out) is det.

:- type empty_ptag_list =< list(ptag)
    --->    [].

:- type whole_ptag_info(CaseRep) =< whole_ptags_info(CaseRep)
    --->    whole_ptags_info(
                wpi_head_ptag           :: ptag,
                wpi_tail_ptags          :: empty_ptag_list,
                wpi_num_functors        :: uint,
                wpi_case_rep            :: CaseRep
            ).

:- type single_ptag_case(CaseRep) =< ptag_case_group(CaseRep)
    --->    one_or_more_whole_ptags(whole_ptag_info(CaseRep))
    ;       one_shared_ptag(shared_ptag_info(CaseRep)).

    % Ensure that each ptag_case_group covers only one ptag value,
    % breaking up any entry in the input that lists two or more ptag values.
    % Put the resulting ptag_case_groups, which are now all specific
    % to one ptag value, into ascending order of those values.
    %
:- pred order_ptag_specific_groups_by_value(
    list(ptag_case_group(CaseRep))::in,
    list(single_ptag_case(CaseRep))::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.prog_type.

:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Map primary tag values to the set of their switch arms.
    %
    % This data structure does not recognize situations in which different
    % ptag values need the same treatment. It is now used only as an
    % intermediate data structure on the way to a data structure,
    % a list of ptag_case_groups, which *does* recognize such sharing.
:- type ptag_case_map(CaseRep) == map(ptag, ptag_case(CaseRep)).

:- type ptag_case_entry(CaseRep)
    --->    ptag_case_entry(
                % The ptag value that has this code.
                ptag,

                % A representation of the code for this primary tag.
                ptag_case(CaseRep)
            ).

:- type ptag_case(CaseRep)
    --->    ptag_case_unshared(
                % The ptag is not shared between function symbols.
                CaseRep
            )
    ;       ptag_case_shared(
                % The ptag is shared between function symbols.
                % The next two fields give the location of the sectag,
                % and its maximum value.
                shared_sectag_locn,
                uint,

                % The map from the value of the sectag to its case.
                sectag_goal_map(CaseRep)
            ).

%---------------------------------------------------------------------------%

group_cases_by_ptag(ModuleInfo, VarType, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD,
        PtagGroups, NumPtagsUsed, MaxPtagUint8) :-
    get_ptag_counts(ModuleInfo, VarType, MaxPtagUint8, PtagSectagMap),
    group_cases_by_ptag_loop(PtagSectagMap, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, map.init, PtagCaseMap),
    map.to_assoc_list(PtagCaseMap, PtagCaseList),
    list.length(PtagCaseList, NumPtagsUsed),
    build_ptag_groups(PtagSectagMap, PtagCaseList,
        map.init, WholePtagsMap, [], SharedPtagInfos),
    map.values(WholePtagsMap, WholePtagsInfos),
    WholePtagGroups = list.map(wrap_whole_ptags_info, WholePtagsInfos),
    SharedPtagGroups = list.map(wrap_shared_ptag_info, SharedPtagInfos),
    PtagGroups = WholePtagGroups ++ SharedPtagGroups.

:- pred group_cases_by_ptag_loop(ptag_sectag_map::in, list(tagged_case)::in,
    pred(tagged_case, CaseRep, StateA, StateA, StateB, StateB,
        StateC, StateC, StateD, StateD)
        ::in(pred(in, out, in, out, in, out, in, out, in, out) is det),
    StateA::in, StateA::out, StateB::in, StateB::out,
    StateC::in, StateC::out, StateD::in, StateD::out,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_cases_by_ptag_loop(_, [], _,
        !StateA, !StateB, !StateC, !StateD, !PtagCaseMap).
group_cases_by_ptag_loop(PtagSectagMap, [TaggedCase | TaggedCases],
        RepresentCase, !StateA, !StateB, !StateC, !StateD, !PtagCaseMap) :-
    TaggedCase = tagged_case(MainTaggedConsId, OtherConsIds, _CaseId, _Goal),
    RepresentCase(TaggedCase, CaseRep, !StateA, !StateB, !StateC, !StateD),
    group_case_by_ptag(PtagSectagMap, CaseRep,
        MainTaggedConsId, !PtagCaseMap),
    list.foldl(group_case_by_ptag(PtagSectagMap, CaseRep),
        OtherConsIds, !PtagCaseMap),
    group_cases_by_ptag_loop(PtagSectagMap, TaggedCases, RepresentCase,
        !StateA, !StateB, !StateC, !StateD, !PtagCaseMap).

:- pred group_case_by_ptag(ptag_sectag_map::in, CaseRep::in,
    tagged_cons_id::in,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

group_case_by_ptag(PtagSectagMap, CaseRep, TaggedConsId, !PtagCaseMap) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    (
        ConsTag = direct_arg_tag(Ptag),
        ( if map.search(!.PtagCaseMap, Ptag, _Group) then
            unexpected($pred, "unshared tag is shared")
        else
            PtagCase = ptag_case_unshared(CaseRep),
            map.det_insert(Ptag, PtagCase, !PtagCaseMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            ( if map.search(!.PtagCaseMap, Ptag, _Group) then
                unexpected($pred, "unshared tag is shared")
            else
                PtagCase = ptag_case_unshared(CaseRep),
                map.det_insert(Ptag, PtagCase, !PtagCaseMap)
            )
        ;
            (
                RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
                RemoteSectag = remote_sectag(Sectag, SectagSize),
                (
                    SectagSize = rsectag_word,
                    SectagLocn = sectag_remote_word
                ;
                    SectagSize = rsectag_subword(SectagBits),
                    SectagBits = sectag_bits(NumSectagBits, Mask),
                    SectagLocn = sectag_remote_bits(NumSectagBits, Mask)
                )
            ;
                RemoteArgsTagInfo = remote_args_ctor(Data),
                Primary = 0u8,
                Ptag = ptag(Primary),
                SectagLocn = sectag_remote_word,
                Sectag = Data
            ),
            add_sectag_to_shared_ptag(PtagSectagMap, Ptag,
                SectagLocn, Sectag, CaseRep, !PtagCaseMap)
        )
    ;
        (
            ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
            LocalSectag = local_sectag(Sectag, _, SectagBits),
            (
                MustMask = lsectag_always_rest_of_word,
                SectagLocn = sectag_local_rest_of_word
            ;
                MustMask = lsectag_must_be_masked,
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ;
            ConsTag = local_args_tag(LocalArgsTagInfo),
            (
                LocalArgsTagInfo = local_args_only_functor,
                % You can't switch on a variable of a type that has
                % only one function symbol.
                unexpected($pred, "local_args_only_functor")
            ;
                LocalArgsTagInfo = local_args_not_only_functor(Ptag,
                    LocalSectag),
                LocalSectag = local_sectag(Sectag, _, SectagBits),
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ),
        add_sectag_to_shared_ptag(PtagSectagMap, Ptag,
            SectagLocn, Sectag, CaseRep, !PtagCaseMap)
    ;
        ( ConsTag = no_tag
        ; ConsTag = dummy_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = closure_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "non-du tag")
    ).

:- pred add_sectag_to_shared_ptag(ptag_sectag_map::in,
    ptag::in, shared_sectag_locn::in, uint::in, CaseRep::in,
    ptag_case_map(CaseRep)::in, ptag_case_map(CaseRep)::out) is det.

add_sectag_to_shared_ptag(PtagSectagMap, Ptag, SectagLocn, Sectag, CaseRep,
        !PtagCaseMap) :-
    ( if map.search(!.PtagCaseMap, Ptag, PtagCase0) then
        (
            PtagCase0 = ptag_case_unshared(_),
            unexpected($pred, "adding shared to unshared")
        ;
            PtagCase0 = ptag_case_shared(SectagLocn0, MaxSectag,
                SectagGoalMap0),
            expect(unify(SectagLocn0, SectagLocn), $pred,
                "sectag locn mismatch"),
            map.det_insert(Sectag, CaseRep, SectagGoalMap0, SectagGoalMap),
            PtagCase = ptag_case_shared(SectagLocn, MaxSectag, SectagGoalMap),
            map.det_update(Ptag, PtagCase, !PtagCaseMap)
        )
    else
        map.lookup(PtagSectagMap, Ptag, PtagSectagInfo),
        PtagSectagInfo = ptag_sectag_info(InfoSectagLocn, MaxSectagInt),
        expect(unify(coerce(SectagLocn), InfoSectagLocn), $pred,
            "SectagLocn != InfoSectagLocn"),
        % For a shared secondary tag, MaxSectagInt cannot be negative.
        MaxSectag = uint.det_from_int(MaxSectagInt),
        SectagGoalMap = map.singleton(Sectag, CaseRep),
        PtagCase = ptag_case_shared(SectagLocn, MaxSectag, SectagGoalMap),
        map.det_insert(Ptag, PtagCase, !PtagCaseMap)
    ).

%---------------------------------------------------------------------------%

:- type whole_ptags_map(CaseRep) == map(CaseRep, whole_ptags_info(CaseRep)).

:- pred build_ptag_groups(ptag_sectag_map::in,
    assoc_list(ptag, ptag_case(CaseRep))::in,
    whole_ptags_map(CaseRep)::in, whole_ptags_map(CaseRep)::out,
    list(shared_ptag_info(CaseRep))::in, list(shared_ptag_info(CaseRep))::out)
    is det.

build_ptag_groups(_PtagSectagMap, [], !WholePtagsMap, !SharedPtagInfos).
build_ptag_groups(PtagSectagMap, [Entry | Entries],
        !WholePtagsMap, !SharedPtagInfos) :-
    Entry = Ptag - PtagCase,
    (
        PtagCase = ptag_case_unshared(CaseRep),
        record_whole_ptag(Ptag, 1u, CaseRep, !WholePtagsMap)
    ;
        PtagCase = ptag_case_shared(SharedSectagLocn, MaxSectag,
            SectagGoalMap),
        % There will only ever be at most one primary tag value with
        % a shared local tag, and there will only ever be at most one primary
        % tag value with a shared remote tag, so we can never have
        %
        % - two ptags with SectagLocn = sectag_local_*
        % - two ptags with SectagLocn = sectag_remote
        %
        % We can never have two entries where one is sectag_local_bits(_)
        % and the other is sectag_local_rest_of_word; all function symbols
        % whose representation includes a local sectag must agree on whether
        % that sectag may ever be followed by arguments (sectag_local_bits)
        % or not (sectag_local_rest_of_word).
        %
        % We can have two ptags, one with either SectagLocn =
        % sectag_local_bits(_) or SectagLocn = sectag_local_rest_of_word,
        % and the other with SectagLocn = sectag_remote, but even if their
        % sectag_value to code maps were identical, their overall code couldn't
        % be identical, since they would have to get the secondary tags from
        % different places.

        map.lookup(PtagSectagMap, Ptag, SectagInfo),
        SectagInfo = ptag_sectag_info(SectagLocn, MaybeMaxSectag),
        expect(unify(coerce(SharedSectagLocn), SectagLocn), $pred,
            "SharedSectagLocn != SectagLocn"),
        ( if
            uint.from_int(MaybeMaxSectag, MaxSectag0),
            MaxSectag0 \= MaxSectag
        then
            unexpected($pred, "MaxSectag0 != MaxSectag")
        else
            true
        ),
        map.foldl2(build_sectag_case_cord_map, SectagGoalMap,
            map.init, SectagCaseCordMap, 0u, NumFunctors),
        SectagCaseMap =
            map.map_values_only(cord_to_one_or_more, SectagCaseCordMap),
        map.to_sorted_assoc_list(SectagCaseMap, SectagCaseList),
        % The +1 is to account for the fact that secondary tags go from
        % 0 to MaxSectag, both inclusive.
        ( if NumFunctors = MaxSectag + 1u then
            SectagSwitchComplete = complete_switch
        else
            SectagSwitchComplete = incomplete_switch
        ),
        ( if
            SectagCaseList = [OneSectagCase],
            SectagSwitchComplete = complete_switch
        then
            % By recording CaseRep as being in effect for *all* function
            % symbols that use Ptag, we allow this specific ptag value
            % to be grouped together with any *other* ptag values that
            % also select CaseRep.
            %
            % This optimization is generally useful, but it is particularly
            % important for compiler-generated comparison predicates created
            % using the quadratic algorithm, because for those, all cons_ids
            % in the type fall into two or three categories:
            %
            % - those less than the current cons_id,
            % - those equal to (i.e. same as) the current cons_id,
            % - those greater than the current cons_id.
            %
            % (One or the other of the first and third sets may be empty.)
            % If all function symbols sharing a given ptag value all fall into
            % (say) the first category, we don't want to test for that ptag
            % value separately from all the other ptag values in that category.
            OneSectagCase = CaseRep - _SectagValues,
            record_whole_ptag(Ptag, NumFunctors, CaseRep, !WholePtagsMap)
        else
            SharedPtagInfo = shared_ptag_info(Ptag, SharedSectagLocn,
                MaxSectag, SectagSwitchComplete, NumFunctors,
                SectagGoalMap, SectagCaseMap),
            !:SharedPtagInfos = [SharedPtagInfo | !.SharedPtagInfos]
        )
    ),
    build_ptag_groups(PtagSectagMap, Entries,
        !WholePtagsMap, !SharedPtagInfos).

:- pred record_whole_ptag(ptag::in, uint::in, CaseRep::in,
    whole_ptags_map(CaseRep)::in, whole_ptags_map(CaseRep)::out) is det.

record_whole_ptag(Ptag, PtagNumFunctors, CaseRep, !GroupMap) :-
    ( if map.search(!.GroupMap, CaseRep, Entry0) then
        Entry0 = whole_ptags_info(HeadPtag, TailPtags0, NumFunctors0,
            CaseRep0),
        expect(unify(CaseRep, CaseRep0), $pred, "CaseRep != CaseRep0"),
        % This is quadratic, but this does not matter, because
        % we can append to the list at most 6 time. The reason is:
        %
        % - a type can have at most eight ptags;
        % - you can't have a switch in which all eight map to the same
        %   CaseRep, because that would be a switch with only one arm,
        %   which we simplify away, and
        % - for the first ptag, the map.search fails, and does not
        %   require an append.
        TailPtags = TailPtags0 ++ [Ptag],
        NumFunctors = NumFunctors0 + PtagNumFunctors,
        Entry = whole_ptags_info(HeadPtag, TailPtags, NumFunctors, CaseRep),
        map.det_update(CaseRep, Entry, !GroupMap)
    else
        Entry = whole_ptags_info(Ptag, [], PtagNumFunctors, CaseRep),
        map.det_insert(CaseRep, Entry, !GroupMap)
    ).

:- type sectag_case_cord_map(CaseRep) == map(CaseRep, cord(uint)).

:- pred build_sectag_case_cord_map(uint::in, CaseRep::in,
    sectag_case_cord_map(CaseRep)::in, sectag_case_cord_map(CaseRep)::out,
    uint::in, uint::out) is det.

build_sectag_case_cord_map(Sectag, CaseRep,
        !SectagCaseCordMap, !NumFunctors) :-
    !:NumFunctors = !.NumFunctors + 1u,
    ( if map.search(!.SectagCaseCordMap, CaseRep, Cord0) then
        cord.snoc(Sectag, Cord0, Cord),
        map.det_update(CaseRep, Cord, !SectagCaseCordMap)
    else
        map.det_insert(CaseRep, cord.singleton(Sectag), !SectagCaseCordMap)
    ).

:- func cord_to_one_or_more(cord(uint)) = one_or_more(uint).

cord_to_one_or_more(Cord) = OoM :-
    List = cord.list(Cord),
    list.det_head_tail(List, Head, Tail),
    OoM = one_or_more(Head, Tail).

:- func wrap_whole_ptags_info(whole_ptags_info(CaseRep)) =
    ptag_case_group(CaseRep).

wrap_whole_ptags_info(WholeInfo) = one_or_more_whole_ptags(WholeInfo).

:- func wrap_shared_ptag_info(shared_ptag_info(CaseRep)) = 
    ptag_case_group(CaseRep).

wrap_shared_ptag_info(SharedInfo) = one_shared_ptag(SharedInfo).

%---------------------------------------------------------------------------%

order_ptag_groups_by_count(Groups, SortedGroups) :-
    list.sort(order_groups_by_more_functors, Groups, SortedGroups).

:- pred order_groups_by_more_functors(ptag_case_group(CaseRep)::in,
    ptag_case_group(CaseRep)::in, comparison_result::out) is det.

order_groups_by_more_functors(GroupA, GroupB, CompareResult) :-
    NumFunctorsA = num_functors_in_ptag_case_group(GroupA),
    NumFunctorsB = num_functors_in_ptag_case_group(GroupB),
    ( if NumFunctorsA > NumFunctorsB then
        % We want the groups with the largest counts first ...
        CompareResult = (<)
    else if NumFunctorsA < NumFunctorsB then
        % ... and the groups with the smallest counts last.
        CompareResult = (>)
    else
        % If two groups cover the same number of function symbols,
        % then put them in the order given by their main ptags.
        MainPtagA = main_ptag_in_ptag_case_group(GroupA),
        MainPtagB = main_ptag_in_ptag_case_group(GroupB),
        compare(CompareResult, MainPtagA, MainPtagB)
    ).

:- func num_functors_in_ptag_case_group(ptag_case_group(CaseRep)) = uint.

num_functors_in_ptag_case_group(Group) = NumFunctors :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        NumFunctors = WholeInfo ^ wpi_num_functors
    ;
        Group = one_shared_ptag(SharedInfo),
        NumFunctors = SharedInfo ^ spi_num_functors
    ).

:- func main_ptag_in_ptag_case_group(ptag_case_group(CaseRep)) = uint8.

main_ptag_in_ptag_case_group(Group) = MainPtagUint8 :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        ptag(MainPtagUint8) = WholeInfo ^ wpi_head_ptag
    ;
        Group = one_shared_ptag(SharedInfo),
        ptag(MainPtagUint8) = SharedInfo ^ spi_ptag
    ).

%---------------------------------------------------------------------------%

order_ptag_specific_groups_by_value(Groups0, SortedSpecificGroups) :-
    specialize_and_record_ptag_case_groups(Groups0, map.init, SpecificMap),
    map.values(SpecificMap, SortedSpecificGroups).

:- pred specialize_and_record_ptag_case_groups(
    list(ptag_case_group(CaseRep))::in,
    map(ptag, single_ptag_case(CaseRep))::in,
    map(ptag, single_ptag_case(CaseRep))::out) is det.

specialize_and_record_ptag_case_groups([], !SpecificMap).
specialize_and_record_ptag_case_groups([Group | Groups], !SpecificMap) :-
    (
        Group = one_or_more_whole_ptags(WholeInfo),
        WholeInfo = whole_ptags_info(MainPtag, OtherPtags, _NF, CaseRep),
        record_specialized_versions(CaseRep, [MainPtag | OtherPtags],
            !SpecificMap)
    ;
        Group = one_shared_ptag(SharedInfo),
        SharedInfo = shared_ptag_info(Ptag, _, _, _, _, _, _),
        SingleGroup = one_shared_ptag(SharedInfo),
        map.det_insert(Ptag, SingleGroup, !SpecificMap)
    ),
    specialize_and_record_ptag_case_groups(Groups, !SpecificMap).

:- pred record_specialized_versions(CaseRep::in, list(ptag)::in,
    map(ptag, single_ptag_case(CaseRep))::in,
    map(ptag, single_ptag_case(CaseRep))::out) is det.

record_specialized_versions(_, [], !SpecificMap).
record_specialized_versions(CaseRep, [Ptag | Ptags], !SpecificMap) :-
    % In the original whole_ptags_info we are splitting up, each of the
    % ptags is an unshared ptag, which means that it corresponds to
    % exactly one function symbol.
    WholeInfo = whole_ptags_info(Ptag, [], 1u, CaseRep),
    Group = one_or_more_whole_ptags(WholeInfo),
    map.det_insert(Ptag, Group, !SpecificMap),
    record_specialized_versions(CaseRep, Ptags, !SpecificMap).

%---------------------------------------------------------------------------%

    % Map primary tag values to information about the secondary tag, if any,
    % for the ptag.
    %
:- type ptag_sectag_map == map(ptag, ptag_sectag_info).
:- type ptag_sectag_info
    --->    ptag_sectag_info(
                % Terms using this primary tags have their secondary tags,
                % if any, in the location indicated by this field.
                sectag_locn,
                
                % The maximum value of the secondary tag, or -1
                % if this ptag has no secondary tag.
                %
                % If this field is MaxSectag, then the number of values using
                % this primary tag is
                %
                % 1,                if MaxSectag = -1
                % MaxSectag + 1,    if MaxSectag != -1
                %
                % The +1 is there to account for the fact that we start
                % assigning secondary tags values at 0, not 1.
                int
            ).

    % get_ptag_counts(ModuleInfo, Type, MaxPrimary, PtagSectagMap):
    %
    % Find the maximum primary used in Type, and find out how many
    % secondary tags share each one of the used primary tags.
    %
    % ZZZ It may be interesting to instrument this code to see whether
    % it is worth caching its output somewhere, though in that case we would
    % have to change the interface to take a type_ctor, not a mer_type.
    % (We only ever look at the top type_ctor of the supplied type.)
    %
:- pred get_ptag_counts(module_info::in, mer_type::in,
    uint8::out, ptag_sectag_map::out) is det.

get_ptag_counts(ModuleInfo, Type, MaxPrimary, PtagSectagMap) :-
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(type_body_du(_, _, _, MaybeRepn, _)),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn),
            CtorRepns = Repn ^ dur_ctor_repns
        )
    ;
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(_)
        ),
        unexpected($pred, "non-du type")
    ),
    MaXPrimary0 = 0u8,
    map.init(PtagSectagMap0),
    get_ptag_counts_loop(CtorRepns,
        MaXPrimary0, MaxPrimary, PtagSectagMap0, PtagSectagMap).

:- pred get_ptag_counts_loop(list(constructor_repn)::in, uint8::in, uint8::out,
    ptag_sectag_map::in, ptag_sectag_map::out) is det.

get_ptag_counts_loop([], !MaxPrimary, !PtagSectagMap).
get_ptag_counts_loop([CtorRepn | CtorRepns], !MaxPrimary, !PtagSectagMap) :-
    ConsTag = CtorRepn ^ cr_tag,
    (
        ConsTag = direct_arg_tag(Ptag),
        SectagLocn = sectag_none_direct_arg,
        Ptag = ptag(Primary),
        !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
        ( if map.search(!.PtagSectagMap, Ptag, _) then
            unexpected($pred, "unshared tag is shared")
        else
            Info = ptag_sectag_info(SectagLocn, -1),
            map.det_insert(Ptag, Info, !PtagSectagMap)
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            SectagLocn = sectag_none,
            Ptag = ptag(Primary),
            !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
            ( if map.search(!.PtagSectagMap, Ptag, _) then
                unexpected($pred, "unshared tag is shared")
            else
                Info = ptag_sectag_info(SectagLocn, -1),
                map.det_insert(Ptag, Info, !PtagSectagMap)
            )
        ;
            (
                RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
                Ptag = ptag(Primary),
                RemoteSectag = remote_sectag(SecondaryUint, SectagSize),
                (
                    SectagSize = rsectag_word,
                    SectagLocn = sectag_remote_word
                ;
                    SectagSize = rsectag_subword(SectagBits),
                    SectagBits = sectag_bits(NumSectagBits, Mask),
                    SectagLocn = sectag_remote_bits(NumSectagBits, Mask)
                ),
                Secondary = uint.cast_to_int(SecondaryUint)
            ;
                RemoteArgsTagInfo = remote_args_ctor(Data),
                Primary = 0u8,
                Ptag = ptag(Primary),
                SectagLocn = sectag_remote_word,
                Secondary = uint.cast_to_int(Data)
            ),
            !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
            ( if map.search(!.PtagSectagMap, Ptag, Info0) then
                Info0 = ptag_sectag_info(OldSectagLocn, MaxSoFar),
                expect(unify(OldSectagLocn, SectagLocn), $pred,
                    "remote tag is shared with non-remote"),
                int.max(Secondary, MaxSoFar, Max),
                Info = ptag_sectag_info(SectagLocn, Max),
                map.det_update(Ptag, Info, !PtagSectagMap)
            else
                Info = ptag_sectag_info(SectagLocn, Secondary),
                map.det_insert(Ptag, Info, !PtagSectagMap)
            )
        )
    ;
        (
            ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
            LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
            (
                MustMask = lsectag_always_rest_of_word,
                SectagLocn = sectag_local_rest_of_word
            ;
                MustMask = lsectag_must_be_masked,
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ;
            ConsTag = local_args_tag(LocalArgsTagInfo),
            (
                LocalArgsTagInfo = local_args_only_functor,
                % You can't switch on a variable of a type that has
                % only one function symbol.
                unexpected($pred, "local_args_only_functor")
            ;
                LocalArgsTagInfo = local_args_not_only_functor(Ptag,
                    LocalSectag),
                LocalSectag = local_sectag(SecondaryUint, _, SectagBits),
                SectagBits = sectag_bits(NumSectagBits, Mask),
                SectagLocn = sectag_local_bits(NumSectagBits, Mask)
            )
        ),
        Ptag = ptag(Primary),
        Secondary = uint.cast_to_int(SecondaryUint),
        !:MaxPrimary = uint8.max(Primary, !.MaxPrimary),
        ( if map.search(!.PtagSectagMap, Ptag, Info0) then
            Info0 = ptag_sectag_info(OldSectagLocn, MaxSoFar),
            expect(unify(OldSectagLocn, SectagLocn), $pred,
                "local tag is shared with non-local"),
            int.max(Secondary, MaxSoFar, Max),
            Info = ptag_sectag_info(SectagLocn, Max),
            map.det_update(Ptag, Info, !PtagSectagMap)
        else
            Info = ptag_sectag_info(SectagLocn, Secondary),
            map.det_insert(Ptag, Info, !PtagSectagMap)
        )
    ;
        ( ConsTag = no_tag
        ; ConsTag = dummy_tag
        ; ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = closure_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "non-du tag")
    ),
    get_ptag_counts_loop(CtorRepns, !MaxPrimary, !PtagSectagMap).

%---------------------------------------------------------------------------%
:- end_module backend_libs.tag_switch_util.
%---------------------------------------------------------------------------%
