%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module contains code to find the top procedures by several criteria.
%
% For comparisons on costs, we sort highest first. For comparisons on names and
% contexts, we sort lowest first. This is consistently what users want.
%-----------------------------------------------------------------------------%

:- module top_procs.

:- interface.

:- import_module interface.
:- import_module measurements.
:- import_module profile.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- func find_top_procs(cost_kind, include_descendants, measurement_scope,
    display_limit, deep) = maybe_error(list(int)).

    % A line group consists of a first line, and optionally a group of later
    % lines. The first line is structured as a sequence of fields, which may
    % have one or two id fields but must have all the measurement fields
    % demanded by the current preferences. The later lines must all be full
    % rows.
    %
:- type line_group(FL, LL)
    --->    line_group(
                group_filename      :: string,
                group_linenumber    :: int,
                group_name          :: string,
                group_own           :: own_prof_info,
                group_desc          :: inherit_prof_info,
                group_first_line_id :: string,
                group_later_lines   :: LL
            ).

:- func sort_line_groups(order_criteria, list(line_group(FL, LL)))
    = list(line_group(FL, LL)).

:- pred sum_line_group_measurements(list(line_group(FL, LL))::in,
    own_prof_info::out, inherit_prof_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

find_top_procs(Sort, InclDesc, Scope, Limit, Deep) = MaybeTopPSIs :-
    find_top_sort_predicate(Sort, InclDesc, Scope, SortCompatible,
        RawSortFunc, FilterPred),
    (
        SortCompatible = no,
        MaybeTopPSIs = error("bad sort specification")
    ;
        SortCompatible = yes,
        ProcStatics = Deep ^ proc_statics,
        array.max(ProcStatics, MaxProcStatic),
        PSIs0 = 1 .. MaxProcStatic,
        deep_lookup_proc_dynamics(Deep, Deep ^ root, RootPD),
        RootPD ^ pd_proc_static = proc_static_ptr(RootPSI),
        list.filter(filter_top_procs(Deep, RootPSI, FilterPred), PSIs0, PSIs),
        SortPred = (pred(PSI1::in, PSI2::in, ComparisonResult::out) is det :-
            ComparisonResult =
                compare_procs_fallback(RawSortFunc, Deep, PSI1, PSI2)
        ),
        list.sort(SortPred, PSIs, DescendingPSIs),
        (
            Limit = rank_range(First, Last),
            ( list.drop(First - 1, DescendingPSIs, RemainingPSIs) ->
                list.take_upto(Last - First + 1, RemainingPSIs, TopPSIs),
                MaybeTopPSIs = ok(TopPSIs)
            ;
                MaybeTopPSIs = ok([])
            )
        ;
            Limit = threshold(Threshold),
            find_threshold_predicate(Sort, InclDesc,
                ThresholdCompatible, RawThresholdPred),
            (
                ThresholdCompatible = no,
                MaybeTopPSIs = error("bad threshold specification")
            ;
                ThresholdCompatible = yes,
                ThresholdPred = (pred(PSI::in) is semidet :-
                    RawThresholdPred(Deep, Threshold, PSI)
                ),
                list.takewhile(ThresholdPred, DescendingPSIs, TopPSIs, _),
                MaybeTopPSIs = ok(TopPSIs)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- type compare_proc_statics == (func(deep, int, int) = comparison_result).

:- func compare_procs_fallback(compare_proc_statics, deep, int, int)
    = comparison_result.

compare_procs_fallback(MainFunc, Deep, PSI1, PSI2) = Result :-
    Result0 = MainFunc(Deep, PSI1, PSI2),
    ( Result0 \= (=) ->
        Result = Result0
    ;
        Result1 = compare_ps_time_both_overall(Deep, PSI1, PSI2),
        ( Result1 \= (=) ->
            Result = Result1
        ;
            Result = compare_ps_words_both_overall(Deep,
                PSI1, PSI2)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred filter_top_procs(deep::in, int::in,
    pred(deep, int)::in(pred(in, in) is semidet), int::in) is semidet.

filter_top_procs(Deep, RootPSI, FilterPred, PSI) :-
    PSI \= RootPSI,
    FilterPred(Deep, PSI).

:- pred find_top_sort_predicate(cost_kind::in, include_descendants::in,
    measurement_scope::in, bool::out,
    compare_proc_statics::out(func(in, in, in) = out is det),
    pred(deep, int)::out(pred(in, in) is semidet)) is det.

find_top_sort_predicate(calls,  self,          overall,  yes,
    compare_ps_calls_self_overall,  filter_ps_calls_self).
find_top_sort_predicate(calls,  self,          per_call, no,
    compare_ps_calls_self_overall,  filter_ps_calls_self).
find_top_sort_predicate(calls,  self_and_desc, overall,  no,
    compare_ps_calls_self_overall,  filter_ps_calls_self).
find_top_sort_predicate(calls,  self_and_desc, per_call, no,
    compare_ps_calls_self_overall,  filter_ps_calls_self).
find_top_sort_predicate(redos,  self,          overall,  yes,
    compare_ps_redos_self_overall,  filter_ps_redos_self).
find_top_sort_predicate(redos,  self,          per_call, no,
    compare_ps_redos_self_overall,  filter_ps_redos_self).
find_top_sort_predicate(redos,  self_and_desc, overall,  no,
    compare_ps_redos_self_overall,  filter_ps_redos_self).
find_top_sort_predicate(redos,  self_and_desc, per_call, no,
    compare_ps_redos_self_overall,  filter_ps_redos_self).
find_top_sort_predicate(time,   self,          overall,  yes,
    compare_ps_time_self_overall,   filter_ps_time_self).
find_top_sort_predicate(time,   self,          per_call, yes,
    compare_ps_time_self_percall,   filter_ps_time_self).
find_top_sort_predicate(time,   self_and_desc, overall,  yes,
    compare_ps_time_both_overall,   filter_ps_time_both).
find_top_sort_predicate(time,   self_and_desc, per_call, yes,
    compare_ps_time_both_percall,   filter_ps_time_both).
find_top_sort_predicate(allocs, self,          overall,  yes,
    compare_ps_allocs_self_overall, filter_ps_allocs_self).
find_top_sort_predicate(allocs, self,          per_call, yes,
    compare_ps_allocs_self_percall, filter_ps_allocs_self).
find_top_sort_predicate(allocs, self_and_desc, overall,  yes,
    compare_ps_allocs_both_overall, filter_ps_allocs_both).
find_top_sort_predicate(allocs, self_and_desc, per_call, yes,
    compare_ps_allocs_both_percall, filter_ps_allocs_both).
find_top_sort_predicate(words,  self,          overall,  yes,
    compare_ps_words_self_overall,  filter_ps_words_self).
find_top_sort_predicate(words,  self,          per_call, yes,
    compare_ps_words_self_percall,  filter_ps_words_self).
find_top_sort_predicate(words,  self_and_desc, overall,  yes,
    compare_ps_words_both_overall,  filter_ps_words_both).
find_top_sort_predicate(words,  self_and_desc, per_call, yes,
    compare_ps_words_both_percall,  filter_ps_words_both).

:- pred find_threshold_predicate(cost_kind::in, include_descendants::in,
    bool::out, pred(deep, float, int)::out(pred(in, in, in) is semidet))
    is det.

find_threshold_predicate(calls,  self,          no,  threshold_ps_time_self).
find_threshold_predicate(calls,  self_and_desc, no,  threshold_ps_time_self).
find_threshold_predicate(redos,  self,          no,  threshold_ps_time_self).
find_threshold_predicate(redos,  self_and_desc, no,  threshold_ps_time_self).
find_threshold_predicate(time,   self,          yes, threshold_ps_time_self).
find_threshold_predicate(time,   self_and_desc, yes, threshold_ps_time_both).
find_threshold_predicate(allocs, self,          yes, threshold_ps_allocs_self).
find_threshold_predicate(allocs, self_and_desc, yes, threshold_ps_allocs_both).
find_threshold_predicate(words,  self,          yes, threshold_ps_words_self).
find_threshold_predicate(words,  self_and_desc, yes, threshold_ps_words_both).

%-----------------------------------------------------------------------------%

:- func compare_ps_calls_self_overall(deep, int, int) = comparison_result.

compare_ps_calls_self_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    OwnCalls1 = calls(Own1),
    OwnCalls2 = calls(Own2),
    compare(Result, OwnCalls2, OwnCalls1).

:- func compare_ps_redos_self_overall(deep, int, int) = comparison_result.

compare_ps_redos_self_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    OwnRedos1 = redos(Own1),
    OwnRedos2 = redos(Own2),
    compare(Result, OwnRedos2, OwnRedos1).

:- func compare_ps_time_self_overall(deep, int, int) = comparison_result.

compare_ps_time_self_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    OwnQuanta1 = quanta(Own1),
    OwnQuanta2 = quanta(Own2),
    compare(Result, OwnQuanta2, OwnQuanta1).

:- func compare_ps_time_self_percall(deep, int, int) = comparison_result.

compare_ps_time_self_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnQuanta1 = quanta(Own1),
    OwnQuanta2 = quanta(Own2),
    OwnQuantaPerCall1 = float(OwnQuanta1) / float(Calls1),
    OwnQuantaPerCall2 = float(OwnQuanta2) / float(Calls2),
    compare(Result, OwnQuantaPerCall2, OwnQuantaPerCall1).

:- func compare_ps_time_both_overall(deep, int, int) = comparison_result.

compare_ps_time_both_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    OwnQuanta1 = quanta(Own1),
    OwnQuanta2 = quanta(Own2),
    DescQuanta1 = inherit_quanta(Desc1),
    DescQuanta2 = inherit_quanta(Desc2),
    TotalQuanta1 = OwnQuanta1 + DescQuanta1,
    TotalQuanta2 = OwnQuanta2 + DescQuanta2,
    compare(Result, TotalQuanta2, TotalQuanta1).

:- func compare_ps_time_both_percall(deep, int, int) = comparison_result.

compare_ps_time_both_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnQuanta1 = quanta(Own1),
    OwnQuanta2 = quanta(Own2),
    DescQuanta1 = inherit_quanta(Desc1),
    DescQuanta2 = inherit_quanta(Desc2),
    TotalQuanta1 = OwnQuanta1 + DescQuanta1,
    TotalQuanta2 = OwnQuanta2 + DescQuanta2,
    TotalQuantaPerCall1 = float(TotalQuanta1) / float(Calls1),
    TotalQuantaPerCall2 = float(TotalQuanta2) / float(Calls2),
    compare(Result, TotalQuantaPerCall2, TotalQuantaPerCall1).

:- func compare_ps_allocs_self_overall(deep, int, int) = comparison_result.

compare_ps_allocs_self_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    OwnAlloc1 = allocs(Own1),
    OwnAlloc2 = allocs(Own2),
    compare(Result, OwnAlloc2, OwnAlloc1).

:- func compare_ps_allocs_self_percall(deep, int, int) = comparison_result.

compare_ps_allocs_self_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnAlloc1 = allocs(Own1),
    OwnAlloc2 = allocs(Own2),
    OwnAllocPerCall1 = float(OwnAlloc1) / float(Calls1),
    OwnAllocPerCall2 = float(OwnAlloc2) / float(Calls2),
    compare(Result, OwnAllocPerCall2, OwnAllocPerCall1).

:- func compare_ps_allocs_both_overall(deep, int, int) = comparison_result.

compare_ps_allocs_both_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    OwnAlloc1 = allocs(Own1),
    OwnAlloc2 = allocs(Own2),
    DescAlloc1 = inherit_allocs(Desc1),
    DescAlloc2 = inherit_allocs(Desc2),
    TotalAlloc1 = OwnAlloc1 + DescAlloc1,
    TotalAlloc2 = OwnAlloc2 + DescAlloc2,
    compare(Result, TotalAlloc2, TotalAlloc1).

:- func compare_ps_allocs_both_percall(deep, int, int) = comparison_result.

compare_ps_allocs_both_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnAlloc1 = allocs(Own1),
    OwnAlloc2 = allocs(Own2),
    DescAlloc1 = inherit_allocs(Desc1),
    DescAlloc2 = inherit_allocs(Desc2),
    TotalAlloc1 = OwnAlloc1 + DescAlloc1,
    TotalAlloc2 = OwnAlloc2 + DescAlloc2,
    TotalAllocPerCall1 = float(TotalAlloc1) / float(Calls1),
    TotalAllocPerCall2 = float(TotalAlloc2) / float(Calls2),
    compare(Result, TotalAllocPerCall2, TotalAllocPerCall1).

:- func compare_ps_words_self_overall(deep, int, int) = comparison_result.

compare_ps_words_self_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    OwnWords1 = words(Own1),
    OwnWords2 = words(Own2),
    compare(Result, OwnWords2, OwnWords1).

:- func compare_ps_words_self_percall(deep, int, int) = comparison_result.

compare_ps_words_self_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnWords1 = words(Own1),
    OwnWords2 = words(Own2),
    OwnWordsPerCall1 = float(OwnWords1) / float(Calls1),
    OwnWordsPerCall2 = float(OwnWords2) / float(Calls2),
    compare(Result, OwnWordsPerCall2, OwnWordsPerCall1).

:- func compare_ps_words_both_overall(deep, int, int) = comparison_result.

compare_ps_words_both_overall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    OwnWords1 = words(Own1),
    OwnWords2 = words(Own2),
    DescWords1 = inherit_words(Desc1),
    DescWords2 = inherit_words(Desc2),
    TotalWords1 = OwnWords1 + DescWords1,
    TotalWords2 = OwnWords2 + DescWords2,
    compare(Result, TotalWords2, TotalWords1).

:- func compare_ps_words_both_percall(deep, int, int) = comparison_result.

compare_ps_words_both_percall(Deep, PSI1, PSI2) = Result :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSOwn, PSI2, Own2),
    array.lookup(PSDesc, PSI1, Desc1),
    array.lookup(PSDesc, PSI2, Desc2),
    Calls1 = calls(Own1),
    Calls2 = calls(Own2),
    OwnWords1 = words(Own1),
    OwnWords2 = words(Own2),
    DescWords1 = inherit_words(Desc1),
    DescWords2 = inherit_words(Desc2),
    TotalWords1 = OwnWords1 + DescWords1,
    TotalWords2 = OwnWords2 + DescWords2,
    TotalWordsPerCall1 = float(TotalWords1) / float(Calls1),
    TotalWordsPerCall2 = float(TotalWords2) / float(Calls2),
    compare(Result, TotalWordsPerCall2, TotalWordsPerCall1).

%-----------------------------------------------------------------------------%

:- pred filter_ps_calls_self(deep::in, int::in) is semidet.

filter_ps_calls_self(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    OwnCalls1 = calls(Own1),
    OwnCalls1 > 0.

:- pred filter_ps_redos_self(deep::in, int::in) is semidet.

filter_ps_redos_self(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    OwnCalls1 = redos(Own1),
    OwnCalls1 > 0.

:- pred filter_ps_time_self(deep::in, int::in) is semidet.

filter_ps_time_self(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    OwnQuanta1 = quanta(Own1),
    OwnQuanta1 > 0.

:- pred filter_ps_time_both(deep::in, int::in) is semidet.

filter_ps_time_both(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSDesc, PSI1, Desc1),
    OwnQuanta1 = quanta(Own1),
    DescQuanta1 = inherit_quanta(Desc1),
    TotalQuanta1 = OwnQuanta1 + DescQuanta1,
    TotalQuanta1 > 0.

:- pred filter_ps_allocs_self(deep::in, int::in) is semidet.

filter_ps_allocs_self(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    OwnAlloc1 = allocs(Own1),
    OwnAlloc1 > 0.

:- pred filter_ps_allocs_both(deep::in, int::in) is semidet.

filter_ps_allocs_both(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSDesc, PSI1, Desc1),
    OwnAlloc1 = allocs(Own1),
    DescAlloc1 = inherit_allocs(Desc1),
    TotalAlloc1 = OwnAlloc1 + DescAlloc1,
    TotalAlloc1 > 0.

:- pred filter_ps_words_self(deep::in, int::in) is semidet.

filter_ps_words_self(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI1, Own1),
    OwnWords1 = words(Own1),
    OwnWords1 > 0.

:- pred filter_ps_words_both(deep::in, int::in) is semidet.

filter_ps_words_both(Deep, PSI1) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI1, Own1),
    array.lookup(PSDesc, PSI1, Desc1),
    OwnWords1 = words(Own1),
    DescWords1 = inherit_words(Desc1),
    TotalWords1 = OwnWords1 + DescWords1,
    TotalWords1 > 0.

%-----------------------------------------------------------------------------%

:- pred threshold_ps_time_self(deep::in, float::in, int::in) is semidet.

threshold_ps_time_self(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI, Own),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnQuanta = quanta(Own),
    RootOwnQuanta = quanta(RootOwn),
    RootDescQuanta = inherit_quanta(RootDesc),
    RootTotalQuanta = RootOwnQuanta + RootDescQuanta,
    100.0 * float(OwnQuanta) > Threshold * float(RootTotalQuanta).

:- pred threshold_ps_time_both(deep::in, float::in, int::in) is semidet.

threshold_ps_time_both(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI, Own),
    array.lookup(PSDesc, PSI, Desc),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnQuanta = quanta(Own),
    RootOwnQuanta = quanta(RootOwn),
    DescQuanta = inherit_quanta(Desc),
    RootDescQuanta = inherit_quanta(RootDesc),
    TotalQuanta = OwnQuanta + DescQuanta,
    RootTotalQuanta = RootOwnQuanta + RootDescQuanta,
    100.0 * float(TotalQuanta) > Threshold * float(RootTotalQuanta).

:- pred threshold_ps_allocs_self(deep::in, float::in, int::in) is semidet.

threshold_ps_allocs_self(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI, Own),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnAlloc = allocs(Own),
    RootOwnAlloc = allocs(RootOwn),
    RootDescAlloc = inherit_allocs(RootDesc),
    RootTotalAlloc = RootOwnAlloc + RootDescAlloc,
    100.0 * float(OwnAlloc) > Threshold * float(RootTotalAlloc).

:- pred threshold_ps_allocs_both(deep::in, float::in, int::in) is semidet.

threshold_ps_allocs_both(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI, Own),
    array.lookup(PSDesc, PSI, Desc),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnAlloc = allocs(Own),
    RootOwnAlloc = allocs(RootOwn),
    DescAlloc = inherit_allocs(Desc),
    RootDescAlloc = inherit_allocs(RootDesc),
    TotalAlloc = OwnAlloc + DescAlloc,
    RootTotalAlloc = RootOwnAlloc + RootDescAlloc,
    100.0 * float(TotalAlloc) > Threshold * float(RootTotalAlloc).

:- pred threshold_ps_words_self(deep::in, float::in, int::in) is semidet.

threshold_ps_words_self(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    array.lookup(PSOwn, PSI, Own),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnWords = words(Own),
    RootOwnWords = words(RootOwn),
    RootDescWords = inherit_words(RootDesc),
    RootTotalWords = RootOwnWords + RootDescWords,
    100.0 * float(OwnWords) > Threshold * float(RootTotalWords).

:- pred threshold_ps_words_both(deep::in, float::in, int::in) is semidet.

threshold_ps_words_both(Deep, Threshold, PSI) :-
    PSOwn = Deep ^ ps_own,
    PSDesc = Deep ^ ps_desc,
    array.lookup(PSOwn, PSI, Own),
    array.lookup(PSDesc, PSI, Desc),
    RootOwn = root_own_info(Deep),
    RootDesc = root_desc_info(Deep),
    OwnWords = words(Own),
    RootOwnWords = words(RootOwn),
    DescWords = inherit_words(Desc),
    RootDescWords = inherit_words(RootDesc),
    TotalWords = OwnWords + DescWords,
    RootTotalWords = RootOwnWords + RootDescWords,
    100.0 * float(TotalWords) > Threshold * float(RootTotalWords).

%-----------------------------------------------------------------------------%

sort_line_groups(Criteria, Groups) = SortedGroups :-
    (
        Criteria = by_context,
        CompFunc = compare_line_groups_by_context
    ;
        Criteria = by_name,
        CompFunc = compare_line_groups_by_name
    ;
        Criteria = by_cost(Measurement, InclDesc, Scope),
        (
            Measurement = calls,
            % We ignore the setting of InclDesc because calls are not
            % inherited from descendants, and we ignore the setting of Scope
            % because sorting on "calls per call" is not useful.
            CompFunc = compare_line_groups_by_calls
        ;
            Measurement = redos,
            CompFunc = compare_line_groups_by_redos
        ;
            Measurement = time,
            InclDesc = self,
            Scope = overall,
            CompFunc = compare_line_groups_by_time_self_overall
        ;
            Measurement = time,
            InclDesc = self,
            Scope = per_call,
            CompFunc = compare_line_groups_by_time_self_percall
        ;
            Measurement = time,
            InclDesc = self_and_desc,
            Scope = overall,
            CompFunc = compare_line_groups_by_time_total_overall
        ;
            Measurement = time,
            InclDesc = self_and_desc,
            Scope = per_call,
            CompFunc = compare_line_groups_by_time_total_percall
        ;
            Measurement = allocs,
            InclDesc = self,
            Scope = overall,
            CompFunc = compare_line_groups_by_allocs_self_overall
        ;
            Measurement = allocs,
            InclDesc = self,
            Scope = per_call,
            CompFunc = compare_line_groups_by_allocs_self_percall
        ;
            Measurement = allocs,
            InclDesc = self_and_desc,
            Scope = overall,
            CompFunc = compare_line_groups_by_allocs_total_overall
        ;
            Measurement = allocs,
            InclDesc = self_and_desc,
            Scope = per_call,
            CompFunc = compare_line_groups_by_allocs_total_percall
        ;
            Measurement = words,
            InclDesc = self,
            Scope = overall,
            CompFunc = compare_line_groups_by_words_self_overall
        ;
            Measurement = words,
            InclDesc = self,
            Scope = per_call,
            CompFunc = compare_line_groups_by_words_self_percall
        ;
            Measurement = words,
            InclDesc = self_and_desc,
            Scope = overall,
            CompFunc = compare_line_groups_by_words_total_overall
        ;
            Measurement = words,
            InclDesc = self_and_desc,
            Scope = per_call,
            CompFunc = compare_line_groups_by_words_total_percall
        )
    ),
    SortedGroups = list.sort(compare_groups_fallback(CompFunc), Groups).

:- type compare_line_groups_func(FL, LL) ==
    (func(line_group(FL, LL), line_group(FL, LL)) = comparison_result).

:- func compare_groups_fallback(compare_line_groups_func(FL, LL),
    line_group(FL, LL), line_group(FL, LL)) = comparison_result.

compare_groups_fallback(MainFunc, Group1, Group2) = Result :-
    Result0 = MainFunc(Group1, Group2),
    ( Result0 \= (=) ->
        Result = Result0
    ;
        Result1 = compare_line_groups_by_context(Group1, Group2),
        ( Result1 \= (=) ->
            Result = Result1
        ;
            Result = compare_line_groups_by_name(Group1, Group2)
        )
    ).

%-----------------------------------------------------------------------------%

:- func compare_line_groups_by_context(line_group(FL, LL), line_group(FL, LL))
    = comparison_result.

compare_line_groups_by_context(Group1, Group2) = Result :-
    compare(ResultFilenames,
        Group1 ^ group_filename, Group2 ^ group_filename),
    ( ResultFilenames = (=) ->
        compare(Result,
            Group1 ^ group_linenumber, Group2 ^ group_linenumber)
    ;
        Result = ResultFilenames
    ).

:- func compare_line_groups_by_name(line_group(FL, LL), line_group(FL, LL))
    = comparison_result.

compare_line_groups_by_name(Group1, Group2) = Result :-
    compare(Result, Group1 ^ group_name, Group2 ^ group_name).

:- func compare_line_groups_by_calls(line_group(FL, LL), line_group(FL, LL))
    = comparison_result.

compare_line_groups_by_calls(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    compare(Result, Calls2, Calls1).

:- func compare_line_groups_by_redos(line_group(FL, LL), line_group(FL, LL))
    = comparison_result.

compare_line_groups_by_redos(Group1, Group2) = Result :-
    Redos1 = redos(Group1 ^ group_own),
    Redos2 = redos(Group2 ^ group_own),
    compare(Result, Redos2, Redos1).

:- func compare_line_groups_by_time_self_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_time_self_overall(Group1, Group2) = Result :-
    Quanta1 = quanta(Group1 ^ group_own),
    Quanta2 = quanta(Group2 ^ group_own),
    compare(Result, Quanta2, Quanta1).

:- func compare_line_groups_by_time_self_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_time_self_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Quanta1 = quanta(Group1 ^ group_own),
    Quanta2 = quanta(Group2 ^ group_own),
    ( Calls1 = 0 ->
        QuantaPerCall1 = 0.0
    ;
        QuantaPerCall1 = float(Quanta1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        QuantaPerCall2 = 0.0
    ;
        QuantaPerCall2 = float(Quanta2) / float(Calls2)
    ),
    compare(Result, QuantaPerCall2, QuantaPerCall1).

:- func compare_line_groups_by_time_total_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_time_total_overall(Group1, Group2) = Result :-
    Quanta1 = quanta(Group1 ^ group_own) +
        inherit_quanta(Group1 ^ group_desc),
    Quanta2 = quanta(Group2 ^ group_own) +
        inherit_quanta(Group2 ^ group_desc),
    compare(Result, Quanta2, Quanta1).

:- func compare_line_groups_by_time_total_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_time_total_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Quanta1 = quanta(Group1 ^ group_own) +
        inherit_quanta(Group1 ^ group_desc),
    Quanta2 = quanta(Group2 ^ group_own) +
        inherit_quanta(Group2 ^ group_desc),
    ( Calls1 = 0 ->
        QuantaPerCall1 = 0.0
    ;
        QuantaPerCall1 = float(Quanta1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        QuantaPerCall2 = 0.0
    ;
        QuantaPerCall2 = float(Quanta2) / float(Calls2)
    ),
    compare(Result, QuantaPerCall2, QuantaPerCall1).

:- func compare_line_groups_by_allocs_self_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_allocs_self_overall(Group1, Group2) = Result :-
    Alloc1 = allocs(Group1 ^ group_own),
    Alloc2 = allocs(Group2 ^ group_own),
    compare(Result, Alloc2, Alloc1).

:- func compare_line_groups_by_allocs_self_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_allocs_self_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Alloc1 = allocs(Group1 ^ group_own),
    Alloc2 = allocs(Group2 ^ group_own),
    ( Calls1 = 0 ->
        AllocPerCall1 = 0.0
    ;
        AllocPerCall1 = float(Alloc1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        AllocPerCall2 = 0.0
    ;
        AllocPerCall2 = float(Alloc2) / float(Calls2)
    ),
    compare(Result, AllocPerCall2, AllocPerCall1).

:- func compare_line_groups_by_allocs_total_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_allocs_total_overall(Group1, Group2) = Result :-
    Alloc1 = allocs(Group1 ^ group_own) +
        inherit_allocs(Group1 ^ group_desc),
    Alloc2 = allocs(Group2 ^ group_own) +
        inherit_allocs(Group2 ^ group_desc),
    compare(Result, Alloc2, Alloc1).

:- func compare_line_groups_by_allocs_total_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_allocs_total_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Alloc1 = allocs(Group1 ^ group_own) +
        inherit_allocs(Group1 ^ group_desc),
    Alloc2 = allocs(Group2 ^ group_own) +
        inherit_allocs(Group2 ^ group_desc),
    ( Calls1 = 0 ->
        AllocPerCall1 = 0.0
    ;
        AllocPerCall1 = float(Alloc1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        AllocPerCall2 = 0.0
    ;
        AllocPerCall2 = float(Alloc2) / float(Calls2)
    ),
    compare(Result, AllocPerCall2, AllocPerCall1).

:- func compare_line_groups_by_words_self_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_words_self_overall(Group1, Group2) = Result :-
    Words1 = words(Group1 ^ group_own),
    Words2 = words(Group2 ^ group_own),
    compare(Result, Words2, Words1).

:- func compare_line_groups_by_words_self_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_words_self_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Words1 = words(Group1 ^ group_own),
    Words2 = words(Group2 ^ group_own),
    ( Calls1 = 0 ->
        WordsPerCall1 = 0.0
    ;
        WordsPerCall1 = float(Words1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        WordsPerCall2 = 0.0
    ;
        WordsPerCall2 = float(Words2) / float(Calls2)
    ),
    compare(Result, WordsPerCall2, WordsPerCall1).

:- func compare_line_groups_by_words_total_overall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_words_total_overall(Group1, Group2) = Result :-
    Words1 = words(Group1 ^ group_own) +
        inherit_words(Group1 ^ group_desc),
    Words2 = words(Group2 ^ group_own) +
        inherit_words(Group2 ^ group_desc),
    compare(Result, Words2, Words1).

:- func compare_line_groups_by_words_total_percall(line_group(FL, LL),
    line_group(FL, LL)) = comparison_result.

compare_line_groups_by_words_total_percall(Group1, Group2) = Result :-
    Calls1 = calls(Group1 ^ group_own),
    Calls2 = calls(Group2 ^ group_own),
    Words1 = words(Group1 ^ group_own) +
        inherit_words(Group1 ^ group_desc),
    Words2 = words(Group2 ^ group_own) +
        inherit_words(Group2 ^ group_desc),
    ( Calls1 = 0 ->
        WordsPerCall1 = 0.0
    ;
        WordsPerCall1 = float(Words1) / float(Calls1)
    ),
    ( Calls2 = 0 ->
        WordsPerCall2 = 0.0
    ;
        WordsPerCall2 = float(Words2) / float(Calls2)
    ),
    compare(Result, WordsPerCall2, WordsPerCall1).

%-----------------------------------------------------------------------------%

sum_line_group_measurements(LineGroups, Own, Desc) :-
    list.foldl2(accumulate_line_group_measurements, LineGroups,
        zero_own_prof_info, Own, zero_inherit_prof_info, Desc).

:- pred accumulate_line_group_measurements(line_group(FL, LL)::in,
    own_prof_info::in, own_prof_info::out,
    inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_line_group_measurements(LineGroup, Own0, Own, Desc0, Desc) :-
    Own = add_own_to_own(Own0, LineGroup ^ group_own),
    Desc = add_inherit_to_inherit(Desc0, LineGroup ^ group_desc).

%-----------------------------------------------------------------------------%
:- end_module top_procs.
%-----------------------------------------------------------------------------%
