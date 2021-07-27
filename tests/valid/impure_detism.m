%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This code is a cut down version of merge_key_ranges_2 in rl_key.m.
% It is a regression test; the 21 June 2004 version of the compiler got
% a compiler abort when compiling this code with deep profiling enabled.
%
% The problem had several causes that had all to be present.
%
% 1. The inlined code of less_or_equal contains a call. Deep profiling inserts
%    prepare_for_normal_call before that call, making it impure.
%
% 2. The body of less_or_equal isn't a complete switch in any argument, so
%    switch detection leaves a disjunction in it.
%
% 3. The inlining of less_or_equal in merge_key_ranges_2 *after* the initial
%    semantic checks causes the second run of simplify to modify the code,
%    removing the redundant check of the second argument of less_or_equal in
%    the second clause. This causes simplify to rerun determinism analysis.
%    (If the inlining is done in the source code, the simplification will have
%    been done in the first run of simplify, and simplify won't find any
%    improvements to do after the deep profiling transformation.)
%
% 4. When determinism analysis looks at the inlined disjunction, it disregards
%    the fact that it has no outputs because it is impure.
%
% The original symptom required -O5 to inline the relevant call in rl_key.m.
% We mark the predicate with pragma inline to force the problem, if present,
% to appear also at lower optimization levels.

:- module impure_detism.

:- interface.

:- import_module bool.
:- import_module list.

:- type bounding_tuple
    --->    infinity
    ;       bound(list(int)).

:- pred merge_key_ranges_2(bounding_tuple::in,
    bounding_tuple::in, bool::out) is det.

:- implementation.

:- import_module std_util.

:- type upper_lower
    --->    upper
    ;       lower.

merge_key_ranges_2(Lower1, Upper2, IsNeeded) :-
    ( if less_or_equal(Lower1, upper, Upper2) then
        IsNeeded = no
    else
        IsNeeded = yes
    ).

:- pragma inline(less_or_equal/3).

:- pred less_or_equal(bounding_tuple::in,
    upper_lower::in, bounding_tuple::in) is semidet.

less_or_equal(infinity, _, _).
less_or_equal(_, upper, infinity).
less_or_equal(bound(_), _, bound(_)) :-
    semidet_succeed.
