%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug reported by Volker Wysk
% in an email to m-rev on 2023 aug 4.
%
% The bug was in equiv_type_hlds.m. It was using the code below to compute
% the final merge_inst_info after expanding out equivalence types in the
% two insts within the initial version of that merge_inst_info:
%
%   ( if ChangedA = unchanged, ChangedB = unchanged then
%       Changed = unchanged,
%       MergeInstInfo = merge_inst_info(InstA, InstB)
%   else
%       Changed = changed,
%       MergeInstInfo = MergeInstInfo0
%   ).
%
% The bug is that the the changed and the unchanged cases each have
% the code meant for the other. The unchanged case having the code meant
% for the changed case leads to a tiny slowdown. The changed case having
% the code meant for the unchanged case means that the replacement of
% any equivalence types within this merge_inst_info, which occurs as a key
% in the merge_inst_table, does not get done. Since this replacement *does*
% get done to merge_insts that occur both in values in inst tables and in
% the instmap_deltas attached to goals, the latter become dangling references.
% Any later attempt to look up these dangling reference merge_insts in the
% merge inst table will then cause a compiler abort.
%
%---------------------------------------------------------------------------%

:- module merge_inst_bug.
:- interface.

:- import_module merge_inst_bug_helper_1.

:- import_module bool.
:- import_module io.
:- import_module list.

:- type amend_type == pred(pred(io, io), bool,
    merge_inst_bug_helper_1.state, merge_inst_bug_helper_1.state).
:- inst amend_inst == (pred(out(pred(di, uo) is det), out, di, uo) is det).

:- pred amend_pred(
    list(amend_type)::in(list(amend_inst)),
    list(amend_type)::in(list(amend_inst)),
    pred(io, io)::out(pred(di, uo) is det),
    merge_inst_bug_helper_1.state::di,
    merge_inst_bug_helper_1.state::uo
) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint64.

amend_pred(_, [], (pred(IO::di, IO::uo) is det :- true), !DB).
amend_pred(All, [Amend | Amends], AmendIO, !DB) :-
    Amend(AmendIO0, Done, !DB),
    (
        Done = yes,
        AmendIO = AmendIO0
    ;
        Done = no,
        amend_pred(All, Amends, AmendIO, !DB)
    ).
