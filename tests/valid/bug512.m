%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantus bug #512. The command
%
% mmc -C -s asm_fast.gc -O5 --intermod-opt bug512.m
%
% used to generate this output:
%
% Uncaught Mercury exception:
% Software Error: predicate `ll_backend.liveness.require_equal'/4:
%   Unexpected: branches of if-then-else disagree on liveness
% First:
% Rest:  Cs_8
%
% The causal chain leading to the abort was quite complex. Here are
% its main steps.
%
% - The call char.to_upper is inlined. Its body is an if-then-else,
%   with the else case copying the input arg to the output arg.
%   In this case, that is C = C0.
%
% - Deforestation pushes the test C \= C0 into both arms of the if-then-else.
%   This leaves the else case consisting of C = C0, not ( C = C0 ).
%
% - The follow-code optimization pushes Cs = [C, ':'] into both arms
%   of the if-then-else. This puts Cs (variable number 8) into the nonlocals
%   set of both arms.
%
% - The final pre-codegen simplify, the first invocation of simplification
%   after deforestation, notices that after C = C0, the goal "C = C0" inside
%   the negation can be replaced by "true", that therefore "not C = C0"
%   can be replaced by "fail", and thus the whole else arm of the if-then-else
%   can be replaced by "fail", whose instmap_delta is unreachable.
%
% So far, so good. But then ...
%
% - Due to the change in determinism, simplify_goal_plain_conj wraps
%   a commit(dont_force_pruning) scope around the "fail" goal.
%
% - The replacement process does *not* set the flag that requests
%   a requantification of the procedure body, followed by the recomputation
%   of its instmap deltas. In the absence of the flag, simplification does
%   neither.
%
% - Without the recomputation of instmap_deltas, the instmap_delta of the
%   commit scope has the instmap_delta of the original else arm goal,
%   which says that the else arm computes Cs_8, and its end is reachable.
%
% - The code of liveness.m starts by invoking quantification, which removes
%   Cs_8 from both the nonlocals set and the instmap_delta of the else arm,
%   but, crucially, leaves the end of the else arm reachable. If the ends
%   of both arms are reachable, then the two arms should define the exact
%   same set of variables, but in this case, they don't. This is therefore
%   effectively a compiler-induced mode error. Compilation is not supposed
%   to go beyond semantic analysis if the program contains any mode errors,
%   so later passes rely on their absence.
%
% - The mode error caused by the incorrect reachability screws up the
%   calculations of the second (deadness) pass of liveness.m.
%
% - That screwup then causes the fourth (resume_points) pass in livenes.m
%   to throw the above exception.
%
% Note that the data in the abort message is backwards. Even though the
% then-arm computes Cs_8 and the else-arm does not, the incorrect post-death
% and post-birth sets that the deadness pass attaches to the then- and
% else-arms respectively makes the resume_point pass believe the exact
% opposite.
%

:- module bug512.
:- interface.

:- import_module string.

:- pred normalise_drive_colon(string::in, string::out) is semidet.

:- implementation.

:- import_module char.
:- import_module list.

normalise_drive_colon(S0, S) :-
    string.to_char_list(S0, Cs0),
    Cs0 = [C0, ':'],
    char.to_upper(C0, C),
    C \= C0,
    Cs = [C, ':'],
    string.from_char_list(Cs, S).
