%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test is an extract from the garbage_out module.
%
% The code matches the middle-recursion pattern. However, the code of
% the base case is somewhat complex, and some versions of the compiler
% generate code that has more than copy of some labels, if given the
% --no-follow-vars option.

:- module middle_rec_labels.

:- interface.

:- import_module list.
:- import_module maybe.

:- type liveinfo
    --->    live_lvalue(
                lval,
                shape_num,
                maybe(list(lval))
            ).

:- type lval
    --->    stackvar(int)
    ;       framevar(int)
    ;       reg(int).

:- type shape_num == int.

:- type det
    --->    deterministic
    ;       nondeterministic
    ;       commit.

:- pred garbage_out_get_det(list(liveinfo), maybe(det), det).
:- mode garbage_out_get_det(in, in, out) is det.

:- implementation.

garbage_out_get_det([], no, nondeterministic).
garbage_out_get_det([], yes(commit), commit).
garbage_out_get_det([], yes(nondeterministic), nondeterministic).
garbage_out_get_det([], yes(deterministic), deterministic).

garbage_out_get_det([L | Ls], OldD, NewDet) :-
    ( if L = live_lvalue(stackvar(_), _, _) then
        ( if OldD = yes(Detism) then
            ( if Detism = nondeterministic then
                Det = yes(commit)
            else
                Det = OldD
            )
        else
            Det = yes(deterministic)
        )
    else if L = live_lvalue(framevar(_), _, _) then
        ( if OldD = yes(Detism) then
            ( if Detism = deterministic then
                Det = yes(commit)
            else
                Det = OldD
            )
        else
            Det = yes(nondeterministic)
        )
    else
        Det = OldD
    ),
    garbage_out_get_det(Ls, Det, NewDet).
