%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The predicate foo caused the following assertion failure in
% rotd-2006-08-03:
%
% Uncaught Mercury exception:
% Software Error: equiv_type_hlds.m: Unexpected: replace_in_goal_expr:
% info not found
%
% The problem was that when switch detection copied the bodies of the
% arms for Kind = b and Kind = c, it requantified the goals.  The
% requantification introduced some new variables but the entries in the
% RTTI varmaps were not updated at the same time.  The fix was to make
% sure that when quantification might introduce new variables, the RTTI
% varmaps are updated.
%
% The test case was supplied by Peter Hawkins.

:- module hawkins_switch_bug.
:- interface.

:- import_module int.
:- import_module map.
:- import_module set.

:- type kind
    --->    a
    ;       b
    ;       c.

:- pred foo(kind::in, map(int, set(int))::in) is det.

:- implementation.

foo(Kind, B) :-
    (
        Kind = a
    ;
        ( Kind = b
        ; Kind = c
        ),
        ( if map.search(B, 0, PDeps0) then
            _PDeps = PDeps0
        else
            _PDeps = set.init : set(int)
        )
    ).
