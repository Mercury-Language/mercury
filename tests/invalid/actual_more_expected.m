%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test error messages for one actual actual type vs two or more
% expected types.
%
%---------------------------------------------------------------------------%

:- module actual_more_expected.

:- interface.
:- import_module list.

:- type dir
    --->    u
    ;       d
    ;       l
    ;       r.

:- pred p1(list(dir)::in, int::in, int::out) is det.

:- implementation.

:- import_module int.
:- import_module string.

p1(D, S, F) :-
    foldl(turn, D, S, F).

:- pred turn(int::in, dir::in, int::out) is det.

turn(S, l, E) :- ( if S rem 3 = 0 then E = S else E = S-1 ).
turn(S, r, E) :- ( if S rem 3 = 2 then E = S else E = S+1 ).
turn(S, u, E) :- ( if S // 3 = 0 then E = S else E = S-3 ).
turn(S, d, E) :- ( if S // 3 = 2 then E = S else E = S+3 ).
