%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Stress test the handling of temp frames on the nondet stack, looking for
% bugs that manifest themselves when we add new segments to the nondet stack,
% or when we delete them.
%
% The code works by building a list of positive numbers, and searching it
% for a negative number. With debugging enabled, which we can do even in
% non-debug grades, the search predicate in_list will have its nondet tail
% recursion disabled, and every one of the nested active calls will have
% a temp frame created for its FAIL event. If the list is long enough,
% these temp frames will spill over into new nondet stack segments.

:- module bug314.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    N = 4000,
    iota(N, [], List),
    test(List, Result),
    io.print(Result, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred iota(int::in, list(int)::in, list(int)::out) is det.

iota(N, !List) :-
    ( if N = 0 then
        true
    else
        !:List = [N | !.List],
        iota(N - 1, !List)
    ).

:- pred test(list(int)::in, string::out) is det.

test(List, Result) :-
    ( if
        % We look for a number that cannot be in the list,
        % to force in_list to use the maximum amount of nondet stack.
        in_list(M, List),
        M = -1
    then
        Result = "found"
    else
        Result = "not found"
    ).

% Our own version of list.member(out, in) so it gets compiled how we want it.
:- pred in_list(int::out, list(int)::in) is nondet.

in_list(N, [H | T]) :-
    (
        N = H
    ;
        in_list(N, T)
    ).
