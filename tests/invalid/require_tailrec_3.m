% vim: ft=mercury ts=4 sw=4 et
% Require tail recursion pragma tests with --warn-non-tail-recursive
:- module require_tailrec_3.

:- interface.

:- import_module bool.
:- import_module int.

:- func even1(int) = bool.
:- func odd1(int) = bool.

:- func even2(int) = bool.
:- func odd2(int) = bool.

:- func even3(int) = bool.
:- func odd3(int) = bool.

%---------------------------------------------------------------------------%

:- implementation.

% mutual non-tail recursion with no pragma
even1(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd1(N))
    ).
odd1(N) =
    ( if N = 0 then
        no
    else
        even1(N - 1)
    ).

% mutual non-tail recursion with mutual pragma
:- pragma require_tail_recursion(even2/1, [error, self_or_mutual_recursion]).
even2(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd2(N))
    ).
odd2(N) =
    ( if N = 0 then
        no
    else
        even2(N - 1)
    ).

% mutual non-tail recursion with default pragma
:- pragma require_tail_recursion(even3/1).
even3(N) =
    ( if N = 0 then
        yes
    else
        bool.not(odd3(N))
    ).
odd3(N) =
    ( if N = 0 then
        no
    else
        even3(N - 1)
    ).

