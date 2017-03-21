%
% Test the require tail recursion pragma with the
% --warn-non-tail-recursion option.  These tests do not raise an error,
% the tests that do raise errors are in invalid/
%

:- module require_tailrec_3.

:- interface.

:- import_module bool.
:- import_module int.
:- import_module list.

:- pred foldl1(pred(X, A, A), list(X), A, A).
:- mode foldl1(pred(in, in, out) is det, in, in, out) is det.

:- func even1(int) = bool.
:- func odd1(int) = bool.

:- func even2(int) = bool.
:- func odd2(int) = bool.

:- func even3(int) = bool.
:- func odd3(int) = bool.

:- func even4(int) = bool.
:- func odd4(int) = bool.

%-----------------------------------------------------------------------%

:- implementation.

% self tail recursive code with no pragma.
foldl1(_, [], !Acc).
foldl1(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    foldl1(P, Xs, !Acc).

% mutual tail recursion without pragma.
even1(N) =
    ( if N = 0 then
        yes
    else
        odd1(N - 1)
    ).

odd1(N) =
    ( if N = 0 then
        no
    else
        even1(N)
    ).

% mutual tail recursion with none pragma.
:- pragma require_tail_recursion(even2/1, [none]).
even2(N) =
    ( if N = 0 then
        yes
    else
        odd2(N - 1)
    ).

:- pragma require_tail_recursion(odd2/1, [self_recursion_only]).
odd2(N) =
    ( if N = 0 then
        no
    else
        even2(N)
    ).

% mutual tail recursion with mutual pragma.
:- pragma require_tail_recursion(even3/1, [self_or_mutual_recursion]).
even3(N) =
    ( if N = 0 then
        yes
    else
        odd3(N - 1)
    ).

% mutual non-tail recursion with none pragma.
:- pragma require_tail_recursion(odd3/1, [none]).
odd3(N) =
    ( if N = 0 then
        no
    else
        bool.not(even3(N))
    ).

% mutual non-tail recursion with self pragma
even4(N) =
    ( if N = 0 then
        yes
    else
        odd4(N - 1)
    ).

:- pragma require_tail_recursion(odd4/1, [self_recursion_only]).
odd4(N) =
    ( if N = 0 then
        no
    else
        bool.not(even4(N))
    ).

