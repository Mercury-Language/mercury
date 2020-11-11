%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%
% Tests of `pragma require_tail_recursion' with
% `--warn-non-tail-recursion self-and-mutual'.
%
% The .exp file is for non-deep-profiling LLDS grades.
% The .exp3 file is for deep profiling LLDS grades.
% The .exp2 file is for MLDS grades.
%
%---------------------------------------------------------------------------%

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

% Suppress inlining of calls to evenN into oddN and vice versa at higher
% optimisation levels, as that would affect the warning messages produced.
:- pragma no_inline(even1/1).
:- pragma no_inline(odd1/1).

:- pragma no_inline(even2/1).
:- pragma no_inline(odd2/1).

:- pragma no_inline(even3/1).
:- pragma no_inline(odd3/1).
