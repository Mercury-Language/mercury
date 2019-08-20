%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Before 2019 aug 19, the compiler aborted when compiling this in hlc.gc.
% The problem was a map.lookup that tried to look up the variable String's
% entry in the constant variable map. Since String is not a constant,
% the lookup aborted. The MLDS code generator had code to ensure that
% the switch (or disjunction) arms on which it tried to do this consisted
% only of conjunctions of unifications which had only its outputs as its
% nonlocal variables, but this code had a bug, which allowed the attempt
% to generate a lookup table to proceed if, beside its outputs, an arm
% also had the switched-on variable as a nonlocal. The second arm of the
% switch on String below uses String in exactly such a manner, triggering
% the bug.

:- module bug481.

:- interface.

:- type result
    --->    none
    ;       enabled(string).

:- pred parse(string::in, result::out) is semidet.

:- implementation.

parse(String, Res) :-
    (
        String = "none",
        Res = none
    ;
        ( String = "enable"
        ; String = "full"
        ),
        Res = enabled(String)
    ).
