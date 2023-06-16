%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module prince_frameopt_helper_1.

:- interface.

:- import_module int.
:- import_module float.
:- import_module string.

:- include_module prince_frameopt_helper_1.prince_frameopt_helper_2.

:- type length
    --->    absolute(float).

:- type value
    --->    ident(string)
    ;       percent(number).

:- type number
    --->    int(int)
    ;       float(float).

:- func get_length(value) = length is det.
:- func get_percent(value) = float is semidet.

:- implementation.

get_length(V) = L :-
    ( if V = ident("zero") then
        L = absolute(0.0)
    else
        L = absolute(1.0)
    ).

get_percent(percent(N0)) = N :-
    (
        N0 = int(N1),
        N = float(N1)
    ;
        N0 = float(N)
    ).
