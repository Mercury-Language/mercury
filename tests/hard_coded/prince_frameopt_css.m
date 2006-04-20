:- module prince_frameopt_css.

:- interface.

:- import_module string, int, float.

:- include_module prince_frameopt_css.style.

:- type length
    --->    absolute(float).

:- type value
    --->    ident(string)
    ;	    percent(number).

:- type number
    --->    int(int)
    ;	    float(float).

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
