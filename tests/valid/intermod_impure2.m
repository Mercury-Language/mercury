% See intermod_impure.m.
:- module intermod_impure2.

:- interface.

:- impure pred intermod_impure(int::out) is det.

:- implementation.

intermod_impure(Int) :-
	impure intermod_impure_2(Int).

:- impure pred intermod_impure_2(int::out) is det.

:- pragma c_header_code(
"
#include <stdio.h>
").

:- pragma c_code(intermod_impure_2(Int::out), will_not_call_mercury,
"
printf(""Output from impure predicate\\n"");
Int = 2;
").
