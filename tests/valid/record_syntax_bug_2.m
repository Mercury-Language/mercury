% The compiler of 8/5/2000 aborted on this test case because it
% didn't properly handle overloading of field access functions
% and constructors.
:- module record_syntax_bug_2.
:- interface.

:- type foo ---> some [T] debug(T) where equality is all_equal.

:- type bar ---> bar( debug :: foo ).

:- pred baz(foo, bar).
:- mode baz(in, out) is cc_multi.

:- pred all_equal(foo, foo).
:- mode all_equal(in, in) is semidet.

:- implementation.
:- import_module std_util.

all_equal(_, _) :- semidet_succeed.

baz(debug(X), X).

