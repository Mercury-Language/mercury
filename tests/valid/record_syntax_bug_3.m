% The compiler of 4/1/2001 reported a mode error for this test
% case because it did not allow reordering of goals into the middle
% of a field update expression.
:- module record_syntax_bug_3.

:- interface.

:- type t1.

:- func record_bug(t1) = t1.

:- implementation.

:- import_module int.

:- type t1
        ---> t1(t1_f1 :: int, t1_f2 :: t2).

:- type t2
        ---> t2(t2_f1 :: int).

record_bug(T0) = T :-
        T = T0 ^ field1(X) ^ field2(Y) := 3,
        Y = X + 1.

:- func field1(int::out, t1::in) = (t2::out) is det.

field1(T ^ t1_f1, T) = T ^ t1_f2.

:- func 'field1 :='(int, t1, t2) = t1. 

'field1 :='(_, T, T2) = T ^ t1_f2 := T2.

:- func field2(int, t2) = int.

field2(Int, T2) = T2 ^ t2_f1 + Int.

:- func 'field2 :='(int, t2, int) = t2.

'field2 :='(Int, T2, Value) = T2 ^ t2_f1 := Value - Int.

