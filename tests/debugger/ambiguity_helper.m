%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambiguity_helper.

:- interface.

:- import_module list.

:- pred zero_int(int::out) is det.
:- pred add_int(int::in, int::in, int::out) is det.
:- func add_int(int, int) = int.

:- pred zero_float(float::out) is det.
:- pred add_float(float::in, float::in, float::out) is det.
:- func add_float(float, float) = float.

:- typeclass summable(T) where [
    (pred zero(T::out) is det),
    (pred sum(T::in, T::in, T::out) is det)
].

:- instance summable(int).
:- instance summable(float).

:- pred get_length_sum(list(T)::in, T::out, int::out) is det
    <= summable(T).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum/3), T = int).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum/3), T = float).

:- pred get_length_sum_via_acc(list(T)::in, T::out, int::out) is det
    <= summable(T).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum_via_acc/3), T = int).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum_via_acc/3), T = float).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.

:- instance summable(int) where [
    pred(zero/1) is zero_int,
    pred(sum/3) is add_int
].

:- instance summable(float) where [
    pred(zero/1) is zero_float,
    pred(sum/3) is add_float
].

%---------------------%

zero_int(0).

add_int(A, B, A + B).

add_int(A, B) = A + B.

%---------------------%

zero_float(0.0).

add_float(A, B, A + B).

add_float(A, B) = A + B.

%---------------------%

get_length_sum([], Sum, 0) :-
    zero(Sum).
get_length_sum([Head | Tail], Sum, Len) :-
    get_length_sum(Tail, TailSum, TailLen),
    sum(Head, TailSum, Sum),
    Len = 1 + TailLen.

get_length_sum_via_acc(List, Sum, Len) :-
    zero(Sum0),
    get_length_sum(List, Sum0, Sum, 0, Len).

:- pred get_length_sum(list(T)::in, T::in, T::out, int::in, int::out) is det
    <= summable(T).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum/5), T = int).
:- pragma type_spec(pred(ambiguity_helper.get_length_sum/5), T = float).

get_length_sum([], !Sum, !Len).
get_length_sum([Head | Tail], !Sum, !Len) :-
    get_length_sum(Tail, !Sum, !Len),
    sum(Head, !.Sum, !:Sum),
    !:Len = 1 + !.Len.
