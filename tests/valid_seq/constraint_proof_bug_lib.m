%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constraint_proof_bug_lib.

:- interface.

:- type date.
:- type code.

:- type field(T1, T2)
    --->    d(T1)
    ;       c(T2).

:- type dep_op == string.

:- typeclass constrainable(T) where [
    pred apply_op(T::in, dep_op::in, T::in) is semidet
].

:- instance constrainable(date).
:- instance constrainable(code).
:- instance constrainable(field(T, T2))
    <= (constrainable(T), constrainable(T2)).

:- pred get_date_date(int::out, int::out, int::out, date::in) is det.

:- implementation.

:- type code == int.
:- type date
    --->    d(int).

get_date_date(Y, M, D, _Date) :-
    Y=1999, M=6, D=25.

:- instance constrainable(date) where [
    pred(apply_op/3) is apply_op_dates
].

:- pred apply_op_dates(date::in, dep_op::in, date::in) is semidet.

apply_op_dates(D1, "=", D2) :-
    get_date_date(Y1, M1, Day1, D1),
    get_date_date(Y1, M1, Day1, D2).

:- instance constrainable(code) where [
    pred(apply_op/3) is apply_op_codes
].

:- pred apply_op_codes(code::in, dep_op::in, code::in) is semidet.

apply_op_codes(D1, "=", D2) :-
    compare((=), D1, D2).

:- instance constrainable(field(T, T2)) <=
    (constrainable(T), constrainable(T2))
where [
    pred(apply_op/3) is apply_op_fields
].

:- pred apply_op_fields(field(T, T2)::in, dep_op::in, field(T, T2)::in)
    is semidet <= (constrainable(T), constrainable(T2)).

apply_op_fields(d(D1), Op, d(D2)) :-
    apply_op(D1, Op, D2).
apply_op_fields(c(D1), Op, c(D2)) :-
    apply_op(D1, Op, D2).
