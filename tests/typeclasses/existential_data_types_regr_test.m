%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The Mercury compiler of 12 Sept 199
% reported a spurious mode error for this test case.

:- module existential_data_types_regr_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- use_module require.

:- typeclass int_singleton(T) where [
   func value(T) = int
].

:- type zero
    --->    zero.

:- instance int_singleton(zero) where [
   func(value/1) is zero_value
].

:- type succ(N)
    --->    succ(N).

:- instance int_singleton(succ(N)) <= int_singleton(N) where [
   func(value/1) is succ_value
].

:- type natural_number
    --->    some [N] (nat(N) => int_singleton(N)).

main(!IO) :-
    nat(N) = to_natural_number(3),
    io.print_line(value(N), !IO).

:- func to_natural_number(int) = natural_number.

to_natural_number(I) = Result :-
    ( if I = 0 then
        Result = 'new nat'(zero)
    else if I > 0 then
        nat(N1) = to_natural_number(I-1),
        Result = 'new nat'(succ(N1))
    else
        require__error("to_natural_number: cannot convert negative integer")
    ).

:- func zero_value(zero) = int.

zero_value(_) = 0.

:- func succ_value(succ(N)) = int <= int_singleton(N).

succ_value(succ(N)) = value(N) + 1.
