%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Earlier versions of the compiler did not notice
% that Acc0 is an input parameter of the lambda expression and thus did not
% detect that the disjunction is a switch on Acc0.

:- module lambda_switch.

:- interface.

:- import_module maybe.

:- type agg_func(T, S)
    --->    agg(S, func(S, T) = S).
:- inst agg_mode == bound(agg(ground, func(in, in) = out is det)).
:- mode agg_in == agg_mode >> agg_mode.
:- mode agg_out == free >> agg_mode.

:- func min(func(T) = int) = agg_func(T, maybe(int)).
:- mode min(func(in) = out is det) = agg_out is det.

:- implementation.

:- import_module int.

min(F) = agg(no, M) :-
    M =
        ( func(Acc0, Val0) = Acc :-
            Val = apply(F, Val0),
            (
                Acc0 = no,
                Acc = yes(Val)
            ;
                Acc0 = yes(Acc1),
                ( Acc1 < Val -> Acc = yes(Acc1) ; Acc = yes(Val))
            )
        ).
