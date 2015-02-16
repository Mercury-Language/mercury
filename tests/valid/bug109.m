%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is a regression test for Mantis bug #109. Its code was derived
% from the function bits/3 in library/bitmap.m.
%
% In high-level C nogc grades, e.g. hlc, hl, hlc_nest, or hl_nest, this
% program used to cause the compiler to abort with:
%
%     Software Error: ml_disj_gen.m: Unexpected: ml_gen_disj: single disjunct
%
% The problem was a bug in add_heap_ops.m, fixed 14 Sep 2010.
%
%---------------------------------------------------------------------------%

:- module bug109.
:- interface.

:- func bits(int) = int.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

bits(NumBits) = Result :-
   ( if
        (
            NumBits < 0
        ;
            X = int.bits_per_int,
            NumBits > X
        )
    then
        Result = 10
    else
        Result = 42
    ).

%---------------------------------------------------------------------------%
