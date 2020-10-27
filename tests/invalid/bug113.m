%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ft=mercury
%---------------------------------------------------------------------------%
%
% This program (derived from library/bitmap.m) causes the following abort
% in the rotd-2009-10-11:
%
% Software Error: prog_type.m: Unexpected: type_to_ctor_and_args_det:
%   type_to_ctor_and_args failed
%
% To reproduce: mmc bug113.m
%
%---------------------------------------------------------------------------%

:- module bug113.
:- interface.

:- import_module bool.
:- import_module list.

:- type bitmap ---> bitmap.
:- type foo == bool.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module string.

new(N) = new(N, no).

new(N, B) = BM :-
    ( if N < 0 then
        throw_bitmap_error("bitmap.new: negative size") = _ : int
    else
        X    = initializer(B),
        BM0  = initialize_bitmap(allocate_bitmap(N), N, X),
        BM   = clear_filler_bits(BM0)
    ).

%---------------------------------------------------------------------------%
