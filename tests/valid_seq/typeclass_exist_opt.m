%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% The compiler aborted when making a .opt file if the module had a
% typeclass instance which implements a method with existentially typed
% arguments, but the predicate which implements the method was not itself
% existentially typed.
%
% Uncaught Mercury exception:
% Software Error: pred_table.m: Unexpected: type error in pred call:
%   no matching pred
%
% This test was originally called intermod_typeclass_exist.
%

:- module typeclass_exist_opt.
:- interface.

:- typeclass dynamic_block(T) where [
    some [Q] pred generate_block(T::in, U::in, Q::out) is det
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

%---------------------------------------------------------------------------%

:- type simple_dynamic_block
    --->    simple_dynamic_block.

:- instance dynamic_block(simple_dynamic_block) where [
    pred(generate_block/3) is do_generate_block
].

:- pred do_generate_block(simple_dynamic_block::in, U::in, string::out) is det.

do_generate_block(simple_dynamic_block, _, "qqq").
