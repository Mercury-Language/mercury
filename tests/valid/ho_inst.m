% This test ensures that special preds for higher order func insts
% are generated and modechecked correctly.
%		-- 18 Jun 1997 bromage

:- module ho_inst.

:- interface.
:- import_module int.

:- type foo == (func(int) = int).

:- type baz.

:- implementation.

:- type bar == (func(int) = int).
:- type baz == (func(int) = int).

