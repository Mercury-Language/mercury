% This file should be listed on the command line for nit before any of
% the others.
% This is done automatically by the Makefile when you do 'make foo.nit'.

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).
:- op(1199, fx, (import_module)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_type)).

:- op(1179, xfy, (--->)).
:- op(1199, fx, (rule)).
:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1175, xfx, (::)).

:- type (pred) == void.

% allow nit to ignore the modes in a pred declaration `:- pred p(T::M)'.

:- type (Type :: Mode) == Type.

% nit detects undefined types even if they're not used
% so we need to give type definitions for all the modes
% to prevent warnings about undefined types

:- type i == any.
:- type o == any.
:- type a == any.
:- type ui == any.
:- type uo == any.
:- type di == any.
:- type wi == any.
:- type wo == any.
:- type (_ -> _) == any.
:- type ground == any.
:- type free_unique == any.
:- type ground_unique == any.

