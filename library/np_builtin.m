% This file should be loaded into np before any of the others.
% (This is done automatically if you just load 'doit.nl'.)
% We should perhaps use ensure_loaded to achieve this, but that
% declaration is broken in NU-Prolog.

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).
:- op(1199, fx, (import_module)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_type)).

:- op(1199, fx, (rule)).
:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(1175, xfx, (::)).

% Prevent warnings about undefined predicates
% when the interpreter tries to execute the new declarations.

:- assert(mode(_)).
:- assert(module(_)).
:- assert(end_module(_)).
:- assert(import_module(_)).
:- assert(export_pred(_)).
:- assert(export_type(_)).
:- assert(interface).
:- assert(implementation).

