% This file should be prepended to any of the .nl files before
% compilation with nc.
% This is done automatically by the Makefile.
% We should perhaps use :- ensure_loaded or :- include to achieve this,
% but the former declaration is broken in NU-Prolog, and the second
% isn't even implemented.

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).
:- op(1199, fx, (import_module)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_type)).

?- op(1179, xfy, (--->)).
?- op(1199, fx, (rule)).
?- op(1199, fx, (mode)).
?- op(1199, fx, (inst)).
?- op(1175, xfx, (::)).

% Prevent warnings about undefined predicate mode/1
% when the compiler tries to execute the mode declarations.

?- assert(mode(_)).

