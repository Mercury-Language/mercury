%-----------------------------------------------------------------------------%
%
% File: sp_builtin.nl:
% Main author: fjh.
%
% This file is for Sicstus Prolog compatibility.
%
%-----------------------------------------------------------------------------%

:- prolog_flag(redefine_warnings, _, off).

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).

:- op(1199, fx, (export_module)).
:- op(1199, fx, (export_sym)).
:- op(1199, fx, (export_pred)).
:- op(1199, fx, (export_cons)).
:- op(1199, fx, (export_type)).
:- op(1199, fx, (export_adt)).
:- op(1199, fx, (export_op)).

:- op(1199, fx, (import_module)).
:- op(1199, fx, (import_sym)).
:- op(1199, fx, (import_pred)).
:- op(1199, fx, (import_cons)).
:- op(1199, fx, (import_type)).
:- op(1199, fx, (import_adt)).
:- op(1199, fx, (import_op)).

:- op(1199, fx, (use_module)).
:- op(1199, fx, (use_sym)).
:- op(1199, fx, (use_pred)).
:- op(1199, fx, (use_cons)).
:- op(1199, fx, (use_type)).
:- op(1199, fx, (use_adt)).
:- op(1199, fx, (use_op)).

:- op(1199, fx, (rule)).

:- op(1199, fx, (type)).
:- op(1199, fx, (pred)).
:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(975, xfx, ('::')).
:- op(700, xfx, ( \= ) ).
:- op(500, fx, ( \ ) ).

:- op(900, xfx, (when)).
:- op(740, xfy, (or)).
:- op(720, xfy, (and)).

% Prevent warnings about undefined predicates
% when the interpreter tries to execute the new declarations.

rule(_).

type(_).
pred(_).
% mode(_).
inst(_).

% module(_).
end_module(_).
interface.
implementation.

import_module(_).
import_sym(_).
import_pred(_).
import_cons(_).
import_type(_).
import_adt(_).
import_op(_).

export_module(_).
export_sym(_).
export_pred(_).
export_cons(_).
export_type(_).
export_adt(_).
export_op(_).

% use_module(_).
use_sym(_).
use_pred(_).
use_cons(_).
use_type(_).
use_adt(_).
use_op(_).

external(_).

% when(_,_).

%-----------------------------------------------------------------------------%
