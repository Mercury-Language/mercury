%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% This file should be loaded before any of the others.
% This is done automatically if you use `mnc' and/or `mnp'.
%
%-----------------------------------------------------------------------------%

% Declare the appropriate operators.

:- op(1199, fx, (module)).
:- op(1199, fx, (end_module)).

% :- op(1199, fx, (export_module)).
% :- op(1199, fx, (export_sym)).
% :- op(1199, fx, (export_pred)).
% :- op(1199, fx, (export_cons)).
% :- op(1199, fx, (export_type)).
% :- op(1199, fx, (export_adt)).
% :- op(1199, fx, (export_op)).

:- op(1199, fx, (import_module)).
% :- op(1199, fx, (import_sym)).
% :- op(1199, fx, (import_pred)).
% :- op(1199, fx, (import_cons)).
% :- op(1199, fx, (import_type)).
% :- op(1199, fx, (import_adt)).
% :- op(1199, fx, (import_op)).

% :- op(1199, fx, (use_module)).
% :- op(1199, fx, (use_sym)).
% :- op(1199, fx, (use_pred)).
% :- op(1199, fx, (use_cons)).
% :- op(1199, fx, (use_type)).
% :- op(1199, fx, (use_adt)).
% :- op(1199, fx, (use_op)).

% :- op(1199, fx, (rule)).

:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(1175, xfx, (::)).

:- op(950, fxy, (lambda)).

% Prevent warnings about undefined predicates
% when the interpreter tries to execute the new declarations.
% We replace all Mercury declarations with `:- fail',
% to minimize their size in the object code.

% termExpansion((:- rule(_)), (:- fail)).

termExpansion((:- type(_)), (:- fail)).
termExpansion((:- pred(_)), (:- fail)).
termExpansion((:- mode(_)), (:- fail)).
termExpansion((:- inst(_)), (:- fail)).

termExpansion((:- module(_)), (:- fail)).
termExpansion((:- end_module(_)), (:- fail)).
termExpansion((:- interface), (:- fail)).
termExpansion((:- implementation), (:- fail)).

termExpansion((:- import_module(_)), (:- fail)).
% termExpansion((:- import_sym(_)), (:- fail)).
% termExpansion((:- import_pred(_)), (:- fail)).
% termExpansion((:- import_cons(_)), (:- fail)).
% termExpansion((:- import_type(_)), (:- fail)).
% termExpansion((:- import_adt(_)), (:- fail)).
% termExpansion((:- import_op(_)), (:- fail)).

% termExpansion((:- export_module(_)), (:- fail)).
% termExpansion((:- export_sym(_)), (:- fail)).
% termExpansion((:- export_pred(_)), (:- fail)).
% termExpansion((:- export_cons(_)), (:- fail)).
% termExpansion((:- export_type(_)), (:- fail)).
% termExpansion((:- export_adt(_)), (:- fail)).
% termExpansion((:- export_op(_)), (:- fail)).

% termExpansion((:- use_module(_)), (:- fail)).
% termExpansion((:- use_sym(_)), (:- fail)).
% termExpansion((:- use_pred(_)), (:- fail)).
% termExpansion((:- use_cons(_)), (:- fail)).
% termExpansion((:- use_type(_)), (:- fail)).
% termExpansion((:- use_adt(_)), (:- fail)).
% termExpansion((:- use_op(_)), (:- fail)).

termExpansion((:- external(_)), (:- fail)).

%-----------------------------------------------------------------------------%
