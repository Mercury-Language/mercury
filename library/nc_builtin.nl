%---------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
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

:- op(1199, fx, (include_module)).

:- op(1199, fx, (import_module)).
% :- op(1199, fx, (import_sym)).
% :- op(1199, fx, (import_pred)).
% :- op(1199, fx, (import_cons)).
% :- op(1199, fx, (import_type)).
% :- op(1199, fx, (import_adt)).
% :- op(1199, fx, (import_op)).

:- op(1199, fx, (use_module)).
% :- op(1199, fx, (use_sym)).
% :- op(1199, fx, (use_pred)).
% :- op(1199, fx, (use_cons)).
% :- op(1199, fx, (use_type)).
% :- op(1199, fx, (use_adt)).
% :- op(1199, fx, (use_op)).

% :- op(1199, fx, (rule)).

:- op(1199, fx, (pragma)).
:- op(1199, fx, (mode)).
:- op(1199, fx, (inst)).
:- op(1179, xfy, (--->)).
:- op(1175, xfx, (::)).

:- op(950, fxy, (lambda)).

:- $setOpField((pred), []).			% remove `pred' as an operator
:- op(800, fx, (pred)).				% and then replace it with
						% lower precedence.
:- op(800, fx, (func)).
:- op(800, fy, (impure)).
:- op(800, fy, (semipure)).

:- op(400, yfx, (rem)).
:- op(400, yfx, (div)).

% In NU-Prolog, ':' has precedence 1175, whereas according to the
% ISO Prolog modules standard it should have precedence 600.
% Also in Mercury it is yfx whereas in ISO Prolog it is xfy.
% Hence we need to override the original precedence for it.
% Because ':' is both unary prefix and binary in NU-Prolog,
% we can't simply set the precedence, since that would cause a
% clash between the precedences of the two different forms of the
% operator, which NU-Prolog does not allow.  Hence we must first
% undefine the existing operator declaration for it.
% As a rather nasty hack, we use `$setOpField((:), [])' rather than
% `op(0, xfy, (:)' because the latter results in lots of spurious
% warning messages.

:- $setOpField((:), []).
:- op(600, yfx, (:)).

% In ISO Prolog, `is' has precedence 700, but
% in Mercury we want `is' to have a higher precedence, so that
% `:- func foo(int) = int is det.' doesn't need parentheses.

:- $setOpField((is), []).
:- op(701, xfx, (is)).

% We don't want `in' to be an operator, since this causes syntax
% errors in mode declarations for operators such as int:'-'/2.
:- $setOpField((in), []).

% Prevent warnings about undefined predicates
% when the interpreter tries to execute the new declarations.
% We replace all Mercury declarations with `:- fail',
% to minimize their size in the object code.

% termExpansion((:- rule(_)), (:- fail)).

termExpansion((:- type(_)), (:- fail)).
termExpansion((:- func(_)), (:- fail)).
termExpansion((:- pred(_)), (:- fail)).
termExpansion((:- mode(_)), (:- fail)).
termExpansion((:- inst(_)), (:- fail)).

termExpansion((:- impure(_)), (:- fail)).
termExpansion((:- semipure(_)), (:- fail)).

termExpansion((:- module(_)), (:- fail)).
termExpansion((:- end_module(_)), (:- fail)).
termExpansion((:- interface), (:- fail)).
termExpansion((:- implementation), (:- fail)).

termExpansion((:- include_module(_)), (:- fail)).

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

termExpansion((:- use_module(_)), (:- fail)).
% termExpansion((:- use_sym(_)), (:- fail)).
% termExpansion((:- use_pred(_)), (:- fail)).
% termExpansion((:- use_cons(_)), (:- fail)).
% termExpansion((:- use_type(_)), (:- fail)).
% termExpansion((:- use_adt(_)), (:- fail)).
% termExpansion((:- use_op(_)), (:- fail)).

termExpansion((:- external(_)), (:- fail)).
termExpansion((:- pragma(_)), (:- fail)).
termExpansion((:- pragma(_, _)), (:- fail)).
termExpansion((:- pragma(_, _, _)), (:- fail)).

% Ignore clauses for functions. 
% (What else can we do?  NU-Prolog doesn't support them, and it
% doesn't even have any hooks for extending is/2.)

termExpansion((_ = _), (:- fail)).
termExpansion(((_ = _) :- _), (:- fail)).

%-----------------------------------------------------------------------------%
