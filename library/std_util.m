%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: std_util.nl.
% Main author: fjh.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module std_util.
:- interface.
:- import_module list.

%-----------------------------------------------------------------------------%

% The universal type.
% Note that the current NU-Prolog implementation of univ_to_type
% is buggy in that it always succeeds, even if the types didn't
% match, so until this gets implemented correctly, don't use
% univ_to_type unless you are sure that the types will definely match.

:- type univ.

:- pred type_to_univ(_T, univ).
:- mode type_to_univ(input, output).
:- mode type_to_univ(output, input).

:- pred univ_to_type(univ, _T).
:- mode univ_to_type(input, output).
:- mode univ_to_type(output, input).

%-----------------------------------------------------------------------------%

% The boolean type.
% Unlike most languages, we use `yes' and `no' as boolean constants
% rather than `true' and `false'.  This is to avoid confusion
% with the predicates `true' and `fail'.

:- type bool ---> yes ; no.

%-----------------------------------------------------------------------------%

% compare/3 is not possible in a strictly parametric polymorphic type
% system such as that of Goedel.

:- type comparison_result ---> (=) ; (<) ; (>).

:- pred compare(comparison_result, T, T).
:- mode compare(output, input, input).

%-----------------------------------------------------------------------------%

:- type pair(T1, T2)	--->	(T1 - T2).

%-----------------------------------------------------------------------------%

:- pred gc_call(pred).

:- pred solutions(pred(T), list(T)).
:- mode solutions(complicated_mode, output).

% The following is a temporary hack until we implement higher-order
% modes.

:- mode complicated_mode :: input.

%-----------------------------------------------------------------------------%

% Declaratively, `report_stats' is the same as `true'.
% It has the side-effect of reporting some memory and time usage statistics
% to stdout.  (Technically, every Mercury implementation must offer
% a mode of invokation which disables this side-effect.)

:- pred report_stats.

%-----------------------------------------------------------------------------%

:- implementation.

/*
:- external("NU-Prolog", gc_call/1).
:- external("NU-Prolog", report_stats/0).
:- external("NU-Prolog", solutions/2).
:- external("NU-Prolog", type_to_univ).
*/

univ_to_type(Univ, X) :- type_to_univ(X, Univ).

:- end_module std_util.

%-----------------------------------------------------------------------------%
