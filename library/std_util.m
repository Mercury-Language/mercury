%-----------------------------------------------------------------------------%

% File: std_util.nl.
% Main author: fjh.

% This file is intended for all the useful standard utilities
% that don't belong elsewhere, like <stdlib.h> in C.

%-----------------------------------------------------------------------------%

:- module std_util.
:- interface.

% Unlike most languages, we use `yes' and `no' as boolean constants
% rather than `true' and `false'.  This is to avoid confusion
% with the predicates `true' and `fail'.

:- type bool ---> yes ; no.

% compare/3 is not possible in a strictly parametric polymorphic type
% system such as that of Goedel.

:- type comparison_result ---> (=) ; (<) ; (>).

:- pred compare(comparison_result, T, T).
:- mode compare(output, input, input).

:- type pair(T1, T2)	--->	(T1 - T2).

:- implementation.

:- end_module std_util.

%-----------------------------------------------------------------------------%
