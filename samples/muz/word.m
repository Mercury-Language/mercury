%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: word.m
% main author: philip

:- module word.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.

:- type zcontext == int.

:- type triple(X, Y, Z) ---> triple(X, Y, Z).

% :- type quadruple(W, X, Y, Z) ---> quadruple(W, X, Y, Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flags and Pragmas

:- type status
    --->    ok
    ;       error.

:- type flag
    --->    on
    ;       off.

:- type zpragma
    --->    zpragma(operators, abbreviations, monotonics, loglib_ids).

:- type flags.

:- func defaults = flags.

:- pred set_zpragma(zpragma::in, flags::in, flags::out) is det.

:- pred set_operators(operators::in, flags::in, flags::out) is det.

:- pred add_operators(op::in, list(ident)::in, flags::in, flags::out) is det.

:- pred search_operators(operators::in, ident::in, op::out) is semidet.

:- pred set_abbreviations(abbreviations::in, flags::in, flags::out) is det.

:- pred set_monotonics(monotonics::in, flags::in, flags::out) is det.

:- pred set_loglib_ids(loglib_ids::in, flags::in, flags::out) is det.

:- pred set_debugging_on(flags::in, flags::out) is det.

:- pred set_toolkit(maybe(string)::in, flags::in, flags::out) is det.

:- pred set_generating_logic(flag::in, flags::in, flags::out) is det.

:- func zpragmaInit = zpragma.

:- func zpragma(flags) = zpragma.

:- func operators(flags) = operators.

:- func abbreviations(flags) = abbreviations.

:- func monotonics(flags) = monotonics.

:- func loglib_ids(flags) = loglib_ids.

:- pred debugging(flags::in) is semidet.

:- func toolkit(flags) = maybe(string).

:- func generating_logic(flags) = flag.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Identifiers and Operators

:- type word == string.

:- type operation
    --->    delta
    ;       xi.

:- type decoration == list(stroke).

:- type ident
    --->    id(maybe(operation), word, decoration).

% :- func powerIdent = ident.
:- func numIdent = ident.
:- func stringIdent = ident.

:- type stroke
    --->    exclamation_mark
    ;       question_mark
    ;       prime
    ;       subscript(string).

:- type ref == int.     % To uniquely identify each reference to an identifier.

:- type renaming == assoc_list(ident, ident).

:- func wordPortray(word) = string.

:- func identPortray(ident) = string.

:- func strokeLPortray(list(stroke)) = list(string).

:- type priority == int.        % >= 1
:- type op
    --->    infun(priority)
    ;       postfun
    ;       inrel
    ;       prerel
    ;       ingen
    ;       pregen.

:- type operators == map(ident, op).
:- type abbreviations == list(ident).
:- type monotonics == list(ident).
:- type loglib_ids == list(ident).

:- func op_to_string(op) = string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module pair.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Flags and Pragmas

%    flags(zpragma(ops, abbrevs, monoton), debug, toolkit, generating_logic).
:- type flags
    --->    flags(zpragma, flag, maybe(string), flag).

:- func default_toolkit = string.
default_toolkit = "/usr/local/apps/muz/lib/toolkit.tex".

defaults = flags(zpragmaInit, off, yes(default_toolkit), off).

zpragmaInit = zpragma(O, [], [], []) :-
    map.init(O).

set_zpragma(ZP, flags( _, D, P, G), flags(ZP, D, P, G)).

set_operators(O,
    flags(zpragma(_, A, M, L), D, P, G),
    flags(zpragma(O, A, M, L), D, P, G)).

add_operators(Op, IdentList, F0, F) :-
    list.map(
        (pred(I::in, O::out) is det :-
            O = I-Op
        ), IdentList, AL),
    map.from_assoc_list(AL, M),
    map.overlay(operators(F0), M, Operators),
    set_operators(Operators, F0, F).

search_operators(Operators, Ident, Op) :-
    map.search(Operators, Ident, Op).

set_abbreviations(A,
    flags(zpragma(O, _, M, L), D, P, G),
    flags(zpragma(O, A, M, L), D, P, G)).

set_monotonics(M,
    flags(zpragma(O, A, _, L), D, P, G),
    flags(zpragma(O, A, M, L), D, P, G)).

set_loglib_ids(L,
    flags(zpragma(O, A, M, _), D, P, G),
    flags(zpragma(O, A, M, L), D, P, G)).

set_debugging_on(flags(ZP, _, P, G), flags(ZP, on, P, G)).

set_toolkit(P, flags(ZP, D, _, G), flags(ZP, D, P, G)).

set_generating_logic(Flag, flags(ZP, D, P, _), flags(ZP, D, P, Flag)).

zpragma(flags(ZP, _, _, _)) = ZP.

operators(flags(zpragma(O, _, _, _), _, _, _)) = O.

abbreviations(flags(zpragma(_, A, _, _), _, _, _)) = A.

monotonics(flags(zpragma(_, _, M, _), _, _, _)) = M.

loglib_ids(flags(zpragma(_, _, _, L), _, _, _)) = L.

debugging(flags(_, on, _, _)).

toolkit(flags(_, _, P, _)) = P.

generating_logic(flags(_, _, _, Gen)) = Gen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Identifiers and Operators

% powerIdent = id(no, "\\power", []).
numIdent = id(no, "\\num", []).
stringIdent = id(no, "\\string", []).

wordPortray(S) = S.

identPortray(id(M, W, D)) = S :-
    ( M = no, S0 = ""
    ; M = yes(O), (O = delta, S0 = "\\Delta "; O = xi, S0 = "\\Xi ")
    ),
    SL = strokeLPortray(D),
    string.append_list([S0, wordPortray(W)|SL], S).

strokeLPortray(LI) = LO :- strokeLPortray([], LI, LO).

:- pred strokeLPortray(list(string), list(stroke), list(string)).
:- mode strokeLPortray(in, in, out) is det.

strokeLPortray(L, [], L).
strokeLPortray(L0, [H0|T0], L) :-
    strokeLPortray([strokePortray(H0)|L0], T0, L).

:- func strokePortray(stroke) = string.

strokePortray(exclamation_mark) = "!".
strokePortray(question_mark) = "?".
strokePortray(prime) = "'".
strokePortray(subscript(S0)) = S :-
    string.append("_", S0, S).

op_to_string(infun(P)) = S :-
    string.int_to_string(P, S0),
    string.append_list(["infun(", S0, ")"], S).
op_to_string(postfun) = "postfun".
op_to_string(inrel) = "inrel".
op_to_string(prerel) = "prerel".
op_to_string(ingen) = "ingen".
op_to_string(pregen) = "pregen".
