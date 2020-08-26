%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2003, 2005-2006, 2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: prolog.m.
% Main author: fjh.
% Stability: high.
%
% This file contains predicates that are intended to help people
% porting Prolog programs, or writing programs in the intersection
% of Mercury and Prolog.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module prolog.
:- interface.

:- import_module list.
:- import_module pair.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% Prolog arithmetic operators.
%

:- pred T =:= T.            % In Mercury, just use =
:- mode in =:= in is semidet.

:- pred T =\= T.            % In Mercury, just use \=
:- mode in =\= in is semidet.

:- pred is(T, T) is det.    % In Mercury, just use =
:- mode is(uo, di) is det.
:- mode is(out, in) is det.

%---------------------------------------------------------------------------%
%
% Prolog term comparison operators.
%

:- pred T == T.             % In Mercury, just use =
:- mode in == in is semidet.

:- pred T \== T.            % In Mercury, just use \=
:- mode in \== in is semidet.

% Prolog's so-called "univ" operator, `=..'.
% Note: this is not related to Mercury's "univ" type!
% In Mercury, use `deconstruct.deconstruct' instead.

:- pred T =.. univ_result.
:- mode in =.. out is det.

    % Note that the Mercury =.. is a bit different to the Prolog one.
    % We could make it slightly more similar by overloading '.'/2,
    % but that would cause ambiguities that might prevent type
    % inference in a lot of cases.
    %
% :- type univ_result ---> '.'(string, list(univ)).
:- type univ_result == pair(string, list(univ)).

    % arg/3.
    % In Mercury, use arg/4 (defined in module deconstruct) instead:
    %
    %   arg(ArgNum, Term, Data) :-
    %       deconstruct.arg(Term, canonicalize, ArgNum - 1, Data).
    %
:- pred arg(int::in, T::in, univ::out) is semidet.

    % det_arg/3: like arg/3, but calls error/1 rather than failing
    % if the index is out of range.
    %
:- pred det_arg(int::in, T::in, univ::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

'=:='(X, X).

'=\\='(X, Y) :-
    X \= Y.

is(X, X).

'=='(X, X).
'\\=='(X, Y) :-
    X \= Y.

'=..'(Term, Functor - Args) :-
    deconstruct(Term, canonicalize, Functor, _Arity, Args).

% We use module qualifiers here to avoid overriding
% the builtin Prolog versions.
prolog.arg(ArgumentIndex, Type, Univ) :-
    deconstruct.arg(Type, canonicalize, ArgumentIndex - 1, Arg),
    type_to_univ(Arg, Univ).

det_arg(ArgumentIndex, Type, Argument) :-
    ( if arg(ArgumentIndex, Type, Arg) then
        Argument = Arg
    else
        error("det_arg: arg failed")
    ).

%---------------------------------------------------------------------------%
