%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2005-2006 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2018, 2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: enum.m.
% Author: stayl.
% Stability: medium
%
% This module provides the typeclass enum, which describes types
% which can be converted to and from integers without loss of information.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module enum.
:- interface.

    % A type T can be declared to be a member of the enum typeclass if
    %
    % - all values X of type T can be converted to an int N using
    %   the instance's to_int member function, with each distinct X
    %   being translated by to_int to a distinct N; and
    %
    % - for all values N of type int that are equal to to_int(X) for some X,
    %   from_int(N) = X.
    %
    % - for all values N of type int that are not equal to to_int(X) for any X,
    %   from_int(N) should fail.
    %
    % In mathematical notation, the following must hold:
    %
    %   all [X] (X = from_int(to_int(X)))
    %   all [X, Y] (to_int(X) = to_int(Y)) => X = Y)
    %   all [N] (some [X] N = to_int(X) => from_int(N) = X)
    %   all [N] (not (some [X] N = to_int(X))) => from_int(N) fails
    %
:- typeclass enum(T) where [
    func to_int(T) = int,
    func from_int(int) = T is semidet
].

    % This is another version of the above typeclass, which maps
    % values of type T to *unsigned* integers.
    %
    % The other difference is that the from_uint method is a semidet
    % *predicate*, not a semidet *function*. This is because programmers
    % are more likely to expect predicates to be able to fail than functions.
    %
:- typeclass uenum(T) where [
    func to_uint(T) = uint,
    pred from_uint(uint::in, T::out) is semidet
].

    % det_from_int(I) returns the result of from_int(I), but throws an
    % exception if from_int fails.
    %
:- func det_from_int(int) = T <= enum(T).
:- func det_from_uint(uint) = T <= uenum(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

det_from_int(I) = X :-
    ( if X0 = from_int(I) then
        X = X0
    else
        unexpected($pred, "from_int failed")
    ).

det_from_uint(U) = X :-
    ( if from_uint(U, X0) then
        X = X0
    else
        unexpected($pred, "from_uint failed")
    ).

%---------------------------------------------------------------------------%
:- end_module enum.
%---------------------------------------------------------------------------%
