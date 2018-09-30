%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/

:- module github_50.submodule.

:- interface.

:- instance intersect_typeclass(rectangle).

:- implementation.

:- instance intersect_typeclass(rectangle) where [
    (intersects(R1, R2) :- rectangles_intersect(R1, R2))
].
