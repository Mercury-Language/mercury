%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/
%
% Released by Transnat Games for testing purposes.
%
% Causes an assert in the Mercury compiler:
% Software Error: hlds.assertion:
% predicate `hlds.assertion.record_preds_used_in'/4:
% Unexpected: invalid pred_id ** Error making `Mercury\cs\test.test.c'.
%
% This appears to be triggered by a typeclass instance in a submodule
% and the promise. It is strange, however, that the promise is NOT about
% the predicate that is used in the typeclass.
%
%---------------------------------------------------------------------------%

:- module github_50.

:- interface.

:- include_module github_50.submodule.

:- type rectangle
    --->    rectangle(x :: float, y :: float, w :: float, h :: float).

:- typeclass intersect_typeclass(T) where [
    pred intersects(T::in, T::in) is semidet
].

:- pred rectangle_intersection(rectangle::in, rectangle::in,
    rectangle::out) is semidet.

:- pred rectangles_intersect(rectangle::in, rectangle::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.

:- promise all [Rectangle] (
    rectangle_intersection(Rectangle, Rectangle, Rectangle)
).

%---------------------------------------------------------------------------%

rectangle_intersection(RectangleA, RectangleB, Rectangle) :-
    RectangleA = rectangle(XA, YA, WA, HA),
    RectangleB = rectangle(XB, YB, WB, HB),

    X = max(XA, XB),
    Y = max(YA, YB),

    Xmax = min(XA + WA, XB + WB),
    Ymax = min(YA + HA, YB + HB),

    W = Xmax - X,
    H = Ymax - Y,

    % Logically required for an intersection to have occurred.
    Xmax >= X,
    Ymax >= Y,

    Rectangle = rectangle(X, Y, W, H).

rectangles_intersect(R1, R2) :-
    rectangle_intersection(R1, R2, _).

%---------------------------------------------------------------------------%
