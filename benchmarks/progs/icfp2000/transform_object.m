% ---------------------------------------------------------------------------- %
%
% This module applies all the transformations to an
% object to place it in world coordinates.
%
% ---------------------------------------------------------------------------- %

:- module transform_object.

:- interface.

:- import_module eval.


            
    % Compose all the transformations applied to an object
    % into a single transformation matrix (and it's inverse).
    %
:- func push_transformations(object) = object.



% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module trans.
:- import_module list, exception, require.

% ---------------------------------------------------------------------------- %

push_transformations(Object) = push_trans([], Object).
    % [] is correct - identity was wrong.

% ---------------------------------------------------------------------------- %

    % Unfortunately, matrix multiplication isn't commutative and
    % the compose_transformation/N functions are coded so as to
    % multiply with the new transformation on the left.
    %
    % We could do something truly gross: invert the transformations
    % at this level (mainly just negate all the arguments, except
    % for scaling where we'd take reciprocals), do the compositions
    % and then switch the transformation matrix and its inverse
    % around (the inverse transformations, of course, are calculated
    % right to left).

:- func push_trans(list(transformation), object) = object.

push_trans(Ts, basic_object(Id, Obj, NoShadowList)) =
    transform(basic_object(Id, Obj, NoShadowList),
    matrix(compose_transformations(Ts))).

push_trans(Ts, transform(Obj, T)) =
    push_trans([T | Ts], Obj).

push_trans(Ts, union(Object1, Object2)) =
    union(push_trans(Ts, Object1), push_trans(Ts, Object2)).

push_trans(Ts, difference(Object1, Object2)) =
    difference(push_trans(Ts, Object1), push_trans(Ts, Object2)).

push_trans(Ts, intersect(Object1, Object2)) =
    intersect(push_trans(Ts, Object1), push_trans(Ts, Object2)).

% ---------------------------------------------------------------------------- %

:- func compose_transformations(list(transformation)) = trans.

compose_transformations(Transformations) =
    list__foldl(
        compose_transformation,
        Transformations,
        identity
    ).

:- func compose_transformation(transformation, trans) = trans.

compose_transformation(translate(X, Y, Z), T) = compose_translate(X, Y, Z, T).
compose_transformation(scale(X, Y, Z), T)     = compose_scale(X, Y, Z, T).
compose_transformation(uscale(X), T)          = compose_uscale(X, T).
compose_transformation(rotatex(X), T)         = compose_rotatex(X, T).
compose_transformation(rotatey(Y), T)         = compose_rotatey(Y, T).
compose_transformation(rotatez(Z), T)         = compose_rotatez(Z, T).
compose_transformation(matrix(_), _)        = _ :-
	error("compose_transformation: this shouldn't happen??? i think???").

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
