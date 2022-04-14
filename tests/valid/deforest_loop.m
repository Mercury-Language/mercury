%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case triggered an infinite loop in deforestation
% in the compiler of 3/11/1998.

:- module deforest_loop.
:- interface.

:- import_module float.
:- import_module list.

    % Lights are modelled as points.
:- type light
    --->    light(
                float,  % Power in range [0.0, 1.0].
                vec     % Position of light.
            ).

:- type attributes == int.

:- pred shade(scene, ray, ray, attributes, colour).
:- mode shade(in(scene), in, in, in, out) is det.

:- type vec == int.

:- inst scene
    --->    scene(
                    list_skel(object),
                    ground,
                    ground,
                    ground,
                    ground,
                    ground,
                    ground
            ).

:- type scene
    --->    scene(
                    list(object),   % objects
                    list(light),    % light sources
                    float,          % ambient illumination
                    float,          % coefficient of specular reflection
                    float,          % exponent of specular reflection
                    float,          % focal length of pin-hole camera
                    colour          % background colour
            ).

:- type colour ---> rgb(float, float, float).
:- type ray == int.

:- type object == pred(int, int).
:- inst object == (pred(in, out) is nondet).

:- implementation.

:- import_module math.

shade(Scene, Ray, Intersection, Attributes, Colour) :-
    Colour0 = colour(Attributes),
    Ambient = scale(ambient(Scene), Colour0),
    list.map(shade_from_light(Scene, Ray, Intersection, Colour0),
            lights(Scene), Colours),
    list.foldl(add_colours, Colours, Ambient, Colour).

:- pred shade_from_light(scene, ray, ray, colour, light, colour).
:- mode shade_from_light(in(scene), in, in, in, in, out) is det.

:- pragma no_inline(shade_from_light/6).

shade_from_light(_, _, _, _, _, rgb(0.0, 0.0, 0.0)).

:- func colour(attributes) = colour.
:- pragma no_inline(colour/1).

colour(_) = rgb(0.0, 0.0, 0.0).

:- func scale(float, colour) = colour.
scale(F, rgb(R, G, B)) = rgb(range(F * R), range(F * G), range(F * B)).

:- func ambient(scene::in(scene)) = (float::out) is det.
:- pragma no_inline(ambient/1).

ambient(_) = 0.0.

:- func lights(scene::in(scene)) = (list(light)::out) is det.
:- pragma no_inline(lights/1).

lights(_) = [].

:- pred add_colours(colour::in, colour::in, colour::out) is det.
add_colours(C0, C1, C0 + C1).

:- func '+'(colour, colour) = colour.
rgb(Ra, Ga, Ba) + rgb(Rb, Gb, Bb) =
    rgb(range(Ra + Rb), range(Ga + Gb), range(Ba + Bb)).

:- func range(float) = float.
:- pragma no_inline(range/1).

range(X) = X.
