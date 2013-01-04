% house.gml
%
% OUTPUTS: house.ppm
%
% A crude house.  This example exercises most of the mechanisms.
%

#include "surface.ins"
#include "rgb.ins"

% the main body of the house
{ /v /u /face
  firebrick3 1.0 0.1 1.0
} cube
5.0 1.5 2.0 scale

% subtract away a bit to make the flat roof
{ /v /u /face
  face 5 eqi { grey66 } { gray20 } if
  1.0 0.1 1.0
} cube
4.99 0.12 1.99 scale
0.005 1.4 0.005 translate
difference

% subtract away a door
white matte apply cube
0.25 1.0 0.02 scale
2.5 -0.125 addf 0.0 -0.005 translate
difference

white matte apply cylinder
0.25 uscale
-90.0 rotatex
2.5 1.0 0.0 translate
difference

/house

{ /v /u /face
  dark-olive-green 1.0 0.0 1.0
} plane
0.0 -2.0 0.0 translate /grass


house -2.5 -2.0 4.0 translate
grass union
/scene

				% render
0.8 0.8 0.8 point		  % ambient light
[ ]				  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
400 240				  % image wid and height
"house.ppm"			  % output file
render
