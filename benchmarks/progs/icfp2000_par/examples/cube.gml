% cube.gml
%
% OUTPUTS: cube0.ppm cube1.ppm cube2.ppm cube3.ppm cube4.ppm cube5.ppm
%
% test cube geometry and basic texturing
%

#include "colors.ins"

[ red green blue magenta yellow cyan ] /faces

{ /v /u /face
  faces face get
  1.0 0.0 1.0
} cube
  -0.5 -0.5 -0.5 translate /box

{ /file /box
  1.0 1.0 1.0 point
  []
  box 0.0 0.0 3.0 translate
  1
  90.0
  320 200
  file
  render
} /doit

% render front view
box "cube0.ppm" doit apply

% render bottom view
box 90.0 rotatex "cube1.ppm" doit apply

% render top view
box -90.0 rotatex "cube2.ppm" doit apply

% render right view
box 90.0 rotatey "cube3.ppm" doit apply

% render left view
box -90.0 rotatey "cube4.ppm" doit apply

% render back view
box 180.0 rotatex "cube5.ppm" doit apply

