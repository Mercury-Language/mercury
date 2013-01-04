% sylinder.gml
%
% OUTPUTS: cylinder0.ppm cylinder1.ppm cylinder2.ppm cylinder3.ppm
%
% test cylinder geometry and basic texturing
%

#include "colors.ins"

{ /v /u /face
  face 0 eqi
  { 0.3 0.3 u point }
  { face 1 eqi { red } { green } if }
  if
  1.0 0.0 1.0
} cylinder
  0.0 -0.5 0.0 translate /box

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
box "cylinder0.ppm" doit apply

% render bottom view
box 90.0 rotatex "cylinder1.ppm" doit apply

% render top view
box -90.0 rotatex "cylinder2.ppm" doit apply

% render back view
box 180.0 rotatey "cylinder3.ppm" doit apply

