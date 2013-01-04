% cone.gml
%
% OUTPUTS: cone0.ppm cone1.ppm cone2.ppm cone3.ppm
%
% test cone geometry and basic texturing
%

#include "colors.ins"

{ /v /u /face
  face 0 eqi
  { 0.3 0.3 u point }
  { red }
  if
  1.0 0.0 1.0
} cone
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
box "cone0.ppm" doit apply

% render bottom view
box 90.0 rotatex "cone1.ppm" doit apply

% render top view
box -90.0 rotatex "cone2.ppm" doit apply

% render back view
box 180.0 rotatey "cone3.ppm" doit apply

