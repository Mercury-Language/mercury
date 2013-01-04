% holes.gml
%
% OUTPUTS: holes.ppm
%
% The sea of holes (all we need is a yellow submarine)
%

#include "surface.ins"
#include "util.ins"

{ black 1.0 0.0 1.0 } /surf1
{ white 1.0 0.0 1.0 } /surf2

{ /v /u /face
  u frac absf apply 0.5 subf /uu
  v frac absf apply 0.5 subf /vv
  uu uu mulf vv vv mulf addf sqrt 0.3 lessf
  surf1
  surf2
  if
} plane 3.0 uscale -20.0 rotatex 0.0 -5.0 0.0 translate /holes

% render the holes with just ambient lighting
1.0 1.0 1.0 point	% ambient
[]			% lights
holes
1
90.0
320 200
"holes.ppm"
render

% now add a submarine

yellow matte apply sphere /s

s 4.0 1.0 1.3 scale
s 0.8 uscale -1.0 0.5 0.0 translate union /sub

sub 25.0 rotatey 0.0 1.0 5.0 translate
holes union /scene

% directional light
1.0 -1.0 1.0 point
1.0 1.0 1.0 point light /l

0.3 0.3 0.3 point
[l]
scene
1
90.0
320 200
"submarine.ppm"
render

