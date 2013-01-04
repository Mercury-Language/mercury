% ellipsoid.gml
%
% OUTPUTS: ellipsoid-1.ppm ellipsoid-2.ppm
%

#include "surface.ins"

yellow matte apply sphere
  2.0 1.0 1.0 scale
  0.0 0.0 3.0 translate
white matte apply plane
  0.0 -2.0 0.0 translate
union
  /scene

% directional light
1.0 -1.0 1.0 point
1.0 1.0 1.0 point light /l

% first we render with just ambient lighting
1.0 1.0 1.0 point	% ambient
[]			% lights
scene
1
90.0
320 200
"ellipsoid-1.ppm"
render

% then we render with directional lighting
0.3 0.3 0.3 point	% ambient
[l]			% lights
scene
1
90.0
320 200
"ellipsoid-2.ppm"
render

