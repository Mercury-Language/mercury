% checked-cube.gml
%
% A cube with a blue and white check pattern.
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

[                                 % 3x3 pattern
  [ blue  white blue  ]
  [ white blue  white ]
  [ blue  white blue  ]
] /texture

{ /v /u /face                     % bind parameters
  {                               % toIntCoord : float -> int
    3.0 mulf floor /i               % i = floor(3.0*i)
    i 3 eqi { 2 } { i } if          % return max(2, i)
  } /toIntCoord
  texture u toIntCoord apply get  % color = texture[u][v]
    v toIntCoord apply get
  1.0                             % kd = 1.0
  0.0                             % ks = 0.0
  1.0                             % n = 1.0
} cube

-0.5 -0.5 -0.5 translate	% center cube
1.5 uscale			% make it bigger
-25.0 rotatex 25.0 rotatey	% rotate
0.0 0.0 3.0 translate		% move to final position

/scene


1.0 1.0	1.0 point	% ambient
[]  			% lights
scene			% object
1			% depth
90.0			% fov
320 200			% wid ht
"checked-cube.ppm"	% output file
render
