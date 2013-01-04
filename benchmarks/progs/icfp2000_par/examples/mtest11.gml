% mdifference.gml
%
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.75 0.0  point /darkgreen
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

% ... <level>  grey  ==>  <color>
{ clampf /level level level level point } /grey

% ... <color> matte  ==>  ... <surface>
{ /color
  { /v /u /face
    color 1.0 0.0 1.0
  }
} /matte

% ... <color> <kd> <ks> <n>  ==>  ... <surface>
{ /n /ks /kd /color
  { /v /u /face
    color kd ks n
  }
} /const-surface

% a directional light
-1.0 -1.0 1.0 point      % direction of light
1.0 1.0 1.0 point light /light1


% a mirror plane
white 0.1 1.0 1.0 const-surface apply plane /mirror

				% a sphere
{ /v /u /face			  % bind arguments
  cyan				  % surface color
  0.4 0.7 5.0			  % kd ks n
} sphere /s1

{ /v /u /face			  % bind arguments
  0.8 v 0.2 point	  	  % surface color
  1.0 0.0 1.0			  % kd ks n
} sphere /s2

{ /v /u /face                     % bind arguments
  white                           % surface color
    1.0 0.5 1.0                     % kd ks n
} cylinder /cyl

{ /v /u /face
  blue
  1.0 0.5 1.0
  } cube /box

{ /v /u /face
  red
  1.0 0.5 1.0
  } cube /box2

{ /v /u /face
  green
  1.0 0.5 1.0
  } cube /box3



box 0.1 0.1 4.0 scale 0.05 0.05 -2.0 translate /post
box2 0.1 0.1 4.0 scale 0.05 0.05 -2.0 translate /post2
box3 0.1 0.1 4.0 scale 0.05 0.05 -2.0 translate /post3

post 80.0 rotatex 0.0 rotatey 0.0 rotatez 0.0 0.0 1.0 translate
post2 30.0 rotatex 90.0 rotatey 0.0 rotatez 0.0 0.0 0.5 translate union
post2 90.0 rotatex 10.0 rotatey 10.0 rotatez -1.0 0.2 1.0 translate union
post3 30.0 rotatex 10.0 rotatey 0.0 rotatez -2.0 0.0 1.0 translate union
post3 0.0 rotatex 0.0 rotatey 30.0 rotatez -1.0 1.0 1.0 translate union
post -30.0 rotatex 10.0 rotatey 30.0 rotatez 2.0 0.0 1.0 translate union


mirror 0.0 -2.0 0.0 translate union

% rotate and place in scene
%-30.0 rotatex
%30.0 rotatey
0.0 0.0 1.0 translate
/scene

0.0 0.0	0.0 point	% ambient
[light1 ] 		% lights
scene			% object
1			% depth
90.0			% fov
320 240			% wid ht
"mtest11.ppm"		% output file
render

