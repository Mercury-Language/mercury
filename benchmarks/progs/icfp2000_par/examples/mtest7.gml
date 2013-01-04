% mtest7.gml
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
0.0 0.0 1.5 point      % direction of light
1.0 1.0 1.0 point pointlight /light1

% a position light
-2.0 0.0 1.0 point      % direction of light
1.0 1.0 1.0 point pointlight /light2

% a mirror plane
white 0.1 0.5 1.0 const-surface apply plane /mirror

				% a sphere
{ /v /u /face			  % bind arguments
  blue				  % surface color
  0.4 0.7 8.0			  % kd ks n
} sphere /s1

				% a sphere
{ /v /u /face			  % bind arguments
  red				  % surface color
  0.4 0.7 8.0			  % kd ks n
} sphere /s2
				% a sphere
{ /v /u /face			  % bind arguments
  green				  % surface color
  0.4 0.7 8.0			  % kd ks n
} sphere /s3
				% a sphere
{ /v /u /face			  % bind arguments
  white				  % surface color
  0.4 0.7 8.0			  % kd ks n
} sphere /s4

{ /v /u /face			  % bind arguments
  0.8 v 0.2 point	  	  % surface color
  1.0 0.0 8.0			  % kd ks n
} sphere /s5

blue 0.6 0.5 3.0 const-surface apply cube /box


box 0.0 0.0 2.0 translate
s1 -1.0 0.0 0.0 translate	  % behind and above viewer
union
s2 1.0 0.0 0.0 translate
union
s3 0.0 0.0 1.732 translate
union
s4 0.0 1.732 0.306 translate
union

%place the stack of spheres
0.0  rotatex
0.0 0.0 3.0 translate

% add the mirrors
mirror -90.0 rotatex 45.0 rotatey 0.0 0.0 6.0 translate
union
mirror -90.0 rotatex -45.0 rotatey 0.0 0.0 6.0 translate
union
mirror 0.0 -2.0 0.0 translate 
union

/scene
0.5 0.5	0.5 point	% ambient
[light1 ] 		% lights
scene			% object
4			% depth
90.0			% fov
320 240			% wid ht
"mtest7.ppm"		% output file
render

