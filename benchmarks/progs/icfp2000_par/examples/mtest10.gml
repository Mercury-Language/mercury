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
1.0 0.0 1.0 point      % direction of light
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

box

box2 0.5 0.5 4.0 scale 0.25 0.25 -2.0 translate difference
box2 0.5 4.0 0.5 scale 0.25 -2.0 0.25 translate difference
box2 4.0 0.5 0.5 scale -2.0 0.25 0.25 translate difference

s1 0.3 uscale 0.0 0.0 0.0 translate difference	  % behind and above viewer
s1 0.3 uscale 1.0 0.0 0.0 translate difference 	  % behind and above viewer
s1 0.3 uscale 0.0 1.0 0.0 translate	 difference  % behind and above viewer
s1 0.3 uscale 0.0 0.0 1.0 translate difference 	  % behind and above viewer
s1 0.3 uscale 0.0 1.0 1.0 translate difference	  % behind and above viewer
s1 0.3 uscale 1.0 1.0 0.0 translate difference	  % behind and above viewer
s1 0.3 uscale 1.0 0.0 1.0 translate difference	  % behind and above viewer
s1 0.3 uscale 0.0 1.0 1.0 translate difference	  % behind and above viewer
%cyl 0.2 1.0 0.2 scale -90.0 rotatex 0.0 0.0 1.5 translate

% rotate and place in scene
-0.5 -0.5 -0.5 translate
-30.0 rotatex
30.0 rotatey
0.0 0.0 1.0 translate
/scene

0.0 0.0	0.0 point	% ambient
[light1 ] 		% lights
scene			% object
2			% depth
90.0			% fov
320 240			% wid ht
"mtest10.ppm"		% output file
render

