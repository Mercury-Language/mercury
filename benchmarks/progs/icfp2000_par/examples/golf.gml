1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.8 0.0 point /darkgreen
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow
0.0 0.0 0.0 point /black

{				% color function for post
 clampf 5.0 mulf floor 2 modi
 1 eqi { red } { white } if
} /toStripe

0.0 /cGreenX
2.0 /cGreenZ
2.0 /rGreenX
1.0 /rGreenZ

{				% takes u v and returns true if on the green
 cGreenZ subf rGreenZ divf /v	  % v = (v - cGreenZ) / rGreenZ
 cGreenX subf rGreenX divf /u	  % u = (u - cGreenZ) / rGreenZ
 u u mulf 
 v v mulf
 addf
 1.0
 lessf
} /onGreen

{				% color function for grass
  /v /u
  u v onGreen apply
  {0.0 0.6 0.0 point}
  {u v mulf /x x x 0.0 lessf {-3000.0} {3000.0} if 
   mulf floor 2 modi
   1 eqi {0.0 0.45 0.0 point} {0.0 0.5 0.0 point} if} 
  if
} /toGrass

{ /v /u /face
  v toStripe apply
  1.0
  0.0
  1.0
} cylinder
0.03 2.0 0.03 scale
/post

{ /v /u /face
  u v toGrass apply
  0.95
  0.05
  1.5
} plane
/field

{ /v /u /face
  blue
  1.0
  0.0
  1.0
} plane
-90.0 rotatex
0.0 0.0 1000.0 translate
/sky

{ /v /u /face
  white
  1.0
  0.0
  1.0
} sphere
0.10 uscale
/ball

{ /v /u /face
  black
  1.0
  0.0
  1.0
} cylinder
0.25 uscale
/hole

{ /v /u /face
  red
  1.0
  0.0
  1.0
} cube
-0.5 -0.5 -0.5 translate
1.0 1.0 0.1 scale
/o1

o1
2.0 1.0 1.2 scale
/o2
o1
o2
-30.0 rotatez
0.0 0.66 0.0 translate
o2
30.0 rotatez
0.0 -0.66 0.0 translate
union
difference
0.8 0.5 1.0 scale
/flag

field
hole
0.0 -0.25 2.0 translate
difference
sky
union
post
0.0 0.0 2.0 translate
union
ball 
-0.3 0.1 1.75 translate
union
flag
0.45 1.8 2.0 translate
union
0.0 -1.0 0.0 translate
/scene

1.0 -1.0 1.0 point %% position
0.4 0.4 0.4 point   %% intensity
light
/sun

0.6 0.6 0.6 point %% Ambient
[ sun ]		%% Lights
scene
2		%% Depth
90.0		%% fov
320		%% width (pixels)
200		%% height (pixels)
"golf.ppm"	%% filename
render
