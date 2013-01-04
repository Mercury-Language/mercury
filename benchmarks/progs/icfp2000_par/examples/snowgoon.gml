%%%% Adapted from a cartoon by Bill Watterson
%%%% in "Attack of the Deranged Mutant Killer Monster Snow Goons"
%%%%
%%%%

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.8 0.0 point /darkgreen
0.0 0.0 1.0 point /blue
1.0 1.0 1.0 point /white
0.5 0.5 0.5 point /grey
1.0 0.0 1.0 point /magenta
1.0 1.0 0.25 point /yellow
0.0 0.0 0.0 point /black
0.5 0.5 0.1 point /brown
0.3 0.2 0.05 point /darkbrown
1.0 0.5 0.25 point /orange


% addp
{ /v2 /v1
  v1 getx v2 getx addf
  v1 gety v2 gety addf
  v1 getz v2 getz addf point
} /addp

%%%%%%%%%%%%% The Sky
{ /u /v /face
  blue
  1.0
  0.0
  1.0
} plane
-90.0 rotatex
0.0 0.0 1000.0 translate
/sky 

%%%%%%%%%%%%% The Field
{ /u /v /face
  white
  0.95
  0.05
  1.0
} plane
/field


%%%%%%%%%%%% The Cannon

{ /u /v /face
  white
  0.95
  0.05
  1.0
} plane
/field

{ /u /v /face
  darkbrown 1.0 0.0 1.0 } cube
0.3 uscale
-0.15 -0.15 -0.15 translate
45.0 rotatez
/spoke

{ /u /v /face
  grey 1.0 0.0 1.0 } cylinder
0.5 0.2 0.5 scale
-90.0 rotatex
spoke 0.0 0.0 -0.2 translate
union
0.0 0.45 0.0 translate
/wheel

{ /u /v /face
  darkbrown
  1.0
  0.0
  1.0
} cube
/simple_cube

simple_cube 2.0 3.5 2.0 scale
/base1
simple_cube 2.0 2.7 2.0 scale
/base2
simple_cube 1.5 1.0 2.0 scale
/base3

base1
base2 
-2.0 0.0 0.0 translate union
base3
-3.5 0.0 0.0 translate union
wheel 
1.0 0.0 0.0 translate union
wheel
-1.0 0.0 0.0 translate union
wheel
1.0 0.0 2.0 translate union
wheel
-1.0 0.0 2.0 translate union
/base

{/u /v /face
 grey
 1.0
 0.0
 1.0
} cylinder
1.0 6.0 1.0 scale
/cyl1

{/u /v /face
 black
 1.0
 0.0
 1.0
} cylinder
1.0 6.0 1.0 scale
/cyl2

{/u /v /face
 grey
 1.0 
 0.0
 1.0
} sphere
/barrel_end

{/u /v /face
 grey
 1.0
 0.0
 1.0} cylinder
0.3 uscale 
0.5 0.0 0.0 translate
/fuse

cyl1
-90.0 rotatez
barrel_end union
fuse 
0.0 1.0 0.0 translate union
/barrel

cyl2 0.9 1.1 0.9 scale
-90.0 rotatez
/barrel_hole

field
0.0 -6.5 0.0 translate
base
barrel
-2.0 3.5 1.0 translate
union
barrel_hole
-2.0 3.5 1.0 translate
difference
/cannon

%%%%%%%%%%%% Frosty
{ /u /v /face
  white
  1.0
  0.0
  1.0
} sphere
1.5 uscale
/ball1

{ /u /v /face
  white
  1.0
  0.0
  1.0
} sphere
2.0 uscale
/ball2

{ /u /v /face
  yellow
  1.0
  0.0
  1.0
} cylinder
0.05 1.7 0.05 scale
/twig

twig  20.0 rotatez
twig  10.0 rotatez union
twig   0.0 rotatez union
twig -10.0 rotatez union
twig -20.0 rotatez union
/broom_head

{ /u /v /face
  brown
  1.0 
  0.0
  1.0
} cylinder
0.1 5.5 0.1 scale
broom_head
0.0 5.5 0.0 translate
union
/broomstick

{ /u /v /face
  grey
  0.2
  0.8
  1.0
} sphere
0.12 uscale
/button

{ /u /v /face
  white
  1.0 
  0.0
  1.0
} sphere
1.1 1.3 1.0 scale
/head

{ /u /v /face
  red
  1.0 
  0.0
  1.0
} sphere
0.2 uscale
/button_nose

{ /u /v /face
  orange
  1.0
  0.0
  1.0
} cone
0.2 1.0 0.2 scale
90.0 rotatex
0.0 0.0 -1.0 translate
/carrot_nose

{ /u /v /face
  black
  1.0
  0.0
  1.0
} sphere
0.2 uscale
/eye

{ /u /v /face
  darkgreen
  0.2 
  0.8
  1.0
} sphere
0.1 uscale
/tooth

0.4
/mouth_width 
0.2
/mouth_height

%% Tooth placement
%% Given an angle, and a center for the mouth returns a tooth.
{ /theta  %% angle -- float
  /center %% center of mouth -- point 
  theta sin mouth_width mulf  %% x placement 
  theta cos mouth_height mulf %% y placement 
  0.0  %% z placement?
  point
  center
  addp apply
  /offset
  tooth
  offset getx 
  offset gety
  offset getz  translate
} /put_tooth

0.0 0.0 0.0 point
/mouth_center

mouth_center 90.0 put_tooth apply
mouth_center 145.0 put_tooth apply
union
mouth_center 180.0 put_tooth apply
union
mouth_center 215.0 put_tooth apply
union
mouth_center 270.0 put_tooth apply
union
/smile

{ /u /v /face
  white
  1.0
  0.0
  1.0
} sphere
mouth_width mouth_height 0.5 scale 
/scream

head
carrot_nose
0.0 -0.2 -1.0 translate 
union

eye
-0.3 0.1 -0.8 translate
eye
0.3 0.1 -0.8 translate
union

union

%%smile
%%0.0 -0.45 -0.9 translate
%% union
scream
0.0 -0.5 -0.9 translate
difference
-30.0 rotatex %% Tilt the head down.

ball1
0.0 -2.5 0.0 translate
union

button
0.0 -2.0 -1.4 translate
union

button
0.0 -2.5 -1.5 translate
union

ball2
0.0 -5.0 0.0 translate
union

broomstick
20.0 rotatez
-5.0 rotatex
-0.5 -6.0 -0.8 translate
union
/frosty

%%%%%%%%%%%% The hole

{/u /v /face
 white
 1.0
 0.0
 1.0 } cylinder
0.6 10.0 0.6 scale
-90.0 rotatex
0.0 0.0 1.5 translate
/hole


%%%%%%%%%%%% The Cannonball

{/u /v /face
 grey
 1.0
 0.0
 1.0 } sphere
0.4 uscale
/cannonball

%%%%%%%%%%%% The scene
field
0.0 -6.3 0.0 translate
frosty
hole
0.0 -2.5 0.0 translate
difference
cannonball
1.0 -6.0 -2.5 translate
union
50.0 rotatey
union
cannon
90.0 rotatey
0.0 -6.5 10.0 translate
union
-60.0 rotatey
1.0 3.0 7.0 translate 
union
0.0 -1.0 0.0 translate
/scene


1.0 -0.75 1.2 point %% direction
0.6 0.6 0.6 point   %% intensity
light
/sun

0.0 5.0 -5.0 point
0.5 0.5 0.5 point
pointlight
/sun_point

0.5 0.5 0.5 point %% Ambient
[ sun sun_point ]  %% Lights
scene
2   %% Depth
75.0  %% fov
300 %% width (pixels)
600 %% height (pixels)
"snowgoon.ppm" %% filename
render
