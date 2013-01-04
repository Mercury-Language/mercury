%
% helikopter-ani.gml
%
% 2000/08/30 Andreas Rossberg
%

% To build an animated gif from the resulting PPMs do:
%
%     convert -loop 0 heli[1-8].ppm heli-ani.gif


%%%% Modify these to get different picture sizes or animation stepping %%%%

320 /width
200 /height

[ "heli1.ppm" "heli2.ppm" "heli3.ppm" "heli4.ppm"
  "heli5.ppm" "heli6.ppm" "heli7.ppm" "heli8.ppm"
] /files


%%%% Some basic colors %%%%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
1.0  0.5  0.0  point /orange
0.0  0.5  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  1.0  0.0  point /yellow
0.2  0.0  0.3  point /darkpurple


%%%% The parts of the heli %%%%


%% Common surface

{ /v /u /face orange 0.2 0.8 1.0 } /body-surface


%% main body

body-surface sphere


%% extension

body-surface cylinder
0.3 3.2 0.3 scale
-90.0 rotatez
0.0 0.7 0.0 translate
union


%% cut the cockpit

body-surface cylinder
0.5 2.0 0.5 scale
90.0 rotatex
-0.7 0.5 -1.0 translate
difference


%% put in the window

{ /v /u /face darkpurple 0.1 0.9 1.0 } sphere
0.92 uscale
union


%% the vats

{ /v /u /face green 1.0 0.0 1.0 } cylinder
0.1 2.8 0.1 scale
90.0 rotatez
1.4 -1.0 0.0 translate
/vat

vat				% right vat
0.0 0.0 0.8 translate
union

vat				% left vat
0.0 0.0 -0.8 translate
union

%% ... this makes the basic heli

/baseheli


%%%% The rotor %%%%

%% One blade

{ /v /u /face
  1 face 4 modi lessi		% if 1 < face mod 4  -- left or right
  {white}			% then white
  {
     u 0.1 lessf		% else if u < 0.1
     {white}			% then white
     {
       0.9 u lessf		% else if u > 0.9
       {white}			% then white
       {red}			% else red
       if
     } if
  } if
  1.0 0.0 1.0
} cube
-0.5 -0.5 -0.5 translate
4.8 0.02 0.3 scale
/blade


%% A rotor

blade blade
90.0 rotatey
union
/rotor


%% Make the main rotor, parameterized over movement angle

{ /angle
  rotor
  angle 50.0 addf rotatey
  0.0 1.1 0.0 translate
} /mk-rotor


%% Make the back rotor, parameterized over movement angle

{ /angle
  rotor
  angle rotatey
  0.3 1.0 0.3 scale
  -90.0 rotatex
  2.8 0.65 -0.4 translate
} /mk-backrotor


%%%% Make a complete heli %%%%

{ /angle
  baseheli
  angle mk-rotor apply union
  angle 2.0 mulf mk-backrotor apply union
} /mk-heli


%%%% The scenery %%%%

%% Lights

[
  -0.5 0.0 -1.0 point
  white
  pointlight

%  -4.0 4.5 -1.0 point
  -3.0 3.5 0.0 point
  white
  pointlight
] /lights


%% Make scene

{ /u /file
  90.0 u mulf mk-heli apply	% angle = u*90
  -5.0 rotatex
  -34.0 rotatey
  0.0 0.0 3.0 translate
  /scene

  0.2 0.2 0.2 point		% ambient light
  lights			% light
  scene				% scene
  1				% depth
  90.0				% field of vision
  width height			% width and height
  file				% file name
  render
} /mk-scene


%%%% Do the Animation %%%%

{ /cont /i
  files length /steps
  i steps lessi			% while i < steps
  {
    files i get			% file = files[i]
    i real steps real divf	% u = i/steps
    mk-scene apply
    i 1 addi cont cont apply	% loop
  } {} if
} /loop

0 loop loop apply
