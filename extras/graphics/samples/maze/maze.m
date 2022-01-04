%-----------------------------------------------------------------------------%
% file: maze.m
% author: conway, June 1997
%
% This source file is hereby placed in the public domain. -conway (the author).
%
% maze is a 2D maze generator. It uses a 'perfect' algorithm that was posted
% to rec.games.programmer by Kent Quirk. The algorithm is 'perfect' in the
% sense that there is a single path between any two cells in the maze; every
% cell is connected to every other cell.
%
% The options are:
%	-x --width <N>	: the width of the maze
%	-y --height <N> : the heigt of the maze
%	-s --seed <N>   : the random number seed to use
%
% GLUT version by juliensf
% - I've also added a keyboard handler so you can press escape 
%   order to quit.
% 
%-----------------------------------------------------------------------------%

:- module maze.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module glut.
:- import_module glut.callback.
:- import_module glut.window.
:- import_module mglu.
:- import_module mogl.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module pair.
:- import_module random.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type option ---> height ; width ; seed.

:- type wall ---> north ; south ; east ; west.

:- type pos ---> pos(int, int).

:- type adj ---> adj(pos, pos).

:- type maze ==	map(pos, set(pos)).

:- type walls == map(pos, list(wall)).

:- type wander ---> w(set(pos), list(pos)).

%------------------------------------------------------------------------------%

main(!IO) :-
	globals.init(!IO),
		% Process the command line options...
	io.command_line_arguments(Args0, !IO),
	getopt.process_options(option_ops_multi(short, long, defaults),
		Args0, _Args, MOpts),
	(
		MOpts = ok(Opts),
			% Create the maze
		io.write_string("generating maze....", !IO),
		io.flush_output(!IO),
		getopt.lookup_int_option(Opts, width, XMax),
		getopt.lookup_int_option(Opts, height, YMax),
		getopt.lookup_int_option(Opts, seed, Seed),
		globals.set("Size", float(XMax), !IO),
		solutions(
			(pred(X::out) is nondet :- between(0, XMax - 1, X)),
			XIndexs),
		solutions(
			(pred(Y::out) is nondet :- between(0, YMax - 1, Y)),
			YIndexes),
		random.init(Seed, Rnd0),
		maze.dig(pos(XMax, YMax), XIndexs, YIndexes, map.init, Maze, 
			Rnd0, Rnd),
		io.write_string(" done.\n", !IO),
		io.flush_output(!IO),
		globals.set("Rnd", Rnd, !IO),
		maze.main_2(Maze, !IO)

	;
		MOpts = error(Str),
		io.stderr_stream(StdErr, !IO),
		io.format(StdErr, "usage: maze [-xN] [-yN] [-sN]\nerror: %s\n",
			[s(option_error_to_string(Str))], !IO),
		io.set_exit_status(1, !IO)
	).

%-----------------------------------------------------------------------------%


	% Set the display mode and initial window attributes.  Register
	% callbacks and then start the thing running.
:- pred maze.main_2(maze::in, io::di, io::uo) is det.

maze.main_2(Maze, !IO) :-
	glut.init_display_mode([double, rgba], !IO),
	glut.init_window_size(300, 300, !IO),
	glut.window.create("Maze", !IO),
	
	maze.create(Maze, !IO),
	glut.callback.display_func(maze.display, !IO),
	glut.callback.reshape_func(maze.reshape, !IO),
	glut.callback.keyboard_func(maze.keyboard, !IO),
	glut.callback.idle_func(maze.idle, !IO),
	
	globals.set("Maze",  Maze, !IO),
	globals.set("Pos",   pos(0, 0), !IO),
	globals.set("Dir",   east, !IO),
	globals.set("Phi",   0.0,  !IO),
	globals.set("Theta", 0.0,  !IO),
	globals.set("W",     w(set.init, []), !IO),
	
	glut.main_loop(!IO).

%------------------------------------------------------------------------------%
% 
% Solve the maze.
%

	% Work out the next position in the traversal of the maze and then
	% tell OpenGL to redisplay it.
:- pred maze.idle(io::di, io::uo) is det.

maze.idle(!IO) :-
	next_pos(!IO),
	glut.window.post_redisplay(!IO).

:- pred next_pos(io::di, io::uo) is det.

next_pos(!IO) :-
	globals.get("Maze", Maze, !IO),
	globals.get("Pos", Pos0, !IO),
	globals.get("W", w(Visited0, Others0), !IO),
	Visited    = set.insert(Visited0, Pos0),
	AdjSet     = Maze ^ det_elem(Pos0),
	Choices0   = set.difference(AdjSet, Visited),
	ChoiceList = set.to_sorted_list(Choices0),
	Others1    = ChoiceList ++ Others0,
	(
		Others1 = [],
		globals.set("W", w(set.init, []), !IO)
	;
		Others1 = [Pos | Others],
		globals.set("Pos", Pos, !IO),
		globals.set("W", w(Visited, Others), !IO)
	).

%-----------------------------------------------------------------------------%

:- pred maze.create(maze::in, io::di, io::uo) is det.

maze.create(Maze, !IO) :-
	MazeList = map.to_assoc_list(Maze),
	Walls    = maze.cons_walls(MazeList),
	WallList = map.to_assoc_list(Walls),
	mogl.new_list(maze_list, compile, !IO),
		mogl.begin(quads, !IO),
			maze.make_mlist(WallList, !IO),
		mogl.end(!IO),
	mogl.end_list(!IO),

	mogl.point_size(1.5, !IO),

	mogl.light_model(light_model_two_side(yes), !IO),
	mogl.light_model(light_model_local_viewer(yes), !IO),

	mogl.enable(normalize,!IO),
	mogl.enable(lighting, !IO),
	mogl.enable(light(0), !IO),
	mogl.enable(light(1), !IO),

	mogl.shade_model(smooth, !IO),
	mogl.enable(depth_test, !IO).

:- pred make_mlist(list(pair(pos, list(wall)))::in, io::di, io::uo) is det.

make_mlist([], !IO).
make_mlist([Pos - Walls | Rest], !IO) :-
	mogl.color3(0.7, 0.7, 0.7, !IO),
	list.foldl(maze.wall(Pos), Walls, !IO),
	maze.make_mlist(Rest, !IO).

:- pred maze.wall(pos::in, wall::in, io::di, io::uo) is det.

maze.wall(pos(X0, Z0), north, !IO) :-
	X = float(X0),
	Z = float(Z0),
	mogl.vertex3(X,       0.0, Z + 1.0, !IO),
	mogl.vertex3(X + 1.0, 0.0, Z + 1.0, !IO),
	mogl.vertex3(X + 1.0, 1.0, Z + 1.0, !IO),
	mogl.vertex3(X,       1.0, Z + 1.0, !IO).
maze.wall(pos(X0, Z0), south, !IO) :-
	X = float(X0),
	Z = float(Z0),
	mogl.vertex3(X,       0.0, Z, !IO),
	mogl.vertex3(X + 1.0, 0.0, Z, !IO),
	mogl.vertex3(X + 1.0, 1.0, Z, !IO),
	mogl.vertex3(X,       1.0, Z, !IO).
maze.wall(pos(X0, Z0), east, !IO) :-
	X = float(X0),
	Z = float(Z0),
	mogl.vertex3(X + 1.0, 0.0, Z,       !IO),
	mogl.vertex3(X + 1.0, 0.0, Z + 1.0, !IO),
	mogl.vertex3(X + 1.0, 1.0, Z + 1.0, !IO),
	mogl.vertex3(X + 1.0, 1.0, Z,       !IO).
maze.wall(pos(X0, Z0), west, !IO) :-
	X = float(X0),
	Z = float(Z0),
	mogl.vertex3(X, 0.0, Z,       !IO),
	mogl.vertex3(X, 0.0, Z + 1.0, !IO),
	mogl.vertex3(X, 1.0, Z + 1.0, !IO),
	mogl.vertex3(X, 1.0, Z,       !IO).

	% The stuff that happens for each frame.
:- pred maze.display(io::di, io::uo) is det.

maze.display(!IO) :-
	globals.get("Size", Size, !IO),
	mogl.clear_color(0.0, 0.0, 0.0, 0.0, !IO),
	mogl.clear([color, depth], !IO),

	mogl.matrix_mode(modelview, !IO),
	mogl.push_matrix(!IO),
		mogl.load_identity(!IO),
		mogl.light(0, position(0.0, 0.0, 0.0, 1.0),   !IO),
		mogl.light(0, ambient(0.5, 0.5, 0.0, 1.0),    !IO),
		mogl.light(0, diffuse(0.7, 0.7, 0.0, 1.0),    !IO),
		mogl.light(0, specular(0.7, 0.7, 0.0, 1.0),   !IO),
		mogl.light(1, position(Size, 0.0, Size, 1.0), !IO),
		mogl.light(1, ambient(0.0, 0.0, 0.7, 1.0),    !IO),
		mogl.light(1, diffuse(0.8, 0.0, 0.7, 1.0),    !IO),
		mogl.light(1, specular(0.0, 0.0, 0.7, 1.0),   !IO),
	mogl.pop_matrix(!IO),

	maze.draw_maze(!IO),

	glut.window.swap_buffers(!IO).

:- pred maze.reshape(int::in, int::in, io::di, io::uo) is det.

maze.reshape(Width, Height, !IO) :-
	mogl.viewport(0, 0, Width, Height, !IO),
	mogl.matrix_mode(projection, !IO),
	mogl.load_identity(!IO),
	mglu.perspective(55.0, float(Width) / float(Height), 0.1, 10000.0, !IO),
	mogl.matrix_mode(modelview, !IO).

%------------------------------------------------------------------------------%
%
% Maze drawing.
%

:- pred draw_maze(io::di, io::uo) is det.

draw_maze(!IO) :-
	mogl.load_identity(!IO),
	globals.get("W", w(Visited, Other), !IO),
	globals.get("Size", Size, !IO),
	globals.get("Phi", Phi, !IO),
	globals.get("Theta", Theta, !IO),
	R = 1.5 * Size,
	Y = R * sin(Theta),
	Q = R * cos(Theta),
	X = Q * cos(Phi) + 0.5 * Size,
	Z = Q * sin(Phi) + 0.5 * Size,
	mglu.look_at(X, Y, Z, 0.5 * Size, 0.0, 0.5 * Size, 0.0, 0.0, 1.0, !IO),
	mogl.disable(lighting, !IO),
	mogl.begin(points, !IO),
		mogl.color3(0.0, 1.0, 0.0, !IO),
		VisList = set.to_sorted_list(Visited),
		list.foldl(draw_vis, VisList, !IO),
		mogl.color3(1.0, 0.0, 0.0, !IO),
		list.foldl(draw_vis, Other, !IO),
	mogl.end(!IO),
	mogl.enable(lighting, !IO),
	mogl.call_list(maze_list, !IO),
	globals.set("Phi", Phi + 0.005, !IO),
	globals.set("Theta", Theta + 0.006, !IO).

:- pred maze.draw_vis(pos::in, io::di, io::uo) is det.

maze.draw_vis(pos(Xi, Zi), !IO) :-
	mogl.vertex3(float(Xi) + 0.5, 0.5, float(Zi) + 0.5, !IO).

%------------------------------------------------------------------------------%

	% Convert the adjacency representation of the maze to a walls
	% representation.
:- func cons_walls(list(pair(pos, set(pos)))) = walls.

cons_walls(Maze) = Walls :-
	ConsWalls = (func(F - T, Wall0) = Wall :-
		( if	Wall0 ^ elem(F) = Sides0
		  then	Sides1 = Sides0
		  else	Sides1 = [north, south, east, west]
		),
		Nexts  = set.to_sorted_list(T),
		Sides2 = list.foldl(remove_side(F), Nexts, Sides1),
		Wall   = Wall0 ^ elem(F) := Sides2
	),
	Walls = list.foldl(ConsWalls, Maze, map.init).

:- func remove_side(pos, pos, list(wall)) = list(wall).

remove_side(pos(X0, Y0), pos(X1, Y1), Sides0) = Sides :-
	( X1 = X0 + 1 -> Side = east
	; X1 = X0 - 1 -> Side = west
	; Y1 = Y0 + 1 -> Side = north
	; 		 Side = south
	),
	Sides = list.delete_all(Sides0, Side).

%------------------------------------------------------------------------------%
%
% Maze creation.
%

:- pred dig(pos::in, list(int)::in, list(int)::in, maze::in, maze::out,
	random.supply::mdi, random.supply::muo) is det.

dig(_, [], _, !Maze, !Rnd).
dig(FarPos, [X | Xs], Ys, !Maze, !Rnd) :-
	dig1(FarPos, X, Ys, !Maze, !Rnd),
	dig(FarPos, Xs, Ys, !Maze, !Rnd).

:- pred dig1(pos::in, int::in, list(int)::in, maze::in, maze::out,
	random.supply::mdi, random.supply::muo) is det.

dig1(_, _, [], !Maze, !Rnd).
dig1(FarPos, X, [Y | Ys], !Maze, !Rnd) :-
	Pos = pos(X, Y),
	adj(FarPos, Pos, AdjPoss, !Rnd),
	dig2(FarPos, AdjPoss, !Maze, !Rnd),
	dig1(FarPos, X, Ys, !Maze, !Rnd).

:- pred dig2(pos::in, list(adj)::in, maze::in, maze::out,
	random.supply::mdi, random.supply::muo) is det.

dig2(_, [], !Maze, !Rnd).
dig2(FarPos, [adj(NewPos, OldPos) | Rest], !Maze, !Rnd) :-
	(
		not map.contains(!.Maze, NewPos)
	->
		knock_out_wall(OldPos, NewPos, !Maze),
		adj(FarPos, NewPos, AdjPoss, !Rnd),
		dig2(FarPos, AdjPoss, !Maze, !Rnd)
	;
		dig2(FarPos, Rest, !Maze, !Rnd)
	).

:- pred maze.adj(pos::in, pos::in, list(adj)::out, random.supply::mdi,
	random.supply::muo) is det.

maze.adj(pos(FarX, FarY), pos(X, Y), Adjs, !Rnd) :-
	Pred = (pred(Adj::out) is nondet :-
			(
				X1 = X - 1,
				Y1 = Y
			;
				X1 = X + 1,
				Y1 = Y
			;
				X1 = X,
				Y1 = Y + 1
			;
				X1 = X,
				Y1 = Y - 1
			),
			Adj = adj(pos(X1, Y1), pos(X, Y)),
			X1 >= 0, X1 < FarX,
			Y1 >= 0, Y1 < FarY
	),
	solutions(Pred, Adjs0),
	shuffle(20, Adjs0, Adjs, !Rnd).

:- pred knock_out_wall(pos::in, pos::in, maze::in, maze::out) is det.

knock_out_wall(NewPos, OldPos, !Maze) :-
	( if	!.Maze ^ elem(NewPos) = NewSet0
	  then	NewSet = set.insert(NewSet0, OldPos)
	  else	NewSet = set.make_singleton_set(OldPos)
	),
	!Maze ^ elem(NewPos) := NewSet,
	( if	!.Maze ^ elem(OldPos) = OldSet0
	  then	OldSet = set.insert(OldSet0, NewPos)
	  else  OldSet = set.make_singleton_set(NewPos)
	),
	!Maze ^ elem(OldPos) := OldSet.

:- pred shuffle(int::in, list(T)::in, list(T)::out, random.supply::mdi,
	random.supply::muo) is det.

shuffle(C, !List, !Rnd) :-
	( C > 0 ->
		L = list.length(!.List),
		random.random(J, !Rnd),
		get_nth(!.List, J mod L, X, !:List),
		shuffle(C - 1, [X | !.List], !:List, !Rnd)
	;
		true
	).

:- pred get_nth(list(T)::in, int::in, T::out, list(T)::out) is det.

get_nth([], _, _, _) :-
	error("get_nth: ran out of items!").
get_nth([X | Xs], I, Y, Ys) :-
	( I =< 0 ->
		Y = X,
		Ys = Xs
	;
		I1 = I - 1,
		get_nth(Xs, I1, Y, Zs),
		Ys = [X | Zs]
	).

:- pred between(int::in, int::in, int::out) is nondet.

between(Min, Max, I) :-
	Min =< Max,
	(
		I = Min
	;
		Min1 = Min + 1,
		between(Min1, Max, I)
	).

%------------------------------------------------------------------------------%
%
% Keyboard handling.
%

:- pred maze.keyboard(char::in, int::in, int::in, io::di, io::uo) is det.

maze.keyboard(Key, _, _, !IO) :-
	( if char.to_int(Key, 27) then glut.quit(!IO) else true ).

%-----------------------------------------------------------------------------%
%
% Options processing.
%

:- pred short(char::in, option::out) is semidet.

short('x', width).
short('y', height).
short('s', seed).

:- pred long(string::in, option::out) is semidet.

long("width",  width).
long("height", height).
long("seed",   seed).

:- pred defaults(option::out, option_data::out) is multi.

defaults(width,  int(12)).
defaults(height, int(12)).
defaults(seed,	  int(0)).

%-----------------------------------------------------------------------------%
%
% Display list ids.
%

:- func maze_list = int.

maze_list = 7.

%-----------------------------------------------------------------------------%
:- end_module maze.
%-----------------------------------------------------------------------------%
