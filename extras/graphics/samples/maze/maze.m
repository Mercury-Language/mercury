%------------------------------------------------------------------------------%
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
%------------------------------------------------------------------------------%
:- module maze.

%------------------------------------------------------------------------------%
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%------------------------------------------------------------------------------%
:- implementation.

:- import_module mtcltk, mogl, mglu, mtogl.
:- import_module globals.
:- import_module bool, getopt, map, set, random, require.
:- import_module char, list, math, string, float, int, std_util.

:- type option
	--->	height
	;	width
	;	seed
	.

:- type wall
	--->	north
	;	south
	;	east
	;	west
	.

:- type pos
	--->	pos(int, int).

:- type adj
	--->	adj(pos, pos).

:- type maze	==	map(pos, set(pos)).

:- type walls	==	map(pos, list(wall)).

:- type wander
	--->	w(set(pos), list(pos)).

%------------------------------------------------------------------------------%

main -->
	init_globals,
		% Process the command line options...
	io__command_line_arguments(Args0),
	{ getopt__process_options(option_ops(short, long, defaults),
		Args0, _Args, MOpts) },
	(
		{ MOpts = ok(Opts) },
			% Create the maze
		io__write_string("generating maze...."),
		io__flush_output,
		{ getopt__lookup_int_option(Opts, width, Xmax) },
		{ getopt__lookup_int_option(Opts, height, Ymax) },
		{ getopt__lookup_int_option(Opts, seed, Seed) },
		set_global("Size", float(Xmax)),
		{ map__init(Maze0) },
		{ XPred = lambda([X::out] is nondet, (
				between(0, Xmax-1, X)
		)) },
		{ solutions(XPred, XIndexs) },
		{ YPred = lambda([Y::out] is nondet, (
				between(0, Ymax-1, Y)
		)) },
		{ solutions(YPred, YIndexes) },
		{ random__init(Seed, Rnd0) },
		{ dig(pos(Xmax, Ymax), XIndexs, YIndexes, Maze0, Maze1,
			Rnd0, Rnd) },
		io__write_string(" done.\n"),
		io__flush_output,
			% enter Tcl/Tk.
		main(doit(Maze1, Rnd), ["maze"])

	;
		{ MOpts = error(Str) },
		{ string__format("usage: maze [-xN] [-yN] [-sN]\nerror: %s\n",
			[s(Str)], Msg) },
		io__stderr_stream(StdErr),
		io__write_string(StdErr, Msg)
	).

%------------------------------------------------------------------------------%

	% Main callback from Tcl/Tk.

:- pred doit(maze, random__supply, tcl_interp, io__state, io__state).
:- mode doit(in, mdi, in, di, uo) is det.

doit(Maze, Rnd0, Interp) -->
		% Initialize the Togl widget.
	mtogl__init(Interp, Res0),
	{ Res0 \= tcl_ok -> error("Mtogl__init failed") ; true },
	mtogl__create(maze__create(Maze)),
	mtogl__display(maze__display),
	mtogl__reshape(maze__reshape),

	set_global("Maze", Maze),
	set_global("Pos", pos(0, 0)),
	set_global("Dir", east),
	set_global("Rnd", Rnd0),
	set_global("Phi", 0.0),
	set_global("Theta", 0.0),
	{ set__init(Set) },
	set_global("W", w(Set, [])),
	create_command(Interp, "nextframe", nextframe),
		% Create a new togl widget
	eval(Interp, "
		togl .togl -rgb true -double true -depth true \
			-privatecmap true -height 300 -width 300
		pack .togl
		nextframe
	", Res1, Str),
	{ Res1 \= tcl_ok -> error(Str) ; true }.

	% Compute each frame of the display.
:- pred nextframe(tcl_interp, list(string),
		tcl_status, string, io__state, io__state).
:- mode nextframe(in, in, out, out, di, uo) is det.

nextframe(Interp, _Args, tcl_ok, "") -->
	next_pos,
	eval(Interp, "
		.togl render
		after 1 nextframe
	", Res, Str),
	{ Res \= tcl_ok -> error(Str) ; true }.

	% Work out the next position in the traversal of the maze.
:- pred next_pos(io__state, io__state).
:- mode next_pos(di, uo) is det.

next_pos -->
	get_global("Maze", Maze),
	get_global("Pos", Pos0),
	get_global("W", w(Visited0, Others0)),
	{ set__insert(Visited0, Pos0, Visited) },
	{ map__lookup(Maze, Pos0, Nexts) },
	{ set__list_to_set(Nexts, AdjSet) },
	{ set__difference(AdjSet, Visited, Choices0) },
	{ set__to_sorted_list(Choices0, ChoiceList) },
	{ list__append(ChoiceList, Others0, Others1) },
	(
		{ Others1 = [] },
		{ set__init(Set) },
		set_global("W", w(Set, []))
	;
		{ Others1 = [Pos|Others] },
		set_global("Pos", Pos),
		set_global("W", w(Visited, Others))
	),
	[].

:- pred move(pos, wall, pos).
:- mode move(in, in, out) is det.

move(pos(X, Y), east, pos(X+1, Y)).
move(pos(X, Y), west, pos(X-1, Y)).
move(pos(X, Y), north, pos(X, Y+1)).
move(pos(X, Y), south, pos(X, Y-1)).

:- pred left(wall, wall).
:- mode left(in, out) is det.
:- mode left(out, in) is det.

left(north, west).
left(west, south).
left(south, east).
left(east, north).

%------------------------------------------------------------------------------%

:- pred maze__create(maze, togl, io__state, io__state).
:- mode maze__create(in, in, di, uo) is det.

	% Set up everything for the display.

maze__create(Maze, _Togl) -->
	{ map__to_assoc_list(Maze, MazeList) },
	{ map__init(Walls0) },
	{ cons_walls(MazeList, Walls0, Walls) },
	{ map__to_assoc_list(Walls, List) },
	new_list(7, compile),
		begin(quads),
		make_mlist(List),
		end,
	end_list,

	point_size(1.5),

	light_model(light_model_two_side(yes)),
	light_model(light_model_local_viewer(yes)),

	enable(normalize),
	enable(lighting),
	enable(light(0)),
	enable(light(1)),

	shade_model(smooth),
	enable(depth_test).

:- pred make_mlist(list(pair(pos, list(wall))), io__state, io__state).
:- mode make_mlist(in, di, uo) is det.

make_mlist([]) --> [].
make_mlist([Pos - Walls|Rest]) -->
/*
	{ Pos = pos(X, Z) },
	{ Xf = float(X) },
	{ Zf = float(Z) },
		% Top
	color3(1.0, 0.7, 0.0),
	vertex3(Xf, 1.0, Zf),
	vertex3(Xf+1.0, 1.0, Zf),
	vertex3(Xf+1.0, 1.0, Zf+1.0),
	vertex3(Xf, 1.0, Zf+1.0),
		% Bottom
	color3(0.0, 0.0, 0.7),
	vertex3(Xf, 0.0, Zf),
	vertex3(Xf+1.0, 0.0, Zf),
	vertex3(Xf+1.0, 0.0, Zf+1.0),
	vertex3(Xf, 0.0, Zf+1.0),
*/
		% Walls
	color3(0.7, 0.7, 0.7),
	list__foldl(wall(Pos), Walls),
	
	make_mlist(Rest).

:- pred wall(pos, wall, io__state, io__state).
:- mode wall(in, in, di, uo) is det.

wall(pos(X0, Z0), north) -->
	{ X = float(X0) },
	{ Z = float(Z0) },
	vertex3(X, 0.0, Z+1.0),
	vertex3(X+1.0, 0.0, Z+1.0),
	vertex3(X+1.0, 1.0, Z+1.0),
	vertex3(X, 1.0, Z+1.0).
wall(pos(X0, Z0), south) -->
	{ X = float(X0) },
	{ Z = float(Z0) },
	vertex3(X, 0.0, Z),
	vertex3(X+1.0, 0.0, Z),
	vertex3(X+1.0, 1.0, Z),
	vertex3(X, 1.0, Z).
wall(pos(X0, Z0), east) -->
	{ X = float(X0) },
	{ Z = float(Z0) },
	vertex3(X+1.0, 0.0, Z),
	vertex3(X+1.0, 0.0, Z+1.0),
	vertex3(X+1.0, 1.0, Z+1.0),
	vertex3(X+1.0, 1.0, Z).
wall(pos(X0, Z0), west) -->
	{ X = float(X0) },
	{ Z = float(Z0) },
	vertex3(X, 0.0, Z),
	vertex3(X, 0.0, Z+1.0),
	vertex3(X, 1.0, Z+1.0),
	vertex3(X, 1.0, Z).

	% The stuff that happens for each frame.

:- pred maze__display(togl, io__state, io__state).
:- mode maze__display(in, di, uo) is det.

maze__display(Togl) -->
	get_global("Size", Size),
	clear_color(0.0, 0.0, 0.0, 0.0),
	clear([color, depth]),

	matrix_mode(modelview),
	push_matrix,
		load_identity,
		light(0, position(0.0, 0.0, 0.0, 1.0)),
		light(0, ambient(0.5, 0.5, 0.0, 1.0)),
		light(0, diffuse(0.7, 0.7, 0.0, 1.0)),
		light(0, specular(0.7, 0.7, 0.0, 1.0)),
		light(1, position(Size, 0.0, Size, 1.0)),
		light(1, ambient(0.0, 0.0, 0.7, 1.0)),
		light(1, diffuse(0.8, 0.0, 0.7, 1.0)),
		light(1, specular(0.0, 0.0, 0.7, 1.0)),
	pop_matrix,

	draw_maze,

	mtogl__swap_buffers(Togl).

	% The stuff that happens if we resize the togl widget.

:- pred maze__reshape(togl, io__state, io__state).
:- mode maze__reshape(in, di, uo) is det.

maze__reshape(_Togl) -->
	matrix_mode(projection),
	load_identity,
	perspective(55.0, 1.0, 0.1, 10000.0),
	matrix_mode(modelview).

%------------------------------------------------------------------------------%

:- pred draw_maze(io__state, io__state).
:- mode draw_maze(di, uo) is det.

draw_maze -->
	load_identity,
	get_global("W", w(Visited, Other)),
	get_global("Size", Size),
	get_global("Phi", Phi),
	get_global("Theta", Theta),
	{ R = 1.5 * Size },
	{ Y = R * sin(Theta) },
	{ Q = R * cos(Theta) },
	{ X = Q * cos(Phi) + 0.5*Size },
	{ Z = Q * sin(Phi) + 0.5*Size },
	look_at(X, Y, Z, 0.5*Size, 0.0, 0.5*Size, 0.0, 0.0, 1.0),
	disable(lighting),
	begin(points),
		color3(0.0, 1.0, 0.0),
		{ set__to_sorted_list(Visited, VisList) },
		list__foldl(draw_vis, VisList),
		color3(1.0, 0.0, 0.0),
		list__foldl(draw_vis, Other),
	end,
	enable(lighting),
	call_list(7),
	set_global("Phi", Phi+0.005),
	set_global("Theta", Theta+0.006).

:- pred draw_vis(pos, io__state, io__state).
:- mode draw_vis(in, di, uo) is det.

draw_vis(pos(Xi, Zi)) -->
	vertex3(float(Xi)+0.5, 0.5, float(Zi)+0.5).

%------------------------------------------------------------------------------%

	% Convert the adjacency representation of the maze to a walls
	% representation.
:- pred cons_walls(list(pair(pos, set(pos))), walls, walls).
:- mode cons_walls(in, in, out) is det.

cons_walls([], Walls, Walls).
cons_walls([F-T|Rest], Walls0, Walls) :-
	(
		map__search(Walls0, F, Sides0)
	->
		Sides1 = Sides0
	;
		Sides1 = [north, south, east, west]
	),
	set__to_sorted_list(T, Nexts),
	list__foldl(remove_side(F), Nexts, Sides1, Sides2),
	map__set(Walls0, F, Sides2, Walls1),
	cons_walls(Rest, Walls1, Walls).

:- pred remove_side(pos, pos, list(wall), list(wall)).
:- mode remove_side(in, in, in, out) is det.

remove_side(pos(X0, Y0), pos(X1, Y1), Sides0, Sides) :-
	( X1 is X0 + 1 ->
		Side = east
	; X1 is X0 - 1 ->
		Side = west
	; Y1 is Y0 + 1 ->
		Side = north
	;
		Side = south
	),
	list__delete_all(Sides0, Side, Sides).

%------------------------------------------------------------------------------%

	% Create the maze.

:- pred dig(pos, list(int), list(int), maze, maze,
		random__supply, random__supply).
:- mode dig(in, in, in, in, out, mdi, muo) is det.

dig(_Pos, [], _, Maze, Maze, Rnd, Rnd).
dig(FarPos, [X|Xs], Ys, Maze0, Maze, Rnd0, Rnd) :-
	dig1(FarPos, X, Ys, Maze0, Maze1, Rnd0, Rnd1),
	dig(FarPos, Xs, Ys, Maze1, Maze, Rnd1, Rnd).

:- pred dig1(pos, int, list(int), maze, maze, random__supply, random__supply).
:- mode dig1(in, in, in, in, out, mdi, muo) is det.

dig1(_FarPos, _X, [], Maze, Maze, Rnd, Rnd).
dig1(FarPos, X, [Y|Ys], Maze0, Maze, Rnd0, Rnd) :-
	Pos = pos(X, Y),
	adj(FarPos, Pos, AdjPoss, Rnd0, Rnd1),
	dig2(FarPos, AdjPoss, Maze0, Maze1, Rnd1, Rnd2),
	dig1(FarPos, X, Ys, Maze1, Maze, Rnd2, Rnd).

:- pred dig2(pos, list(adj), maze, maze, random__supply, random__supply).
:- mode dig2(in, in, in, out, mdi, muo) is det.

dig2(_FarPos, [], Maze, Maze, Rnd, Rnd).
dig2(FarPos, [adj(NewPos, OldPos)|Rest], Maze0, Maze, Rnd0, Rnd) :-
	(
		\+ map__contains(Maze0, NewPos)
	->
		knock_out_wall(OldPos, NewPos, Maze0, Maze1),
		adj(FarPos, NewPos, AdjPoss, Rnd0, Rnd1),
		dig2(FarPos, AdjPoss, Maze1, Maze, Rnd1, Rnd)
	;
		dig2(FarPos, Rest, Maze0, Maze, Rnd0, Rnd)
	).

:- pred adj(pos, pos, list(adj), random__supply, random__supply).
:- mode adj(in, in, out, mdi, muo) is det.

adj(pos(FarX, FarY), pos(X, Y), Adjs, Rnd0, Rnd) :-
	Pred = lambda([Adj::out] is nondet, (
			(
				X1 is X - 1,
				Y1 = Y
			;
				X1 is X + 1,
				Y1 = Y
			;
				X1 = X,
				Y1 is Y + 1
			;
				X1 = X,
				Y1 is Y - 1
			),
			Adj = adj(pos(X1, Y1), pos(X, Y)),
			X1 >= 0, X1 < FarX,
			Y1 >= 0, Y1 < FarY
	)),
	solutions(Pred, Adjs0),
	shuffle(20, Adjs0, Adjs, Rnd0, Rnd).

:- pred knock_out_wall(pos, pos, maze, maze).
:- mode knock_out_wall(in, in, in, out) is det.

knock_out_wall(NewPos, OldPos, Maze0, Maze) :-
	(
		map__search(Maze0, NewPos, NewSet0)
	->
		set__insert(NewSet0, OldPos, NewSet)
	;
		set__singleton_set(NewSet, OldPos)
	),
	map__set(Maze0, NewPos, NewSet, Maze1),
	(
		map__search(Maze1, OldPos, OldSet0)
	->
		set__insert(OldSet0, NewPos, OldSet)
	;
		set__singleton_set(OldSet, NewPos)
	),
	map__set(Maze1, OldPos, OldSet, Maze).

:- pred shuffle(int, list(T), list(T), random__supply, random__supply).
:- mode shuffle(in, in, out, mdi, muo) is det.

shuffle(C, List0, List, Rnd0, Rnd) :-
	( C > 0 ->
		list__length(List0, L),
		random__random(J, Rnd0, Rnd1),
		get_nth(List0, J mod L, X, List1),
		C1 is C - 1,
		shuffle(C1, [X|List1], List, Rnd1, Rnd)
	;
		List = List0,
		Rnd = Rnd0
	).

:- pred get_nth(list(T), int, T, list(T)).
:- mode get_nth(in, in, out, out) is det.

get_nth([], _, _, _) :-
	error("get_nth: ran out of items!").
get_nth([X|Xs], I, Y, Ys) :-
	( I =< 0 ->
		Y = X,
		Ys = Xs
	;
		I1 is I - 1,
		get_nth(Xs, I1, Y, Zs),
		Ys = [X|Zs]
	).

:- pred between(int, int, int).
:- mode between(in, in, out) is nondet.

between(Min, Max, I) :-
	Min =< Max,
	(
		I = Min
	;
		Min1 is Min + 1,
		between(Min1, Max, I)
	).

%------------------------------------------------------------------------------%

:- pred short(char, option).
:- mode short(in, out) is semidet.

short('x', width).
short('y', height).
short('s', seed).

:- pred long(string, option).
:- mode long(in, out) is semidet.

long("width", width).
long("height", height).
long("seed", seed).

:- pred defaults(option, option_data).
:- mode defaults(out, out) is nondet.

defaults(Option, Value) :-
	semidet_succeed, defaults0(Option, Value).

:- pred defaults0(option, option_data).
:- mode defaults0(out, out) is multi.

defaults0(width,	int(12)).
defaults0(height,	int(12)).
defaults0(seed,		int(0)).
