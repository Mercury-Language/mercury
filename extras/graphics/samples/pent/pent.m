%------------------------------------------------------------------------------%
% file: pent.m
% author: Tyson Dowd, August 1997 (based on code by Tom Conway)
%
% pent tries to place pentominoes on a 6x5 grid.
%
% This source file is hereby placed in the public domain. -Tyson Dowd
% (the author).
%
%------------------------------------------------------------------------------%
:- module pent.

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
:- import_module place_pent.

:- type option
	--->	height
	;	width
	.

%------------------------------------------------------------------------------%

main -->
	init_globals,
		% Process the command line options...
	io__command_line_arguments(Args0),
	{ getopt__process_options(option_ops(short, long, defaults),
		Args0, _Args, MOpts) },
	(
		{ MOpts = ok(Opts) },
			% Create the pentomino boards
		io__write_string("generating pentominoes...."),
		io__flush_output,
		{ getopt__lookup_int_option(Opts, width, Width) },
		{ getopt__lookup_int_option(Opts, height, Height) },
		set_global("Size", float(MaxX)), % XXX
		set_global("MaxX", MaxX),
		set_global("MaxY", MaxY), 

		{ MaxX = Width - 1 },
		{ MaxY = Height - 1 },
		{ initial_board(Board0) },
		{ initial_sqs(MaxX, MaxY, Sqs0) },
		{ initial_pieces(Pieces) },

		{ solutions(fill_board(MaxX, MaxY, Sqs0, Pieces, Board0),
			Boards) },

		io__write_string(" done.\n"),
		io__flush_output,
			% enter Tcl/Tk.
		main(doit(Boards, MaxX, MaxY), ["maze"])

	;
		{ MOpts = error(Str) },
		{ string__format("usage: maze [-xN] [-yN] [-sN]\nerror: %s\n",
			[s(Str)], Msg) },
		io__stderr_stream(StdErr),
		io__write_string(StdErr, Msg)
	).

%------------------------------------------------------------------------------%

	% Main callback from Tcl/Tk.

:- pred doit(list(board), int, int, tcl_interp, io__state, io__state).
:- mode doit(in, in, in, in, di, uo) is det.

doit([], _, _, _) --> { error("no solutions to draw") }.
doit([Board | Boards], MaxX, MaxY, Interp) -->
		% Initialize the Togl widget.
	mtogl__init(Interp, Res0),
	{ Res0 \= tcl_ok -> error("Mtogl__init failed") ; true },
	mtogl__create(pent__create(Board, MaxX, MaxY)),
	mtogl__display(pent__display),
	mtogl__reshape(pent__reshape),

	set_global("Boards", Boards),
	set_global("Phi", 0.0),
	set_global("Theta", 0.0),
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
	eval(Interp, "
		.togl render
		after 1 nextframe
	", Res, Str),
	{ Res \= tcl_ok -> error(Str) ; true }.

%------------------------------------------------------------------------------%

:- pred pent__create(board, int, int, togl, io__state, io__state).
:- mode pent__create(in, in, in, in, di, uo) is det.

	% Set up everything for the display.

pent__create(_Board, _MaxX, _MaxY, _Togl) -->

	get_global("Size", Size),

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
	point_size(1.5),

	light_model(light_model_two_side(yes)),
	light_model(light_model_local_viewer(yes)),

	enable(normalize),
	enable(lighting),
	enable(light(0)),
	enable(light(1)),

	shade_model(smooth),
	enable(depth_test).

:- pred make_mlist(list(pair(square, piece)), io__state, io__state).
:- mode make_mlist(in, di, uo) is det.

make_mlist([]) --> [].
make_mlist([Pos - Piece |Rest]) -->

	{ X = sq_x(Pos) },
	{ Y = sq_y(Pos) },

	{ Xf = float(X) },
	{ Zf = float(Y) },

	(
		{ not Piece = e }
	->
		push_matrix,
			translate(Xf, 0.0, Zf),
			set_colour_of_piece(Piece),
			draw_cube,
		pop_matrix
	;
		[]
	),
	make_mlist(Rest).

:- pred set_colour_of_piece(piece::in, io__state::di, io__state::uo) is det.

set_colour_of_piece(i) --> material(front, specular(0.6, 0.0, 1.0, 1.0)).
set_colour_of_piece(y) --> material(front, specular(0.8, 0.4, 0.8, 1.0)).
set_colour_of_piece(l) --> material(front, specular(0.8, 0.6, 1.0, 1.0)).
set_colour_of_piece(n) --> material(front, specular(0.6, 0.4, 1.0, 1.0)).
set_colour_of_piece(t) --> material(front, specular(0.4, 0.2, 1.0, 1.0)).
set_colour_of_piece(v) --> material(front, specular(0.6, 0.8, 1.0, 1.0)).
set_colour_of_piece(f) --> material(front, specular(0.8, 0.6, 0.8, 1.0)).
set_colour_of_piece(w) --> material(front, specular(0.0, 0.6, 0.6, 1.0)).
set_colour_of_piece(z) --> material(front, specular(0.6, 0.8, 0.8, 1.0)).
set_colour_of_piece(p) --> material(front, specular(0.0, 0.4, 0.6, 1.0)).
set_colour_of_piece(u) --> material(front, specular(0.6, 0.0, 0.8, 1.0)).
set_colour_of_piece(x) --> material(front, specular(0.0, 0.4, 1.0, 1.0)).
set_colour_of_piece(e) --> material(front, specular(0.4, 0.0, 0.4, 1.0)).

:- pred draw_cube(io__state::di, io__state::uo) is det.

draw_cube -->
	{ Left = 0.10 },
	{ Right = 0.90 },
	material(front, ambient(0.0, 0.0, 0.0, 1.0)),
	material(front, diffuse(0.0, 0.0, 0.0, 1.0)),
	material(front, emission(0.0, 0.0, 0.0, 1.0)),
	begin(quads),
		vertex3(Left, Left, Left),
		vertex3(Left, Right, Left),
		vertex3(Right, Right, Left),
		vertex3(Right, Left, Left),

		vertex3(Left, Left, Right),
		vertex3(Left, Right, Right),
		vertex3(Right, Right, Right),
		vertex3(Right, Left, Right),

		vertex3(Left, Left, Left),
		vertex3(Left, Left, Right),
		vertex3(Right, Left, Right),
		vertex3(Right, Left, Left),

		vertex3(Left, Right, Left),
		vertex3(Left, Right, Right),
		vertex3(Right, Right, Right),
		vertex3(Right, Right, Left),

		vertex3(Left, Left, Left),
		vertex3(Left, Left, Right),
		vertex3(Left, Right, Right),
		vertex3(Left, Right, Left),

		vertex3(Right, Left, Left),
		vertex3(Right, Left, Right),
		vertex3(Right, Right, Right),
		vertex3(Right, Right, Left),
	end.


	% The stuff that happens for each frame.

:- pred pent__display(togl, io__state, io__state).
:- mode pent__display(in, di, uo) is det.

pent__display(Togl) -->

	clear_color(0.0, 0.0, 0.0, 0.0),
	clear([color, depth]),

	draw_maze,

	mtogl__swap_buffers(Togl).

	% The stuff that happens if we resize the togl widget.

:- pred pent__reshape(togl, io__state, io__state).
:- mode pent__reshape(in, di, uo) is det.

pent__reshape(_Togl) -->
	matrix_mode(projection),
	load_identity,
	perspective(55.0, 1.0, 0.1, 10000.0),
	matrix_mode(modelview).

%------------------------------------------------------------------------------%

:- pred draw_maze(io__state, io__state).
:- mode draw_maze(di, uo) is det.

draw_maze -->
	load_identity,

		% Set the viewpoint
	get_global("Size", Size),
	get_global("Phi", Phi),
	get_global("Theta", Theta),
	get_global("Boards", Boards0),
	{ R = 1.5 * Size },
	{ Y = R * sin(Theta) },
	{ Q = R * cos(Theta) },
	{ X = Q * cos(Phi) + 0.5*Size },
	{ Z = Q * sin(Phi) + 0.5*Size },
	look_at(X, Y, Z, 0.5*Size, 0.0, 0.5*Size, 0.0, 0.0, 1.0),

	( { Boards0 = [Board | Boards] } ->
		new_list(7, compile),
			make_mlist(Board),
		end_list,
		set_global("Boards", Boards)
	;
		{ error("no more boards") }
	),

	call_list(7),
	
		% Turn the thing
	set_global("Phi", Phi+0.005),
	set_global("Theta", Theta+0.006).

%------------------------------------------------------------------------------%

:- pred short(char, option).
:- mode short(in, out) is semidet.

short('x', width).
short('y', height).

:- pred long(string, option).
:- mode long(in, out) is semidet.

long("width", width).
long("height", height).

:- pred defaults(option, option_data).
:- mode defaults(out, out) is nondet.

defaults(Option, Value) :-
	semidet_succeed, defaults0(Option, Value).

:- pred defaults0(option, option_data).
:- mode defaults0(out, out) is multi.

defaults0(width,	int(5)).
defaults0(height,	int(5)).
