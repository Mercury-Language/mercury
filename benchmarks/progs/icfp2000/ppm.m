%-----------------------------------------------------------------------------$
%
% Adapted from the following:
% >	433-380 Project, part B
% >	Robert Jeschofnik (rejj), 55572
% >
% >	poly.image.m
% >
% >	Written in Mercury, because C sucks.
% >
% >	This submodule handles the task of drawing the image file itself.
%
%-----------------------------------------------------------------------------%

:- module ppm.

:- interface.

:- import_module array, io.
:- import_module vector.

%-----------------------------------------------------------------------------%

%
% XXX I'm not sure if this interface is ideal; we might need to
%     revisit it.  -fjh.
%

% Type to represent the image that is being drawn. It is a two dimensional
% array of integers, representing the X and Y locations for every pixel
% (and their value)
:- type image == array(point). % array of color values, indexed by Y * Width + X

:- pred ppm__init(int, int, image).
:- mode ppm__init(in, in, array_uo) is det.

:- pred ppm__draw_image(string, int, int, image, io__state, io__state).
:- mode ppm__draw_image(in, in, in, array_ui, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, float, exception, require.

% Create the empty image buffer
ppm__init(Width, Height, Image) :-
	array__init(Width * Height, point(0.0, 0.0, 0.0), Image).

% Print out the header information for the PPM format, then output the image
% buffer.
ppm__draw_image(FileName, Width, Height, Image) -->
	io__open_output(FileName, Result),
	( { Result = ok(Stream) } ->
		io__set_output_stream(Stream, _Old),
		{ private_builtin__unsafe_type_cast(Stream, BinStream) },
		io__set_binary_output_stream(BinStream, _OldBin),
		draw_image_2(Width, Height, Image)
	;
		{ throw(Result) }
	).

:- pred ppm__draw_image_2(int, int, image, io__state, io__state).
:- mode ppm__draw_image_2(in, in, array_ui, di, uo) is det.

ppm__draw_image_2(Width, Height, Image) -->
	io__print("P6\n"),
	io__print("# Merry Mercurians\n"),
	io__print(Width), io__print(" "), io__print(Height), io__nl,
	io__print(255), io__nl,
	io__flush_output,
	do_image_draw(Width, Height, 0, 0, Image).


:- pred do_image_draw(int, int, int, int, image, io__state, io__state).
:- mode do_image_draw(in, in, in, in, in, di, uo) is det.

% Output the image buffer in binary format. There are three bytes for every
% pixel (Red, Green, Blue). These bytes are just streamed to stdout.

do_image_draw(Width, Height, X, Y, Image) -->
	(   
	    { Y < Height }
	->
	    (
		{ X < Width }
	    ->
		{ array__lookup(Image, (Y * Width) + X, Point) },
		{ Point = point(R0, G0, B0),
		  R = round_to_int(R0 * 255.0),
		  G = round_to_int(G0 * 255.0),
		  B = round_to_int(B0 * 255.0)
		},
		io__write_byte(R),
		io__write_byte(G),
		io__write_byte(B),
		do_image_draw(Width, Height, X + 1, Y, Image)
	    ;
		do_image_draw(Width, Height, 0, Y + 1, Image)
	    )
	;
	    % The empty list is used to represent an empty clause in a DCG
	    []
	).
