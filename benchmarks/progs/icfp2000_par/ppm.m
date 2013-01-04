%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

%-----------------------------------------------------------------------------$
%
% Adapted from the following:
% > 433-380 Project, part B
% > Robert Jeschofnik (rejj), 55572
% >
% > poly.image.m
% >
% > Written in Mercury, because C sucks.
% >
% > This submodule handles the task of drawing the image file itself.
%
%-----------------------------------------------------------------------------%

:- module ppm.

:- interface.

:- import_module vector.

:- import_module array.
:- import_module io.

%-----------------------------------------------------------------------------%

%
% XXX I'm not sure if this interface is ideal; we might need to
%     revisit it.  -fjh.
%

% Type to represent the image that is being drawn. It is a two dimensional
% array of integers, representing the X and Y locations for every pixel
% (and their value)
:- type image == array(point). % array of color values, indexed by Y * Width + X

    % Create the empty image buffer.
    %
:- pred ppm.init(int::in, int::in, image::array_uo) is det.

    % Print out the header information for the PPM format, then output
    % the image buffer.
    %
:- pred ppm.draw_image(string::in, int::in, int::in, image::array_ui,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module require.

ppm.init(Width, Height, Image) :-
    array.init(Width * Height, point(0.0, 0.0, 0.0), Image).

ppm.draw_image(FileName, Width, Height, Image, !IO) :-
    io.open_output(FileName, Result, !IO),
    ( Result = ok(Stream) ->
        io.set_output_stream(Stream, _Old, !IO),
        private_builtin.unsafe_type_cast(Stream, BinStream),
        io.set_binary_output_stream(BinStream, _OldBin, !IO),
        draw_image_2(Width, Height, Image, !IO)
    ;
        throw(Result)
    ).

:- pred ppm.draw_image_2(int::in, int::in, image::array_ui,
    io::di, io::uo) is det.

ppm.draw_image_2(Width, Height, Image, !IO) :-
    io.print("P6\n", !IO),
    io.print("# Merry Mercurians\n", !IO),
    io.print(Width, !IO),
    io.print(" ", !IO),
    io.print(Height, !IO),
    io.nl(!IO),
    io.print(255, !IO),
    io.nl(!IO),
    io.flush_output(!IO),
    do_image_draw(Width, Height, 0, 0, Image, !IO).

:- pred do_image_draw(int::in, int::in, int::in, int::in, image::in,
    io::di, io::uo) is det.

% Output the image buffer in binary format. There are three bytes for every
% pixel (Red, Green, Blue). These bytes are just streamed to stdout.

do_image_draw(Width, Height, X, Y, Image, !IO) :-
    ( Y < Height ->
        ( X < Width ->
            array__lookup(Image, (Y * Width) + X, Point),
            Point = point(R0, G0, B0),
            R = round_to_int(R0 * 255.0),
            G = round_to_int(G0 * 255.0),
            B = round_to_int(B0 * 255.0),
            io__write_byte(R, !IO),
            io__write_byte(G, !IO),
            io__write_byte(B, !IO),
            do_image_draw(Width, Height, X + 1, Y, Image, !IO)
        ;
            do_image_draw(Width, Height, 0, Y + 1, Image, !IO)
        )
    ;
        true
    ).
