:- module ppm.
 
% This module handles reading and writing of PPM files
% Output always goes to stdout.

% Note:  I didn't have enough time to implement ppm_read

:- interface.

:- import_module io, gfx, int.

/*
:- type ppm_input_stream --->
        binary(io__binary_input_stream) ;
        text(io__input_stream).
*/

%    ppm_write_header(X, Y),
%    write the header for an ascii ppm file with image size X x Y.
:- pred ppm_write_header(int, int, io__state, io__state).
:- mode ppm_write_header(in, in, di, uo) is det.
    
%      ppm_write_pixel(Colour)
%      write a pixel to stdout
%      ensures that the pixels values are between 0 and 255 inclusive.
:- pred ppm_write_pixel(colour, io__state, io__state).
:- mode ppm_write_pixel(in, di, uo) is det.
                      

/*
:- pred ppm_read_header(string, int, int, ppm_input_stream, io__state, io__state).
:- mode ppm_read_header(in, out, out, out, di, uo) is det.

:- pred ppm_read_pixel(ppm_input_stream, colour, io__state, io__state).
:- mode ppm_read_pixel(in, out, di, uo) is det.
*/

:- implementation.

:- import_module list, string, float.

ppm_write_header(X, Y) -->
        { string__format("P3 %d %d 255\n", [i(X), i(Y)], S) },
        io__write_string(S).

ppm_write_pixel(colour(R, G, B)) -->
        { R1 = R * 255.0 },
        { G1 = G * 255.0 },
        { B1 = B * 255.0 },
        { R2 = float__round_to_int(R1) },
        { G2 = float__round_to_int(G1) },
        { B2 = float__round_to_int(B1) },
        { ( R2 < 0  ->  R3 = 0  ;  R3 = R2) },
        { ( G2 < 0  ->  G3 = 0  ;  G3 = G2) },
        { ( B2 < 0  ->  B3 = 0  ;  B3 = B2) },
        { ( R3 > 255  -> R4 = 255  ; R4 = R3) },
        { ( G3 > 255  -> G4 = 255  ; G4 = G3) },
        { ( B3 > 255  -> B4 = 255  ; B4 = B3) },
        { string__format("%d %d %d\n", [i(R4), i(G4), i(B4)], S) },
        io__write_string(S).

/*
ppm_read_header(Filename, X, Y, PPMStream) -->
         io__open_input(Filename, Result1),
            (
                { Result1 = error(ErrorCode) },
                { io__error_message(ErrorCode, ErrorString) },
                { error(ErrorString) }
            ;
                { Result1 = ok(Stream) },
                io__set_input_stream(Stream, _),
                io__close_input(Stream)

*/
