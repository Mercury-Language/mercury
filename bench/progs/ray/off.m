% vim: ts=4 sw=4 expandtab

:- module off.
% read in an off object and put it in the standard form:  i.e. scale it so it
% just fits in a 1x1x1 cube and centre it at the origin.

:- interface.

:- import_module io, int, list, vec3, gfx.

:- type off_object --->
        obj(
                list(vec3),     % list of vertices
                list(off_face)   % list of faces
           ).

:- type off_face --->
        face(
                int,            % number of vertices
                list(int),      % list of vertex indices
                colour          % intrinsic colour of face
            ).

% read_off(Filename, Object)
% read Object from Filename and ``standardise'' it
:- pred read_off(string, off_object, io__state, io__state).
:- mode read_off(in, out, di, uo) is det.

:- implementation.

:- import_module char, require, string, split_line, float, debug.

read_off(Filename, Object) -->
    (
        io__open_input(Filename, Result),
            (
                { Result = error(ErrorCode) },
                { io__error_message(ErrorCode, ErrorString) },
                { error(ErrorString) }
            ;
                { Result = ok(Stream) },
                io__set_input_stream(Stream, OldStream), % set input stream to the off file
                read_header(NumVertices, NumFaces),
                read_vertices(NumVertices, Vertices0),
                read_faces(NumFaces, Faces),
                { standardise(Vertices0, Vertices) },
                { Object = obj(Vertices, Faces) },
                io__set_input_stream(OldStream, _), % set input stream back to the scene file
                io__close_input(Stream)
            )
    ).

% read_header(NumVertices, NumFaces)
% read the off file header return the number of vertices and faces
:- pred read_header(int, int, io__state, io__state).
:- mode read_header(out, out, di, uo) is det.

read_header(NumVertices, NumFaces) -->
    (
        get_next_word(Word0),
            (
                % detect optional "OFF" keyword at start of file
                { Word0 = "OFF" }
            -> 
                get_next_word(Word1)
            ;
                { Word1 = Word0 }
            ),
            (
                { string__to_int(Word1, X1) }
            ->
                { NumVertices = X1 }
            ;
                io__input_stream_name(Filename),
                { string__append(Filename, ": Error reading OFF header", ErrorString) },
                { error(ErrorString) }
            ),
        get_next_word(Word2),
            (
                { string__to_int(Word2, X2) }
            ->
                { NumFaces = X2 }
            ;
                io__input_stream_name(Filename),
                { string__append(Filename, ": Error reading OFF file", ErrorString1) },
                { error(ErrorString1) }
            ),
        get_next_word(_)    % read in and ignore number of edges
    ).

% read in vertices
:- pred read_vertices(int, list(vec3), io__state, io__state).
:- mode read_vertices(in, out, di, uo).

read_vertices(N, L) -->
    (
        { N =< 0 }
    ->
        { L = [] }
    ;
        get_next_float(X),
        get_next_float(Y),
        get_next_float(Z),
        { V = vec(X, Y, Z) },
        read_vertices(N - 1, L1),
        { L = [V | L1] }
    ).

% read in next space-delimited word and convert to a float
:- pred get_next_float(float, io__state, io__state).
:- mode get_next_float(out, di, uo) is det.

get_next_float(F) -->
    (
        get_next_word(W),
            (
                { string__to_float(W, F1) }
            ->
                { F = F1 }
            ;
                io__input_stream_name(FileName),
                { string__append(FileName, ": Error reading OFF vertex list", ErrorString) },
                { error(ErrorString) }
            )
    ).

% get next space-delimited word from the OFF file, strip comments and return a
% string.  Call error/1 if string can't be read in.
:- pred get_next_word(string, io__state, io__state).
:- mode get_next_word(out, di, uo) is det.

get_next_word(Word) -->
        get_next_word0(CharList),
        { string__from_char_list(CharList, Word) }.

:- pred get_next_word0(list(char), io__state, io__state).
:- mode get_next_word0(out, di, uo) is det.

get_next_word0(L) -->
    (
        io__read_word(Result),
            (
                { Result = eof },
                io__input_stream_name(Filename),
                { string__append(Filename, ": OFF file too short", ErrorString) },
                { error(ErrorString) }
            ;
                { Result = error(ErrorCode) },
                { io__error_message(ErrorCode, ErrorString1) },
                { error(ErrorString1) }
            ;
                { Result = ok(L0) },
                strip_comments_from_word(L0, L1),
                    (
                        { L1 = [] }
                    -> 
                        get_next_word0(L)
                    ;
                        { L = L1 }
                    )
            )
    ).

:- pred strip_comments_from_word(list(char), list(char), io__state, io__state).
:- mode strip_comments_from_word(in, out, di, uo) is det.

strip_comments_from_word(Lin, Lout) -->
    (
        { Lin = [] },
        { Lout = [] }
    ;
        { Lin = [C | L0] },
            (
                { C = '#'}
            ->
                { Lout = [] },
                io__read_line(_) % discard rest of line
            ;
                strip_comments_from_word(L0, L1),
                { Lout = [C | L1] }
            )
    ).
            
% read_faces(NumFaces, Faces)
% read in the faces from the OFF file
% each line of the file contains 1 face
:- pred read_faces(int, list(off_face), io__state, io__state).
:- mode read_faces(in, out, di, uo) is det.

read_faces(N, Fs) -->
    (
        { N =< 0}
    ->
        { Fs = [] }
    ;
        io__read_line(Result),
            (   
                { Result = eof },
                io__input_stream_name(Filename),
                { string__append(Filename, ": OFF file too short", ErrorString) },
                { error(ErrorString) }
            ;
                { Result = error(ErrorCode) },
                { io__error_message(ErrorCode, ErrorString1) },
                { error(ErrorString1) }
            ;
                { Result = ok(L0) },
                io__input_stream_name(Filename),
                { strip_comments_from_line(L0, L1) },
                { split_line(L1, Words0) },
                    (
                        % check for a blank line
                        { Words0 = [] },
                        read_faces(N, Fs)
                    ;
                        { Words0 = [W1 | Words1] },
                        %{ dump("off.m:read_faces W1 **%s**", [s(W1)]) }, %% dmo
                            (
                                { W1 = ""}
                            ->
                                read_faces(N, Fs)
                            ;
                                (
                                    { string__to_int(W1, N1) }
                                ->
                                    { NumVerts = N1 },
                                    %{ dump("off.m:read_faces about to call get_vertex_list for %d vertices\n", [i(NumVerts)]) }, %dmo
                                    { get_vertex_list(NumVerts, Filename, VertList, Words1, Words2) },
                                    { get_colour(Words2, Colour) },
                                    { P = face(NumVerts, VertList, Colour) },
                                    read_faces(N - 1, Fs1),
                                    { Fs = [P | Fs1] }
                                ;
                                    { string__append(Filename, ": Error reading OFF faces", ErrorString2) },
                                    { error(ErrorString2) }
                                )
                            )
                    )
            )
    ).

% strip comments from a line of the file (list of chars)
:- pred strip_comments_from_line(list(char), list(char)).
:- mode strip_comments_from_line(in, out) is det.

strip_comments_from_line(Lin, Lout) :-
    (
        Lin = [],
        Lout = []
    ;
        Lin = [C | L1],
            (
                C = '#'
            ->
                Lout = []
            ;
                strip_comments_from_line(L1, L2),
                Lout = [C | L2]
            )
    ).

% get the list of vertex indices for a face
% filename is passed in for error reporting
:- pred get_vertex_list(int, string, list(int), list(string), list(string)).
:- mode get_vertex_list(in, in, out, in, out) is det.

get_vertex_list(N, Filename, Vs, WordsIn, WordsOut) :-
    (
        N =< 0
    ->
        Vs = [],
        WordsOut = WordsIn
    ;
        (
            WordsIn = [],
            string__append(Filename, ": Error reading OFF faces", ErrorString),
            error(ErrorString)
        ;
            WordsIn = [W | Ws1],
                (
                    string__to_int(W, I)
                ->
                    %dump("off.m:get_vertex_list: found vertex %d\n", [i(I)]), %dmo
                    get_vertex_list(N - 1, Filename, Vs1, Ws1, WordsOut),
                    Vs = [I | Vs1]
                ;
                    string__append(Filename, ": Error reading OFF faces", ErrorString1),
                    error(ErrorString1)
                )
        )
    ).
                    

% get colour for face
:- pred get_colour(list(string), colour).
:- mode get_colour(in, out) is det.

get_colour(Words, Colour) :-
    (
        Words = [W1, W2, W3 | _]
    ->
        (
            string__to_int(W1, I1), 0 =< I1, I1 =< 255,
            string__to_int(W2, I2), 0 =< I2, I2 =< 255,
            string__to_int(W3, I3), 0 =< I3, I3 =< 255
        ->
            int__to_float(I1, F1a),
            int__to_float(I2, F2a),
            int__to_float(I3, F3a),
            Colour = colour(F1a/255.0, F2a/255.0, F3a/255.0)
        ;
            string__to_float(W1, F1), 0.0 =< F1, F1 =< 1.0,
            string__to_float(W2, F2), 0.0 =< F2, F2 =< 1.0,
            string__to_float(W3, F3), 0.0 =< F3, F3 =< 1.0
        ->
            Colour = colour(F1, F2, F3)
        ;
            % if can't convert properly, use default colour
            Colour = default_colour    
        )
    ;
        Words = [W1 | _]
    ->
        % if a single number is given, try to use it as a greyscale value
        (
            string__to_int(W1, I1), 0 =< I1, I1 =< 255
        ->
            int__to_float(I1, F1a),
            F1b = F1a/255.0,
            Colour = colour(F1b, F1b, F1b)
        ;
            string__to_float(W1, F1), 0.0 =< F1, F1 =< 1.0
        ->
            Colour = colour(F1, F1, F1)
        ;
            Colour = default_colour
        )
    ;
        Colour = default_colour
    ).

% transform the object into standard form.  IE transform to fit into a 1x1 cube
% centred at the origin.
:- pred standardise(list(vec3), list(vec3)).
:- mode standardise(in, out) is det.

standardise(VLin, VLout) :-
    (
        get_min_max(VLin, MinX, MaxX, MinY, MaxY, MinZ, MaxZ),
        % dump("get_min_max results: %f %f %f %f %f %f\n", [f(MinX), f(MaxX), f(MinY), f(MaxY), f(MinZ), f(MaxZ)]), % debug - dmo
        Max1 = float__max(MaxX - MinX, MaxY - MinY),
        MaxExtent = float__max(Max1, MaxZ - MinZ),
        veclist_translate(VLin, vec(-(MinX+MaxX)/2.0, -(MinY+MaxY)/2.0, -(MinZ+MaxZ)/2.0), VL1),
        ( MaxExtent = 0.0 -> error("Max Extent is 0!!") ; true ),
        veclist_scale(VL1, 1.0/MaxExtent, VLout)
    ).

% find min and max extents in each direction
:- pred get_min_max(list(vec3), float, float, float, float, float, float).
:- mode get_min_max(in, out, out, out, out, out, out) is det.

get_min_max(VLin, MinX, MaxX, MinY, MaxY, MinZ, MaxZ) :-
    (
        VLin = [], 
        MinX = float__max,
        MinY = float__max,
        MinZ = float__max,
        MaxX = float__min,
        MaxY = float__min,
        MaxZ = float__min
    ;
        VLin = [vec(X, Y, Z) | VL1],
        %dump("get_min_max: current vector %f %f %f\n", [f(X), f(Y), f(Z)]),  % debug - dmo
        get_min_max(VL1, MinX1, MaxX1, MinY1, MaxY1, MinZ1, MaxZ1),
        MinX = float__min(X, MinX1),
        MinY = float__min(Y, MinY1),
        MinZ = float__min(Z, MinZ1),
        MaxX = float__max(X, MaxX1),
        MaxY = float__max(Y, MaxY1),
        MaxZ = float__max(Z, MaxZ1)
    ).

% translate all vectors in list
:- pred veclist_translate(list(vec3), vec3, list(vec3)).
:- mode veclist_translate(in, in, out) is det.

veclist_translate(VLin, VT, VLout) :-
    (
        VLin = [],
        VLout = []
    ;
        VLin = [V1 | VL1],
        V2 = V1 + VT,
        veclist_translate(VL1, VT, VL2),
        VLout = [V2 | VL2]
    ).

% scale all vectors in list
:- pred veclist_scale(list(vec3), float, list(vec3)).
:- mode veclist_scale(in, in, out) is det.

veclist_scale(VLin, Scale, VLout) :-
    (
        VLin = [],
        VLout = []
    ;
        VLin = [V1 | VL1],
        V2 = scale(Scale, V1),
        veclist_scale(VL1, Scale, VL2),
        VLout = [V2 | VL2]
    ).
