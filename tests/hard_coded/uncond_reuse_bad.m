%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bad unconditional direct structure reuse.

:- module uncond_reuse_bad.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.

:- type bounding_box
    --->    bb(vector, vector).

:- type vector
    --->    point(float, float, float).

:- type something       % == sizeof(vector)
    --->    something(
                bounding_box,
                float,
                int
            ).

main(!IO) :-
    BBox = make_bb,
    % Created in here for unconditional direct reuse.

    BBox = bb(Min, _Max),
    Min = point(X1, _Y1, _Z1),

    io.write_string("BBox before: ", !IO),
    io.write(BBox, !IO),
    io.nl(!IO),

    Something = something(BBox, X1, 100),
    % Bad reuse of Min occurs here.

    io.write_string("BBox after: ", !IO),
    io.write(BBox, !IO),
    io.nl(!IO),

    io.write(Something, !IO),
    io.nl(!IO).

:- func make_bb = bounding_box.
:- pragma no_inline(make_bb/0).

make_bb = BBox :-
    Orig = bb(point(-2.25, -5.0, -1.5), point(3.3, 3.6, 9.3)),
    copy(Orig, BBox).
