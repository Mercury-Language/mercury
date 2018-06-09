%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au.
%
% This module contains predicates that perform various generic matrix
% operations on cairo transformation matrices.
%
%---------------------------------------------------------------------------%

:- module cairo.matrix.
:- interface.

%---------------------------------------------------------------------------%

    % matrix.init(XX, YX, XY, YY, X0, Y0, Matrix, !IO):
    % Matrix is the affine transformation given by:
    %
    %     <x_new> = XX * <x> + XY * <y> + X0
    %     <y_new> = YX * <x> + YY * <y> +Y0
    %
:- pred init(float::in, float::in, float::in, float::in,
    float::in, float::in, matrix::out, io::di, io::uo) is det.

    % matrix.init_identity(Matrix, !IO):
    % Matrix is the identity transformation.
    %
:- pred init_identity(matrix::out, io::di, io::uo) is det.

    % matrix.init_translate(Tx, Ty, Matrix, !IO):
    % Matrix is a transformation that translates by Tx and Ty in the X and Y
    % directions respectively.
    %
:- pred init_translate(float::in, float::in, matrix::out, io::di, io::uo)
   is det.

    % matrix.init_scale(Sx, Sy, Matrix, !IO):
    % Matrix is a transformation that scales by Sx and Sy in the X and Y
    % directions respectively.
    %
:- pred init_scale(float::in, float::in, matrix::out, io::di, io::uo)
   is det.

    % matrix.init_rotate(R, Matrix, !IO):
    % Matrix is a transformation that rotates by R radians.
    % The direction of rotation is defined such that positive angles rotate
    % in the direction from the positive X axis toward the positive Y axis.
    % With the default axis orientation of cairo, positive angles rotate
    % in a clockwise direction.
    %
:- pred init_rotate(float::in, matrix::out, io::di, io::uo) is det.

    % matrix.translate(Matrix, Tx, Ty, !IO):
    % Apply a translation by Tx, Ty to the transformation in Matrix.
    %
:- pred translate(matrix::in, float::in, float::in, io::di, io::uo) is det.

    % matrix.scale(Matrix, Sx, Sy, !IO):
    % Apply scaling by Sx, Sy to the transformation in Matrix.
    %
:- pred scale(matrix::in, float::in, float::in, io::di, io::uo) is det.

    % matrix.rotate(Matrix, R, !IO):
    % Apply a rotation by R radians to the transformation in Matrix.
    %
:- pred rotate(matrix::in, float::in, io::di, io::uo) is det.

    % matrix.invert(Matrix, !IO):
    % Update Matrix to be the inverse of its original value.
    % Throws a cairo.error/0 exception if Matrix is not invertible.
    %
:- pred invert(matrix::in, io::di, io::uo) is det.

    % matrix.invert(Matrix, HasInverse, !IO):
    % If Matrix has an inverse then HasInverse = yes and update Matrix to its
    % inverse. Otherwise HasInverse = no.
    %
:- pred invert(matrix::in, bool::out, io::di, io::uo) is det.

    % matrix.multiply(Result, A, B, !IO):
    % Update Result to be the product of the affine transformations in A and B.
    %
:- pred multiply(matrix::in, matrix::in, matrix::in, io::di, io::uo) is det.

    % matrix.unpack(Matrix, XX, YX, XY, YY, X0, Y0, !IO):
    % Return the six components of Matrix.
    %
:- pred unpack(matrix::in, float::out, float::out, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

    % matrix.transform_distance(Matrix, Dx0, Dy0, Dx, Dy, !IO):
    % Transforms the distance vector (Dx0, Dy0) by Matrix.
    %
:- pred transform_distance(matrix::in, float::in, float::in,
     float::out, float::out, io::di, io::uo) is det.

    % matrix.transform_point(Matrix, X0, Y0, X, Y, !IO):
    % (X, Y) is the point (X0, Y0) transformed by Matrix.
    %
:- pred transform_point(matrix::in, float::in, float::in,
     float::out, float::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    init(Xx::in, Yx::in, Xy::in, Yy::in, X0::in, Y0::in,
        Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *new_matrix;

    new_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_matrix_init(new_matrix, Xx, Yx, Xy, Yy, X0, Y0);
    Matrix = new_matrix;
").

:- pragma foreign_proc("C",
    init_identity(Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *new_matrix;

    new_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_matrix_init_identity(new_matrix);
    Matrix = new_matrix;
").

:- pragma foreign_proc("C",
    init_translate(Tx::in, Ty::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *new_matrix;

    new_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_matrix_init_translate(new_matrix, Tx, Ty);
    Matrix = new_matrix;
").

:- pragma foreign_proc("C",
    init_scale(Sx::in, Sy::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *new_matrix;

    new_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_matrix_init_scale(new_matrix, Sx, Sy);
    Matrix = new_matrix;
").

:- pragma foreign_proc("C",
    init_rotate(Radians::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *new_matrix;

    new_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_matrix_init_rotate(new_matrix, Radians);
    Matrix = new_matrix;
").

:- pragma foreign_proc("C",
    translate(Matrix::in, Tx::in, Ty::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_translate(Matrix, Tx, Ty);
").

:- pragma foreign_proc("C",
    scale(Matrix::in, Sx::in, Sy::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_matrix_scale(Matrix, Sx, Sy);
").

:- pragma foreign_proc("C",
   rotate(Matrix::in, Radians::in, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury],
"
    cairo_matrix_rotate(Matrix, Radians);
").

invert(Matrix, !IO) :-
    invert(Matrix, IsValid, !IO),
    (
        IsValid = yes
    ;
        IsValid = no,
        throw(cairo.error("invert/3", status_invalid_matrix))
    ).

:- pragma foreign_proc("C",
   invert(Matrix::in, IsValid::out, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury],
"
   if (cairo_matrix_invert(Matrix) == CAIRO_STATUS_SUCCESS) {
       IsValid = MR_YES;
   } else {
       IsValid = MR_NO;
   }
").

:- pragma foreign_proc("C",
   multiply(Result::in, A::in, B::in, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury],
"
    cairo_matrix_multiply(Result, A, B);
").

:- pragma foreign_proc("C",
    unpack(Matrix::in, Xx::out, Yx::out, Xy::out, Yy::out, X0::out, Y0::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Xx = Matrix->xx;
    Yx = Matrix->yx;
    Xy = Matrix->xy;
    Yy = Matrix->yy;
    X0 = Matrix->x0;
    Y0 = Matrix->y0;
").

:- pragma foreign_proc("C",
   transform_distance(Matrix::in, Dx0::in, Dy0::in, Dx::out, Dy::out,
       _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury],
"
    double dx;
    double dy;

    dx = Dx0;
    dy = Dy0;

    cairo_matrix_transform_distance(Matrix, &dx, &dy);
    Dx = dx;
    Dy = dy;
").

:- pragma foreign_proc("C",
   transform_point(Matrix::in, X0::in, Y0::in, X::out, Y::out,
       _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury],
"
    double x;
    double y;

    x = X0;
    y = Y0;

    cairo_matrix_transform_point(Matrix, &x, &y);
    X = x;
    Y = y;
").

%---------------------------------------------------------------------------%
:- end_module cairo.matrix.
%---------------------------------------------------------------------------%
