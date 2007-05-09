%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.math3d.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.math3d.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type matrix_f.

:- func identity_matrix_f = matrix_f.
:- pred get_translation_matrix_f(matrix_f::out, float::in, float::in,
	float::in) is det.
:- pred get_scaling_matrix_f(matrix_f::out, float::in, float::in, float::in)
	is det.
:- pred get_x_rotate_matrix_f(matrix_f::out, float::in) is det.
:- pred get_y_rotate_matrix_f(matrix_f::out, float::in) is det.
:- pred get_z_rotate_matrix_f(matrix_f::out, float::in) is det.
:- pred get_rotation_matrix_f(matrix_f::out, float::in, float::in, float::in)
	is det.
:- pred get_align_matrix_f(matrix_f::out, float::in, float::in, float::in, 
	float::in, float::in, float::in) is det.
:- pred get_vector_rotation_matrix_f(matrix_f::out,
	float::in, float::in, float::in, float::in) is det.
:- pred get_transformation_matrix_f(matrix_f::out, float::in, 
	float::in, float::in, float::in, 
	float::in, float::in, float::in) is det.
:- pred get_camera_matrix_f(matrix_f::out, 
	float::in, float::in, float::in, 
	float::in, float::in, float::in, 
	float::in, float::in, float::in, 
	float::in, float::in) is det.
:- pred qtranslate_matrix_f(matrix_f::in, float::in, float::in, float::in,
	matrix_f::out) is det.
:- pred qscale_matrix_f(matrix_f::in, float::in, matrix_f::out) is det.
:- pred matrix_mul_f(matrix_f::in, matrix_f::in, matrix_f::out) is det.
:- func vector_length_f(float, float, float) = float.
:- pred normalize_vector_f(float::in, float::in, float::in,
	float::out, float::out, float::out) is det.
:- func dot_product_f(float, float, float, float, float, float) = float
	is det.
:- pred cross_product_f(float::in, float::in, float::in,
	float::in, float::in, float::in,
	float::out, float::out, float::out) is det.
% polygon_z_normal_f
:- pred apply_matrix_f(matrix_f::in, float::in, float::in, float::in,
	float::out, float::out, float::out) is det.
:- pred set_projection_viewport(int::in, int::in, int::in, int::in,
	io::di, io::uo) is det.
:- pred persp_project(float::in, float::in, float::in, float::out, float::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", matrix_f, "MATRIX_f *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    identity_matrix_f = (Matrix::out),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    (*Matrix) = identity_matrix_f;
").

:- pragma foreign_proc("C",
    get_translation_matrix_f(Matrix::out, X::in, Y::in, Z::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_translation_matrix_f(Matrix, X, Y, Z);
").

:- pragma foreign_proc("C",
    get_scaling_matrix_f(Matrix::out, X::in, Y::in, Z::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_scaling_matrix_f(Matrix, X, Y, Z);
").

:- pragma foreign_proc("C",
    get_x_rotate_matrix_f(Matrix::out, R::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_x_rotate_matrix_f(Matrix, R);
").

:- pragma foreign_proc("C",
    get_y_rotate_matrix_f(Matrix::out, R::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_y_rotate_matrix_f(Matrix, R);
").

:- pragma foreign_proc("C",
    get_z_rotate_matrix_f(Matrix::out, R::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_z_rotate_matrix_f(Matrix, R);
").

:- pragma foreign_proc("C",
    get_rotation_matrix_f(Matrix::out, X::in, Y::in, Z::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_rotation_matrix_f(Matrix, X, Y, Z);
").

:- pragma foreign_proc("C",
    get_align_matrix_f(Matrix::out, XFront::in, YFront::in, ZFront::in,
	XUp::in, YUp::in, ZUp::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_align_matrix_f(Matrix, XFront, YFront, ZFront, XUp, YUp, ZUp);
").

:- pragma foreign_proc("C",
    get_vector_rotation_matrix_f(Matrix::out, X::in, Y::in, Z::in, A::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_vector_rotation_matrix_f(Matrix, X, Y, Z, A);
").

:- pragma foreign_proc("C",
    get_transformation_matrix_f(Matrix::out, Scale::in,
	XRot::in, YRot::in, ZRot::in, X::in, Y::in, Z::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_transformation_matrix_f(Matrix, Scale, XRot, YRot, ZRot, X, Y, Z);
").

:- pragma foreign_proc("C",
    get_camera_matrix_f(Matrix::out, 
	X::in, Y::in, Z::in, 
	XFront::in, YFront::in, ZFront::in, 
	XUp::in, YUp::in, ZUp::in, 
	FOV::in, Aspect::in),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    get_camera_matrix_f(Matrix, X, Y, Z, XFront, YFront, ZFront, XUp, YUp, ZUp,
	FOV, Aspect);
").

:- pragma foreign_proc("C",
    qtranslate_matrix_f(Matrix0::in, X::in, Y::in, Z::in, Matrix::out),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    (*Matrix) = (*Matrix0);
    qtranslate_matrix_f(Matrix, X, Y, Z);
").

:- pragma foreign_proc("C",
    qscale_matrix_f(Matrix0::in, Scale::in, Matrix::out),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    (*Matrix) = (*Matrix0);
    qscale_matrix_f(Matrix, Scale);
").

:- pragma foreign_proc("C",
    matrix_mul_f(MA::in, MB::in, Matrix::out),
    [will_not_call_mercury, promise_pure],
"
    Matrix = MR_NEW(MATRIX_f);
    matrix_mul_f(MA, MB, Matrix);
").

:- pragma foreign_proc("C",
    vector_length_f(X::in, Y::in, Z::in) = (Length::out),
    [will_not_call_mercury, promise_pure],
"
    Length = vector_length_f(X, Y, Z);
").

:- pragma foreign_proc("C",
    normalize_vector_f(X0::in, Y0::in, Z0::in, X::out, Y::out, Z::out),
    [will_not_call_mercury, promise_pure],
"
    float X1 = X0;
    float Y1 = Y0;
    float Z1 = Z0;
    normalize_vector_f(&X1, &Y1, &Z1);
    X = X1;
    Y = Y1;
    Z = Z1;
").

:- pragma foreign_proc("C",
    dot_product_f(X1::in, Y1::in, Z1::in, X2::in, Y2::in, Z2::in) = (DP::out),
    [will_not_call_mercury, promise_pure],
"
    DP = dot_product_f(X1, Y1, Z1, X2, Y2, Z2);
").

:- pragma foreign_proc("C",
    cross_product_f(X1::in, Y1::in, Z1::in, X2::in, Y2::in, Z2::in,
	X::out, Y::out, Z::out),
    [will_not_call_mercury, promise_pure],
"
    float X3, Y3, Z3;
    cross_product_f(X1, Y1, Z1, X2, Y2, Z2, &X3, &Y3, &Z3);
    X = X3;
    Y = Y3;
    Z = Z3;
").

:- pragma foreign_proc("C",
    apply_matrix_f(Matrix::in, X0::in, Y0::in, Z0::in, X::out, Y::out, Z::out),
    [will_not_call_mercury, promise_pure],
"
    float X1, Y1, Z1;
    apply_matrix_f(Matrix, X0, Y0, Z0, &X1, &Y1, &Z1);
    X = X1;
    Y = Y1;
    Z = Z1;
").

:- pragma foreign_proc("C",
    set_projection_viewport(X::in, Y::in, W::in, H::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_projection_viewport(X, Y, W, H);
    IO = IO0;
").

:- pragma foreign_proc("C",
    persp_project(X::in, Y::in, Z::in, XOut::out, YOut::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    float XOutf, YOutf;
    persp_project_f(X, Y, Z, &XOutf, &YOutf);
    XOut = XOutf;
    YOut = YOutf;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
