%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% The predicates in this sub-module manipulate a cairo context's current
% transformation matrix.
%
%-----------------------------------------------------------------------------%

:- module cairo.transformations.
:- interface.

%-----------------------------------------------------------------------------%

    % transformations.translate(Context, Tx, Ty, !IO):
    % Modifies the current transformation matrix for Context by translating the
    % user space origin by (Tx, Ty).
    %
:- pred translate(context(T)::in, float::in, float::in, io::di, io::uo) is det.

    % transformations.scale(Context, Sx, Sy, !IO):
    % Modifies the current transformation matrix for Context by scaling the X
    % and Y user space axes by Sx and Sy respectively.
    %
:- pred scale(context(T)::in, float::in, float::in, io::di, io::uo) is det.

    % transformations.rotate(Context, Angle, !IO):
    % Modifies the current transformation matrix for context by rotating the
    % user space axes by Angle radians.
    %
:- pred rotate(context(T)::in, float::in, io::di, io::uo) is det.

    % transformations.transform(Context, Matrix, !IO):
    % Modifies the current transformation matrix for Context by applying
    % Matrix as an additional transformation.
    % The new transformation of user space takes place after any existing
    % transformation.
    %
:- pred transform(context(T)::in, matrix::in, io::di, io::uo) is det.

    % transformations.set_matrix(Context, Matrix, !IO):
    % Set the current transformation matrix for Context to Matrix.
    %
:- pred set_matrix(context(T)::in, matrix::in, io::di, io::uo) is det.

    % transformations.get_matrix(Context, Matrix, !IO):
    % Matrix is the current transformation matrix for Context.
    %
:- pred get_matrix(context(T)::in, matrix::out, io::di, io::uo) is det.

    % transformations.identity_matrix(Context, !IO):
    % Reset the current transformation matrix for Context by setting
    % it equal to the identity matrix.
    %
:- pred identity_matrix(context(T)::in, io::di, io::uo) is det.

    % transformations.user_to_device(Context, X_usr, Y_usr,
    %   X_dev, Y_dev, !IO):
    %
    % Transform a coordinate from user space to device space.
    %
:- pred user_to_device(context(T)::in, float::in, float::in,
    float::out, float::out, io::di, io::uo) is det.

    % transformations.user_to_device_distance(Context, DX_usr, DY_usr,
    %   DX_dev, DY_dev, !IO):
    %
    % Transform a distance vector from device space to user space.
    %
:- pred user_to_device_distance(context(T)::in, float::in, float::in,
    float::out, float::out, io::di, io::uo) is det.

    % transformations.device_to_user(Context, X_dev, Y_dev,
    %   X_usr, Y_usr, !IO):
    %
    % Transform a coordinate from device space to user space.
    %
:- pred device_to_user(context(T)::in, float::in, float::in,
    float::out, float::out, io::di, io::uo) is det.

    % transformations.device_to_user_distance(Context, DX_dev, DY_dev,
    %   DX_usr, DY_usr, !IO):
    %
    % Transform a distance vector from device space to user space.
    %
:- pred device_to_user_distance(context(T)::in, float::in, float::in,
    float::out, float::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    translate(Ctxt::in, Tx::in, Ty::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_translate(Ctxt->mcairo_raw_context, Tx, Ty);
").

:- pragma foreign_proc("C",
    scale(Ctxt::in, Sx::in, Sy::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_scale(Ctxt->mcairo_raw_context, Sx, Sy);
").

:- pragma foreign_proc("C",
    rotate(Ctxt::in, Angle::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rotate(Ctxt->mcairo_raw_context, Angle);
").

:- pragma foreign_proc("C",
    transform(Ctxt::in, Matrix::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_transform(Ctxt->mcairo_raw_context, Matrix);
").

:- pragma foreign_proc("C",
    set_matrix(Ctxt::in, Matrix::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_set_matrix(Ctxt->mcairo_raw_context, Matrix);
").

:- pragma foreign_proc("C",
    get_matrix(Ctxt::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_get_matrix(Ctxt->mcairo_raw_context, Matrix);
").

:- pragma foreign_proc("C",
    identity_matrix(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_identity_matrix(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    user_to_device(Ctxt::in, Ux::in, Uy::in, Dx::out, Dy::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x;
    double  y;

    x = Ux;
    y = Uy;
    cairo_user_to_device(Ctxt->mcairo_raw_context, &x, &y);
    Dx = x;
    Dy = y;
").

:- pragma foreign_proc("C",
    user_to_device_distance(Ctxt::in, Ux::in, Uy::in, Dx::out, Dy::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x;
    double  y;

    x = Ux;
    y = Uy;
    cairo_user_to_device_distance(Ctxt->mcairo_raw_context, &x, &y);
    Dx = x;
    Dy = y;
").

:- pragma foreign_proc("C",
    device_to_user(Ctxt::in, Dx::in, Dy::in, Ux::out, Uy::out,
       _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x;
    double  y;

    x = Dx;
    y = Dy;
    cairo_device_to_user(Ctxt->mcairo_raw_context, &x, &y);
    Ux = x;
    Uy = y;
").

:- pragma foreign_proc("C",
    device_to_user_distance(Ctxt::in, Dx::in, Dy::in, Ux::out, Uy::out,
       _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x;
    double  y;

    x = Dx;
    y = Dy;
    cairo_device_to_user_distance(Ctxt->mcairo_raw_context, &x, &y);
    Ux = x;
    Uy = y;
").

%---------------------------------------------------------------------------%
:- end_module cairo.transformations.
%---------------------------------------------------------------------------%
