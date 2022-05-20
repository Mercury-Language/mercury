%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module provides image surface, which allow rendering to memory
% buffers.
%
%---------------------------------------------------------------------------%

:- module cairo.image.
:- interface.

%---------------------------------------------------------------------------%

:- type image_surface.

:- instance surface(image_surface).

%---------------------------------------------------------------------------%

    % image.create_surface(Format, Width, Height, Surface, !IO):
    % Surface is a new image surface.
    % Throws a cairo.error/0 exception if the surface cannot be created.
    %
:- pred create_surface(format::in, int::in, int::in, image_surface::out,
    io::di, io::uo) is det.

    % image.get_format(Surface, Format, !IO):
    % Format is the pixel format for Surface.
    %
:- pred get_format(image_surface::in, format::out, io::di, io::uo) is det.

    % image.get_width(Surface, Width, !IO):
    % Width is the width of Surface (in pixels).
    %
:- pred get_width(image_surface::in, int::out, io::di, io::uo) is det.

    % image.get_height(Surface, Height, !IO):
    % Height is the height of Surface (in pixels).
    %
:- pred get_height(image_surface::in, int::out, io::di, io::uo) is det.

    % image.get_stride(Surface, Stride, !IO):
    % Stride is the stride of Surface (in bytes).
    %
:- pred get_stride(image_surface::in, int::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include \"cairo.mh\"").

:- pragma foreign_type("C", image_surface, "MCAIRO_surface *",
    [can_pass_as_mercury_type]).

:- instance surface(image_surface) where [].

%-----------------------------------------------------------------------------%
%
% Image surface creation
%

create_surface(Format, Width, Height, Surface, !IO) :-
    create_surface_2(Format, Width, Height, Status, Surface, !IO),
    ( if Status = status_success then
        true
    else
        throw(cairo.error("image.create_surface/6", Status))
    ).

:- pred create_surface_2(format::in, int::in, int::in, cairo.status::out,
    image_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(Fmt::in, W::in, H::in, Status::out, Surface::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_surface_t     *raw_surface;

    raw_surface = cairo_image_surface_create((cairo_format_t)Fmt,
        (int)W, (int)H);
    Status = cairo_surface_status(raw_surface);

    if (Status == CAIRO_STATUS_SUCCESS) {
        Surface = MR_GC_NEW_ATTRIB(MCAIRO_surface, MR_ALLOC_ID);
        Surface->mcairo_raw_surface = raw_surface;
        MR_GC_register_finalizer(Surface, MCAIRO_finalize_surface, 0);
    } else {
        Surface = NULL;
    }
").

:- pragma foreign_proc("C",
    get_format(Surface::in, Fmt::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Fmt = cairo_image_surface_get_format(Surface->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    get_width(Surface::in, Width::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Width = cairo_image_surface_get_width(Surface->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    get_height(Surface::in, Height::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Height = cairo_image_surface_get_height(Surface->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    get_stride(Surface::in, Stride::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Stride = cairo_image_surface_get_stride(Surface->mcairo_raw_surface);
").

%---------------------------------------------------------------------------%
:- end_module cairo.image.
%---------------------------------------------------------------------------%
