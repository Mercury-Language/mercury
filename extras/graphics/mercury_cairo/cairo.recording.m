%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Peter Wang
%
% This sub-module provides recording surfaces.
%
%---------------------------------------------------------------------------%

:- module cairo.recording.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type recording_surface.

:- instance surface(recording_surface).

%---------------------------------------------------------------------------%

    % recording.have_recording_surface:
    % Succeeds if recording surfaces are supported by this implementation.
    %
:- pred have_recording_surface is semidet.

    % recording.create_surface(Content, MaybeExtents, Surface, !IO):
    % Surface is a new recording surface.
    % Throws a cairo.error/0 exception if the surface cannot be created.
    %
:- pred create_surface(content::in, maybe(rectangle_f)::in,
    recording_surface::out, io::di, io::uo) is det.

    % recording.ink_extents(Surface, X0, Y0, Width, Height):
    % Measures the extents of the operations stored within the recording
    % surface.
    %
:- pred ink_extents(recording_surface::in, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

    % recording.get_extents(Surface, MaybeExtents):
    % Get the extents of the recording surface.
    %
:- pred get_extents(recording_surface::in, maybe(rectangle_f)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include \"cairo.mh\"").

:- pragma foreign_type("C", recording_surface, "MCAIRO_surface *",
    [can_pass_as_mercury_type]).

:- instance surface(recording_surface) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_recording_surface,
    [promise_pure, will_not_call_mercury],
"
#if defined(CAIRO_HAS_RECORDING_SURFACE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%-----------------------------------------------------------------------------%
%
% Recording surface creation
%

create_surface(Content, MaybeExtents, Surface, !IO) :-
    (
        MaybeExtents = yes(rectangle_f(X, Y, W, H)),
        HaveExtents = yes
    ;
        MaybeExtents = no,
        HaveExtents = no,
        X = 0.0,
        Y = 0.0,
        W = 0.0,
        H = 0.0
    ),
    create_surface_2(Content, HaveExtents, X, Y, W, H, Supported, Status,
        Surface, !IO),
    (
        Supported = yes,
        ( if Status = status_success then
            true
        else
            throw(cairo.error("recording.create_surface/4", Status))
        )
    ;
        Supported = no,
        throw(cairo.unsupported_surface_error("recording"))
    ).

:- pred create_surface_2(content::in, bool::in, float::in, float::in,
    float::in, float::in, bool::out, cairo.status::out, recording_surface::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(Content::in, HaveExtents::in, X::in, Y::in,
        W::in, H::in, Supported::out, Status::out, Surface::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_RECORDING_SURFACE)

    cairo_surface_t     *raw_surface;

    Supported = MR_YES;
    if (HaveExtents) {
        cairo_rectangle_t rect = {X, Y, W, H};
        raw_surface = cairo_recording_surface_create(Content, &rect);
    } else {
        raw_surface = cairo_recording_surface_create(Content, NULL);
    }

    Status = cairo_surface_status(raw_surface);

    if (Status == CAIRO_STATUS_SUCCESS) {
        Surface = MR_GC_NEW_ATTRIB(MCAIRO_surface, MR_ALLOC_ID);
        Surface->mcairo_raw_surface = raw_surface;
        MR_GC_register_finalizer(Surface, MCAIRO_finalize_surface, 0);
    } else {
        Surface = NULL;
    }
#else
    Supported = MR_NO;
    Status = CAIRO_STATUS_SUCCESS;
    Surface = NULL;
#endif
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    ink_extents(Surface::in, X::out, Y::out, W::out, H::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_RECORDING_SURFACE)
    double  x, y, w, h;

    cairo_recording_surface_ink_extents(Surface->mcairo_raw_surface,
        &x, &y, &w, &h);
    X = x;
    Y = y;
    W = w;
    H = h;
#else
    MR_external_fatal_error(\"Mercury cairo\",
        \"Recording surfaces not supported by this installation\");
#endif
").

%---------------------------------------------------------------------------%

get_extents(Surface, MaybeExtents, !IO) :-
    get_extents_2(Surface, Ok, X, Y, W, H, !IO),
    (
        Ok = yes,
        MaybeExtents = yes(rectangle_f(X, Y, W, H))
    ;
        Ok = no,
        MaybeExtents = no
    ).

:- pred get_extents_2(recording_surface::in, bool::out, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_extents_2(Surface::in, Ok::out, X::out, Y::out, W::out, H::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_t   rect;

    if (cairo_recording_surface_get_extents(Surface->mcairo_raw_surface,
            &rect))
    {
        Ok = MR_YES;
        X = rect.x;
        Y = rect.y;
        W = rect.width;
        H = rect.height;
    } else {
        Ok = MR_NO;
        X = 0.0;
        Y = 0.0;
        W = 0.0;
        H = 0.0;
    }
").

%---------------------------------------------------------------------------%
:- end_module cairo.recording.
%---------------------------------------------------------------------------%
