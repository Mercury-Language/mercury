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
% This module provides SVG surfaces, which allow rendering to SVG documents.
%
%---------------------------------------------------------------------------%

:- module cairo.svg.
:- interface.

%---------------------------------------------------------------------------%

:- type svg_surface.

:- instance surface(svg_surface).

    % The version number of the SVG specification that a generated SVG file
    % will conform to.
    %
:- type svg_version
    --->    svg_version_1_1
            % Version 1.1 of the SVG specification.

    ;       svg_version_1_2.
            % Version 1.2 of the SVG specification.

%---------------------------------------------------------------------------%

    % Succeeds if SVG surfaces are supported by this implementation.
    %
:- pred have_svg_surface is semidet.

    % svg.create_surface(FileName, Width, Height, Surface, !IO):
    % Surface is an SVG surface of the specified Width and Height in points
    % to be written to FileName.
    %
:- pred create_surface(string::in, int::in, int::in, svg_surface::out,
    io::di, io::uo) is det.

% restrict_to_version
% get_versions

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "

#include \"cairo.mh\"

#if defined(CAIRO_HAS_SVG_SURFACE)
  #include <cairo-svg.h>
#else
  /* These are unlikely to change. */
  enum {
    CAIRO_SVG_VERSION_1_1,
    CAIRO_SVG_VERSION_1_2
  };
#endif

").

:- pragma foreign_type("C", svg_surface, "MCAIRO_surface *",
    [can_pass_as_mercury_type]).

:- instance surface(svg_surface) where [].

:- pragma foreign_enum("C", svg_version/0, [
    svg_version_1_1 - "CAIRO_SVG_VERSION_1_1",
    svg_version_1_2 - "CAIRO_SVG_VERSION_1_2"
]).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
   have_svg_surface,
   [promise_pure, will_not_call_mercury],
"
#if defined(CAIRO_HAS_SVG_SURFACE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%---------------------------------------------------------------------------%
%
% SVG surface creation
%

create_surface(FileName, Width, Height, Surface, !IO) :-
    create_surface_2(FileName, Width, Height, Supported, Status, Surface, !IO),
    (
        Supported = yes,
        ( if Status = status_success then
            true
        else
            throw(cairo.error("svg.create_surface/6", Status))
        )
    ;
        Supported = no,
        throw(cairo.unsupported_surface_error("SVG"))
    ).

:- pred create_surface_2(string::in, int::in, int::in, bool::out,
    cairo.status::out, svg_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(FileName::in, W::in, H::in,
        Supported::out, Status::out, Surface::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_SVG_SURFACE)

    cairo_surface_t     *raw_surface;

    Supported = MR_YES;
    raw_surface = cairo_svg_surface_create(FileName, (int)W, (int)H);
    Status = cairo_surface_status(raw_surface);

    if (Status == CAIRO_STATUS_SUCCESS) {
        Surface = MR_GC_NEW(MCAIRO_surface);
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
:- end_module cairo.svg.
%---------------------------------------------------------------------------%
