%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

#if defined(CAIRO_HAS_SVG_SURFACE)
  #include <cairo-svg.h>
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

:- type maybe_svg_surface
    --->    svg_surface_ok(svg_surface)
    ;       svg_surface_error(cairo.status)
    ;       svg_surface_unsupported.

:- pragma foreign_export("C", make_svg_surface_ok(in) = out,
    "MCAIRO_svg_surface_ok").
:- func make_svg_surface_ok(svg_surface) = maybe_svg_surface.

make_svg_surface_ok(Surface) = svg_surface_ok(Surface).

:- pragma foreign_export("C", make_svg_surface_error(in) = out,
    "MCAIRO_svg_surface_error").
:- func make_svg_surface_error(cairo.status) = maybe_svg_surface.

make_svg_surface_error(Status) = svg_surface_error(Status).

:- pragma foreign_export("C", make_svg_surface_unsupported = out,
    "MCAIRO_svg_surface_usupported").
:- func make_svg_surface_unsupported = maybe_svg_surface.

make_svg_surface_unsupported = svg_surface_unsupported.

create_surface(FileName, Height, Width, Surface, !IO) :-
    create_surface_2(FileName, Height, Width, MaybeSurface, !IO),
    (
        MaybeSurface = svg_surface_ok(Surface)
    ;
        MaybeSurface = svg_surface_error(ErrorStatus),
        throw(cairo.error("svg.create_surface/6", ErrorStatus))
    ;
        MaybeSurface = svg_surface_unsupported,
        throw(cairo.unsupported_surface_error("SVG"))
    ).    

:- pred create_surface_2(string::in, int::in, int::in, maybe_svg_surface::out,
	io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(FileName::in, H::in, W::in, MaybeSurface::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_SVG_SURFACE)

    MCAIRO_surface      *surface;
    cairo_surface_t		*raw_surface;
    cairo_status_t      status;

    raw_surface = cairo_svg_surface_create(FileName, (int)H, (int)W);
    status = cairo_surface_status(raw_surface);

    switch (status) {
        case CAIRO_STATUS_SUCCESS:
            surface = MR_GC_NEW(MCAIRO_surface);
            surface->mcairo_raw_surface = raw_surface;
            MR_GC_register_finalizer(surface, MCAIRO_finalize_surface, 0);
            MaybeSurface = MCAIRO_svg_surface_ok(surface);
            break;
        
        case CAIRO_STATUS_NULL_POINTER:
        case CAIRO_STATUS_NO_MEMORY:
        case CAIRO_STATUS_READ_ERROR:
        case CAIRO_STATUS_INVALID_CONTENT:
        case CAIRO_STATUS_INVALID_FORMAT:
        case CAIRO_STATUS_INVALID_VISUAL:
            MaybeSurface = MCAIRO_svg_surface_error(status);
            break;
        
        default:
            MR_fatal_error(\"cairo: unknown SVG surface status\");
    }
#else
    MaybeSurface = MCAIRO_svg_surface_unsupported();
#endif

").

%---------------------------------------------------------------------------%
:- end_module cairo.svg.
%---------------------------------------------------------------------------%
