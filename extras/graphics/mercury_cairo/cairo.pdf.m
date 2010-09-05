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
% This sub-module contains support for rendering PDF documents.
%
%---------------------------------------------------------------------------%

:- module cairo.pdf.
:- interface.

%---------------------------------------------------------------------------%

:- type pdf_surface.

:- instance surface(pdf_surface).

%---------------------------------------------------------------------------%

    % pdf.have_pdf_surface:
    % Succeeds if PDF surfaces are supported by this implementation.
    %
:- pred have_pdf_surface is semidet.

    % pdf.create_surface(FileName, Height, Width, Surface, !IO):
    % Surface is a PDF surface of the specified Height and Width in points
    % to be written to FileName. 
    % Throws an unsupported_surface_error/0 exception if PDF surfaces are
    % not supported by this implementation.  Throws a cairo.error/0 exception
    % if any other error occurs.
    %
:- pred create_surface(string::in, float::in, float::in, pdf_surface::out,
	io::di, io::uo) is det.

    % pdf.set_size(Surface, Height, Width, !IO):
    % Change the size of a PDF surface for the current (and subsequent) pages.
    %
:- pred set_size(pdf_surface::in, float/*height*/::in, float/*width*/::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "

#if defined(CAIRO_HAS_PDF_SURFACE)
  #include <cairo-pdf.h>
#endif

").

:- pragma foreign_type("C", pdf_surface, "MCAIRO_surface *",
	[can_pass_as_mercury_type]).

:- instance surface(pdf_surface) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
   have_pdf_surface,
   [promise_pure, will_not_call_mercury],
"
#if defined(CAIRO_HAS_PDF_SURFACE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
"). 

%---------------------------------------------------------------------------%
% 
% PDF surface creation
%

:- type maybe_pdf_surface
    --->    pdf_surface_ok(pdf_surface)
    ;       pdf_surface_error(cairo.status)
    ;       pdf_surface_unsupported.

:- pragma foreign_export("C", make_pdf_surface_ok(in) = out,
    "MCAIRO_pdf_surface_ok").
:- func make_pdf_surface_ok(pdf_surface) = maybe_pdf_surface.

make_pdf_surface_ok(Surface) = pdf_surface_ok(Surface).

:- pragma foreign_export("C", make_pdf_surface_error(in) = out,
    "MCAIRO_pdf_surface_error").
:- func make_pdf_surface_error(cairo.status) = maybe_pdf_surface.

make_pdf_surface_error(Status) = pdf_surface_error(Status).

:- pragma foreign_export("C", make_pdf_surface_unsupported = out,
    "MCAIRO_pdf_surface_unsupported").
:- func make_pdf_surface_unsupported = maybe_pdf_surface.

make_pdf_surface_unsupported = pdf_surface_unsupported.

create_surface(FileName, Height, Width, Surface, !IO) :-
    create_surface_2(FileName, Height, Width, MaybeSurface, !IO),
    (
        MaybeSurface = pdf_surface_ok(Surface)
    ;
        MaybeSurface = pdf_surface_error(ErrorStatus),
        throw(cairo.error("pdf.create_surface/6", ErrorStatus))
    ;
        MaybeSurface = pdf_surface_unsupported,
        throw(cairo.unsupported_surface_error("PDF"))
    ).

:- pred create_surface_2(string::in,
	float::in, float::in, maybe_pdf_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
	create_surface_2(FileName::in, H::in, W::in, MaybeSurface::out,
		_IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PDF_SURFACE)

    MCAIRO_surface      *surface;
    cairo_surface_t		*raw_surface;
    cairo_status_t      status;

    raw_surface = cairo_pdf_surface_create(FileName, H, W);
    status = cairo_surface_status(raw_surface);

    switch (status) {
        case CAIRO_STATUS_SUCCESS:
            surface = MR_GC_NEW(MCAIRO_surface);
            surface->mcairo_raw_surface = raw_surface;
            MR_GC_register_finalizer(surface, MCAIRO_finalize_surface, 0);
            MaybeSurface = MCAIRO_pdf_surface_ok(surface);
            break;

        case CAIRO_STATUS_NULL_POINTER:
        case CAIRO_STATUS_NO_MEMORY:
        case CAIRO_STATUS_READ_ERROR:
        case CAIRO_STATUS_INVALID_CONTENT:
        case CAIRO_STATUS_INVALID_FORMAT:
        case CAIRO_STATUS_INVALID_VISUAL:
            MaybeSurface = MCAIRO_pdf_surface_error(status);
            break;

        default:
            MR_fatal_error(\"cairo: unknown PDF surface status\");
    }
#else
    MaybeSurface = MCAIRO_pdf_surface_unsupported();
#endif
").

:- pragma foreign_proc("C",
    set_size(Surface::in, Height::in, Width::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PDF_SURFACE)
    cairo_pdf_surface_set_size(Surface->mcairo_raw_surface,
        Height, Width);
#else
    MR_fatal_error(\"Cairo PDF surface not available\");
#endif
").

%---------------------------------------------------------------------------%
:- end_module cairo.pdf.
%---------------------------------------------------------------------------%
