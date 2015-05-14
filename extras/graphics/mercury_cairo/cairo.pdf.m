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

#include \"cairo.mh\"

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

create_surface(FileName, Height, Width, Surface, !IO) :-
    create_surface_2(FileName, Height, Width, Supported, Status, Surface, !IO),
    (
        Supported = yes,
        ( Status = status_success ->
            true
        ;
            throw(cairo.error("svg.create_surface/6", Status))
        )
    ;
        Supported = no,
        throw(cairo.unsupported_surface_error("PDF"))
    ).

:- pred create_surface_2(string::in, float::in, float::in,
    bool::out, cairo.status::out, pdf_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
	create_surface_2(FileName::in, H::in, W::in,
        Supported::out, Status::out, Surface::out, _IO0::di, _IO::uo),
	[promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PDF_SURFACE)

    cairo_surface_t		*raw_surface;

    Supported = MR_YES;
    raw_surface = cairo_pdf_surface_create(FileName, H, W);
    Status = cairo_surface_status(raw_surface);

    switch (Status) {
        case CAIRO_STATUS_SUCCESS:
            Surface = MR_GC_NEW(MCAIRO_surface);
            Surface->mcairo_raw_surface = raw_surface;
            MR_GC_register_finalizer(Surface, MCAIRO_finalize_surface, 0);
            break;

        case CAIRO_STATUS_NULL_POINTER:
        case CAIRO_STATUS_NO_MEMORY:
        case CAIRO_STATUS_READ_ERROR:
        case CAIRO_STATUS_INVALID_CONTENT:
        case CAIRO_STATUS_INVALID_FORMAT:
        case CAIRO_STATUS_INVALID_VISUAL:
            Surface = NULL;
            break;

        default:
            MR_external_fatal_error(\"Mercury cairo\",
                \"unknown PDF surface status\");
    }
#else
    Supported = MR_NO;
    Status = CAIRO_STATUS_SUCCESS;
    Surface = NULL;
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
    MR_external_fatal_error(\"Mercury cairo\",
        \"PDF surfaces not supported by this installation\");
#endif
").

%---------------------------------------------------------------------------%
:- end_module cairo.pdf.
%---------------------------------------------------------------------------%
