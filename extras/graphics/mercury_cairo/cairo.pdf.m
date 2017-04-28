%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2017 The Mercury team.
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

    % pdf.create_surface(FileName, Width, Height, Surface, !IO):
    % Surface is a PDF surface of the specified Width and Height in points
    % to be written to FileName.
    % Throws an unsupported_surface_error/0 exception if PDF surfaces are
    % not supported by this implementation.  Throws a cairo.error/0 exception
    % if any other error occurs.
    %
:- pred create_surface(string::in, float::in, float::in, pdf_surface::out,
    io::di, io::uo) is det.

    % pdf.set_size(Surface, Width, Height, !IO):
    % Change the size of a PDF surface for the current (and subsequent) pages.
    %
:- pred set_size(pdf_surface::in, float::in, float::in,
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
        throw(cairo.unsupported_surface_error("PDF"))
    ).

:- pred create_surface_2(string::in, float::in, float::in,
    bool::out, cairo.status::out, pdf_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(FileName::in, W::in, H::in,
        Supported::out, Status::out, Surface::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PDF_SURFACE)

    cairo_surface_t     *raw_surface;

    Supported = MR_YES;
    raw_surface = cairo_pdf_surface_create(FileName, W, H);
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

:- pragma foreign_proc("C",
    set_size(Surface::in, Width::in, Height::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PDF_SURFACE)
    cairo_pdf_surface_set_size(Surface->mcairo_raw_surface,
        Width, Height);
#else
    MR_external_fatal_error(\"Mercury cairo\",
        \"PDF surfaces not supported by this installation\");
#endif
").

%---------------------------------------------------------------------------%
:- end_module cairo.pdf.
%---------------------------------------------------------------------------%
