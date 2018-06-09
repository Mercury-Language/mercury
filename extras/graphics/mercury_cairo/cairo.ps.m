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
% This sub-module contains support for rendering PostScript documents.
%
%---------------------------------------------------------------------------%

:- module cairo.ps.
:- interface.

%---------------------------------------------------------------------------%

:- type ps_surface.

:- instance surface(ps_surface).

%---------------------------------------------------------------------------%

    % This type describes the language level of the PostScript Language
    % Reference that a generated PostScript file will conform to.
    %
:- type ps_level
    --->    ps_level_2
            % The language level 2 of the PostScript specification.

    ;       ps_level_3.
            % The language level 3 of the PostScript specification.

%---------------------------------------------------------------------------%

    % Succeeds if PostScript surfaces are supported by this implementation.
    %
:- pred have_ps_surface is semidet.

    % ps.create_surface(FileName, Width, Height, Surface, !IO):
    % Surface is a PostScript surface of the specified Width and Height in
    % in points to be written to FileName.
    % Throw an unsupported_surface_error/0 exception if PostScript surfaces
    % are not supported by this implementation.  Throws a cairo.error/0
    % exception if any other error occurs.
    %
:- pred create_surface(string::in, float::in, float::in, ps_surface::out,
    io::di, io::uo) is det.

    % ps.restrict_to_level(Surface, Level, !IO):
    % Restrict Surface to the given Level of the PostScript specification.
    %
:- pred restrict_to_level(ps_surface::in, ps_level::in,
    io::di, io::uo) is det.

    % ps.set_eps(Surface, EPS, !IO):
    % If EPS is "yes" then Surface will output Encapsulated PostScript.
    %
:- pred set_eps(ps_surface::in, bool::in, io::di, io::uo) is det.

    % ps.get_eps(Surface, EPS, !IO):
    % EPS is "yes" if Surface is set to output Encapsulated PostScript.
    %
:- pred get_eps(ps_surface::in, bool::out, io::di, io::uo) is det.

    % ps.set_size(Surface, Width, Height, !IO):
    % Change the size of Surface for the current (and subsequent) pages.
    %
:- pred set_size(ps_surface::in, float::in, float::in, io::di, io::uo) is det.

    % ps.dsc_begin_setup(Surface, !IO):
    %
:- pred dsc_begin_setup(ps_surface::in, io::di, io::uo) is det.

    % ps.dsc_begin_page_setup(Surface, !IO):
    %
:- pred dsc_begin_page_setup(ps_surface::in, io::di, io::uo) is det.

    % ps.dsc_comment(Surface, Comment, !IO):
    % Emit Comment into the PostScript output as a comment for Surface.
    % (See the cairo manual entry for cairo_ps_surface_dsc_comment() for
    % further details.)
    %
:- pred dsc_comment(ps_surface::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "

#include \"cairo.mh\"

#if defined(CAIRO_HAS_PS_SURFACE)
  #include <cairo-ps.h>
#else
  /* These are unlikely to change. */
  enum {
    CAIRO_PS_LEVEL_2,
    CAIRO_PS_LEVEL_3
  };
#endif

").

:- pragma foreign_type("C", ps_surface, "MCAIRO_surface *",
    [can_pass_as_mercury_type]).

:- instance surface(ps_surface) where [].

:- pragma foreign_enum("C", ps_level/0, [
    ps_level_2 - "CAIRO_PS_LEVEL_2",
    ps_level_3 - "CAIRO_PS_LEVEL_3"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
   have_ps_surface,
   [promise_pure, will_not_call_mercury],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%-----------------------------------------------------------------------------%
%
% PostScript surface creation
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
        throw(cairo.unsupported_surface_error("PostScript"))
    ).

:- pred create_surface_2(string::in, float::in, float::in,
    bool::out, cairo.status::out, ps_surface::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_surface_2(FileName::in, W::in, H::in,
        Supported::out, Status::out, Surface::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)

    cairo_surface_t     *raw_surface;

    Supported = MR_YES;
    raw_surface = cairo_ps_surface_create(FileName, W, H);
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
    restrict_to_level(Surface::in, Level::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_restrict_to_level(Surface->mcairo_raw_surface, Level);
#else
    MR_external_fatal_error(\"Mercury cairo\",
        \" PostScript surfaces are not supported by this installation\");
#endif
").

:- pragma foreign_proc("C",
    set_eps(Surface::in, EPS::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_set_eps(Surface->mcairo_raw_surface,
        (EPS = MR_YES ? 1 : 0));
#endif
").

:- pragma foreign_proc("C",
    get_eps(Surface::in, EPS::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    EPS = cairo_ps_surface_get_eps(Surface->mcairo_raw_surface)
        ? MR_YES : MR_NO;
#else
    EPS = MR_NO;
#endif
").

:- pragma foreign_proc("C",
    set_size(Surface::in, W::in, H::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_set_size(Surface->mcairo_raw_surface, W, H);
#endif
").

:- pragma foreign_proc("C",
    dsc_begin_setup(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_dsc_begin_setup(Surface->mcairo_raw_surface);
#endif
").

:- pragma foreign_proc("C",
    dsc_begin_page_setup(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_dsc_begin_page_setup(Surface->mcairo_raw_surface);
#endif
").

:- pragma foreign_proc("C",
    dsc_comment(Surface::in, Comment::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
#if defined(CAIRO_HAS_PS_SURFACE)
    cairo_ps_surface_dsc_comment(Surface->mcairo_raw_surface, Comment);
#endif
").

%---------------------------------------------------------------------------%
:- end_module cairo.ps.
%---------------------------------------------------------------------------%
