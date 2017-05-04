%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module provides various generic operations on cairo surfaces.
%
%---------------------------------------------------------------------------%

:- module cairo.surface.
:- interface.

%---------------------------------------------------------------------------%

% NYI. create_surface_similar

    % surface.finish(Surface, !IO):
    % Finish Surface and drop all references to external resources.
    %
:- pred finish(S::in, io::di, io::uo) is det <= surface(S).

    % surface.flush(Surface, !IO):
    % Do any pending drawing of Surface and also restore any temporary
    % modifications cairo has made to the surface's state.
    %
:- pred flush(S::in, io::di, io::uo) is det <= surface(S).

    % surface.get_font_options(Surface, FontOptions, !IO):
    % FontOptions is the default font rendering options for Surface.
    %
:- pred get_font_options(S::in, font_options::out, io::di, io::uo)
    is det <= surface(S).

    % surface.get_content(Surface, ContentType, !IO):
    % ContentType is the content type of Surface.
    %
:- pred get_content(S::in, content::out, io::di, io::uo) is det <= surface(S).

    % surface.mark_dirty(Surface, !IO):
    % Tell cairo that drawing has been done to Surface using means other than
    % cairo and that it should re-read any cached areas.
    %
:- pred mark_dirty(S::in, io::di, io::uo) is det <= surface(S).

    % surface.mark_dirty_rectangle(Surface, X, Y, Width, Height, !IO):
    %  XXX - rest of documentation.
:- pred mark_dirty_rectangle(S::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det <= surface(S).

    % surface.set_device_offset(Surface, X, Y, !IO):
    % Sets an offset, X (Y) in device units in that X (Y) direction, that is
    % added to the device coordinates determined by the current transformation
    % matrix  when drawing to Surface.
    %
:- pred set_device_offset(S::in, float::in, float::in,
    io::di, io::uo) is det <= surface(S).

    % surface.get_device_offset(Surface, X, Y, !IO):
    % Return the device offsets set by the above.
    %
:- pred get_device_offset(S::in, float::out, float::out,
    io::di, io::uo) is det <= surface(S).

    % surface.set_device_scale(Surface, XScale, YScale, !IO):
    % Sets a scale that is multiplied to the device coordinates determined by
    % the current transformation matrix when drawing to Surface.
    %
:- pred set_device_scale(S::in, float::in, float::in,
    io::di, io::uo) is det <= surface(S).

    % surface.get_device_scale(Surface, XScale, YScale, !IO):
    % Return the previous device scale set by the above.
    %
:- pred get_device_scale(S::in, float::out, float::out,
    io::di, io::uo) is det <= surface(S).

    % surface.set_fallback_resolution(Surface, X, Y, !IO):
    % Set the horizontal and vertical resolution for image fallbacks.
    %
:- pred set_fallback_resolution(S::in, float::in, float::in, io::di, io::uo)
    is det <= surface(S).

    % surface.get_fallback_resolution(Surface, X, Y, !IO):
    % Get the current fallback resolution for Surface.
    %
:- pred get_fallback_resolution(S::in, float::out, float::out, io::di, io::uo)
    is det <= surface(S).

    % surface.copy_page(Surface, !IO):
    %
:- pred copy_page(S::in, io::di, io::uo) is det <= surface(S).

    % surface.show_page(Surface, !IO):
    %
:- pred show_page(S::in, io::di, io::uo) is det <= surface(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
    finish(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_finish(((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    flush(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_flush(((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    get_font_options(Surface::in, FntOpts::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_t    *raw_font_options;

    raw_font_options = cairo_font_options_create();
    cairo_surface_get_font_options(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface,
        raw_font_options);
    FntOpts = MR_GC_NEW(MCAIRO_font_options);
    FntOpts->mcairo_raw_font_options = raw_font_options;
    MR_GC_register_finalizer(FntOpts, MCAIRO_finalize_font_options, 0);
").

:- pragma foreign_proc("C",
    get_content(Surface::in, Content::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Content = cairo_surface_get_content(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    mark_dirty(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_mark_dirty(((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    mark_dirty_rectangle(Surface::in, X::in, Y::in, Width::in, Height::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_mark_dirty_rectangle(
       ((MCAIRO_surface *)Surface)->mcairo_raw_surface, X, Y, Width, Height);
").

:- pragma foreign_proc("C",
    set_device_offset(Surface::in, X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_set_device_offset(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, X, Y);
").

:- pragma foreign_proc("C",
    get_device_offset(Surface::in, X::out, Y::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_get_device_offset(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, &X, &Y);
").

:- pragma foreign_proc("C",
    set_device_scale(Surface::in, XScale::in, YScale::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_set_device_scale(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, XScale, YScale);
").

:- pragma foreign_proc("C",
    get_device_scale(Surface::in, XScale::out, YScale::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x_scale, y_scale;

    cairo_surface_get_device_scale(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, &x_scale, &y_scale);
    XScale = x_scale;
    YScale = y_scale;
").

:- pragma foreign_proc("C",
    set_fallback_resolution(Surface::in, X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_set_fallback_resolution(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, X, Y);
").

:- pragma foreign_proc("C",
    get_fallback_resolution(Surface::in, X::out, Y::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_get_fallback_resolution(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, &X, &Y);
").

:- pragma foreign_proc("C",
    copy_page(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_copy_page(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    show_page(Surface::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_show_page(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

%---------------------------------------------------------------------------%
:- end_module cairo.surface.
%---------------------------------------------------------------------------%
