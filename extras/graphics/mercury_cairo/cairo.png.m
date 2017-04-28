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
% This sub-module contains support for reading and writing PNG images.
%
%---------------------------------------------------------------------------%

:- module cairo.png.
:- interface.

:- import_module cairo.image.

%---------------------------------------------------------------------------%

   % Succeeds if the reading and writing PNG files is supported by
   % this implementation.
   %
:- pred png_is_supported is semidet.

    % png.image_surface_create_from_png(FileName, Surface, !IO):
    % Surface is a new image surface whose contents are the PNG image
    % from FileName.
    % Throws a cairo.error/0 exception if an error occurs.
    %
:- pred image_surface_create_from_png(string::in, image_surface::out,
    io::di, io::uo) is det.

    % write_surface_to_png(Surface, FileName, !IO):
    % Write the contents of Surface to a new file FileName as a PNG image.
    % Throws a cairo.error/0 exception if an error occurs.
    %
:- pred write_surface_to_png(S::in, string::in,
    io::di, io::uo) is det <= surface(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    png_is_supported,
    [promise_pure, will_not_call_mercury],
"
#if defined(CAIRO_HAS_PNG_FUNCTIONS)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%---------------------------------------------------------------------------%

image_surface_create_from_png(FileName, Surface, !IO) :-
    image_surface_create_from_png_2(FileName, Surface, !IO),
    cairo.surface_status(Surface, Status, !IO),
    ( if Status = status_success then
        true
    else
        throw(cairo.error("png.image_surface_create_from_png/4", Status))
    ).

:- pred image_surface_create_from_png_2(string::in, image_surface::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    image_surface_create_from_png_2(FileName::in, Surface::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_surface_t *raw_image;

    raw_image = cairo_image_surface_create_from_png(FileName);
    Surface = MR_GC_NEW(MCAIRO_surface);
    Surface->mcairo_raw_surface = raw_image;
    MR_GC_register_finalizer(Surface, MCAIRO_finalize_surface, 0);
").

%---------------------------------------------------------------------------%

:- inst png_write_result
    --->    status_success
    ;       status_no_memory
    ;       status_surface_type_mismatch
    ;       status_write_error.

write_surface_to_png(Surface, FileName, !IO) :-
    write_surface_to_png_2(Surface, FileName, Result, !IO),
    (
        Result = status_success
    ;
        ( Result = status_no_memory
        ; Result = status_surface_type_mismatch
        ; Result = status_write_error
        ),
        throw(cairo.error("png.write_surface_to_png/4", Result))
    ).

:- pred write_surface_to_png_2(S::in, string::in,
    cairo.status::out(png_write_result), io::di, io::uo) is det <= surface(S).

:- pragma foreign_proc("C",
    write_surface_to_png_2(Surface::in, FileName::in,
        Result::out(png_write_result), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Result = cairo_surface_write_to_png(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface, FileName);
").

%---------------------------------------------------------------------------%
:- end_module cairo.png.
%---------------------------------------------------------------------------%
