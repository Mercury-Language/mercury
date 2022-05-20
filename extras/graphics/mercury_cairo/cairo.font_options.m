%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module contains predicates for dealing with font_options
% objects, which are how cairo controls how fonts are rendered.
%
%----------------------------------------------------------------------------%

:- module cairo.font_options.
:- interface.

%----------------------------------------------------------------------------%

    % font_options.create(FontOpts, !IO):
    %
:- pred create(font_options::out, io::di, io::uo) is det.

    % font_options.copy(Original, Copy, !IO):
    %
:- pred copy(font_options::in, font_options::out, io::di, io::uo) is det.

    % set_antialias(FontOptions, AntiAlias, !IO):
    %
:- pred set_antialias(font_options::in, antialias::in, io::di, io::uo) is det.

    % font_options.get_antialias(FontOptions, AntiAlias, !IO):
    % AntiAlias is the antialiasing mode for FontOptions.
    %
:- pred get_antialias(font_options::in, antialias::out, io::di, io::uo) is det.

:- type subpixel_order
    --->    subpixel_order_default
    ;       subpixel_order_rgb
    ;       subpixel_order_bgr
    ;       subpixel_order_vrgb
    ;       subpixel_order_vbgr.

    % font_options.set_subpixel_order(FontOptions, SubpixelOrder, !IO):
    %
:- pred set_subpixel_order(font_options::in, subpixel_order::in,
    io::di, io::uo) is det.

    % font_options.get_subpixel_order(FontOptions, SubpixelOrder, !IO):
    % SubpixelOrder is the current subpixel order for FontOptions.
    %
:- pred get_subpixel_order(font_options::in, subpixel_order::out,
    io::di, io::uo) is det.

    % The type of hinting to do on font outlines.
    %
:- type hint_style
    --->    hint_style_default
            % Use the default hint style for font backend and target device.

    ;       hint_style_none
            % Do not hint outlines.

    ;       hint_style_slight
            % Hint outlines slightly to improve contrast while retaining good
            % fidelity to the original shapes.

    ;       hint_style_medium
            % Hint outlines with medium strength giving a compromise between
            % fidelity to the original shapes and contrast.

    ;       hint_style_full.
            % Hint outlines to maximize contrast.

    % font_options.set_hint_style(FontOptions, HintStyle, !IO):
    %
:- pred set_hint_style(font_options::in, hint_style::in, io::di, io::uo)
    is det.

    % font_options.get_hint_style(FontOptions, HintStyle, !IO):
    %
:- pred get_hint_style(font_options::in, hint_style::out, io::di, io::uo)
    is det.

:- type hint_metrics
    --->    hint_metrics_default
            % Hint metrics in the default manner for the font backend and
            % target device.

    ;       hint_metrics_off
            % Do not hint font metric.

    ;       hint_metrics_on.
            % Hint font metrics.

    % font_option.set_hint_metrics(FontOptions, HintMetrics, !IO):
    %
:- pred set_hint_metrics(font_options::in, hint_metrics::in, io::di, io::uo)
    is det.

    % font_options.get_hint_metrics(FontOptions, HintMetrics, !IO):
    %
:- pred get_hint_metrics(font_options::in, hint_metrics::out, io::di, io::uo)
    is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_enum("C", subpixel_order/0, [
    subpixel_order_default - "CAIRO_SUBPIXEL_ORDER_DEFAULT",
    subpixel_order_rgb     - "CAIRO_SUBPIXEL_ORDER_RGB",
    subpixel_order_bgr     - "CAIRO_SUBPIXEL_ORDER_BGR",
    subpixel_order_vrgb    - "CAIRO_SUBPIXEL_ORDER_VRGB",
    subpixel_order_vbgr    - "CAIRO_SUBPIXEL_ORDER_VBGR"
]).

:- pragma foreign_enum("C", hint_style/0, [
    hint_style_default    - "CAIRO_HINT_STYLE_DEFAULT",
    hint_style_none       - "CAIRO_HINT_STYLE_NONE",
    hint_style_slight     - "CAIRO_HINT_STYLE_SLIGHT",
    hint_style_medium     - "CAIRO_HINT_STYLE_MEDIUM",
    hint_style_full       - "CAIRO_HINT_STYLE_FULL"
]).

:- pragma foreign_enum("C", hint_metrics/0, [
    hint_metrics_default - "CAIRO_HINT_METRICS_DEFAULT",
    hint_metrics_off     - "CAIRO_HINT_METRICS_OFF",
    hint_metrics_on      - "CAIRO_HINT_METRICS_ON"
]).

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    create(FntOpts::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_t    *raw_font_options;

    raw_font_options = cairo_font_options_create();
    FntOpts = MR_GC_NEW_ATTRIB(MCAIRO_font_options, MR_ALLOC_ID);
    FntOpts->mcairo_raw_font_options = raw_font_options;
    MR_GC_register_finalizer(FntOpts, MCAIRO_finalize_font_options, 0);
").

:- pragma foreign_proc("C",
    copy(Orig::in, Copy::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_t    *raw_copy;

    raw_copy = cairo_font_options_copy(Orig->mcairo_raw_font_options);
    Copy = MR_GC_NEW_ATTRIB(MCAIRO_font_options, MR_ALLOC_ID);
    Copy->mcairo_raw_font_options = raw_copy;
    MR_GC_register_finalizer(Copy, MCAIRO_finalize_font_options, 0);
").

:- pragma foreign_proc("C",
    set_antialias(FntOpts::in, AntiAlias::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_set_antialias(FntOpts->mcairo_raw_font_options,
        AntiAlias);
").

:- pragma foreign_proc("C",
    get_antialias(FntOpts::in, AntiAlias::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    AntiAlias =
        cairo_font_options_get_antialias(FntOpts->mcairo_raw_font_options);
").

:- pragma foreign_proc("C",
    set_subpixel_order(FntOpts::in, SPO::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_set_subpixel_order(FntOpts->mcairo_raw_font_options,
        SPO);
").

:- pragma foreign_proc("C",
    get_subpixel_order(FntOpts::in, SPO::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    SPO = cairo_font_options_get_subpixel_order(
        FntOpts->mcairo_raw_font_options);
").

:- pragma foreign_proc("C",
    set_hint_style(FntOpts::in, HintStyle::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_set_hint_style(FntOpts->mcairo_raw_font_options,
        HintStyle);
").

:- pragma foreign_proc("C",
    get_hint_style(FntOpts::in, HintStyle::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    HintStyle = cairo_font_options_get_hint_style(
        FntOpts->mcairo_raw_font_options);
").

:- pragma foreign_proc("C",
    set_hint_metrics(FntOpts::in, HintMetrics::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_set_hint_metrics(FntOpts->mcairo_raw_font_options,
        HintMetrics);
").

:- pragma foreign_proc("C",
    get_hint_metrics(FntOpts::in, HintMetrics::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    HintMetrics = cairo_font_options_get_hint_metrics(
        FntOpts->mcairo_raw_font_options);
").

%----------------------------------------------------------------------------%
:- end_module cairo.font_options.
%----------------------------------------------------------------------------%
