%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004, 2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% xlib.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Jun 21 17:48:24 EST 2004
%
% A low-level interface to parts of Xlib (this is very little more than a
% few useful symbol definitions and wrappers around various Xlib calls.)
%
%-----------------------------------------------------------------------------%

:- module xlib.
:- interface.

:- import_module list.



:- type display_ptr.

:- impure func open_display = (display_ptr::out) is semidet.

:- impure func open_display(string::in) = (display_ptr::out) is semidet.

:- impure pred flush(display_ptr::in) is det.

:- impure pred sync(display_ptr::in) is det.


:- type drawable.

:- impure func create_simple_window(display_ptr::in, int::in, int::in)
    = (drawable::out) is semidet.

:- impure pred set_window_name(display_ptr::in, drawable::in, string::in)
    is semidet.

:- impure pred map_raised(display_ptr::in, drawable::in) is det.

:- impure pred clear_window(display_ptr::in, drawable::in) is det.

:- impure pred resize_window(display_ptr::in, drawable::in, int::in, int::in)
    is det.

:- impure func create_matching_pixmap(display_ptr::in, drawable::in)
    = (drawable::out) is semidet.

:- impure pred free_pixmap(display_ptr::in, drawable::in) is det.

:- impure pred copy_area(display_ptr::in, drawable::in, drawable::in, gc::in,
    int::in, int::in, int::in, int::in, int::in, int::in) is det.


:- type gc.

:- impure func create_gc(display_ptr::in, drawable::in,
    value_mask::in, gc_values_ptr::in) = (gc::out) is semidet.

:- impure pred change_gc(display_ptr::in, gc::in, value_mask::in,
    gc_values_ptr::in) is det.


:- type value_mask == int.

:- func gc_fg           = value_mask.
:- func gc_bg           = value_mask.
:- func gc_line_width   = value_mask.
:- func gc_cap_style    = value_mask.
:- func gc_join_style   = value_mask.
:- func gc_font         = value_mask.


:- type gc_values_ptr.

:- func new_gc_values_ptr = gc_values_ptr.

:- impure pred set_gv_values_fg(gc_values_ptr::in, color_ptr::in) is det.
:- impure pred set_gv_values_bg(gc_values_ptr::in, color_ptr::in) is det.
:- impure pred set_gv_values_line_width(gc_values_ptr::in, int::in) is det.
:- impure pred set_gv_values_cap_style(gc_values_ptr::in, cap_style::in)
    is det.
:- impure pred set_gv_values_join_style(gc_values_ptr::in, join_style::in)
    is det.
:- impure pred set_gv_values_font(gc_values_ptr::in, font_struct_ptr::in)
    is det.


:- type color_ptr.

:- impure func alloc_named_color(display_ptr::in, string::in) =
    (color_ptr::out) is semidet.

:- impure func alloc_rgb_color(display_ptr::in, int::in, int::in, int::in) =
    (color_ptr::out) is semidet.


:- type cap_style.

:- func cap_not_last = cap_style.
:- func cap_butt = cap_style.
:- func cap_round = cap_style.
:- func cap_projecting = cap_style.


:- type join_style.

:- func join_mitre = join_style.
:- func join_miter = join_style.             % Synonym for `mitre'.
:- func join_round = join_style.
:- func join_bevel = join_style.


:- type font_struct_ptr.

:- impure func load_query_font(display_ptr::in, string::in) =
                        (font_struct_ptr::out) is semidet.

:- func font_struct_ptr ^ ascent  = int.
:- func font_struct_ptr ^ descent = int.
:- func font_struct_ptr ^ height  = int.

:- func text_width(font_struct_ptr, string) = int.


:- impure pred set_fg(display_ptr::in, gc::in, color_ptr::in) is det.
:- impure pred set_bg(display_ptr::in, gc::in, color_ptr::in) is det.
:- impure pred set_line_attributes(display_ptr::in, gc::in,
    int::in, cap_style::in, join_style::in) is det.
:- impure pred set_font(display_ptr::in, gc::in, font_struct_ptr::in) is det.


:- impure pred draw_string(display_ptr::in, drawable::in, gc::in,
    int::in, int::in, string::in) is det.

:- impure pred draw_image_string(display_ptr::in, drawable::in, gc::in,
    int::in, int::in, string::in) is det.

:- impure pred draw_point(display_ptr::in, drawable::in, gc::in,
    int::in, int::in) is det.

:- impure pred draw_line(display_ptr::in, drawable::in, gc::in,
    int::in, int::in, int::in, int::in) is det.

:- impure pred draw_arc(display_ptr::in, drawable::in, gc::in,
    int::in, int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred fill_arc(display_ptr::in, drawable::in, gc::in,
    int::in, int::in, int::in, int::in, int::in, int::in) is det.

:- impure pred draw_rectangle(display_ptr::in, drawable::in, gc::in,
                    int::in, int::in, int::in, int::in) is det.

:- impure pred fill_rectangle(display_ptr::in, drawable::in, gc::in,
                    int::in, int::in, int::in, int::in) is det.


:- type xpoint.

:- type xpoints.

:- func xpoint(int, int) = xpoint.

:- func xpoints(list(xpoint)) = xpoints.


:- impure pred draw_lines(display_ptr::in, drawable::in, gc::in, xpoints::in)
    is det.

:- impure pred fill_polygon(display_ptr::in, drawable::in, gc::in, xpoints::in)
    is det.


:- type event_ptr.

:- type buttons_and_modifiers.

:- type button_no == int.

:- type keycode.


:- impure func window_event(display_ptr::in, drawable::in) =
    (event_ptr::out) is det.

:- impure func check_window_event(display_ptr::in, drawable::in) =
    (event_ptr::out) is semidet.


:- pred expose_event(event_ptr::in, int::out, int::out, int::out, int::out,
    int::out) is semidet.

:- pred resize_event(event_ptr::in, int::out, int::out) is semidet.

:- pred button_press_event(event_ptr::in, int::out, int::out,
    buttons_and_modifiers::out, button_no::out) is semidet.

:- pred button_release_event(event_ptr::in, int::out, int::out,
    buttons_and_modifiers::out, button_no::out) is semidet.

:- pred key_press_event(event_ptr::in, int::out, int::out,
    buttons_and_modifiers::out, string::out) is semidet.

:- pred key_release_event(event_ptr::in, int::out, int::out,
    buttons_and_modifiers::out, string::out) is semidet.

:- pred pointer_motion_event(event_ptr::in, int::out, int::out,
    buttons_and_modifiers::out) is semidet.


:- pred button1(buttons_and_modifiers::in) is semidet.
:- pred button2(buttons_and_modifiers::in) is semidet.
:- pred button3(buttons_and_modifiers::in) is semidet.
:- pred button4(buttons_and_modifiers::in) is semidet.
:- pred button5(buttons_and_modifiers::in) is semidet.
:- pred shift(buttons_and_modifiers::in) is semidet.
:- pred lock(buttons_and_modifiers::in) is semidet.
:- pred control(buttons_and_modifiers::in) is semidet.
:- pred mod1(buttons_and_modifiers::in) is semidet.
:- pred mod2(buttons_and_modifiers::in) is semidet.
:- pred mod3(buttons_and_modifiers::in) is semidet.
:- pred mod4(buttons_and_modifiers::in) is semidet.
:- pred mod5(buttons_and_modifiers::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include <string.h>").
:- pragma foreign_decl("C", "#include <X11/X.h>").
:- pragma foreign_decl("C", "#include <X11/Xlib.h>").
:- pragma foreign_decl("C", "#include <X11/Xutil.h>").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", display_ptr, "Display *",
    [can_pass_as_mercury_type]).


:- pragma foreign_proc("C",
    open_display = (DisplayPtr::out),
    [will_not_call_mercury, thread_safe],
"
    DisplayPtr = XOpenDisplay(NULL);
    SUCCESS_INDICATOR = (DisplayPtr != NULL);
").

:- pragma foreign_proc("C",
    open_display(DisplayName::in) = (DisplayPtr::out),
    [will_not_call_mercury, thread_safe],
"
    DisplayPtr = XOpenDisplay(DisplayName);
    SUCCESS_INDICATOR = (DisplayPtr != NULL);
").

:- pragma foreign_proc("C",
    flush(DisplayPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XFlush(DisplayPtr);
").

:- pragma foreign_proc("C",
    sync(DisplayPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XSync(DisplayPtr, 1 /* Discard pending events */);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", drawable, "Drawable").

:- pragma foreign_proc("C",
    create_simple_window(DisplayPtr::in, W::in, H::in) = (Win::out),
    [will_not_call_mercury, thread_safe],
"
    XSetWindowAttributes winattrs;
    winattrs.background_pixmap = None;
    winattrs.bit_gravity       = StaticGravity;
    winattrs.win_gravity       = StaticGravity;
    winattrs.backing_store     = NotUseful;
    winattrs.event_mask        = MyEventMask;

    Win =   XCreateWindow(
                DisplayPtr,
                DefaultRootWindow(DisplayPtr),
                0, 0,                   /* x, y */
                W, H,
                0,                      /* Border width */
                DefaultDepth(DisplayPtr, DefaultScreen(DisplayPtr)),
                InputOutput,
                CopyFromParent,
                CWBackPixmap | CWBitGravity | CWWinGravity |
                    CWBackingStore | CWEventMask,
                &winattrs
            );

    SUCCESS_INDICATOR =
        (   Win != BadAlloc
        &&  Win != BadMatch
        &&  Win != BadValue
        &&  Win != BadWindow
        );
").


:- pragma foreign_proc("C",
    set_window_name(DisplayPtr::in, Win::in, TitleText::in),
    [will_not_call_mercury, thread_safe],
"
    XTextProperty TitleTextProperty;

    SUCCESS_INDICATOR =
        XStringListToTextProperty(&TitleText, 1, &TitleTextProperty);

    if (SUCCESS_INDICATOR) {
        XSetWMName(DisplayPtr, Win, &TitleTextProperty);
        XSetWMIconName(DisplayPtr, Win, &TitleTextProperty);
    }
").

:- pragma foreign_proc("C",
    map_raised(DisplayPtr::in, Win::in),
    [will_not_call_mercury, thread_safe],
"
    XMapRaised(DisplayPtr, Win);
").

:- pragma foreign_proc("C",
    clear_window(DisplayPtr::in, Win::in),
    [will_not_call_mercury, thread_safe],
"
    XClearWindow(DisplayPtr, Win);
").

:- pragma foreign_proc("C",
    resize_window(DisplayPtr::in, Win::in, W::in, H::in),
    [will_not_call_mercury, thread_safe],
"
    XResizeWindow(DisplayPtr, Win, W, H);
").

:- pragma foreign_proc("C",
    create_matching_pixmap(DisplayPtr::in, Win::in) = (Pix::out),
    [will_not_call_mercury, thread_safe],
"
    XWindowAttributes winattrs;

    XGetWindowAttributes(DisplayPtr, Win, &winattrs);
    Pix = XCreatePixmap(DisplayPtr, Win,
                    winattrs.width, winattrs.height, winattrs.depth);

    SUCCESS_INDICATOR =
        (   Win != BadAlloc
        &&  Win != BadDrawable
        &&  Win != BadValue
        );
").

:- pragma foreign_proc("C",
    free_pixmap(DisplayPtr::in, Pix::in),
    [will_not_call_mercury, thread_safe],
"
    XFreePixmap(DisplayPtr, Pix);
").


:- pragma foreign_proc("C",
    copy_area(DisplayPtr::in, Pix::in, Win::in, Gc::in,
        X1::in, Y1::in, W::in, H::in, X2::in, Y2::in),
    [will_not_call_mercury, thread_safe],
"
    XCopyArea(DisplayPtr, Pix, Win, Gc, X1, Y1, W, H, X2, Y2);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", gc, "GC").

:- pragma foreign_proc("C",
    create_gc(DisplayPtr::in, Win::in, ValueMask::in, GCValuesPtr::in)
        = (Gc::out),
    [will_not_call_mercury, thread_safe],
"
    Gc = XCreateGC(DisplayPtr, Win, ValueMask, GCValuesPtr);

    SUCCESS_INDICATOR = (
            (int) Gc != BadAlloc
        &&  (int) Gc != BadDrawable
        &&  (int) Gc != BadFont
        &&  (int) Gc != BadMatch
        &&  (int) Gc != BadPixmap
        &&  (int) Gc != BadValue
    );
").

:- pragma foreign_proc("C",
    change_gc(DisplayPtr::in, Gc::in, ValueMask::in, GCValuesPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XChangeGC(DisplayPtr, Gc, ValueMask, GCValuesPtr);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    gc_fg = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCForeground;").

:- pragma foreign_proc("C", gc_bg         = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCBackground;").

:- pragma foreign_proc("C", gc_line_width = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCLineWidth;" ).

:- pragma foreign_proc("C", gc_cap_style  = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCCapStyle;"  ).

:- pragma foreign_proc("C", gc_join_style = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCJoinStyle;" ).

:- pragma foreign_proc("C", gc_font       = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = GCFont;"      ).


%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", gc_values_ptr, "XGCValues *",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    new_gc_values_ptr = (GCValuesPtr::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    GCValuesPtr = MR_GC_NEW(XGCValues);
").

:- pragma foreign_proc("C", set_gv_values_fg(GCValuesPtr::in, ColourPtr::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->foreground = ColourPtr->pixel;
").

:- pragma foreign_proc("C",
    set_gv_values_bg(GCValuesPtr::in, ColourPtr::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->background = ColourPtr->pixel;
").

:- pragma foreign_proc("C",
    set_gv_values_line_width(GCValuesPtr::in, LineWidth::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->line_width = LineWidth;
").

:- pragma foreign_proc("C",
    set_gv_values_cap_style(GCValuesPtr::in, CapStyle::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->cap_style = CapStyle;
").

:- pragma foreign_proc("C",
    set_gv_values_join_style(GCValuesPtr::in, JoinStyle::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->join_style = JoinStyle;
").

:- pragma foreign_proc("C",
    set_gv_values_font(GCValuesPtr::in, FontStructPtr::in),
    [will_not_call_mercury, thread_safe],
"
    GCValuesPtr->font = FontStructPtr->fid;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", color_ptr, "XColor *",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    alloc_named_color(DisplayPtr::in, ColorName::in) = (ColorPtr::out),
    [will_not_call_mercury, thread_safe],
"
    XColor ExactDefReturn;
    ColorPtr = MR_GC_NEW(XColor);
    SUCCESS_INDICATOR =
        XAllocNamedColor(
            DisplayPtr,
            DefaultColormap(DisplayPtr, DefaultScreen(DisplayPtr)),
            ColorName,
            ColorPtr,
            &ExactDefReturn
        );
").

:- pragma foreign_proc("C",
    alloc_rgb_color(DisplayPtr::in, R::in, G::in, B::in) = (ColorPtr::out),
    [will_not_call_mercury, thread_safe],
"
    ColorPtr = MR_GC_NEW(XColor);
    ColorPtr->red   = R;
    ColorPtr->green = G;
    ColorPtr->blue  = B;
    SUCCESS_INDICATOR =
        XAllocColor(
            DisplayPtr,
            DefaultColormap(DisplayPtr, DefaultScreen(DisplayPtr)),
            ColorPtr
        );
").

%-----------------------------------------------------------------------------%

    % XXX this should be defined using a foreign_enum.
    %
:- pragma foreign_type("C", cap_style, "int", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C", cap_not_last = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = CapNotLast;"   ).

:- pragma foreign_proc("C", cap_butt = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = CapButt;"      ).

:- pragma foreign_proc("C", cap_round = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = CapRound;"     ).

:- pragma foreign_proc("C", cap_projecting = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = CapProjecting;").


%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", join_style, "int", [can_pass_as_mercury_type]).

join_miter = join_mitre.

:- pragma foreign_proc("C", join_mitre = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = JoinMiter;").

:- pragma foreign_proc("C", join_round = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = JoinRound;").

:- pragma foreign_proc("C", join_bevel = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure], "X = JoinBevel;").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", font_struct_ptr, "XFontStruct *",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    load_query_font(DisplayPtr::in, Name::in) = (FontStructPtr::out),
    [will_not_call_mercury, thread_safe],
"
    FontStructPtr = XLoadQueryFont(DisplayPtr, Name);
    SUCCESS_INDICATOR = (FontStructPtr != NULL);
").

:- pragma foreign_proc("C",
    ascent(FontStructPtr::in) = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    X = FontStructPtr->ascent;
").

:- pragma foreign_proc("C",
    descent(FontStructPtr::in) = (X::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    X = FontStructPtr->descent;
").

FontStructPtr^height = FontStructPtr^ascent + FontStructPtr^descent.

:- pragma foreign_proc("C",
    text_width(FontStructPtr::in, Text::in) = (Width::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Width = XTextWidth(FontStructPtr, Text, strlen(Text));
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    set_fg(DisplayPtr::in, Gc::in, ColorPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XSetForeground(DisplayPtr, Gc, ColorPtr->pixel);
").

:- pragma foreign_proc("C",
    set_bg(DisplayPtr::in, Gc::in, ColorPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XSetBackground(DisplayPtr, Gc, ColorPtr->pixel);
").

:- pragma foreign_proc("C",
    set_font(DisplayPtr::in, Gc::in, FontStructPtr::in),
    [will_not_call_mercury, thread_safe],
"
    XSetFont(DisplayPtr, Gc, FontStructPtr->fid);
").

:- pragma foreign_proc("C",
    set_line_attributes(DisplayPtr::in, Gc::in, Width::in, CapStyle::in,
        JoinStyle::in),
    [will_not_call_mercury, thread_safe],
"
    XSetLineAttributes(DisplayPtr, Gc,
        Width, LineSolid, CapStyle, JoinStyle);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    draw_string(DisplayPtr::in, Win::in, Gc::in, X::in, Y::in, Text::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawString(DisplayPtr, Win, Gc, X, Y, Text, strlen(Text));
").

:- pragma foreign_proc("C",
    draw_image_string(DisplayPtr::in, Win::in, Gc::in, X::in, Y::in, Text::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawImageString(DisplayPtr, Win, Gc, X, Y, Text, strlen(Text));
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    draw_point(DisplayPtr::in, Win::in, Gc::in, X::in, Y::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawPoint(DisplayPtr, Win, Gc, X, Y);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    draw_line(DisplayPtr::in, Win::in, Gc::in, X1::in, Y1::in, X2::in, Y2::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawLine(DisplayPtr, Win, Gc, X1, Y1, X2, Y2);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    draw_arc(DisplayPtr::in, Win::in, Gc::in, X1::in, Y1::in, X2::in, Y2::in,
        Angle1::in, Angle2::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawArc(DisplayPtr, Win, Gc, X1, Y1, X2, Y2, Angle1, Angle2);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    fill_arc(DisplayPtr::in, Win::in, Gc::in, X1::in, Y1::in, X2::in, Y2::in,
        Angle1::in, Angle2::in),
    [will_not_call_mercury, thread_safe],
"
    XFillArc(DisplayPtr, Win, Gc, X1, Y1, X2, Y2, Angle1, Angle2);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    draw_rectangle(DisplayPtr::in, Win::in, Gc::in,
        X1::in, Y1::in, X2::in, Y2::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawRectangle(DisplayPtr, Win, Gc, X1, Y1, X2, Y2);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", fill_rectangle(DisplayPtr::in, Win::in, Gc::in,
                                X1::in, Y1::in, X2::in, Y2::in),
    [will_not_call_mercury, thread_safe],
    "
        XFillRectangle(DisplayPtr, Win, Gc, X1, Y1, X2, Y2);
    ").

%-----------------------------------------------------------------------------%

:- type xpoints
    --->    xpoints(int, xpoint_array).

:- type xpoint_array.

:- pragma foreign_type("C", xpoint, "XPoint *", [can_pass_as_mercury_type]).

:- pragma foreign_type("C", xpoint_array, "XPoint *",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C", xpoint(X::in, Y::in) = (XPt::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    XPt = MR_GC_NEW(XPoint);
    XPt->x = X;
    XPt->y = Y;
").

xpoints(XYs) = xpoints(N, XPts) :-
    N = length(XYs),
    XPts = xpoint_array(N, XYs).

:- func xpoint_array(int, list(xpoint)) = xpoint_array.
:- pragma foreign_proc("C",
    xpoint_array(N::in, XPts::in) = (XPtArray::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    MR_Integer i;
    XPtArray = MR_GC_NEW_ARRAY(XPoint, N);
    for(i = 0; i < N; i++) {
        XPoint *xpt   = (XPoint *) MR_list_head(XPts);
        XPtArray[i].x = xpt->x;
        XPtArray[i].y = xpt->y;
        XPts          = MR_list_tail(XPts);
    }
").

draw_lines(DisplayPtr, Win, Gc, xpoints(N, XPts)) :-
    impure draw_lines_2(DisplayPtr, Win, Gc, XPts, N).

:- impure pred draw_lines_2(display_ptr::in, drawable::in, gc::in,
    xpoint_array::in, int::in) is det.
:- pragma foreign_proc("C",
    draw_lines_2(DisplayPtr::in, Win::in, Gc::in, XPts::in, N::in),
    [will_not_call_mercury, thread_safe],
"
    XDrawLines(DisplayPtr, Win, Gc, XPts, N, CoordModeOrigin);
").

fill_polygon(DisplayPtr, Win, Gc, xpoints(N, XPts)) :-
    impure fill_polygon_2(DisplayPtr, Win, Gc, XPts, N).

:- impure pred fill_polygon_2(display_ptr::in, drawable::in, gc::in,
    xpoint_array::in, int::in) is det.
:- pragma foreign_proc("C",
    fill_polygon_2(DisplayPtr::in, Win::in, Gc::in, XPts::in, N::in),
    [will_not_call_mercury, thread_safe],
"
    XFillPolygon(DisplayPtr, Win, Gc, XPts, N, Complex, CoordModeOrigin);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", event_ptr, "XEvent *", [can_pass_as_mercury_type]).

:- type buttons_and_modifiers == int.

:- type keycode == int.

:- pragma foreign_decl("C", "

#define MyEventMask     (   KeyPressMask        \
                        |   KeyReleaseMask      \
                        |   ButtonPressMask     \
                        |   ButtonReleaseMask   \
                        |   PointerMotionMask   \
                        |   ButtonMotionMask    \
                        |   ExposureMask        \
                        |   StructureNotifyMask \
                        )

").

:- pragma foreign_proc("C",
    window_event(DisplayPtr::in, Win::in) = (EventPtr::out),
    [will_not_call_mercury, thread_safe],
"
    EventPtr = MR_GC_NEW(XEvent);
    XWindowEvent(DisplayPtr, Win, MyEventMask, EventPtr);
").

:- pragma foreign_proc("C",
    check_window_event(DisplayPtr::in, Win::in) = (EventPtr::out),
    [will_not_call_mercury, thread_safe],
"
    EventPtr = MR_GC_NEW(XEvent);
    SUCCESS_INDICATOR = XCheckWindowEvent(DisplayPtr, Win, MyEventMask,
        EventPtr);
").


:- pragma foreign_proc("C",
    expose_event(EventPtr::in, X::out, Y::out, W::out, H::out, Count::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == Expose );
    if (SUCCESS_INDICATOR) {
        X     = EventPtr->xexpose.x;
        Y     = EventPtr->xexpose.y;
        W     = EventPtr->xexpose.width;
        H     = EventPtr->xexpose.height;
        Count = EventPtr->xexpose.count;
    }
").

:- pragma foreign_proc("C",
    resize_event(EventPtr::in, W::out, H::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == ConfigureNotify );
    if (SUCCESS_INDICATOR) {
        W = EventPtr->xconfigure.width;
        H = EventPtr->xconfigure.height;
    }
").

:- pragma foreign_proc("C",
    button_press_event(EventPtr::in, X::out, Y::out, State::out, Button::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == ButtonPress );
    if (SUCCESS_INDICATOR) {
        X       = EventPtr->xbutton.x;
        Y       = EventPtr->xbutton.y;
        State   = EventPtr->xbutton.state;
        Button  = EventPtr->xbutton.button;
    }
").

:- pragma foreign_proc("C",
    button_release_event(EventPtr::in, X::out, Y::out, State::out, Button::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == ButtonRelease );
    if (SUCCESS_INDICATOR) {
        X       = EventPtr->xbutton.x;
        Y       = EventPtr->xbutton.y;
        State   = EventPtr->xbutton.state;
        Button  = EventPtr->xbutton.button;
    }
").

:- pragma foreign_proc("C",
    key_press_event(EventPtr::in, X::out, Y::out, State::out,
        KeysymString::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == KeyPress );
    if (SUCCESS_INDICATOR) {
        X            = EventPtr->xkey.x;
        Y            = EventPtr->xkey.y;
        State        = EventPtr->xkey.state;
        KeysymString = XKeysymToString(XKeycodeToKeysym(
                            EventPtr->xkey.display,
                            EventPtr->xkey.keycode,
                            ((State & ShiftMask) != 0)));
        if (KeysymString == NULL) {
            KeysymString = XKeysymToString(XKeycodeToKeysym(
                                EventPtr->xkey.display,
                                EventPtr->xkey.keycode,
                                0));
        }
        if (KeysymString == NULL) {
            KeysymString = (MR_String) \"\";
        }
    }
").

:- pragma foreign_proc("C",
    key_release_event(EventPtr::in, X::out, Y::out, State::out,
        KeysymString::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == KeyRelease );
    if (SUCCESS_INDICATOR) {
        X            = EventPtr->xkey.x;
        Y            = EventPtr->xkey.y;
        State        = EventPtr->xkey.state;
        KeysymString = XKeysymToString(XKeycodeToKeysym(
                            EventPtr->xkey.display,
                            EventPtr->xkey.keycode,
                            ((State & ShiftMask) != 0)));
        if (KeysymString == NULL) {
            KeysymString = XKeysymToString(XKeycodeToKeysym(
                                EventPtr->xkey.display,
                                EventPtr->xkey.keycode,
                                0));
        }
        if (KeysymString == NULL) {
            KeysymString = (MR_String) \"\";
        }
    }
").

:- pragma foreign_proc("C",
    pointer_motion_event(EventPtr::in, X::out, Y::out, State::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ( EventPtr->type == MotionNotify );
    if (SUCCESS_INDICATOR) {
        X       = EventPtr->xmotion.x;
        Y       = EventPtr->xmotion.y;
        State   = EventPtr->xmotion.state;
    }
").

:- pragma foreign_proc("C", button1(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Button1Mask;
").

:- pragma foreign_proc("C", button2(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Button2Mask;
").

:- pragma foreign_proc("C", button3(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Button3Mask;
").

:- pragma foreign_proc("C", button4(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Button4Mask;
").

:- pragma foreign_proc("C", button5(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Button5Mask;
").

:- pragma foreign_proc("C", shift(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & ShiftMask;
").

:- pragma foreign_proc("C", lock(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & LockMask;
").

:- pragma foreign_proc("C", control(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & ControlMask;
").

:- pragma foreign_proc("C", mod1(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Mod1Mask;
").

:- pragma foreign_proc("C", mod2(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Mod2Mask;
").

:- pragma foreign_proc("C", mod3(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Mod3Mask;
").

:- pragma foreign_proc("C", mod4(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Mod4Mask;
").

:- pragma foreign_proc("C", mod5(State::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = State & Mod5Mask;
").

%-----------------------------------------------------------------------------%
:- end_module xlib.
%-----------------------------------------------------------------------------%
