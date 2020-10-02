%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998,2000, 2003-2006 The University of Melbourne.
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
% Copyright (C) 2014, 2018, 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% file: mtk.
% main author: conway.
% stability: very low.
%
%
% This file provides an interface to the Tk widget set, [conceptually]
% bypassing Tcl.
%
% 07/13/01 hkrug@rationalizer.com:
%       * interface:
%         - added constant root_window for access of the root window by
%           the application
%       * implementation:
%         - more careful formation of internal Tk-names for created
%           widgets, when the parent is the root window:
%           string.format("%s.button%d", [s("."), i(13)], Widget)
%           would result in the invalid name "..button13"
%
%-----------------------------------------------------------------------------%

:- module mtk.
:- interface.

:- import_module mtcltk.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type widget.

:- type config
    --->    active(bool)
    ;       active_background(string)
    ;       active_foreground(string)
    ;       active_relief(string)
    ;       anchor(anchor)
    ;       aspect(int)
    ;       background(string)
    ;       bitmap(string)
    ;       border_width(int)
    ;       command(pred(tcl_interp, io, io))
    ;       cursor(string)
    ;       disabled_foreground(string)
    ;       element_border_width(int)
    ;       export_selection(bool)
    ;       fill_color(string)
    ;       font(string)
    ;       foreground(string)
    ;       highlight_background(string)
    ;       highlight_color(string)
    ;       highlight_thickness(int)
    ;       height(int)
    ;       indicator_color(string)
    ;       indicator_on(bool)
    ;       insert_background(string)
    ;       insert_border_width(int)
    ;       insert_off_time(int)
    ;       insert_on_time(int)
    ;       insert_width(int)
    ;       jump(bool)
    ;       justify(string)
    ;       label(string)
    ;       menu(widget)
    ;       multiple_select(bool)
    ;       orient(orientation)
    ;       outline_color(string)
    ;       padx(int)
    ;       pady(int)
    ;       relief(string)
    ;       repeat_delay(int)
    ;       repeat_interval(int)
    ;       scale_height(int)
    ;       scale_range(int, int)
    ;       scale_text(string)
    ;       screen(string)
    ;       scroll_region(int, int)
    ;       select_background(string)
    ;       select_border_width(int)
    ;       select_foreground(string)
    ;       show(bool)
    ;       slider_length(int)
    ;       take_focus(bool)
    ;       text(string)
    ;       text_variable(string)
    ;       tick_interval(int)
    ;       title(string)
    ;       trough_color(string)
    ;       underline(int)
    ;       width(int)
    ;       winposition(int, int)
    ;       winsize(int, int)
    ;       wrap(bool)
    ;       wrap_length(int).

:- inst config for config/0
    --->    active(ground)
    ;       active_background(ground)
    ;       active_foreground(ground)
    ;       active_relief(ground)
    ;       anchor(ground)
    ;       aspect(ground)
    ;       background(ground)
    ;       bitmap(ground)
    ;       border_width(ground)
    ;       command(pred(in, di, uo) is det)
    ;       cursor(ground)
    ;       disabled_foreground(ground)
    ;       element_border_width(ground)
    ;       export_selection(ground)
    ;       fill_color(ground)
    ;       font(ground)
    ;       foreground(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       height(ground)
    ;       indicator_color(ground)
    ;       indicator_on(ground)
    ;       insert_background(ground)
    ;       insert_border_width(ground)
    ;       insert_off_time(ground)
    ;       insert_on_time(ground)
    ;       insert_width(ground)
    ;       jump(ground)
    ;       justify(ground)
    ;       label(ground)
    ;       menu(bound(menu(ground)))
    ;       multiple_select(ground)
    ;       orient(ground)
    ;       outline_color(ground)
    ;       padx(ground)
    ;       pady(ground)
    ;       relief(ground)
    ;       repeat_delay(ground)
    ;       repeat_interval(ground)
    ;       scale_height(ground)
    ;       scale_range(ground, ground)
    ;       scale_text(ground)
    ;       screen(ground)
    ;       scroll_region(ground, ground)
    ;       select_background(ground)
    ;       select_border_width(ground)
    ;       select_foreground(ground)
    ;       show(ground)
    ;       slider_length(ground)
    ;       take_focus(ground)
    ;       text(ground)
    ;       text_variable(ground)
    ;       tick_interval(ground)
    ;       title(ground)
    ;       trough_color(ground)
    ;       underline(ground)
    ;       width(ground)
    ;       winposition(ground, ground)
    ;       winsize(ground, ground)
    ;       wrap(ground)
    ;       wrap_length(ground).

:- type orientation
    --->    horiz
    ;       vert.

:- inst toplevel for widget/0
    --->    window(ground).

    % A constant to access the root window in the Tk hierarchy,
    % comp. John K. Ousterhout, Tcl and the Tk Toolkit, 1994, p.151.
    % The root window is the one designated as "." in Tk way of speaking.
    % It is necessary to access not only a top level window created by the
    % application, but the main application window provided by the framework.
    %
:- func root_window = (widget::out(toplevel)) is det.

:- inst window for widget/0
    --->    window(ground)
    ;       frame(ground).

:- inst button for widget/0
    --->    button(ground).

:- inst button_config for config/0
    --->    active(ground)
    ;       active_foreground(ground)
    ;       active_background(ground)
    ;       anchor(ground)
    ;       background(ground)
    ;       bitmap(ground)
    ;       border_width(ground)
    ;       command(pred(in, di, uo) is det)
    ;       cursor(ground)
    ;       disabled_foreground(ground)
    ;       font(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       height(ground)
    ;       justify(ground)
    ;       padx(ground)
    ;       pady(ground)
    ;       relief(ground)
    ;       take_focus(ground)
    ;       text(ground)
    ;       text_variable(ground)
    ;       underline(ground)
    ;       width(ground)
    ;       wrap_length(ground).

:- inst canvas for widget/0
    --->    canvas(ground).

:- inst entry for widget/0
    --->    entry(ground).

:- inst entry_config for config/0
    --->    active(ground)
    ;       background(ground)
    ;       border_width(ground)
    ;       cursor(ground)
    ;       export_selection(ground)
    ;       font(ground)
    ;       foreground(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       height(ground)
    ;       insert_background(ground)
    ;       insert_border_width(ground)
    ;       insert_off_time(ground)
    ;       insert_on_time(ground)
    ;       insert_width(ground)
    ;       justify(ground)
    ;       relief(ground)
    ;       select_background(ground)
    ;       select_border_width(ground)
    ;       select_foreground(ground)
    ;       show(ground)
    ;       take_focus(ground)
    ;       width(ground).

:- inst label for widget/0
    --->    label(ground).

:- inst listbox for widget/0
    --->    listbox(ground).

:- inst listbox_config for config/0
    --->    background(ground)
    ;       border_width(ground)
    ;       cursor(ground)
    ;       export_selection(ground)
    ;       font(ground)
    ;       foreground(ground)
    ;       height(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       multiple_select(ground)
    ;       relief(ground)
    ;       select_background(ground)
    ;       select_border_width(ground)
    ;       select_foreground(ground)
    ;       take_focus(ground)
    ;       width(ground).

:- inst radiobutton for widget/0
    --->    radiobutton(ground).

:- inst scrollbar for widget/0
    --->    scrollbar(ground).

:- inst scrollbar_config for config/0
    --->    active_background(ground)
    ;       active_relief(ground)
    ;       background(ground)
    ;       border_width(ground)
    ;       cursor(ground)
    ;       element_border_width(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       jump(ground)
    ;       orient(ground)
    ;       relief(ground)
    ;       repeat_delay(ground)
    ;       repeat_interval(ground)
    ;       select_background(ground)
    ;       select_border_width(ground)
    ;       select_foreground(ground)
    ;       take_focus(ground)
    ;       trough_color(ground)
    ;       width(ground).

:- inst text for widget/0
    --->    text(ground).

:- inst text_config for config/0
    --->    active(ground)
    ;       background(ground)
    ;       border_width(ground)
    ;       cursor(ground)
    ;       export_selection(ground)
    ;       font(ground)
    ;       foreground(ground)
    ;       height(ground)
    ;       highlight_background(ground)
    ;       highlight_color(ground)
    ;       highlight_thickness(ground)
    ;       insert_background(ground)
    ;       insert_border_width(ground)
    ;       insert_off_time(ground)
    ;       insert_on_time(ground)
    ;       insert_width(ground)
    ;       padx(ground)
    ;       pady(ground)
    ;       relief(ground)
    ;       select_background(ground)
    ;       select_border_width(ground)
    ;       select_foreground(ground)
    ;       take_focus(ground)
    ;       width(ground)
    ;       wrap(ground).

:- type anchor
    --->    n ; ne ; e ; se ; s ; sw ; w ; nw ; c.

:- pred window(tcl_interp::in, list(config)::in(list(config)),
    widget::out(toplevel), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Button widget.
%

:- pred button(tcl_interp::in, list(config)::in(list(button_config)),
    widget::in(window), widget::out(button), io::di, io::uo) is det.

:- pred flash(tcl_interp::in, widget::in(button), io::di, io::uo) is det.

:- pred invoke(tcl_interp::in, widget::in(button), io::di, io::uo) is det.

:- pred enable(tcl_interp::in, widget::in(button), io::di, io::uo) is det.

:- pred disable(tcl_interp::in, widget::in(button), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Canvas widget.
%

:- pred canvas(tcl_interp::in, list(config)::in(list(config)),
    widget::in(window), widget::out(canvas), io::di, io::uo) is det.

:- type canvas_item.

:- type canvas_item_type
    --->    arc(int, int, int, int)
    ;       bitmap(int, int)
    ;       image(int, int)
    ;       line(list(pair(int, int)))
    ;       oval(int, int, int, int)
    ;       polygon(list(pair(int, int)))
    ;       rectangle(int, int, int, int)
    ;       text(int, int).

:- type item_config
    --->    anchor(anchor)
    ;       background(string)
    ;       bitmap(string)
    ;       extent(int)
    ;       fill(string)
    ;       foreground(string)
    ;       outline(string)
    ;       start(int)
    ;       type(string)
    ;       width(int).

:- type item_spec
    --->    item(canvas_item)
    ;       (all).

:- pred create(tcl_interp::in, widget::in, canvas_item_type::in,
    list(item_config)::in, canvas_item::out, io::di, io::uo) is det.

:- pred canvas_delete(tcl_interp::in, widget::in, item_spec::in,
    io::di, io::uo) is det.

:- pred canvas_item_configure(tcl_interp::in, widget::in, canvas_item::in,
    list(item_config)::in, io::di, io::uo) is det.

:- pred canvas_item_coords(tcl_interp::in, widget::in, canvas_item::in,
    int::in, int::in, io::di, io::uo) is det.

:- pred canvas_move(tcl_interp::in, widget::in, canvas_item::in,
    int::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Entry widget.
%

:- pred entry(tcl_interp::in, list(config)::in(list(entry_config)),
    widget::in(window), widget::out(entry), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Frame widget.
%

:- pred frame(tcl_interp::in, list(config)::in(list(config)),
    widget::in(window), widget::out(window), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Label widget.
%

:- pred label(tcl_interp::in, list(config)::in(list(config)),
    widget::in(window), widget::out(label), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Listbox widget.
%

:- pred listbox(tcl_interp::in, list(config)::in(list(listbox_config)),
    widget::in(window), widget::out(listbox), io::di, io::uo) is det.

:- pred link(tcl_interp::in, widget::in(listbox), widget::in(scrollbar),
    io::di, io::uo) is det.

:- type insertion
    --->    start
    ;       at(int)
    ;       end.

:- pred insert(tcl_interp::in, widget::in(listbox), insertion::in, string::in,
    io::di, io::uo) is det.

:- pred delete(tcl_interp::in, widget::in(listbox), insertion::in,
    io::di, io::uo) is det.

:- pred delete_all(tcl_interp::in, widget::in(listbox),
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Menus.
%

:- inst menubutton for widget/0
    --->    menubutton(ground).

:- pred menubutton(tcl_interp::in, list(config)::in(list(config)),
    widget::in(window), widget::out(menubutton), io::di, io::uo) is det.

:- inst menu for widget/0
    --->    menu(ground).

:- pred menu(tcl_interp::in, list(config)::in(list(config)),
    widget::in(menubutton), widget::out(menu), io::di, io::uo) is det.

:- type menu_item
    --->    command
    ;       separator.

:- pred add_menu_item(tcl_interp::in, widget::in(menu), menu_item::in,
    list(config)::in(list(config)), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Radiobutton widget.
%

:- type radio_group.

:- pred new_radio_group(tcl_interp::in, radio_group::out,
    io::di, io::uo) is det.

:- pred set_radio_group(tcl_interp::in, radio_group::in, string::in,
    io::di, io::uo) is det.

:- pred radiobutton(tcl_interp::in, list(config)::in(list(config)),
    widget::in(window), radio_group::in, string::in, widget::out(radiobutton),
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Scrollbar widget.
%

:- pred scrollbar(tcl_interp::in, list(config)::in(list(scrollbar_config)),
    widget::in(window), widget::out(scrollbar), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Text widget.
%

:- pred text(tcl_interp::in, list(config)::in(list(text_config)),
    widget::in(window), widget::out(text), io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type pack_option
    --->    top
    ;       bottom
    ;       left
    ;       right
    ;       expand
    ;       fill_x
    ;       fill_y
    ;       fill_both.

:- type pack_item
    --->    pack(widget, list(pack_option)).

:- type grid_option
    --->    row(int)
    ;       col(int).

:- type grid_item
    --->    grid(widget, list(grid_option)).

:- pred pack(tcl_interp::in, list(pack_item)::in, io::di, io::uo) is det.

:- pred grid(tcl_interp::in, list(grid_item)::in, io::di, io::uo) is det.

:- pred layout_grid(tcl_interp::in, list(list(maybe(widget)))::in,
    io::di, io::uo) is det.

:- pred destroy(tcl_interp::in, widget::in, io::di, io::uo) is det.

:- type (event)
    --->    event(list(event_modifier), event_type).

:- type event_modifier
    --->    control
    ;       shift
    ;       lock
    ;       button1
    ;       button2
    ;       button3
    ;       button4
    ;       button5
    ;       mod1
    ;       mod2
    ;       mod3
    ;       mod4
    ;       mod5
    ;       meta
    ;       alt
    ;       double
    ;       triple.

:- type event_type
    --->    button(int) % press
    ;       button_release(int)
    ;       circulate
    ;       colormap
    ;       configure
    ;       destroy
    ;       enter
    ;       expose
    ;       focus_in
    ;       focus_out
    ;       gravity
    ;       key %press
    ;       key_release
    ;       leave
    ;       map
    ;       motion
    ;       property
    ;       reparent
    ;       unmap
    ;       visibility.

:- type event_data_spec
    --->    button
    ;       key
    ;       x
    ;       y.

:- type event_data
    --->    button(int)
    ;       key(int)
    ;       x(int)
    ;       y(int).

:- pred bind(tcl_interp::in, widget::in, (event)::in,
    list(event_data_spec)::in,
    pred(tcl_interp, list(event_data), io, io)::in(pred(in, in, di, uo) is det),
    io::di, io::uo) is det.

:- pred configure(tcl_interp::in, widget::in, list(config)::in(list(config)),
    io::di, io::uo) is det.

:- pred get(tcl_interp, widget, string, io, io).
:- mode get(in, in(entry), out, di, uo) is det.
:- mode get(in, in(text), out, di, uo) is det.

:- pred set(tcl_interp, widget, string, io, io).
:- mode set(in, in(entry), in, di, uo) is det.
:- mode set(in, in(text), in, di, uo) is det.

:- pred get_selection(tcl_interp::in, widget::in, string::out,
    io::di, io::uo) is det.

:- pred ensure_window(widget::in, widget::out(window)) is det.

% XXX hack until we can implement cget.
:- pred unbind_command(tcl_interp::in, widget::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

%------------------------------------------------------------------------------%

:- interface.

    % This type should be private but its constructors are referred to
    % by insts in the interface of this module.
:- type widget
    --->    window(wpath)
    ;       frame(wpath)
    ;       button(wpath)
    ;       canvas(wpath)
    ;       entry(wpath)
    ;       label(wpath)
    ;       listbox(wpath)
    ;       menubutton(wpath)
    ;       menu(wpath)
    ;       radiobutton(wpath)
    ;       scrollbar(wpath)
    ;       text(wpath).

:- type wpath == string.

%------------------------------------------------------------------------------%

:- implementation.

:- type canvas_item == string.

%------------------------------------------------------------------------------%

% The straightforward implementation of root_window.
root_window = window(".").

window(Interp, Configs, window(Widget), !IO) :-
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format(".toplevel%d", [i(Id)], Widget),
    string.format("toplevel %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Button widget.
%

button(Interp, Configs, ParentWidget, button(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.button%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("button %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

flash(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s flash", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

invoke(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s invoke", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

enable(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s configure -state normal", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

disable(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s configure -state disabled", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Canvas widget.
%

canvas(Interp, Configs, ParentWidget, canvas(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.canvas%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("canvas %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

create(Interp, Widget, ItemType, Configs, CanvasItem, !IO) :-
    unwrap(Widget, Path),
    stringify_item_configs(Configs, ConfStr),
    stringify_itemtype(ItemType, ItemStr),
    string.format("%s create %s %s", [s(Path), s(ItemStr), s(ConfStr)],
        CmdStr),
    eval(Interp, CmdStr, Res, CanvasItem, !IO),
    throw_on_tcl_error(Res, CanvasItem, !IO).

canvas_delete(Interp, Widget, item(CanvasItem), !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete %s", [s(Path), s(CanvasItem)], CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).
canvas_delete(Interp, Widget, all, !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete all", [s(Path)], CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

canvas_item_configure(Interp, Widget, Item, Configs, !IO) :-
    unwrap(Widget, Path),
    stringify_item_configs(Configs, ConfStr),
    string.format("%s itemconfigure %s %s", [s(Path), s(Item), s(ConfStr)],
        CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

canvas_item_coords(Interp, Widget, Item, Dx, Dy, !IO) :-
    unwrap(Widget, Path),
    string.format("%s coords %s %d %d", [s(Path), s(Item), i(Dx), i(Dy)],
        CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

canvas_move(Interp, Widget, Item, Dx, Dy, !IO) :-
    unwrap(Widget, Path),
    string.format("%s move %s %d %d", [s(Path), s(Item), i(Dx), i(Dy)],
        CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

:- pred stringify_itemtype(canvas_item_type::in, string::out) is det.

stringify_itemtype(arc(X1, Y1, X2, Y2), Str) :-
    string.format("arc %d %d %d %d", [i(X1), i(Y1), i(X2), i(Y2)], Str).
stringify_itemtype(bitmap(X, Y), Str) :-
    string.format("bitmap %d %d", [i(X), i(Y)], Str).
stringify_itemtype(image(X, Y), Str) :-
    string.format("image %d %d", [i(X), i(Y)], Str).
stringify_itemtype(line(Points), Str) :-
    stringify_points(Points, PointStr),
    string.append("line ", PointStr, Str).
stringify_itemtype(oval(X1, Y1, X2, Y2), Str) :-
    string.format("oval %d %d %d %d", [i(X1), i(Y1), i(X2), i(Y2)], Str).
stringify_itemtype(polygon(Points), Str) :-
    stringify_points(Points, PointStr),
    string.append("polygon ", PointStr, Str).
stringify_itemtype(rectangle(X1, Y1, X2, Y2), Str) :-
    string.format("rectangle %d %d %d %d", [i(X1),i(Y1),i(X2),i(Y2)], Str).
stringify_itemtype(text(X, Y), Str) :-
    string.format("text %d %d", [i(X), i(Y)], Str).

:- pred stringify_points(list(pair(int, int))::in, string::out) is det.

stringify_points([], "").
stringify_points([X - Y], Str) :-
    string.format("%d %d", [i(X), i(Y)], Str).
stringify_points([X - Y | Rest], Str) :-
    Rest = [_ | _],
    stringify_points(Rest, RestStr),
    string.format("%d %d ", [i(X), i(Y)], ThisStr),
    string.append(ThisStr, RestStr, Str).

:- pred stringify_item_configs(list(item_config)::in, string::out) is det.

stringify_item_configs([], "").
stringify_item_configs([Cfg | Cfgs], Str) :-
    (
        Cfg = anchor(Anc),
        stringify_anchor(Anc, AStr),
        string.format("-anchor %s ", [s(AStr)], CfgStr)
    ;
        Cfg = background(Col),
        string.format("-background %s ", [s(Col)], CfgStr)
    ;
        Cfg = bitmap(Bitm),
        string.format("-bitmap %s ", [s(Bitm)], CfgStr)
    ;
        Cfg = extent(Ext),
        string.format("-extent %d ", [i(Ext)], CfgStr)
    ;
        Cfg = fill(Col),
        string.format("-fill %s ", [s(Col)], CfgStr)
    ;
        Cfg = foreground(Col),
        string.format("-foreground %s ", [s(Col)], CfgStr)
    ;
        Cfg = outline(Col),
        string.format("-outline %s ", [s(Col)], CfgStr)
    ;
        Cfg = start(St),
        string.format("-width %d ", [i(St)], CfgStr)
    ;
        Cfg = type(Col),
        string.format("-type %s ", [s(Col)], CfgStr)
    ;
        Cfg = width(Int),
        string.format("-width %d ", [i(Int)], CfgStr)
    ),
    stringify_item_configs(Cfgs, Str0),
    string.append(CfgStr, Str0, Str).

%------------------------------------------------------------------------------%
%
% Entry widget.
%

entry(Interp, Configs, ParentWidget, entry(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.entry%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("entry %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Frame widget.
%

frame(Interp, Configs, ParentWidget, frame(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.frame%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("frame %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Label widget.
%

label(Interp, Configs, ParentWidget, label(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.label%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("label %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Listbox widget.
%

listbox(Interp, Configs, ParentWidget, listbox(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.listbox%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("listbox %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

link(Interp, ListBoxWidget, ScrollBarWidget, !IO) :-
    unwrap(ListBoxWidget, ListBox),
    unwrap(ScrollBarWidget, ScrollBar),
    string.format("%s configure -yscroll ""%s set""",
        [s(ListBox), s(ScrollBar)], Str1),
    eval(Interp, Str1, Res1, ResStr1, !IO),
    throw_on_tcl_error(Res1, ResStr1, !IO),
    string.format("%s configure -command ""%s yview""",
        [s(ScrollBar), s(ListBox)], Str2),
    eval(Interp, Str2, Res2, ResStr2, !IO),
    throw_on_tcl_error(Res2, ResStr2, !IO).

insert(Interp, Widget, start, Text, !IO) :-
    unwrap(Widget, Path),
    string.format("%s insert 0 ""%s""", [s(Path), s(Text)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).
insert(Interp, Widget, at(N), Text, !IO) :-
    unwrap(Widget, Path),
    string.format("%s insert %d ""%s""", [s(Path), i(N), s(Text)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).
insert(Interp, Widget, end, Text, !IO) :-
    unwrap(Widget, Path),
    string.format("%s insert end ""%s""", [s(Path), s(Text)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

delete(Interp, Widget, start, !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete 0", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).
delete(Interp, Widget, at(N), !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete %d", [s(Path), i(N)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).
delete(Interp, Widget, end, !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete end", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

delete_all(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s delete 0 end", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

get_selection(Interp, _Widget, Selection, !IO) :-
    eval(Interp, "selection get", Res, Selection, !IO),
    throw_on_tcl_error(Res, Selection, !IO).

%------------------------------------------------------------------------------%

menubutton(Interp, Configs, ParentWidget, menubutton(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.menubutton%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("menubutton %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

menu(Interp, Configs, ParentWidget, menu(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.menu%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("menu %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

add_menu_item(Interp, Widget, command, Configs, !IO) :-
    unwrap(Widget, Path),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.format("%s add command %s", [s(Path), s(Str1)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

add_menu_item(Interp, Widget, separator, Configs, !IO) :-
    unwrap(Widget, Path),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.format("%s add separator %s", [s(Path), s(Str1)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%

:- type radio_group == string.

new_radio_group(_Interp, RadioVar, !IO) :-
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("radiovar%d", [i(Id)], RadioVar).

set_radio_group(Interp, RadVar, Value, !IO) :-
    string.format("set %s %s", [s(RadVar), s(Value)], CmdStr),
    eval(Interp, CmdStr, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

radiobutton(Interp, Configs, ParentWidget, RadioVar, Value,
        radiobutton(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.radiobutton%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("radiobutton %s -indicatoron 0 -variable %s -value %s ",
        [s(Widget), s(RadioVar), s(Value)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Scrollbar widget.
%

scrollbar(Interp, Configs, ParentWidget, scrollbar(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.scrollbar%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("scrollbar %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%
%
% Text widget.
%

text(Interp, Configs, ParentWidget, text(Widget), !IO) :-
    unwrap(ParentWidget, Parent),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("%s.text%d", [s(no_dot_wpath(Parent)), i(Id)], Widget),
    string.format("text %s ", [s(Widget)], Str0),
    stringify_configs(Interp, Configs, Str1, !IO),
    string.append(Str0, Str1, Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%

pack(_Interp, [], !IO).
pack(Interp, [Item | Items], !IO) :-
    pack_item(Interp, Item, !IO),
    pack(Interp, Items, !IO).

:- pred pack_item(tcl_interp::in, pack_item::in, io::di, io::uo) is det.

pack_item(Interp, PackItem, !IO) :-
    PackItem = pack(Widget, Options),
    unwrap(Widget, Path),
    stringify_pack_options(Options, OptStr),
    string.format("pack %s %s", [s(Path), s(OptStr)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

:- pred stringify_pack_options(list(pack_option)::in, string::out) is det.

stringify_pack_options([], "").
stringify_pack_options([Opt], Str) :-
    stringify_pack_option(Opt, Str).
stringify_pack_options([Opt | Opts], Str) :-
    Opts = [_ | _],
    stringify_pack_option(Opt, Str0),
    stringify_pack_options(Opts, Str1),
    string.format("%s %s", [s(Str0), s(Str1)], Str).

:- pred stringify_pack_option(pack_option::in, string::out) is det.

stringify_pack_option(left, "-side left").
stringify_pack_option(right, "-side right").
stringify_pack_option(top, "-side top").
stringify_pack_option(bottom, "-side bottom").
stringify_pack_option(expand, "-expand 1").
stringify_pack_option(fill_x, "-fill x").
stringify_pack_option(fill_y, "-fill y").
stringify_pack_option(fill_both, "-fill both").

%------------------------------------------------------------------------------%

grid(_Interp, [], !IO).
grid(Interp, [Item | Items], !IO) :-
    grid_item(Interp, Item, !IO),
    grid(Interp, Items, !IO).

:- pred grid_item(tcl_interp::in, grid_item::in, io::di, io::uo) is det.

grid_item(Interp, PackItem, !IO) :-
    PackItem = grid(Widget, Options),
    unwrap(Widget, Path),
    stringify_grid_options(Options, OptStr),
    string.format("grid %s %s", [s(Path), s(OptStr)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

:- pred stringify_grid_options(list(grid_option)::in, string::out) is det.

stringify_grid_options([], "").
stringify_grid_options([Opt], Str) :-
    stringify_grid_option(Opt, Str).
stringify_grid_options([Opt | Opts], Str) :-
    Opts = [_ | _],
    stringify_grid_option(Opt, Str0),
    stringify_grid_options(Opts, Str1),
    string.format("%s %s", [s(Str0), s(Str1)], Str).

:- pred stringify_grid_option(grid_option::in, string::out) is det.

stringify_grid_option(row(I), Str) :-
    string.format("-row %d", [i(I)], Str).
stringify_grid_option(col(I), Str) :-
    string.format("-column %d", [i(I)], Str).

%------------------------------------------------------------------------------%

layout_grid(Interp, Widgets, !IO) :-
    layout_grid2(Interp, Widgets, 1, !IO).

:- pred layout_grid2(tcl_interp::in, list(list(maybe(widget)))::in, int::in,
    io::di, io::uo) is det.

layout_grid2(_Interp, [], _, !IO).
layout_grid2(Interp, [Row|Rows], N, !IO) :-
    layout_grid3(Interp, Row, N, 1, !IO),
    layout_grid2(Interp, Rows, N + 1, !IO).

:- pred layout_grid3(tcl_interp::in, list(maybe(widget))::in, int::in, int::in,
    io::di, io::uo) is det.

layout_grid3(_Interp, [], _, _, !IO).
layout_grid3(Interp, [MWid | MWids], N, M, !IO) :-
    (
        MWid = yes(Widget),
        grid(Interp, [grid(Widget, [row(N), col(M)])], !IO)
    ;
        MWid = no
    ),
    layout_grid3(Interp, MWids, N, M + 1, !IO).

%------------------------------------------------------------------------------%

destroy(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("destroy %s", [s(Path)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%

bind(Interp, Widget, Event, EDataSpecs, Closure, !IO) :-
    unwrap(Widget, Path),
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("cmd%d", [i(Id)], CmdName),
    stringify_event(Event, EventStr),
    stringify_edata_specs(EDataSpecs, EDataStr),
    create_command(Interp, CmdName, bind_wrapper(Closure, EDataSpecs), !IO),
    string.format("bind %s %s { %s %s }", [s(Path), s(EventStr),
        s(CmdName), s(EDataStr)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

:- pred bind_wrapper(
    pred(tcl_interp, list(event_data), io, io)::in(pred(in, in, di, uo) is det),
    list(event_data_spec)::in, tcl_interp::in, list(string)::in,
    tcl_status::out, string::out, io::di, io::uo) is det.

bind_wrapper(Closure, EDataSpecs, Interp, Args0, tcl_ok, "", !IO) :-
    (
        Args0 = [],
        error("bind_wrapper: no args")
    ;
        Args0 = [_ | Args],
        decode_edata(EDataSpecs, Args, EData),
        Closure(Interp, EData, !IO)
    ).

:- pred stringify_event((event)::in, string::out) is det.

stringify_event(event(Mods, Type), Str) :-
    stringify_event_type(Type, TypeStr),
    (
        Mods = [],
        string.format("<%s>", [s(TypeStr)], Str)
    ;
        Mods = [_ | _],
        stringify_mods(Mods, ModsStr),
        string.format("<%s%s>", [s(ModsStr), s(TypeStr)], Str)
    ).

:- pred stringify_event_type(event_type::in, string::out) is det.

stringify_event_type(button(N), Str) :-
    string.format("Button-%d", [i(N)], Str).
stringify_event_type(button_release(N), Str) :-
    string.format("ButtonRelease-%d", [i(N)], Str).
stringify_event_type(circulate, "Circulate").
stringify_event_type(colormap, "ColorMap").
stringify_event_type(configure, "Configure").
stringify_event_type(destroy, "Destroy").
stringify_event_type(enter, "Enter").
stringify_event_type(expose, "Expose").
stringify_event_type(focus_in, "FocusIn").
stringify_event_type(focus_out, "FocusOut").
stringify_event_type(gravity, "Gravity").
stringify_event_type(key, "Key").                       % XXX add keysyms
stringify_event_type(key_release, "KeyRelease").
stringify_event_type(leave, "Leave").
stringify_event_type(map, "Map").
stringify_event_type(motion, "Motion").
stringify_event_type(property, "Property").
stringify_event_type(reparent, "Reparent").
stringify_event_type(unmap, "Unmap").
stringify_event_type(visibility, "Visibility").

:- pred stringify_mods(list(event_modifier)::in, string::out) is det.

stringify_mods([], "").
stringify_mods([Mod | Mods], Str) :-
    stringify_mods(Mods, Str0),
    stringify_mod(Mod, Str1),
    string.format("%s-%s", [s(Str1), s(Str0)], Str).

:- pred stringify_mod(event_modifier::in, string::out) is det.

stringify_mod(control, "Control").
stringify_mod(shift, "Shift").
stringify_mod(lock, "Lock").
stringify_mod(button1, "Button1").
stringify_mod(button2, "Button2").
stringify_mod(button3, "Button3").
stringify_mod(button4, "Button4").
stringify_mod(button5, "Button5").
stringify_mod(mod1, "Mod1").
stringify_mod(mod2, "Mod2").
stringify_mod(mod3, "Mod3").
stringify_mod(mod4, "Mod4").
stringify_mod(mod5, "Mod5").
stringify_mod(meta, "Meta").
stringify_mod(alt, "Alt").
stringify_mod(double, "Double").
stringify_mod(triple, "Triple").

:- pred stringify_edata_specs(list(event_data_spec)::in, string::out) is det.

stringify_edata_specs([], "").
stringify_edata_specs([Spec|Rest], Str) :-
    ( Spec = button,  SpecStr = "%b "
    ; Spec = key,     SpecStr = "%k "
    ; Spec = x,       SpecStr = "%x "
    ; Spec = y,       SpecStr = "%y "
    ),
    stringify_edata_specs(Rest, Str0),
    string.append(SpecStr, Str0, Str).

:- pred decode_edata(list(event_data_spec)::in, list(string)::in,
    list(event_data)::out) is det.

decode_edata([], [], []).
decode_edata([_ | _], [], _) :-
    error("decode_edata: ran out of args").
decode_edata([], [_ | _], _) :-
    error("decode_edata: too many args").
decode_edata([Spec | Specs], [Str | Strs], [Datum | Data]) :-
    decode_edata(Specs, Strs, Data),
    (
        Spec = button,
        ( if string.to_int(Str, Int) then
            Datum = button(Int)
        else
            error("decode_edata: data format error")
        )
    ;
        Spec = key,
        ( if string.to_int(Str, Int) then
            Datum = key(Int)
        else
            error("decode_edata: data format error")
        )
    ;
        Spec = x,
        ( if string.to_int(Str, Int) then
            Datum = x(Int)
        else
            error("decode_edata: data format error")
        )
    ;
        Spec = y,
        ( if string.to_int(Str, Int) then
            Datum = y(Int)
        else
            error("decode_edata: data format error")
        )
    ).

%------------------------------------------------------------------------------%

configure(Interp, Widget, Configs, !IO) :-
    unwrap(Widget, Path),
    stringify_configs(Interp, Configs, ConfStr, !IO),
    string.format("%s configure %s", [s(Path), s(ConfStr)], Str),
    eval(Interp, Str, Res, ResStr, !IO),
    throw_on_tcl_error(Res, ResStr, !IO).

%------------------------------------------------------------------------------%

ensure_window(Widget0, Widget) :-
    ( if
        ( Widget0 = window(_)
        ; Widget0 = frame(_)
        )
    then
        Widget = Widget0
    else
        error("ensure_window: not a window")
    ).

%------------------------------------------------------------------------------%

get(Interp, entry(Path), String, !IO) :-
    string.format("%s get", [s(Path)], CmdStr),
    eval(Interp, CmdStr, Res, String, !IO),
    throw_on_tcl_error(Res, String, !IO).
get(Interp, text(Path), String, !IO) :-
    string.format("%s get 1.0 end", [s(Path)], CmdStr),
    eval(Interp, CmdStr, Res, String, !IO),
    throw_on_tcl_error(Res, String, !IO).

set(Interp, entry(Path), String, !IO) :-
    string.format("%s delete 0 end", [s(Path)], DelStr),
    eval(Interp, DelStr, DelRes, DelString, !IO),
    throw_on_tcl_error(DelRes, DelString, !IO),
    string.format("%s insert 0 ""%s""", [s(Path), s(String)], InsStr),
    eval(Interp, InsStr, InsRes, InsString, !IO),
    throw_on_tcl_error(InsRes, InsString, !IO).
set(Interp, text(Path), String, !IO) :-
    string.format("%s delete 1.0 end", [s(Path)], DelStr),
    eval(Interp, DelStr, DelRes, DelString, !IO),
    throw_on_tcl_error(DelRes, DelString, !IO),
    string.format("%s insert 1.0 ""%s""", [s(Path), s(String)], InsStr),
    eval(Interp, InsStr, InsRes, InsString, !IO),
    throw_on_tcl_error(InsRes, InsString, !IO).

%------------------------------------------------------------------------------%

unbind_command(Interp, Widget, !IO) :-
    unwrap(Widget, Path),
    string.format("%s cget -command", [s(Path)], Cmd1),
    eval(Interp, Cmd1, Res1, ResStr1, !IO),
    throw_on_tcl_error(Res1, ResStr1, !IO),
    ( if ResStr1 \= "" then
        delete_command(Interp, ResStr1, _, !IO)
    else
        true
    ).

%------------------------------------------------------------------------------%

:- pred stringify_configs(tcl_interp::in, list(config)::in(list(config)),
    string::out, io::di, io::uo) is det.

stringify_configs(_Interp, [], "", !IO).
stringify_configs(Interp, [Config | Configs], Str, !IO) :-
    stringify_configs(Interp, Configs, Str0, !IO),
    stringify_config(Interp, Config, Str1, !IO),
    ( if Str0 \= "" then
        string.append(" ", Str0, Str2),
        string.append(Str1, Str2, Str)
    else
        Str = Str1
    ).

:- pred stringify_config(tcl_interp::in, config::in(config),
    string::out, io::di, io::uo) is det.

stringify_config(_Interp, active(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "normal"
    ;
        Indicator = no,
        IndicatorStr = "disabled"
    ),
    string.format("-state %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, active_background(Color), Str, !IO) :-
    string.format("-activebackground %s", [s(Color)], Str).

stringify_config(_Interp, active_foreground(Color), Str, !IO) :-
    string.format("-activeforeground %s", [s(Color)], Str).

stringify_config(_Interp, active_relief(Relief), Str, !IO) :-
    string.format("-activerelief %s", [s(Relief)], Str).

stringify_config(_Interp, anchor(Ank), Str, !IO) :-
    stringify_anchor(Ank, AnkStr),
    string.format("-anchor %s", [s(AnkStr)], Str).

stringify_config(_Interp, aspect(Asp), Str, !IO) :-
    string.format("-aspect %d", [i(Asp)], Str).

stringify_config(_Interp, background(Color), Str, !IO) :-
    string.format("-background %s", [s(Color)], Str).

stringify_config(_Interp, bitmap(Bitmap), Str, !IO) :-
    string.format("-bitmap %s", [s(Bitmap)], Str).

stringify_config(_Interp, border_width(Wdth), Str, !IO) :-
    string.format("-borderwidth %d", [i(Wdth)], Str).

stringify_config(Interp, command(Closure), Str, !IO) :-
    get_thingy_counter(Id, !IO),
    set_thingy_counter(Id + 1, !IO),
    string.format("cmd%d", [i(Id)], CmdName),
    create_command(Interp, CmdName, command_wrapper(Closure), !IO),
    string.format("-command %s", [s(CmdName)], Str).

stringify_config(_Interp, cursor(Curse), Str, !IO) :-
    string.format("-cursor %s", [s(Curse)], Str).

stringify_config(_Interp, disabled_foreground(Color), Str, !IO) :-
    string.format("-disabledforeground %s", [s(Color)], Str).

stringify_config(_Interp, element_border_width(Height), Str, !IO) :-
    string.format("-elementborderwidth %d", [i(Height)], Str).

stringify_config(_Interp, export_selection(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "yes"
    ;
        Indicator = no,
        IndicatorStr = "no"
    ),
    string.format("-export_selection %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, fill_color(Color), Str, !IO) :-
    string.format("-fill %s", [s(Color)], Str).

stringify_config(_Interp, font(Font), Str, !IO) :-
    string.format("-font %s", [s(Font)], Str).

stringify_config(_Interp, foreground(Color), Str, !IO) :-
    string.format("-foreground %s", [s(Color)], Str).

stringify_config(_Interp, height(Height), Str, !IO) :-
    string.format("-height %d", [i(Height)], Str).

stringify_config(_Interp, highlight_background(Color), Str, !IO) :-
    string.format("-highlightbackground %s", [s(Color)], Str).

stringify_config(_Interp, highlight_color(Color), Str, !IO) :-
    string.format("-highlightcolor %s", [s(Color)], Str).

stringify_config(_Interp, highlight_thickness(Width), Str, !IO) :-
    string.format("-highlightthickness %d", [i(Width)], Str).

stringify_config(_Interp, indicator_color(Color), Str, !IO) :-
    string.format("-selectcolor %s", [s(Color)], Str).

stringify_config(_Interp, indicator_on(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "yes"
    ;
        Indicator = no,
        IndicatorStr = "no"
    ),
    string.format("-indicatoron %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, insert_background(Color), Str, !IO) :-
    string.format("-insertbackground %s", [s(Color)], Str).

stringify_config(_Interp, insert_border_width(Val), Str, !IO) :-
    string.format("-insertborderwidth %d", [i(Val)], Str).

stringify_config(_Interp, insert_off_time(Val), Str, !IO) :-
    string.format("-insertofftime %d", [i(Val)], Str).

stringify_config(_Interp, insert_on_time(Val), Str, !IO) :-
    string.format("-insertontime %d", [i(Val)], Str).

stringify_config(_Interp, insert_width(Val), Str, !IO) :-
    string.format("-insertwidth %d", [i(Val)], Str).

stringify_config(_Interp, jump(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "yes"
    ;
        Indicator = no,
        IndicatorStr = "no"
    ),
    string.format("-jump %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, justify(Just), Str, !IO) :-
    string.format("-justify %s", [s(Just)], Str).

stringify_config(_Interp, label(Label), Str, !IO) :-
    string.format("-label {%s}", [s(Label)], Str).

stringify_config(_Interp, menu(menu(Path)), Str, !IO) :-
    string.format("-menu %s", [s(Path)], Str).

stringify_config(_Interp, multiple_select(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "extended"
    ;
        Indicator = no,
        IndicatorStr = "browse"
    ),
    string.format("-selectmode %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, orient(Orientation), Str, !IO) :-
    (
        Orientation = horiz,
        OrientStr = "horizontal"
    ;
        Orientation = vert,
        OrientStr = "vertical"
    ),
    string.format("-orient %s", [s(OrientStr)], Str).

stringify_config(_Interp, outline_color(Color), Str, !IO) :-
    string.format("-outline %s", [s(Color)], Str).

stringify_config(_Interp, padx(Pad), Str, !IO) :-
    string.format("-padx %d", [i(Pad)], Str).

stringify_config(_Interp, pady(Pad), Str, !IO) :-
    string.format("-pady %d", [i(Pad)], Str).

stringify_config(_Interp, relief(Rel), Str, !IO) :-
    string.format("-relief %s", [s(Rel)], Str).

stringify_config(_Interp, repeat_delay(Len), Str, !IO) :-
    string.format("-repeatdelay %d", [i(Len)], Str).

stringify_config(_Interp, repeat_interval(Len), Str, !IO) :-
    string.format("-repeatinterval %d", [i(Len)], Str).

stringify_config(_Interp, scale_height(Len), Str, !IO) :-
    string.format("-length %d", [i(Len)], Str).

stringify_config(_Interp, scale_range(Min, Max), Str, !IO) :-
    string.format("-from %d -to %d", [i(Min), i(Max)], Str).

stringify_config(_Interp, scroll_region(H, W), Str, !IO) :-
    string.format("-scrollregion {0 0 %d %d}", [i(H), i(W)], Str).

stringify_config(_Interp, screen(Txt), Str, !IO) :-
    string.format("-screen %s", [s(Txt)], Str).

stringify_config(_Interp, scale_text(Txt), Str, !IO) :-
    string.format("-label %s", [s(Txt)], Str).

stringify_config(_Interp, select_background(Color), Str, !IO) :-
    string.format("-selectbackground %s", [s(Color)], Str).

stringify_config(_Interp, select_border_width(Width), Str, !IO) :-
    string.format("-selectborderwidth %d", [i(Width)], Str).

stringify_config(_Interp, select_foreground(Color), Str, !IO) :-
    string.format("-selectforeground %s", [s(Color)], Str).

stringify_config(_Interp, show(Show), Str, !IO) :-
    (
        Show = yes,
        ShowStr = "yes"
    ;
        Show = no,
        ShowStr = "no"
    ),
    string.format("-show %s", [s(ShowStr)], Str).

stringify_config(_Interp, slider_length(Len), Str, !IO) :-
    string.format("-sliderlength %d", [i(Len)], Str).

stringify_config(_Interp, take_focus(Focus), Str, !IO) :-
    (
        Focus = yes,
        FocusStr = "yes"
    ;
        Focus = no,
        FocusStr = "no"
    ),
    string.format("-takefocus %s", [s(FocusStr)], Str).

stringify_config(_Interp, text(Text), Str, !IO) :-
    string.format("-text ""%s""", [s(Text)], Str).

stringify_config(_Interp, text_variable(Text), Str, !IO) :-
    string.format("-textvariable %s", [s(Text)], Str).

stringify_config(_Interp, tick_interval(Len), Str, !IO) :-
    string.format("-tickinterval %d", [i(Len)], Str).

stringify_config(_Interp, title(Text), Str, !IO) :-
    string.format("-title ""%s""", [s(Text)], Str).

stringify_config(_Interp, trough_color(Col), Str, !IO) :-
    string.format("-troughcolor %s", [s(Col)], Str).

stringify_config(_Interp, underline(U), Str, !IO) :-
    string.format("-underline %d", [i(U)], Str).

stringify_config(_Interp, width(Width), Str, !IO) :-
    string.format("-width %d", [i(Width)], Str).

stringify_config(_Interp, winposition(Width, Height), Str, !IO) :-
    string.format("-geometry %d+%d", [i(Width), i(Height)], Str).

stringify_config(_Interp, winsize(Width, Height), Str, !IO) :-
    string.format("-geometry %dx%d", [i(Width), i(Height)], Str).

stringify_config(_Interp, wrap(Indicator), Str, !IO) :-
    (
        Indicator = yes,
        IndicatorStr = "yes"
    ;
        Indicator = no,
        IndicatorStr = "no"
    ),
    string.format("-wrap %s", [s(IndicatorStr)], Str).

stringify_config(_Interp, wrap_length(Width), Str, !IO) :-
    string.format("-wraplength %d", [i(Width)], Str).

:- pred stringify_anchor(anchor::in, string::out) is det.

stringify_anchor(Ank, AnkStr) :-
    ( Ank = n, AnkStr = "n"
    ; Ank = s, AnkStr = "s"
    ; Ank = e, AnkStr = "e"
    ; Ank = w, AnkStr = "w"
    ; Ank = ne, AnkStr = "ne"
    ; Ank = nw, AnkStr = "nw"
    ; Ank = se, AnkStr = "se"
    ; Ank = sw, AnkStr = "sw"
    ; Ank = c, AnkStr = "c"
    ).

%-----------------------------------------------------------------------------%

:- pred unwrap(widget::in, wpath::out) is det.

unwrap(button(Path), Path).
unwrap(canvas(Path), Path).
unwrap(entry(Path), Path).
unwrap(frame(Path), Path).
unwrap(label(Path), Path).
unwrap(listbox(Path), Path).
unwrap(menubutton(Path), Path).
unwrap(menu(Path), Path).
unwrap(radiobutton(Path), Path).
unwrap(scrollbar(Path), Path).
unwrap(text(Path), Path).
unwrap(window(Path), Path).

    % Function to reduce the path "." of the root window to the
    % empty string "", used when forming names for new widgets
:- func no_dot_wpath(wpath) = wpath.

no_dot_wpath(WPATH) = REDUCED_WPATH :-
    unwrap(root_window, ROOT_WPATH),
    ( if WPATH = ROOT_WPATH then
        REDUCED_WPATH = ""
    else
        REDUCED_WPATH = WPATH
    ).

:- pred command_wrapper(pred(tcl_interp, io, io)::in(pred(in, di, uo) is det),
    tcl_interp::in, list(string)::in, tcl_status::out,
    string::out, io::di, io::uo) is det.

command_wrapper(Closure, Interp, _Args, tcl_ok, "", !IO) :-
    Closure(Interp, !IO).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    extern MR_Integer tk_direct_thingy_counter;
").

:- pragma foreign_code("C", "
    MR_Integer tk_direct_thingy_counter = 0;
").

:- pred get_thingy_counter(int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_thingy_counter(Int::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Int = tk_direct_thingy_counter;
").

:- pred set_thingy_counter(int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_thingy_counter(Int::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    tk_direct_thingy_counter = Int;
").

%------------------------------------------------------------------------------%

:- pred throw_on_tcl_error(tcl_status::in, string::in, io::di, io::uo) is det.

throw_on_tcl_error(Status, Msg, !IO) :-
    (
        Status = tcl_ok
    ;
        Status = tcl_error,
        error(Msg)
    ).

%-----------------------------------------------------------------------------%
:- end_module mtk.
%-----------------------------------------------------------------------------%
