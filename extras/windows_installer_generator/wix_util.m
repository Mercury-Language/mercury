%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: Ian MacLarty (maclarty@cs.mu.oz.au).
%
%---------------------------------------------------------------------------%
%
% Various misc predicates and functions used by the wix modules.
%

:- module wix_util.

:- interface.

:- import_module list.
:- import_module pair.
:- import_module term_to_xml.

:- import_module wix.
:- import_module wix_gui.

:- type privilege
    --->    admin
    ;       normal.

%----------------------------------------------------------------------------%
%
% Various attributes used in the generation of the Wix source file.
%

:- func guid_attr(guid) = attr.

:- func id_attr(string) = attr.

:- func disk_id_attr = attr.

:- func name_attrs(string, string) = list(attr).

:- func size_attrs(size) = list(attr).

:- func pos_attrs(position) = list(attr).

:- func title_attr(string) = attr.

:- func default_attr(button_default) = attr.

:- func modeless_attr(modeless) = attr.

:- func text_attr(string) = attr.

:- func type_attr(string) = attr.

:- func shortcut_where_attr(shortcut_where) = attr.

%----------------------------------------------------------------------------%

:- type position == pair(int).

:- type size == pair(int).

:- func dir_sep = string.

:- type id == string.

:- type guid == string.

:- type id_supply.

:- pred allocate_id(id::out, id_supply::in, id_supply::out) is det.

:- func init_id_supply = id_supply.

:- func attr_if_not_blank(attr) = list(attr).

:- func version_no_to_string(version_no) = string.

:- func env_vars_component_id = string.

:- func desktop_id = string.

:- func programs_menu_id = string.

:- func how_set_to_string(env_var_how_set) = string.

:- func system_or_user_to_string(env_var_system_or_user) = string.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module counter. 
:- import_module dir. 
:- import_module exception.
:- import_module int. 
:- import_module require. 
:- import_module string.

guid_attr(GUID) = attr("Guid", GUID).

id_attr(Id) = attr("Id", Id).

    % XXX Currently installers can span only one volume.
    %
disk_id_attr = attr("DiskId", "1").

name_attrs(FileName, ShortName) = Attrs :-
    ( if is_legal_windows_short_name(FileName) then
        Attrs = [attr("Name", FileName)]
    else
        Attrs = [attr("Name", ShortName), attr("LongName", FileName)]
    ).

pos_attrs(X - Y) = [attr("X", int_to_string(X)), attr("Y", int_to_string(Y))].

size_attrs(Width - Height) = 
    [attr("Width", int_to_string(Width)), 
    attr("Height", int_to_string(Height))].

title_attr(Title) = attr("Title", Title).

default_attr(default) = attr("Default", "yes").
default_attr(not_default) = attr("Default", "no").

text_attr(Text) = attr("Text", Text).

type_attr(Type) = attr("Type", Type).

shortcut_where_attr(programs) = attr("Directory", programs_menu_id).
shortcut_where_attr(desktop) = attr("Directory", desktop_id).

modeless_attr(modeless) = attr("Modeless", "yes").
modeless_attr(not_modeless) = attr("Modeless", "no").
modeless_attr(keep_modeless) = attr("KeepModeless", "yes").

%----------------------------------------------------------------------------%

:- pred is_legal_windows_short_name(string::in) is semidet.

is_legal_windows_short_name(FileName) :-
    Parts = string.words_separator(unify('.'), FileName),
    (
        Parts = [BaseName, Extension],
        string.length(BaseName) =< 8,
        string.length(Extension) =< 3,
        string.is_all_alnum_or_underscore(BaseName),
        string.is_all_alnum_or_underscore(Extension)
    ;
        Parts = [BaseName],
        string.length(BaseName) =< 8,
        string.is_all_alnum_or_underscore(BaseName)
    ).

    % XXX I don't know exactly how to do this, so am ignoring it for 
    % now.  It should only cause problems on versions of windows which
    % don't support long file names.  To my knowledge this is versions 
    % prior to Windows 95 which Mercury won't run on anyway - Ian MacLarty.
    %
:- func make_short_filename(string) = string.

make_short_filename(_) = "XXX.XXX".

dir_sep = char_to_string(dir.directory_separator).

:- type id_supply == counter.

allocate_id("id" ++ int_to_string(Id), !IdSupply) :-
    counter.allocate(Id, !IdSupply).

init_id_supply = counter.init(1).

attr_if_not_blank(attr(Name, Value)) = AttrList :-
    ( if Value = "" then
        AttrList = []
    else
        AttrList = [attr(Name, Value)]
    ).

version_no_to_string(version_no(Major, Minor, Build, Other)) = 
    int_to_string(Major) ++ "." ++ int_to_string(Minor) ++ "." ++
    int_to_string(Build) ++ "." ++ int_to_string(Other).

%----------------------------------------------------------------------------%

env_vars_component_id = "ENVIRONMENT_VARIABLES_COMPONENT".
desktop_id = "DesktopFolder".
programs_menu_id = "PROGRAMSMENU".

%----------------------------------------------------------------------------%

how_set_to_string(replace) = "all".
how_set_to_string(prepend) = "first".
how_set_to_string(append) = "last".

system_or_user_to_string(system) = "yes".
system_or_user_to_string(user) = "no".
