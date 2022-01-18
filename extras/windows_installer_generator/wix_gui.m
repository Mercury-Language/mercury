%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006, 2012 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: Ian MacLarty (maclarty@cs.mu.oz.au).
%
% This module is responsible for generating the GUI components of the
% installer.
%
% Throughout this module the type variable L indicates a language
% independent token and the type variable D indicates a type representing
% a set of dialog boxes (such as wizard_step_dlg).
%
%---------------------------------------------------------------------------%

:- module wix_gui.

:- interface.

:- import_module wix.
:- import_module wix_util.

:- import_module list.
:- import_module map.
:- import_module term_to_xml.

:- type wizard_step(L)
    --->    wizard_step(
                wiz_step_heading   :: L,
                wiz_step_controls  :: list(control(L, wizard_step_dlg))
            )
    ;       wizard_start(L, L).

:- func license_wizard_step(L, L, L) = wizard_step(L).

:- func notice_wizard_step(L, L) = wizard_step(L).

    % Different types of wizard steps that can occur in the installer.
    %
:- type wizard_step_dlg
    --->    wiz_step(int)
    ;       cancel_dlg
    ;       install_progress_dlg
    ;       remove_progress_dlg
    ;       remove_dlg
    ;       finish_dlg
    ;       files_in_use_dlg.

    % A general dialog box.
    %
:- type dialog(L, D)
    --->    dialog(
                dialog_id           :: D,
                dialog_size         :: size,
                dialog_title        :: L,
                dialog_modeless     :: modeless,
                dialog_controls     :: list(control(L, D))
            ).

    % Mode options for dialog boxes.
    %
:- type modeless
    --->    modeless
    ;       not_modeless
    ;       keep_modeless.

    % Various widgets that can be placed in dialog boxes.
    %
:- type control(L, D)
    --->    button(
                button_pos      :: position,
                button_size     :: size,
                button_default  :: button_default,
                button_text     :: L,
                button_events   :: list(event(D))
            )
    ;       bitmap(
                bitmap_pos      :: position,
                bitmap_size     :: size,
                bitmap_src      :: string
            )
    ;       line(
                line_pos        :: position,
                line_size       :: size
            )
    ;       scrollable_text(
                scroll_text_pos     :: position,
                scroll_text_size    :: size,
                scroll_text_source  :: L
            )
    ;       text(
                text_pos        :: position,
                text_size       :: size,
                text_text       :: L,
                text_style      :: text_style
            )
    ;       progress_bar(
                progress_bar_pos    :: position,
                progress_bar_size   :: size
            )
    ;       progress_text(
                progress_text_pos   :: position,
                progress_text_size  :: size
            )
    ;       files_in_use_list(
                files_in_use_pos    :: position,
                files_in_use_size   :: size
            ).

    % Used to indicate if a button is selected by default or not.
    %
:- type button_default
    --->    default
    ;       not_default.

:- type text_style
    --->    normal
    ;       bold
    ;       heading.

:- type event(D)
    --->    new_dialog(D)   % Replace the current dialog with a new one.
    ;       spawn_dialog(D) % Create the dialog in a new window.
    ;       return          % End the current sequence.
                            % Ending during the wizard causes installation
                            % to start.
    ;       exit            % Exit the installer.
    ;       remove_all      % Uninstall the product.

    ;       retry           % Retry an operation that failed.
                            % Currently this event only makes sense when
                            % triggered from a files-in-use dialog
                            % that tells the user that certain files
                            % are being locked by other applications.

    ;       ignore.         % Ignore some error condition and continue
                            % the installation regardless.
                            % Currently this event only makes sense when
                            % triggered from a files-in-use dialog
                            % that tells the user that certain files
                            % are being locked by other applications.

:- type annotated_dialog.

:- pred set_ann_dialog_id(id::in,
    annotated_dialog::in, annotated_dialog::out) is det.

:- pred annotate_dialogs(language::in,
    list(dialog(L, D))::in, list(annotated_dialog)::out,
    id_supply::in, id_supply::out,
    map(D, id)::in, map(D, id)::out,
    map(string, id)::in, map(string, id)::out) is det
    <= language_independent_tokens(L).

:- pred annotate_dialog(language::in, dialog(L, D)::in, annotated_dialog::out,
    id_supply::in, id_supply::out,
    map(D, id)::in, map(D, id)::out,
    map(string, id)::in, map(string, id)::out) is det
    <= language_independent_tokens(L).

:- func bitmaps_to_xml(list(string), list(id)) = list(xml).

:- pred generate_wizard_dialogs(L::in, L::in, L::in, L::in, L::in, string::in,
    string::in, list(wizard_step(L))::in,
    list(dialog(L, wizard_step_dlg))::out) is det.

%----------------------------------------------------------------------------%
%
% Various default dialogs.
%

:- func cancel_dialog(L, L, L, L) = dialog(L, wizard_step_dlg).

:- func install_progress_dialog(L, L, L, L, string)
    = dialog(L, wizard_step_dlg).

:- func remove_progress_dialog(L, L, L, L, string)
    = dialog(L, wizard_step_dlg).

:- func remove_dialog(L, L, L, L, L, string) = dialog(L, wizard_step_dlg).

:- func finish_dialog(L, L, L, L, string) = dialog(L, wizard_step_dlg).

:- func files_in_use_dialog(L, L, L, L, L, L, string)
    = dialog(L, wizard_step_dlg).

%----------------------------------------------------------------------------%

    % Generate a list containing one UI element if the input list has
    % any elements and an empty list if there are no dialogs in the input
    % list.
    %
:- func maybe_ui_elements(list(annotated_dialog), id, id) = list(xml).

%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module pair.
:- import_module string.

:- import_module wix_language.

:- type annotated_dialog
    --->    annotated_dialog(
                ann_dialog_id       :: id,
                ann_dialog_size     :: size,
                ann_dialog_title    :: string,
                ann_dialog_modeless :: modeless,
                ann_dialog_controls :: list(annotated_control)
            ).

set_ann_dialog_id(Id, AD, AD ^ ann_dialog_id := Id).

:- type annotated_control
    --->    annotated_button(
                ann_button_id       :: id,
                ann_button_pos      :: position,
                ann_button_size     :: size,
                ann_button_default  :: button_default,
                ann_button_text     :: string,
                ann_button_events   :: list(annotated_event)
            )
    ;       annotated_bitmap(
                ann_bitmap_id       :: id,
                ann_bitmap_pos      :: position,
                ann_bitmap_size     :: size,
                ann_bitmap_src_id   :: id
            )
    ;       annotated_line(
                ann_line_id     :: id,
                ann_line_pos        :: position,
                ann_line_size       :: size
            )
    ;       annotated_scrollable_text(
                ann_scroll_text_id  :: id,
                ann_scroll_text_pos :: position,
                ann_scroll_text_size    :: size,
                ann_scroll_text_text    :: string
            )
    ;       annotated_text(
                ann_text_id     :: id,
                ann_text_pos        :: position,
                ann_text_size       :: size,
                ann_text_text       :: string,
                ann_text_style      :: text_style
            )
    ;       annotated_progress_bar(
                ann_prog_bar_id     :: id,
                ann_prog_bar_pos    :: position,
                ann_prog_bar_size   :: size
            )
    ;       annotated_progress_text(
                ann_prog_text_id    :: id,
                ann_prog_text_pos   :: position,
                ann_prog_text_size  :: size
            )
    ;       annotated_files_in_use_list(
                ann_files_in_use_id :: id,
                ann_files_in_use_pos    :: position,
                ann_files_in_use_size   :: size
            ).

:- type annotated_event
    --->    annotated_new_dialog(id)
    ;       annotated_spawn_dialog(id)
    ;       annotated_return
    ;       annotated_exit
    ;       annotated_remove_all
    ;       annotated_ignore
    ;       annotated_retry.

license_wizard_step(Heading, Instructions, LicenseSrc) =
    wizard_step(Heading, [
        scrollable_text(20 - 60, 330 - 160, LicenseSrc),
        text(25 - 23, 280 - 15, Instructions, normal)]).

notice_wizard_step(Heading, Notice) =
    wizard_step(Heading, [text(20 - 60, 330 - 160, Notice, normal)]).

install_progress_dialog(Title, Heading, Description, Cancel, BitMapSrc) =
    dialog(install_progress_dlg, 370 - 270, Title, modeless, [
        button(304 - 243, 56 - 17, default, Cancel,
            [spawn_dialog(cancel_dlg)]),
        progress_bar(35 - 115, 300 - 10),
        progress_text(70 - 100, 265 - 10),
        bitmap(0 - 0, 370 - 44, BitMapSrc),
        text(35 - 65, 300 - 20, Description, normal),
        text(20 - 15, 200 - 15, Heading, bold),
        line(0 - 234, 370 - 0),
        line(0 - 44, 370 - 0)]).

remove_progress_dialog(Title, Heading, Description, Cancel, BitMapSrc) =
    dialog(remove_progress_dlg, 370 - 270, Title, modeless, [
        button(304 - 243, 56 - 17, default, Cancel,
            [spawn_dialog(cancel_dlg)]),
        progress_bar(35 - 115, 300 - 10),
        progress_text(70 - 100, 265 - 10),
        bitmap(0 - 0, 370 - 44, BitMapSrc),
        text(35 - 65, 300 - 20, Description, normal),
        text(20 - 15, 200 - 15, Heading, bold),
        line(0 - 234, 370 - 0),
        line(0 - 44, 370 - 0)]).

remove_dialog(Title, Heading, AreYouSure, Remove, Cancel, BitMapSrc) =
    dialog(remove_dlg, 370 - 270, Title, not_modeless, [
        button(304 - 243, 56 - 17, default, Cancel,
            [spawn_dialog(cancel_dlg)]),
        button(236 - 243, 56 - 17, not_default, Remove,
            [remove_all, new_dialog(remove_progress_dlg)]),
        bitmap(0 - 0, 370 - 44, BitMapSrc),
        text(35 - 65, 300 - 20, AreYouSure, normal),
        text(20 - 15, 200 - 15, Heading, bold),
        line(0 - 234, 370 - 0),
        line(0 - 44, 370 - 0)]).

finish_dialog(Title, Heading, Message, Finish, BackgroundSrc) =
    dialog(finish_dlg, 370 - 270, Title, not_modeless, [
        button(236 - 243, 56 - 17, not_default, Finish, [exit]),
        bitmap(0 - 0, 370 - 234, BackgroundSrc),
        text(135 - 70, 220 - 160, Message, normal),
        text(135 - 20, 220 - 60, Heading, heading),
        line(0 - 234, 370 - 0)]).

files_in_use_dialog(Title, Heading, Message, Retry, Ignore, Cancel, BitMapSrc)
    = dialog(files_in_use_dlg, 370 - 270, Title, keep_modeless, [
        button(304 - 243, 56 - 17, default, Retry, [retry]),
        button(235 - 243, 56 - 17, not_default, Ignore, [ignore]),
        button(166 - 243, 56 - 17, not_default, Cancel, [exit]),
        bitmap(0 - 0, 370 - 44, BitMapSrc),
        text(35 - 65, 300 - 20, Message, normal),
        text(20 - 15, 200 - 15, Heading, bold),
        line(0 - 234, 370 - 0),
        line(0 - 44, 370 - 0),
        files_in_use_list(20 - 87, 330 - 130)]).

cancel_dialog(Title, CancelMessage, Yes, No) =
    dialog(cancel_dlg, 260 - 85, Title, not_modeless, [
        button(132 - 57, 56 - 17, default, No, [return]),
        button(72 - 57, 56 - 17, not_default, Yes, [exit]),
        text(48 - 15, 194 - 30, CancelMessage, normal)]).

annotate_dialogs(_, [], [], !IdSupply, !DialogIdMap, !BitMaps).
annotate_dialogs(Language, [Dialog | Dialogs], [AnnDialog | AnnDialogs],
        !IdSupply, !DialogIdMap, !BitMaps) :-
    annotate_dialogs(Language, Dialogs, AnnDialogs, !IdSupply,
        !DialogIdMap, !BitMaps),
    annotate_dialog(Language, Dialog, AnnDialog, !IdSupply,
        !DialogIdMap, !BitMaps).

annotate_dialog(Language, Dialog, AnnDialog, !IdSupply, !DialogIdMap,
        !BitMaps) :-
    Dialog = dialog(DialogToken, Size, TitleToken, Modeless, Controls),
    det_translate(TitleToken, Language, Title),
    lookup_dialog_id(DialogToken, DialogId, !DialogIdMap, !IdSupply),
    annotate_controls(Language, Controls, AnnControls, !DialogIdMap,
        !IdSupply, !BitMaps),
    AnnDialog = annotated_dialog(DialogId, Size, Title, Modeless,
        AnnControls).

:- func annotated_dialog_to_xml(annotated_dialog) = xml.

annotated_dialog_to_xml(annotated_dialog(Id, Size, Title, Modeless, Controls))
    = elem("Dialog",
        [id_attr(Id)] ++
        size_attrs(Size) ++
        [title_attr(Title)] ++
        [modeless_attr(Modeless)] ++
        [attr("NoMinimize", "yes")],
        list.map(annotated_control_to_xml, Controls)).

:- pred annotate_controls(language::in, list(control(L, D))::in,
    list(annotated_control)::out,
    map(D, id)::in, map(D, id)::out,
    id_supply::in, id_supply::out,
    map(string, id)::in, map(string, id)::out) is det
    <= language_independent_tokens(L).

annotate_controls(_, [], [], !DialogIdMap, !IdSupply, !BitMaps).
annotate_controls(Language, [Control | Controls], [AnnControl | AnnControls],
        !DialogIdMap, !IdSupply, !BitMaps) :-
    annotate_controls(Language, Controls, AnnControls, !DialogIdMap,
        !IdSupply, !BitMaps),
    annotate_control(Language, Control, AnnControl, !DialogIdMap,
        !IdSupply, !BitMaps).

:- pred annotate_control(language::in,
    control(L, D)::in,
    annotated_control::out,
    map(D, id)::in, map(D, id)::out,
    id_supply::in, id_supply::out,
    map(string, id)::in, map(string, id)::out) is det
    <= language_independent_tokens(L).

annotate_control(Language, Control, AnnControl, !DialogIdMap, !IdSupply,
        !BitMaps) :-
    (
        Control = button(Pos, Size, Default, TextToken, Events),
        det_translate(TextToken, Language, Text),
        allocate_id(Id, !IdSupply),
        annotate_events(Events, AnnEvents, !DialogIdMap, !IdSupply),
        AnnControl = annotated_button(Id, Pos, Size, Default, Text,
            AnnEvents)
    ;
        Control = bitmap(Pos, Size, Source),
        allocate_id(BitMapControlId, !IdSupply),
        ( if map.search(!.BitMaps, Source, FoundSourceId) then
            BitMapSourceId = FoundSourceId
        else
            allocate_id(BitMapSourceId, !IdSupply),
            map.det_insert(Source, BitMapSourceId, !BitMaps)
        ),
        AnnControl = annotated_bitmap(BitMapControlId, Pos, Size,
            BitMapSourceId)
    ;
        Control = line(Pos, Size),
        allocate_id(LineId, !IdSupply),
        AnnControl = annotated_line(LineId, Pos, Size)
    ;
        Control = scrollable_text(Pos, Size, TextSrcToken),
        det_translate(TextSrcToken, Language, TextSrc),
        allocate_id(TextId, !IdSupply),
        AnnControl = annotated_scrollable_text(TextId, Pos, Size,
            TextSrc)
    ;
        Control = text(Pos, Size, TextToken, Style),
        det_translate(TextToken, Language, Text),
        allocate_id(TextId, !IdSupply),
        AnnControl = annotated_text(TextId, Pos, Size, Text, Style)
    ;
        Control = progress_bar(Pos, Size),
        allocate_id(Id, !IdSupply),
        AnnControl = annotated_progress_bar(Id, Pos, Size)
    ;
        Control = progress_text(Pos, Size),
        allocate_id(Id, !IdSupply),
        AnnControl = annotated_progress_text(Id, Pos, Size)
    ;
        Control = files_in_use_list(Pos, Size),
        allocate_id(Id, !IdSupply),
        AnnControl = annotated_files_in_use_list(Id, Pos, Size)
    ).

:- func annotated_control_to_xml(annotated_control) = xml.

annotated_control_to_xml(annotated_button(Id, Pos, Size, Deflt, Text, Events))
    = elem(control_elem,
        [type_attr("PushButton")] ++
        [id_attr(Id)] ++
        size_attrs(Size) ++
        pos_attrs(Pos) ++
        [default_attr(Deflt)] ++
        [text_attr(Text)],
        list.map(annotated_event_to_xml, Events)).
annotated_control_to_xml(annotated_bitmap(Id, Pos, Size, SrcId)) =
    elem(control_elem,
        [type_attr("Bitmap")] ++
        [id_attr(Id)] ++
        size_attrs(Size) ++
        pos_attrs(Pos) ++
        [text_attr(SrcId)] ++
        [attr("TabSkip", "no")], []).
annotated_control_to_xml(annotated_line(Id, Pos, Size)) =
    elem(control_elem,
        [type_attr("Line")] ++
        [id_attr(Id)] ++
        size_attrs(Size) ++
        pos_attrs(Pos), []).
annotated_control_to_xml(annotated_scrollable_text(Id, Pos, Size, TextSrc)) =
    elem(control_elem,
        [type_attr("ScrollableText")] ++
        [id_attr(Id)] ++
        pos_attrs(Pos) ++
        size_attrs(Size) ++
        [attr("Sunken", "yes")] ++
        [attr("TabSkip", "no")], [
            elem("Text", [attr("src", TextSrc)], [])]).
annotated_control_to_xml(annotated_text(Id, Pos, Size, Text, Style)) =
    elem(control_elem,
        [type_attr("Text")] ++
        [id_attr(Id)] ++
        pos_attrs(Pos) ++
        size_attrs(Size) ++
        [attr("Transparent", "yes")] ++
        [attr("NoPrefix", "yes")], [
            elem("Text", [], [
                data(text_style_modifier(Style) ++ Text)])]).
annotated_control_to_xml(annotated_progress_bar(Id, Pos, Size)) =
    elem(control_elem,
        [id_attr(Id)] ++
        [type_attr("ProgressBar")] ++
        pos_attrs(Pos) ++
        size_attrs(Size) ++
        [attr("ProgressBlocks", "yes")] ++
        [attr("Text", " ")],
        [elem("Subscribe", [
            attr("Event", "SetProgress"),
            attr("Attribute", "Progress")], [])]).
annotated_control_to_xml(annotated_progress_text(Id, Pos, Size)) =
    elem(control_elem,
        [id_attr(Id)] ++
        [type_attr("Text")] ++
        pos_attrs(Pos) ++
        size_attrs(Size),
        [elem("Subscribe", [
            attr("Event", "ActionText"),
            attr("Attribute", "Text")], [])]).
annotated_control_to_xml(annotated_files_in_use_list(Id, Pos, Size)) =
    elem(control_elem,
        [id_attr(Id)] ++
        [type_attr("ListBox")] ++
        pos_attrs(Pos) ++
        size_attrs(Size) ++
        [attr("Property", "FileInUseProcess")] ++
        [attr("TabSkip", "yes")] ++
        [attr("Sunken", "yes")],
        [elem("Subscribe", [
            attr("Event", "ActionText"),
            attr("Attribute", "Text")], [])]).

:- func text_style_modifier(text_style) = string.

text_style_modifier(normal) = "".
text_style_modifier(bold) = "{&DlgFontBold8}".
text_style_modifier(heading) = "{\\VerdanaBold13}".

:- func annotated_event_to_xml(annotated_event) = xml.

annotated_event_to_xml(annotated_new_dialog(DialogId)) =
    elem(publish_elem, [
        attr("Event", "NewDialog"),
        attr("Value", DialogId)],
        [data("1")]).
annotated_event_to_xml(annotated_spawn_dialog(DialogId)) =
    elem(publish_elem, [
        attr("Event", "SpawnDialog"),
        attr("Value", DialogId)],
        [data("1")]).
annotated_event_to_xml(annotated_return) =
    elem(publish_elem, [
        attr("Event", "EndDialog"),
        attr("Value", "Return")],
        [data("1")]).
annotated_event_to_xml(annotated_exit) =
    elem(publish_elem, [
        attr("Event", "EndDialog"),
        attr("Value", "Exit")],
        [data("1")]).
annotated_event_to_xml(annotated_remove_all) =
    elem(publish_elem, [
        attr("Event", "Remove"),
        attr("Value", "All")],
        [data("1")]).
annotated_event_to_xml(annotated_retry) =
    elem(publish_elem, [
        attr("Event", "EndDialog"),
        attr("Value", "Retry")],
        [data("1")]).
annotated_event_to_xml(annotated_ignore) =
    elem(publish_elem, [
        attr("Event", "EndDialog"),
        attr("Value", "Ignore")],
        [data("1")]).

:- func control_elem = string.

control_elem = "Control".

:- func publish_elem = string.

publish_elem = "Publish".

bitmaps_to_xml([], []) = [].
bitmaps_to_xml([Source | Sources], [Id | Ids]) =
    [elem("Binary", [
        id_attr(Id),
        attr("src", Source)],
        [])] ++
    bitmaps_to_xml(Sources, Ids).
bitmaps_to_xml([], [_ | _]) = _ :-
    error("bitmaps_to_xml: more ids than sources").
bitmaps_to_xml([_ | _], []) = _ :-
    error("bitmaps_to_xml: more sources than ids").

:- pred annotate_events(list(event(D))::in, list(annotated_event)::out,
    map(D, id)::in, map(D, id)::out,
    id_supply::in, id_supply::out) is det.

annotate_events([], [], !DialogIdMap, !IdSupply).
annotate_events([Event | Events], [AnnEvent | AnnEvents], !DialogIdMap,
        !IdSupply) :-
    annotate_events(Events, AnnEvents, !DialogIdMap, !IdSupply),
    annotate_event(Event, AnnEvent, !DialogIdMap, !IdSupply).

:- pred annotate_event(event(D)::in, annotated_event::out,
    map(D, id)::in, map(D, id)::out,
    id_supply::in, id_supply::out) is det.

annotate_event(Event, AnnEvent, !DialogIdMap, !IdSupply) :-
    (
        Event = new_dialog(DialogToken),
        lookup_dialog_id(DialogToken, DialogId, !DialogIdMap,
            !IdSupply),
        AnnEvent = annotated_new_dialog(DialogId)
    ;
        Event = spawn_dialog(DialogToken),
        lookup_dialog_id(DialogToken, DialogId, !DialogIdMap,
            !IdSupply),
        AnnEvent = annotated_spawn_dialog(DialogId)
    ;
        Event = return,
        AnnEvent = annotated_return
    ;
        Event = exit,
        AnnEvent = annotated_exit
    ;
        Event = remove_all,
        AnnEvent = annotated_remove_all
    ;
        Event = retry,
        AnnEvent = annotated_retry
    ;
        Event = ignore,
        AnnEvent = annotated_ignore
    ).

:- pred lookup_dialog_id(D::in, id::out,
    map(D, id)::in, map(D, id)::out,
    id_supply::in, id_supply::out) is det.

lookup_dialog_id(DialogToken, DialogId, !DialogIdMap, !IdSupply) :-
    ( if map.search(!.DialogIdMap, DialogToken, FoundDialogId) then
        DialogId = FoundDialogId
    else
        allocate_id(DialogId, !IdSupply),
        map.det_insert(DialogToken, DialogId, !DialogIdMap)
    ).

generate_wizard_dialogs(Title, Next, Back, Cancel, Install, BannerSrc,
        BackgroundSrc, WizardSteps, Dialogs) :-
    generate_wizard_dialogs_2(Title, Next, Back, Cancel, Install,
        BannerSrc, BackgroundSrc, WizardSteps, 0, [], Dialogs).

:- pred generate_wizard_dialogs_2(L::in, L::in, L::in, L::in, L::in,
    string::in, string::in, list(wizard_step(L))::in, int::in,
    list(dialog(L, wizard_step_dlg))::in,
    list(dialog(L, wizard_step_dlg))::out) is det.

generate_wizard_dialogs_2(_, _, _, _, _, _, _, [], _, Dialogs,
    list.reverse(Dialogs)).
generate_wizard_dialogs_2(Title, Next, Back, Cancel, Install, BannerSrc,
        BackgroundSrc, [WizardStep | WizardSteps], StepNum, !Dialogs)
        :-
    (
        WizardSteps = [],
        NextButton = [button(236 - 243, 56 - 17, default, Install,
            [new_dialog(install_progress_dlg)])]
    ;
        WizardSteps = [_ | _],
        NextButton = [button(236 - 243, 56 - 17, default, Next,
            [new_dialog(wiz_step(StepNum + 1))])]
    ),
    ( if StepNum = 0 then
        BackButton = []
    else
        BackButton = [button(180 - 243, 56 - 17, not_default, Back,
            [new_dialog(wiz_step(StepNum - 1))])]
    ),
    CancelButton = [button(304 - 243, 56 - 17, not_default, Cancel,
        [spawn_dialog(cancel_dlg)])],
    (
        WizardStep = wizard_step(Heading, Controls),
        Banner = [bitmap(0 - 0, 370 - 44, BannerSrc)],
        BottomLine = [line(0 - 234, 370 - 0)],
        BannerLine = [line(0 - 44, 370 - 0)],
        HeadingText = [text(15 - 6, 280 - 15, Heading, bold)],
        WizControls = Controls ++ NextButton ++ BackButton ++
            CancelButton ++ Banner ++ BottomLine ++ BannerLine ++
            HeadingText
    ;
        WizardStep = wizard_start(Heading, Message),
        Background = [bitmap(0 - 0, 370 - 234, BackgroundSrc)],
        HeadingText = [text(135 - 20, 220 - 60, Heading, heading)],
        MessageText = [text(135 - 70, 220 - 160, Message, normal)],
        BottomLine = [line(0 - 234, 370 - 0)],
        WizControls = NextButton ++ CancelButton ++
            HeadingText ++ MessageText ++ Background ++
            BottomLine
    ),
    Dialog = dialog(wiz_step(StepNum), 370 - 270, Title, not_modeless,
        WizControls),
    generate_wizard_dialogs_2(Title, Next, Back, Cancel, Install,
        BannerSrc, BackgroundSrc, WizardSteps, StepNum + 1,
        [Dialog | !.Dialogs], !:Dialogs).

maybe_ui_elements([], _, _) = [].
maybe_ui_elements([Dialog | Dialogs], RemoveDlgId, FinishDlgId) = XML :-
    DialogId = Dialog ^ ann_dialog_id,
    XML = [
        elem("UI", [],
            [elem("Property", [id_attr("DefaultUIFont")], [data("DlgFont8")])]
            ++ list.map(annotated_dialog_to_xml, [Dialog | Dialogs]) ++
            [elem("InstallUISequence", [], [
                elem("Show", [
                    %
                    % Launch the first step of the wizard if the product is
                    % not installed.
                    %
                    attr("Dialog", DialogId),
                    attr("After", "LaunchConditions")
                ], [data("NOT Installed")]),
                elem("Show", [
                    %
                    % If the product is already installed should the
                    % removal dialog box.
                    %
                    attr("Dialog", RemoveDlgId),
                    attr("After", "MigrateFeatureStates")
                ], [data("Installed")]),
                elem("Show", [
                    %
                    % Should the finish dialog when installation is complete.
                    %
                    attr("Dialog", FinishDlgId),
                    attr("OnExit", "success")
                ], [data("NOT Installed")])
            ])] ++
            %
            % Define some commonly used fonts.
            %
            [elem("TextStyle", [
                id_attr("DlgFont8"),
                attr("FaceName", "Tahoma"),
                attr("Size", "8")], [])] ++
            [elem("TextStyle", [
                id_attr("DlgFontBold8"),
                attr("FaceName", "Tahoma"),
                attr("Size", "8"),
                attr("Bold", "yes")], [])] ++
            [elem("TextStyle", [
                id_attr("VerdanaBold13"),
                attr("FaceName", "Verdana"),
                attr("Size", "13"),
                attr("Bold", "yes")], [])]
        )].

