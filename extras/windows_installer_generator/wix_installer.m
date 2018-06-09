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
% This module is responsible for generating the Wix source code for the
% installer.
% The installer is first converted to an annotated installer and then
% to XML.  The annotated installer contains extra information, like Ids
% for all the elements.
%

:- module wix_installer.

:- interface.

:- import_module io.
:- import_module term_to_xml.

:- import_module wix.

:- type annotated_installer.

% gen_annotated_installer(Installer, GUIDStream, AnnotatedInstaller, !IO).
%

:- pred gen_annotated_installer(installer(L)::in, io.input_stream::in,
    annotated_installer::out, io::di, io::uo) is det 
    <= language_independent_tokens(L).

:- instance xmlable(annotated_installer).

%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string. 
:- import_module term_to_xml.

:- import_module wix_files.
:- import_module wix_gui.
:- import_module wix_language.
:- import_module wix_util.

:- type annotated_installer
    --->    annotated_installer(
                ann_installer_product_info  :: annotated_product,
                ann_installer_language      :: language,
                ann_installer_env_vars_guid :: guid,
                ann_installer_set_env_vars  :: list(ann_set_env_var),
                ann_installer_wizard_steps  :: list(annotated_dialog),
                ann_installer_bitmaps       :: map(string, id),
                ann_installer_removedlg_id  :: id,
                ann_installer_finish_id     :: id,
                ann_installer_checkifadmin  :: maybe(string),
                ann_insatller_all_users     :: bool
            ).

:- type ann_set_env_var
    --->    ann_set_env_var(
                ann_env_var                 :: string,
                ann_env_var_value           :: string,
                ann_env_var_how_set         :: env_var_how_set,
                ann_env_var_system_or_user  :: env_var_system_or_user,
                ann_env_var_id              :: id
            ).

:- type annotated_product
    --->    annotated_product(
                ann_prod_guid                   :: guid,
                    % XXX Upgrade installers are not yet supported,
                    % however the upgrade code is required so that
                    % deployed packages can be upgraded when 
                    % upgrade installers are supported.
                ann_prod_upgrade_guid           :: guid,
                ann_prod_manufacturer           :: string,
                ann_prod_name                   :: string,
                ann_prod_version                :: version_no,
                ann_prod_description            :: string,
                ann_prod_comments               :: string,
                ann_prod_contents               :: list(annotated_file),
                ann_prod_default_install_loc    :: string
            ).

:- instance xmlable(annotated_installer) where [
    func(to_xml/1) is annotated_installer_to_xml
].

:- func annotated_installer_to_xml(annotated_installer::in) = 
    (xml::out(xml_doc)) is det.

annotated_installer_to_xml(Installer) = XML :-
    Installer = annotated_installer(Product, LanguageId, EnvVarsGUID, EnvVars,
        WizardSteps, BitMaps, RemoveDlgId, FinishDlgId, CheckIfAdmin,
        AllUsers),
    language_to_lcid(LanguageId, LCID),
    Product = annotated_product(
        GUID,
        UpgradeGUID,
        Manufacturer,
        Name,
        Version,
        Description,
        Comments,
        Contents,
        DefInstallLoc),
    (
        AllUsers = yes,
        AllUsersPropertyList = [elem("Property", [id_attr("ALLUSERS")],
            [data("2")])]
    ;
        AllUsers = no,
        AllUsersPropertyList = []
    ),
    XML = elem("Wix", [
            attr("xmlns","http://schemas.microsoft.com/wix/2003/01/wix")],
        [
        elem("Product", [
            id_attr(GUID),
            attr("UpgradeCode", UpgradeGUID),
            attr("Name", Name),
            attr("Version", version_no_to_string(Version)),
            attr("Language", int_to_string(LCID)),
            attr("Manufacturer", Manufacturer)
        ],
            [elem("Package",
                % The following causes wix to generate a fresh guid each time
                % it compiles the XML file.
                [id_attr("????????-????-????-????-????????????")] ++
                attr_if_not_blank(attr("Description", Description)) ++
                [attr("Manufacturer", Manufacturer)] ++
                attr_if_not_blank(attr("Comments", Comments)) ++
                [attr("InstallerVersion", "150")] ++
                [attr("Compressed", "yes")]
                , [])] ++
            ( if CheckIfAdmin = yes(MustBeAdminMessage) then
                [elem("Condition", [attr("Message", MustBeAdminMessage)],
                    [data("Privileged")])]
            else
                []
            ) ++
            AllUsersPropertyList ++
            [elem("Media", [
                id_attr("1"),
                attr("Cabinet", "contents.cab"),
                attr("EmbedCab", "yes"),
                attr("CompressionLevel", "high")], [])] ++
            [elem("Directory", [
                id_attr("TARGETDIR"), 
                attr("Name", "SourceDir")
            ], [
                elem("Component", [
                    id_attr(env_vars_component_id),
                    guid_attr(EnvVarsGUID)
                ], list.map(ann_set_env_var_to_xml, EnvVars)),
                elem("Directory", [
                    id_attr("ProgramFilesFolder"),
                    attr("Name", "PFiles")
                ], [
                    elem("Directory",
                        [id_attr("INSTALLDIR")] ++
                        name_attrs(DefInstallLoc, "INSDIR"),
                        list.map(annotated_file_to_xml, Contents))
                ]),
                elem("Directory", [
                    id_attr(desktop_id), 
                    attr("Name", "Desktop")], 
                   [])
                ] ++
                programs_menu_directory_if_required(Name, Contents)
            )] ++
            generate_feature_elements(Contents) ++
            maybe_ui_elements(WizardSteps, RemoveDlgId, FinishDlgId) ++
            bitmaps_to_xml(map.keys(BitMaps), map.values(BitMaps))
        )
    ]).

:- func ann_set_env_var_to_xml(ann_set_env_var) = xml.

ann_set_env_var_to_xml(ann_set_env_var(Name, Value, HowSet, SysOrUser, EnvId))
    = elem("Environment", [
        id_attr(EnvId),
        attr("Name", Name),
        attr("Part", how_set_to_string(HowSet)),
        attr("Action", "set"),
        attr("System", system_or_user_to_string(SysOrUser)),
        attr("Value", Value)], []).

gen_annotated_installer(Installer, GUIDStream, AnnotatedInstaller, !IO) :-
    some [!IdSupply, !DialogIdMap, !BitMaps] (
        !:IdSupply = init_id_supply,
        !:DialogIdMap = map.init,
        !:BitMaps = map.init,
        Installer = installer(
            Product, 
            Language, 
            EnvVars, 
            ShortCuts, AllUsers,
            Title, InstallHeading, InstallDescr,
            Next, Back, Cancel, Install, CancelMessage, 
            RemoveHeading, RemoveConfirm, Remove,
            RemoveProgressHeading, RemoveProgressDescr,
            FinishHeading, FinishMessage, Finish,
            FilesInUseHeading, FilesInUseMessage, Ignore, Retry,
            Yes, No, MustBeAdminMessage, BannerSrc, BackgroundSrc, 
            WizardSteps),
        Product = product(
            ManufacturerToken, 
            NameToken, 
            Version, 
            DescriptionToken,
            CommentsToken,
            FilesPath,
            DefaultInstallToken),
        generate_wizard_dialogs(Title, Next, Back, Cancel, Install,
            BannerSrc, BackgroundSrc, WizardSteps, WizDialogs),
        CancelDlg = cancel_dialog(Title, CancelMessage, Yes, No),
        FinishDlg = finish_dialog(Title, FinishHeading, FinishMessage, 
            Finish, BackgroundSrc),
        InstallProgressDlg = 
            install_progress_dialog(Title, InstallHeading, InstallDescr,
                Cancel, BannerSrc),
        RemoveProgressDlg = 
            remove_progress_dialog(Title, RemoveProgressHeading, 
                RemoveProgressDescr, Cancel, BannerSrc),
        RemoveDlg = remove_dialog(Title, RemoveHeading, RemoveConfirm, 
            Remove, Cancel, BannerSrc),
        annotate_dialogs(Language, WizDialogs ++ 
            [CancelDlg, InstallProgressDlg,
            RemoveDlg, RemoveProgressDlg, FinishDlg], 
            AnnDialogs0, !IdSupply, !DialogIdMap, !BitMaps),
        %
        % Generate the default files-in-use dialog which asks the user
        % to close applications that are using installed files.
        % The dialog must be handled separately because it has the
        % reserved id `FilesInUse'.
        %
        FilesInUseDlg = files_in_use_dialog(Title, FilesInUseHeading, 
            FilesInUseMessage, Retry, Ignore, Cancel, BannerSrc),
        annotate_dialog(Language, FilesInUseDlg, AnnFilesInUseDlg0, !IdSupply,
            !DialogIdMap, !BitMaps),
        set_ann_dialog_id("FilesInUse", AnnFilesInUseDlg0, AnnFilesInUseDlg),
        list.append(AnnDialogs0, [AnnFilesInUseDlg], AnnDialogs),

        % Look up the ID assigned to the remove dialog which will be 
        % shown if the product is already installed.
        map.lookup(!.DialogIdMap, remove_dlg, RemoveDlgId),

        % Look up the ID assigned to the finish dialog so we can call it
        % after installation has finished.
        map.lookup(!.DialogIdMap, finish_dlg, FinishDlgId),

        annotate_env_vars(Language, EnvVars, AnnEnvVars, !IdSupply,
            RequiredPrivilege),
        ( if ( RequiredPrivilege = admin ; AllUsers = yes ) then
            det_translate(MustBeAdminMessage, Language, MustBeAdminMsgStr),
            CheckForAdmin = yes(MustBeAdminMsgStr)
        else
            CheckForAdmin = no
        ),
        det_translate(ManufacturerToken, Language, Manufacturer),
        det_translate(NameToken, Language, Name),
        det_translate(DescriptionToken, Language, Description),
        det_translate(CommentsToken, Language, Comments),
        det_translate(DefaultInstallToken, Language, DefInsLoc),
        gen_files(FilesPath, ShortCuts, Files, !IO),
        annotate_files(Language, Files, !.IdSupply, _, GUIDStream, FilesPath, 
            AnnotatedFiles, !IO),
        gen_guid(GUIDStream, ProductGUID, !IO),
        gen_guid(GUIDStream, UpgradeGUID, !IO),
        gen_guid(GUIDStream, EnvVarsGUID, !IO),
        AnnotatedInstaller = 
            annotated_installer(
                annotated_product(
                    ProductGUID,
                    UpgradeGUID,
                    Manufacturer,
                    Name,
                    Version,
                    Description,
                    Comments,
                    AnnotatedFiles,
                    DefInsLoc
                ),
                Language,
                EnvVarsGUID,
                AnnEnvVars,
                AnnDialogs,
                !.BitMaps,
                RemoveDlgId,
                FinishDlgId,
                CheckForAdmin,
                AllUsers
            )
    ).

:- pred annotate_env_vars(language::in, 
    list(set_env_var(L))::in, list(ann_set_env_var)::out,
    id_supply::in, id_supply::out, privilege::out) 
    is det <= language_independent_tokens(L).

annotate_env_vars(_, [], [], !IdSupply, normal).
annotate_env_vars(Language, [Var | Vars], [AnnVar | AnnVars], !IdSupply, Priv) 
        :-
    Var = set_env_var(VarName, ValueToken, HowSet, SysOrUser),
    det_translate(ValueToken, Language, Value),
    allocate_id(EnvId, !IdSupply),
    AnnVar = ann_set_env_var(VarName, Value, HowSet, SysOrUser, EnvId),
    annotate_env_vars(Language, Vars, AnnVars, !IdSupply, Priv0),
    (
        SysOrUser = system,
        Priv = admin
    ;
        SysOrUser = user,
        Priv = Priv0
    ).

:- func programs_menu_directory_if_required(string, list(annotated_file)) =
    list(xml).

programs_menu_directory_if_required(Name, Contents) = XML :-
    ( if is_shortcut_from_programs_menu(Contents) then
        XML = [elem("Directory", [
            id_attr("ProgramMenuFolder"),
            attr("Name", "PMENU"),
            attr("LongName", "Programs")
        ], [
            elem("Directory",
                [id_attr(programs_menu_id)] ++
                name_attrs(Name, "PROGMENU"), [])
        ])]
    else
        XML = []
    ).
