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

:- module wix.
:- interface.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module string.

:- import_module wix_gui.

    % generate_installer(Installer, GUIDFile, FileName, Result, !IO).
    %
    % Generate a Wix source file which can then be compiled into a
    % Windows installer (.msi) file.
    %
    % The name of the source file to generate is given by FileName.
    % By convention Wix source files have the suffix `.wxs'.
    % The Installer argument contains all the information necessary to
    % generate the installer (see the installer type below).
    % GUIDFile is a file of GUIDs (one per line).
    % A GUID is a unique identifier that Windows uses to keep track of
    % each installed component.
    % There should be at least as many GUIDs in the file as there are files in
    % the product the installer is for.
    %
    % To compile the generated .wxs file into a windows installer first
    % download the Wix toolset from wix.sourceforge.net and then run:
    %
    % candle <filename>.wxs
    % light <filename>.wixobj
    %
    % from the Windows command prompt.  This will produce <filename>.msi.
    %
    % Result unifies with `ok' if the installer was successfully generated
    % and `wix_error(Error)' if there was a problem.  See the definition of
    % the wix_result type below for the possible errors that could occur.
    %
:- pred generate_installer(installer(L)::in, string::in, string::in,
    wix_result::out, io::di, io::uo)
    is det <= language_independent_tokens(L).

    % Types of the following class are tokens used to represent
    % fragments of text in the user interface of the installer in
    % a language independent way.  The translate member should
    % translate all the tokens for the languages supported by the installer.
    % If translate fails for a given token/language combination during the
    % generation of a Wix source file, then the source file will not be
    % generated and generate_installer will result a wix_error result.
    % It is a good idea to define translate in terms of another function
    % that is deterministic for the languages you want to support.  That
    % way the compiler will catch any missing translations.
    % For a list of possible languages see the file languages.m in this
    % directory.
    %
:- typeclass language_independent_tokens(L) where [
    pred translate(L::in, language::in, string::out) is semidet
].

    % The installer type is used to define a Windows installer.
    % The values of type L are tokens representing fragments of
    % text in the installer GUI.  The tokens are converted to the
    % appropriate language based on the value of the wix_language field.
    %
:- type installer(L)
    --->    installer(

                    % Information about the product being installed.
                    % (see below)
                wix_product_info            :: product(L),

                    % The language to be used in the GUI of the installer.
                wix_language                :: language,

                    % A list of environment variables to set when installing
                    % the product (see the definition of set_env_var below).
                wix_set_env_vars            :: list(set_env_var(L)),

                    % A mapping from filenames to shortcuts which
                    % should be placed on the desktop and/or programs
                    % menu (see the definition of the shortcut_function
                    % type below).
                wix_shortcut_func           :: shortcut_function(L),

                    % If yes then the shortcuts will be visible to all users.
                wix_all_users               :: bool,

                    % A token representing the text to display in the
                    % title bar of the installer GUI.
                wix_title                   :: L,

                    % Tokens representing the heading and description
                    % to display while installation is progressing.
                wix_install_heading         :: L,
                wix_install_descr           :: L,

                    % Tokens representing the text in the Next, Back, Cancel
                    % and Install wizard buttons.
                wix_next_button             :: L,
                wix_back_button             :: L,
                wix_cancel_button           :: L,
                wix_install_button          :: L,

                    % A token representing the message to display to the user
                    % when confirmin a cancel operation.
                wix_cancel_message          :: L,

                    % Tokens representing the heading, confirmation message
                    % and Remove button text displayed in the
                    % uninstallation confirmation dialog.
                wix_remove_heading          :: L,
                wix_remove_confirm          :: L,
                wix_remove_button           :: L,

                    % Tokens representing the heading and description to
                    % display while uninstallation is progressing.
                wix_remove_progress_heading :: L,
                wix_remove_progress_descr   :: L,

                    % Tokens representing the heading, message and Finish
                    % button text to display in the final wizard dialog
                    % which is displayed after installation has successfully
                    % completed.
                wix_finish_heading          :: L,
                wix_finish_message          :: L,
                wix_finish_button           :: L,

                    % Tokens representing the heading and message to display
                    % if the installer detects that some files are in
                    % use which it needs to delete.
                    % A list of the programs locking the files in use is
                    % displayed after the message.
                wix_files_in_use_heading    :: L,
                wix_files_in_use_message    :: L,

                    % Tokens representing the text to display on Ignore,
                    % Retry, Yes and No buttons.
                wix_ignore_button           :: L,
                wix_retry_button            :: L,
                wix_yes_button              :: L,
                wix_no_button               :: L,

                    % A token representing the text to display if the
                    % user is not an administrator, but administrator
                    % privileges are required for the installation.
                    % (Administrator privileges will be required if any
                    % system level environment variables need to be set
                    % by the installer).
                wix_must_be_admin_msg       :: L,

                    % The path to a bitmap file to be displayed at the top of
                    % each wizard step.  The bitmap can be any size, but will
                    % be scaled to fit an area of 370x44 pixels.
                    %
                wix_banner_source           :: string,

                    % The path to a larger bitmap image that will be
                    % used as the background for the final wizard step
                    % (after successful installation).  This bitmap will
                    % also be used for wizard_start wizard steps
                    % (see the wizard_start function in gui.m).
                    % The bitmap can be any size, but will be scaled
                    % to fit any area of 370x234 pixels.
                wix_background_source       :: string,

                    % A list of wizard steps the installer should go through
                    % with the user before installation.
                wix_wizard_steps            :: list(wizard_step(L))
            ).

    % The shortcut function determines which files should have shortcuts
    % installed on the Desktop and/or the Programs menu.
    % It takes two string arguments - the directory of the file and the
    % name of the file.  It returns a list of shortcuts to generate for the
    % given file.
    %
:- type shortcut_function(L) == (func(string, string) = list(shortcut(L))).

:- type shortcut(L)
    --->    shortcut(
                shortcut_where  :: shortcut_where,  % Where to place the
                                                    % shortcut.
                shortcut_name   :: L                % The title of the shortcut
                                                    % as the user will see it.
            ).

:- type shortcut_where
    --->    programs    % Place the shortcut in the Programs menu under a
                        % folder with the same name as the product.
    ;       desktop.    % Place the shortcut on the Desktop.

    % Values of this type give information about the product being
    % installed.
    %
:- type product(L)
    --->    product(
                product_manufacturer        :: L,
                product_name                :: L,
                product_version             :: version_no,
                product_description         :: L,
                product_comments            :: L,

                    % The path to the directory containing the files
                    % and directories that should be installed to the user's
                    % computer, on the machine where the installer will be
                    % generated.
                product_files_path          :: string,

                    % The name of the folder under the "Program Files" folder
                    % to install the product on the user's machine.
                product_default_install_loc :: L
            ).

    % Wix only accepts version numbers of the form NN.NN.NN.NN.
    %
:- type version_no
    --->    version_no(
                version_major   :: int,
                version_minor   :: int,
                version_build   :: int,
                version_other   :: int
            ).

    % This type is used to tell the installer what environment variables
    % to set during the installation.
    % Values can be appended or prepended to existing environment variables,
    % or the value of an old environment variable can be overwritten
    % completely.  If the environment variable doesn't exist it will be
    % created.  Environement variables can also be set in the user or
    % system environment space.  Setting an environment variable in the
    % system user space will require the user to have administrator privileges.
    %
:- type set_env_var(L)
    --->    set_env_var(
                env_var_name            :: string,
                env_var_value           :: L,
                env_var_how_set         :: env_var_how_set,
                env_var_system_or_user  :: env_var_system_or_user
            ).

:- type env_var_how_set
    --->    replace % Replace the value of the environment variable with the
                    % new value.
    ;       prepend % Prepend the new value to the old value, separating the
                    % old and new values with a `;' character.
    ;       append. % Append the new value to the end of the old value,
                    % separating the old and new values with a `;' character.

    % Should the environment variable be set in the system or
    % user environment space?
    % Setting an environment variable in the system environment
    % space requires administrator privileges, but the environment variable
    % will be visible to all users.
    %
:- type env_var_system_or_user
    --->    system
    ;       user.

    % license_wizard_step(Heading, Instructions, LicenseSrc) = WizardStep.
    % Generate a wizard step that displays the contents of a rich text format
    % (.rtf) file.  The name of the .rtf file should be given by LicenseSrc.
    % Heading and Instructions are displayed above the contents
    % of the .rtf file.
    %
:- func license_wizard_step(L, L, L) = wizard_step(L).

    % welcome_wizard_step(Heading, Message) = WizardStep.
    % Generate a welcome dialog with the given Heading and Message.
    %
:- func welcome_wizard_step(L, L) = wizard_step(L).

    % notice_wizard_step(Heading, Message) = WizardStep.
    % The notice wizard step is similar to the license wizard step,
    % except that the text to display is given as a string, instead of
    % in an .rtf file.  The text is also not displayed in a scrollable
    % area, but in a static text area.
    %
:- func notice_wizard_step(L, L) = wizard_step(L).

:- type language
    --->    afrikaans_south_africa
    ;       albanian_albania
    ;       amharic_ethiopia
    ;       arabic_saudi_arabia
    ;       arabic_algeria
    ;       arabic_bahrain
    ;       arabic_egypt
    ;       arabic_iraq
    ;       arabic_jordan
    ;       arabic_kuwait
    ;       arabic_lebanon
    ;       arabic_libya
    ;       arabic_morocco
    ;       arabic_oman
    ;       arabic_qatar
    ;       arabic_syria
    ;       arabic_tunisia
    ;       arabic_u_a_e
    ;       arabic_yemen
    ;       armenian_armenia
    ;       assamese
    ;       azeri_cyrillic
    ;       azeri_latin
    ;       basque
    ;       belarusian
    ;       bengali
    ;       bengali_bangladesh
    ;       bosnian_bosnia_herzegovina
    ;       bulgarian
    ;       burmese
    ;       catalan
    ;       cherokee_united_states
    ;       chinese_peoples_republic_of_china
    ;       chinese_singapore
    ;       chinese_taiwan
    ;       chinese_hong_kong_sar
    ;       chinese_macao_sar
    ;       croatian
    ;       croatian_bosnia_herzegovina
    ;       czech
    ;       danish
    ;       divehi
    ;       dutch_netherlands
    ;       dutch_belgium
    ;       edo
    ;       english_united_states
    ;       english_united_kingdom
    ;       english_australia
    ;       english_belize
    ;       english_canada
    ;       english_caribbean
    ;       english_hong_kong_sar
    ;       english_india
    ;       english_indonesia
    ;       english_ireland
    ;       english_jamaica
    ;       english_malaysia
    ;       english_new_zealand
    ;       english_philippines
    ;       english_singapore
    ;       english_south_africa
    ;       english_trinidad
    ;       english_zimbabwe
    ;       estonian
    ;       faroese
    ;       farsi
    ;       filipino
    ;       finnish
    ;       french_france
    ;       french_belgium
    ;       french_cameroon
    ;       french_canada
    ;       french_democratic_rep_of_congo
    ;       french_cote_divoire
    ;       french_haiti
    ;       french_luxembourg
    ;       french_mali
    ;       french_monaco
    ;       french_morocco
    ;       french_north_africa
    ;       french_reunion
    ;       french_senegal
    ;       french_switzerland
    ;       french_west_indies
    ;       frisian_netherlands
    ;       fulfulde_nigeria
    ;       fyro_macedonian
    ;       gaelic_ireland
    ;       gaelic_scotland
    ;       galician
    ;       georgian
    ;       german_germany
    ;       german_austria
    ;       german_liechtenstein
    ;       german_luxembourg
    ;       german_switzerland
    ;       greek
    ;       guarani_paraguay
    ;       gujarati
    ;       hausa_nigeria
    ;       hawaiian_united_states
    ;       hebrew
    ;       hindi
    ;       hungarian
    ;       ibibio_nigeria
    ;       icelandic
    ;       igbo_nigeria
    ;       indonesian
    ;       inuktitut
    ;       italian_italy
    ;       italian_switzerland
    ;       japanese
    ;       kannada
    ;       kanuri_nigeria
    ;       kashmiri
    ;       kashmiri_arabic
    ;       kazakh
    ;       khmer
    ;       konkani
    ;       korean
    ;       kyrgyz_cyrillic
    ;       lao
    ;       latin
    ;       latvian
    ;       lithuanian
    ;       malay_malaysia
    ;       malay_brunei_darussalam
    ;       malayalam
    ;       maltese
    ;       manipuri
    ;       maori_new_zealand
    ;       marathi
    ;       mongolian_cyrillic
    ;       mongolian_mongolian
    ;       nepali
    ;       nepali_india
    ;       norwegian_bokmal
    ;       norwegian_nynorsk
    ;       oriya
    ;       oromo
    ;       papiamentu
    ;       pashto
    ;       polish
    ;       portuguese_brazil
    ;       portuguese_portugal
    ;       punjabi
    ;       punjabi_pakistan
    ;       quecha_bolivia
    ;       quecha_ecuador
    ;       quecha_peru
    ;       rhaeto_romanic
    ;       romanian
    ;       romanian_moldava
    ;       russian
    ;       russian_moldava
    ;       sami_lappish
    ;       sanskrit
    ;       sepedi
    ;       serbian_cyrillic
    ;       serbian_latin
    ;       sindhi_india
    ;       sindhi_pakistan
    ;       sinhalese_sri_lanka
    ;       slovak
    ;       slovenian
    ;       somali
    ;       sorbian
    ;       spanish_spain_modern_sort
    ;       spanish_spain_traditional_sort
    ;       spanish_argentina
    ;       spanish_bolivia
    ;       spanish_chile
    ;       spanish_colombia
    ;       spanish_costa_rica
    ;       spanish_dominican_republic
    ;       spanish_ecuador
    ;       spanish_el_salvador
    ;       spanish_guatemala
    ;       spanish_honduras
    ;       spanish_latin_america
    ;       spanish_mexico
    ;       spanish_nicaragua
    ;       spanish_panama
    ;       spanish_paraguay
    ;       spanish_peru
    ;       spanish_puerto_rico
    ;       spanish_united_states
    ;       spanish_uruguay
    ;       spanish_venezuela
    ;       sutu
    ;       swahili
    ;       swedish
    ;       swedish_finland
    ;       syriac
    ;       tajik
    ;       tamazight_arabic
    ;       tamazight_latin
    ;       tamil
    ;       tatar
    ;       telugu
    ;       thai
    ;       tibetan_bhutan
    ;       tibetan_peoples_republic_of_china
    ;       tigrigna_eritrea
    ;       tigrigna_ethiopia
    ;       tsonga
    ;       tswana
    ;       turkish
    ;       turkmen
    ;       uighur_china
    ;       ukrainian
    ;       urdu
    ;       urdu_india
    ;       uzbek_cyrillic
    ;       uzbek_latin
    ;       venda
    ;       vietnamese
    ;       welsh
    ;       xhosa
    ;       yi
    ;       yiddish
    ;       yoruba
    ;       zulu
    ;       hid_human_interface_device.

    % The following three types describe the possible errors that could occur
    % while generating a Wix source file.

:- type wix_result
    --->    ok
    ;       wix_error(wix_error).

:- type wix_error
    --->    some [L] no_translation(L, language)
    ;       io_error(io.error)
    ;       guid_gen_error(guid_error).

:- type guid_error
    --->    guid_eof
    ;       guid_io_error(io.error)
    ;       guid_cmd_error(io.system_result).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module wix_installer.
:- import_module wix_files.
:- import_module wix_util.
:- import_module wix_language.

:- import_module exception.
:- import_module term_to_xml.
:- import_module univ.

%-----------------------------------------------------------------------------%

generate_installer(Installer, GUIDFile, FileName, Result, !IO) :-
    io.open_output(FileName, OpenOutputResult, !IO),
    (
        OpenOutputResult = ok(OutStream),
        io.open_input(GUIDFile, OpenInputResult, !IO),
        (
            OpenInputResult = ok(GUIDStream),
            %
            % We know that, operationally, for a given input,
            % gen_annotated_installer will either always throw an exception
            % or always succeed.
            %
            promise_equivalent_solutions [TryResult, !:IO] (
                try_io(
                    wix_installer.gen_annotated_installer(Installer, GUIDStream),
                    TryResult,
                    !IO)
            ),
            (
                TryResult = succeeded(AnnInstaller),
                write_xml_doc(OutStream, AnnInstaller, !IO),
                Result = ok
            ;
                TryResult = exception(Univ),
                ( if univ_to_type(Univ, WixError) then
                    Result = wix_error(WixError)
                else
                    throw(Univ)
                )
            )
        ;
            OpenInputResult = error(IOError),
            Result = wix_error(io_error(IOError))
        )
    ;
        OpenOutputResult = error(IOError),
        Result = wix_error(io_error(IOError))
    ).

welcome_wizard_step(Heading, Message) = wizard_start(Heading, Message).

wix.license_wizard_step(Heading, Instructions, LicenseSrc) =
    wix_gui.license_wizard_step(Heading, Instructions, LicenseSrc).

wix.notice_wizard_step(Heading, Message) =
    wix_gui.notice_wizard_step(Heading, Message).
