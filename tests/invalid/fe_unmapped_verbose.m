%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%----------------------------------------------------------------------------%
% Test verbose error messages produced for missing construtors in the
% constructor - foreign value mapping of foreign_enum pragmas.
%----------------------------------------------------------------------------%

:- module fe_unmapped_verbose.
:- interface.

    % If --verbose-error-messages is enabled then all the constructors missing
    % foreign values should be listed.
    %
:- type shade_of_white
    --->    anti_flash_white
    ;       antique_white
    ;       beige
    ;       blond
    ;       cornsilk
    ;       cosmic_latte
    ;       cream
    ;       eggshell
    ;       floral_white
    ;       ghost_white
    ;       honeydew
    ;       isabelline
    ;       ivory
    ;       lavender_blush
    ;       lemon_chiffon
    ;       linen
    ;       magnolia
    ;       mint_cream
    ;       navajo_white
    ;       old_lace
    ;       papaya_whip
    ;       peach
    ;       pearl
    ;       seashell
    ;       snow
    ;       splashed_white
    ;       vanilla
    ;       white
    ;       white_smoke.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_enum("C", shade_of_white/0, [
    anti_flash_white - "0xF2F3F4",
    antique_white    - "0xFAEBD7",
    beige            - "0xF5F5DC"
]).

%----------------------------------------------------------------------------%
