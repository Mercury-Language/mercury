%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%----------------------------------------------------------------------------%
% Test non-verbose error messages produced for missing construtors in the
% constructor - foreign value mapping of foreign_enum pragmas.
%----------------------------------------------------------------------------%

:- module fe_unmapped_nonverbose.
:- interface.

    % Test for 1 missing foreign value.
    %
:- type voice
    --->    soprano
    ;       alto
    ;       tenor
    ;       bass.

    % Test for > 1 and < 11 missing foreign values.
    %
:- type chordophone
    --->    guitar
    ;       violin
    ;       lyre
    ;       harp
    ;       cello
    ;       banjo
    ;       double_bass
    ;       dulcimer
    ;       lute
    ;       piano
    ;       sitar
    ;       ukulele
    ;       viola.

    % Test for > 10 constructors missing foreign values.
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

:- pragma foreign_enum("C", voice/0, [
    soprano - "1",
    tenor - "3",
    bass  - "4"
]).

:- pragma foreign_enum("C", chordophone/0, [
    guitar - "0",
    violin - "1",
    lyre   - "2"
]).

:- pragma foreign_enum("C", shade_of_white/0, [
    anti_flash_white - "0xF2F3F4",
    antique_white    - "0xFAEBD7",
    beige            - "0xF5F5DC"
]).

%----------------------------------------------------------------------------%
