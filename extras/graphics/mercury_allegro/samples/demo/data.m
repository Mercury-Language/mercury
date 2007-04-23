%-----------------------------------------------------------------------------%

:- module data.
:- interface.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.datafile.
:- import_module allegro.digi.
:- import_module allegro.flic.
:- import_module allegro.midi.
:- import_module allegro.palette.
:- import_module allegro.rle.
:- import_module allegro.text.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type data --->
        data(
            asta        :: list(rle_sprite),
            astb        :: list(rle_sprite),
            astc        :: list(rle_sprite),
            boom_spl    :: sample,
            death_spl   :: sample,
            end_font    :: font,
            engine      :: list(rle_sprite),
            engine_spl  :: sample,
            game_music  :: midi,
            game_pal    :: palette,
            go_bmp      :: bitmap,
            intro_anim  :: memory_fli,
            intro_bmp_1 :: bitmap,
            intro_bmp_2 :: bitmap,
            intro_bmp_3 :: bitmap,
            intro_bmp_4 :: bitmap,
            intro_music :: midi,
            intro_spl   :: sample,
            rocket      :: rle_sprite,
            ship1       :: rle_sprite,
            ship2       :: rle_sprite,
            ship3       :: rle_sprite,
            ship4       :: rle_sprite,
            ship5       :: rle_sprite,
            shoot_spl   :: sample,
            title_bmp   :: bitmap,
            title_font  :: font,
            title_music :: midi,
            title_pal   :: palette,
            welcome_spl :: sample,
            explosions  :: list(rle_sprite)
        ).

:- pred init_data(outer_datafile::in, list(rle_sprite)::in, data::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

init_data(DF, Explosions, Data) :-
    % Ick.
    find_datafile_object_dat(DF, "ASTA01",      dat_rle_sprite, ASTA01),
    find_datafile_object_dat(DF, "ASTA02",      dat_rle_sprite, ASTA02),
    find_datafile_object_dat(DF, "ASTA03",      dat_rle_sprite, ASTA03),
    find_datafile_object_dat(DF, "ASTA04",      dat_rle_sprite, ASTA04),
    find_datafile_object_dat(DF, "ASTA05",      dat_rle_sprite, ASTA05),
    find_datafile_object_dat(DF, "ASTA06",      dat_rle_sprite, ASTA06),
    find_datafile_object_dat(DF, "ASTA07",      dat_rle_sprite, ASTA07),
    find_datafile_object_dat(DF, "ASTA08",      dat_rle_sprite, ASTA08),
    find_datafile_object_dat(DF, "ASTA09",      dat_rle_sprite, ASTA09),
    find_datafile_object_dat(DF, "ASTA10",      dat_rle_sprite, ASTA10),
    find_datafile_object_dat(DF, "ASTA11",      dat_rle_sprite, ASTA11),
    find_datafile_object_dat(DF, "ASTA12",      dat_rle_sprite, ASTA12),
    find_datafile_object_dat(DF, "ASTA13",      dat_rle_sprite, ASTA13),
    find_datafile_object_dat(DF, "ASTA14",      dat_rle_sprite, ASTA14),
    find_datafile_object_dat(DF, "ASTA15",      dat_rle_sprite, ASTA15),
    find_datafile_object_dat(DF, "ASTB01",      dat_rle_sprite, ASTB01),
    find_datafile_object_dat(DF, "ASTB02",      dat_rle_sprite, ASTB02),
    find_datafile_object_dat(DF, "ASTB03",      dat_rle_sprite, ASTB03),
    find_datafile_object_dat(DF, "ASTB04",      dat_rle_sprite, ASTB04),
    find_datafile_object_dat(DF, "ASTB05",      dat_rle_sprite, ASTB05),
    find_datafile_object_dat(DF, "ASTB06",      dat_rle_sprite, ASTB06),
    find_datafile_object_dat(DF, "ASTB07",      dat_rle_sprite, ASTB07),
    find_datafile_object_dat(DF, "ASTB08",      dat_rle_sprite, ASTB08),
    find_datafile_object_dat(DF, "ASTB09",      dat_rle_sprite, ASTB09),
    find_datafile_object_dat(DF, "ASTB10",      dat_rle_sprite, ASTB10),
    find_datafile_object_dat(DF, "ASTB11",      dat_rle_sprite, ASTB11),
    find_datafile_object_dat(DF, "ASTB12",      dat_rle_sprite, ASTB12),
    find_datafile_object_dat(DF, "ASTB13",      dat_rle_sprite, ASTB13),
    find_datafile_object_dat(DF, "ASTB14",      dat_rle_sprite, ASTB14),
    find_datafile_object_dat(DF, "ASTB15",      dat_rle_sprite, ASTB15),
    find_datafile_object_dat(DF, "ASTC01",      dat_rle_sprite, ASTC01),
    find_datafile_object_dat(DF, "ASTC02",      dat_rle_sprite, ASTC02),
    find_datafile_object_dat(DF, "ASTC03",      dat_rle_sprite, ASTC03),
    find_datafile_object_dat(DF, "ASTC04",      dat_rle_sprite, ASTC04),
    find_datafile_object_dat(DF, "ASTC05",      dat_rle_sprite, ASTC05),
    find_datafile_object_dat(DF, "ASTC06",      dat_rle_sprite, ASTC06),
    find_datafile_object_dat(DF, "ASTC07",      dat_rle_sprite, ASTC07),
    find_datafile_object_dat(DF, "ASTC08",      dat_rle_sprite, ASTC08),
    find_datafile_object_dat(DF, "ASTC09",      dat_rle_sprite, ASTC09),
    find_datafile_object_dat(DF, "ASTC10",      dat_rle_sprite, ASTC10),
    find_datafile_object_dat(DF, "ASTC11",      dat_rle_sprite, ASTC11),
    find_datafile_object_dat(DF, "ASTC12",      dat_rle_sprite, ASTC12),
    find_datafile_object_dat(DF, "ASTC13",      dat_rle_sprite, ASTC13),
    find_datafile_object_dat(DF, "ASTC14",      dat_rle_sprite, ASTC14),
    find_datafile_object_dat(DF, "ASTC15",      dat_rle_sprite, ASTC15),
    find_datafile_object_dat(DF, "BOOM_SPL",    dat_sample,     BOOM_SPL),
    find_datafile_object_dat(DF, "DEATH_SPL",   dat_sample,     DEATH_SPL),
    find_datafile_object_dat(DF, "END_FONT",    dat_font,       END_FONT),
    find_datafile_object_dat(DF, "ENGINE1",     dat_rle_sprite, ENGINE1),
    find_datafile_object_dat(DF, "ENGINE2",     dat_rle_sprite, ENGINE2),
    find_datafile_object_dat(DF, "ENGINE3",     dat_rle_sprite, ENGINE3),
    find_datafile_object_dat(DF, "ENGINE4",     dat_rle_sprite, ENGINE4),
    find_datafile_object_dat(DF, "ENGINE5",     dat_rle_sprite, ENGINE5),
    find_datafile_object_dat(DF, "ENGINE6",     dat_rle_sprite, ENGINE6),
    find_datafile_object_dat(DF, "ENGINE7",     dat_rle_sprite, ENGINE7),
    find_datafile_object_dat(DF, "ENGINE_SPL",  dat_sample,     ENGINE_SPL),
    find_datafile_object_dat(DF, "GAME_MUSIC",  dat_midi,       GAME_MUSIC),
    find_datafile_object_dat(DF, "GAME_PAL",    dat_palette,    GAME_PAL),
    find_datafile_object_dat(DF, "GO_BMP",      dat_bitmap,     GO_BMP),
    find_datafile_object_dat(DF, "INTRO_ANIM",  dat_fli,        INTRO_ANIM),
    find_datafile_object_dat(DF, "INTRO_BMP_1", dat_bitmap,     INTRO_BMP_1),
    find_datafile_object_dat(DF, "INTRO_BMP_2", dat_bitmap,     INTRO_BMP_2),
    find_datafile_object_dat(DF, "INTRO_BMP_3", dat_bitmap,     INTRO_BMP_3),
    find_datafile_object_dat(DF, "INTRO_BMP_4", dat_bitmap,     INTRO_BMP_4),
    find_datafile_object_dat(DF, "INTRO_MUSIC", dat_midi,       INTRO_MUSIC),
    find_datafile_object_dat(DF, "INTRO_SPL",   dat_sample,     INTRO_SPL),
    find_datafile_object_dat(DF, "ROCKET",      dat_rle_sprite, ROCKET),
    find_datafile_object_dat(DF, "SHIP1",       dat_rle_sprite, SHIP1),
    find_datafile_object_dat(DF, "SHIP2",       dat_rle_sprite, SHIP2),
    find_datafile_object_dat(DF, "SHIP3",       dat_rle_sprite, SHIP3),
    find_datafile_object_dat(DF, "SHIP4",       dat_rle_sprite, SHIP4),
    find_datafile_object_dat(DF, "SHIP5",       dat_rle_sprite, SHIP5),
    find_datafile_object_dat(DF, "SHOOT_SPL",   dat_sample,     SHOOT_SPL),
    find_datafile_object_dat(DF, "TITLE_BMP",   dat_bitmap,     TITLE_BMP),
    find_datafile_object_dat(DF, "TITLE_FONT",  dat_font,       TITLE_FONT),
    find_datafile_object_dat(DF, "TITLE_MUSIC", dat_midi,       TITLE_MUSIC),
    find_datafile_object_dat(DF, "TITLE_PAL",   dat_palette,    TITLE_PAL),
    find_datafile_object_dat(DF, "WELCOME_SPL", dat_sample,     WELCOME_SPL),
    Data = data(
        [ASTA01, ASTA02, ASTA03, ASTA04, ASTA05, ASTA06, ASTA07,
         ASTA08, ASTA09, ASTA10, ASTA11, ASTA12, ASTA13, ASTA14, ASTA15],
        [ASTB01, ASTB02, ASTB03, ASTB04, ASTB05, ASTB06, ASTB07,
         ASTB08, ASTB09, ASTB10, ASTB11, ASTB12, ASTB13, ASTB14, ASTB15],
        [ASTC01, ASTC02, ASTC03, ASTC04, ASTC05, ASTC06, ASTC07,
         ASTC08, ASTC09, ASTC10, ASTC11, ASTC12, ASTC13, ASTC14, ASTC15],
        BOOM_SPL,
        DEATH_SPL,
        END_FONT,
        [ENGINE1, ENGINE2, ENGINE3, ENGINE4, ENGINE5, ENGINE6, ENGINE7], 
        ENGINE_SPL,
        GAME_MUSIC,
        GAME_PAL,
        GO_BMP,
        INTRO_ANIM,
        INTRO_BMP_1,
        INTRO_BMP_2,
        INTRO_BMP_3,
        INTRO_BMP_4,
        INTRO_MUSIC,
        INTRO_SPL,
        ROCKET,
        SHIP1,
        SHIP2,
        SHIP3,
        SHIP4,
        SHIP5,
        SHOOT_SPL,
        TITLE_BMP,
        TITLE_FONT,
        TITLE_MUSIC,
        TITLE_PAL,
        WELCOME_SPL,
        Explosions
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
