%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This program demonstrates how to access the contents of an
% Allegro datafile (created by the grabber utility). The example
% loads the file `example.dat', then blits a bitmap and shows
% a font, both from this datafile.
% 
%-----------------------------------------------------------------------------%

:- module exdata.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.color.
:- import_module allegro.datafile.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.load_bitmap.
:- import_module allegro.keyboard.
:- import_module allegro.palette.
:- import_module allegro.text.

:- import_module bool.
:- import_module maybe.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(SystemOk, !IO),
    (
        SystemOk = yes,
        install_keyboard(KeyboardOk, !IO),
        (
            KeyboardOk = yes,
            set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
            (
                GfxOk = yes,
                main_2(!IO),
                set_gfx_mode(gfx_text, 0, 0, 0, 0, _, !IO)
            ;
                GfxOk = no
            ),
            remove_keyboard(!IO)
        ;
            KeyboardOk = no
        ),
        allegro_exit(!IO)
    ;
        SystemOk = no
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    set_color_conversion(colorconv_none, !IO),
    load_datafile("example.dat", MaybeDatafile, !IO),
    (
        MaybeDatafile = yes(Datafile),
        (
            find_datafile_object(Datafile, "THE_PALETTE", yes(ThePaletteObj)),
            dat_palette(ThePaletteObj, Palette)
        ->
            set_palette(Palette, !IO)
        ;
            true
        ),
        set_color_conversion(colorconv_total, !IO),
        det_screen(Screen, !IO),
        font(Font, !IO),
        makecol(255, 255, 255, White, !IO),
        makecol(0, 255, 0, Green, !IO),
        textout_ex(Screen, Font, "This is the bitmap:", 32, 16, White, -1,
            !IO),
        (
            find_datafile_object(Datafile, "SILLY_BITMAP",
                yes(SillyBitmapObj)),
            dat_bitmap(SillyBitmapObj, SillyBitmap)
        ->
            blit(SillyBitmap, Screen, 0, 0, 64, 32, 64, 64, !IO)
        ;
            true
        ),
        (
            find_datafile_object(Datafile, "BIG_FONT", yes(BigFontObj)),
            dat_font(BigFontObj, BigFont)
        ->
            textout_ex(Screen, BigFont, "And this is a big font!",
                32, 128, Green, -1, !IO)
        ;
            true
        ),
        readkey(_, !IO),
        unload_datafile(Datafile, !IO)
    ;
        MaybeDatafile = no
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
