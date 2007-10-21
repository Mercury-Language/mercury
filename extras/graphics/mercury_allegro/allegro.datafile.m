%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.datafile.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.datafile.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.compiled.
:- import_module allegro.digi.
:- import_module allegro.flic.
:- import_module allegro.midi.
:- import_module allegro.palette.
:- import_module allegro.rle.
:- import_module allegro.text.

:- import_module char.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type outer_datafile.
:- type inner_datafile.
:- type normal_datafile_object.
:- type loose_datafile_object.
:- type dat_id.

:- typeclass datafile(D).
:- instance datafile(outer_datafile).
:- instance datafile(inner_datafile).

:- typeclass datafile_object(DO).
:- instance datafile_object(normal_datafile_object).
:- instance datafile_object(loose_datafile_object).

:- pred load_datafile(string::in, maybe(outer_datafile)::out, io::di, io::uo)
    is det.
:- pred unload_datafile(outer_datafile::in, io::di, io::uo) is det.

:- pred load_datafile_object(string::in, string::in,
    maybe(loose_datafile_object)::out, io::di, io::uo) is det.
:- pred unload_datafile_object(loose_datafile_object::in, io::di, io::uo)
    is det.

:- pred find_datafile_object(D::in, string::in,
    maybe(normal_datafile_object)::out) is det
    <= datafile(D).
:- pred get_datafile_property(DO::in, dat_id::in, maybe(string)::out) is det
    <= datafile_object(DO).
% register_datafile_object
:- pred fixup_datafile(D::in, io::di, io::uo) is det <= datafile(D).
:- func dat_id(char, char, char, char) = dat_id.

    % Additions to the C API.
    %
:- pred get_datafile_object(D::in, int::in, normal_datafile_object::out)
    is det <= datafile(D).
:- pred find_datafile_object_dat(D::in, string::in,
    pred(normal_datafile_object, U)::in(pred(in, out) is semidet), U::out)
    is semidet <= datafile(D).

:- pred dat_file(DO::in, inner_datafile::out) is semidet
    <= datafile_object(DO).
% dat_data
:- pred dat_font(DO::in, font::out) is semidet
    <= datafile_object(DO).
:- pred dat_sample(DO::in, sample::out) is semidet
    <= datafile_object(DO).
:- pred dat_midi(DO::in, midi::out) is semidet
    <= datafile_object(DO).
% dat_patch
:- pred dat_fli(DO::in, memory_fli::out) is semidet
    <= datafile_object(DO).
:- pred dat_bitmap(DO::in, bitmap::out) is semidet
    <= datafile_object(DO).
:- pred dat_rle_sprite(DO::in, rle_sprite::out) is semidet
    <= datafile_object(DO).
:- pred dat_compiled_sprite(DO::in, compiled_sprite::out) is semidet
    <= datafile_object(DO).
:- pred dat_palette(DO::in, palette::out) is semidet
    <= datafile_object(DO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

:- pragma foreign_import_module("C", allegro).

%-----------------------------------------------------------------------------%

:- type datafile_cptr.
:- type datafile_object_cptr.

:- pragma foreign_type("C", datafile_cptr, "DATAFILE *",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C", datafile_object_cptr, "DATAFILE *",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C", dat_id, "int").

:- type outer_datafile
    --->    outer_datafile(
                outer_datafile_get_cptr :: datafile_cptr
            ).

:- type inner_datafile
    --->    inner_datafile(
                inner_datafile_get_cptr :: datafile_cptr
            ).

:- type normal_datafile_object
    --->    normal_datafile_object(
                normal_datafile_object_get_cptr :: datafile_object_cptr
            ).

:- type loose_datafile_object
    --->    loose_datafile_object(
                loose_datafile_object_get_cptr :: datafile_object_cptr
            ).

:- typeclass datafile(D) where [
    func get_cptr(D) = datafile_cptr is det
].

:- instance datafile(outer_datafile) where [
    func(get_cptr/1) is outer_datafile_get_cptr
].

:- instance datafile(inner_datafile) where [
    func(get_cptr/1) is inner_datafile_get_cptr
].

:- typeclass datafile_object(DO) where [
    func get_obj_cptr(DO) = datafile_object_cptr is det
].

:- instance datafile_object(normal_datafile_object) where [
    func(get_obj_cptr/1) is normal_datafile_object_get_cptr
].

:- instance datafile_object(loose_datafile_object) where [
    func(get_obj_cptr/1) is loose_datafile_object_get_cptr
].

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    load_datafile(Filename::in, MaybeDatafile::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    DATAFILE *Datafile = load_datafile(Filename);
    if (Datafile) {
        MaybeDatafile = _mal_make_yes_outer_datafile(Datafile);
    } else {
        MaybeDatafile = _mal_make_no_outer_datafile();
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

unload_datafile(outer_datafile(Datafile), !IO) :-
    unload_datafile_2(Datafile, !IO).

:- pred unload_datafile_2(datafile_cptr::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unload_datafile_2(Datafile::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    unload_datafile(Datafile);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    load_datafile_object(Filename::in, Objectname::in, MaybeObject::out,
        IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    DATAFILE *Object = load_datafile_object(Filename, Objectname);
    if (Object) {
        MaybeObject = _mal_make_yes_loose_datafile_object(Object);
    } else {
        MaybeObject = _mal_make_no_loose_datafile_object();
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

unload_datafile_object(loose_datafile_object(Object), !IO) :-
    unload_datafile_object_2(Object, !IO).

:- pred unload_datafile_object_2(datafile_object_cptr::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    unload_datafile_object_2(Object::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    unload_datafile_object(Object);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

find_datafile_object(Datafile, Objectname, MaybeObject) :-
    find_datafile_object_2(get_cptr(Datafile), Objectname, MaybeObject).

:- pred find_datafile_object_2(datafile_cptr::in, string::in,
    maybe(normal_datafile_object)::out) is det.

:- pragma foreign_proc("C",
    find_datafile_object_2(Datafile::in, Objectname::in, MaybeObject::out),
    [may_call_mercury, promise_pure],
"
    DATAFILE *Object = find_datafile_object(Datafile, Objectname);
    if (Object) {
        MaybeObject = _mal_make_yes_normal_datafile_object(Object);
    } else {
        MaybeObject = _mal_make_no_normal_datafile_object();
    }
").

%-----------------------------------------------------------------------------%

get_datafile_property(DO, Type, MaybeProperty) :-
    get_datafile_property_2(get_obj_cptr(DO), Type, MaybeProperty).

:- pred get_datafile_property_2(datafile_object_cptr::in, dat_id::in,
    maybe(string)::out) is det.

:- pragma foreign_proc("C",
    get_datafile_property_2(Object::in, Type::in, MaybeProperty::out),
    [may_call_mercury, promise_pure],
"
    const char *Property = get_datafile_property(Object, Type);
    if (Property && Property[0]) {
        MaybeProperty = _mal_make_yes_string((char *)Property);
    } else {
        MaybeProperty = _mal_make_no_string();
    }
").

%-----------------------------------------------------------------------------%

fixup_datafile(Datafile, !IO) :-
    fixup_datafile_2(get_cptr(Datafile), !IO).

:- pred fixup_datafile_2(datafile_cptr::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fixup_datafile_2(Datafile::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fixup_datafile(Datafile);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    dat_id(A::in, B::in, C::in, D::in) = (Result::out),
    [will_not_call_mercury, promise_pure],
"
    Result = DAT_ID(A, B, C, D);
").

%-----------------------------------------------------------------------------%

get_datafile_object(Datafile, Index, Object) :-
    get_datafile_object_2(get_cptr(Datafile), Index, Object).

:- pred get_datafile_object_2(datafile_cptr::in, int::in,
    normal_datafile_object::out) is det.

:- pragma foreign_proc("C",
    get_datafile_object_2(Datafile::in, Index::in, Object::out),
    [may_call_mercury, promise_pure],
"
    DATAFILE *CPointer = &Datafile[Index];
    Object = _mal_make_normal_datafile_object(CPointer);
").

%-----------------------------------------------------------------------------%

find_datafile_object_dat(Datafile, Objectname, Adaptor, Dat) :-
    find_datafile_object(Datafile, Objectname, MaybeObject),
    MaybeObject = yes(Object),
    Adaptor(Object, Dat).

%-----------------------------------------------------------------------------%

dat_file(DO, Result) :-
    dat_file_2(get_obj_cptr(DO), Result).

:- pred dat_file_2(datafile_object_cptr::in, inner_datafile::out) is semidet.
:- pragma foreign_proc("C",
    dat_file_2(Object::in, Result::out),
    [may_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = (Object->type == DAT_FONT);
    if (SUCCESS_INDICATOR) {
        Result = _mal_make_inner_datafile(Object->dat);
    }
").

dat_font(DO, Result) :-
    dat_font_2(get_obj_cptr(DO), Result).

:- pred dat_font_2(datafile_object_cptr::in, font::out) is semidet.
:- pragma foreign_proc("C",
    dat_font_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_FONT);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_sample(DO, Result) :-
    dat_sample_2(get_obj_cptr(DO), Result).

:- pred dat_sample_2(datafile_object_cptr::in, sample::out) is semidet.
:- pragma foreign_proc("C",
    dat_sample_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_SAMPLE);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_midi(DO, Result) :-
    dat_midi_2(get_obj_cptr(DO), Result).

:- pred dat_midi_2(datafile_object_cptr::in, midi::out) is semidet.
:- pragma foreign_proc("C",
    dat_midi_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_MIDI);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_fli(DO, Result) :-
    dat_fli_2(get_obj_cptr(DO), Result).

:- pred dat_fli_2(datafile_object_cptr::in, memory_fli::out) is semidet.
:- pragma foreign_proc("C",
    dat_fli_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_FLI);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_bitmap(DO, Result) :-
    dat_bitmap_2(get_obj_cptr(DO), Result).

:- pred dat_bitmap_2(datafile_object_cptr::in, bitmap::out) is semidet.
:- pragma foreign_proc("C",
    dat_bitmap_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_BITMAP);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_rle_sprite(DO, Result) :-
    dat_rle_sprite_2(get_obj_cptr(DO), Result).

:- pred dat_rle_sprite_2(datafile_object_cptr::in, rle_sprite::out) is semidet.
:- pragma foreign_proc("C",
    dat_rle_sprite_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_RLE_SPRITE);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_compiled_sprite(DO, Result) :-
    dat_compiled_sprite_2(get_obj_cptr(DO), Result).

:- pred dat_compiled_sprite_2(datafile_object_cptr::in, compiled_sprite::out)
    is semidet.
:- pragma foreign_proc("C",
    dat_compiled_sprite_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (
        Object->type == DAT_C_SPRITE ||
        Object->type == DAT_XC_SPRITE
    );
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

dat_palette(DO, Result) :-
    dat_palette_2(get_obj_cptr(DO), Result).

:- pred dat_palette_2(datafile_object_cptr::in, palette::out) is semidet.
:- pragma foreign_proc("C",
    dat_palette_2(Object::in, Result::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Object->type == DAT_PALETTE);
    if (SUCCESS_INDICATOR) {
        Result = Object->dat;
    }
").

%-----------------------------------------------------------------------------%

:- func make_inner_datafile(datafile_cptr) = inner_datafile.

:- pragma foreign_export("C", make_inner_datafile(in) = out,
    "_mal_make_inner_datafile").

make_inner_datafile(X) = inner_datafile(X).

%-----------------------------------------------------------------------------%

:- func make_yes_outer_datafile(datafile_cptr) = maybe(outer_datafile).
:- func make_no_outer_datafile = maybe(outer_datafile).

:- pragma foreign_export("C", make_yes_outer_datafile(in) = out,
    "_mal_make_yes_outer_datafile").
:- pragma foreign_export("C", make_no_outer_datafile = out,
    "_mal_make_no_outer_datafile").

make_yes_outer_datafile(X) = yes(outer_datafile(X)).
make_no_outer_datafile = no.

%-----------------------------------------------------------------------------%

:- func make_yes_inner_datafile(datafile_cptr) = maybe(inner_datafile).
:- func make_no_inner_datafile = maybe(inner_datafile).

:- pragma foreign_export("C", make_yes_inner_datafile(in) = out,
    "_mal_make_yes_inner_datafile").
:- pragma foreign_export("C", make_no_inner_datafile = out,
    "_mal_make_no_inner_datafile").

make_yes_inner_datafile(X) = yes(inner_datafile(X)).
make_no_inner_datafile = no.

%-----------------------------------------------------------------------------%

:- func make_normal_datafile_object(datafile_object_cptr) =
    normal_datafile_object.

:- pragma foreign_export("C", make_normal_datafile_object(in) = out,
    "_mal_make_normal_datafile_object").

make_normal_datafile_object(X) = normal_datafile_object(X).

%-----------------------------------------------------------------------------%

:- func make_yes_normal_datafile_object(datafile_object_cptr) =
    maybe(normal_datafile_object).
:- func make_no_normal_datafile_object =
    maybe(normal_datafile_object).

:- pragma foreign_export("C", make_yes_normal_datafile_object(in) = out,
    "_mal_make_yes_normal_datafile_object").
:- pragma foreign_export("C", make_no_normal_datafile_object = out,
    "_mal_make_no_normal_datafile_object").

make_yes_normal_datafile_object(X) = yes(normal_datafile_object(X)).
make_no_normal_datafile_object = no.

%-----------------------------------------------------------------------------%

:- func make_yes_loose_datafile_object(datafile_object_cptr) =
    maybe(loose_datafile_object).
:- func make_no_loose_datafile_object =
    maybe(loose_datafile_object).

:- pragma foreign_export("C", make_yes_loose_datafile_object(in) = out,
    "_mal_make_yes_loose_datafile_object").
:- pragma foreign_export("C", make_no_loose_datafile_object = out,
    "_mal_make_no_loose_datafile_object").

make_yes_loose_datafile_object(X) = yes(loose_datafile_object(X)).
make_no_loose_datafile_object = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
