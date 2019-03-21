% vim: ft=mercury ts=4 sw=4 et
:- module hello.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module posix.
:- import_module posix.open.
:- import_module posix.write.

:- import_module bitmap.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    open("/dev/tty", [wronly], Res0, !IO),
    (
        Res0 = ok(Fd),
        Str = "hello world.\n",
        length(Str, Len),
        write(Fd, Len, string_to_bitmap(Str), Res1, !IO),
        (
            Res1 = ok(NWritten),
            ( NWritten \= Len ->
                % We didn't write all of it!
                io.write_string("failed to write it all\n", !IO)
            ;
                true 
            )
        ;
            Res1 = error(Err),
            io.write(Err, !IO),
            io.nl(!IO)
        )
    ;
        Res0 = error(Err),
        io.write(Err, !IO),
        io.nl(!IO)
    ).

:- func string_to_bitmap(string::in) = (bitmap::bitmap_uo) is det.

string_to_bitmap(String) = Bitmap :-
    NumBytes = string.length(String),
    Bitmap0 = bitmap.init(NumBytes * bits_per_byte),
    string.to_char_list(String, Chars),
    char_list_to_bitmap(Chars, 0, Bitmap0, Bitmap). 

:- pred char_list_to_bitmap(list(char)::in, int::in,
    bitmap::bitmap_di, bitmap::bitmap_uo) is det.

char_list_to_bitmap([], _, !Bitmap).
char_list_to_bitmap([C | Cs], Index, !Bitmap) :-
    char.to_int(C, I),
    !:Bitmap = !.Bitmap ^ byte(Index) := I,
    char_list_to_bitmap(Cs, Index + 1, !Bitmap). 
