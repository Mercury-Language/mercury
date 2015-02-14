%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test.  bit_buffer.write.finalize_to_bitmap aborted on some input.
%
% Uncaught Mercury exception:
% bitmap_error("copy_bits (source): 0 bits starting at bit 0
%   is out of bounds [0, 0).")
%
%---------------------------------------------------------------------------%

:- module finalize_to_bitmap.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module bit_buffer.
:- import_module bit_buffer.write.

%---------------------------------------------------------------------------%

main(!IO) :-
    some [!Buf] (
        !:Buf = new_bitmap_builder(8),
        put_byte(1, !Buf),
        put_byte(2, !Buf),
        put_byte(3, !Buf),
        put_byte(4, !Buf),
        put_byte(5, !Buf),
        put_byte(6, !Buf),
        put_byte(7, !Buf),
        put_byte(8, !Buf),
        Bitmap = finalize_to_bitmap(!.Buf),
        io.write(Bitmap, !IO),
        io.nl(!IO)
    ).
