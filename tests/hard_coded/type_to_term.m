%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test various special cases for type_to_term/2 and term_to_type/2.
%

:- module type_to_term.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module list.
:- import_module require.
:- import_module term.
:- import_module term_conversion.
:- import_module term_io.
:- import_module type_desc.
:- import_module univ.
:- import_module varset.
:- import_module version_array.

main(!IO) :-
    varset.init(VarSet),

    % Test handling of characters
    %
    Char = 'A',
    type_to_term(Char, CharTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, CharTerm, !IO),
    io.nl(!IO),
    det_term_to_type(CharTerm, CharValue : character),
    io.write_string("Type: ", !IO),
    io.write_char(CharValue, !IO),
    io.nl(!IO),

    % Test handling of ints.
    %
    Int = 42,
    type_to_term(Int, IntTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, IntTerm, !IO),
    io.nl(!IO),
    det_term_to_type(IntTerm, IntValue : int),
    io.write_string("Type: ", !IO),
    io.write_int(IntValue, !IO),
    io.nl(!IO),

    % Test handling of uints.
    %
    UInt = 42u,
    type_to_term(UInt, UIntTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, UIntTerm, !IO),
    io.nl(!IO),
    det_term_to_type(UIntTerm, UIntValue : uint),
    io.write_string("Type: ", !IO),
    io.write_uint(UIntValue, !IO),
    io.nl(!IO),

    % Test handling int8s.
    %
    Int8 = 42i8,
    type_to_term(Int8, Int8Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, Int8Term, !IO),
    io.nl(!IO),
    det_term_to_type(Int8Term, Int8Value : int8),
    io.write_string("Type: ", !IO),
    io.write_int8(Int8Value, !IO),
    io.nl(!IO),

    % Test handling uint8s.
    %
    UInt8 = 42u8,
    type_to_term(UInt8, UInt8Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, UInt8Term, !IO),
    io.nl(!IO),
    det_term_to_type(UInt8Term, UInt8Value : uint8),
    io.write_string("Type: ", !IO),
    io.write_uint8(UInt8Value, !IO),
    io.nl(!IO),

    % Test handling int16s.
    %
    Int16 = 42i16,
    type_to_term(Int16, Int16Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, Int16Term, !IO),
    io.nl(!IO),
    det_term_to_type(Int16Term, Int16Value : int16),
    io.write_string("Type: ", !IO),
    io.write_int16(Int16Value, !IO),
    io.nl(!IO),

    % Test handling uint16s.
    %
    UInt16 = 42u16,
    type_to_term(UInt16, UInt16Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, UInt16Term, !IO),
    io.nl(!IO),
    det_term_to_type(UInt16Term, UInt16Value : uint16),
    io.write_string("Type: ", !IO),
    io.write_uint16(UInt16Value, !IO),
    io.nl(!IO),

    % Test handling int32s.
    %
    Int32 = 42i32,
    type_to_term(Int32, Int32Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, Int32Term, !IO),
    io.nl(!IO),
    det_term_to_type(Int32Term, Int32Value : int32),
    io.write_string("Type: ", !IO),
    io.write_int32(Int32Value, !IO),
    io.nl(!IO),

    % Test handling uint32s.
    %
    UInt32 = 42u32,
    type_to_term(UInt32, UInt32Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, UInt32Term, !IO),
    io.nl(!IO),
    det_term_to_type(UInt32Term, UInt32Value : uint32),
    io.write_string("Type: ", !IO),
    io.write_uint32(UInt32Value, !IO),
    io.nl(!IO),

    % Test handling int64s.
    %
    Int64 = 561i64,
    type_to_term(Int64, Int64Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, Int64Term, !IO),
    io.nl(!IO),
    det_term_to_type(Int64Term, Int64Value : int64),
    io.write_string("Type: ", !IO),
    io.write_int64(Int64Value, !IO),
    io.nl(!IO),

    % Test handling uint64s.
    %
    UInt64 = 561u64,
    type_to_term(UInt64, UInt64Term : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, UInt64Term, !IO),
    io.nl(!IO),
    det_term_to_type(UInt64Term, UInt64Value : uint64),
    io.write_string("Type: ", !IO),
    io.write_uint64(UInt64Value, !IO),
    io.nl(!IO),

    % Test handling of floats.
    %
    Float = 12345.6789,
    type_to_term(Float, FloatTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, FloatTerm, !IO),
    io.nl(!IO),
    det_term_to_type(FloatTerm, FloatValue : float),
    io.write_string("Type: ", !IO),
    io.write_float(FloatValue, !IO),
    io.nl(!IO),

    % Test handling of strings.
    %
    String = "abcdefghijklmnopqrstuvwxyz",
    type_to_term(String, StringTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, StringTerm, !IO),
    io.nl(!IO),
    det_term_to_type(StringTerm, StringValue : string),
    io.write_string("Type: ", !IO),
    io.write_string(StringValue, !IO),
    io.nl(!IO),

    % Ttest handling of bitmaps.
    %
    ( if Bitmap0 = bitmap.from_string("<24:10AFBD>") then
        Bitmap = Bitmap0
       else
        error("bitmap.from_string/1 failed")
    ),
    type_to_term(Bitmap, BitmapTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, BitmapTerm, !IO),
    io.nl(!IO),
    det_term_to_type(BitmapTerm, BitmapValue : bitmap),
    io.write_string("Type: ", !IO),
    io.write_string(bitmap.to_string(BitmapValue), !IO),
    io.nl(!IO),

    % Test handling of type_descs.
    %
    TypeDesc = type_of([1, 2, 3]),
    type_to_term(TypeDesc, TypeDescTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, TypeDescTerm, !IO),
    io.nl(!IO),
    % We don't currently support converting terms to type_descs.
    io.write_string("Type: ", !IO),
    ( if term_to_type(TypeDescTerm, TypeDescValue : type_desc) then
       io.write(TypeDescValue, !IO)
      else
       io.write_string("<<term_to_type/2 failed>> (as expected)", !IO)
    ),
    io.nl(!IO),

    % Test handling of arrays.
    %
    Array = array([1, 2, 3]),
    type_to_term(Array, ArrayTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, ArrayTerm, !IO),
    io.nl(!IO),
    det_term_to_type(ArrayTerm, ArrayValue : array(int)),
    io.write_string("Type: ", !IO),
    io.write_line(ArrayValue, !IO),

    % Test handling of version arrays.
    %
    VArray = version_array([1, 2, 3]),
    type_to_term(VArray, VArrayTerm : term(generic)),
    io.write_string("Term: ", !IO),
    term_io.write_term(VarSet, VArrayTerm, !IO),
    io.nl(!IO),
    det_term_to_type(VArrayTerm, VArrayValue : version_array(int)),
    io.write_string("Type: ", !IO),
    io.write_line(VArrayValue, !IO).
