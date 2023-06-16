%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test store references (bleh!)

:- module store_ref.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module store.

:- type enum
    --->    enum1
    ;       enum2
    ;       enum3.

:- type struct
    --->    unused_struct(int, int)
    ;       struct(int, float, enum, enum, enum, string, {int, int}).

%---------------------------------------------------------------------------%

main(!IO) :-
    Term = struct(1, 2.2, enum1, enum2, enum3, "four", {5, 6}),
    new_ref(Term, TermRef, !IO),
    ref_functor(TermRef, Functor, Arity, !IO),
    io.write_line({Functor, Arity}, !IO),
    copy_ref_value(TermRef, CopyTerm, !IO),
    io.write_line(CopyTerm, !IO),

    arg_ref(TermRef, 0, IntRef, !IO),
    arg_ref(TermRef, 1, FloatRef, !IO),
    arg_ref(TermRef, 2, EnumRefA, !IO),
    arg_ref(TermRef, 3, EnumRefB, !IO),
    arg_ref(TermRef, 5, StringRef, !IO),
    arg_ref(TermRef, 6, TupleRef, !IO),
    copy_ref_value(IntRef, Int : int, !IO),
    copy_ref_value(FloatRef, Float : float, !IO),
    copy_ref_value(EnumRefA, EnumA : enum, !IO),
    copy_ref_value(EnumRefB, EnumB : enum, !IO),
    copy_ref_value(StringRef, String : string, !IO),
    copy_ref_value(TupleRef, Tuple : {int, int}, !IO),
    io.write_line({Int, Float, EnumA, EnumB, String, Tuple}, !IO),

    copy(Term, TermA),
    new_arg_ref(TermA, 1, NewFloatRef, !IO),
    copy_ref_value(NewFloatRef, NewFloatA, !IO),
    set_ref_value(NewFloatRef, 3.3, !IO),
    copy_ref_value(NewFloatRef, NewFloatB, !IO),
    io.write_line({NewFloatA, NewFloatB}, !IO),

    copy(Term, TermB),
    new_arg_ref(TermB, 3, NewEnumRef, !IO), % enum2
    copy_ref_value(NewEnumRef, NewEnumA, !IO),
    set_ref_value(NewEnumRef, enum3, !IO),
    copy_ref_value(NewEnumRef, NewEnumB, !IO),
    io.write_line({NewEnumA, NewEnumB}, !IO).
