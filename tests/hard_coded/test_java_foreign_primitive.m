%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test support for primitive types as foreign_types in the Java grade.
%
%---------------------------------------------------------------------------%

:- module test_java_foreign_primitive.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.print_line(byte_to_string(byte(16) `plus_byte` byte(16)), !IO),
    io.print_line(short_to_string(short(16) `plus_short` short(16)), !IO),
    io.print_line(int_to_string(int(16) `plus_int` int(16)), !IO),
    io.print_line(long_to_string(long(16) `plus_long` long(16)), !IO),
    io.print_line(float_to_string(float(16) `plus_float` float(16)), !IO),
    io.print_line(double_to_string(double(16) `plus_double` double(16)), !IO),

    ListByte = [byte(-128), byte(-1), byte(0), byte(1), byte(127)],
    io.write_list(ListByte, ", ", write_byte, !IO),
    io.nl(!IO),

    ListShort = [short(-10), short(0), short(1), short(48)],
    io.write_list(ListShort, ", ", write_short, !IO),
    io.nl(!IO),

    ListInt = [int(-100), int(-1), int(0), int(561), int(60000)],
    io.write_list(ListInt, ", ", write_int, !IO),
    io.nl(!IO),

    ListLong = [long(1), long(2), long(60), long(231231231)],
    io.write_list(ListLong, ", ", write_long, !IO),
    io.nl(!IO),

    ListFloat = [float(-100), float(0), float(10), float(561)],
    io.write_list(ListFloat, ", ", write_float, !IO),
    io.nl(!IO),

    ListDouble = [double(-100), double(0), double(10), double(561)],
    io.write_list(ListDouble, ", ", write_double, !IO),
    io.nl(!IO),

    ListBoolean = [jtrue, jfalse, jfalse, jtrue],
    io.write_list(ListBoolean, ", ", write_boolean, !IO),
    io.nl(!IO),

    ListChar = [char(65), char(66), char(67), char(68)],
    io.write_list(ListChar, ", ", write_char, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- type jboolean.
:- type jbyte.
:- type jchar.
:- type jshort.
:- type jint.
:- type jlong.
:- type jfloat.
:- type jdouble.

:- pragma foreign_type("Java", jboolean, "boolean").
:- pragma foreign_type("Java", jchar, "char").
:- pragma foreign_type("Java", jbyte, "byte").
:- pragma foreign_type("Java", jshort, "short").
:- pragma foreign_type("Java", jint, "int").
:- pragma foreign_type("Java", jlong, "long").
:- pragma foreign_type("Java", jfloat, "float").
:- pragma foreign_type("Java", jdouble, "double").

%---------------------------------------------------------------------------%

:- func byte(int) = jbyte.
:- pragma foreign_proc("Java",
    byte(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (byte) A;
").

:- func short(int) = jshort.
:- pragma foreign_proc("Java",
    short(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (short) A;
").

:- func int(int) = jint.
:- pragma foreign_proc("Java",
    int(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A;
").

:- func long(int) = jlong.
:- pragma foreign_proc("Java",
    long(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = A;
").

:- func float(int) = jfloat.
:- pragma foreign_proc("Java",
    float(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (float) A;
").

:- func double(int) = jdouble.
:- pragma foreign_proc("Java",
    double(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (double) A;
").

:- func char(int) = jchar.
:- pragma foreign_proc("Java",
    char(A::in) = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = (char) A;
").

%---------------------------------------------------------------------------%

:- func plus_byte(jbyte, jbyte) = jbyte.
:- pragma foreign_proc("Java",
    plus_byte(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (byte) (A + B);
").

:- func plus_short(jshort, jshort) = jshort.
:- pragma foreign_proc("Java",
    plus_short(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = (short) (A + B);
").

:- func plus_int(jint, jint) = jint.
:- pragma foreign_proc("Java",
    plus_int(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A + B;
").

:- func plus_long(jlong, jlong) = jlong.
:- pragma foreign_proc("Java",
    plus_long(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A + B;
").

:- func plus_float(jfloat, jfloat) = jfloat.
:- pragma foreign_proc("Java",
    plus_float(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A + B;
").

:- func plus_double(jdouble, jdouble) = jdouble.
:- pragma foreign_proc("Java",
    plus_double(A::in, B::in) = (C::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = A + B;
").

%---------------------------------------------------------------------------%

:- func byte_to_string(jbyte) = string.
:- pragma foreign_proc("Java",
    byte_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(N);
").

:- func short_to_string(jshort) = string.
:- pragma foreign_proc("Java",
    short_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(N);
").

:- func int_to_string(jint) = string.
:- pragma foreign_proc("Java",
    int_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Integer.toString(N);
").

:- func long_to_string(jlong) = string.
:- pragma foreign_proc("Java",
    long_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Long.toString(N);
").

:- func float_to_string(jfloat) = string.
:- pragma foreign_proc("Java",
    float_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Float.toString(N);
").

:- func double_to_string(jdouble) = string.
:- pragma foreign_proc("Java",
    double_to_string(N::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Double.toString(N);
").

:- func boolean_to_string(jboolean) = string.
:- pragma foreign_proc("Java",
    boolean_to_string(B::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = B ? \"true\" : \"false\";
").

:- func char_to_string(jchar) = string.
:- pragma foreign_proc("Java",
    char_to_string(C::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = java.lang.Character.toString(C);
").

%---------------------------------------------------------------------------%

:- pred write_byte(jbyte::in, io::di, io::uo) is det.

write_byte(N, !IO) :-
    S = byte_to_string(N),
    io.write_string(S, !IO).

:- pred write_short(jshort::in, io::di, io::uo) is det.

write_short(N, !IO) :-
    S = short_to_string(N),
    io.write_string(S, !IO).

:- pred write_int(jint::in, io::di, io::uo) is det.

write_int(N, !IO) :-
    S = int_to_string(N),
    io.write_string(S, !IO).

:- pred write_long(jlong::in, io::di, io::uo) is det.

write_long(N, !IO) :-
    S = long_to_string(N),
    io.write_string(S, !IO).

:- pred write_float(jfloat::in, io::di, io::uo) is det.

write_float(N, !IO) :-
    S = float_to_string(N),
    io.write_string(S, !IO).

:- pred write_double(jdouble::in, io::di, io::uo) is det.

write_double(N, !IO) :-
    S = double_to_string(N),
    io.write_string(S, !IO).

:- pred write_boolean(jboolean::in, io::di, io::uo) is det.

write_boolean(B, !IO) :-
    S = boolean_to_string(B),
    io.write_string(S, !IO).

:- pred write_char(jchar::in, io::di, io::uo) is det.

write_char(C, !IO) :-
    S = char_to_string(C),
    io.write_string(S, !IO).

%---------------------------------------------------------------------------%

:- func jtrue = jboolean.
:- pragma foreign_proc("Java",
    jtrue = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = true;
").

:- func jfalse = jboolean.
:- pragma foreign_proc("Java",
    jfalse = (B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = false;
").

%---------------------------------------------------------------------------%
:- end_module test_java_foreign_primitive.
%---------------------------------------------------------------------------%
