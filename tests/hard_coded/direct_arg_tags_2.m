%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that the compiler properly handles construction and deconstruction
% involving direct_arg_tags both when the ptag is 0 and when it is nonzero.
%
%---------------------------------------------------------------------------%

:- module direct_arg_tags_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module string.

:- type test_3_tag_bits
    --->    direct_arg0(struct0)
    ;       direct_arg1(struct1)
    ;       direct_arg2(struct2)
    ;       direct_arg3(struct3)
    ;       direct_arg4(struct4)
    ;       direct_arg5(struct5)
    ;       direct_arg6(struct6)
    ;       direct_arg7(struct7)
    ;       non_direct_arg(int, int).

:- type struct0
    --->    struct0(int, int).

:- type struct1
    --->    struct1(int, int, bool).

:- type struct2
    --->    struct2(int, int, bool, bool).

:- type struct3
    --->    struct3(int, int, bool, bool, bool).

:- type struct4
    --->    struct4(int, int, bool, bool, bool, bool).

:- type struct5
    --->    struct5(int, int, bool, bool, bool, bool, bool).

:- type struct6
    --->    struct6(int, int, bool, bool, bool, bool, bool, bool).

:- type struct7
    --->    struct7(int, int, bool, bool, bool, bool, bool, bool, bool).

%---------------------------------------------------------------------------%

main(!IO) :-
    % Test the static construction of function symbols represented by
    % direct arg tags.
    StaticList = [
        direct_arg0(struct0(0, 40)),
        direct_arg1(struct1(1, 39, no)),
        direct_arg2(struct2(2, 38, yes, no)),
        direct_arg3(struct3(3, 37, no, yes, no)),
        direct_arg4(struct4(4, 36, yes, no, yes, no)),
        direct_arg5(struct5(5, 35, no, yes, no, yes, no)),
        direct_arg6(struct6(6, 34, yes, no, yes, no, yes, no)),
        direct_arg7(struct7(7, 33, no, yes, no, yes, no, yes, no)),
        non_direct_arg(8, 32)
    ],
    % Test the dynamic construction of function symbols represented by
    % direct arg tags.
    DynamicList = [
        direct_arg0(id(struct0(0, 40))),
        direct_arg1(id(struct1(1, 39, no))),
        direct_arg2(id(struct2(2, 38, yes, no))),
        direct_arg3(id(struct3(3, 37, no, yes, no))),
        direct_arg4(id(struct4(4, 36, yes, no, yes, no))),
        direct_arg5(id(struct5(5, 35, no, yes, no, yes, no))),
        direct_arg6(id(struct6(6, 34, yes, no, yes, no, yes, no))),
        direct_arg7(id(struct7(7, 33, no, yes, no, yes, no, yes, no))),
        non_direct_arg(id(8), 32)
    ],
    list.foldl(test_direct_arg, StaticList, !IO),
    list.foldl(test_direct_arg, DynamicList, !IO).

:- func id(T) = T.
:- pragma no_inline(id/1).

id(X) = X.

:- pred test_direct_arg(test_3_tag_bits::in, io::di, io::uo) is det.

test_direct_arg(X, !IO) :-
    % Test the deconstruction of function symbols represented by
    % direct arg tags.
    %
    % We write out spaces to align the outputs of StructN and X vertically.
    io.nl(!IO),
    (
        X = direct_arg0(Struct0),
        io.write_string("            ", !IO),
        io.write_line(Struct0, !IO)
    ;
        X = direct_arg1(Struct1),
        io.write_string("            ", !IO),
        io.write_line(Struct1, !IO)
    ;
        X = direct_arg2(Struct2),
        io.write_string("            ", !IO),
        io.write_line(Struct2, !IO)
    ;
        X = direct_arg3(Struct3),
        io.write_string("            ", !IO),
        io.write_line(Struct3, !IO)
    ;
        X = direct_arg4(Struct4),
        io.write_string("            ", !IO),
        io.write_line(Struct4, !IO)
    ;
        X = direct_arg5(Struct5),
        io.write_string("            ", !IO),
        io.write_line(Struct5, !IO)
    ;
        X = direct_arg6(Struct6),
        io.write_string("            ", !IO),
        io.write_line(Struct6, !IO)
    ;
        X = direct_arg7(Struct7),
        io.write_string("            ", !IO),
        io.write_line(Struct7, !IO)
    ;
        X = non_direct_arg(A, B),
        io.format("non_direct_arg %d %d\n", [i(A), i(B)], !IO)
    ),
    io.write_line(X, !IO).
