%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test. The compiler incorrectly assigned all primary tags to direct
% argument functors, leaving none for other functors remaining in the type.
%---------------------------------------------------------------------------%

:- module direct_arg_tags.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type test_2_tag_bits
    --->    direct_arg0(struct)
    ;       direct_arg1(struct)
    ;       direct_arg2(struct)
    ;       direct_arg3(struct) % should share last primary tag
    ;       non_direct_arg(int, int).

:- type test_3_tag_bits
    --->    direct_arg0(struct)
    ;       direct_arg1(struct)
    ;       direct_arg2(struct)
    ;       direct_arg3(struct)
    ;       direct_arg4(struct)
    ;       direct_arg5(struct)
    ;       direct_arg6(struct)
    ;       direct_arg7(struct) % should share last primary tag
    ;       non_direct_arg(int, int).

:- type struct
    --->    struct(int, int).

%---------------------------------------------------------------------------%

main(!IO) :-
    S = struct(1, 2),
    L1 = [
        direct_arg0(S) : test_2_tag_bits,
        direct_arg1(S),
        direct_arg2(S),
        direct_arg3(S),
        non_direct_arg(3, 4)
    ],
    L2 = [
        direct_arg0(S) : test_3_tag_bits,
        direct_arg1(S),
        direct_arg2(S),
        direct_arg3(S),
        direct_arg4(S),
        direct_arg5(S),
        direct_arg6(S),
        direct_arg7(S),
        non_direct_arg(3, 4)
    ],
    list.foldl(io.write_line, L1, !IO),
    io.nl(!IO),
    list.foldl(io.write_line, L2, !IO).
