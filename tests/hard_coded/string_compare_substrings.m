%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_compare_substrings.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    S0 = "😀",
    S1 = string.between(S0, 0, count_code_units(S0) - 1),
    X = "small" ++ S0 ++ "dog" ++ S1 ++ "cat",
    Y = "big" ++ S0 ++ "cat" ++ S1 ++ "dog",
    solutions(generate_params(X, Y), Params),
    foldl(test_compare_substrings(X, Y), Params, !IO),
    io.write_string("done.\n", !IO).

:- pred generate_params(string::in, string::in, {int, int, int}::out) is nondet.

generate_params(X, Y, {IX, IY, Len}) :-
    list.member(IX, -1 .. length(X) + 1),
    list.member(IY, -1 .. length(Y) + 1),
    list.member(Len, -1 .. max(length(X), length(Y)) + 1).

:- pred test_compare_substrings(string::in, string::in, {int, int, int}::in,
    io::di, io::uo) is det.

test_compare_substrings(X, Y, {IX, IY, Len}, !IO) :-
    ( if compare_substrings(Rel, X, IX, Y, IY, Len) then
        ( if ref_compare_substrings(RefRel, X, IX, Y, IY, Len) then
            ( if Rel = RefRel then
                true
            else
                io.write_string(
                    "error: result does not match reference implementation\n",
                    !IO)
            )
        else
            io.write_string(
                "error: succeeded but reference implementation failed\n", !IO)
        )
    else
        ( if ref_compare_substrings(_RelRef, X, IX, Y, IY, Len) then
            io.write_string(
                "error: failed but reference implementation succeeded\n", !IO)
        else
            true
        )
    ).

:- pred ref_compare_substrings(comparison_result::uo, string::in, int::in,
    string::in, int::in, int::in) is semidet.

ref_compare_substrings(Res, X, StartX, Y, StartY, Length) :-
    strict_between(X, StartX, StartX + Length, SubX),
    strict_between(Y, StartY, StartY + Length, SubY),
    compare(Res, SubX, SubY),
    /*
    trace [io(!IO)] (
        io.print_line({Res, SubX, SubY}, !IO)
    ),
    */
    true.

:- pred strict_between(string::in, int::in, int::in, string::out) is semidet.

strict_between(Str, Start, End, SubStr) :-
    % string.between adjusts offsets (unfortunately).
    Start >= 0,
    End >= Start,
    Start =< length(Str),
    End =< length(Str),
    string.between(Str, Start, End, SubStr).
