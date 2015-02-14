%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% string_strip.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Feb 12 17:10:49 EST 2003
%
% Test the string strip type functions.
%
%---------------------------------------------------------------------------%

:- module string_strip.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module char.
:- import_module solutions.

:- pred space(char, char).
:- mode space(in,   out ) is semidet.
:- mode space(out,  out ) is multi.

space(' ',  ' ').
space('\t', 't').
space('\n', 'n').

%---------------------------------------------------------------------------%

main(!IO) :-
    Strings =
        condense(
            map(func(Spaces) = ["fȯö", Spaces ++ "fȯö", "fȯö" ++ Spaces,
                                Spaces ++ "fȯö" ++ Spaces],
                condense(
                    map(func(Length) = solutions(mk_spaces(Length)),
                        1`..`2
                    )
                )
            )
        ),

    Tests = [
        "chomp" -
        chomp,

        "lstrip" -
        lstrip,

        "rstrip" -
        rstrip,

        "strip" -
        strip,

        "lstrip_pred(is_alpha)" -
        lstrip_pred(is_alpha_ext),

        "rstrip_pred(is_alpha)" -
        rstrip_pred(is_alpha_ext),

        "prefix_length(is_whitespace)" -
        ( func(S) = format("%d", [i(prefix_length(char.is_whitespace, S))]) ),

        "suffix_length(is_whitespace)" -
        ( func(S) = format("%d", [i(suffix_length(char.is_whitespace, S))]) )
    ],

    Results =
        condense(
            map(func(Name - Test) =
                map(func(String) =
                    format("%s(\"%s\") = \"%s\"",
                        [s(Name), s(quote(String)), s(quote(Test(String)))]),
                    Strings
                ),
                Tests
            )
        ),
    io.write_list(Results, "\n", io.write_string, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- func quote(string) = string.

quote(S) =
    string.from_char_list(
        foldr(
            func(C, Cs) = ( if space(C, X) then ['\\', X | Cs] else [C | Cs] ),
            string.to_char_list(S),
            []
        )
    ).

%---------------------------------------------------------------------------%

:- pred mk_spaces(int, string).
:- mode mk_spaces(in, out) is multi.

mk_spaces(N, S) :-
    map((pred(_::in, C::out) is multi :- space(C, _)), 1`..`N, Cs),
    S = string.from_char_list(Cs).

%---------------------------------------------------------------------------%

:- pred is_alpha_ext(char::in) is semidet.

is_alpha_ext('ȯ').
is_alpha_ext('ö').
is_alpha_ext(Char) :-
    char.is_alpha(Char).
