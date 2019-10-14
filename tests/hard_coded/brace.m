%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module brace.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module dir.
:- import_module list.

main(!IO) :-
    list.foldl(expand_print, test_inputs, !IO).

:- pred expand_print(string::in, io::di, io::uo) is det.

expand_print(Arg, !IO) :-
    io.write_string(Arg, !IO),
    io.nl(!IO),
    write_expansions(expand_braces(Arg), !IO).

:- pred write_expansions(list(string)::in, io::di, io::uo) is det.

write_expansions([], !IO).
write_expansions([Str | Strs], !IO) :-
    io.write_string("  ", !IO),
    io.write_string(Str, !IO),
    io.nl(!IO),
    write_expansions(Strs, !IO).

:- func test_inputs = list(string).

test_inputs = [
    "abc",
    "{a,b}",
    "a{b,c}d",
    "aa{bb,cc}dd",
    "aa{bb,cc,{d,e}}ff",
    "aa{{b1,b2,b3},cc,{d,e}}ff",
    "aa{{b1,{b21,b22,b23},b3},cc,{d,e}}ff",
    "a{b,c}d{e,f,g}h",
    "aa{bb,cc,{{d1,d2},e}}ff{g,hh}"
].
