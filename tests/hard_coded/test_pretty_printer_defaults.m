%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test_pretty_printer_defaults.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Aug  7 15:29:20 EST 2007
%
% Test the default pretty_printer formatters.
%
%---------------------------------------------------------------------------%

:- module test_pretty_printer_defaults.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pretty_printer.
:- import_module version_array.

%---------------------------------------------------------------------------%

main(!IO) :-
    L = 1..100,
    A = array(L),
    VA = version_array(L),
    M = map.from_corresponding_lists(L, L) : map(int, int),
    pretty_printer.get_default_formatter_map(FMap, !IO),
    io.print_line(FMap, !IO),
    Doc = docs([
        str("list:    "), format(L), nl,
        str("array:   "), format(A), nl,
        str("map:     "), format(M), nl,
        str("tuple:   "), format({1, '2', 3.0, "four"}), nl,
        str("strings: "), format("this is a string"), nl,
        str("ints:    "), format(42), str(" "), format(-123), nl,
        str("floats:  "), format(3.141), str(" "), format(-10.0), nl,
        str("chars:   "), format([a, '*', '\n']), nl,
        str("version_array:"), format(VA), nl
    ]),
    pretty_printer.write_doc(Doc, !IO).
