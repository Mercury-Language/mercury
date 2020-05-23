%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the c specifier of string.format.
%
%---------------------------------------------------------------------------%

:- module string_format_c.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main(!IO) :-
    Chars = [c('a'), c(' ')],
    list.foldl(output_list(Chars), format_strings("c"), !IO).

%---------------------------------------------------------------------------%
