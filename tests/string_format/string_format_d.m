%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the d, i specifiers of string__format.
%
%---------------------------------------------------------------------------%

:- module string_format_d.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module string_format_lib.

main -->
    { Ints = [i(0), i(-1), i(1), i(10), i(-10),
        i(100), i(-100), i(min_int), i(max_int)] },
    list__foldl(output_list(Ints), format_strings("d")),
    list__foldl(output_list(Ints), format_strings("i")).

%---------------------------------------------------------------------------%
