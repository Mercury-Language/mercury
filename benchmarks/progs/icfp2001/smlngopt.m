%------------------------------------------------------------------------------%
% smlngopt.m
% Ralph Becket <rbeck@microsoft.com>
% Sun Jul 29 09:16:44 BST 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%------------------------------------------------------------------------------%

:- module smlngopt.

:- interface.

:- import_module io.



:- pred main(io__state::di, io__state::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module string, list, array, int, exception.
:- import_module input, optimize.

%------------------------------------------------------------------------------%

main -->

    io__command_line_arguments(ArgV),

    ( if { ArgV = [BaseName] } then

        { file_names(BaseName, 0, FirstSMLNG, FirstDone) },
        read_smlng(FirstSMLNG, FirstDone, Str, Attrss, Extents),
        write_optimized_files(BaseName, Str, Attrss, Extents, 6)

      else

        { throw("usage: smlngopt <BaseName>") }
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
