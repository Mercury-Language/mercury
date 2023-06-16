%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% In MLDS grades this threw the exception
%
% Software Error: ml_code_util.m: Unexpected: ml_gen_arg_decls: length mismatch

:- module intermod_unused_args.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module intermod_unused_args_helper_1.

main(!IO) :-
    callee(1, 2, 3, X),
    io.print_line(X, !IO).
