% In grades using the MLDS this threw the exception
%
% Software Error: ml_code_util.m: Unexpected: ml_gen_arg_decls: length mismatch

:- module intermod_unused_args.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module intermod_unused_args2.

main(!IO) :-
    callee(1,2,3,X),
    io.print(X,!IO),
    io.nl(!IO).
