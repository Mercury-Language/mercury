%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the transformation of comparisons of enumerations into
% integer comparisons.
%
% With the compiler of 31/10/2000, this test case failed
% to link due to references to private_builtin__unsafe_type_cast
% in the generated code (calls to private_builtin__unsafe_type_cast
% should be generated inline).
%

:- module compare_spec.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

main(!IO) :-
    ( if compare_bool then
        io.write_string("failed\n", !IO)
    else
        io.write_string("succeeded\n", !IO)
    ).

:- pred compare_bool is semidet.

compare_bool :-
    compare(Result, yes, no),
    Result = (=).
