% Test the transformation of comparisons of enumerations into
% integer comparisons.
%
% With the compiler of 31/10/2000, this test case failed
% to link due to references to private_builtin__unsafe_type_cast
% in the generated code (calls to private_builtin__unsafe_type_cast
% should be generated inline).
:- module compare_spec.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool.

main -->
	( { compare_bool } ->
		io__write_string("failed\n")
	;
		io__write_string("succeeded\n")
	).

:- pred compare_bool is semidet.

compare_bool :-
        compare(Result, yes, no),
        Result = (=).

