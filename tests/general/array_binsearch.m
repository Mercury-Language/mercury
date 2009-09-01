%-----------------------------------------------------------------------------%
% array_binsearch.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Sep  1 11:08:30 EST 2009
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Test the array.[approx_]binary_search predicates.
%
%-----------------------------------------------------------------------------%

:- module array_binsearch.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array, int, list, solutions, string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    solutions.aggregate(run_test, io.write_string, !IO).

%-----------------------------------------------------------------------------%

:- pred run_test(string::out) is nondet.

run_test(Result) :-
    A = array([1, 2, 4, 5, 5, 7]),
    list.member(X, 0..8),
    Result0 = "searching " ++ string.string(A) ++ " for " ++ string.string(X),
    ( if array.binary_search(A, X, I) then
        Result1 = " - exact     " ++ string.string(I)
      else
        Result1 = " - exact fails"
    ),
    ( if array.approx_binary_search(A, X, J) then
        Result2 = " - approx     " ++ string.string(J)
      else
        Result2 = " - approx fails"
    ),
    Result = Result0 ++ Result1 ++ Result2 ++ "\n".

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
