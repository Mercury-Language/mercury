% This is a regression test.
%
% It tests for a bug that showed up when tracing
% calls to procedures like `std_util__det_arg'
% or `io__read' that have an output argument
% whose type is a universally quantified type
% variable that does not occur in the type of
% any of the input arguments.
% Version rotd-2000-04-10 failed this test case.

:- module polymorphic_output.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, list, string, map, int.

main -->
    { map__from_assoc_list(["two"-2, "one"-1, "three"-3, "four"-4], M) },
    io__write_list(functor_names(M), "\n", io__write_string),
    io__nl.

:- func functor_names(T) = list(string).

functor_names(X) =
    [Name | ( if Arity = 0 then ArgNames else [] ) ]
:-
    functor(X, Name, Arity),
    ArgNames = arg_names(X, Arity - 1).

:- func arg_names(T, int) = list(string).

arg_names(X, I) =
    ( if I < 0 then
        []
      else
        list__append(functor_names(det_arg(X, I)), arg_names(X, I - 1))
    ).
