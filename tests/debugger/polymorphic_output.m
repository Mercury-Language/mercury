% This is a regression test.
%
% It tests for a bug that showed up when tracing
% calls to procedures like `std_util.det_arg' (now deconstruct.det_arg)
% or `io.read' that have an output argument whose type is a universally
% quantified type variable that does not occur in the type of any of the
% input arguments.
%
% Version rotd-2000-04-10 failed this test case.

:- module polymorphic_output.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    map.from_assoc_list(["two"-2, "one"-1, "three"-3, "four"-4], M),
    io.write_list(functor_names(M), "\n", io.write_string, !IO),
    io.nl(!IO).

:- func functor_names(T) = list(string).

functor_names(X) = [Name | ( if Arity = 0 then ArgNames else [] )] :-
    functor(X, canonicalize, Name, Arity),
    ArgNames = arg_names(X, Arity - 1).

:- func arg_names(T, int) = list(string).

arg_names(X, I) = Strs :-
    ( if I < 0 then
        Strs = []
      else
        det_arg(X, canonicalize, I, Arg),
	Strs = functor_names(Arg) ++ arg_names(X, I - 1)
    ).
