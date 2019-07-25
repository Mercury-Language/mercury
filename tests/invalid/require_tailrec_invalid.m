%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests for invalid uses of the require_tail_recursion pragma.
% It does not test the use of this pragma on a non tail recursive
% predicate or function, that will be tested separately.
%
%---------------------------------------------------------------------------%

:- module require_tailrec_invalid.

:- interface.

:- import_module list.
:- import_module int.

% The pragma shouldn't be allowed in the interface
:- pragma require_tail_recursion(length/2, [warn]).

:- pred length(list(T)::in, int::out) is det.

:- implementation.

% The pragma used with an non-existent predicate or function.
:- pragma require_tail_recursion(non_existent_pred/3, [warn]).
:- pragma require_tail_recursion(non_existent_proc(in, out), [error]).
:- pragma require_tail_recursion(non_existent_func_proc(in) = out, [error]).

% or with a non existent mode of a predicate that does exist.
:- pragma require_tail_recursion(length(out, in), [self_recursion_only]).

% conflicting options.
:- pragma require_tail_recursion(length1/2, [warn, error]).
:- pred length1(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length2/2, [warn, none]).
:- pred length2(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length3/2, [error, none]).
:- pred length3(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length4/2, [self_or_mutual_recursion,
    self_recursion_only]).
:- pred length4(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length5/2, [self_or_mutual_recursion,
    none]).
:- pred length5(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length6/2, [self_recursion_only, none]).
:- pred length6(list(T)::in, int::out) is det.

% malformed arguments / options.
:- pragma require_tail_recursion(length7/2, [blahblahblah]).
:- pred length7(list(T)::in, int::out) is det.

% This gets read as a 0-arity predicate, that is then non-existent.
:- pragma require_tail_recursion(blahblahblah).

:- pragma require_tail_recursion(length8/2, Woop).
:- pred length8(list(T)::in, int::out) is det.

:- pragma require_tail_recursion(length9/2, 23).
:- pred length9(list(T)::in, int::out) is det.

% Multiple problems, this tests that each problem is reported, not just the
% first.  However the non-existent pred/proc is not checked until
% add_pragma.m, but this predicate is rejected earlier (prog_io_pragma.m)
% due to the bad attribute list.
:- pragma require_tail_recursion(length_nonexistent/3, [none, warn,
    self_recursion_only, grasshopper]).

% Multiple pragmas for the same predicate.
:- pragma require_tail_recursion(length10/2, [warn,
    self_or_mutual_recursion]).
:- pragma require_tail_recursion(length10/2, [error,
    self_recursion_only]).
% Even the same options applied multiple times should cause an error.
:- pragma require_tail_recursion(length10/2, [error,
    self_recursion_only]).

:- pred length10(list(T)::in, int::out) is det.

% Multiple definitions for the same mode of a predicate.

:- pred append(list(T), list(T), list(T)).
:- mode append(in, in, out) is det.
:- mode append(out, out, in) is multi.
:- mode append(in, in, in) is semidet.

:- pragma require_tail_recursion(append(in, in, out), [warn]).
:- pragma require_tail_recursion(append(in, in, in), [warn]).
:- pragma require_tail_recursion(append/3, [warn]). % error should be here.

