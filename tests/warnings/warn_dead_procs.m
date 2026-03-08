%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case was added to test warnings about unused procedures.
% It now also serves to test warnings about unused type definitions
% (specifically, definitions of discriminated union and equivalence types).
%---------------------------------------------------------------------------%

:- module warn_dead_procs.
:- interface.

:- type expr
    --->    div(expr, expr).

:- implementation.

:- import_module list.
:- import_module maybe.

:- type expr2
    --->    div2(expr2, expr2).

:- pred foo is det.
foo.

:- pred bar(int).
:- mode bar(in) is semidet.
:- mode bar(out) is det.
bar(42).

baz.

:- promise ((X `with_type` int) = (Y `with_type` int) <=> Y = X).

% The reference to e2 in the definition of e1 marks e2 as "used",
% but e1 is correctly reported to be unused. Once its definition
% is deleted, the next compiler invocation will report e2 as also unused.
:- type e1 == list(e2).
:- type e2 == list(int).

% Given a set of mutually recursive equivalence types, we cannot ever
% generate warnings about them being unused, because the compiler
% will generate *errors* about them being mutually recursive,
% and exit before it gets to the pass that would generate such warnings.
%
% The only kind of mutual recursive set of type definitions
% in which it makes sense to expect a warning about an unused equivalence type
% is one in which at least one member of a set is a discriminated union type.
%
% This is such a set of definitions. See the comment on the code
% computing the set of unused equivalence types in unused_types.m
% on why we do not (yet) report e4 as unused.
:- type e3
    --->    e3(e4).
:- type e4 == maybe(e3).
