%-----------------------------------------------------------------------------%
:- module mercury_lib.
:- interface.

% a Mercury predicate with multiple modes
:- pred foo(int).
:- mode foo(in) is semidet.
:- mode foo(out) is multi.

%-----------------------------------------------------------------------------%
:- implementation.
:- import_module std_util.	% for solutions/2

% well, this is just a silly example...
foo(42).
foo(53).
foo(197).

%-----------------------------------------------------------------------------%

% The following code provides provides access to the Mercury predicate foo
% from C code.

	% Currently only det modes can be exported directly.
	% To export a semidet mode, we need to create a det predicate
	% which return an extra success/failure indicator.
	% We give it the name foo_test() in C.
:- pred foo_test(int::in, int::out) is det.
:- pragma export(foo_test(in, out), "foo_test").
foo_test(X, R) :- (foo(X) -> R = 1 ; R = 0).

	% The nondet mode of `foo' cannot be exported directly with
	% the current Mercury/C interface.  To get all solutions,
	% must define a predicate which returns all the solutions of foo,
	% and export it to C.  We give it the name foo_list() in C.
:- pred all_foos(list(int)::out) is det.
:- pragma export(all_foos(out), "foo_list").
all_foos(L) :- solutions((pred(X::out) is multi :- foo(X)), L).

	% If we just want one solution, and don't care which one, then
	% we can export a `cc_multi' (committed-choice nondeterminism)
	% version of `foo'. We give it the name one_foo().
:- pred cc_foo(int::out) is cc_multi.
:- pragma export(cc_foo(out), "one_foo").
cc_foo(X) :- foo(X).

%-----------------------------------------------------------------------------%
