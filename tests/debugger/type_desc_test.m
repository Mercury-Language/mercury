% This is a test of the debugger's handling of the builtin types type_desc and
% type_ctor_desc.
%
% Note that we call type_of through get_type_desc instead of directly because
% we want to get control when get_type_desc returns to print its arguments.
% we can't do that without get_type_desc if deconstruct.m was compiled without
% debugging enabled. We call type_ctor through get_type_ctor_desc for the
% same reason.

:- module type_desc_test.

:- interface.
:- import_module io.

:- pred main(io__state::di, state::uo) is det.

:- implementation.

:- import_module type_desc, list.

main -->
	test([1, 2]),
	test(["one", "two", "three"]).

:- pred test(T::in, io__state::di, io__state::uo) is det.

test(Val) -->
	{ TypeDesc = get_type_desc(Val) },
	io__write_string("type_desc: "),
	io__write(TypeDesc),
	io__nl,
	{ TypeCtorDesc = get_type_ctor_desc(TypeDesc) },
	io__write_string("type_ctor_desc: "),
	io__write(TypeCtorDesc),
	io__nl.

:- func get_type_desc(T) = type_desc.

get_type_desc(Val) = type_of(Val).

:- func get_type_ctor_desc(type_desc) = type_ctor_desc.

get_type_ctor_desc(TypeDesc) = type_ctor(TypeDesc).
