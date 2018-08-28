%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
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

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module type_desc.

main(!IO) :-
    test([1, 2], !IO),
    test(["one", "two", "three"], !IO).

:- pred test(T::in, io::di, io::uo) is det.

test(Val, !IO) :-
    TypeDesc = get_type_desc(Val),
    io.write_string("type_desc: ", !IO),
    io.write(TypeDesc, !IO),
    io.nl(!IO),
    TypeCtorDesc = get_type_ctor_desc(TypeDesc),
    io.write_string("type_ctor_desc: ", !IO),
    io.write(TypeCtorDesc, !IO),
    io.nl(!IO).

:- func get_type_desc(T) = type_desc.

get_type_desc(Val) = type_of(Val).

:- func get_type_ctor_desc(type_desc) = type_ctor_desc.

get_type_ctor_desc(TypeDesc) = type_ctor(TypeDesc).
