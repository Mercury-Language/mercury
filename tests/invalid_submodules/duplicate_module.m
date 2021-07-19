%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test of modules declared as both nested and separate modules.
% This is referenced from duplicate_module_test.m.

:- module duplicate_module.
:- interface.
:- import_module io.

:- pred do_main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

:- include_module duplicate_module.child.

:- module duplicate_module.child.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("duplicate_module.child.hello\n", !IO).

:- end_module duplicate_module.child.

%---------------------------------------------------------------------------%

:- include_module child2.

:- module duplicate_module.child2.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("duplicate_module.child2.hello\n", !IO).

:- end_module duplicate_module.child2.

%---------------------------------------------------------------------------%

:- include_module duplicate_module.child3.

:- module child3.
:- interface.
:- import_module io.

:- type foo
    --->    bar
    ;       baz(int).

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("duplicate_module.child3.hello\n", !IO).

:- end_module child3.

%---------------------------------------------------------------------------%

:- module child4.
:- interface.
:- import_module io.

:- pred hello(io::di, io::uo) is det.

:- implementation.

hello(!IO) :-
    io.write_string("duplicate_module.child4.hello\n", !IO).

:- end_module duplicate_module.child4.

:- include_module duplicate_module.child4.

%---------------------------------------------------------------------------%

:- include_module duplicate_module.child5.
:- include_module duplicate_module.child5.

%---------------------------------------------------------------------------%

% Now we are back in the parent module.

:- use_module duplicate_module.child.
:- use_module duplicate_module.child2.
:- use_module duplicate_module.child3.
:- import_module require.
:- import_module std_util.

do_main(!IO) :-
    duplicate_module.child.hello,
    duplicate_module.child2.hello,
    duplicate_module.child3.hello.

:- end_module duplicate_module.
