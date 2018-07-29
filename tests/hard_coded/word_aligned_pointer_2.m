%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module word_aligned_pointer_2.
:- interface.

:- import_module io.

    % abstract exported foreign type with `word_aligned_pointer' assertion
:- type foo.

:- type bar
    --->    yes(foo)    % direct argument functor
    ;       no.

:- func make_foo = foo.

:- func get_foo(foo) = int.

:- func make_bar = bar.

:- pred write_bar(bar::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- type foo
    --->    foo(int).

:- pragma foreign_type("C", foo, "MR_Word", [word_aligned_pointer]).

%---------------------------------------------------------------------------%

:- pragma no_inline(make_foo/0).

:- pragma foreign_proc("C",
    make_foo = (X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = MR_mkbody(0xcafe);
").

make_foo = foo(0xcafe).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_foo(X::in) = (Int::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (X != MR_strip_tag(X)) {
        fprintf(stderr, ""tag bits not clear in value""
            ""0x%"" MR_INTEGER_LENGTH_MODIFIER ""x\\n"", (MR_Unsigned) X);
        abort();
    }
    Int = MR_unmkbody(X);
").

get_foo(foo(I)) = I.

%---------------------------------------------------------------------------%

:- pragma no_inline(make_bar/0).

make_bar = yes(make_foo).

%---------------------------------------------------------------------------%

:- pragma no_inline(write_bar/3).

write_bar(Bar, !IO) :-
    (
        Bar = yes(Foo),
        io.format("yes(0x%x)\n", [i(get_foo(Foo))], !IO)
    ;
        Bar = no,
        io.write_string("no", !IO)
    ).

%---------------------------------------------------------------------------%
