%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% "Hello World" in Mercury, using nested modules.

:- module nested_impl_in_int.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

    :- module nested_impl_in_int.child.
    :- interface.
    :- import_module io.

    :- type foo
        --->    bar
        ;       baz(int).

    :- pred hello(io__state::di, io__state::uo) is det.

    :- end_module nested_impl_in_int.child.

    :- module nested_impl_in_int.child.
    :- implementation.

        hello -->
            io.write_string("nested_impl_in_int.child.hello\n").

    :- end_module nested_impl_in_int:child.

%---------------------------------------------------------------------------%

    :- module nested_impl_in_int.child2.
    :- interface.
        :- import_module io.

        :- type foo
            --->    bar
            ;       baz(int).

        :- pred hello(io::di, io::uo) is det.

    :- implementation.

        hello -->
            io.write_string("nested_impl_in_int.child2.hello\n").

    :- end_module nested_impl_in_int.child2.

:- implementation.

%---------------------------------------------------------------------------%

% now we're back in the parent module.

:- import_module nested_impl_in_int.child.
:- use_module nested_impl_in_int.child2.
:- import_module require.
:- import_module std_util.

:- type t1 == nested_impl_in_int.child.foo.
:- type t2 == child.foo.
:- type t3 == foo.
:- type t4 == nested_impl_in_int.child2.foo.
% :- type t5 == child2.foo. % XXX mixing of use_module and import_module
                            % is not yet supported.
:- type t5 == nested_impl_in_int.child2.foo.

main -->
    nested_impl_in_int.child.hello,
    child.hello,
    hello,
    nested_impl_in_int.child2.hello,
    % child2.hello,     % XXX mixing of use_module and import_module
                        % is not yet supported.

    print("t1 = "), print(type_of(has_type_t1)), nl,
    print("t2 = "), print(type_of(has_type_t2)), nl,
    print("t3 = "), print(type_of(has_type_t3)), nl,
    print("t4 = "), print(type_of(has_type_t4)), nl,
    print("t5 = "), print(type_of(has_type_t5)), nl,

    print("has_type_t1 = "), print(has_type_t1), nl,
    print("has_type_t2 = "), print(has_type_t2), nl,
    print("has_type_t3 = "), print(has_type_t3), nl,
    print("has_type_t4 = "), print(has_type_t4), nl,
    print("has_type_t5 = "), print(has_type_t5), nl,

    { true }.

:- func has_type_t1 = t1.
:- func has_type_t2 = t2.
:- func has_type_t3 = t3.
:- func has_type_t4 = t4.
:- func has_type_t5 = t5.

has_type_t1 = nested_impl_in_int.child.bar.
has_type_t2 = child.bar.
has_type_t3 = bar.
has_type_t4 = nested_impl_in_int.child2.bar.
% has_type_t5 = child2:bar.  % XXX mixing of use_module and import_module
                            % is not yet supported.
has_type_t5 = nested_impl_in_int.child2.bar.

:- end_module nested_impl_in_int.
