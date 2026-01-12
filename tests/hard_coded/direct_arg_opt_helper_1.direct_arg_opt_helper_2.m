%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_opt_helper_1.direct_arg_opt_helper_2.
:- interface.

    % This test case tests an issue that arises from maybe_object being defined
    % in a different module from 'test_object'. The location of the two
    % predicates below is irrelevant.
    %
:- type maybe_object
    --->    no_object
    ;       yes_object(test_object).

:- pred new_object(test_object::out) is det.

:- pred maybe_check_object(maybe_object::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- pragma foreign_decl("C", "
typedef struct {
    MR_Word magic;
} TestObject;
").

new_object(test_object(-1)).

:- pragma foreign_proc("C",
    new_object(Ob::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_export_body],
"
    Ob = MR_GC_NEW(TestObject);
    Ob->magic = 0xdeadbeef;
").

:- pred get_magic(test_object::in, int::out) is det.

get_magic(test_object(Magic), Magic).

:- pragma foreign_proc(c,
    get_magic(Ob::in, Magic::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_export_body],
"
    Magic = Ob->magic;
    assert(Magic == 0xdeadbeef);
").

maybe_check_object(MaybeOb, !IO) :-
    (
        MaybeOb = yes_object(Ob),
        get_magic(Ob, Magic),
        ( if Magic = 0xdeadbeef then
            io.write_string("ok\n", !IO)
        else if Magic = -1 then
            io.write_string("ok\n", !IO)
        else
            io.format("BUG: Magic = %#x\n", [i(Magic)], !IO)
        )
    ;
        MaybeOb = no_object
    ).
