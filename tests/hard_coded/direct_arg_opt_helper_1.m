%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module direct_arg_opt_helper_1.
:- interface.

:- import_module io.

:- include_module direct_arg_opt_helper_2.

:- type test_object.

:- pred indirect_new_object(test_object::out) is det.

:- pred indirect_check_object(test_object::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module direct_arg_opt_helper_1.direct_arg_opt_helper_2.

:- type test_object
    --->    test_object(int).

:- pragma foreign_type("C", test_object, "TestObject *",
    [word_aligned_pointer]).

indirect_new_object(Ob) :-
    new_object(Ob).

% The test case requires this predicate to be opt-exported.
:- pragma inline(pred(indirect_check_object/3)).

indirect_check_object(Ob, !IO) :-
    maybe_check_object(yes_object(Ob), !IO).
