%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test (extracted from some code in std_util.m).
% The MLDS back-end in Mercury 0.10.1 generated incorrect code for this
% test case. In particular, when the float argument is passed to
% private_builtin.var(T::unused), it generated code which passed a pointer
% and then tried to unbox the float value returned, even though no value
% was actually returned, so it ended up dereferencing an uninitialized pointer.

:- module unused_float_box_test.
:- interface.

:- import_module io.
:- import_module list.
:- import_module univ.

:- pred main(io::di, io::uo) is det.

:- type my_functor_tag_info
        --->    my_functor_integer(int)
        ;       my_functor_float(float)
        ;       my_functor_string(string)
        ;       my_functor_enum(int)
        ;       my_functor_local(int, int)
        ;       my_functor_remote(int, int, list(univ))
        ;       my_functor_unshared(int, list(univ))
        ;       my_functor_notag(univ)
        ;       my_functor_equiv(univ).

:- type my_univ
    --->    some [T] my_univ_cons(T).

:- pred my_get_functor_info(my_univ::in, my_functor_tag_info::out) is semidet.

:- implementation.
:- import_module int.

main(!IO) :-
    wipe_stack(200, !IO),
    ( if my_get_functor_info('new my_univ_cons'(42.0), R) then
        io.print_line(R, !IO)
    else
        io.print_line("failed", !IO)
    ).

:- pred wipe_stack(int, io, io).

wipe_stack(N, !IO) :-
    ( if N =< 0 then
        true
    else
        wipe_stack(N - 1, !IO),
        wipe_stack(N // 10 - 1, !IO)
    ).

:- pragma no_inline(my_get_functor_info/2).
my_get_functor_info(Univ, FunctorInfo) :-
    ( if my_univ_to_type(Univ, Int) then
        FunctorInfo = my_functor_integer(Int)
    else if my_univ_to_type(Univ, Float) then
        FunctorInfo = my_functor_float(Float)
    else if my_univ_to_type(Univ, String) then
        FunctorInfo = my_functor_string(String)
    else if get_enum_functor_info(Univ, Enum) then
        FunctorInfo = my_functor_enum(Enum)
    else if get_du_functor_info(Univ, Where, Ptag, Sectag, Args) then
        ( if Where = 0 then
            FunctorInfo = my_functor_unshared(Ptag, Args)
        else if Where > 0 then
            FunctorInfo = my_functor_remote(Ptag, Sectag, Args)
        else
            FunctorInfo = my_functor_local(Ptag, Sectag)
        )
    else if get_notag_functor_info(Univ, ExpUniv) then
        FunctorInfo = my_functor_notag(ExpUniv)
    else if get_equiv_functor_info(Univ, ExpUniv) then
        FunctorInfo = my_functor_equiv(ExpUniv)
    else
        fail
    ).

:- pred get_notag_functor_info(Univ::in, ExpUniv::out) is semidet.

:- pragma foreign_proc("C",
    get_notag_functor_info(_Univ::in, _ExpUniv::out),
    [will_not_call_mercury, promise_pure], "
{
    abort();
}").
get_notag_functor_info(_, _) :-
    semidet_succeed,
    private_builtin.sorry("local get_notag_functor_info").

    % from the type stored in the univ.)
:- pred get_equiv_functor_info(Univ::in, ExpUniv::out) is semidet.

:- pragma foreign_proc("C",
    get_equiv_functor_info(_Univ::in, _ExpUniv::out),
    [will_not_call_mercury, promise_pure], "
{
    abort();
}").
get_equiv_functor_info(_, _) :-
    semidet_succeed,
    private_builtin.sorry("local get_equiv_functor_info").

:- pred get_enum_functor_info(Univ::in, Int::out) is semidet.

:- pragma foreign_proc("C",
    get_enum_functor_info(_Univ::in, _Enum::out),
    [will_not_call_mercury, promise_pure], "
{
    abort();
}").
get_enum_functor_info(_, _) :-
    semidet_succeed,
    private_builtin.sorry("local get_enum_functor_info").

:- pred get_du_functor_info(my_univ::in, int::out, int::out, int::out,
    list(univ)::out) is semidet.

:- pragma foreign_proc("C", get_du_functor_info(_Univ::in, _Where::out,
    _Ptag::out, _Sectag::out, _Args::out),
    [will_not_call_mercury, promise_pure], "
{
    abort();
}").
get_du_functor_info(_, _, _, _, _) :-
    semidet_succeed,
    private_builtin.sorry("local get_du_functor_info").

%---------------------------------------------------------------------------%

:- pred my_type_to_univ(T, my_univ).
:- pragma promise_pure(my_type_to_univ/2).

my_univ_to_type(Univ, X) :-
    my_type_to_univ(X, Univ).

my_type_to_univ(T, Univ) :-
    (
        impure private_builtin.var(T),
        Univ = my_univ_cons(T0),
        private_builtin.typed_unify(T0, T)
    ;
        impure private_builtin.var(Univ),
        Univ0 = 'new my_univ_cons'(T),
        unsafe_promise_unique(Univ0, Univ)
    ).

%---------------------------------------------------------------------------%
