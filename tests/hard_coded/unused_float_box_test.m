% This is a regression test (extracted from some code in std_util.m).
% The MLDS back-end in Mercury 0.10.1 generated incorrect code
% for this test case.  In particular when the float argument is
% passed to private_builtin__var(T::unused), it generated code
% which passes a pointer and then tries to unbox the float value returned,
% even though no value was actually returned, so it ends up dereferencing
% an uninitialized pointer.

:- module unused_float_box_test.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- import_module std_util.

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

:- pred my_get_functor_info(my_univ::in, my_functor_tag_info::out) is semidet.

:- implementation.
:- import_module list, int.

main -->
	wipe_stack(200),
	( { my_get_functor_info('new my_univ_cons'(42.0), R) } ->
		print(R), nl
	;
		print("failed"), nl
	).

:- pred wipe_stack(int, io__state, io__state).
wipe_stack(N) -->
	( if { N =< 0 } then []
	else wipe_stack(N - 1), wipe_stack(N // 10 - 1)
	).

:- pragma no_inline(my_get_functor_info/2).
my_get_functor_info(Univ, FunctorInfo) :-
    ( my_univ_to_type(Univ, Int) ->
        FunctorInfo = my_functor_integer(Int)
    ; my_univ_to_type(Univ, Float) ->
        FunctorInfo = my_functor_float(Float)
    ; my_univ_to_type(Univ, String) ->
        FunctorInfo = my_functor_string(String)
    ; get_enum_functor_info(Univ, Enum) ->
        FunctorInfo = my_functor_enum(Enum)
    ; get_du_functor_info(Univ, Where, Ptag, Sectag, Args) ->
        ( Where = 0 ->
            FunctorInfo = my_functor_unshared(Ptag, Args)
        ; Where > 0 ->
            FunctorInfo = my_functor_remote(Ptag, Sectag, Args)
        ;
            FunctorInfo = my_functor_local(Ptag, Sectag)
        )
    ; get_notag_functor_info(Univ, ExpUniv) ->
        FunctorInfo = my_functor_notag(ExpUniv)
    ; get_equiv_functor_info(Univ, ExpUniv) ->
        FunctorInfo = my_functor_equiv(ExpUniv)
    ;
        fail
    ).

:- pred get_notag_functor_info(Univ::in, ExpUniv::out) is semidet.

:- pragma foreign_proc("C", 
	get_notag_functor_info(_Univ::in, _ExpUniv::out),
	[will_not_call_mercury, promise_pure], "
{
	abort();
}").

    % from the type stored in the univ.)
:- pred get_equiv_functor_info(Univ::in, ExpUniv::out) is semidet.

:- pragma foreign_proc("C",
	get_equiv_functor_info(_Univ::in, _ExpUniv::out),
    [will_not_call_mercury, promise_pure], "
{
	abort();
}").

:- pred get_enum_functor_info(Univ::in, Int::out) is semidet.

:- pragma foreign_proc("C",
	get_enum_functor_info(_Univ::in, _Enum::out),
	[will_not_call_mercury, promise_pure], "
{
	abort();
}").

:- pred get_du_functor_info(my_univ::in, int::out, int::out, int::out,
    list(univ)::out) is semidet.

:- pragma foreign_proc("C", get_du_functor_info(_Univ::in, _Where::out,
    _Ptag::out, _Sectag::out, _Args::out),
    [will_not_call_mercury, promise_pure], "
{
	abort();
}").

%------------------------------------------------------------------------------%

:- type my_univ --->
	some [T] my_univ_cons(T).

my_univ_to_type(Univ, X) :- my_type_to_univ(X, Univ).

:- pred my_type_to_univ(T, my_univ).
:- pragma promise_pure(my_type_to_univ/2).
my_type_to_univ(T, Univ) :-
	(
		impure private_builtin__var(T),
		Univ = my_univ_cons(T0),
		private_builtin__typed_unify(T0, T)
	;
		impure private_builtin__var(Univ),
		Univ0 = 'new my_univ_cons'(T),
		unsafe_promise_unique(Univ0, Univ)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
