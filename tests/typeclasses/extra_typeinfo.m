%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests four things:
%
% 1) Inclusion and ordering of extra typeinfos by higher_order.m
% with --typeinfo-liveness.
%
% 2) Updating of the typeclass_info_varmap for specialised version by
% higher_order.m.
%
% 3) Handling of semidet class_method_calls with argument typeclass_infos.
% The runtime of 7/9/1998 contained a bug where the arguments were not
% set up properly for this case.
% Symptom: runtime segfault.
%
% 4) Bugs in the introduction of type_info_from_typeclass_info - the
% typeinfo_varmap was being erroneously updated.
% Symptom: code generator abort.
%
% Unfortunately you really need to look at the HLDS dump to check 1) and 2).
% Compile this with options:
%   --typeinfo-liveness --optimize-higher-order --no-type-specialization
% The --no-type-specialization is required to ensure that call_foldl
% remains polymorphic.
%
%---------------------------------------------------------------------------%

:- module extra_typeinfo.
:- interface.

:- import_module io.
:- import_module list.

:- typeclass foo(T) where [
    pred foo_pred(T::in) is semidet
].

:- pred main(io::di, io::uo) is det.

:- pred call_foldl(list(list(T)),
    list(list(U)), list(list(U))) <= foo(T).
:- mode call_foldl(in, in, out) is semidet.

:- implementation.

:- import_module std_util.

main(!IO) :-
    L1 = [[1, 2, 3], [4, 5, 6]],
    L2 = [[7, 8, 9], [10, 11, 12]],
    io.write(L1, !IO),
    io.write(L2, !IO),
    ( if call_foldl(L1, L2, L) then
        io.write_line(L, !IO)
    else
        io.write_string("failed\n", !IO)
    ).

call_foldl(In, Out0, Out) :-
    % This calls foldl so that the original type variables in foldl
    % get mapped to non-variable types, so higher_order.m needs to add
    % extra argument type_infos for the type variables in the types
    % of the specialised arguments.
    Pred = (pred(Int::in) is semidet :- Int = 2),
    list_foldl(Pred, [2], In, _, Out0, Out).

:- pred list_foldl(pred(V), list(V), T, T, U, U) <= foo(T).
:- mode list_foldl((pred(in) is semidet), in, in, out, in, out) is semidet.

list_foldl(_P, [], T, T, U, U).
list_foldl(P, [V | Vs], T0, T, U0, U) :-
    call(P, V),
    foo_pred(T),
    list_foldl(P, Vs, T0, T, U0, U).

:- instance foo(int) where [
    pred(foo_pred/1) is nothing
].

:- instance foo(list(T)) <= foo(T) where [
    pred(foo_pred/1) is test_first_foo
].

:- pred test_first_foo(list(T)::in) is semidet <= foo(T).

test_first_foo([A | _]) :-
    foo_pred(A).

:- pred nothing(int::in) is semidet.

nothing(_) :-
    semidet_succeed.

%---------------------------------------------------------------------------%
