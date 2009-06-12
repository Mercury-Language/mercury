%-----------------------------------------------------------------------------%
% Regression test.
%
% Software Error: hlds_rtti.m: Unexpected: inconsistent type_infos:
% Type: type_variable(var(1), kind_star)
% ExistingType: defined_type(qualified(unqualified("profdeep_rtti_varmap"),
% "checked_out_task"), [type_variable(var(1), kind_star)], kind_star)
%
% The problem had to do with an incorrect rtti varmap entry after a type_info
% was extracted from a typeclass_info.
%-----------------------------------------------------------------------------%

:- module tci_spec_varmap.
:- interface.

:- import_module io.

:- pred something(T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

something(_, !IO) :-
    _ = marking([] : list(checked_out_task(T))).

:- type checked_out_task(T)
	--->	checked_out_task.

:- type marking
    --->    mark.

:- typeclass marking(T) where [
    func marking(T) = marking
].

:- instance marking(list(T)) <= marking(T) where [
    (marking([]) = mark),
    (marking([_ | Cs]) = marking(Cs))
].

:- instance marking(checked_out_task(T)) where [
    marking(checked_out_task) = mark
].

%-----------------------------------------------------------------------------%
