%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Regression test for bug #184.
% This program caused the compiler to abort with:
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%    Key Type: parse_tree.prog_data.prog_constraint
%    Key Value: constraint(qualified(unqualified("bug184"), "myclass"),
%        [type_variable(var(1), kind_star)])
%    Value Type: set_ordlist.set_ordlist(hlds.hlds_data.constraint_id)
%
%---------------------------------------------------------------------------%

:- module bug184.
:- interface.

:- func test = int.

:- implementation.

:- import_module list.

test = f([]).

:- typeclass myclass(T) where [
    func f(T) = int
].

:- instance myclass(list(T)) <= myclass(T) where [
    f(_) = 4
].

