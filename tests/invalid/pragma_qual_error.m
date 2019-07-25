%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Test for error messages produced by module qualification errors in
% pragma declarations.
%---------------------------------------------------------------------------%

:- module pragma_qual_error.
:- interface.

:- type foo ---> foo.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%
%
% foreign_export_enum.
%

:- pragma foreign_export_enum("C", bar/0).

%---------------------------------------------------------------------------%
%
% foreign_enum.
%

:- pragma foreign_enum("C", bar/0, [abc - "1"]).

%---------------------------------------------------------------------------%
%
% reserve_tag. Not supported anymore.
%

% :- pragma reserve_tag(baaz/0).

%---------------------------------------------------------------------------%
%
% foreign_proc.
%

:- pragma foreign_proc("C",
     foo(A::not_a_mode, B::out),
     [promise_pure, will_not_call_mercury],
"
    B = A;
").

:- pred foo(A::in, B::out) is det.

%---------------------------------------------------------------------------%
%
% memo.
%

:- pragma memo(memoproc(in, in, not_a_mode)).

:- pred memoproc(int::in, int::in, int::out) is det.

memoproc(A, B, A + B).

%---------------------------------------------------------------------------%
%
% loop_check.
%

:- pragma loop_check(loop_check_proc(in, not_a_mode)).

:- pred loop_check_proc(int::in, int::out) is det.

loop_check_proc(N, R) :-
   ( if N = 0 then
        R = 1
   else
        lookup_check_proc(N - 1, R0),
        R = 10 * R0
   ).

%---------------------------------------------------------------------------%
%
% minimal_model.
%

:- pragma minimal_model(tc(in, not_a_mode)).

:- pred tc(int::in, int::out) is nondet.

tc(A, B) :-
    edge(A, C),
    (
        B = C
    ;
        tc(C, B)
    ).

:- pred edge(int::in, int::out) is nondet.

edge(1, 2).
edge(1, 3).
edge(2, 1).
edge(3, 4).

%---------------------------------------------------------------------------%
%
% type_spec.
%

:- pragma type_spec(type_spec_pred(in, not_a_mode), (T = not_a_type)).

:- pred type_spec_pred(T::in, T::out) is det.

type_spec_pred(A, A).

%---------------------------------------------------------------------------%
:- end_module pragma_qual_error.
%---------------------------------------------------------------------------%
