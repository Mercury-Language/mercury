%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test: rotd-1999-10-27 did not handle optimization of
% a singleton switch on an existentially typed constructor.
% Symptom:
%   Uncaught exception:
%   Software Error: instmap_delta_from_mode_list_2
%
%---------------------------------------------------------------------------%

:- module existential_type_switch_opt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type maybe
    --->    e_no
    ;       some [U] e_maybe(U)
    ;       some [T] e_yes(T).

:- pred p(maybe).
:- mode p(in) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module std_util.

main(!IO) :-
    ( if semidet_fail then
        X = 'new e_maybe'(2)
    else
        X = 'new e_yes'(1)
    ),
    ( if p(X) then
        io.write_string("succeeded\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pragma inline(p/1).
p(e_no).
p(e_yes(_)).
