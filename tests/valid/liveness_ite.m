%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% The compiler aborted on this module with tracing enabled.
%
% Uncaught Mercury exception:
% Software Error: liveness.m: Unexpected:
%   branches of if-then-else disagree on liveness
% First:
% Rest:  TypeInfo_for_T_8
%
%---------------------------------------------------------------------------%

:- module liveness_ite.
:- interface.

:- pred mypred(T::in, T::out) is erroneous.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%---------------------------------------------------------------------------%

:- type rollback_exception
    --->    rollback_invalid_transaction
    ;       rollback_retry.

:- type stm_validation_result
    --->    stm_transaction_valid
    ;       stm_transaction_invalid.

mypred(In, _Out) :-
    ( semidet_true ->
        IsValid = ff,
        (
            IsValid = stm_transaction_valid,
            throw(rollback_retry)
        ;
            IsValid = stm_transaction_invalid,
            throw(rollback_invalid_transaction)
        )
    ;
        throw(In)
    ).

:- func ff = stm_validation_result.
:- pragma no_inline(ff/0).

ff = stm_transaction_valid.

%---------------------------------------------------------------------------%
