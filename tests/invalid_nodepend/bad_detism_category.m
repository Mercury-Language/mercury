%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Test the error message generated when the determinism on a pred, func,
% or mode declaration is invalid.
%---------------------------------------------------------------------------%

:- module bad_detism_category.
:- interface.

:- import_module io.

    % Predmode decl.
:- pred report_usage_error(io::di, io::uo)
    is et.    % Context of the error should be this line.

:- func test_function(T::in) = (T::out) is et.

:- pred report_usage_message2(io, io).
:- mode report_usage_message2(di, uo) is et.

:- some [T] pred existsq_pred(T::out) is et.

:- semipure pred smp_pred(T::out) is et.
:- impure pred imp_pred(T::out) is et.

:- pred constr_pred(T::in, T::out) is et <= foo(T).

:- typeclass foo(T) where [
    pred method1(T::in, T::out) is et,

    pred method2(T, T),
    mode method2(in, out) is et,

    func method3(T::in) = (T::out) is et,

    some [U] pred method4(T::in, U::out) is et
].
