%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambig_pred_helper_2.

:- interface.

:- import_module ambig_pred_helper_4.

:- pred confuse(T::in, b::in, T::in) is det.

:- implementation.

confuse(_, _, _).

:- end_module ambig_pred_helper_2.
