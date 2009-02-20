:- module ambig_confuse1.

:- interface.

:- import_module ambig_types.

:- pred confuse(a::in, T::in, T::in) is det.

:- implementation.

confuse(_, _, _).

:- end_module ambig_confuse1.
