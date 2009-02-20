:- module ambig_confuse2.

:- interface.

:- import_module ambig_types.

:- pred confuse(T::in, b::in, T::in) is det.

:- implementation.

confuse(_, _, _).

:- end_module ambig_confuse2.
