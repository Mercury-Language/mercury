:- module ambig_confuse3.

:- interface.

:- import_module ambig_types.

:- pred confuse(T::in, T::in, c::in) is det.

:- implementation.

confuse(_, _, _).

:- end_module ambig_confuse3.
