% Check that the compiler doesn't give
% "determinism declaration could be tighter"
% warnings for type class method implementations.
% They will usually be spurious.
:- module typeclass_det_warning.

:- interface.

:- typeclass my_enum(T) where [
	func my_from_int(int) = T is semidet
].

:- instance my_enum(int).

:- implementation.

:- import_module std_util.

:- instance my_enum(int) where [
	func(my_from_int/1) is id
].

