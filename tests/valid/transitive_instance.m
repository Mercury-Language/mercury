:- module transitive_instance.

:- interface.

:- type t.

:- implementation.

:- import_module transitive_instance__sub2.

:- type t ---> some [T] c(T) => c2(T).

:- module transitive_instance__sub1.

	:- interface.

	:- typeclass c1(T) where []. 

	:- instance c1(int) where [].

:- end_module transitive_instance__sub1.


:- module transitive_instance__sub2.
	
	:- interface.

	:- import_module transitive_instance__sub1.

	:- typeclass c2(T) <= c1(T) where [].

	:- instance c2(int) where [].

:- end_module transitive_instance__sub2.
