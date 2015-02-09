
:- module sub_a.

:- interface.

:- type t1.

:- module sub1.

:- interface.

:- type t2.

:- end_module sub1.

:- implementation.

:- module sub1.

:- implementation.

:- import_module int.

:- type t2 == int.

:- end_module sub1.

:- import_module float.

:- type t1 == float.

