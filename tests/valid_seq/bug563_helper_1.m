:- module bug563_helper_1.
:- interface.

:- import_module bug563_helper_2.

:- type props
    --->    props(
                prop_foo    :: int,
                quirks      :: quirks
            ).
