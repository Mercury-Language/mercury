:- module aditi_error_bug3.
:- interface.

:- import_module list.

:- type real == float.
:- type count == int.

:- type city == string.
:- type us_state == string.
:- type year == int.
:- type rate == real.
:- type region == string.
:- type percent == real.
:- type crime_type ---> rape ; robbery ; murder ; assault ; arson ; mv_theft.

:- type ints == list(int).
