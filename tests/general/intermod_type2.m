:- module intermod_type2.

:- interface.

:- type win.

:- type wopt
	--->	border
	;	title(string)
	.

:- implementation.

%------------------------------------------------------------------------------%

:- import_module array, char, int, list, require, std_util, store, string.

:- type curse	== store(some_store_type).

:- type win == mutvar(window, some_store_type).

:- type window
	--->	win(
			win,		% parent
			int,		% width
			int,		% height
			list(wopt),
			array(char),	% contents
			list(child),	% visible
			list(child)	% hidden
		).

:- type child
	--->	child(
			int,		% x
			int,		% y
			win
		).

:- type cursor
	--->	cursor(int, int).

%------------------------------------------------------------------------------%

