% simple22
% test

% the test is just that this compiles
% (testing the generate_unify in bytecode_gen.m)

% --------------------------------------------------------------------------
:- module simple.

% --------------------------------------------------------------------------
:- interface.

:- import_module io.
:- import_module int.
:- import_module char.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

% --------------------------------------------------------------------------
:- implementation.

main -->
	{
		dothings
	}.

% ------------------------------------------
:- func sgetch = character is det.
sgetch = 'a'.

:- pred checkch is det.
checkch :- (	sgetch = sgetch
	->	true ;	true).
% ---------------------------------
:- func sgets = string is det.
sgets = "a".

:- pred checks is det.
checks :- (	sgets = sgets
	->	true ;	true).
% ---------------------------------
:- func sgeti = int is det.
sgeti = 3.

:- pred checki is det.
checki :- (	sgeti = sgeti
	->	true ;	true).
% ---------------------------------
:- func sgetf = float is det.
sgetf = 3.0.

:- pred checkf is det.
checkf :- (	sgetf = sgetf
	->	true ;	true).

% -----------------------------------------------
:- pred dothings is det.
dothings :-
	checkch,
	checks,
	checki,
	checkf.

