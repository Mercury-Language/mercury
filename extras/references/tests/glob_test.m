%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2000 Monash University.
% Copyright (C) 2003, 2006 The University of Melbourne & KU Leuven.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%

% Module to test "statically" allocated references.
% This code is adapted from output of the HAL compiler.
% Main author: wharvey@cs.monash.edu.au (Warwick Harvey)

:- module glob_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module require.
:- import_module io.
:- import_module reference.
:- import_module nb_reference.

%-----------------------------------------------------------------------------%

:- type yesno
	--->	yes
	;	no.

:- type target_lang
	--->	mercury
	;	sicstus.

:- func glob_Optimise = reference(yesno).
:- func glob_TargetLang = nb_reference(target_lang).

:- func glob_var_init_Optimise_mode_proc_1=yesno.
:- mode glob_var_init_Optimise_mode_proc_1=out is det.

:- func glob_var_init_TargetLang_mode_proc_1=target_lang.
:- mode glob_var_init_TargetLang_mode_proc_1=out is det.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
	#include ""c_reference.h""
	extern ME_Reference HAL_glob_Optimise;
	extern ME_NbReference HAL_glob_TargetLang;
").

:- pragma foreign_code("C", "
	ME_Reference HAL_glob_Optimise;
	ME_NbReference HAL_glob_TargetLang;
").

glob_Optimise = reference.from_c_pointer(glob_Optimise_2).

:- func glob_Optimise_2 = c_pointer.
:- pragma foreign_proc("C",
	glob_Optimise_2 = (X::out),
	[promise_pure, will_not_call_mercury],
"
	X = (MR_Word) &HAL_glob_Optimise;
").

glob_TargetLang = nb_reference__from_c_pointer(glob_TargetLang_2).

:- func glob_TargetLang_2 = c_pointer.
:- pragma foreign_proc("C",
	glob_TargetLang_2 = (X::out),
	[promise_pure, will_not_call_mercury],
"
	X = (MR_Word) &HAL_glob_TargetLang;
").

:- impure pred glob_var_init is det.

glob_var_init :-
        =(ResultTargetLang,glob_var_init_TargetLang_mode_proc_1),
        impure (init(glob_TargetLang,ResultTargetLang)),
        =(ResultOptimise,glob_var_init_Optimise_mode_proc_1),
        impure (init(glob_Optimise,ResultOptimise)).

% :- func glob_var_init_Optimise_mode_proc_1=yesno.
% :- mode glob_var_init_Optimise_mode_proc_1=out is det.

=(glob_var_init_Optimise_mode_proc_1,Y76) :-
        =(Y76,yes).

% :- func glob_var_init_TargetLang_mode_proc_1=glob:target_lang.
% :- mode glob_var_init_TargetLang_mode_proc_1=out is det.

=(glob_var_init_TargetLang_mode_proc_1,Y76) :-
        =(Y76,sicstus).


:- pragma promise_pure(main/2).

main -->
	{ impure glob_var_init },
        { semipure value(glob_Optimise, Opt0) },
	io__write_string("Initial value of $Optimise: "),
	io__write(Opt0),
	nl,
	io__write_string("Setting $Optimise to `no'.\n"),
        { impure update(glob_Optimise, no) },
        { semipure value(glob_Optimise, Opt1) },
	io__write_string("New value of $Optimise: "),
	io__write(Opt1),
	nl,
	io__write_string("Setting $Optimise to `yes' in failing branch.\n"),
	{
		impure update(glob_Optimise, yes),
		fail
	;
		true
	},
        { semipure value(glob_Optimise, Opt2) },
	io__write_string("New value of $Optimise: "),
	io__write(Opt2),
	nl,
	{ semipure value(glob_TargetLang, Lang0) },
	io__write_string("Initial value of $TargetLang: "),
	io__write(Lang0),
	nl,
	io__write_string("Setting $TargetLang to `mercury'.\n"),
	{ impure update(glob_TargetLang, mercury) },
	{ semipure value(glob_TargetLang, Lang1) },
	io__write_string("New value of $TargetLang: "),
	io__write(Lang1),
	nl,
	io__write_string("Setting $TargetLang to `sicstus' in failing branch.\n"),
	{
		impure update(glob_TargetLang, sicstus),
		fail
	;
		true
	},
	{ semipure value(glob_TargetLang, Lang2) },
	io__write_string("New value of $TargetLang: "),
	io__write(Lang2),
	nl.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
