%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_temploc.m - An abstract data type for temporary locations.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_temploc.

:- interface.

:- import_module vn_type, vn_table.
:- import_module list.

:- type templocs.

	% Initialize the list of temporary locations.

:- pred vn_temploc__init_templocs(vn_params, vnlvalset, vn_tables, templocs).
:- mode vn_temploc__init_templocs(in, in, in, out) is det.

	% Get a non-float temporary location.

:- pred vn_temploc__next_tempr(templocs, templocs, vnlval).
:- mode vn_temploc__next_tempr(in, out, out) is det.

	% Get a float temporary location.

:- pred vn_temploc__next_tempf(templocs, templocs, vnlval).
:- mode vn_temploc__next_tempf(in, out, out) is det.

	% Prevent the use of this location as a temporary.

:- pred vn_temploc__no_temploc(vnlval, templocs, templocs).
:- mode vn_temploc__no_temploc(in, in, out) is det.

	% Make a location available for reuse as a temporary location
	% _if_  it is not live. Heap locations are implicitly live; other
	% locations are live if they were recorded as such during init.

:- pred vn_temploc__reuse_templocs(list(vnlval), templocs, templocs).
:- mode vn_temploc__reuse_templocs(in, in, out) is det.

	% Give the numbers of the highest temp variable used.

:- pred vn_temploc__max_tempr(templocs, int).
:- mode vn_temploc__max_tempr(in, out) is det.

:- pred vn_temploc__max_tempf(templocs, int).
:- mode vn_temploc__max_tempf(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, require, int, set.

:- type templocs ---> templocs(
				vnlvalset,	% live values; don't use
				list(vnlval),	% queue of r regs and temps
				list(vnlval),	% queue of f regs and temps
				int,		% max r temp used
				int,		% next r temp to use
				int,		% max f temp used
				int		% next f temp to use
			).

vn_temploc__init_templocs(Params, Livevals, VnTables, Templocs) :-
	vn_type__real_r_regs(Params, MaxRReg),
	vn_type__real_f_regs(Params, MaxFReg),
	vn_type__real_r_temps(Params, MaxRTemp),
	vn_type__real_f_temps(Params, MaxFTemp),
	vn_temploc__get_n_r_temps(1, MaxRTemp, RTemps),
	vn_temploc__get_n_f_temps(1, MaxFTemp, FTemps),
	vn_temploc__find_free_r_regs(1, MaxRReg, Livevals, VnTables, RRegs),
	vn_temploc__find_free_f_regs(1, MaxFReg, Livevals, VnTables, FRegs),
	list__append(RRegs, RTemps, RQueue),
	list__append(FRegs, FTemps, FQueue),
	NextRTemp is MaxRTemp + 1,
	NextFTemp is MaxFTemp + 1,
	Templocs = templocs(Livevals, RQueue, FQueue,
		0, NextRTemp, 0, NextFTemp).

vn_temploc__next_tempr(Templocs0, Templocs, Vnlval) :-
	Templocs0 = templocs(Live, RQueue0, FQueue0, MaxR0, NextR0, MF, NF),
	( RQueue0 = [Head | Tail] ->
		Vnlval = Head,
		RQueue = Tail,
		NextR = NextR0
	;
		Vnlval = vn_temp(r, NextR0),
		RQueue = RQueue0,
		NextR is NextR0 + 1
	),
	( Vnlval = vn_temp(r, R) ->
		int__max(MaxR0, R, MaxR)
	;
		MaxR = MaxR0
	),
	Templocs = templocs(Live, RQueue, FQueue0, MaxR, NextR, MF, NF).

vn_temploc__next_tempf(Templocs0, Templocs, Vnlval) :-
	Templocs0 = templocs(Live, RQueue0, FQueue0, MR, NR, MaxF0, NextF0),
	( FQueue0 = [Head | Tail] ->
		Vnlval = Head,
		FQueue = Tail,
		NextF = NextF0
	;
		Vnlval = vn_temp(f, NextF0),
		FQueue = FQueue0,
		NextF is NextF0 + 1
	),
	( Vnlval = vn_temp(f, F) ->
		int__max(MaxF0, F, MaxF)
	;
		MaxF = MaxF0
	),
	Templocs = templocs(Live, RQueue0, FQueue, MR, NR, MaxF, NextF).

vn_temploc__no_temploc(Vnlval, Templocs0, Templocs) :-
	Templocs0 = templocs(Live, RQueue0, FQueue0, MR, NR, MF, NF),
	list__delete_all(RQueue0, Vnlval, RQueue),
	list__delete_all(FQueue0, Vnlval, FQueue),
	Templocs = templocs(Live, RQueue, FQueue, MR, NR, MF, NF).

vn_temploc__reuse_templocs([], Templocs, Templocs).
vn_temploc__reuse_templocs([Vnlval | Vnlvals], Templocs0, Templocs) :-
	Templocs0 = templocs(Live, RQueue0, FQueue0, MR, NR, MF, NF),
	( set__member(Vnlval, Live) ->
		Templocs1 = Templocs0
	; ( Vnlval = vn_reg(r, _) ; Vnlval = vn_temp(r, _) ) ->
		list__append(RQueue0, [Vnlval], RQueue1),
		Templocs1 = templocs(Live, RQueue1, FQueue0, MR, NR, MF, NF)
	; ( Vnlval = vn_reg(f, _) ; Vnlval = vn_temp(f, _) ) ->
		list__append(FQueue0, [Vnlval], FQueue1),
		Templocs1 = templocs(Live, RQueue0, FQueue1, MR, NR, MF, NF)
	;
		Templocs1 = Templocs0
	),
	vn_temploc__reuse_templocs(Vnlvals, Templocs1, Templocs).

vn_temploc__max_tempr(templocs(_, _, _, MaxRUsed, _, _, _), MaxRUsed).

vn_temploc__max_tempf(templocs(_, _, _, _, _, MaxFUsed, _), MaxFUsed).

	% Return the non-live registers from the first N r registers.

:- pred vn_temploc__find_free_r_regs(int, int, vnlvalset, vn_tables,
	list(vnlval)).
:- mode vn_temploc__find_free_r_regs(in, in, in, in, out) is det.

vn_temploc__find_free_r_regs(N, Max, Livevals, VnTables, Freeregs) :-
	( N > Max ->
		Freeregs = []
	;
		N1 is N + 1,
		vn_temploc__find_free_r_regs(N1, Max, Livevals, VnTables,
			Freeregs0),
		(
			set__member(vn_reg(r, N), Livevals)
		->
			Freeregs = Freeregs0
		;
			Vnrval = vn_origlval(vn_reg(r, N)), 
			vn_table__search_assigned_vn(Vnrval, Vn, VnTables)
		->
			(
				vn_table__search_uses(Vn, Uses, VnTables),
				Uses \= []
			->
				Freeregs = Freeregs0
			;
				Freeregs = [vn_reg(r, N) | Freeregs0]
			)
		;
			Freeregs = [vn_reg(r, N) | Freeregs0]
		)
	).

	% Return the non-live registers from the first N f registers.

:- pred vn_temploc__find_free_f_regs(int, int, vnlvalset, vn_tables,
	list(vnlval)).
:- mode vn_temploc__find_free_f_regs(in, in, in, in, out) is det.

vn_temploc__find_free_f_regs(N, Max, Livevals, VnTables, Freeregs) :-
	( N > Max ->
		Freeregs = []
	;
		N1 is N + 1,
		vn_temploc__find_free_f_regs(N1, Max, Livevals, VnTables,
			Freeregs0),
		(
			set__member(vn_reg(f, N), Livevals)
		->
			Freeregs = Freeregs0
		;
			Vnrval = vn_origlval(vn_reg(f, N)), 
			vn_table__search_assigned_vn(Vnrval, Vn, VnTables)
		->
			(
				vn_table__search_uses(Vn, Uses, VnTables),
				Uses \= []
			->
				Freeregs = Freeregs0
			;
				Freeregs = [vn_reg(f, N) | Freeregs0]
			)
		;
			Freeregs = [vn_reg(f, N) | Freeregs0]
		)
	).

	% Return a list of the first N tempr locations.

:- pred vn_temploc__get_n_r_temps(int, int, list(vnlval)).
:- mode vn_temploc__get_n_r_temps(in, in, out) is det.

vn_temploc__get_n_r_temps(N, Max, Temps) :-
	( N > Max ->
		Temps = []
	;
		N1 is N + 1,
		vn_temploc__get_n_r_temps(N1, Max, Temps0),
		Temps = [vn_temp(r, N) | Temps0]
	).

	% Return a list of the first N tempf locations.

:- pred vn_temploc__get_n_f_temps(int, int, list(vnlval)).
:- mode vn_temploc__get_n_f_temps(in, in, out) is det.

vn_temploc__get_n_f_temps(N, Max, Temps) :-
	( N > Max ->
		Temps = []
	;
		N1 is N + 1,
		vn_temploc__get_n_f_temps(N1, Max, Temps0),
		Temps = [vn_temp(f, N) | Temps0]
	).
