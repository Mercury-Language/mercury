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
:- import_module list, set, int.

:- type templocs.

	% Initialize the list of temporary locations.

:- pred vn__init_templocs(int, int, vnlvalset, vn_tables, templocs).
:- mode vn__init_templocs(in, in, in, in, out) is det.

	% Get a temporary location.

:- pred vn__next_temploc(templocs, templocs, vnlval).
:- mode vn__next_temploc(di, uo, out) is det.

	% Prevent the use of this location as a temporary.

:- pred vn__no_temploc(vnlval, templocs, templocs).
:- mode vn__no_temploc(in, di, uo) is det.

	% Make a location available for reuse as a temporary location
	% _if_  it is not live. Heap locations are implicitly live; other
	% locations are live if they were recorded as such during init.

:- pred vn__reuse_templocs(list(vnlval), templocs, templocs).
:- mode vn__reuse_templocs(in, di, uo) is det.

	% Give the number of the highest temp variable used.

:- pred vn__max_temploc(templocs, int).
:- mode vn__max_temploc(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, require.

:- type templocs ---> quad(list(vnlval), vnlvalset, int, int).

vn__init_templocs(MaxTemp, MaxReg, Livevals, VnTables, Templocs) :-
	vn__get_n_temps(1, MaxTemp, Temps),
	vn__find_free_regs(1, MaxReg, Livevals, VnTables, Regs),
	list__append(Regs, Temps, Queue),
	NextTemp is MaxTemp + 1,
	Templocs = quad(Queue, Livevals, 0, NextTemp).

vn__next_temploc(quad(Queue0, Livevnlvals, MaxUsed0, Next0),
		quad(Queue1, Livevnlvals, MaxUsed1, Next1), Vnlval) :-
	( Queue0 = [Head | Tail] ->
		Vnlval = Head,
		Queue1 = Tail,
		Next1 = Next0
	;
		Vnlval = vn_temp(Next0),
		Queue1 = Queue0,
		Next1 is Next0 + 1
	),
	( Vnlval = vn_temp(N) ->
		int__max(MaxUsed0, N, MaxUsed1)
	;
		MaxUsed1 = MaxUsed0
	).

vn__no_temploc(Vnlval, quad(Queue0, Livevnlvals, MaxUsed, Next),
		quad(Queue, Livevnlvals, MaxUsed, Next)) :-
	list__delete_all(Queue0, Vnlval, Queue).

vn__reuse_templocs([], Templocs, Templocs).
vn__reuse_templocs([Vnlval | Vnlvals], Templocs0, Templocs) :-
	Templocs0 = quad(Queue0, Livevnlvals, MaxUsed, Next),
	( Vnlval = vn_field(_, _, _) ->
		Templocs1 = Templocs0
	; set__member(Vnlval, Livevnlvals) ->
		Templocs1 = Templocs0
	;
		list__append(Queue0, [Vnlval], Queue1),
		Templocs1 = quad(Queue1, Livevnlvals, MaxUsed, Next)
	),
	vn__reuse_templocs(Vnlvals, Templocs1, Templocs).

vn__max_temploc(quad(_Queue0, _Livevnlvals, MaxUsed, _Next), MaxUsed).

	% Return the non-live registers from the first N registers.

:- pred vn__find_free_regs(int, int, vnlvalset, vn_tables, list(vnlval)).
:- mode vn__find_free_regs(in, in, in, in, out) is det.

vn__find_free_regs(N, Max, Livevals, VnTables, Freeregs) :-
	( N > Max ->
		Freeregs = []
	;
		N1 is N + 1,
		vn__find_free_regs(N1, Max, Livevals, VnTables, Freeregs0),
		(
			set__member(vn_reg(r(N)), Livevals)
		->
			Freeregs = Freeregs0
		;
			Vnrval = vn_origlval(vn_reg(r(N))), 
			vn__search_assigned_vn(Vnrval, Vn, VnTables)
		->
			(
				vn__search_uses(Vn, Uses, VnTables),
				Uses \= []
			->
				Freeregs = Freeregs0
			;
				Freeregs = [vn_reg(r(N)) | Freeregs0]
			)
		;
			Freeregs = [vn_reg(r(N)) | Freeregs0]
		)
	).

	% Return a list of the first N temp locations.

:- pred vn__get_n_temps(int, int, list(vnlval)).
:- mode vn__get_n_temps(in, in, out) is det.

vn__get_n_temps(N, Max, Temps) :-
	( N > Max ->
		Temps = []
	;
		N1 is N + 1,
		vn__get_n_temps(N1, Max, Temps0),
		Temps = [vn_temp(N) | Temps0]
	).
