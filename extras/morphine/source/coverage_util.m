%------------------------------------------------------------------------------%
% Copyright (C) 2001 IFSIC.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%

:- module coverage_util.
:- interface.
:- import_module list, string, term, io, std_util.

:- type proc_det == pair(
	pair(string, string),   % Procedure and module name 
	string).		% Determinism
:- type exit_or_fail ---> exit ; fail ; exception.


:- pred get_read_item_list(list(term__term)::out, 
	io__state::di, io__state::uo) is det.

:- pred get_imported_module_list(string::in, list(term)::in, 
	list(string)::out) is det.
:- pred get_all_imported_module_list(
	string::in,	% Path of the Mercury library source files
	list(string)::in,  % Current list of imported modules
	list(string)::in,  % List of modules to be visited
	list(string)::out, % New list of imported modules
	list(string)::out, % New list of modules to be visited
	io__state::di, io__state::uo
    ) is det.


:- pred get_all_proc_det_list(list(string)::in, list(proc_det)::out, 
	io__state::di, io__state::uo) is det.
:- pred get_proc_det_list(string::in, list(term)::in, list(proc_det)::out) 
	is det.

:- pred det_to_port_list(string::in, list(exit_or_fail)::out) is det.

:- pred generate_monitor(string::in, list(T)::in, string::in,
	io__state::di, io__state::uo) is det.

:- implementation.

:- import_module char, int, require, parser, term_io, set.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

get_read_item_list(ItemList) -->
	parser__read_term(ReadTerm),
	( 
		{ ReadTerm = eof },
		{ ItemList0 = [] },
		{ ItemList1 = [] }
	;
		{ ReadTerm =  term(_Varset, Term) },
		{ ItemList0 = [Term] },
		get_read_item_list(ItemList1)
	;
		{ ReadTerm =  error(_string, _int) },
		print("*** Parse Error\n\n"),
		{ ItemList0 = [] },
		{ ItemList1 = [] }
	),
	{ append(ItemList0, ItemList1, ItemList) }.


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%


get_imported_module_list(LibPath, ItemList, ModList) :-
	list__filter_map(get_imported_module, ItemList, ModListList0), 
	list__condense(ModListList0, ModList0),
	list__remove_dups(ModList0, ModList1),
	list__map(add_prefix(LibPath), ModList1, ModList2),
	list__map(add_suffix(".m"), ModList2, ModList).


% To be able to apply list__map
:- pred add_suffix(string::in, string::in, string::out) is det.
add_suffix(Suffix, String0, String) :-
	append(String0, Suffix, String).

:- pred add_prefix(string::in, string::in, string::out) is det.
add_prefix(Prefix, String0, String) :-
	append(Prefix, String0, String).

:- pred get_imported_module(term__term::in, list(string)::out) is semidet.
get_imported_module(Term, ImpModList) :-
	Term =  functor(atom(":-"), [Term2 | _], _),
	Term2 = functor(atom("import_module"), [Term3 | _], _),
	Term3 = functor(atom(A), _, _),
	( A = "," ->
		% more than one module is imported
		get_imported_module2(Term3, ImpModList)
	;
		% Only one module is imported
		ImpModList = [A]
	 ).

:- pred get_imported_module2(term__term::in, list(string)::out) is semidet.
get_imported_module2(Term, ImpModList) :-
	Term =  functor(atom(","), [Term1, Term2], _),
	Term1 = functor(atom(A1), _, _),
	Term2 = functor(atom(A2), _, _),
	( A2 = "," ->
		get_imported_module2(Term2, ImpModList0)
	;
		ImpModList0 = [A2]
	),
	ImpModList = [A1 | ImpModList0].

%-----------------------------------------------------------------------%


get_all_imported_module_list(LibPath, ML0, MLv0, ML, MLv) -->
	list__map_foldl(get_module_list_from_file(LibPath), MLv0, MLL1),
	{ list__condense(MLL1, ML1) },
	{ set__list_to_set(ML0, MS0) },
	{ set__list_to_set(ML1, MS1) },
	{ set__union(MS0, MS1, MS2) },
	{ set__to_sorted_list(MS2, ML2) },
	{ set__intersect(MS0, MS1, Inter) },
	{ set__difference(MS1, Inter, MSv2) },
	{ set__to_sorted_list(MSv2, MLv2) },
	( { MLv2 = [] } ->
		% The fix point is reached
		{ ML = ML2 },
		{ MLv = MLv2 }
	    ;
		get_all_imported_module_list(LibPath, ML2, MLv2, ML, MLv)
	).


:- pred get_module_list_from_file(string::in, string::in, list(string)::out, 
	io__state::di, io__state::uo) is det.
get_module_list_from_file(LibPath, FileName, ML) -->
	io__see(FileName, Res), 
	( { Res = ok } ->
		get_read_item_list(ItemList),  
		io__seen,  
		{ get_imported_module_list("", ItemList, ML) }
	    ;
		{ append_list([LibPath, FileName], FileName2) },
		io__see(FileName2, Res2),
		( { Res2 = ok } ->
			get_read_item_list(ItemList),
			io__seen,
			{ get_imported_module_list(LibPath, ItemList, ML) }
		 ;
			{ ML = [] }
		)
	).


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%


% XXX This duplicates a little bit the code of get_all_imported_module_list
% in some way
get_all_proc_det_list(FileList, ProcDetList) -->
	list__map_foldl(get_proc_det_list_from_file, FileList, ProcDetListList),
	{ list__condense(ProcDetListList, ProcDetList) }.
	
% Ditto.
:- pred get_proc_det_list_from_file(string::in, list(proc_det)::out, 
	io__state::di, io__state::uo) is det.
get_proc_det_list_from_file(FileName, PDL) -->
	io__see(FileName, Res),
	( { Res = ok } ->
		get_read_item_list(ItemList),
		io__seen,
		{ get_proc_det_list(FileName, ItemList, PDL) }
	   ;
		{ PDL = [] }
	).


get_proc_det_list(Mod, ItemList, ProcDetList) :-
	list__filter_map(get_proc_det(Mod), ItemList, ProcDetList0), 
	list__remove_dups(ProcDetList0, ProcDetList).


% get_proc_det(Mod, Item, ProcDet) takes an item and outputs a procedure name
% and its determinism if Item is a declaration, fails otherwise.
%
:- pred get_proc_det(string::in, term__term::in, proc_det::out) 
	is semidet.
get_proc_det(Mod, Term, (Mod - ProcName) - Det) :-
	Term =  functor(atom(":-"), [Term2 | _], _),
	Term2 = functor(atom(A),    [Term3 | _], _),
	( A = "pred" ; A = "func" ; A = "mode"),
	Term3 = functor(atom("is"), [Term4, Term5 | _], _),
	Term4 = functor(atom(B),    [Term6 | _], _),
	(
		B = "=" 
	->
		Term6 = functor(atom(ProcName0), _, _)
	;
		ProcName0 = B
	),
	Term5 = functor(atom(Det), _, _),
	remove_module_qualifier(Mod, ProcName0, ProcName).

:- pred remove_module_qualifier(string::in, string::in, string::out) is det.
remove_module_qualifier(Module, ProcName0, ProcName) :-
	%
	% Extract the module name from the file name
	ListStr = string__words(is_slash, Module),
	reverse(ListStr, ListStrRev),
	(
		ListStrRev = [ModuleName|_],
		remove_suffix(ModuleName, ".m", ModuleBaseName0)
	 ->
		append(ModuleBaseName0, "__", ModuleBaseName)
	 ;
		error("Fail to extract the module name from the file name ") 
	),
	%
	% remove the module qualifier if necessary 
	% XXX Maybe I should rather add them when necessary?
	( append(ModuleBaseName, ProcName1, ProcName0) ->   
		ProcName = ProcName1
	;
		ProcName = ProcName0
	).
	

:- pred is_slash(char::in) is semidet.
is_slash('/').

% XXX should read a config file to determine which determinism generates
% which list of ports to be covered.
det_to_port_list(Det, PortList) :-
	(
		( Det = "det" ; Det = "cc_multi" )
	->
		PortList = [exit]
	;
		Det = "nondet"
	->
		PortList = [exit, exit, fail]
	;
		Det = "multi"
	->
		PortList = [exit, exit]
	;
		( Det = "semidet" ; Det = "cc_nondet" )
	->
		PortList = [exit, fail]
	;
		Det = "failure"
	->
		PortList = [fail]
	;
		PortList = [exception]
	).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

generate_monitor(FileName0, CritList, CovType) -->
	( { remove_suffix(FileName0, ".m", FileName1) } ->
		{ FileName2 = FileName1 }
	;
		{ FileName2 = FileName0 }
	),
	{ append_list([FileName2, "__", CovType, "_cov"], FileName3) },
	io__tell(FileName3, Res1),
	{ append(CovType, "_cov.in", FileIn) },
	io__see( FileIn, Res2),
	( 
		{ Res1 = error(Msg1) }
	->
		print(Msg1),
		io__told
	;
		{ Res2 = error(Msg2) }
	->
		print(Msg2),
		print("\nMake sure that "),
		print(CovType),
		print("_cov.in is in the current"),
		print(" directory.\nMaybe you can try to "),	    
		print("`ln -s .../morphine/source/"),
		print(CovType),
		print("_cov.in'\n"),
		io__seen
	;
	    io__read_file_as_string(BeginningResult),
	    (
		{ BeginningResult = error(_, Msg3) },
		print(Msg3)
	    ;
		{ BeginningResult = ok(Beginning) },
		print("% File automatically generated by get_"),
	        print(CovType),
		print(".m\n\n"),
		print(Beginning),
		print("initialize(Acc) :- \n\t list__condense(["),
		% I do not generate the list of call site in the first place
		% as the Mercury compiler has trouble to compile large list
		% (square time in the size of the list).
		% Therefore I cut the list into chunks and apply list__condense
		% to it.
		% XXX check if it really changes anything
		print_crit_list(CritList, _),
		print("], Acc)."),
		io__told,
		io__seen
	    )
	),
	{ append_list([FileName2, "__", CovType], FileName4) },
	io__tell(FileName4, Res3),
	( 
		{ Res3 = error(Msg4) }
	->
		print(Msg4)
	;
		print(CritList), 
		print(".\n"),
		io__told
	).

:- pred print_crit_list(list(T)::in, list(T)::out, 
	io__state::di, io__state::uo) is det.
print_crit_list(List0, List) -->
	( { List0 = [] } ->
		{ List = List0 }
	;
		print("["),
		print_crit_list2(10, List0, List1),
		( { List1 = [] } ->
			print("\t\t] ")
		;
			print("\t\t], ")
		), 
		print_crit_list(List1, List)
	   ).

:- pred print_crit_list2(int::in, list(T)::in, list(T)::out, 
	io__state::di, io__state::uo) is det.
print_crit_list2(_, [], [], S, S).
print_crit_list2(N, [X|Xs], List) -->
	( { N = 0 ; Xs = [] } ->
		{ List = Xs },
		print("\t\t"),
		print(X),
		print(" \n")
	;
		print("\t\t"),
		print(X),
		print(", \n"),
		print_crit_list2(N-1, Xs, List)
	).
