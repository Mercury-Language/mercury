%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2002 IFSIC.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
% Computes the call site list of a Mercury module, and generates a monitor 
% (to be run by collect) that performs the call site coverage.
%
% Note that it will not only generate the call sites of the module, but
% also the ones of the modules it imports that are in the same directory
% (i.e., library modules excepted).
%
% This program is intended to be called by call_site_cov/2 morphine command
% defined in morphine/source/coverage.op scenario.
% 
% Limitations:
%
%    This program is largely untested, and hence almost certainly buggy.
%
%    Higher-order calls are not handled: e.g. solutions(foo, L) will miss the call
%    to foo (should be easy to fix).  
%
%    I suppose there is one mode per predicate or function; indeed, it is 
%    impossible to know in which mode a predicate in called from the source
%    code only. 
%    A way to fix that would be to search for that information in the HLDS 
%    dump.
%
%    In the same vein, library calls without module qualifiers that are 
%    defined in several modules with the same name (e.g., append) are
%    not handled cleanly. But that should not be a big deal as library modules
%    with the same name ougth to have the same mode.
%    Here again this can be fixed by looking at the HLDS dump.
%
%    Currently, I do not generate call sites for builtin procedures such 
%    as =/2 or true/0 as they don't appear in the trace.
%    Maybe it would be better not to ignore them and let the monitors
%    believe that they are uncovered even when they are ???


:- module generate_call_site_cov.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module term, string, parser, io, term_io, list.
:- import_module std_util, set, char, int, require.
:- import_module coverage_util.

main -->
	io__command_line_arguments(Args),
	( 
	    (
		 { Args = [FileName] , LibPath = "" }
	       ; { Args = [FileName, LibPath] } 
	    )
	-> 
	    ( 
		io__see(FileName, Result),
		( 
		    { Result = ok }
		->
		    get_read_item_list(ItemList),
		    io__seen,

		    { get_imported_module_list("", ItemList, ImpModList) },
		    get_all_imported_module_list(LibPath, ImpModList, 
			    ImpModList, ReallyAllModList, _),
		    get_all_imported_module_list("", ImpModList, ImpModList, 
			    AllModList, _),
		    % ReallyAllModList should also contain all imported modules,
		    % even library ones.
		    ( { AllModList = ReallyAllModList } ->
			    print_list(
			    ["*** Warning: No library source files were found",
			     "*** Have you set the second arg of get_call_site",
			     "*** to the correct Mercury library path?\n"])
			;
			    []
		    ),
		    
		    { get_proc_det_list(FileName, ItemList, DetList1) }, 
		    get_all_proc_det_list(ReallyAllModList, DetList2),
		    { append(DetList1, DetList2, DetList) },

 		    { get_call_site_list(ItemList, CallSiteList1) },
 		    get_all_call_site_list(AllModList, CallSiteList2),
		    { append(CallSiteList1, CallSiteList2, CallSiteList)},

		    { get_call_site_list_criteria(DetList, CallSiteList,
			CallSiteCritList) },
		    generate_monitor(FileName, csc(CallSiteCritList), "call_site")
		;
		    io__write_string("File does not exist\n")
		 ))
	;
	     io__write_string("*** Bad number of args when calling\n"),
	     io__write_string("*** Usage: get_call_site <file> "),
	     io__write_string("[<Mercury lib path>]\n")
    ).

:- type call_site ---> cs(string, string, int).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred get_all_call_site_list(list(string)::in, list(call_site)::out, 
	io__state::di, io__state::uo) is det.
get_all_call_site_list(FileList, CallSiteList) -->
	list__map_foldl(get_call_site_list_from_file, FileList, 
		CallSiteListList),
	{ list__condense(CallSiteListList, CallSiteList) }.
	

:- pred get_call_site_list_from_file(string::in, list(call_site)::out, 
	io__state::di, io__state::uo) is det.
get_call_site_list_from_file(FileName, CSL) -->
	io__see(FileName, Res),
	( { Res = ok } ->
		get_read_item_list(ItemList), 
		io__seen,   
		{ get_call_site_list(ItemList, CSL) }
	   ;
		{ CSL = [] }
	).


:- pred get_call_site_list(list(term)::in, list(call_site)::out) is det.
get_call_site_list(ItemList, CallSiteList) :-
	list__filter_map(get_call_site, ItemList, CallSiteListList), 
	list__condense(CallSiteListList, CallSiteList0),
	list__remove_dups(CallSiteList0, CallSiteList).


:- pred get_call_site(term__term::in, list(call_site)::out) is semidet.
get_call_site(term__functor(Const, [_, TermBody], _context), CallSiteList) :-
	( Const = atom(":-") ; Const = atom("-->") ),
	get_call_site_body(TermBody, CallSiteList).


:- pred get_call_site_body(term__term::in, list(call_site)::out) is semidet.
get_call_site_body(term__functor(Const, ListTerm, context(Mod, LN)), 
    CallSiteList) :-
	Const = atom(Atom),
	CallSite = cs(Mod, Atom, LN),
	( 
	        length(ListTerm, Arity),
	        is_a_constructor(Atom, Arity)
	->
		list__filter_map(get_call_site_body, ListTerm, 
			CallSiteListList),
		list__condense(CallSiteListList, CallSiteList)
	;
   		ignore_call(Atom)
	->
		CallSiteList = []
	;
		% High order calls (call(Closure, Arg1, ...), 
		% apply(Func, Arg1, ...)) 
   		is_a_high_order_call(Atom) 
		%
		% XXX HO (pred and func) calls with the syntax Var(Arg1, ...) 
		% are missed
		%
		% XXX We also miss high order calls made by other high 
		% order procs such as `std_util__solutions' or 
		% `list__filter_map'. Maybe it does not matter too much 
		% since the call is necessary being done if the HO pred 
		% is called.
	->
		ListTerm = [term__functor(atom(Hofa), _, context(ModH, LNH))|_],
		CallSiteList = [cs(Hofa, ModH, LNH)]
	;
		CallSiteList = [CallSite]
	).


:- pred is_a_constructor(string::in, int::in) is semidet.
is_a_constructor(";", 2).
is_a_constructor(",", 2).
is_a_constructor("{}", _).
is_a_constructor("->", 2).
is_a_constructor("if", 1).
is_a_constructor("then", 2).
is_a_constructor("else", 2) .
is_a_constructor("=>", 2).
is_a_constructor("<=", 2).
is_a_constructor("<=>", 2).
is_a_constructor("all", 2).
is_a_constructor("some", 2).
is_a_constructor("not", 1).
is_a_constructor("\\+", 1).


:- pred ignore_call(string::in) is semidet.
ignore_call("[|]"). % Corresponds to the var list of `all' and `some' 
		    % quantifiers.
ignore_call("[]").  % Null DCG goal.

% XXX Ignore those builtin predicates as they cannot be traced?
ignore_call("true").  
ignore_call("false").
ignore_call("is"). 

% XXX Should really be ignored ?
ignore_call("=").   % XXX We would like to check those calls, but they
		    % do not appear in the trace. That's a real pain...
ignore_call("\\="). % XXX Ditto


:- pred is_a_high_order_call(string::in) is semidet.
is_a_high_order_call("call").  % pred call
is_a_high_order_call("apply"). % func call
is_a_high_order_call("").      % High order function application of the 
		               % form: `FuncVar(Arg1, ...)'.  


%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred get_call_site_list_criteria(list(proc_det)::in, list(call_site)::in, 
 	list(call_site_crit)::out) is det.
get_call_site_list_criteria(DetList, CallSiteList, CallSiteCritList) :-
	map(call_site_to_call_site_crit(DetList), CallSiteList, 
		CallSiteCritList).


:- pred call_site_to_call_site_crit(list(proc_det)::in, call_site::in, 
	call_site_crit::out) is det.
call_site_to_call_site_crit(Pdl, Cs, Csc) :-
% XXX Actually, there can be more than one solution if several procs
% have the same name in different modules. I need the type inference
% result to solve that problem. Note that it should not occur too often,
% and when it happens, the determinism ougth to be the same 
% (e.g., append for lists and strings...)
%
% XXX It would also require a mode analysis to know in which mode append is
% called... Sigh, I should really parse the HLDS ...
%
% It means that I suppose it works for one mode => one predicate !
% 
% Well, the false entry probably can be fixed by hand...
%
	solutions(call_site_to_call_site_crit_cc(Pdl, Cs), [Csc|_]).

:- pred call_site_to_call_site_crit_cc(list(proc_det)::in, call_site::in, 
	call_site_crit::out) is multi.
call_site_to_call_site_crit_cc(DetList,  cs(Mod0, Pred0, Ln), 
    csc(Mod, Pred, Ln, Crit)) :-
	( remove_suffix(Mod0, ".m", Mod1) -> Mod = Mod1 ; Mod = Mod0),
	% remove module qualifier if present
	( sub_string_search(Pred0, "__", Int) ->
		Pred = string__right(Pred0, length(Pred0) - (Int+2))
	;
		Pred = Pred0
	),
	(
	        is_predefined(Pred, Det)
	->
	        det_to_port_list(Det, Crit)
	;
	        member(_Mod - Pred - Det, DetList)
	->
		det_to_port_list(Det, Crit)
	;
	        Crit = []
	).


% Some calls won't appear in user programs, neitheir in libs...
:- pred is_predefined(string::in, string::out) is semidet.
is_predefined("true", "multi").
is_predefined("fail", "failure").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred print_list(list(T)::in, io__state::di, io__state::uo) is det.

print_list([]) --> [].
print_list([Term|Tail]) -->
	print(Term), nl,
	print_list(Tail).


