%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2002 IFSIC.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file License in the Morphine distribution.
% 
% Author : Erwan Jahier <jahier@irisa.fr>
%
% Generates a monitor (to be run by collect) that performs the predicate 
% coverage.
%

:- module generate_pred_cov.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module term, string, parser, io, term_io, list.
:- import_module std_util, set, char, int, require.
:- import_module coverage_util.

main -->
	io__command_line_arguments(Args),
	( { Args = [FileName| _]  } -> 
	    ( 
		io__see(FileName, Result),
		( { Result = ok } ->
		    get_read_item_list(ItemList),
		    io__seen,

		    { get_imported_module_list("", ItemList, ImpModList) },
		    get_all_imported_module_list("", ImpModList, ImpModList, 
			    AllModList, _),

		    % we want the predicates defined in the module it imports
		    % which are in the same directory
		    { get_proc_det_list(FileName, ItemList, DetList1) }, 
		    get_all_proc_det_list(AllModList, DetList2),
		    { append(DetList1, DetList2, DetList) },
		    
		    { get_pred_list_criteria(DetList, PredCritList) },
		    generate_monitor(FileName, pc(PredCritList), "pred")
		;
		    io__write_string("File does not exist\n")
		 ))
	;
	     print("*** generate_pred_cov take a mercury file name as input\n")
    ).




%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- pred get_pred_list_criteria(list(proc_det)::in, list(pred_crit)::out) 
        is det.
get_pred_list_criteria(DetList, PredCritList) :-
	map(pred_to_pred_crit, DetList, PredCritList).


:- pred pred_to_pred_crit(proc_det::in, pred_crit::out) is det.
pred_to_pred_crit((Mod0-Pred0) - Det, pc(Mod, Pred, PortList)) :-
	( remove_suffix(Mod0, ".m", Mod1) -> Mod = Mod1 ; Mod = Mod0 ),
	( sub_string_search(Pred0, "__", Int) ->
		Pred = string__right(Pred0, length(Pred0) - (Int+2))
	;
		Pred = Pred0
	),
	det_to_port_list(Det, PortList).



