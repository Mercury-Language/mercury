% This caused the following assertion failure in the term size
% profiling grades with rotd-2005-01-11 and before:
%
%	Uncaught Mercury exception:
%	Software Error: pred_args_to_func_args: function missing return value?
%
% To reproduce the bug, compile with:
% 	
% 	mmc -C --grade asm_fast.gc.tsw size_prof_ho_bug. 
%

:- module size_prof_ho_bug. 

:- interface.

:- type version_hash_table.

:- pred set(version_hash_table::in, version_hash_table::out) is det.

:- implementation.

:- type version_hash_table 
	---> ht(
		
		hash_func  :: ((func) = int),
		bucket     :: bucket
	).

:- type bucket ---> bucket.

set(A, B) :-
	A = ht(F, bucket),
	B = ht(F, bucket).
