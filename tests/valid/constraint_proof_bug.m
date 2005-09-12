:- module constraint_proof_bug.

:-interface.

:-import_module io.

:-pred main(io.state::di,io.state::uo) is det.

:- implementation.
:-import_module constraint_proof_bug_lib,io,string,require.

:-type service==string.
:-type prestationid==int.
:-type provider==int.

:-type bevent--->
       prest(prestationid,date,code,provider)
 ;     day(int,date)
 ;     admit(date,service)
 ;     discharge(date,service)
 ;     transfer(date,service,service)
 ;     wrong(date).

:-some [T] pred get_field(bevent,string,T) => constrainable(T).
:-mode get_field(in,in,out) is semidet.

get_field(Ev,Field,R) :-
	Field="date" -> R=d(Dt),get_date_field(Ev,Dt)
	;
	Field="code" -> R=c(Cd),get_code_field(Ev,Cd)
	;
	error("No handler for this field"). 

:-pred get_date_field(bevent::in,date::out) is det.
get_date_field(prest(_,Dt,_,_),Dt).
get_date_field(day(_,Dt),Dt).
get_date_field(admit(Dt,_),Dt).
get_date_field(discharge(Dt,_),Dt).
get_date_field(transfer(Dt,_,_),Dt).
get_date_field(wrong(Dt),Dt).

:-pred get_code_field(bevent::in,code::out) is semidet.
get_code_field(prest(_,_,Cd,_),Cd).

main-->
	print("hello world\n").
