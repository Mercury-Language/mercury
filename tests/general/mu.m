% This is a regression test.
% Mercury 0.6 generated incorrect code for this test case.

:- (module mu). 
:- interface. 
:- (import_module io). 
:- (pred (main((io__state '::' di), (io__state '::' uo)) is cc_multi)). 
:- implementation. 
:- (import_module (list ',' (int ',' (std_util ',' (char ',' string))))). 

main -->
	{mu(N,[m,(.)],Charlist,N0)}->
	{string__from_char_list(Charlist, S)},
	io__write_string(S), io__write_string("\n"),
	write_pint(N), write_pint(N0)
;
	io__write_string("Failed\n"). 

:- (type (pint '--->' (z ';' s(pint)))). 

:-pred write_pint(pint::in, io__state::di, io__state::uo) is det.
write_pint(z)--> io__write_string("0\n").
write_pint(s(N))--> io__write_string("1+"), write_pint(N).

:- (pred mu(pint, list(char), list(char), pint)). 
:- (mode (mu(out, in, out, out) is nondet)). 
mu(z, ['.'|Charlist], Charlist, z) .
mu(s(z), [m|Charlist1], Charlist1, N0) :- 	
	% s(N)=s(z), % if this line is commented out --> SEGV
	mu(N0, Charlist1, _Charlist, _).
