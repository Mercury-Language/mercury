%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% il_peephole.m - local ILDS to ILDS optimizations based on pattern-matching.

% Authors: trd (based on peephole.m by fjh and zs).
%
% Please note that some of the optimizations in this module are required
% for verifiability of IL.
%
% Also, some of these optimizations would be more appropriate at the
% MLDS level.
%
% Patterns to add:
%
% [ ] starg, ldarg patterns (rare, but they are introduced by tailcall)
%
% [ ] loop hoisting (tailcall again)
%     looptop:
%	ldarg X
%	...instrs...
%	starg X
%	br looptop (no other branches to looptop).
%
%	This isn't really a peephole optimization, might be better done
%	elsewhere.

%-----------------------------------------------------------------------------%

:- module il_peephole.

:- interface.

:- import_module list.
:- import_module ilasm.

	% Peephole optimize a list of instructions.

:- pred il_peephole__optimize(list(ilasm__decl)::in, list(ilasm__decl)::out) 
	is det.

:- implementation.

:- type instrs == list(instr).

:- import_module assoc_list, bool, map, string, std_util, int.
:- import_module ilds, require.

	% We zip down to the end of the instruction list, and start attempting
	% to optimize instruction sequences. As long as we can continue
	% optimizing the instruction sequence, we keep doing so;
	% when we find a sequence we can't optimize, we back up and try
	% to optimize the sequence starting with the previous instruction.

optimize(Decls0, Decls) :-
	list__map_foldl(optimize_decl, Decls0, Decls, no, _Mod).

	% Mod is a bool that says whether the code was modified as a
	% result of the optimization (that is, whether Decl \= Decl0).
	% This can be used to decide whether to keep repeat the
	% optimizations.
:- pred optimize_decl(decl::in, decl::out, bool::in, bool::out) is det.
optimize_decl(Decl0, Decl, Mod0, Mod) :-
	( Decl0 = class(A, B, C, D, ClassMembers0) ->
		list__map_foldl(optimize_class_member, ClassMembers0,
			ClassMembers, Mod0, Mod),
		Decl = class(A, B, C, D, ClassMembers)
	; Decl0 = method(A, MethodDecls0) ->
		list__map_foldl(optimize_method_decl, MethodDecls0,
			MethodDecls, Mod0, Mod),
		Decl = method(A, MethodDecls)
	; Decl0 = namespace(A, NamespaceDecls0) ->
		list__map_foldl(optimize_decl, NamespaceDecls0,
			NamespaceDecls, Mod0, Mod),
		Decl = namespace(A, NamespaceDecls)
	;
		Mod = Mod0,
	 	Decl0 = Decl 
	).

:- pred optimize_class_member(class_member::in, class_member::out, 
	bool::in, bool::out) is det.
optimize_class_member(Decl0, Decl, Mod0, Mod) :-
	( Decl0 = method(A, MethodDecls0) ->
		list__map_foldl(optimize_method_decl, MethodDecls0,
			MethodDecls1, Mod0, Mod),
		( Mod = yes ->
				% find the new maxstack
			MaxStacks = list__map((func(X) = 
				( if X = instrs(I) 
				  then calculate_max_stack(I) 
				  else 0
				)), MethodDecls1),
			NewMaxStack = list__foldl((func(X, Y0) = X + Y0),
				MaxStacks, 0
			),
				% set the maxstack
			MethodDecls = list__map((func(X) = 
				( if X = maxstack(_) 
				  then maxstack(int32(NewMaxStack))
				  else X
				)), MethodDecls1),
			Decl = method(A, MethodDecls)
		;
			Decl = method(A, MethodDecls1)
		)
	;
		Mod = no,
	 	Decl0 = Decl 
	).

:- pred optimize_method_decl(method_body_decl::in, method_body_decl::out, 
	bool::in, bool::out) is det.
optimize_method_decl(Decl0, Decl, Mod0, Mod) :-
	( Decl0 = instrs(Instrs0) ->
		optimize_instrs(Instrs0, Instrs, Mod1),
		bool__or(Mod0, Mod1, Mod),
		Decl = instrs(Instrs)
	;
		Mod = Mod0,
	 	Decl0 = Decl 
	).

:- pred optimize_instrs(instrs::in, instrs::out, bool::out) is det.

optimize_instrs(Instrs0, Instrs, Mod) :-
	optimize_2(Instrs0, Instrs, Mod).

:- pred optimize_2(instrs, instrs, bool).
:- mode optimize_2(in, out, out) is det.
optimize_2([], [], no).
optimize_2([Instr0 | Instrs0], Instrs, Mod) :-
	optimize_2(Instrs0, Instrs1, Mod0),
	opt_instr(Instr0, Instrs1, Instrs, Mod1),
	bool__or(Mod0, Mod1, Mod).

	% Try to optimize the beginning of the given instruction sequence.
	% If successful, try it again.

:- pred opt_instr(instr, instrs, instrs, bool).
:- mode opt_instr(in, in, out, out) is det.

opt_instr(Instr0, Instrs0, Instrs, Mod) :-
	(
		match(Instr0, Instrs0, Instrs2)
	->
		( Instrs2 = [Instr2 | Instrs3] ->
			opt_instr(Instr2, Instrs3, Instrs, _)
		;
			Instrs = Instrs2
		),
		Mod = yes
	;
		Instrs = [Instr0 | Instrs0],
		Mod = no
	).

%-----------------------------------------------------------------------------%

	% Look for code patterns that can be optimized, and optimize them.

:- pred match(instr, instrs, instrs).
:- mode match(in, in, out) is semidet.

	% If a ret is followed by anything other than a label,
	% then we can delete the instruction that follows,
	% since it is unreachable.
	% This is needed for verifiability, since otherwise
	% we sometimes generate some redundant instructions
	% that the verifier can't handle, even though they are unreachable.
	%
	% Push ret past nops so we can find instructions on the other
	% side of them (but don't eliminate them because they may be
	% useful).
match(ret, Instrs0, Replacement) :-
	list__takewhile((pred(X::in) is semidet :- 
		X \= label(_)
	), Instrs0, PreLabel, NextInstrs0),
	PreLabel \= [],

	list__filter((pred(X::in) is semidet :- equivalent_to_nop(X) = yes), 
		PreLabel, KeepInstrs),
	Replacement = list__condense([KeepInstrs,
			[comment("peephole -- eliminated instrs after ret"),
			 ret],
			NextInstrs0]).

	% A branch to a label that is followed by a return can be reduced
	% to just the return.
	% NOTE: We only look for forwards branches. 

match(br(label_target(Label)), Instrs0, Instrs) :-
	list__takewhile((pred(X::in) is semidet :- 
		X \= label(Label)
	), Instrs0, _, [label(Label) | NextInstrs0]),
	skip_nops(NextInstrs0, NextInstrs, _),
	NextInstrs = [ret | _],
	Instrs = [comment("peephole -- eliminated branch to ret"), ret | 
		Instrs0].

	% stloc(X)
	% ldloc(X)
	%
	% is turned into
	%
	% dup
	% stloc(X)
	%
	% This might be slightly denser, and is easier to detect and
	% remove if it turns out the local is not used.

match(stloc(Var), Instrs0, Instrs) :-
		% The pattern
	skip_nops(Instrs0, Instrs1, Nops),
	Instrs1 = [ldloc(Var) | Rest],
		% Comment and replacement
	Comment = "peephole: stloc(X), ldloc(X) --> dup, stloc(X)",
	Replacement = list__append([dup | Nops],  [stloc(Var)]),
	Instrs = [comment(Comment) | list__append(Replacement, Rest)].

	% ldc(C)
	% stloc(X)
	% ... other instrs ... (no branching, labels, stores to X or calls)
	% ldloc(X)
	%
	% is turned into
	%
	% ... other instrs ... (no branching or labels)
	% ldc(C)
	% dup
	% stloc(X)

match(ldc(Type, Const), [stloc(Var)| Instrs0], Instrs) :-
		% The pattern
	list__takewhile((pred(X::in) is semidet :- 
		X \= ldloc(Var),
		X \= label(_),
		X \= stloc(Var),
		X \= stind(_),
		can_call(X) = no,
		can_branch(X) = no
	), Instrs0, PreLdInstrs, [ldloc(Var) | Rest]),

		% Comment and replacement
	Comment = comment(
	    "peephole: ldc(X), stloc(X), ldloc(X) --> ldc(X), dup, stloc(X)"),
	Replacement = list__append(PreLdInstrs, 
		[Comment, ldc(Type, Const), dup, stloc(Var)]),
	Instrs = list__append(Replacement, Rest).

	% Two patterns begin with start_scope.
match(start_block(scope(Locals), Id)) -->
	( 
		match_start_scope_1(start_block(scope(Locals), Id))
	->
		[]
	;	
		match_start_scope_2(start_block(scope(Locals), Id))
	).

	% If this is a scope with a local variable that is stored to but not
	% loaded anywhere, we can eliminate the stores.
	% scope([...X...]) ... dup, stloc(X)
	% becomes
	% scope([...X...]) ... <nothing>
	% This relies on other peephole optimizations to create dup,
	% stloc(X) patterns.
	% This could be more efficient if it stopped looking outside the
	% enclosing scope.
:- pred match_start_scope_1(instr, instrs, instrs).
:- mode match_start_scope_1(in, in, out) is semidet.
match_start_scope_1(start_block(scope(Locals), Id), Instrs0, Instrs) :-

		% Is this variable a local that is unused?
	IsUnusedLocal = (pred(V::in) is semidet :-
			% Var is in the locals
		V = name(VN),
		assoc_list__search(Locals, VN, _),

		% No ldloc(Var) or ldloca(Var) anywhere in the scope
		% (should only really look until the end of this scope)
		list__takewhile((pred(X::in) is semidet :- 
			X \= ldloc(V),
			X \= ldloca(V)
		), Instrs0, _, [])

	),

		% A producer, which finds "dup" and returns the rest of
		% the input, and a result.
		% The result is the preceeding input so far and the
		% remainder of the input.
		% Note that the preceeding input is a reversed list of
		% instructions (we reverse and flatten it later).
	FindDup = (pred(InstrIn::in, NextInput::out, R0::in, R::out) 
			is semidet :-
		R0 = Pre0 - _NextInput0,
		list__takewhile(
			(pred(X::in) is semidet :- X \= dup), 
			InstrIn, Pre, Post0),
		Post0 = [dup | NextInput],
		( Pre0 = [] -> 	InsertDup = []
		;		InsertDup = [dup]
		),
		R = [Pre, InsertDup | Pre0] - NextInput
	),

		% A condition, that checks the result of FindDup to see
		% whether there is a trailing stloc(V), which is an
		% unused local variable.
		% Our result is just the parts of the instruction list
		% that we are going to put together later.
	FindStloc = (pred(R0::in, R::out) is semidet :-
		R0 = Pre0 - Post0,
		Post0 = InstrIn0,
		list__takewhile(
			(pred(X::in) is semidet :- equivalent_to_nop(X) = yes), 
			InstrIn0, Pre, MaybePost),
		MaybePost = [stloc(V) | Post],
		IsUnusedLocal(V),
		R = V - Pre0 - Pre - Post
	),

	no_handwritten_code(Instrs0, Id),

		% Keep looking for "dups" until it is followed by a
		% suitable stloc.
	keep_looking(FindDup, FindStloc, Instrs0, [] - [], Result, _Left),
	Result = Var - PreStlocInstrsList - Nops - PostStlocInstrs,
	Var = name(VarName),

	PreStlocInstrs = condense(reverse(PreStlocInstrsList)),
		% Comment and replacement
	Comment = string__format(
		"peephole: dup, stloc(%s) --> nothing (%s unused in scope)",
		[s(VarName), s(VarName)]),
	Instrs = list__condense([[start_block(scope(Locals), Id)],
			PreStlocInstrs,
			Nops,
			[comment(Comment)],
			PostStlocInstrs]).

	% Any scope with a local variable that is unused may eliminate
	% it.
	% This could be more efficient if it stopped looking outside the
	% enclosing scope.
:- pred match_start_scope_2(instr, instrs, instrs).
:- mode match_start_scope_2(in, in, out) is semidet.
match_start_scope_2(start_block(scope(Locals), Id), Instrs0, Instrs) :-

	no_handwritten_code(Instrs0, Id),

		% The pattern
	list__filter((pred(VarName - _Type::in) is semidet :-
		Var = name(VarName),
			% No stloc(Var) or ldloc(Var) or ldloca(Var)
			% anywhere in the scope (should only really look
			% until the end of this scope)
		list__takewhile((pred(X::in) is semidet :- 
			X \= ldloc(Var),
			X \= ldloca(Var),
			X \= stloc(Var)
		), Instrs0, _, [])),
		Locals, UnusedLocals, UsedLocals),
	UnusedLocals \= [],


		% Comment and replacement
	list__map((pred(VarName - _Type::in, Comment::out) is det :-
		string__format(
			"peephole: unused local var %s eliminated",
			[s(VarName)], CommentStr),
		Comment = comment(CommentStr)
			), UnusedLocals, Comments),
	Replacement = [start_block(scope(UsedLocals), Id)],

	Instrs = list__condense([Comments, Replacement, Instrs0]).

	% Any scope without local variables may be eliminated.
	% XXX we don't do this yet because it would requirer finding the
	% matching end_block and removing it too.  Now that block IDs
	% are available we could actually do this, but
	% currently we don't, because the code below is incomplete.
	% This procedure is not yet called from anywhere.
:- pred match4(instr, instrs, instrs).
:- mode match4(in, in, out) is semidet.
match4(start_block(scope([]), _), Instrs0, Instrs) :-
	Replacement = [],
	Rest = Instrs0,
	Instrs = list__append(Replacement, Rest).



%-----------------------------------------------------------------------------%

	% Succeeds if there is no handwritten code within the current block.
	% (excluding sub-blocks).
:- pred no_handwritten_code(instrs::in, int::in) is semidet.

no_handwritten_code([], _).
no_handwritten_code([Instr | Instrs], Id) :-
	( Instr = il_asm_code(_, _) ->
		fail
	; Instr = end_block(_, Id) ->
		true
	; Instr = start_block(_, SkipId) ->
		InstrsAfterBlock = skip_over_block(Instrs, SkipId),
		no_handwritten_code(InstrsAfterBlock, Id)
	; 
		no_handwritten_code(Instrs, Id)
	).

	% Skips over a block until the end of the block (with Id matching
	% the given Id) is found.
:- func skip_over_block(instrs, int) = instrs.
skip_over_block([], _) = []. 
skip_over_block([Instr | Instrs], Id) = 
	( Instr = end_block(_, Id) ->
		Instrs
	;
		skip_over_block(Instrs, Id)
	).



	% Skip over all the comments.
:- pred skip_comments(instrs::in, instrs::out, instrs::out) is det.

skip_comments(Instrs0, Instrs, Comments) :-
        list__takewhile(pred(ilds__comment(_)::in) is semidet,
	                Instrs0, Comments, Instrs).

	% Skip over all the nop equivalents.
:- pred skip_nops(instrs::in, instrs::out, instrs::out) is det.

skip_nops(Instrs0, Instrs, Nops) :-
        list__takewhile((pred(X::in) is semidet :- equivalent_to_nop(X) = yes), 
		Instrs0, Nops, Instrs).



	% keep_looking(Producer, Condition, Input, IntermediateResult0, 
	%	FinalResult, Leftovers) :-
	% Producer consumes Input and produces an intermediate result
	% and the leftover input.
	% Condition checks the intermediate result and produces a final
	% result.
	% If Condition fails, we use the leftover input as the next
	% input for Producer.
	% If Producer ever fails, keep_looking fails.
	%
	% It is best to use Producer to find the start of a pattern,
	% and Condition to check that the rest of the pattern is what we
	% want.  keep_looking doesn't keep track of the part of the
	% Input that you threw away while looking for a match.  However
	% it is easy to put this part of the input in the intermediate
	% and final results.
:- pred keep_looking(pred(A, A, B, B), pred(B, C), A, B, C, A).
:- mode keep_looking(pred(in, out, in, out) is semidet, 
		pred(in, out) is semidet, in, in, out, out) is semidet.

keep_looking(Producer, Condition, Input, IntermediateResult0, 
		FinalResult, Leftovers) :-
	Producer(Input, NextInput, IntermediateResult0, IntermediateResult),
	( Condition(IntermediateResult, FinalResult0) ->
		Leftovers = NextInput,
		FinalResult = FinalResult0
	;
		keep_looking(Producer, Condition, NextInput, 
			IntermediateResult, FinalResult, Leftovers)
	).

%-----------------------------------------------------------------------------%

	% These instructions can make a call
:- func can_call(instr) = bool.
can_call(call(_)) 				= yes.
can_call(calli(_)) 				= yes.
can_call(callvirt(_)) 				= yes.
can_call(jmp(_)) 				= yes.
can_call(newobj(_))			 	= yes.
can_call(il_asm_code(_, _))		 	= yes.

can_call(comment(_Comment)) 			= no. 
can_call(label(_Label)) 			= no. 
can_call(start_block(_, _Id)) 			= no.
can_call(end_block(_, _Id)) 			= no.
can_call(context(_, _)) 			= no.
can_call(ret) 					= no. 
can_call((and)) 				= no. 
can_call(arglist) 				= no. 
can_call(break) 				= no. 
can_call(ceq) 					= no. 
can_call(ckfinite) 				= no. 
can_call(cpblk) 				= no. 
can_call(dup) 					= no.
can_call(endfilter) 				= no. 
can_call(endfinally) 				= no. 
can_call(initblk) 				= no. 
can_call(ldnull) 				= no. 
can_call(localloc)	 			= no. 
can_call(neg) 					= no. 
can_call(nop) 					= no. 
can_call((not)) 				= no. 
can_call((or)) 					= no. 
can_call(pop) 					= no. 
can_call(shl) 					= no. 
can_call(tailcall) 				= no. 
can_call(volatile) 				= no. 
can_call(xor) 					= no. 
can_call(ldlen) 				= no. 
can_call(throw)	 				= no. 
can_call(ldarg(_)) 				= no. 
can_call(ldc(_Type, _Const)) 			= no.
can_call(ldstr(_String)) 			= no. 
can_call(add(_Overflow, _Signed)) 		= no. 
can_call(beq(_Target)) 				= no.
can_call(bge(_Signed, _Target)) 		= no.
can_call(bgt(_Signed, _Target)) 		= no. 
can_call(ble(_Signed, _Target)) 		= no.
can_call(blt(_Signed, _Target)) 		= no.
can_call(bne(_Signed, _Target)) 		= no.
can_call(br(_Target)) 				= no.
can_call(brfalse(_Target)) 			= no. 
can_call(brtrue(_Target)) 			= no. 
can_call(cgt(_Signed)) 				= no.
can_call(clt(_Signed)) 				= no.
can_call(conv(_SimpleType)) 			= no. 
can_call(div(_Signed)) 				= no. 
can_call(ldarga(_Variable)) 			= no.
can_call(ldftn(_MethodRef)) 			= no. 
can_call(ldind(_SimpleType)) 			= no. 
can_call(ldloc(_Variable)) 			= no. 
can_call(ldloca(_Variable)) 			= no.
can_call(leave(_Target)) 			= no. 
can_call(mul(_Overflow, _Signed)) 		= no. 
can_call(rem(_Signed)) 				= no. 
can_call(shr(_Signed)) 				= no. 
can_call(starg(_Variable)) 			= no.
can_call(stind(_SimpleType)) 			= no. 
can_call(stloc(_Variable)) 			= no. 
can_call(sub(_OverFlow, _Signed)) 		= no. 
can_call(switch(_)) 				= no.
can_call(unaligned(_)) 				= no.
can_call(box(_Type)) 				= no. 
can_call(castclass(_Type)) 			= no.
can_call(cpobj(_Type)) 				= no. 
can_call(initobj(_Type)) 			= no. 
can_call(isinst(_Type)) 			= no. 
can_call(ldelem(_SimpleType)) 			= no. 
can_call(ldelema(_Type)) 			= no. 
can_call(ldfld(_FieldRef)) 			= no.
can_call(ldflda(_FieldRef)) 			= no.
can_call(ldobj(_Type)) 				= no.
can_call(ldsfld(_FieldRef)) 			= no. 
can_call(ldsflda(_FieldRef)) 			= no. 
can_call(ldtoken(_)) 				= no.
can_call(ldvirtftn(_MethodRef)) 		= no.
can_call(mkrefany(_Type)) 			= no.
can_call(newarr(_Type)) 			= no. 
can_call(refanytype)	 			= no. 
can_call(refanyval(_)) 				= no. 
can_call(rethrow) 				= no. 
can_call(sizeof(_Type))				= no. 
can_call(stobj(_Type)) 				= no. 
can_call(stelem(_SimpleType)) 			= no. 
can_call(stfld(_FieldRef)) 			= no. 
can_call(stsfld(_FieldRef)) 			= no. 
can_call(unbox(_Type)) 				= no.

	% These instructions generate no actual code and do not affect
	% control flow, they are simply part of instr for
	% convenience.
:- func equivalent_to_nop(instr) = bool.
equivalent_to_nop(comment(_)) 				= yes.
equivalent_to_nop(start_block(scope(_), _)) 		= yes.
equivalent_to_nop(end_block(scope(_), _))	 	= yes.
equivalent_to_nop(nop) 					= yes. 
equivalent_to_nop(context(_, _)) 			= yes. 

equivalent_to_nop(il_asm_code(_, _)) 			= no. 
equivalent_to_nop(start_block(try, _))			= no.
equivalent_to_nop(end_block(try, _))			= no.
equivalent_to_nop(start_block(catch(_), _))		= no.
equivalent_to_nop(end_block(catch(_), _))		= no.
equivalent_to_nop(label(_Label)) 			= no. 
equivalent_to_nop(call(_MethodRef)) 			= no. 
equivalent_to_nop(calli(_Signature)) 			= no.
equivalent_to_nop(callvirt(_MethodRef)) 		= no. 
equivalent_to_nop(ret) 					= no. 
equivalent_to_nop((and)) 				= no. 
equivalent_to_nop(arglist) 				= no. 
equivalent_to_nop(break) 				= no. 
equivalent_to_nop(ceq) 					= no. 
equivalent_to_nop(ckfinite) 				= no. 
equivalent_to_nop(cpblk) 				= no. 
equivalent_to_nop(dup) 					= no.
equivalent_to_nop(endfilter) 				= no. 
equivalent_to_nop(endfinally) 				= no. 
equivalent_to_nop(initblk) 				= no. 
equivalent_to_nop(ldnull) 				= no. 
equivalent_to_nop(localloc)	 			= no. 
equivalent_to_nop(neg) 					= no. 
equivalent_to_nop((not)) 				= no. 
equivalent_to_nop((or)) 				= no. 
equivalent_to_nop(pop) 					= no. 
equivalent_to_nop(shl) 					= no. 
equivalent_to_nop(tailcall) 				= no. 
equivalent_to_nop(volatile) 				= no. 
equivalent_to_nop(xor) 					= no. 
equivalent_to_nop(ldlen) 				= no. 
equivalent_to_nop(throw) 				= no. 
equivalent_to_nop(ldarg(_)) 				= no. 
equivalent_to_nop(ldc(_Type, _Const)) 			= no.
equivalent_to_nop(ldstr(_String)) 			= no. 
equivalent_to_nop(add(_Overflow, _Signed)) 		= no. 
equivalent_to_nop(beq(_Target)) 			= no.
equivalent_to_nop(bge(_Signed, _Target)) 		= no.
equivalent_to_nop(bgt(_Signed, _Target)) 		= no. 
equivalent_to_nop(ble(_Signed, _Target)) 		= no.
equivalent_to_nop(blt(_Signed, _Target)) 		= no.
equivalent_to_nop(bne(_Signed, _Target)) 		= no.
equivalent_to_nop(br(_Target)) 				= no.
equivalent_to_nop(brfalse(_Target)) 			= no. 
equivalent_to_nop(brtrue(_Target)) 			= no. 
equivalent_to_nop(cgt(_Signed)) 			= no.
equivalent_to_nop(clt(_Signed)) 			= no.
equivalent_to_nop(conv(_SimpleType)) 			= no. 
equivalent_to_nop(div(_Signed)) 			= no. 
equivalent_to_nop(jmp(_MethodRef)) 			= no. 
equivalent_to_nop(ldarga(_Variable)) 			= no.
equivalent_to_nop(ldftn(_MethodRef)) 			= no. 
equivalent_to_nop(ldind(_SimpleType)) 			= no. 
equivalent_to_nop(ldloc(_Variable)) 			= no. 
equivalent_to_nop(ldloca(_Variable)) 			= no.
equivalent_to_nop(leave(_Target)) 			= no. 
equivalent_to_nop(mul(_Overflow, _Signed)) 		= no. 
equivalent_to_nop(rem(_Signed)) 			= no. 
equivalent_to_nop(shr(_Signed)) 			= no. 
equivalent_to_nop(starg(_Variable)) 			= no.
equivalent_to_nop(stind(_SimpleType)) 			= no. 
equivalent_to_nop(stloc(_Variable)) 			= no. 
equivalent_to_nop(sub(_OverFlow, _Signed)) 		= no. 
equivalent_to_nop(switch(_)) 				= no.
equivalent_to_nop(unaligned(_)) 			= no.
equivalent_to_nop(box(_Type)) 				= no. 
equivalent_to_nop(castclass(_Type)) 			= no.
equivalent_to_nop(cpobj(_Type)) 			= no. 
equivalent_to_nop(initobj(_Type)) 			= no. 
equivalent_to_nop(isinst(_Type)) 			= no. 
equivalent_to_nop(ldelem(_SimpleType)) 			= no. 
equivalent_to_nop(ldelema(_Type)) 			= no. 
equivalent_to_nop(ldfld(_FieldRef)) 			= no.
equivalent_to_nop(ldflda(_FieldRef)) 			= no.
equivalent_to_nop(ldobj(_Type)) 			= no.
equivalent_to_nop(ldsfld(_FieldRef)) 			= no. 
equivalent_to_nop(ldsflda(_FieldRef)) 			= no. 
equivalent_to_nop(ldtoken(_)) 				= no.
equivalent_to_nop(ldvirtftn(_MethodRef)) 		= no.
equivalent_to_nop(mkrefany(_Type)) 			= no.
equivalent_to_nop(newarr(_Type)) 			= no. 
equivalent_to_nop(newobj(_MethodRef)) 			= no. 
equivalent_to_nop(refanytype)				= no.
equivalent_to_nop(refanyval(_))				= no.
equivalent_to_nop(rethrow)				= no.
equivalent_to_nop(stelem(_SimpleType)) 			= no. 
equivalent_to_nop(stfld(_FieldRef)) 			= no. 
equivalent_to_nop(sizeof(_Type))			= no. 
equivalent_to_nop(stobj(_))				= no.
equivalent_to_nop(stsfld(_FieldRef)) 			= no. 
equivalent_to_nop(unbox(_Type)) 			= no.


	% These instructions can branch control flow.
:- func can_branch(instr) = bool.

	% XXX we should refine what we mean by can_branch -- it seems to only
	% mean local branching to local labels (which il_asm_code shouldn't do)
	% but we will be conservative for now.
can_branch(il_asm_code(_, _))				= yes.
can_branch(br(_)) 					= yes.
can_branch(brtrue(_))					= yes.
can_branch(brfalse(_))					= yes.
can_branch(beq(_))					= yes.
can_branch(bge(_, _))					= yes.
can_branch(bgt(_, _))					= yes.
can_branch(ble(_, _))					= yes.
can_branch(blt(_, _))					= yes.
can_branch(bne(_, _))					= yes.
can_branch(switch(_))					= yes.

can_branch(end_block(_, _)) 				= no.
can_branch(comment(_)) 					= no.
can_branch(start_block(_, _)) 				= no.
can_branch(context(_, _)) 				= no.
can_branch(nop) 					= no. 
can_branch(label(_Label)) 				= no. 
can_branch(call(_MethodRef)) 				= no. 
can_branch(calli(_Signature)) 				= no.
can_branch(callvirt(_MethodRef)) 			= no. 
can_branch(ret) 					= no. 
can_branch((and)) 					= no. 
can_branch(arglist) 					= no. 
can_branch(break) 					= no. 
can_branch(ceq) 					= no. 
can_branch(ckfinite) 					= no. 
can_branch(cpblk) 					= no. 
can_branch(dup) 					= no.
can_branch(endfilter) 					= no. 
can_branch(endfinally) 					= no. 
can_branch(initblk) 					= no. 
can_branch(ldnull) 					= no. 
can_branch(localloc)		 			= no. 
can_branch(neg) 					= no. 
can_branch((not)) 					= no. 
can_branch((or)) 					= no. 
can_branch(pop) 					= no. 
can_branch(shl) 					= no. 
can_branch(tailcall) 					= no. 
can_branch(volatile) 					= no. 
can_branch(xor) 					= no. 
can_branch(ldlen) 					= no. 
can_branch(throw) 					= no. 
can_branch(ldarg(_)) 					= no. 
can_branch(ldc(_Type, _Const)) 				= no.
can_branch(ldstr(_String)) 				= no. 
can_branch(add(_Overflow, _Signed))	 		= no. 
can_branch(cgt(_Signed)) 				= no.
can_branch(clt(_Signed)) 				= no.
can_branch(conv(_SimpleType)) 				= no. 
can_branch(div(_Signed)) 				= no. 
can_branch(jmp(_MethodRef)) 				= no. 
can_branch(ldarga(_Variable)) 				= no.
can_branch(ldftn(_MethodRef)) 				= no. 
can_branch(ldind(_SimpleType)) 				= no. 
can_branch(ldloc(_Variable)) 				= no. 
can_branch(ldloca(_Variable)) 				= no.
can_branch(leave(_Target)) 				= no. 
can_branch(mul(_Overflow, _Signed)) 			= no. 
can_branch(rem(_Signed)) 				= no. 
can_branch(shr(_Signed)) 				= no. 
can_branch(starg(_Variable)) 				= no.
can_branch(stind(_SimpleType)) 				= no. 
can_branch(stloc(_Variable)) 				= no. 
can_branch(sub(_OverFlow, _Signed)) 			= no. 
can_branch(unaligned(_)) 				= no.
can_branch(box(_Type)) 					= no. 
can_branch(castclass(_Type)) 				= no.
can_branch(cpobj(_Type)) 				= no. 
can_branch(initobj(_Type)) 				= no. 
can_branch(isinst(_Type)) 				= no. 
can_branch(ldelem(_SimpleType)) 			= no. 
can_branch(ldelema(_Type)) 				= no. 
can_branch(ldfld(_FieldRef)) 				= no.
can_branch(ldflda(_FieldRef)) 				= no.
can_branch(ldobj(_Type)) 				= no.
can_branch(ldsfld(_FieldRef)) 				= no. 
can_branch(ldsflda(_FieldRef)) 				= no. 
can_branch(ldtoken(_)) 					= no.
can_branch(ldvirtftn(_MethodRef)) 			= no.
can_branch(mkrefany(_Type)) 				= no.
can_branch(newarr(_Type)) 				= no. 
can_branch(newobj(_MethodRef)) 				= no. 
can_branch(rethrow) 					= no. 
can_branch(refanytype) 					= no. 
can_branch(refanyval(_)) 				= no. 
can_branch(stelem(_SimpleType)) 			= no. 
can_branch(stfld(_FieldRef)) 				= no. 
can_branch(stobj(_)) 					= no. 
can_branch(sizeof(_Type))				= no. 
can_branch(stsfld(_FieldRef)) 				= no. 
can_branch(unbox(_Type)) 				= no.

