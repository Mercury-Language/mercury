% main author: petdr
:- module peephole.

:- interface.

:- import_module eval.

:- pred peephole(code::in, code::out) is det.

:- implementation.

:- import_module gml, op, vector.
:- import_module bool, float, int, list, io, map, string.
:- import_module maybe.

:- type peephole
	--->	state(
			known_ids	:: map(string, token_group)
			% used_ids	:: list(string)
		).

peephole(Tokens0, list__reverse(Tokens)) :-
	peephole_2([], list__reverse(Tokens0), Tokens1,
			state(map__init), _),
	Tokens = Tokens1.

%	peephole_2([], Tokens1, Tokens, state(map__init), _).

	% peephole_2(UsedIds, Code, OptCode)
	% 	Given a piece of code which uses all the identifers,
	% 	UsedIds, in the code located after Code generate some
	% 	optimized code, OptCode.  Note that the code is in
	% 	reverse order to the order that is executed in.
	%		
:- pred peephole_2(list(string)::in,
		code::in, code::out, peephole::in, peephole::out) is det.

peephole_2(_, [], []) --> [].
peephole_2(UsedIds0, [Token | Tokens], NewTokens) -->
	{ Token = single_token(_) },
	{ UsedIds = used_ids([Token]) `list__append` UsedIds0 },
	peephole_2(UsedIds, Tokens, NewTokens0),
	( peephole__match(UsedIds0, Token, NewTokens0, Result) ->
		{ NewTokens = Result }
	;
		{ NewTokens = [Token | NewTokens0] }
	).

peephole_2(UsedIds0, [Token | Tokens], NewTokens) -->
	{ Token = function(TokenList) },
	{ UsedIds = used_ids(TokenList) `list__append` UsedIds0 },
	peephole_2(UsedIds, Tokens, NewTokens0),

		% Process the function
	=(State),
	{ peephole_2(UsedIds0, list__reverse(TokenList), OptTokenList,
			State, _) },
	{ NewTokens = [function(list__reverse(OptTokenList)) | NewTokens0] }.

peephole_2(UsedIds0, [Token | Tokens], NewTokens) -->
	{ Token = array(TokenList) },
	{ UsedIds = used_ids(TokenList) `list__append` UsedIds0 },
	peephole_2(UsedIds, Tokens, NewTokens0),

		% Process the array
	=(State),
	{ peephole_2(UsedIds0, list__reverse(TokenList), OptTokenList,
			State, _) },
	{ NewTokens = [array(list__reverse(OptTokenList)) | NewTokens0] }.


:- pred peephole__insert(string::in, token_group::in,
		peephole::in, peephole::out) is det.

peephole__insert(Id, Token, State0, State) :-
	State = State0 ^ known_ids :=
			map__set(State0 ^ known_ids, Id, Token).

:- pred peephole__delete(string::in, peephole::in, peephole::out) is det.

peephole__delete(Id, State0, State) :-
	State = State0 ^ known_ids := map__delete(State0 ^ known_ids, Id).

:- pred peephole__search(string::in, maybe(token_group)::out,
		peephole::in, peephole::out) is det.

peephole__search(Id, MaybeToken, State, State) :-
	( map__search(State ^ known_ids, Id, Token) ->
		MaybeToken = yes(Token)
	;
		MaybeToken = no
	).

%------------------------------------------------------------------------------%

	% Determine all the identifiers that are used in a piece of
	% code.
:- func used_ids(code) = list(string).

used_ids([]) = [].
used_ids([single_token(Single) | Tokens]) =
	( Single = identifier(Id) ->
		[Id | used_ids(Tokens)]
	;
		used_ids(Tokens)
	).
used_ids([function(TokenList) | Tokens])
	= used_ids(TokenList) `list__append` used_ids(Tokens).
used_ids([array(TokenList) | Tokens])
	= used_ids(TokenList) `list__append` used_ids(Tokens).


%------------------------------------------------------------------------------%

	%
	% Currently the following optimizations are performed:
	%	- constant propogation for all the numeric functions.
	%	- if branch condition is known remove untaken branch.
	%	- replace all symbolic names with the actual token_group
	%	  when the token group is a value.
	%	- replace all unused identifers with popn(1) instructions.
	%	- merge popn(N1) popn(N2) -> popn(N1+N2)
	%	- recognise constant surface functions, and use the
	%	  constant_shape instruction.
	%	- recognise constant points and use the constant point instr
	%	- remove values constructed followed by a pop
	%	- use dup to avoid multiple lookups or binds followed
	%	  by lookups
	%	- avoid redundant lookups followed by binds or vice
	%	  versa
	%	- introduce Mercury closures for some important patterns
	%
:- pred peephole__match(list(string)::in, token_group::in, token_list::in,
		token_list::out, peephole::in, peephole::out) is semidet.


	% If you see a function followed by apply, you can just run
	% the function directly.  
	% This is turned off because we need to rename variables apart
	% before we can use it.  And 5:51am is not a good time to
	% write variable renaming code.  Come to think of it, there is
	% no good time to write variable renaming code.
peephole__match(_UsedIds, single_token(operator(apply)), Args, Result) -->
	{ semidet_fail },
	{ Args = [function(FunctionTokens) | Rest] },
	{ Result = list__append(list__reverse(FunctionTokens), Rest) }.

	% Real/Integer constant propagation.
peephole__match(_UsedIds, single_token(operator(acos)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_acos(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(addi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [integer_token(op_addi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(addf)), Args, Result) -->
	{ top_two_real_args(Args, A, B, Rest) },
	{ Result = [real_token(op_addf(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(asin)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_asin(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(clampf)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_clampf(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(cos)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_cos(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(divi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [integer_token(op_divi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(divf)), Args, Result) -->
	( 
		{ top_two_real_args(Args, A, B, Rest) }
	->
		{ Result = [real_token(op_divf(A, B)) | Rest] }
	;
		{ fail }
	).

peephole__match(_UsedIds, single_token(operator(eqi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [boolean_token(op_eqi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(eqf)), Args, Result) -->
	{ top_two_real_args(Args, A, B, Rest) },
	{ Result = [boolean_token(op_eqf(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(floor)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [integer_token(op_floor(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(frac)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_frac(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(lessi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [boolean_token(op_lessi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(lessf)), Args, Result) -->
	{ top_two_real_args(Args, A, B, Rest) },
	{ Result = [boolean_token(op_lessf(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(modi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [integer_token(op_modi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(muli)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [integer_token(op_muli(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(mulf)), Args, Result) -->
	{ top_two_real_args(Args, A, B, Rest) },
	{ Result = [real_token(op_mulf(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(negi)), Args, Result) -->
	{ Args = [integer_token(A) | Rest] },
	{ Result = [integer_token(op_negi(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(negf)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_negf(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(real)), Args, Result) -->
	{ Args = [integer_token(A) | Rest] },
	{ Result = [real_token(op_real(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(sin)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ Result = [real_token(op_sin(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(sqrt)), Args, Result) -->
	{ Args = [real_token(A) | Rest] },
	{ A >= 0.0 },
	{ Result = [real_token(op_sqrt(A)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(subi)), Args, Result) -->
	{ top_two_integer_args(Args, A, B, Rest) },
	{ Result = [integer_token(op_subi(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(subf)), Args, Result) -->
	{ top_two_real_args(Args, A, B, Rest) },
	{ Result = [real_token(op_subf(A, B)) | Rest] }.

peephole__match(_UsedIds, single_token(operator(if)), Args, Result) -->
	{
		% Branch reduction
		Args = [function(False), function(True),
			boolean_token(YesNo) | Rest]
	->
		(
			YesNo = yes,
			Result = list__reverse(True)
				`list__append` Rest
		;
			YesNo = no,
			Result = list__reverse(False)
				`list__append` Rest
		)
	;
		% If with constant args
		Args = [function(False), function(True) | Rest],
		False = [FalseConst],
		True = [TrueConst],
		constant_value(FalseConst, FalseValue),
		constant_value(TrueConst, TrueValue)
	->
		Result = [single_token(extra(
			constant_if(TrueValue, FalseValue))) | Rest]
	;
		fail
	}.

	% Removal of symbolic names, this should allow other
	% optimizations to apply.
	% Also if the identifier is unused we replace it with a popn(1)
	% instruction.
peephole__match(UsedIds, single_token(binder(Id)), Args, Result) -->

		% identifier followed by binder.
		% this can't do anything.
	( { Args = [single_token(identifier(Id)) | Rest] } ->
		{ Result = Rest }
	;
		( { list__member(Id, UsedIds) } ->
			{ Args = [_ | Rest] },
			{ chase_dups(Args, Bound) },
			( { value_token_group(Bound) } ->
				{ Result = Rest },
				peephole__insert(Id, Bound)
			;
				% Delete the binding so we don't use an old
				% binding
				{ Result = [single_token(binder(Id)) | Args] },
				peephole__delete(Id)
			)
		;
				% If the id is unused then replace it with a popn
				% instruction, and then merge popn instructions.
			{ NewToken = single_token(extra(popn(1))) },
			( peephole__match(UsedIds, NewToken, Args, Result0) ->
				{ Result = Result0 }
			;
				{ Result = [NewToken | Args] }
			)
		)
	).

peephole__match(UsedIds, single_token(identifier(Id)), Args, Result) -->
	peephole__search(Id, MaybeNewToken),
	{
		MaybeNewToken = yes(NewToken),
		Result = [NewToken | Args]
	;
		MaybeNewToken = no,
		(
			% often we have identifier(X) followed by
			% identifier(X) -- replace the duplicate with
			% a dup operation instead.
			Args = [single_token(identifier(Id)) | Rest]
		->
			Result = [
				single_token(extra(dup)),
				single_token(identifier(Id))
				| Rest]
		;
			% often we have binder(X) followed by
			% identifier(X) -- should duplicate then bind
			% instead -- unless it is unused, in which case 
			% we can just eliminate it.
			Args = [single_token(binder(Id)) | Rest]
		->
			( list__member(Id, UsedIds) ->
				Result = [
					single_token(binder(Id)),
					single_token(extra(dup))
					| Rest]
			;
				Result = Rest
			)
		;
			Result = [single_token(identifier(Id)) | Args]
		)
	}.

peephole__match(_UsedIds, single_token(extra(popn(N1))), Args, Result) -->
	( 
		% Merge popn instructions.
		{ Args = [single_token(extra(popn(N2))) | Rest] }
	->
		{ Result = [single_token(extra(popn(N1 + N2))) | Rest] }
	;
		% Remove the code used to construct the value, which is
		% popped off the stack if possible.
		{ N1 = 1 },
		{ remove_code_to_construct_one_value(Args, Rest) }
	->
		{ Result = Rest }
	;
		{ fail }
	).
	
	% Recognise constant points
peephole__match(_UsedIds, single_token(operator(point)), Args, Result) -->
	{ top_three_real_args(Args, R, G, B, Rest) },
	{ Point = point(R, G, B) },
	{ Result = [single_token(extra(constant_point(Point))) | Rest ] }.

	% constant_point get functions
peephole__match(_UsedIds, single_token(operator(getx)), Args, Result) -->
	{ Args = [single_token(extra(constant_point(Point))) | Rest] },
	{ Point = point(X, _Y, _Z) },
	{ Result = [real_token(X) | Rest ] }.
peephole__match(_UsedIds, single_token(operator(gety)), Args, Result) -->
	{ Args = [single_token(extra(constant_point(Point))) | Rest] },
	{ Point = point(_X, Y, _Z) },
	{ Result = [real_token(Y) | Rest ] }.
peephole__match(_UsedIds, single_token(operator(getz)), Args, Result) -->
	{ Args = [single_token(extra(constant_point(Point))) | Rest] },
	{ Point = point(_X, _Y, Z) },
	{ Result = [real_token(Z) | Rest ] }.


	% Constant surface functions.
peephole__match(_UsedIds, single_token(operator(sphere)), Args, Result) -->
	{ Args = [function(SurfaceFunc) | Rest] },
	{ 
		constant_surface_function(SurfaceFunc, SurfaceProperties)
	->
		ConstantObj = constant_sphere(SurfaceProperties),
		Result = [single_token(extra(ConstantObj)) | Rest ]
	;
		Result = [single_token(operator(sphere)) | Args]
	}.

peephole__match(_UsedIds, single_token(operator(plane)), Args, Result) -->
	{ Args = [function(SurfaceFunc) | Rest] },
	{ 
		constant_surface_function(SurfaceFunc, SurfaceProperties)
	->
		ConstantObj = constant_plane(SurfaceProperties),
		Result = [single_token(extra(ConstantObj)) | Rest ]
	;
		Result = [single_token(operator(plane)) | Args]
	}.

peephole__match(_UsedIds, single_token(operator(cone)), Args, Result) -->
	{ Args = [function(SurfaceFunc) | Rest] },
	{ 
		constant_surface_function(SurfaceFunc, SurfaceProperties)
	->
		ConstantObj = constant_cone(SurfaceProperties),
		Result = [single_token(extra(ConstantObj)) | Rest ]
	;
		Result = [single_token(operator(cone)) | Args]
	}.

peephole__match(_UsedIds, single_token(operator(cube)), Args, Result) -->
	{ Args = [function(SurfaceFunc) | Rest] },
	{ 
		constant_surface_function(SurfaceFunc, SurfaceProperties)
	->
		ConstantObj = constant_cube(SurfaceProperties),
		Result = [single_token(extra(ConstantObj)) | Rest ]
	;
		Result = [single_token(operator(cube)) | Args]
	}.

peephole__match(_UsedIds, single_token(operator(cylinder)), Args, Result) -->
	{ Args = [function(SurfaceFunc) | Rest] },
	{ 
		constant_surface_function(SurfaceFunc, SurfaceProperties)
	->
		ConstantObj = constant_cylinder(SurfaceProperties),
		Result = [single_token(extra(ConstantObj)) | Rest ]
	;
		Result = [single_token(operator(cylinder)) | Args]
	}.

peephole__match(_UsedIds, single_token(operator(get)), Args, Result) -->
    { semidet_fail },
    { Args = [integer_token(I), array(TokenList) | Rest] },
    { X = list__det_index0(TokenList, I) },
    { Result = [X | Rest] }.

peephole__match(_UsedIds, single_token(operator(length)), Args, Result) -->
    { semidet_fail },
    { Args = [array(TokenList) | Rest] },
    { Result = [integer_token(list__length(TokenList)) | Rest] }.


:- pred constant_surface_function(code::in, surface_properties::out) is semidet.

constant_surface_function(SurfaceFunc, SurfaceProperties) :-
	SurfaceFunc = [ single_token(extra(popn(3))),
			single_token(extra(constant_point(Point))),
			single_token(number(real(Diffuse))),
			single_token(number(real(Specular))),
			single_token(number(real(Phong)))],

	SurfaceProperties = surface_properties(Point, Diffuse, Specular, Phong).

	% Since id(N) id(N) is replaced by dup(N) id(N) we need to chase
	% to the end of the dup instructions to see whether we can store
	% the result in the state.
:- pred chase_dups(code::in, token_group::out) is semidet.

chase_dups([Token | Tokens], Bound) :-
	( Token = single_token(extra(dup)) ->
		chase_dups(Tokens, Bound)
	;
		value_token_group(Token),
		Bound = Token
		
	).
:- pred value_token_group(token_group::in) is semidet.

value_token_group(single_token(identifier(_))).
value_token_group(single_token(boolean(_))).
value_token_group(single_token(number(_))).
value_token_group(single_token(string(_))).
value_token_group(function(_)).
value_token_group(array(_)).
value_token_group(single_token(extra(constant_point(_)))).
% slows down spheres.gml
% value_token_group(single_token(extra(constant_sphere(_)))).

:- pred constant_value(token_group::in, value::out) is semidet.
constant_value(single_token(boolean(X)), boolean(X)).
constant_value(single_token(number(real(X))), real(X)).
constant_value(single_token(number(integer(X))), int(X)).
constant_value(single_token(string(X)), string(X)).
constant_value(single_token(extra(constant_point(P))), point(P)).


	% Remove code from the start of code list to construct one
	% value.  Note we assume that the code list is in reverse order.

:- pred remove_code_to_construct_one_value(code::in, code::out) is semidet.

remove_code_to_construct_one_value([function(_) | Tokens], Tokens).
remove_code_to_construct_one_value([array(_) | Tokens], Tokens).
remove_code_to_construct_one_value([single_token(Token) | Tokens0], Tokens) :-
	remove_single(Token, Tokens0, Tokens).

:- pred remove_single(token::in, code::in, code::out) is semidet.

remove_single(identifier(_), Tokens, Tokens).
remove_single(boolean(_), Tokens, Tokens).
remove_single(number(_), Tokens, Tokens).
remove_single(string(_), Tokens, Tokens).
remove_single(extra(constant_point(_)), Tokens, Tokens).
remove_single(extra(constant_sphere(_)), Tokens, Tokens).
remove_single(extra(constant_cube(_)), Tokens, Tokens).
remove_single(extra(constant_cylinder(_)), Tokens, Tokens).
remove_single(extra(constant_cone(_)), Tokens, Tokens).
% remove_single(binder(Binder), Tokens0, Tokens) :-
remove_single(operator(Op), Tokens0, Tokens) :-
	remove_op(Op, Tokens0, Tokens).

:- pred remove_op(operator::in, code::in, code::out) is semidet.

remove_op(Op, Tokens0, Tokens) :-
	args(Op, Input, MaybeOutput),
	MaybeOutput = yes(1),
	remove_n_values(Input, Tokens0, Tokens).

:- pred remove_n_values(int::in, code::in, code::out) is semidet.

remove_n_values(N, Tokens0, Tokens) :-
	( N =< 0 ->
		Tokens = Tokens0
	;
		remove_code_to_construct_one_value(Tokens0, Tokens1),
		remove_n_values(N - 1, Tokens1, Tokens)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pred top_three_real_args(token_list::in, float::out, float::out, float::out,
		token_list::out) is semidet.

top_three_real_args(Args, A, B, C, Rest) :-
	Args = [ArgC, ArgB, ArgA | Rest],
	ArgA = real_token(A),
	ArgB = real_token(B),
	ArgC = real_token(C).

:- pred top_two_real_args(token_list::in, float::out, float::out,
		token_list::out) is semidet.

top_two_real_args(Args, A, B, Rest) :-
	Args = [ArgB, ArgA | Rest],
	ArgA = real_token(A),
	ArgB = real_token(B).

:- pred top_two_integer_args(token_list::in, int::out, int::out,
		token_list::out) is semidet.

top_two_integer_args(Args, A, B, Rest) :-
	Args = [ArgB, ArgA | Rest],
	ArgA = integer_token(A),
	ArgB = integer_token(B).

:- func real_token(float) = token_group.
:- mode real_token(in) = out is det.
:- mode real_token(out) = in is semidet.

real_token(N) = single_token(number(real(N))).

:- func integer_token(int) = token_group.
:- mode integer_token(in) = out is det.
:- mode integer_token(out) = in is semidet.

integer_token(N) = single_token(number(integer(N))).

:- func boolean_token(bool) = token_group.
:- mode boolean_token(in) = out is det.
:- mode boolean_token(out) = in is semidet.

boolean_token(B) = single_token(boolean(B)).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
