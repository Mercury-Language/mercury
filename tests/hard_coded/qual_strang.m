%
%  A module similar to string, with only a string__format exported function.
%  Prints everything out in uppercase.

:- module qual_strang.

:- interface.
:- import_module list, string.

:- pred string__format(string, list(string__poly_type), string).
:- mode string__format(in, in, out) is det.

:- func string__format_func(string, list(string__poly_type)) = string.
:- mode string__format_func(in, in) = out is det.

:- implementation.

:- import_module std_util, char, int, float, require, bool.

string__format_func(FString, PolyList) = String :-
	qual_strang:string__format(FString, PolyList, String).

string__format( Fstring, Poly_list, Ostring ) :-
	string__to_char_list(Fstring, Clist),
	string__format_2q(Clist, Poly_list, Ostring) .

:- pred	string__format_2q(list(char), list(string__poly_type), string).
:- mode string__format_2q(in, in, out) is det.
%
%	string__format_2q( stream, char_f, vars, IO, IO).
%		The main function, with different input types.
%
%	Accumulator recursion is not used, as it would involve adding a
%	short string to the end of a long string many times, which I understand
%	is not efficient.  Instead, many short strings are added to the front
%	of a (long) and growing string.
%
string__format_2q( [], _, "").
string__format_2q( [Achar|As], Vars_in, Ostring) :-
	(
		Achar = '%'
	->	
		( 
			As = ['%' | Ass]
		->
			string__format_2q(Ass, Vars_in, Temp_out),
			string__first_char(Ostring, '%', Temp_out)
		;
			(
				string__format_top_convert_variable(As, As_out,
					Vars_in, Vars_out, String_1)
			->
				string__format_2q(As_out, Vars_out, String_2),
				string__append(String_1, String_2, Ostring)
			;
				error("string__format: Too few variables.")
			)
		)
	;
		string__format_2q(As, Vars_in, Temp_out),
		string__first_char(Ostring, Bchar, Temp_out),
		char__to_upper(Achar, Bchar)
	).

:- pred string__format_top_convert_variable( list(char), list(char), 
		list(string__poly_type), list(string__poly_type), string).
:- mode string__format_top_convert_variable(in, out, in, out, out) is semidet.
%
%    string__format_top_convert_variable( formated string in, out, var in, out,
%			Out string)
%		Return a string of the formatted variable.
%
string__format_top_convert_variable(['%'|Bs], Bs, [], [], "%").
			% Above rule should not be executed... defensive rule.
string__format_top_convert_variable( F_chars_in, F_chars_out, 
			[Var_in|Vars_l_in], Vars_out, Out_string ) :-
	string__format_takewhile1( F_chars_in, [Conv_char_0|F_chars_out],
			Fmt_info),
			     %	Seperate formatting info from formatting string.
			     %	in, out, out
	string__format_get_optional_args( Fmt_info, Flags, Int_width, 
			Int_precis, Conv_modify),
			     %	Parse formatting info.
			     %	in, out, out, out, out.
	string__format_mod_conv_char( Precision, Var_in, Conv_char_1, 
			Conv_char_0),
			     %	Modify(?) conversion character.
			     %	in, in, out, in
	string__format_do_mod_char( Conv_modify, Conv_char_2, Conv_char_1),
			     %	Interperate input conversion modifiers.
			     %	in, out, in
	string__format_read_star( Vars_out, Vars_l_in, Width, Int_width, 
			Precision, Int_precis),
			     %	Do something if a precision or width was '*'
			     %  out, in, out, in, out, in
	string__format_do_conversion(Conv_char_2, Var_in, Ostring, Precision,
			Flags, Move_i0),
			     %	Actually convert a Variable to a string
			     %	in, out, in, in, out, in, in, out
	string__format_add_sign( Ostring2, Ostring, Flags, Var_in, Move_i0, 
			Move_i1),
			     %	Adds an optional '+' or ' ' to string.
			     %	out, in, in, in, in, out
	string__format_pad_width( Ostring2, Width, Flags, Out_string, Move_i1).
			     %	Ensures that the string is at least width.
			     %	in, in, in, out, in

%
%	Change conversion character.
%
%	Ideally the outer "->" symbols could be removed, the last case given
%	a guard, and the compiler accept this as det, rather than non-det.
%
:- pred string__format_mod_conv_char( int, string__poly_type, char, char).
:- mode string__format_mod_conv_char( in, in, out, in) is det.
string__format_mod_conv_char( Precision, Poly_var, Conv_c_out, Conv_c_in) :- 
	( Conv_c_in = 'i' ->
		Conv_c_out = 'd'		% %d = %i
	; 
	(Conv_c_in = 'g' ->			%g is either %e of %f
		(Poly_var = f(F) ->
			string__float_abs(F, Ft),
			int__pow(10, Precision, P),
			int__to_float(P, Pe),
			( 
				builtin_float_gt(Ft, 0.0001),
				builtin_float_gt(Pe, Ft)
			->
				Conv_c_out = 'f'
			;
				Conv_c_out = 'e'
			)
		;
			error("string__format:  %g without a f(Float).")
		)
	 ;
	(Conv_c_in = 'G' ->		%G is either %E of %f
		(Poly_var = f(F) ->
			string__float_abs(F, Ft),
			int__pow(10, Precision, P),
			int__to_float(P, Pe),
			(
				builtin_float_gt(Ft, 0.0001),
				builtin_float_gt(Pe, Ft)
			->
				Conv_c_out = 'f'
			;
				Conv_c_out = 'E'
			)
		;
			error("string__format:  %G without a f(float).")
		)
	;
		Conv_c_out = Conv_c_in
	))).

%	This function glances at the input-modification flags, only applicable
%	with a more complicated type system
%
%	Another function that would be better off as a switch.
%
:- pred string__format_do_mod_char( char, char, char).
:- mode string__format_do_mod_char( in, out, in) is det.
string__format_do_mod_char( Char_mods, C_out, C_in) :- 
	(
		Char_mods = 'h'
	->
		C_out = C_in
	;
	(	Char_mods = 'l'
	->
		C_out = C_in
	;
	(	Char_mods = 'L'
	->
		C_out = C_in
	;
		C_out = C_in
	))).

%
%	Change Width or Precision value, if '*' was spcified
%
:- pred string__format_read_star( list(string__poly_type), list(string__poly_type), int, int, int, int).
:- mode string__format_read_star( out, in, out, in, out, in) is semidet.
string__format_read_star( Polys_out, Polys_in, Width, Int_width, Precision, Int_precis) :-
	(
		string__special_precision_and_width(Int_width)
	->
		Polys_in = [ i(Width) |  Poly_temp]
	;
		Polys_in = Poly_temp,
		Int_width = Width
	),
	(
		string__special_precision_and_width(Int_precis)
	->
		Poly_temp = [ i(Precision) | Polys_out]
	;
		Polys_out = Poly_temp,
		Int_precis = Precision
	).



%
%	This function did the variable conversion to string.
%	Now done by string__do_conversion_0/6.
%
%
%	Mv_width records the length of the prefix in front of the number,
%	so that it is more easy to insert width and precision padding and 
%	optional signs, in the correct place.
%
:- pred string__format_do_conversion( char, string__poly_type, string, int, 
		list(char), int).
:- mode string__format_do_conversion( in, in, out, in, in, out)
		is det.
string__format_do_conversion( Conv_c, Poly_t, Ostring, Precision, Flags,
		Mv_width) :-
	(
		string__do_conversion_0(Conv_c, Poly_t, Tstring, Precision, 
			Flags, TMv_width)
	->
		TMv_width = Mv_width,
		Ostring = Tstring
	;
		string__do_conversion_fail(Conv_c)
	).

:- pred string__do_conversion_0(char, string__poly_type, string, int, 
		list(char), int).
:- mode string__do_conversion_0(in, in, out, in, in, out) is semidet.
string__do_conversion_0(Conv_c, Poly_t, Ostring, Precision, Flags, 
		Mv_width) :-
	(
	Conv_c = 'd',
		Poly_t = i(I),
		string__int_to_string(I, S),
		string__format_int_precision(S, Ostring, Precision, _),
		(
			I < 0
		->
			Mv_width is 1
		;
			Mv_width is 0 
		)
			
	; 
	Conv_c = 'o',
		Poly_t = i(I),
		( I = 0 ->
			S = "0",
			string__format_int_precision(S, Ostring, Precision, _),
			Pfix_len = 0
		;
			string__int_to_base_string(I, 8, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member('#', Flags) ->
				string__first_char(Ostring, '0', SS),
				Pfix_len = 1
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conv_c = 'x' ,
		Poly_t = i(I),
		( I = 0 ->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, S),
			string__format_int_precision(S, SS, Precision, _),
			(
				list__member( '#', Flags)
			->
				string__append( "0x", SS, Ostring),
				Pfix_len = 2
			;
				Ostring = SS,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conv_c = 'X',
		Poly_t = i(I),
		( I = 0 ->
			SS = "0",
			Pfix_len = 0,
			string__format_int_precision(SS, Ostring, Precision, _)
		;
			string__int_to_base_string(I, 16, Otemp),
			string__to_upper(Otemp, S),
			string__format_int_precision(S, SS, Precision, _),
			( list__member( '#', Flags) ->
				string__append( "0X", SS, Ostring),
				Pfix_len = 2
			;
				SS = Ostring,
				Pfix_len = 0
			)
		),
		( I < 0 -> Mv_width is Pfix_len + 1 ; Mv_width is Pfix_len )
	;
	Conv_c = 'u' ,
		Poly_t = i(I),
		int__abs(I, J),
		string__int_to_string(J, S),
		string__format_int_precision(S, Ostring, Precision, Mvt),
		Mv_width = Mvt
	;		
	Conv_c = 'c' ,
		Poly_t = c(C),
		string__char_to_string( C, Ostring),
		Mv_width = 0
	;
	Conv_c = 's' ,
		Poly_t = s(S),
		( string__default_precision_and_width(Precision) ->
			string__to_upper(S, T),
			T = Ostring
		;
			string__split(S, Precision, Ostring, _)
		),
		Mv_width = 0
	;
	Conv_c = 'f' ,
		Poly_t = f(F),
		string__float_to_string(F, Fstring),
		string__format_calc_prec(Fstring, Ostring, Precision),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;	
	Conv_c = 'e',
		Poly_t = f(F),
		string__format_calc_exp(F, Ostring, Precision, 0),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;
	Conv_c = 'E' ,
		Poly_t = f(F),
		string__format_calc_exp(F, Otemp, Precision, 0),
		string__to_upper(Otemp, Ostring),
		(builtin_float_lt(F, 0.0)-> Mv_width = 1 ; Mv_width = 0)
	;
	Conv_c = 'p' ,
		Poly_t = i(I),
		string__int_to_string(I, Ostring),
		((I < 0) -> Mv_width = 1 ; Mv_width = 0)
	).

:- pred string__do_conversion_fail(char).
:- mode string__do_conversion_fail(in) is erroneous.
string__do_conversion_fail(Conv_c) :-
	qual_strang:string__format("%s `%%%c', without a correct poly-variable.", 
		[s("string__format: statement has used type"), c(Conv_c)],
		Error_message),
	error(Error_message).

%
%	Use precision information to modify string.  - for integers
%
:- pred string__format_int_precision(string, string, int, int).
:- mode string__format_int_precision( in, out, in, out) is semidet.
string__format_int_precision(S, Ostring, Precision, Added_width) :-
	( string__default_precision_and_width(Precision) ->
		Prec = 0
	;
		Prec = Precision
	),
	string__length(S, L),
	( string__first_char(S, '-', _) ->
		Xzeros is Prec - L + 1
	;
		Xzeros is Prec - L
	),
	Added_width = Xzeros,
	( Xzeros > 0 ->	
		string__duplicate_char( '0', Xzeros, Pfix),
		string__first_char(S, C, Rest),
		(
			C \= ('-'),
			C \= ('+')
		->
			string__append(Pfix, S, Ostring)
		;
			string__append( Pfix, Rest, Temps),
			string__first_char( Ostring, C, Temps)
		)
	;
		Ostring = S
	).

%	Function  to calculate exponent for a %e conversion of a float
%
:- pred string__format_calc_exp(float, string, int, int).
:- mode string__format_calc_exp(in, out, in, in) is det.
string__format_calc_exp(F, Fstring, Precision, Exp) :-
	( builtin_float_lt(F, 0.0) -> 	
		builtin_float_minus( 0.0, F, Tf),
		string__format_calc_exp( Tf, Tst, Precision, Exp),
		string__first_char(Fstring, '-', Tst)
	;
		( builtin_float_lt(F, 1.0) ->
			Texp is Exp - 1,
			builtin_float_times(F, 10.0, FF),
			string__format_calc_exp( FF, Fstring, Precision, Texp)
		;
		( builtin_float_ge(F, 10.0) ->
			Texp is Exp + 1,
			builtin_float_divide(F, 10.0, FF),
			string__format_calc_exp( FF, Fstring, Precision, Texp)
		;
			string__float_to_string(F, Fs),
			string__format_calc_prec(Fs, Fs2, Precision),
			string__int_to_string(Exp, Exps),
			( Exp < 0 ->
				string__append( "e", Exps, TFstring),
				string__append( Fs2, TFstring, Fstring)
			;
				string__append( "e+", Exps, TFstring),
				string__append( Fs2, TFstring, Fstring)
			)
		)
		)
	).

	
%
%	This precision output-modification predicate handles floats.
%
:- pred string__format_calc_prec(string, string, int).
:- mode string__format_calc_prec(in, out, in) is det.
string__format_calc_prec(Istring, Ostring, Precision) :-
	(
		string__default_precision_and_width(Precision)
	->
		Prec = 6
	;
		Prec = Precision
	),
	(
		string__find_index( Istring, '.', Index)
	->
		Spa is Prec + Index
	;
		string__length(Istring, Spa_0),
		Spa is Spa_0 + 1
		%  This branch should never be called if mercury is implemented
		%  in ansi-C, according to Kernighan and Ritchie p244, as a 
		%  float converted to a string using sprintf should always have
		%  a decimal point.  (where specified precision != 0.  
		%  string__float_to_string doesn't specify a precision to be
		%  used.)  If a future implementation changes the 
		%  way string__float_to_string is implemented, and a float can
		%  be converted to a string without a decimal point, then this
		%  rule would be useful.  It is not expected that
		%  string__float_to_string will ever produce %e style output.
	),
	(
		string__length(Istring, L1),
		L1 < Spa
	->
		string__duplicate_char('0', Precision, P0s),
		string__append( Istring, P0s, Mstring)
	;
		Mstring = Istring
	),
	(
		Precision = 0
	->
		Space is Spa - 1
	;
		Space = Spa
	),
	string__split(Mstring, Space, Ostring, _).

%	string__find_index is a funky little predicate to find the first
%	occurrence of a particular character in a string.

:- pred string__find_index( string, char, int).
:- mode string__find_index( in, in, out) is semidet.
string__find_index(Str, C, Index) :-
	string__to_char_list(Str, List),
	string__find_index_2(List, C, Index).

:- pred string__find_index_2(list(char), char, int).
:- mode string__find_index_2(in, in, out) is semidet.
string__find_index_2([], _C, _Index) :- fail.
string__find_index_2([X|Xs], C, Index) :-
	(
		X = C
	->
		Index = 1
	;
		string__find_index_2(Xs, C, Index0),
		Index is Index0 + 1
	).

%string__find_index( A, Ch, Check, Ret) :-
%	(
%		string__length(A, Len),
%		Len < Check
%	->
%		fail
%	;
%		string__index(A, Check, Ch)
%	->
%		Ret = Check
%	;
%		Check2 is Check + 1,
%		string__find_index( A, Ch, Check2, Ret)
%	).
%

%	Add a '+' or ' ' sign, if it is needed in this output.
%
:- pred string__format_add_sign( string, string, list(char), string__poly_type,
			int, int).
:- mode string__format_add_sign( out, in, in, in, in, out) is det.
%			Mvw is the prefix-length in front of the number.
string__format_add_sign( Ostring, Istring, Flags, _V, Mvw1, Mvw2) :-
	T1 is Mvw1 - 1,
	(
		string__index(Istring, T1, '-')
	->
		Ostring = Istring,
		Mvw2 = Mvw1
	;
		string__split(Istring, Mvw1, Lstring, Rstring),
		(
			list__member(('+'), Flags)
		->
			string__append( "+", Rstring, Astring),
			string__append( Lstring, Astring, Ostring),
			Mvw2 is Mvw1 + 1
		;
			(
				list__member(' ', Flags)
			->
				string__append( " ", Rstring, Astring),
				string__append( Lstring, Astring, Ostring),
				Mvw2 is Mvw1 + 1
			; 
				Ostring = Istring,
				Mvw2 = Mvw1
			)
		)
	).

%
% This function pads some characters to the left or right of a string that is
% shorter than it's width.
%
:- pred string__format_pad_width( string, int, list(char), string, int).
:- mode string__format_pad_width( in, in, in, out, in) is det.
%		( String in, width, flags, Output string, #Moveables).
string__format_pad_width( Istring, Width, Flags, Out_string, Mv_cs) :-
	string__length(Istring, Len),
	(Len < Width ->
		% time for some FLAG tests
		Xspace is Width - Len,
		(
			list__member('0', Flags)
		->
			Pad_char = '0'
		;
			Pad_char = ' '
		),
		string__duplicate_char(Pad_char, Xspace, Pad_string),
		( 
			list__member('-', Flags)
		->
			string__append(Istring, Pad_string, Out_string)
		;
			(
				list__member('0', Flags)
			->
				string__split(Istring, Mv_cs, B4, After),
				string__append( Pad_string, After, Astring),
				string__append( B4, Astring, Out_string)
			;
				string__append( Pad_string, Istring, Out_string)
			)
		)
	;
		Out_string = Istring
	).

:- pred string__format_get_optional_args( list(char), list(char), int, int, char).
:- mode string__format_get_optional_args( in, out, out, out, out) is det.
%	string__format_get_optional_args( format info, flags, width, precision, modifier)
%		format is assumed to be in ANSI C format.
%		p243-4 of Kernighan & Ritchie 2nd Ed. 1988
%		"Parse" format informtion.
%
% A function to do some basic parsing on the optional printf arguments.
%
% The ites make this det.  It would be nicer to see a det switch on A, but the
% determinism checker does not `see' the equity tests that are hidden one layer
% further down.
%
string__format_get_optional_args( [], Flags, Width, Precision, Mods) :-
		Flags = [],
		Width = 0,
		string__default_precision_and_width(Precision),
		Mods = ' '.
string__format_get_optional_args( [A|As], Flags, Width, Precision, Mods) :-
	(
		(A = (-) ; A = (+) ; A = ' ' ; A = '0' ; A = '#' )
	->
		string__format_get_optional_args( As, Oflags, Width, Precision, Mods),
		UFlags = [A | Oflags],
		list__sort_and_remove_dups(UFlags, Flags)
	;
	(	
		( A = (.) ; A = '1' ; A = '2' ; A = '3' ; A = '4' ;
		  A = '5' ; A = '6' ; A = '7' ; A = '8' ; A = '9' )
	->
		string__format_string_to_ints([A|As], Bs, Numl1, Numl2, yes),
		string__format_int_from_char_list( Numl1, Width),
		string__format_int_from_char_list( Numl2, Prec),
		string__format_get_optional_args( Bs, Flags, _, Ptemp, Mods),
		(Numl2 = [] ->
			Precision = Ptemp
		;
			Precision = Prec
		)
	;
	(	( A = 'h' ; A = 'l' ; A = 'L' )
	->
		Mods = A,
		string__format_get_optional_args( As, Flags, Width, Precision, _)
	;
	(	A = ('*')
	->
		string__format_get_optional_args( As, Flags, W, P, Mods),
		(
			As = [(.)|_]
		->
			Precision = P, 
			string__special_precision_and_width(Width)
		;
			Width = W,
			string__special_precision_and_width(Precision)
		)
%		(
%			string__default_precision_and_width(P)
%		->
%			string__special_precision_and_width(Precision)
%		; 
%			Precision = P
%		),
%		string__special_precision_and_width(Width)
	;
		error("string__format:  Unrecognised formatting information\n")
		)
	))) .

:- pred string__format_takewhile1(list(char), list(char), list(char)).
:- mode string__format_takewhile1(in, out, out) is det.
%	string__format_takewhile(formatted string in, out, format info).
%		A HACK.  Would be much nicer with a proper string__takewhile.
%		Looses the format info from the front of the first argument,
%		puts this in the last argument, while the second is the
%		remainder of the string.
%
%		XXXXXX
%
string__format_takewhile1([], [], []).
string__format_takewhile1( [A|As], Rem, Finf) :-
	(
		( A = 'd' ; A = 'i' ; A = 'o' ; A = 'x' ; A = 'X' ; A = 'u' ;
		  A = 's' ; A = '%' ; A = 'c' ; A = 'f' ; A = 'e' ; A = 'E' ;
		  A = 'g' ; A = 'G' ; A = 'p')
	->	
		Rem = [A|As],
		Finf = []
	;
		string__format_takewhile1(As, Rem, F),
		Finf = [A|F]
	).
	
:- pred string__format_string_to_ints(list(char), list(char), list(char), list(char), bool).
:- mode string__format_string_to_ints(in, out, out, out, in) is det.
% 			( String in, out, Number1, Number2, seen '.' yet?)
%		Takes in a char list and splits off the rational number at the 
%		start of the list.  This is split into 2 parts - an int and a
%		fraction.
%
string__format_string_to_ints( [], [], [], [], _).
string__format_string_to_ints( [A|As], Bs, Int1, Int2, Bool) :-
	(char__is_digit(A) ->
		( Bool = yes ->
			string__format_string_to_ints( As, Bs, I1, Int2, yes),
			Int1 = [A|I1]
		;
			string__format_string_to_ints(As, Bs, Int1, I2, no),
			Int2 = [A|I2]
		)
	;
		( A = ('.') ->
			string__format_string_to_ints( As, Bs, Int1, Int2, no)
		;
			Bs = [A|As],
			Int1 = [],
			Int2 = []
		)
	).

:- pred string__format_int_from_char_list( list(char), int).
:- mode string__format_int_from_char_list( in, out) is det.
%		Convert a char_list to an int
%
string__format_int_from_char_list( [], 0).
string__format_int_from_char_list( [L|Ls], I) :-
	(
		string__from_char_list( [L|Ls], S),
		string__to_int( S, I_0)
	->
		I = I_0
	;
		I = 0
	).

:- pred string__float_abs(float, float).
:- mode string__float_abs(in, out) is det.
string__float_abs(Fin, Fout) :-
	( builtin_float_lt(Fin, 0.0) ->
		builtin_float_minus(0.0, Fin, Fout)
	;
		Fout = Fin
	).

:- pred string__default_precision_and_width(int).
:- mode string__default_precision_and_width(out) is det.
string__default_precision_and_width(-6).

:- pred string__special_precision_and_width(int).
:- mode string__special_precision_and_width(out) is det.
string__special_precision_and_width(-1).




%-----------------------------------------------------------------------------%
