%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: gcc.m
% Main author: fjh

% This module is the Mercury interface to the GCC compiler back-end.
%
% This module provides a thin wrapper around the C types,
% constants, and functions defined in gcc/tree.{c,h,def}
% and gcc/mercury/mercury-gcc.c in the GCC source.
% (The functions in gcc/mercury/mercury-gcc.c are in turn a
% thicker wrapper around the more complicated parts of GCC's
% source-language-independent back-end.)
%
% Note that we want to keep this code as simple as possible.
% Any complicated C code, which might require changes for new versions
% of gcc, should go in gcc/mercury/mercury-gcc.c rather than in
% inline C code here.  That way, the GCC developers (who know C,
% but probably don't know Mercury) can help maintain it.
% 
% For symmetry, any complicated Mercury code should probably go in
% mlds_to_gcc.m rather than here, although that is not so important.
%
% This module makes no attempt to be a *complete* interface to the
% gcc back-end; we only define interfaces to those parts of the gcc
% back-end that we need for compiling Mercury.
%
% REFERENCES
%
% For more information about the GCC compiler back-end,
% see the documentation at <http://gcc.gnu.org> and
% <http://gcc.gnu.org/readings.html>, in particular
% "Writing a Compiler Front End to GCC" by Joachim Nadler
% and Tim Josling <tej@melbpc.org.au>.
%
% Many of the procedures here which are implemented using
% stuff defined by the gcc back-end are documented better
% in the comments in the gcc source code.
%
% Many of the procedures here which are implemented using
% stuff defined by the gcc back-end are documented better
% in the comments in the gcc source code.
%
% QUOTES
%
%	``GCC is a software Vietnam.''
%		-- Simon Peyton-Jones.
%
%	``Never get involved in a land war in Asia.''
%		-- from the movie "The Princess Bride".
%

%-----------------------------------------------------------------------------%

:- module gcc.
:- interface.
:- import_module io, bool.

%-----------------------------------------------------------------------------%

% The GCC `tree' type.
:- type gcc__tree.
:- type gcc__tree_code.

%-----------------------------------------------------------------------------%
%
% Types
%

% A GCC `tree' representing a type.
:- type gcc__type.

	% Builtin types
:- func void_type_node = gcc__type.
:- func boolean_type_node = gcc__type.
:- func char_type_node = gcc__type.
:- func string_type_node = gcc__type.	% `char *'
:- func double_type_node = gcc__type.
:- func ptr_type_node = gcc__type.	% `void *'
:- func integer_type_node = gcc__type.	% C `int'.
					% (Note that we use `intptr_t' for
					% the Mercury `int' type.)
:- func int8_type_node = gcc__type.	% C99 `int8_t'
:- func int16_type_node = gcc__type.	% C99 `int16_t'
:- func int32_type_node = gcc__type.	% C99 `int32_t'
:- func int64_type_node = gcc__type.	% C99 `int64_t'
:- func intptr_type_node = gcc__type.	% C99 `intptr_t'
:- func jmpbuf_type_node = gcc__type.	% `__builtin_jmpbuf', i.e. `void *[5]'
					% This is used for `__builtin_setjmp'
					% and `__builtin_longjmp'.
	
	% Given a type `T', produce a pointer type `T *'.
:- pred build_pointer_type(gcc__type::in, gcc__type::out,
		io__state::di, io__state::uo) is det.

	% Given a type `T', and a size N, produce an array type `T[N]'.
:- pred build_array_type(gcc__type::in, int::in, gcc__type::out,
		io__state::di, io__state::uo) is det.

	% build_range_type(Type, Min, Max, RangeType):
	% Given a discrete (integer, enum, boolean, or char) type,
	% produce a new type which is the sub-range of that type
	% with low bound Min and high bound Max.
:- pred build_range_type(gcc__type::in, int::in, int::in,
		gcc__type::out, io__state::di, io__state::uo) is det.

% A GCC `tree' representing a list of parameter types.
:- type gcc__param_types.
:- func empty_param_types = gcc__param_types.
:- func cons_param_types(gcc__type, gcc__param_types) = gcc__param_types.

	% Produce a function type, given the return type and
	% the parameter types.
:- pred build_function_type(gcc__type::in, gcc__param_types::in,
		gcc__type::out, io__state::di, io__state::uo) is det.

	% Return a type that was defined in a type declaration
	% (see the section on type declarations, below).
:- func declared_type(gcc__type_decl) = gcc__type.

	% Given an array type, return the array element type.
	% This procedure must only be called with an array type,
	% otherwise it will abort.
:- pred get_array_elem_type(gcc__type::in, gcc__type::out,
		io__state::di, io__state::uo) is det.

	% Given a struct type, return the field declarations for
	% that struct.
	% This procedure must only be called with a struct type,
	% otherwise it will abort.
:- pred get_struct_field_decls(gcc__type::in, gcc__field_decls::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%
% Declarations
%

% A GCC `tree' representing a declaration.
:- type gcc__decl.

%
% Stuff for variable declarations
%

% A GCC `tree' representing a local variable.
:- type gcc__var_decl.

:- type var_name == string.

	% build an extern variable declaration
:- pred build_extern_var_decl(var_name::in, gcc__type::in, gcc__var_decl::out,
		io__state::di, io__state::uo) is det.

	% build an initialized static variable definition
	% This can be used for either global variables
	% or local static variables.
	%
	% After calling this, the caller should call set_var_decl_public
	% and/or set_var_decl_readonly, if appropriate,
	% and then finish_static_var_decl.
:- pred build_static_var_decl(var_name::in, gcc__type::in, gcc__expr::in,
		gcc__var_decl::out, io__state::di, io__state::uo) is det.

	% Finish off the definition of a static variable that
	% was begun with build_static_var_decl, above.
:- pred finish_static_var_decl(gcc__var_decl::in, io__state::di, io__state::uo)
		is det.

	% build an ordinary local variable definition
	% i.e. one with automatic (rather than static) storage duration
:- pred build_local_var_decl(var_name::in, gcc__type::in, gcc__var_decl::out,
		io__state::di, io__state::uo) is det.

	% mark a variable as being accessible from outside this
	% translation unit
:- pred set_var_decl_public(gcc__var_decl::in, io__state::di, io__state::uo) is det.

	% mark a variable as read-only
:- pred set_var_decl_readonly(gcc__var_decl::in, io__state::di, io__state::uo) is det.

%
% Routines to start/end a block.
%
% Every start_block must be matched by a corresponding end_block.
% The lifetime of any local variable declarations
% within the block will end at the corresponding end_block.
%

	% Like `{' in C.
:- pred start_block(io__state, io__state).
:- mode start_block(di, uo) is det.

	% Like `}' in C.
:- pred end_block(io__state, io__state).
:- mode end_block(di, uo) is det.

%
% Stuff for function declarations
%

% A GCC `tree' representing a function parameter.
:- type gcc__param_decl == gcc__var_decl.

	% build a function parameter declaration
:- type param_name == string.
:- pred build_param_decl(param_name::in, gcc__type::in, gcc__param_decl::out,
		io__state::di, io__state::uo) is det.

% A GCC `tree' representing a list of parameters.
:- type gcc__param_decls.

	% routines for building parameter lists
:- func empty_param_decls = gcc__param_decls.
:- func cons_param_decls(gcc__param_decl, gcc__param_decls) = gcc__param_decls.

% A GCC `tree' representing a function declaration.
:- type gcc__func_decl.

	% build a function declaration
:- type func_name == string.
:- type func_asm_name == string.
:- pred build_function_decl(func_name, func_asm_name, gcc__type,
		gcc__param_types, gcc__param_decls, gcc__func_decl,
		io__state, io__state).
:- mode build_function_decl(in, in, in, in, in, out, di, uo) is det.

	% Declarations for builtin functions.
	%
	% Note that some of these are quite Mercury-specific;
	% they are defined by C part of the Mercury front-end,
	% in gcc/mercury/mercury-gcc.c.  (XXX We might want to
	% consider moving these to a separate module, to make
	% this module more language-independent.)
:- func alloc_func_decl = gcc__func_decl.	% GC_malloc()
:- func strcmp_func_decl = gcc__func_decl.	% strcmp()
:- func hash_string_func_decl = gcc__func_decl.	% MR_hash_string()
:- func box_float_func_decl = gcc__func_decl.	% MR_box_float()
:- func setjmp_func_decl = gcc__func_decl.	% __builtin_setjmp()
:- func longjmp_func_decl = gcc__func_decl.	% __builtin_longjmp()

	% mark a function as being accessible from outside this
	% translation unit
:- pred set_func_decl_public(gcc__func_decl::in,
		io__state::di, io__state::uo) is det.

%
% Stuff for type declarations
%

	% A GCC `tree' representing a field declaration
:- type gcc__field_decl.

	% build a field declaration
:- type field_name == string.
:- pred build_field_decl(field_name::in, gcc__type::in, gcc__field_decl::out,
		io__state::di, io__state::uo) is det.

	% get the type of a field
:- pred field_type(gcc__field_decl::in, gcc__type::out,
		io__state::di, io__state::uo) is det.

	% A GCC `tree' representing a list of field declarations
:- type gcc__field_decls.

	% Construct an empty field list.
:- pred empty_field_list(gcc__field_decls, io__state, io__state).
:- mode empty_field_list(out, di, uo) is det.

	% Give a new field decl, cons it into the start of a field list.
	% Note that each field decl can only be on one field list.
:- pred cons_field_list(gcc__field_decl, gcc__field_decls, gcc__field_decls,
		io__state, io__state).
:- mode cons_field_list(in, in, out, di, uo) is det.

	% Given a non-empty field list, return the first field decl
	% and the remaining field decls.
	% This procedure must only be called with a non-empty input list,
	% otherwise it will abort.
:- pred next_field_decl(gcc__field_decls, gcc__field_decl, gcc__field_decls,
		io__state, io__state).
:- mode next_field_decl(in, out, out, di, uo) is det.

:- type gcc__type_decl.

:- type struct_name == string.
:- pred build_struct_type_decl(gcc__struct_name, gcc__field_decls,
		gcc__type_decl, io__state, io__state).
:- mode build_struct_type_decl(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% Operators
%

% GCC tree_codes for operators
% See gcc/tree.def for documentation on these.
:- type gcc__op.

:- func plus_expr  = gcc__op.		% +
:- func minus_expr = gcc__op.		% *
:- func mult_expr  = gcc__op.		% -
:- func trunc_div_expr = gcc__op.	% / (truncating integer division)
:- func trunc_mod_expr = gcc__op.	% % (remainder after truncating
					%    integer division)

:- func eq_expr = gcc__op.		% ==
:- func ne_expr = gcc__op.		% !=
:- func lt_expr = gcc__op.		% <
:- func gt_expr = gcc__op.		% >
:- func le_expr = gcc__op.		% <=
:- func ge_expr = gcc__op.		% >=

:- func truth_andif_expr = gcc__op.	% &&
:- func truth_orif_expr = gcc__op.	% ||
:- func truth_not_expr = gcc__op.	% !

:- func bit_ior_expr = gcc__op.		% | (bitwise inclusive or)
:- func bit_xor_expr = gcc__op.		% ^ (bitwise exclusive or)
:- func bit_and_expr = gcc__op.		% & (bitwise and)
:- func bit_not_expr = gcc__op.		% ~ (bitwise complement)

:- func lshift_expr = gcc__op.		% << (left shift)
:- func rshift_expr = gcc__op.		% >> (left shift)

:- func array_ref = gcc__op.		% [] (array indexing)
					% first operand is the array,
					% second operand is the index

%-----------------------------------------------------------------------------%
%
% Expressions
%

% A GCC `tree' representing an expression.
:- type gcc__expr.

	% look up the type of an expression
:- pred expr_type(gcc__expr, gcc__type, io__state, io__state).
:- mode expr_type(in, out, di, uo) is det.

%
% constants
%

	% build an expression for an integer constant
:- pred build_int(int, gcc__expr, io__state, io__state).
:- mode build_int(in, out, di, uo) is det.

	% build an expression for a floating-point constant
:- pred build_float(float, gcc__expr, io__state, io__state).
:- mode build_float(in, out, di, uo) is det.

	% build an expression for a Mercury string constant
:- pred build_string(string, gcc__expr, io__state, io__state).
:- mode build_string(in, out, di, uo) is det.

	% Build an expression for a string constant,
	% with the specified length.  This length must
	% include the terminating null, if one is desired.
:- pred build_string(int, string, gcc__expr, io__state, io__state).
:- mode build_string(in, in, out, di, uo) is det.

	% build an expression for a null pointer
:- pred build_null_pointer(gcc__expr, io__state, io__state).
:- mode build_null_pointer(out, di, uo) is det.

%
% operator expressions
%

	% build a unary expression
:- pred build_unop(gcc__op, gcc__type, gcc__expr, gcc__expr,
		io__state, io__state).
:- mode build_unop(in, in, in, out, di, uo) is det.

	% build a binary expression
:- pred build_binop(gcc__op, gcc__type, gcc__expr, gcc__expr, gcc__expr,
		io__state, io__state).
:- mode build_binop(in, in, in, in, out, di, uo) is det.

	% take the address of an expression
:- pred build_addr_expr(gcc__expr, gcc__expr, io__state, io__state).
:- mode build_addr_expr(in, out, di, uo) is det.

	% build a pointer dereference expression
:- pred build_pointer_deref(gcc__expr, gcc__expr, io__state, io__state).
:- mode build_pointer_deref(in, out, di, uo) is det.

	% build a field extraction expression
:- pred build_component_ref(gcc__expr, gcc__field_decl, gcc__expr,
		io__state, io__state).
:- mode build_component_ref(in, in, out, di, uo) is det.

	% build a type conversion expression
:- pred convert_type(gcc__expr, gcc__type, gcc__expr, io__state, io__state).
:- mode convert_type(in, in, out, di, uo) is det.

%
% variables
%

	% build an expression for a variable
:- func var_expr(gcc__var_decl) = gcc__expr.

%
% stuff for function calls
%

	% build a function pointer expression
	% i.e. take the address of a function
:- pred build_func_addr_expr(gcc__func_decl, gcc__expr, io__state, io__state).
:- mode build_func_addr_expr(in, out, di, uo) is det.

	% A GCC `tree' representing a list of arguments.
:- type gcc__arg_list.

:- pred empty_arg_list(gcc__arg_list, io__state, io__state).
:- mode empty_arg_list(out, di, uo) is det.

:- pred cons_arg_list(gcc__expr, gcc__arg_list, gcc__arg_list, io__state, io__state).
:- mode cons_arg_list(in, in, out, di, uo) is det.

	% build an expression for a function call
:- pred build_call_expr(gcc__expr, gcc__arg_list, bool, gcc__expr,
		io__state, io__state).
:- mode build_call_expr(in, in, in, out, di, uo) is det.

%
% Initializers
%

	% A GCC `tree' representing an array index or field to initialize.
:- type gcc__init_elem.

	% Create a gcc__init_elem that represents an initializer
	% for an array element at the given array index.
:- pred array_elem_initializer(int, gcc__init_elem, io__state, io__state).
:- mode array_elem_initializer(in, out, di, uo) is det.

	% Create a gcc__init_elem that represents an initializer
	% for the given field of a structure.
:- pred struct_field_initializer(gcc__field_decl, gcc__init_elem,
		io__state, io__state).
:- mode struct_field_initializer(in, out, di, uo) is det.

	% A GCC `tree' representing a list of initializers
	% for an array or structure.
:- type gcc__init_list.

:- pred empty_init_list(gcc__init_list, io__state, io__state).
:- mode empty_init_list(out, di, uo) is det.

:- pred cons_init_list(gcc__init_elem, gcc__expr, gcc__init_list,
		gcc__init_list, io__state, io__state).
:- mode cons_init_list(in, in, in, out, di, uo) is det.

	% build an expression for an array or structure initializer
:- pred build_initializer_expr(gcc__init_list, gcc__type, gcc__expr,
		io__state, io__state).
:- mode build_initializer_expr(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% Functions
%

	% start generating code for a function
:- pred start_function(gcc__func_decl, io__state, io__state).
:- mode start_function(in, di, uo) is det.

	% finish generating code for a function
:- pred end_function(io__state, io__state).
:- mode end_function(di, uo) is det.

	% set_context(Filename, LineNumber):
	%	Set the source location that GCC uses for subsequent
	%	declarations and diagnostics.  This should be called
	%	before `start_function' and also before `end_function'.
:- pred set_context(string, int, io__state, io__state).
:- mode set_context(in, in, di, uo) is det.

	% gen_line_note(FileName, LineNumber):
	%	Generate a marker indicating the source location.
	%	This should be called before generating each statement.
:- pred gen_line_note(string, int, io__state, io__state).
:- mode gen_line_note(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%
% Statements
%

%
% routines to generate code for if-then-elses
%

	% start generating code for an if-then-else
	% the argument is the gcc tree for the condition
:- pred gen_start_cond(gcc__expr, io__state, io__state).
:- mode gen_start_cond(in, di, uo) is det.

	% start the else part (optional)
:- pred gen_start_else(io__state, io__state).
:- mode gen_start_else(di, uo) is det.

	% finish the if-then-else
:- pred gen_end_cond(io__state, io__state).
:- mode gen_end_cond(di, uo) is det.

%
% routines to generate code for switches
%

:- pred gen_start_switch(gcc__expr, gcc__type, io__state, io__state).
:- mode gen_start_switch(in, in, di, uo) is det.

:- pred gen_case_label(gcc__expr, gcc__label, io__state, io__state).
:- mode gen_case_label(in, in, di, uo) is det.

:- pred gen_default_case_label(gcc__label, io__state, io__state).
:- mode gen_default_case_label(in, di, uo) is det.

:- pred gen_break(io__state, io__state).
:- mode gen_break(di, uo) is det.

:- pred gen_end_switch(gcc__expr, io__state, io__state).
:- mode gen_end_switch(in, di, uo) is det.

%
% routines to generate code for loops
%

:- type gcc__loop.

:- pred gen_start_loop(gcc__loop, io__state, io__state).
:- mode gen_start_loop(out, di, uo) is det.

:- pred gen_exit_loop_if_false(gcc__loop, gcc__expr, io__state, io__state).
:- mode gen_exit_loop_if_false(in, in, di, uo) is det.

:- pred gen_end_loop(io__state, io__state).
:- mode gen_end_loop(di, uo) is det.

%
% routines to generate code for calls/returns
%

	% generate code for an expression with side effects
	% (e.g. a call)
:- pred gen_expr_stmt(gcc__expr, io__state, io__state).
:- mode gen_expr_stmt(in, di, uo) is det.

	% generate code for a return statement
:- pred gen_return(gcc__expr, io__state, io__state).
:- mode gen_return(in, di, uo) is det.

%
% assignment
%

	% gen_assign(LHS, RHS):
	% generate code for an assignment statement
:- pred gen_assign(gcc__expr, gcc__expr, io__state, io__state).
:- mode gen_assign(in, in, di, uo) is det.

%
% labels and goto
%

:- type gcc__label.
:- type gcc__label_name == string.

	% Build a gcc tree node for a label.
	% Note that you also need to use gen_label
	% (or gen_case_label) to define the label.
:- pred build_label(gcc__label_name, gcc__label, io__state, io__state).
:- mode build_label(in, out, di, uo) is det.

:- pred build_unnamed_label(gcc__label, io__state, io__state).
:- mode build_unnamed_label(out, di, uo) is det.

:- pred gen_label(gcc__label, io__state, io__state).
:- mode gen_label(in, di, uo) is det.

:- pred gen_goto(gcc__label, io__state, io__state).
:- mode gen_goto(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, string.

:- pragma c_header_code("

#ifndef MC_GUARD_GCC_HEADERS
#define MC_GUARD_GCC_HEADERS

#include ""gcc/config.h""
#include ""gcc/system.h""
#include ""gcc/gansidecl.h""
#include ""gcc/toplev.h""
#include ""gcc/tree.h""
/* XXX we should eliminate the dependency on the C front-end */
#include ""gcc/c-tree.h""

#include ""gcc/mercury/mercury-gcc.h""

#endif

").


:- type gcc__tree ---> gcc__tree(c_pointer).
:- type gcc__tree_code == int.

%-----------------------------------------------------------------------------%
%
% Types
%

:- type gcc__type == gcc__tree.

:- type gcc__func_decl == gcc__type.

:- pragma c_code(void_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) void_type_node;
").
:- pragma c_code(boolean_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) boolean_type_node;
").
:- pragma c_code(char_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) char_type_node;
").
:- pragma c_code(string_type_node = (Type::out), [will_not_call_mercury], "
	/*
	** XXX we should consider using const when appropriate,
	** i.e. when the string doesn't have a unique mode
	*/
	Type = (MR_Word) string_type_node;
").
:- pragma c_code(double_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) double_type_node;
").
:- pragma c_code(ptr_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) ptr_type_node;
").
:- pragma c_code(integer_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) integer_type_node;
").
:- pragma c_code(int8_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_int8_type_node;
").
:- pragma c_code(int16_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_int16_type_node;
").
:- pragma c_code(int32_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_int32_type_node;
").
:- pragma c_code(int64_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_int64_type_node;
").
:- pragma c_code(intptr_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_intptr_type_node;
").
:- pragma c_code(jmpbuf_type_node = (Type::out), [will_not_call_mercury], "
	Type = (MR_Word) merc_jmpbuf_type_node;
").

:- pragma c_code(build_pointer_type(Type::in, PtrType::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	PtrType = (MR_Word) build_pointer_type((tree) Type);
").

:- pragma c_code(build_array_type(ElemType::in, NumElems::in, ArrayType::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	/* XXX Move this code to `mercury-gcc.c'. */
	/* XXX Do we need to check that NumElems fits in a HOST_WIDE_INT?  */
	HOST_WIDE_INT max = (HOST_WIDE_INT) NumElems - (HOST_WIDE_INT) 1;
	tree index_type = build_index_type (build_int_2 (max, 
		(max < 0 ? -1 : 0)));
	ArrayType = (MR_Word) build_array_type((tree) ElemType, index_type);
").

:- pragma c_code(build_range_type(Type::in, Min::in, Max::in, RangeType::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	RangeType = (MR_Word) build_range_type((tree) Type,
			build_int_2 (Min, (Min < 0 ? -1 : 0)),
			build_int_2 (Max, (Max < 0 ? -1 : 0)));
").

:- type gcc__param_types == gcc__tree.

:- pragma c_code(empty_param_types = (ParamTypes::out), [will_not_call_mercury],
"
	ParamTypes = (MR_Word) merc_empty_param_type_list();
").

:- pragma c_code(cons_param_types(Type::in, Types0::in) = (Types::out),
		[will_not_call_mercury],
"
	Types = (MR_Word)
		merc_cons_param_type_list((tree) Type, (tree) Types0);
").

:- pragma c_code(build_function_type(RetType::in, ParamTypes::in,
	FunctionType::out, _IO0::di, _IO::uo), [will_not_call_mercury],
"
	FunctionType = (MR_Word) build_function_type((tree) RetType,
		(tree) ParamTypes);
").

:- pragma c_code(declared_type(TypeDecl::in) = (Type::out),
	[will_not_call_mercury],
"
	Type = (MR_Word) TREE_TYPE((tree) TypeDecl);
").

:- pragma c_code(get_array_elem_type(ArrayType::in, ElemType::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	ElemType = (MR_Word) TREE_TYPE((tree) ArrayType);
").

:- pragma c_code(get_struct_field_decls(StructType::in, FieldDecls::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	FieldDecls = (MR_Word) TYPE_FIELDS((tree) StructType);
").

%-----------------------------------------------------------------------------%
%
% Declarations
%

%
% Stuff for variable declarations
%

:- type gcc__var_decl == gcc__tree.

:- pragma c_code(build_extern_var_decl(Name::in, Type::in, Decl::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_extern_var_decl(Name, (tree) Type);
").

:- pragma c_code(build_static_var_decl(Name::in, Type::in, Init::in, Decl::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_static_var_decl(Name, (tree) Type,
		(tree) Init);
").

:- pragma c_code(finish_static_var_decl(Decl::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_finish_static_var_decl((tree) Decl);
").

:- pragma c_code(build_local_var_decl(Name::in, Type::in, Decl::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_local_var_decl(Name, (tree) Type);
").

:- pragma c_code(set_var_decl_public(Decl::in,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	TREE_PUBLIC((tree) Decl) = 1;
").

:- pragma c_code(set_var_decl_readonly(Decl::in,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	TREE_READONLY((tree) Decl) = 1;
").

%
% Stuff for function declarations
%

:- type gcc__param_decls == gcc__tree.

:- pragma c_code(build_param_decl(Name::in, Type::in, Decl::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_param_decl(Name, (tree) Type);
").

:- pragma c_code(empty_param_decls = (Decl::out), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_empty_param_list();
").

:- pragma c_code(cons_param_decls(Decl::in, Decls0::in) = (Decls::out),
		[will_not_call_mercury],
"
	Decls = (MR_Word) merc_cons_param_list((tree) Decl, (tree) Decls0);
").

:- pragma c_code(build_function_decl(Name::in, AsmName::in,
	RetType::in, ParamTypes::in, Params::in, Decl::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_function_decl(Name, AsmName,
			(tree) RetType, (tree) ParamTypes, (tree) Params);
").

:- pragma c_code(alloc_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_alloc_function_node;
").

:- pragma c_code(strcmp_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_strcmp_function_node;
").

:- pragma c_code(hash_string_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_hash_string_function_node;
").

:- pragma c_code(box_float_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_box_float_function_node;
").

:- pragma c_code(setjmp_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_setjmp_function_node;
").

:- pragma c_code(longjmp_func_decl = (Decl::out),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_longjmp_function_node;
").

:- pragma c_code(set_func_decl_public(Decl::in,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	TREE_PUBLIC((tree) Decl) = 1;
").

%
% Stuff for type declarations.
%

:- type gcc__field_decl == gcc__tree.

:- pragma c_code(build_field_decl(Name::in, Type::in, Decl::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_field_decl(Name, (tree) Type);
").

:- pragma c_code(field_type(Decl::in, Type::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Type = (MR_Word) TREE_TYPE((tree) Decl);
").

:- type gcc__field_decls == gcc__tree.

:- pragma c_code(empty_field_list(Decl::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_empty_field_list();
").

:- pragma c_code(cons_field_list(Decl::in, Decls0::in, Decls::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Decls = (MR_Word) merc_cons_field_list((tree) Decl, (tree) Decls0);
").

:- pragma c_code(next_field_decl(Decls::in, Decl::out, RemainingDecls::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	assert((tree) Decls != NULL_TREE);
	Decl = (MR_Word) (tree) Decls;
	RemainingDecls = (MR_Word) TREE_CHAIN((tree) Decls);
").

:- type gcc__type_decl == gcc__tree.

:- pragma c_code(build_struct_type_decl(Name::in, FieldTypes::in, Decl::out,
	_IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Decl = (MR_Word) merc_build_struct_type_decl(Name, (tree) FieldTypes);
").

%-----------------------------------------------------------------------------%
%
% Operators
%

:- type gcc__op == gcc__tree_code.

:- pragma c_code(plus_expr = (Code::out), [will_not_call_mercury], "
	Code = PLUS_EXPR;
").
:- pragma c_code(minus_expr = (Code::out), [will_not_call_mercury], "
	Code = MINUS_EXPR;
").
:- pragma c_code(mult_expr = (Code::out), [will_not_call_mercury], "
	Code = MULT_EXPR;
").
:- pragma c_code(trunc_div_expr = (Code::out), [will_not_call_mercury], "
	Code = TRUNC_DIV_EXPR;
").
:- pragma c_code(trunc_mod_expr = (Code::out), [will_not_call_mercury], "
	Code = TRUNC_MOD_EXPR;
").

:- pragma c_code(eq_expr = (Code::out), [will_not_call_mercury], "
	Code = EQ_EXPR;
").
:- pragma c_code(ne_expr = (Code::out), [will_not_call_mercury], "
	Code = NE_EXPR;
").
:- pragma c_code(lt_expr = (Code::out), [will_not_call_mercury], "
	Code = LT_EXPR;
").
:- pragma c_code(gt_expr = (Code::out), [will_not_call_mercury], "
	Code = GT_EXPR;
").
:- pragma c_code(le_expr = (Code::out), [will_not_call_mercury], "
	Code = LE_EXPR;
").
:- pragma c_code(ge_expr = (Code::out), [will_not_call_mercury], "
	Code = GE_EXPR;
").

:- pragma c_code(truth_andif_expr = (Code::out), [will_not_call_mercury], "
	Code = TRUTH_ANDIF_EXPR;
").
:- pragma c_code(truth_orif_expr = (Code::out), [will_not_call_mercury], "
	Code = TRUTH_ORIF_EXPR;
").
:- pragma c_code(truth_not_expr = (Code::out), [will_not_call_mercury], "
	Code = TRUTH_NOT_EXPR;
").

:- pragma c_code(bit_ior_expr = (Code::out), [will_not_call_mercury], "
	Code = BIT_IOR_EXPR;
").
:- pragma c_code(bit_xor_expr = (Code::out), [will_not_call_mercury], "
	Code = BIT_XOR_EXPR;
").
:- pragma c_code(bit_and_expr = (Code::out), [will_not_call_mercury], "
	Code = BIT_AND_EXPR;
").
:- pragma c_code(bit_not_expr = (Code::out), [will_not_call_mercury], "
	Code = BIT_NOT_EXPR;
").

:- pragma c_code(lshift_expr = (Code::out), [will_not_call_mercury], "
	Code = LSHIFT_EXPR;
").
:- pragma c_code(rshift_expr = (Code::out), [will_not_call_mercury], "
	Code = RSHIFT_EXPR;
").

:- pragma c_code(array_ref = (Code::out), [will_not_call_mercury], "
	Code = ARRAY_REF;
").

%-----------------------------------------------------------------------------%
%
% Expressions
%

:- type gcc__expr == gcc__tree.

:- pragma c_code(expr_type(Expr::in, Type::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Type = (MR_Word) TREE_TYPE((tree) Expr);
").

%
% constants
%

build_int(Val, IntExpr) -->
	{ Lowpart = Val },
	{ Highpart = (if Val < 0 then -1 else 0) },
	build_int_2(Lowpart, Highpart, IntExpr).

	% build_int_2(Lowpart, Highpart):
	% build an expression for an integer constant.
	% Lowpart gives the low word, and Highpart gives the high word.
:- pred build_int_2(int, int, gcc__expr, io__state, io__state).
:- mode build_int_2(in, in, out, di, uo) is det.

:- pragma c_code(build_int_2(Low::in, High::in, Expr::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Expr = (MR_Word) build_int_2(Low, High);
").

build_float(Val, Expr) -->
	build_real(gcc__double_type_node, Val, Expr).

	% build an expression for a floating-point constant
	% of the specified type.
:- pred build_real(gcc__type, float, gcc__expr, io__state, io__state).
:- mode build_real(in, in, out, di, uo) is det.

:- pragma c_code(build_real(Type::in, Value::in, Expr::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	/* XXX should move to mercury-gcc.c */
	/* XXX this won't work if cross-compiling */
	union { double dbl; HOST_WIDE_INT ints[20]; } u;
	u.dbl = Value;
	Expr = (MR_Word) build_real((tree) Type,
		REAL_VALUE_FROM_TARGET_DOUBLE(u.ints));
").

build_string(String, Expr) -->
	build_string(string__length(String) + 1, String, Expr).

:- pragma c_code(build_string(Len::in, String::in, Expr::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Expr = (MR_Word) merc_build_string(Len, String);
").

:- pragma c_code(build_null_pointer(NullPointerExpr::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	NullPointerExpr = (MR_Word) null_pointer_node;
").

%
% operator expressions
%

:- pragma c_code(build_unop(Op::in, Type::in, Arg::in, Expr::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Expr = (MR_Word) fold(build1(Op, (tree) Type, (tree) Arg));
").

:- pragma c_code(build_binop(Op::in, Type::in, Arg1::in, Arg2::in, Expr::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	Expr = (MR_Word) fold(build(Op, (tree) Type, (tree) Arg1, (tree) Arg2));
").

:- pragma c_code(build_pointer_deref(Pointer::in, DerefExpr::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	/* XXX should move to mercury-gcc.c */
	tree ptr = (tree) Pointer;
	tree ptr_type = TREE_TYPE (ptr);
	tree type = TREE_TYPE (ptr_type);
	DerefExpr = (MR_Word) build1 (INDIRECT_REF, type, ptr);
").

:- pragma c_code(build_component_ref(ObjectExpr::in, FieldDecl::in,
	FieldExpr::out, _IO0::di, _IO::uo), [will_not_call_mercury],
"
	/* XXX should move to mercury-gcc.c */
	tree field_type = TREE_TYPE ((tree) FieldDecl);
	FieldExpr = (MR_Word) build (COMPONENT_REF, field_type,
		(tree) ObjectExpr, (tree) FieldDecl);
").

:- pragma c_code(convert_type(Expr::in, Type::in, ResultExpr::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	/*
	** XXX should we use convert() instead?
	** if not, should we expose the CONVERT_EXPR gcc__op
	** and just use gcc__build_binop?
	*/
	ResultExpr = (MR_Word) build1 (CONVERT_EXPR, (tree) Type, (tree) Expr);
").

	% We building an address expression, we need to call
	% mark_addressable to let the gcc back-end know that we've
	% taken the address of this expression, so that (e.g.)
	% if the expression is a variable, then gcc will know to
	% put it in a stack slot rather than a register.
	% To make the interface to this module safer,
	% we don't export the `addr_expr' operator directly.
	% Instead, we only export the procedure `build_addr_expr'
	% which includes the necessary call to mark_addressable.

build_addr_expr(Expr, AddrExpr) -->
	mark_addressable(Expr),
	expr_type(Expr, Type),
	build_pointer_type(Type, PtrType),
	build_unop(addr_expr, PtrType, Expr, AddrExpr).

:- func addr_expr = gcc__op.		% & (address-of)
:- pragma c_code(addr_expr = (Code::out), [will_not_call_mercury], "
	Code = ADDR_EXPR;
").

:- pred mark_addressable(gcc__expr::in, io__state::di, io__state::uo) is det.
:- pragma c_code(mark_addressable(Expr::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	mark_addressable((tree) Expr);
").

%
% variables
%


	% GCC represents variable expressions just by (the pointer to)
	% their declaration tree node.
var_expr(Decl) = Decl.

%
% stuff for function calls
%

	% GCC represents functions pointer expressions just as ordinary
	% ADDR_EXPR nodes whose operand is the function declaration tree node.
build_func_addr_expr(FuncDecl, Expr) -->
	build_addr_expr(FuncDecl, Expr).

:- type gcc__arg_list == gcc__tree.

:- pragma c_code(empty_arg_list(ArgList::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	ArgList = (MR_Word) merc_empty_arg_list();
").

:- pragma c_code(cons_arg_list(Arg::in, ArgList0::in, ArgList::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	ArgList = (MR_Word)
		merc_cons_arg_list((tree) Arg, (tree) ArgList0);
").

:- pragma c_code(build_call_expr(Func::in, Args::in, IsTailCall::in,
	CallExpr::out, _IO0::di, _IO::uo), [will_not_call_mercury],
"
	CallExpr = (MR_Word) merc_build_call_expr((tree) Func, (tree) Args,
		(int) IsTailCall);
").

%
% Initializers
%

:- type gcc__init_elem == gcc__tree.

gcc__array_elem_initializer(Int, GCC_Int) -->
	build_int(Int, GCC_Int).

gcc__struct_field_initializer(FieldDecl, FieldDecl) --> [].

:- type gcc__init_list == gcc__tree.

:- pragma c_code(empty_init_list(InitList::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	InitList = (MR_Word) merc_empty_init_list();
").

:- pragma c_code(cons_init_list(Elem::in, Init::in, InitList0::in, InitList::out,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	InitList = (MR_Word)
		merc_cons_init_list((tree) Elem, (tree) Init, (tree) InitList0);
").

:- pragma c_code(build_initializer_expr(InitList::in, Type::in,
	Expr::out, _IO0::di, _IO::uo), [will_not_call_mercury],
"
	Expr = (MR_Word) build(CONSTRUCTOR, (tree) Type, NULL_TREE,
		(tree) InitList);
").

%-----------------------------------------------------------------------------%
%
% Functions
%

:- pragma c_code(start_function(FuncDecl::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_start_function((tree) FuncDecl);
").

:- pragma import(end_function(di, uo), [will_not_call_mercury],
	"merc_end_function").

:- pragma c_code(set_context(FileName::in, LineNumber::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_set_context(FileName, LineNumber);
").

:- pragma c_code(gen_line_note(FileName::in, LineNumber::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	emit_line_note(FileName, LineNumber);
").

%-----------------------------------------------------------------------------%
%
% Statements.
%

%
% blocks
%

:- pragma c_code(start_block(_IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	pushlevel(0);
	expand_start_bindings(0);
").

:- pragma c_code(end_block(_IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	tree block = poplevel(/*keep=*/1, /*reverse=*/1, /*functionbody=*/0);
	expand_end_bindings(block, /*mark_ends=*/1, /*dont_jump_in=*/0);
").

%
% if-then-else
%

:- pragma c_code(gen_start_cond(Cond::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_start_cond((tree) Cond, 0);
").

:- pragma import(gen_start_else(di, uo), [will_not_call_mercury],
	"expand_start_else").

:- pragma import(gen_end_cond(di, uo), [will_not_call_mercury],
	"expand_end_cond").

%
% switch statements
%

:- pragma c_code(gen_start_switch(Expr::in, Type::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_start_case(1, (tree) Expr, (tree) Type, ""switch"");
").

:- pragma c_code(gen_case_label(Value::in, Label::in,
	_IO0::di, _IO::uo), [will_not_call_mercury],
"
	merc_gen_switch_case_label((tree) Value, (tree) Label);
").

:- pragma c_code(gen_default_case_label(Label::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_gen_switch_case_label(NULL_TREE, (tree) Label);
").

:- pragma c_code(gen_break(_IO0::di, _IO::uo), [will_not_call_mercury],
"
	int result = expand_exit_something();
	assert(result != 0);
").

:- pragma c_code(gen_end_switch(Expr::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_end_case((tree) Expr);
").

%
% loops
%

	% the type `gcc__loop' corresponds to the
	% C type `struct nesting *'
:- type gcc__loop ---> gcc__loop(c_pointer).

:- pragma c_code(gen_start_loop(Loop::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Loop = (MR_Word) expand_start_loop(0);
").

:- pragma c_code(gen_exit_loop_if_false(Loop::in, Expr::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	int res = expand_exit_loop_if_false((struct nesting *) Loop,
			(tree) Expr);
	assert(res != 0);
").

:- pragma c_code(gen_end_loop(_IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_end_loop();
").

%
% calls and return
%

:- pragma c_code(gen_expr_stmt(Expr::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_gen_expr_stmt((tree) Expr);
").

:- pragma c_code(gen_return(Expr::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_gen_return((tree) Expr);
").

%
% assignment
%

:- pragma c_code(gen_assign(LHS::in, RHS::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	merc_gen_assign((tree) LHS, (tree) RHS);
").

%
% labels and gotos
%

:- type gcc__label == gcc__tree.

:- pragma c_code(build_label(Name::in, Label::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Label = (MR_Word) merc_build_label(Name);
").

:- pragma c_code(build_unnamed_label(Label::out, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	Label = (MR_Word) merc_build_label(NULL);
").

:- pragma c_code(gen_label(Label::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_label((tree) Label);
").

:- pragma c_code(gen_goto(Label::in, _IO0::di, _IO::uo),
	[will_not_call_mercury],
"
	expand_goto((tree) Label);
").

%-----------------------------------------------------------------------------%
