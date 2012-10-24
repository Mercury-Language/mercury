%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2006, 2009-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: gcc.m
% Main author: fjh
%
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
% GARBAGE COLLECTION
%
% The GCC compiler uses its own garbage collector (see gcc/ggc.h).
% This garbage collector only collects memory when ggc_collect()
% is called, which currently only happens in rest_of_compilation(),
% which is called from gcc.end_function//0.  But it requires
% that all pointers to the GCC heap be either explicitly registered
% with e.g. ggc_register_tree_root(), or protected by calling
% gcc.push_context//0 and gcc.pop_context//0.
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
% QUOTES
%
%   ``GCC is a software Vietnam.''
%       -- Simon Peyton-Jones.
%
%   ``Never get involved in a land war in Asia.''
%       -- from the movie "The Princess Bride".
%
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module gcc.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % gcc.run_backend(CommandLine, ReturnValue, FrontEndCallBack, Output):
    %
    % This is the top-level routine that MUST be used to invoke the
    % GCC back-end.  It makes sure GCC has been initialized, using
    % the specified CommandLine for GCC's command-line parameters,
    % and then calls the specified FrontEndCallBack procedure.
    % The FrontEndCallBack should then call the appropriate
    % routines defined below (e.g. gcc.{start,end}_function,
    % gcc.gen_expr_stmt, etc.) to generate code.  When it
    % is finished, the FrontEndCallBack can return an Output.
    % gcc.run_backend will then finish generating the assembler
    % file and clean up.  Finally gcc.run_backend will return
    % the Output of the FrontEndCallBack procedure back to the
    % caller of gcc.run_backend.  ReturnValue will be the
    % return value from the GCC backend, i.e. zero if all is OK,
    % and non-zero if something went wrong.
    %
    % WARNING: The other functions and predicates defined in this
    % module MUST NOT be called directly; they can only be called
    % from the FrontEndCallBack routine passed to gcc.run_backend.
    % Otherwise the GCC back-end won't get properly initialized.
    %
    % Due to limitations in the GCC back-end, this routine must
    % not be called more than once; if it is, it will print an
    % error message and abort execution.

:- type gcc_frontend_callback(T) == pred(T, io.state, io.state).
:- inst gcc_frontend_callback == (pred(out, di, uo) is det).

:- pred gcc.run_backend(string::in, int::out,
    gcc_frontend_callback(T)::in(gcc_frontend_callback), T::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% The GCC `tree' type.
:- type gcc.tree.
:- type gcc.tree_code.

%-----------------------------------------------------------------------------%
%
% Types.
%

    % A GCC `tree' representing a type.
:- type gcc.gcc_type.

    % Builtin types
:- func void_type_node = gcc.gcc_type.
:- func boolean_type_node = gcc.gcc_type.
:- func char_type_node = gcc.gcc_type.
:- func string_type_node = gcc.gcc_type.    % `char *'
:- func double_type_node = gcc.gcc_type.
:- func ptr_type_node = gcc.gcc_type.       % `void *'
:- func integer_type_node = gcc.gcc_type.   % C `int'.
                                            % (Note that we use `intptr_t' for
                                            % the Mercury `int' type.)
:- func int8_type_node = gcc.gcc_type.      % C99 `int8_t'
:- func int16_type_node = gcc.gcc_type.     % C99 `int16_t'
:- func int32_type_node = gcc.gcc_type.     % C99 `int32_t'
:- func int64_type_node = gcc.gcc_type.     % C99 `int64_t'
:- func intptr_type_node = gcc.gcc_type.    % C99 `intptr_t'
:- func jmpbuf_type_node = gcc.gcc_type.    % `__builtin_jmpbuf', i.e.
                                            %`void *[5]'
                                            % This is used for
                                            % `__builtin_setjmp' and
                                            % `__builtin_longjmp'.

    % Given a type `T', produce a pointer type `T *'.
    %
:- pred build_pointer_type(gcc.gcc_type::in, gcc.gcc_type::out,
    io::di, io::uo) is det.

    % Given a type `T', and a size N, produce an array type `T[N]'.
    %
:- pred build_array_type(gcc.gcc_type::in, int::in, gcc.gcc_type::out,
    io::di, io::uo) is det.

    % build_range_type(Type, Min, Max, RangeType):
    % Given a discrete (integer, enum, boolean, or char) type,
    % produce a new type which is the sub-range of that type
    % with low bound Min and high bound Max.
:- pred build_range_type(gcc.gcc_type::in, int::in, int::in, gcc.gcc_type::out,
    io::di, io::uo) is det.

% A GCC `tree' representing a list of parameter types.
:- type gcc.param_types.
:- func empty_param_types = gcc.param_types.
:- func cons_param_types(gcc.gcc_type, gcc.param_types) = gcc.param_types.

    % Produce a function type, given the return type and the parameter types.
    %
:- pred build_function_type(gcc.gcc_type::in, gcc.param_types::in,
    gcc.gcc_type::out, io::di, io::uo) is det.

    % Return a type that was defined in a type declaration
    % (see the section on type declarations, below).
    %
:- func declared_type(gcc.type_decl) = gcc.gcc_type.

    % Given an array type, return the array element type.
    % This procedure must only be called with an array type,
    % otherwise it will abort.
    %
:- pred get_array_elem_type(gcc.gcc_type::in, gcc.gcc_type::out,
    io::di, io::uo) is det.

    % Given a struct type, return the field declarations for that struct.
    % This procedure must only be called with a struct type,
    % otherwise it will abort.
    %
:- pred get_struct_field_decls(gcc.gcc_type::in, gcc.field_decls::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Declarations.
%

%
% Stuff for variable declarations.
%

    % A GCC `tree' representing a local variable.
:- type gcc.var_decl.

:- type var_name == string.

    % Build an extern variable declaration.
    %
:- pred build_extern_var_decl(var_name::in, gcc.gcc_type::in,
    gcc.var_decl::out, io::di, io::uo) is det.

    % Build an initialized static variable definition. This can be used for
    % either global variables or local static variables.
    %
    % After calling this, the caller should call set_var_decl_public,
    % set_var_decl_readonly, and/or set_var_asm_name, if appropriate,
    % and then finish_static_var_decl.
    %
    % The name passed in here should be the source level name.
    % By default, this is also used as the assembler name.
    % If that name contains special characters that might confuse
    % the assembler, the caller needs call set_var_asm_name with
    % a mangled name that is free of such characters.
    %
:- pred build_static_var_decl(var_name::in, gcc.gcc_type::in, gcc.expr::in,
    gcc.var_decl::out, io::di, io::uo) is det.

    % Finish off the definition of a static variable that was begun
    % with build_static_var_decl, above.
    %
:- pred finish_static_var_decl(gcc.var_decl::in, io::di, io::uo) is det.

    % Build an ordinary local variable definition,
    % i.e. one with automatic (rather than static) storage duration.
    %
:- pred build_local_var_decl(var_name::in, gcc.gcc_type::in, gcc.var_decl::out,
    io::di, io::uo) is det.

    % Mark a variable as being accessible from outside this translation unit.
    %
:- pred set_var_decl_public(gcc.var_decl::in, io::di, io::uo) is det.

    % Mark a variable as read-only.
    %
:- pred set_var_decl_readonly(gcc.var_decl::in, io::di, io::uo) is det.

    % Set the assembler name to use for a variable.
    %
:- pred set_var_decl_asm_name(gcc.var_decl::in, gcc.var_name::in,
    io::di, io::uo) is det.

%
% Routines to start/end a block.
%
% Every start_block must be matched by a corresponding end_block.
% The lifetime of any local variable declarations
% within the block will end at the corresponding end_block.
%

    % Like `{' in C.
    %
:- pred start_block(io::di, io::uo) is det.

    % Like `}' in C.
:- pred end_block(io::di, io::uo) is det.

%
% Stuff for function declarations.
%

    % A GCC `tree' representing a function parameter.
:- type gcc.param_decl == gcc.var_decl.

    % Build a function parameter declaration.
    %
:- type param_name == string.
:- pred build_param_decl(param_name::in, gcc.gcc_type::in, gcc.param_decl::out,
    io::di, io::uo) is det.

    % A GCC `tree' representing a list of parameters.
:- type gcc.param_decls.

    % Routines for building parameter lists.
    %
:- func empty_param_decls = gcc.param_decls.
:- func cons_param_decls(gcc.param_decl, gcc.param_decls) = gcc.param_decls.

    % A GCC `tree' representing a function declaration.
:- type gcc.func_decl.

    % Build a function declaration.
    %
:- type func_name == string.
:- type func_asm_name == string.
:- pred build_function_decl(func_name::in, func_asm_name::in, gcc.gcc_type::in,
    gcc.param_types::in, gcc.param_decls::in, gcc.func_decl::out,
    io::di, io::uo) is det.

    % Declarations for builtin functions.
    %
    % Note that some of these are quite Mercury-specific;
    % they are defined by C part of the Mercury front-end,
    % in gcc/mercury/mercury-gcc.c.  (XXX We might want to
    % consider moving these to a separate module, to make
    % this module more language-independent.)
:- func alloc_func_decl = gcc.func_decl.        % GC_malloc()
:- func strcmp_func_decl = gcc.func_decl.       % strcmp()
:- func hash_string_func_decl = gcc.func_decl.  % MR_hash_string()
:- func box_float_func_decl = gcc.func_decl.    % MR_asm_box_float()
:- func setjmp_func_decl = gcc.func_decl.       % __builtin_setjmp()
:- func longjmp_func_decl = gcc.func_decl.      % __builtin_longjmp()

    % Mark a function as being accessible from outside this translation unit.
    %
:- pred set_func_decl_public(gcc.func_decl::in, io::di, io::uo) is det.

%
% Stuff for type declarations.
%

    % A GCC `tree' representing a field declaration.
:- type gcc.field_decl.

    % Build a field declaration.
    %
:- type field_name == string.
:- pred build_field_decl(field_name::in, gcc.gcc_type::in, gcc.field_decl::out,
    io::di, io::uo) is det.

    % Get the type of a field.
    %
:- pred field_type(gcc.field_decl::in, gcc.gcc_type::out,
    io::di, io::uo) is det.

    % A GCC `tree' representing a list of field declarations
:- type gcc.field_decls.

    % Construct an empty field list.
    %
:- pred empty_field_list(gcc.field_decls::out, io::di, io::uo) is det.

    % Give a new field decl, cons it into the start of a field list.
    % Note that each field decl can only be on one field list.
    %
:- pred cons_field_list(gcc.field_decl::in, gcc.field_decls::in,
    gcc.field_decls::out, io::di, io::uo) is det.

    % Given a non-empty field list, return the first field decl
    % and the remaining field decls.
    % This procedure must only be called with a non-empty input list,
    % otherwise it will abort.
    %
:- pred next_field_decl(gcc.field_decls::in, gcc.field_decl::out,
    gcc.field_decls::out, io::di, io::uo) is det.

:- type gcc.type_decl.

:- type struct_name == string.
:- pred build_struct_type_decl(gcc.struct_name::in, gcc.field_decls::in,
    gcc.type_decl::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Operators.
%

% GCC tree_codes for operators
% See gcc/tree.def for documentation on these.

:- type gcc.op.

:- func plus_expr  = gcc.op.        % +
:- func minus_expr = gcc.op.        % *
:- func mult_expr  = gcc.op.        % -
:- func rdiv_expr = gcc.op.         % / (floating-point division)
:- func trunc_div_expr = gcc.op.    % / (truncating integer division)
:- func trunc_mod_expr = gcc.op.    % % (remainder after truncating
                                    %    integer division)

:- func eq_expr = gcc.op.           % ==
:- func ne_expr = gcc.op.           % !=
:- func lt_expr = gcc.op.           % <
:- func gt_expr = gcc.op.           % >
:- func le_expr = gcc.op.           % <=
:- func ge_expr = gcc.op.           % >=

:- func truth_andif_expr = gcc.op.  % &&
:- func truth_orif_expr = gcc.op.   % ||
:- func truth_not_expr = gcc.op.    % !

:- func bit_ior_expr = gcc.op.      % | (bitwise inclusive or)
:- func bit_xor_expr = gcc.op.      % ^ (bitwise exclusive or)
:- func bit_and_expr = gcc.op.      % & (bitwise and)
:- func bit_not_expr = gcc.op.      % ~ (bitwise complement)

:- func lshift_expr = gcc.op.       % << (left shift)
:- func rshift_expr = gcc.op.       % >> (left shift)

:- func array_ref = gcc.op.         % [] (array indexing)
                                    % First operand is the array,
                                    % second operand is the index.

%-----------------------------------------------------------------------------%
%
% Expressions.
%

    % A GCC `tree' representing an expression.
:- type gcc.expr.

    % Look up the type of an expression.
    %
:- pred expr_type(gcc.expr::in, gcc.gcc_type::out, io::di, io::uo) is det.

%
% Constants.
%

    % Build an expression for an integer constant.
    %
:- pred build_int(int::in, gcc.expr::out, io::di, io::uo) is det.

    % Build an expression for a floating-point constant.
    %
:- pred build_float(float::in, gcc.expr::out, io::di, io::uo) is det.

    % Build an expression for a Mercury string constant.
    %
:- pred build_string(string::in, gcc.expr::out, io::di, io::uo) is det.

    % Build an expression for a string constant, with the specified length.
    % This length must include the terminating null, if one is desired.
    %
:- pred build_string(int::in, string::in, gcc.expr::out,
    io::di, io::uo) is det.

    % Build an expression for a null pointer.
    %
:- pred build_null_pointer(gcc.expr::out, io::di, io::uo) is det.

%
% Operator expressions.
%

    % Build a unary expression.
    %
:- pred build_unop(gcc.op::in, gcc.gcc_type::in, gcc.expr::in, gcc.expr::out,
    io::di, io::uo) is det.

    % Build a binary expression.
    %
:- pred build_binop(gcc.op::in, gcc.gcc_type::in, gcc.expr::in, gcc.expr::in,
    gcc.expr::out, io::di, io::uo) is det.

    % Take the address of an expression.
    %
:- pred build_addr_expr(gcc.expr::in, gcc.expr::out, io::di, io::uo) is det.

    % Build a pointer dereference expression.
    %
:- pred build_pointer_deref(gcc.expr::in, gcc.expr::out,
    io::di, io::uo) is det.

    % Build a field extraction expression.
    %
:- pred build_component_ref(gcc.expr::in, gcc.field_decl::in, gcc.expr::out,
    io::di, io::uo) is det.

    % Build a type conversion expression.
    %
:- pred convert_type(gcc.expr::in, gcc.gcc_type::in, gcc.expr::out,
    io::di, io::uo) is det.

%
% Variables.
%

    % Build an expression for a variable.
    %
:- func var_expr(gcc.var_decl) = gcc.expr.

%
% Stuff for function calls.
%

    % Build a function pointer expression i.e. take the address of a function.
    %
:- pred build_func_addr_expr(gcc.func_decl::in, gcc.expr::out,
    io::di, io::uo) is det.

    % A GCC `tree' representing a list of arguments.
:- type gcc.arg_list.

:- pred empty_arg_list(gcc.arg_list::out, io::di, io::uo) is det.

:- pred cons_arg_list(gcc.expr::in, gcc.arg_list::in, gcc.arg_list::out,
    io::di, io::uo) is det.

    % Build an expression for a function call.
    %
:- pred build_call_expr(gcc.expr::in, gcc.arg_list::in, bool::in,
    gcc.expr::out, io::di, io::uo) is det.

%
% Initializers.
%

    % A GCC `tree' representing an array index or field to initialize.
:- type gcc.init_elem.

    % Create a gcc.init_elem that represents an initializer
    % for an array element at the given array index.
    %
:- pred array_elem_initializer(int::in, gcc.init_elem::out,
    io::di, io::uo) is det.

    % Create a gcc.init_elem that represents an initializer
    % for the given field of a structure.
    %
:- pred struct_field_initializer(gcc.field_decl::in, gcc.init_elem::out,
    io::di, io::uo) is det.

    % A GCC `tree' representing a list of initializers
    % for an array or structure.
:- type gcc.init_list.

:- pred empty_init_list(gcc.init_list::out, io::di, io::uo) is det.

:- pred cons_init_list(gcc.init_elem::in, gcc.expr::in,
    gcc.init_list::in, gcc.init_list::out, io::di, io::uo) is det.

    % Build an expression for an array or structure initializer.
    %
:- pred build_initializer_expr(gcc.init_list::in, gcc.gcc_type::in,
    gcc.expr::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Routines to protect memory from being collected by the GCC garbage
% collector (see gcc/ggc.h).
%

    % This starts a new GGC context. Memory allocated in previous contexts
    % will not be collected while the new context is active.
    %
:- pred push_gc_context(io::di, io::uo) is det.

    % Finish a GC context. Any uncollected memory in the new context
    % will be merged with the old context.
    %
:- pred pop_gc_context(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Functions
%

    % Start generating code for a function.
    %
:- pred start_function(gcc.func_decl::in, io::di, io::uo) is det.

    % Finish generating code for a function.
    % WARNING: this will invoke the GCC garbage collector.
    % So any GCC tree nodes which are referenced only from the stack(s),
    % from Mercury data structures, or from global variables, and which
    % were allocated since the most recent call to gcc.push_gc_context,
    % must be registered as roots (e.g. using ggc_add_tree_root())
    % when this is called.  Generally it is easier to call
    % gcc.push_gc_context.
    %
:- pred end_function(io::di, io::uo) is det.

    % set_context(Filename, LineNumber):
    %
    % Set the source location that GCC uses for subsequent declarations and
    % diagnostics. This should be called before `start_function' and also
    % before `end_function'.
    %
:- pred set_context(string::in, int::in, io::di, io::uo) is det.

    % gen_line_note(FileName, LineNumber):
    %   Generate a marker indicating the source location.
    %   This should be called before generating each statement.
:- pred gen_line_note(string::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Statements
%

%
% Routines to generate code for if-then-elses.
%

    % Start generating code for an if-then-else.
    % The argument is the gcc tree for the condition.
    %
:- pred gen_start_cond(gcc.expr::in, io::di, io::uo) is det.

    % Start the else part (optional).
    %
:- pred gen_start_else(io::di, io::uo) is det.

    % Finish the if-then-else.
    %
:- pred gen_end_cond(io::di, io::uo) is det.

%
% Routines to generate code for switches.
%

:- pred gen_start_switch(gcc.expr::in, gcc.gcc_type::in, io::di, io::uo)
    is det.

:- pred gen_case_label(gcc.expr::in, gcc.label::in, io::di, io::uo) is det.

:- pred gen_default_case_label(gcc.label::in, io::di, io::uo) is det.

:- pred gen_break(io::di, io::uo) is det.

:- pred gen_end_switch(gcc.expr::in, io::di, io::uo) is det.

%
% Routines to generate code for loops.
%

:- type gcc.loop.

:- pred gen_start_loop(gcc.loop::out, io::di, io::uo) is det.

:- pred gen_exit_loop_if_false(gcc.loop::in, gcc.expr::in,
    io::di, io::uo) is det.

:- pred gen_end_loop(io::di, io::uo) is det.

%
% Routines to generate code for calls/returns.
%

    % Generate code for an expression with side effects (e.g. a call).
    %
:- pred gen_expr_stmt(gcc.expr::in, io::di, io::uo) is det.

    % Generate code for a return statement.
    %
:- pred gen_return(gcc.expr::in, io::di, io::uo) is det.

%
% Assignment.
%

    % gen_assign(LHS, RHS):
    %
    % Generate code for an assignment statement.
    %
:- pred gen_assign(gcc.expr::in, gcc.expr::in, io::di, io::uo) is det.

%
% Labels and goto.
%

:- type gcc.label.
:- type gcc.label_name == string.

    % Build a gcc tree node for a label.
    % Note that you also need to use gen_label
    % (or gen_case_label) to define the label.
    %
:- pred build_label(gcc.label_name::in, gcc.label::out, io::di, io::uo) is det.

:- pred build_unnamed_label(gcc.label::out, io::di, io::uo) is det.

:- pred gen_label(gcc.label::in, io::di, io::uo) is det.

:- pred gen_goto(gcc.label::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- pragma foreign_decl("C", "

#ifndef MC_GUARD_GCC_HEADERS
#define MC_GUARD_GCC_HEADERS

#include ""gcc/config.h""
#include ""gcc/system.h""
#include ""gcc/toplev.h""
#include ""gcc/tree.h""

/* XXX The inclusion of c-tree.h is an undesirable dependency on GCC's
   C front-end.  It is only needed for versions of
   the mercury-gcc distribution based on GCC 3.2 or earlier.
   In later versions, I modified the mercury-gcc distribution so that
   it no longer depends on the C front-end, making the inclusion of
   c-tree.h unnecessary.
   This line should be removed once GCC 3.3 has been official released
   and we have then officially released a version of Mercury that uses
   GCC 3.3. */
#include ""gcc/c-tree.h""
#include ""gcc/ggc.h""

#include ""gcc/mercury/mercury-gcc.h""

#endif

").

%-----------------------------------------------------------------------------%

gcc.run_backend(CommandLine, ReturnValue, FrontEndCallBack, Output, !IO) :-
    in_gcc(InGCC, !IO),

    % For gcc.run_backend, there's two possible cases, depending on who
    % defined main():
    %
    %   1. GCC main():
    %       gcc/toplev.c gets control first.
    %
    %       In this case, by the time we get to here
    %       (gcc.m), the GCC back-end has already
    %       been initialized.  We can go ahead and directly
    %       call the front-end callback to generate the
    %       GCC tree and RTL.  When we return back to
    %       main/2 in mercury_compile, and that returns,
    %       the gcc back-end will continue on and will
    %       generate the asm file.
    %
    %       Note that mercury_compile.m can't invoke the
    %       assembler to produce an object file, since
    %       the assembler won't get produced until
    %       after main/2 has exited!  Instead, the gcc
    %       driver program (`gcc') will invoke the assembler.
    %
    %   2. Mercury main():
    %       mercury_compile.m gets control first.
    %
    %       When we get here (gcc.m), the gcc back-end
    %       has not been initialized. We need to save
    %       the front-end callback in a global variable,
    %       and then invoke the GCC toplev_main() here.
    %       This will start the GCC back-end, which will
    %       eventually call MC_continue_frontend().
    %       MC_continue_frontend() will then call the front-end
    %       callback that we saved in a global earlier.
    %       Eventually MC_continue_frontend() will
    %       return and the gcc back-end will continue.
    %
    %       It's OK for mercury_compile.m to invoke the assembler.
    %
    %       XXX For programs with nested modules,
    %       we'll end up calling the gcc back-end
    %       more than once; this will lead to an abort.
    %

    (
        InGCC = yes,
        FrontEndCallBack(Output, !IO),
        ReturnValue = 0
    ;
        InGCC = no,
        set_global_frontend_callback(FrontEndCallBack, !IO),
        call_gcc_backend(CommandLine, ReturnValue, !IO),
        get_global_frontend_callback_output(Output, !IO)
    ).

    % Returns `yes' iff we've already entered the gcc back-end.
    %
:- pred in_gcc(bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    in_gcc(Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MC_in_gcc(&Result);
").

:- pred call_gcc_backend(string::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    call_gcc_backend(AllArgs::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    MC_call_gcc_backend(AllArgs, &Result);
").

:- pragma foreign_decl("C", "
/* We use an `MC_' prefix for C code in the mercury/compiler directory. */

extern MR_Word MC_frontend_callback;
extern MR_Word MC_frontend_callback_output;
extern MR_Word MC_frontend_callback_type;

void MC_in_gcc(MR_Word *result);
void MC_call_gcc_backend(MR_String all_args, MR_Integer *result);
void MC_continue_frontend(void);

#include ""mercury_wrapper.h""      /* for MR_make_argv() */
#include <stdio.h>          /* for fprintf() */
#include <stdlib.h>         /* for exit() */
").

:- pragma foreign_code("C", "

/* We use an `MC_' prefix for C code in the mercury/compiler directory. */
MR_Word MC_frontend_callback;
MR_Word MC_frontend_callback_output;
MR_Word MC_frontend_callback_type;

extern int toplev_main(int argc, char **argv);

void
MC_in_gcc(MR_Word *result)
{
    /* If we've already entered gcc, then gcc will have set progname. */
    *result = (progname != NULL);
}

void
MC_call_gcc_backend(MR_String all_args, MR_Integer *result)
{
    char *args;
    char **argv;
    int argc;
    const char *error_msg;
    static int num_calls = 0;

    /*
    ** The gcc back-end cannot be called more than once.
    ** If you try, it uses up all available memory.
    ** So we need to abort nicely in that case.
    **
    ** That case will happen if (a) there were nested
    ** sub-modules or (b) the user specified more than
    ** one module on the command line.
    */
    num_calls++;
    if (num_calls > 1) {
        fprintf(stderr, ""Sorry, not implemented: ""
            ""calling GCC back-end multiple times.\\n""
            ""This can occur if you are trying to ""
            ""compile more than one module\\n""
            ""at a time with `--target asm'.\\n""
            ""Please use separate sub-modules ""
            ""rather than nested sub-modules,\\n""
            ""i.e. put each sub-module in its own file, ""
            ""and don't specify more\\n""
            ""than one module on the command line ""
            ""(use Mmake instead).\\n""
            ""Or alternatively, just use `--target c'.\\n"");
        exit(EXIT_FAILURE);
    }

    error_msg = MR_make_argv(all_args, &args, &argv, &argc);
    if (error_msg) {
        fprintf(stderr,
            ""Error parsing GCC back-end arguments:\n%s\n"",
            error_msg);
        exit(EXIT_FAILURE);
    }

    merc_continue_frontend = &MC_continue_frontend;
    *result = toplev_main(argc, argv);

    /*
    ** Reset GCC's progname after we return from toplev_main(),
    ** so that MC_in_gcc() knows that we're no longer in GCC.
    */
    progname = NULL;

    MR_GC_free(args);
    MR_GC_free(argv);
}

/*
** This is called from yyparse() in mercury/mercury-gcc.c
** in the gcc back-end.
*/
void
MC_continue_frontend(void)
{
    MC_call_frontend_callback(MC_frontend_callback_type,
        MC_frontend_callback, &MC_frontend_callback_output);
}
").

:- pred call_frontend_callback(
    gcc_frontend_callback(T)::in(gcc_frontend_callback), T::out,
    io::di, io::uo) is det.

:- pragma foreign_export("C",
    call_frontend_callback(in(gcc_frontend_callback), out, di, uo),
    "MC_call_frontend_callback").

call_frontend_callback(FrontEndCallBack, Output) -->
    FrontEndCallBack(Output).

:- pred get_global_frontend_callback(
    gcc_frontend_callback(T)::out(gcc_frontend_callback),
    io::di, io::uo) is det.
:- pred set_global_frontend_callback(
    gcc_frontend_callback(T)::in(gcc_frontend_callback),
    io::di, io::uo) is det.
:- pred get_global_frontend_callback_output(T::out, io::di, io::uo) is det.
:- pred set_global_frontend_callback_output(T::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_global_frontend_callback(CallBack::out(gcc_frontend_callback),
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CallBack = MC_frontend_callback;
").
:- pragma foreign_proc("C",
    set_global_frontend_callback(CallBack::in(gcc_frontend_callback),
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MC_frontend_callback = CallBack;
    MC_frontend_callback_type = TypeInfo_for_T;

").
:- pragma foreign_proc("C",
    get_global_frontend_callback_output(Output::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Output = MC_frontend_callback_output;
").
:- pragma foreign_proc("C",
    set_global_frontend_callback_output(Output::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MC_frontend_callback_output = Output;
").

%-----------------------------------------------------------------------------%

:- type gcc.tree ---> gcc.tree(c_pointer).
:- type gcc.tree_code == int.

%-----------------------------------------------------------------------------%
%
% Types
%

:- type gcc.gcc_type == gcc.tree.

:- type gcc.func_decl == gcc.gcc_type.

:- pragma foreign_proc("C",
    void_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) void_type_node;
").
:- pragma foreign_proc("C",
    boolean_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) boolean_type_node;
").
:- pragma foreign_proc("C",
    char_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) char_type_node;
").
:- pragma foreign_proc("C",
    string_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** XXX we should consider using const when appropriate,
    ** i.e. when the string doesn't have a unique mode
    */
    Type = (MR_Word) string_type_node;
").
:- pragma foreign_proc("C",
    double_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) double_type_node;
").
:- pragma foreign_proc("C",
    ptr_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) ptr_type_node;
").
:- pragma foreign_proc("C",
    integer_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) integer_type_node;
").
:- pragma foreign_proc("C",
    int8_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_int8_type_node;
").
:- pragma foreign_proc("C",
    int16_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_int16_type_node;
").
:- pragma foreign_proc("C",
    int32_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_int32_type_node;
").
:- pragma foreign_proc("C",
    int64_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_int64_type_node;
").
:- pragma foreign_proc("C",
    intptr_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_intptr_type_node;
").
:- pragma foreign_proc("C",
    jmpbuf_type_node = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) merc_jmpbuf_type_node;
").

:- pragma foreign_proc("C",
    build_pointer_type(Type::in, PtrType::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    PtrType = (MR_Word) build_pointer_type((tree) Type);
").

:- pragma foreign_proc("C",
    build_array_type(ElemType::in, NumElems::in, ArrayType::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* XXX Move this code to `mercury-gcc.c'. */
    /* XXX Do we need to check that NumElems fits in a HOST_WIDE_INT?  */
    HOST_WIDE_INT max = (HOST_WIDE_INT) NumElems - (HOST_WIDE_INT) 1;
    tree index_type = build_index_type (build_int_2 (max,
        (max < 0 ? -1 : 0)));
    ArrayType = (MR_Word) build_array_type((tree) ElemType, index_type);
").

:- pragma foreign_proc("C",
    build_range_type(Type::in, Min::in, Max::in, RangeType::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    RangeType = (MR_Word) build_range_type((tree) Type,
            build_int_2 (Min, (Min < 0 ? -1 : 0)),
            build_int_2 (Max, (Max < 0 ? -1 : 0)));
").

:- type gcc.param_types == gcc.tree.

:- pragma foreign_proc("C",
    empty_param_types = (ParamTypes::out),
    [will_not_call_mercury, promise_pure],
"
    ParamTypes = (MR_Word) merc_empty_param_type_list();
").

:- pragma foreign_proc("C",
    cons_param_types(Type::in, Types0::in) = (Types::out),
    [will_not_call_mercury, promise_pure],
"
    Types = (MR_Word)
        merc_cons_param_type_list((tree) Type, (tree) Types0);
").

:- pragma foreign_proc("C",
    build_function_type(RetType::in, ParamTypes::in, FunctionType::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    FunctionType = (MR_Word) build_function_type((tree) RetType,
        (tree) ParamTypes);
").

:- pragma foreign_proc("C",
    declared_type(TypeDecl::in) = (Type::out),
    [will_not_call_mercury, promise_pure],
"
    Type = (MR_Word) TREE_TYPE((tree) TypeDecl);
").

:- pragma foreign_proc("C",
    get_array_elem_type(ArrayType::in, ElemType::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ElemType = (MR_Word) TREE_TYPE((tree) ArrayType);
").

:- pragma foreign_proc("C",
    get_struct_field_decls(StructType::in, FieldDecls::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    FieldDecls = (MR_Word) TYPE_FIELDS((tree) StructType);
").

%-----------------------------------------------------------------------------%
%
% Declarations.
%

%
% Stuff for variable declarations.
%

:- type gcc.var_decl == gcc.tree.

:- pragma foreign_proc("C",
    build_extern_var_decl(Name::in, Type::in, Decl::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_extern_var_decl(Name, (tree) Type);
").

:- pragma foreign_proc("C",
    build_static_var_decl(Name::in, Type::in, Init::in, Decl::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_static_var_decl(Name, (tree) Type,
        (tree) Init);
").

:- pragma foreign_proc("C",
    finish_static_var_decl(Decl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_finish_static_var_decl((tree) Decl);
").

:- pragma foreign_proc("C",
    build_local_var_decl(Name::in, Type::in, Decl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_local_var_decl(Name, (tree) Type);
").

:- pragma foreign_proc("C",
    set_var_decl_public(Decl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    TREE_PUBLIC((tree) Decl) = 1;
").

:- pragma foreign_proc("C",
    set_var_decl_readonly(Decl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    TREE_READONLY((tree) Decl) = 1;
").

:- pragma foreign_proc("C",
    set_var_decl_asm_name(Decl::in, AsmName::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    SET_DECL_ASSEMBLER_NAME((tree) Decl, get_identifier(AsmName));
").

%
% Stuff for function declarations.
%

:- type gcc.param_decls == gcc.tree.

:- pragma foreign_proc("C",
    build_param_decl(Name::in, Type::in, Decl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_param_decl(Name, (tree) Type);
").

:- pragma foreign_proc("C",
    empty_param_decls = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_empty_param_list();
").

:- pragma foreign_proc("C",
    cons_param_decls(Decl::in, Decls0::in) = (Decls::out),
    [will_not_call_mercury, promise_pure],
"
    Decls = (MR_Word) merc_cons_param_list((tree) Decl, (tree) Decls0);
").

:- pragma foreign_proc("C",
    build_function_decl(Name::in, AsmName::in, RetType::in, ParamTypes::in,
        Params::in, Decl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_function_decl(Name, AsmName,
            (tree) RetType, (tree) ParamTypes, (tree) Params);
").

:- pragma foreign_proc("C",
    alloc_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_alloc_function_node;
").

:- pragma foreign_proc("C",
    strcmp_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_strcmp_function_node;
").

:- pragma foreign_proc("C",
    hash_string_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_hash_string_function_node;
").

:- pragma foreign_proc("C",
    box_float_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_box_float_function_node;
").

:- pragma foreign_proc("C",
    setjmp_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_setjmp_function_node;
").

:- pragma foreign_proc("C",
    longjmp_func_decl = (Decl::out),
    [will_not_call_mercury, promise_pure],
"
    Decl = (MR_Word) merc_longjmp_function_node;
").

:- pragma foreign_proc("C",
    set_func_decl_public(Decl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    TREE_PUBLIC((tree) Decl) = 1;
").

%
% Stuff for type declarations.
%

:- type gcc.field_decl == gcc.tree.

:- pragma foreign_proc("C",
    build_field_decl(Name::in, Type::in, Decl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_field_decl(Name, (tree) Type);
").

:- pragma foreign_proc("C",
    field_type(Decl::in, Type::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Type = (MR_Word) TREE_TYPE((tree) Decl);
").

:- type gcc.field_decls == gcc.tree.

:- pragma foreign_proc("C",
    empty_field_list(Decl::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_empty_field_list();
").

:- pragma foreign_proc("C",
    cons_field_list(Decl::in, Decls0::in, Decls::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decls = (MR_Word) merc_cons_field_list((tree) Decl, (tree) Decls0);
").

:- pragma foreign_proc("C",
    next_field_decl(Decls::in, Decl::out, RemainingDecls::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    assert((tree) Decls != NULL_TREE);
    Decl = (MR_Word) (tree) Decls;
    RemainingDecls = (MR_Word) TREE_CHAIN((tree) Decls);
").

:- type gcc.type_decl == gcc.tree.

:- pragma foreign_proc("C",
    build_struct_type_decl(Name::in, FieldTypes::in, Decl::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Decl = (MR_Word) merc_build_struct_type_decl(Name, (tree) FieldTypes);
").

%-----------------------------------------------------------------------------%
%
% Operators.
%

:- type gcc.op == gcc.tree_code.

:- pragma foreign_proc("C",
    plus_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = PLUS_EXPR;
").
:- pragma foreign_proc("C",
    minus_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = MINUS_EXPR;
").
:- pragma foreign_proc("C",
    mult_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = MULT_EXPR;
").
:- pragma foreign_proc("C",
    rdiv_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = RDIV_EXPR;
").
:- pragma foreign_proc("C",
    trunc_div_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = TRUNC_DIV_EXPR;
").
:- pragma foreign_proc("C",
    trunc_mod_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = TRUNC_MOD_EXPR;
").

:- pragma foreign_proc("C",
    eq_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = EQ_EXPR;
").
:- pragma foreign_proc("C",
    ne_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = NE_EXPR;
").
:- pragma foreign_proc("C",
    lt_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = LT_EXPR;
").
:- pragma foreign_proc("C",
    gt_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = GT_EXPR;
").
:- pragma foreign_proc("C",
    le_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = LE_EXPR;
").
:- pragma foreign_proc("C",
    ge_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = GE_EXPR;
").

:- pragma foreign_proc("C",
    truth_andif_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = TRUTH_ANDIF_EXPR;
").
:- pragma foreign_proc("C",
    truth_orif_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = TRUTH_ORIF_EXPR;
").
:- pragma foreign_proc("C",
    truth_not_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = TRUTH_NOT_EXPR;
").

:- pragma foreign_proc("C",
    bit_ior_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = BIT_IOR_EXPR;
").
:- pragma foreign_proc("C",
    bit_xor_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = BIT_XOR_EXPR;
").
:- pragma foreign_proc("C",
    bit_and_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = BIT_AND_EXPR;
").
:- pragma foreign_proc("C",
    bit_not_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = BIT_NOT_EXPR;
").

:- pragma foreign_proc("C",
    lshift_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = LSHIFT_EXPR;
").
:- pragma foreign_proc("C",
    rshift_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = RSHIFT_EXPR;
").

:- pragma foreign_proc("C",
    array_ref = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = ARRAY_REF;
").

%-----------------------------------------------------------------------------%
%
% Expressions.
%

:- type gcc.expr == gcc.tree.

:- pragma foreign_proc("C",
    expr_type(Expr::in, Type::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Type = (MR_Word) TREE_TYPE((tree) Expr);
").

%
% Constants.
%

build_int(Val, IntExpr) -->
    { Lowpart = Val },
    { Highpart = (if Val < 0 then -1 else 0) },
    build_int_2(Lowpart, Highpart, IntExpr).

    % build_int_2(Lowpart, Highpart):
    %
    % Build an expression for an integer constant.
    % Lowpart gives the low word, and Highpart gives the high word.
    %
:- pred build_int_2(int::in, int::in, gcc.expr::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    build_int_2(Low::in, High::in, Expr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) build_int_2(Low, High);
").

build_float(Val, Expr) -->
    build_real(gcc.double_type_node, Val, Expr).

    % Build an expression for a floating-point constant of the specified type.
    %
:- pred build_real(gcc.gcc_type::in, float::in, gcc.expr::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    build_real(Type::in, Value::in, Expr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) merc_build_real((tree) Type, Value);
").

build_string(String, Expr) -->
    build_string(string.length(String) + 1, String, Expr).

:- pragma foreign_proc("C",
    build_string(Len::in, String::in, Expr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) merc_build_string(Len, String);
").

:- pragma foreign_proc("C",
    build_null_pointer(NullPointerExpr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    NullPointerExpr = (MR_Word) null_pointer_node;
").

%
% Operator expressions.
%

:- pragma foreign_proc("C",
    build_unop(Op::in, Type::in, Arg::in, Expr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) fold(build1(Op, (tree) Type, (tree) Arg));
").

:- pragma foreign_proc("C",
    build_binop(Op::in, Type::in, Arg1::in, Arg2::in, Expr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) fold(build(Op, (tree) Type, (tree) Arg1, (tree) Arg2));
").

:- pragma foreign_proc("C",
    build_pointer_deref(Pointer::in, DerefExpr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* XXX should move to mercury-gcc.c */
    tree ptr = (tree) Pointer;
    tree ptr_type = TREE_TYPE (ptr);
    tree type = TREE_TYPE (ptr_type);
    DerefExpr = (MR_Word) build1 (INDIRECT_REF, type, ptr);
").

:- pragma foreign_proc("C",
    build_component_ref(ObjectExpr::in, FieldDecl::in, FieldExpr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /* XXX should move to mercury-gcc.c */
    tree field_type = TREE_TYPE ((tree) FieldDecl);
    FieldExpr = (MR_Word) build (COMPONENT_REF, field_type,
        (tree) ObjectExpr, (tree) FieldDecl);
").

:- pragma foreign_proc("C",
    convert_type(Expr::in, Type::in, ResultExpr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    /*
    ** XXX should we use convert() instead?
    ** if not, should we expose the CONVERT_EXPR gcc.op
    ** and just use gcc.build_binop?
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

build_addr_expr(Expr, AddrExpr, !IO) :-
    mark_addressable(Expr, !IO),
    expr_type(Expr, Type, !IO),
    build_pointer_type(Type, PtrType, !IO),
    build_unop(addr_expr, PtrType, Expr, AddrExpr, !IO).

:- func addr_expr = gcc.op.        % & (address-of)
:- pragma foreign_proc("C",
    addr_expr = (Code::out),
    [will_not_call_mercury, promise_pure],
"
    Code = ADDR_EXPR;
").

:- pred mark_addressable(gcc.expr::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    mark_addressable(Expr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mark_addressable((tree) Expr);
").

%
% Variables.
%


    % GCC represents variable expressions just by (the pointer to)
    % their declaration tree node.
var_expr(Decl) = Decl.

%
% Stuff for function calls.
%

build_func_addr_expr(FuncDecl, Expr, !IO) :-
    % GCC represents functions pointer expressions just as ordinary
    % ADDR_EXPR nodes whose operand is the function declaration tree node.
    build_addr_expr(FuncDecl, Expr, !IO).

:- type gcc.arg_list == gcc.tree.

:- pragma foreign_proc("C",
    empty_arg_list(ArgList::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ArgList = (MR_Word) merc_empty_arg_list();
").

:- pragma foreign_proc("C",
    cons_arg_list(Arg::in, ArgList0::in, ArgList::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ArgList = (MR_Word)
        merc_cons_arg_list((tree) Arg, (tree) ArgList0);
").

:- pragma foreign_proc("C",
    build_call_expr(Func::in, Args::in, IsTailCall::in, CallExpr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    CallExpr = (MR_Word) merc_build_call_expr((tree) Func, (tree) Args,
        (int) IsTailCall);
").

%
% Initializers.
%

:- type gcc.init_elem == gcc.tree.

gcc.array_elem_initializer(Int, GCC_Int) -->
    build_int(Int, GCC_Int).

gcc.struct_field_initializer(FieldDecl, FieldDecl) --> [].

:- type gcc.init_list == gcc.tree.

:- pragma foreign_proc("C",
    empty_init_list(InitList::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    InitList = (MR_Word) merc_empty_init_list();
").

:- pragma foreign_proc("C",
    cons_init_list(Elem::in, Init::in, InitList0::in, InitList::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    InitList = (MR_Word)
        merc_cons_init_list((tree) Elem, (tree) Init, (tree) InitList0);
").

:- pragma foreign_proc("C",
    build_initializer_expr(InitList::in, Type::in, Expr::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Expr = (MR_Word) build(CONSTRUCTOR, (tree) Type, NULL_TREE,
        (tree) InitList);
").

%-----------------------------------------------------------------------------%
%
% Routines to protect memory from being collected by the GCC garbage
% collector (see gcc/ggc.h).
%

:- pragma foreign_proc("C",
    push_gc_context(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    ggc_push_context();
").

:- pragma foreign_proc("C",
    pop_gc_context(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    ggc_pop_context();
").

%-----------------------------------------------------------------------------%
%
% Functions.
%

:- pragma foreign_proc("C",
    start_function(FuncDecl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_start_function((tree) FuncDecl);
").

:- pragma foreign_proc("C",
    end_function(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_end_function();
").

:- pragma foreign_proc("C",
    set_context(FileName::in, LineNumber::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_set_context(FileName, LineNumber);
").

:- pragma foreign_proc("C",
    gen_line_note(FileName::in, LineNumber::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    emit_line_note(FileName, LineNumber);
").

%-----------------------------------------------------------------------------%
%
% Statements.
%

%
% Blocks.
%

:- pragma foreign_proc("C",
    start_block(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    pushlevel(0);
    expand_start_bindings(0);
").

:- pragma foreign_proc("C",
    end_block(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    tree block = poplevel(/*keep=*/1, /*reverse=*/1, /*functionbody=*/0);
    expand_end_bindings(block, /*mark_ends=*/1, /*dont_jump_in=*/0);
").

%
% If-then-else.
%

:- pragma foreign_proc("C",
    gen_start_cond(Cond::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_start_cond((tree) Cond, 0);
").

:- pragma foreign_proc("C",
    gen_start_else(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    expand_start_else();
").

:- pragma foreign_proc("C",
    gen_end_cond(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    expand_end_cond();
").

%
% Switch statements.
%

:- pragma foreign_proc("C",
    gen_start_switch(Expr::in, Type::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_start_case(1, (tree) Expr, (tree) Type, ""switch"");
").

:- pragma foreign_proc("C",
    gen_case_label(Value::in, Label::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_gen_switch_case_label((tree) Value, (tree) Label);
").

:- pragma foreign_proc("C",
    gen_default_case_label(Label::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_gen_switch_case_label(NULL_TREE, (tree) Label);
").

:- pragma foreign_proc("C",
    gen_break(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    int result = expand_exit_something();
    assert(result != 0);
").

:- pragma foreign_proc("C",
    gen_end_switch(Expr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_end_case((tree) Expr);
").

%
% Loops.
%

    % The type `gcc.loop' corresponds to the C type `struct nesting *'.
:- type gcc.loop ---> gcc.loop(c_pointer).

:- pragma foreign_proc("C",
    gen_start_loop(Loop::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Loop = (MR_Word) expand_start_loop(0);
").

:- pragma foreign_proc("C",
    gen_exit_loop_if_false(Loop::in, Expr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    int res = expand_exit_loop_if_false((struct nesting *) Loop,
            (tree) Expr);
    assert(res != 0);
").

:- pragma foreign_proc("C",
    gen_end_loop(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_end_loop();
").

%
% Calls and return.
%

:- pragma foreign_proc("C",
    gen_expr_stmt(Expr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_gen_expr_stmt((tree) Expr);
").

:- pragma foreign_proc("C",
    gen_return(Expr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_gen_return((tree) Expr);
").

%
% Assignment.
%

:- pragma foreign_proc("C",
    gen_assign(LHS::in, RHS::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    merc_gen_assign((tree) LHS, (tree) RHS);
").

%
% Labels and gotos.
%

:- type gcc.label == gcc.tree.

:- pragma foreign_proc("C",
    build_label(Name::in, Label::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Label = (MR_Word) merc_build_label(Name);
").

:- pragma foreign_proc("C",
    build_unnamed_label(Label::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Label = (MR_Word) merc_build_label(NULL);
").

:- pragma foreign_proc("C",
    gen_label(Label::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_label((tree) Label);
").

:- pragma foreign_proc("C",
    gen_goto(Label::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    expand_goto((tree) Label);
").

%-----------------------------------------------------------------------------%
