%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% ilds - The IL instruction set.
% Main author: trd.
%
% The IL instruction set is documented in the Microsoft .NET Framework SDK.
%
% See 
% 	http://msdn.microsoft.com/net/
% for more info, including a downloadable (Windows only) version of the
% SDK available here:
% 	http://msdn.microsoft.com/downloads/default.asp?URL=/code/sample.asp?url=/msdn-files/027/000/976/msdncompositedoc.xml
%
%-----------------------------------------------------------------------------%

:- module ilds.

:- interface.

:- import_module list, std_util, bool, assoc_list.

	% A method parameter
:- type param == pair(
			ilds__type,	% type of the parameter
			maybe(ilds__id) % name of the parameter (if any)
		).

	% A method signature
:- type signature 
	---> signature(
		call_conv,	% calling convention
		ret_type,	% return type
		list(param)	% parameters
	).

	% A method reference.
:- type methodref
	---> methoddef(call_conv, ret_type, class_member_name,
		list(ilds__type))
			% XXX not sure whether methodref is used.
	;    methodref(call_conv, ret_type, class_name,
		list(ilds__type))
	;    local_method(call_conv, ret_type, member_name,
		list(ilds__type)).

	% A field reference
:- type fieldref
	---> fieldref(ilds__type, class_member_name).

% -------------------------------------------------------------------------

	% if an assembly name is empty it is a reference to a local type
	% in the same assembly. 

:- type structured_name ---> 
		structured_name(assembly_name, namespace_qual_name).

:- type assembly_name == ilds__id. 
:- type namespace_qual_name == list(ilds__id). 

	
	% A namespace qualified class name is a structured name.
	% [Foo]Foo::Bar::Baz is structured_name("Foo", ["Foo", "Bar", "Baz"])
:- type class_name == structured_name.

	% A member of a class 
:- type class_member_name
	---> class_member_name(
			class_name, 
			member_name
	).

	% The name of a member (method, field, event or property)
:- type member_name 
	--->	ctor		% constructor (initializes instances
				% of this class)
	; 	cctor		% class constructor (initializes
				% non-instance fields).
	;	id(ilds__id).	% ordinary method or field name

	% calling conventions.
:- type call_conv   
	--->	call_conv(
			bool,		% is this an instance method call?
			call_kind	% what kind of call is it
	).

:- type call_kind 
	--->	default
	; 	vararg	
	;	unmanaged_cdecl	
	;	unmanaged_stdcall
	;	unmanaged_thiscall
	;	unmanaged_fastcall.


	% XXX types have changed significantly in the spec since this
	% was written, we should update this section (indeed, we should
	% update all of ilds.m and ilasm.m).

	% return types
:- type ret_type
	--->	void
	;	simple_type(simple_type).

:- type ilds__type
	--->	ilds__type(list(ilds__type_modifier), simple_type).

:- type ilds__type_modifier
	--->	const
	;	readonly
	;	volatile.

:- type simple_type
	---> 	int8
	;	int16
	;	int32
	;	int64	
	;	uint8
	;	uint16
	;	uint32
	;	uint64
	;	native_int	
	;	native_uint		% Also used for unmanaged pointers.
	;	float32
	;	float64
	;	native_float
	;	bool
	;	char			% A unicode character.
	;	refany			% a reference to value with an attached
					% type
	; 	class(class_name)
	;	value_class(class_name)
	;	interface(class_name)
	;	'[]'(ilds__type, bounds) % An array
	;	'&'(ilds__type)		 % A managed pointer
	;	'*'(ilds__type).	 % A transient pointer (could become 
					 % managed or unmanaged depending on
					 % usage).

:- type bounds == list(bound).

:- type bound 
	---> 	upper(int)		% 0 <= index <= int
	;    	lower(int)		% int <= index <= maxint
	;    	between(int, int).	% int <= index <= int2

	% an ID must start with "<", "_" or an alphabetic character.
	% This initial character can be followed by any number of alphabetic
	% characters, decimal digits, ">", "<", or "_".
:- type ilds__id == string.

	% XXX should really limit this, but we don't really support
	% the alignment instruction just yet.
:- type alignment == int.

:- type constant
	--->	i(int)
	;	f(float).

:- type overflow 
	--->	checkoverflow
	;	nocheckoverflow.

:- type signed 
	--->	signed
	;	unsigned.  % or unordered for comparisons

	% A variable (local or argument) can be referred to by name or index
:- type variable 
	--->	name(ilds__id)
	;	index(index). 

:- type index == int.

:- type target 
	---> 	offset_target(int)
	;	label_target(label).

	% Local variables, they all have names.
	% This should probably be the same as params.
:- type locals == assoc_list(ilds__id, ilds__type).

	% blocks can be just scope for locals, or can 
	% introduce try or catch code.
:- type blocktype 
	--->	scope(locals)
	;	try
	;	catch(class_name).

	% each block has a unique identifier (mainly so you can tell which
	% ones match up without counting them).
	% XXX should probably use counter type instead.
:- type blockid == int. 

:- type instr 


	% NOT INSTRUCTIONS AT ALL
	% These are just added to the IL instructions to make it easy to
	% generate instructions and include debugging information.
	--->	comment(string)
	;	label(label)				% a label 
	;	start_block(blocktype, blockid)		% new block 
	;	end_block(blocktype, blockid)		% end block
	;	context(string, int)			% context of following 
							% code (filename, line)

	% BASE INSTRUCTIONS

	; 	add(overflow, signed)	% add numeric values
	;	(and)			% bitwise and
	;	arglist			% return arglist handle for current meth
	;	beq(target)		% branch to target if equal
	;	bge(signed, target)	% branch to target if >=
	;	bgt(signed, target)	% branch to target if >
	;	ble(signed, target)	% branch to target if <=
	;	blt(signed, target)	% branch to target if <
	;	bne(signed, target)	% branch to target if !=
	;	br(target)		% branch to target
	;	break			% inform debugger breakpoint reached
	;	brfalse(target)		% branch to target if value is zero
	;	brtrue(target)		% branch to target if value is non-zero
	;	call(methodref)	% call method described by methodref
	;	calli(signature)	% call method indicated on stack using
					% args described by the signature
	;	ceq		% compare equal: push 1 if value1 equals value2
	;	cgt(signed)	% compare >: push 1 if value1 > value2
	;	ckfinite	% throw ArithmeticException if value not finite
	;	clt(signed)	% compare <: push 1 if value1 < value2
	;	conv(simple_type)	% convert value to data type
	;	cpblk		% copy data from memory to memory
	; 	div(signed)	% divide values
	;	dup		% duplicate the value on the top of the stack
	;	endcatch	% end exception handler
	;	endfilter	% end filter clause of SEH exception handling
	;	endfinally	% end finally clause of an exception block
	;	initblk		% initialize a block
	;	jmp(methodref) % jump to a specified method
	;	jmpi		% exit current method and jump to spec method
	;	ldarg(variable)	% load argument onto the stack
	; 	ldarga(variable)	% fetch address of argument
	;	ldc(simple_type, constant)	
					% load a numeric constant
	;	ldftn(methodref)	% push a pointer to a method
	;	ldind(simple_type)	% indirect load a value onto the stack
	;	ldloc(variable)	% load a local variable onto the stack
	;	ldloca(variable) 	% load a local variable address
	;	ldnull			% push a null GC reference onto stack
	;	leave(target)	% exit a protected region of code
	;	localloc		% allocate space from local pool
	;	mul(overflow, signed)	% multiply values
	;	neg			% negate value
	;	nop			% no operation
	;	(not)			% bitwise negation
	;	(or)			% bitwise or
	;	pop			% pop a value from the stack
	;	rem(signed)		% compute remainder
	;	ret			% return from method
	;	shl			% shift integer left
	;	shr(signed)		% shift integer right
	;	starg(variable)		% store a value in argument slot
	;	stind(simple_type)	% store indirect at address from stack
	;	stloc(variable)		% pop value from stack to local var
	;	sub(overflow, signed)	% subtract value2 from value1
	;	switch(list(target))	% table switch on value
	;	tailcall		% remove frame before following call
	;	unaligned(alignment)	% subsequent pointer not aligned 
	;	volatile		% subsequent pointer ref is volatile
	;	xor			% bitwise XOR of integer values

	% OBJECT MODEL INSTRUCTIONS 

	;	box(ilds__type)		% convert pointer to reference
	;	callvirt(methodref)	% call a method associated with obj
	;	castclass(ilds__type)	% cast obj to class
	;	cpobj(ilds__type)	% copy a value type
	;	entercrit		% enter a critical region with obj
	;	exitcrit		% exit a critical region with obj
	;	initobj(ilds__type)	% initialize a value type
	;	isinst(ilds__type)	% test if obj is an instance
	;	ldelem(simple_type)	% load an element of an array
	;	ldelema(ilds__type)	% load address of element of array
	;	ldfld(fieldref)		% load value of field of obj
	;	ldflda(fieldref)	% load field address of obj
	;	ldlen			% load length of array
	;	ldobj(ilds__type)	% copy value type to stack
	;	ldrefany(index)		% push refany in arg num onto stack
		% XXX this appears to have been removed
%	;	ldrefanya(index, ilds__type) % push refany addr into arg num
	;	ldsfld(fieldref)	% load static field of a class
	;	ldsflda(fieldref)	% load static field address
	;	ldstr(string)		% load a literal string
	;	ldtoken(signature)	% load runtime rep of metadata token
	;	ldvirtftn(methodref)	% push a pointer to a virtual method
	;	mkrefany(ilds__type)	% push a refany pointer of type class
	;	newarr(ilds__type)	% create a zero based 1D array
	;	newobj(methodref)	% create new obj and call constructor
	;	stelem(simple_type)	% store an element of an array
	;	stfld(fieldref)		% store into a field of an object
	;	stsfld(fieldref)	% replace the value of field with val
	;	throw			% throw an exception
	;	typerefany(index)	% extract type info from refany nth arg
	;	unbox(ilds__type)	% convert boxed value type to raw form

	% ANNOTATIONS (only used for OPT-IL)

	;	ann_call(signature)	% start of simple calling sequence
	;	ann_catch		% start an exception filter or handler
	;	ann_data(int)		% int32 bytes of uninterp. data follows 
	;	ann_dead(location)	% stack location is no longer live
	;	ann_def			% start SSA node
	;	ann_hoisted(signature)  % start of complex argument evaluation
	;	ann_hoisted_call	% start of simple part of hoisted call
	;	ann_lab			% label (mark a branch target location)
	;	ann_live(location)	% mark a stack location as live
	;	ann_phi(list(node_number)) % merge SSA definition nodes
	;	ann_ref(node_number).	% SSA reference node -- value at 
					% node_number is same as next 
					% instruction

	% locations marked as dead by ann_dead -- positive numbers are
	% stack slots, negative numbers are locals.
:- type location == int.

	% static single assignment nodes are generated, numbered from 0,
	% by ann_def and ann_phi.
:- type node_number == int.

:- type label == string.


	% Utility functions and predicates.

	% Get the namespace portion of a class name.

:- func get_class_namespace(ilds__class_name) = ilds__namespace_qual_name.

	% Add an extra identifier to the end of an IL class name, e.g.
	% append Foo to [mercury]mercury.runtime to make
	% [mercury]mercury.runtime.Foo

:- func append_class_name(ilds__class_name, ilds__namespace_qual_name) =
	ilds__class_name.

:- implementation.

:- import_module error_util.

get_class_namespace(structured_name(_, FullName)) = NamespaceName :-
	( 
		list__last(FullName, Last),
		list__remove_suffix(FullName, [Last], NamespaceName0)
	->
		NamespaceName0 = NamespaceName
	;
			% This class has no name whatsoever.
		unexpected(this_file, "get_class_namespace: list__drop failed")
	).

append_class_name(structured_name(Assembly, ClassName), ExtraClass) =
		structured_name(Assembly, NewClassName) :-
	list__append(ClassName, ExtraClass, NewClassName).



:- func this_file = string.
this_file = "ilds.m".

:- end_module ilds.
