//
// Copyright (C) 2001-2003 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class TypeCtorInfo_Struct extends PseudoTypeInfo {
    
	public int                              arity;
	public int                              type_ctor_version;
	public int                              type_ctor_num_ptags; // if DU
	public mercury.runtime.TypeCtorRep      type_ctor_rep;
	public mercury.runtime.MethodPtr        unify_pred;
	public mercury.runtime.MethodPtr        compare_pred;
	public java.lang.String                 type_ctor_module_name;
	public java.lang.String                 type_ctor_name;
	public mercury.runtime.TypeFunctors     type_functors;
	public mercury.runtime.TypeLayout       type_layout;
	public int 			        type_ctor_num_functors;
	public /* short */ int 			type_ctor_flags;

	public TypeCtorInfo_Struct(
			int type_arity, int version, int num_ptags, int rep,
			Object unify_proc, Object compare_proc, 
			String module, String name,
			// mercury.runtime.TypeFunctors
			java.lang.Object name_ordered_functor_descs,
			// mercury.runtime.TypeLayout
			java.lang.Object value_ordered_functor_descs,
			int num_functors, int flags)
	{
		arity = type_arity;
		type_ctor_version = version;
		type_ctor_num_ptags = num_ptags;
		type_ctor_rep = new TypeCtorRep(rep);
		unify_pred = (mercury.runtime.MethodPtr) unify_proc;
		compare_pred = (mercury.runtime.MethodPtr) compare_proc;
		type_ctor_module_name = module;
		type_ctor_name = name;
		type_functors = (mercury.runtime.TypeFunctors)
			name_ordered_functor_descs;
		type_layout = (mercury.runtime.TypeLayout)
			value_ordered_functor_descs;
		type_ctor_flags = flags;
	}
}
