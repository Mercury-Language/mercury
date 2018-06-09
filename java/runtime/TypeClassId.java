//
// Copyright (C) 2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// This corresponds to the C type MR_TypeClassId
// in runtime/mercury_typeclass_info.h.

public class TypeClassId implements java.io.Serializable {
	public String			tc_id_module_name;
	public String			tc_id_name;
	public int			tc_id_arity;
	public int			tc_id_num_type_vars; // XXX redundant
	public int			tc_id_num_methods;   // XXX redundant
	public String[]			tc_id_var_names;
	public TypeClassMethod[]	tc_id_methods;

	public TypeClassId()
	{
	}

	public void init(String module_name, String name, int arity,
		int num_type_vars, int num_methods,
		String[] var_names, TypeClassMethod[] methods)
	{
		tc_id_module_name = module_name;
		tc_id_name = name;
		tc_id_arity = arity;
		tc_id_num_type_vars = num_type_vars;
		tc_id_num_methods = num_methods;
		tc_id_var_names = var_names;
		tc_id_methods = methods;
	}
}
