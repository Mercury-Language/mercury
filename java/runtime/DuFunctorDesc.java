//
// Copyright (C) 2001-2002 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

public class DuFunctorDesc {
    
	public java.lang.String du_functor_name;
	public int du_functor_orig_arity;
	public int du_functor_arg_type_contains_var;
	public mercury.runtime.Sectag_Locn du_functor_sectag_locn;
	public int du_functor_primary;
	public int du_functor_secondary;
	public int du_functor_ordinal;
	// XXX PseudoTypeInfo's have not been implemented properly
	//     yet, so this may not be correct.
	public /*final*/ mercury.runtime.PseudoTypeInfo[] du_functor_arg_types;
	public /*final*/ java.lang.String[] du_functor_arg_names;
	public /*final*/ mercury.runtime.DuExistInfo[] du_functor_exist_info;

}
