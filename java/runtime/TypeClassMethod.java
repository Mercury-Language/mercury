//
// Copyright (C) 2004 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.
//

package mercury.runtime;

// This corresponds to the C type MR_TypeClassMethod
// in runtime/mercury_typeclass_info.h.

public class TypeClassMethod {
	public /* final */ java.lang.String		tc_method_name;
	public /* final */ int				tc_method_arity;
	public /* final */ int /* PredFunc */		tc_method_pred_func;

	public TypeClassMethod(java.lang.String name, int arity,
		int pred_func)
	{
		tc_method_name = name;
		tc_method_arity = arity;
		tc_method_pred_func = pred_func;
	}
}
