// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2004 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

// This corresponds to the C type MR_TypeClassMethod
// in runtime/mercury_typeclass_info.h.

public class TypeClassMethod implements java.io.Serializable {
    public /* final */ java.lang.String     tc_method_name;
    public /* final */ int                  tc_method_arity;
    public /* final */ int /* PredFunc */   tc_method_pred_func;

    public TypeClassMethod(java.lang.String name, int arity, int pred_func) {
        tc_method_name = name;
        tc_method_arity = arity;
        tc_method_pred_func = pred_func;
    }
}
