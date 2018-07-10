// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2003 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class MaybeResAddrFunctorDesc {
    public java.lang.String     maybe_res_name;
    public int                  maybe_res_arity;
    public boolean              maybe_res_is_res;
    public MaybeResFunctorDesc  maybe_res_ptr;

    public MaybeResAddrFunctorDesc(java.lang.String name,
        int arity,
        boolean is_res,
        MaybeResFunctorDesc ptr)
    {
        maybe_res_name = name;
        maybe_res_arity = arity;
        maybe_res_is_res = is_res;
        maybe_res_ptr = ptr;
    }
}
