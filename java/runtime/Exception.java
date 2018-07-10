// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2009 The University of Melbourne.
// Copyright (C) 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.
//

package jmercury.runtime;

public class Exception extends java.lang.Error {
    // This is to be set when the exception module is initialised,
    // to avoid having the runtime depend on the standard library.
    public static MethodPtr1 getMessageHook = null;

    // Should be univ.Univ_0 but we don't want to depend on the standard
    // library.
    public Object exception;

    public Exception(Object exception) {
        this.exception = exception;
    }

    public String getMessage() {
        if (getMessageHook != null) {
            return (String) getMessageHook.call___0_0(exception);
        } else {
            return null;
        }
    }
}
