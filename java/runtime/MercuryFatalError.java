// vim: ts=4 sw=4 expandtab ft=java
//
// Copyright (C) 2024 The Mercury Team.
// This file is distributed under the terms specified in COPYING.LIB.
//
// This exception is for when fatal errors in the Mercury runtime.

package jmercury.runtime;

public class MercuryFatalError extends java.lang.Error {

    public MercuryFatalError(String message) {
        super(message);
    }

    public MercuryFatalError(String message, Throwable cause) {
        super(message, cause);
    }

    public MercuryFatalError(Throwable cause) {
        super(cause);
    }
}
