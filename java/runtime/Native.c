/*
** Copyright (C) 2004, 2006 The University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/

/*
** File: Native.c	- Native code for java/runtime/Native.java
*/

#include <jni.h>

/*
 * Class:     Native
 * Method:    clock
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_jmercury_runtime_Native_clock(JNIEnv *, jclass);

/*
 * Class:     Native
 * Method:    clocks_per_sec
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_jmercury_runtime_Native_clocks_1per_1sec(
		JNIEnv *, jclass);

/*
 * Class:     Native
 * Method:    times
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_jmercury_runtime_Native_times(
		JNIEnv *, jclass);

/*
 * Class:     Native
 * Method:    clk_tck
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_jmercury_runtime_Native_clk_1tck(JNIEnv *, jclass);

/*
 * Class:     Native
 * Method:    get_user_cpu_milliseconds
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_jmercury_runtime_Native_get_1user_1cpu_1milliseconds(
		JNIEnv *, jclass);

#include "mercury_imp.h"
#include "mercury_timing.h"

#include <time.h>
#ifdef MR_HAVE_SYS_TYPES_H
	#include <sys/types.h>
#endif
#ifdef MR_HAVE_SYS_TIMES_H
	#include <sys/times.h>
#endif
#ifdef MR_HAVE_UNISTD_H
	#include <unistd.h>
#endif

JNIEXPORT jint JNICALL
Java_jmercury_runtime_Native_clock(JNIEnv *env, jclass obj) {
	return (MR_Integer) clock();
}

JNIEXPORT jint JNICALL
Java_jmercury_runtime_Native_clocks_1per_1sec(JNIEnv *env, jclass obj) {
	return CLOCKS_PER_SEC;
}

JNIEXPORT jintArray JNICALL
Java_jmercury_runtime_Native_times(JNIEnv *env, jclass obj) {
	jint		intarray[5];
	jintArray	result;

#ifdef MR_HAVE_POSIX_TIMES
	struct tms	t;

	intarray[0] = (MR_Integer) times(&t);
	intarray[1] = (MR_Integer) t.tms_utime;
	intarray[2] = (MR_Integer) t.tms_stime;
	intarray[3] = (MR_Integer) t.tms_cutime;
	intarray[4] = (MR_Integer) t.tms_cstime;
#else
	intarray[0] = -1;
#endif

	result = (*env)->NewIntArray(env, 5);
	if (result != NULL) {
		(*env)->SetIntArrayRegion(env, result, 0, 5, intarray);
	}

	return result;
}

JNIEXPORT jint JNICALL Java_jmercury_runtime_Native_clk_1tck(
		JNIEnv *env, jclass obj)
{
#if defined(MR_CLOCK_TICKS_PER_SECOND)
	return MR_CLOCK_TICKS_PER_SECOND;
#else
	return -1;
#endif
}

JNIEXPORT jint JNICALL
Java_jmercury_runtime_Native_get_1user_1cpu_1milliseconds(
		JNIEnv *env, jclass obj)
{
	return MR_get_user_cpu_milliseconds();
}

