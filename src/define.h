#ifndef __LISP_DEFINE_HEADER__
#define __LISP_DEFINE_HEADER__

/*
 *  autoconf
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_URL
#undef PACKAGE_VERSION
#undef VERSION
#endif


/*
 *  define.h
 */
#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif

#include <limits.h>
#include <stdint.h>
#include <float.h>
#include "define_compile.h"
#include "version.h"

/*
 *  Force disable THREAD
 */
#undef LISP_THREAD


/*
 *  Mode
 */
#if defined(LISP_MODE_CORE)
#undef LISP_MODE_STANDALONE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "core"

#elif defined(LISP_MODE_STANDALONE)
#undef LISP_MODE_CORE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "standalone"

#elif defined(LISP_MODE_DEGRADE)
#undef LISP_MODE_CORE
#undef LISP_MODE_STANDALONE
#define LISP_MODE_STRING "degrade"

#else
#undef LISP_MODE_CORE
#define LISP_MODE_STANDALONE
#undef LISP_MODE_DEGRADE
#define LISP_MODE_STRING "standalone"
#endif


/*
 *  Version
 */
#ifndef LISP_REVISION
#define LISP_REVISION "0"
#endif

#ifndef LISP_VERSION_A
#define LISP_VERSION_A 0
#endif
#ifndef LISP_VERSION_B
#define LISP_VERSION_B 0
#endif
#ifndef LISP_VERSION_C
#define LISP_VERSION_C 0
#endif


/*
 *  Pointer size
 *    LISP_ARCH_64BIT
 *    LISP_ARCH_32BIT
 */
#undef LISP_ARCH_64BIT
#undef LISP_ARCH_32BIT

#if (SIZEOF_VOID_P == 8)
#define LISP_ARCH_MODE "64bit"
#define LISP_ARCH_64BIT
#elif (SIZEOF_VOID_P == 4)
#define LISP_ARCH_MODE "32bit"
#define LISP_ARCH_32BIT
#elif (0xFFFFFFFFUL < SIZE_MAX)
#define LISP_ARCH_MODE "64bit"
#define LISP_ARCH_64BIT
#else
#define LISP_ARCH_MODE "32bit"
#define LISP_ARCH_32BIT
#endif


/*
 *  Select architecture
 */
#if defined(LISP_ANSIC)
/* ANSI C */
#define LISP_MODE "ANSI-C"
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_WINDOWS
#ifdef LISP_THREAD
#error Arch error, LISP_ANSIC do not allow LISP_THREAD.
#endif
#define LISP_THREAD_REMOVE

#elif defined(LISP_FREEBSD)
/* FreeBSD */
#define LISP_MODE "FreeBSD"
#define LISP_UNIX
#undef LISP_ANSIC
#undef LISP_LINUX
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_FREEBSD
#endif

#elif defined(LISP_LINUX)
/* Linux */
#define LISP_MODE "Linux"
#define LISP_UNIX
#undef LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_LINUX
#endif

#elif defined(LISP_WINDOWS)
/* Windows */
#define LISP_MODE "Windows"
#undef LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#define LISP_THREAD_WINDOWS
#endif

#else
/* ANSI C [default] */
#define LISP_MODE "ANSI-C"
#define LISP_ANSIC
#undef LISP_FREEBSD
#undef LISP_LINUX
#undef LISP_UNIX
#undef LISP_WINDOWS
#undef LISP_ANSIC_WINDOWS
#ifdef LISP_THREAD
#error Arch error
#endif
#define LISP_THREAD_REMOVE
#endif

#if defined(LISP_ANSIC) && defined(LISP_ANSIC_WINDOWS)
#define LISP_MODE "ANSI-C [Windows]"
#endif


/* Windows */
#if defined(LISP_WINDOWS) || defined(LISP_ANSIC_WINDOWS)
#define LISP_WINDOWS_OR_ANSI
#else
#undef LISP_WINDOWS_OR_ANSI
#endif

#ifdef LISP_WINDOWS_OR_ANSI
#if defined(LISP_CONSOLE)
#undef LISP_WINMAIN
#elif defined(LISP_WINMAIN)
#undef LISP_CONSOLE
#else
#define LISP_CONSOLE
#undef LISP_WINMAIN
#endif

#if defined(LISP_CYGWIN)
#undef LISP_NO_CYGWIN
#elif defined(LISP_NO_CYGWIN)
#undef LISP_CYGWIN
#elif defined(__CYGWIN__)
#define LISP_CYGWIN
#undef LISP_NO_CYGWIN
#else
#undef LISP_CYGWIN
#define LISP_NO_CYGWIN
#endif

#else
#undef LISP_CONSOLE
#undef LISP_WINMAIN
#undef LISP_CYGWIN
#undef LISP_NO_CYGWIN
#endif

#if defined(LISP_WINMAIN) && defined(LISP_CYGWIN)
#error Platform cygwin must use a main function (not WinMain).
#endif

#if defined(LISP_WINDOWS_OR_ANSI) || defined(LISP_NO_CYGWIN)
#define LISP_WINDOWS_WIDE
#else
#undef LISP_WINDOWS_WIDE
#endif


/*
 *  Lisp mode
 *    LISP_64BIT  [default]
 *    LISP_32BIT
 */
#if defined(LISP_64BIT)
#undef LISP_32BIT
#elif defined(LISP_32BIT)
#undef LISP_64BIT
#else
#if defined(LISP_ARCH_64BIT)
#define LISP_64BIT
#else
#define LISP_32BIT
#endif
#endif

#ifdef LISP_64BIT
#define LISP_FIXNUM_MODE "64bit"
#else
#define LISP_FIXNUM_MODE "32bit"
#endif

#if defined(LISP_64BIT) && defined(LISP_ARCH_32BIT)
#error arch error.
#endif


/*
 *  Thread mode
 *    LISP_THREAD_SINGLE      -> add LISP_THREAD_DISABLE  [default]
 *    LISP_THREAD_REMOVE      -> add LISP_THREAD_DISABLE
 *    LISP_THREAD_FREEBSD     -> add LISP_THREAD_UNIX
 *    LISP_THREAD_LINUX       -> add LISP_THREAD_UNIX
 *    LISP_THREAD_WINDOWS     -> use Vista module
 */
#undef LISP_THREAD_ENABLE
#undef LISP_THREAD_DISABLE

/* single thread */
#if defined(LISP_THREAD_SINGLE)
#define LISP_THREAD_MODE     "single"
#define LISP_THREAD_DISABLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* single thread [remove] */
#elif defined(LISP_THREAD_REMOVE)
#define LISP_THREAD_MODE     "remove"
#define LISP_THREAD_DISABLE
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* FreeBSD */
#elif defined(LISP_THREAD_FREEBSD)
#define LISP_THREAD_MODE     "FreeBSD"
#define LISP_THREAD_UNIX
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* FreeBSD */
#elif defined(LISP_THREAD_UNIX)
#define LISP_THREAD_MODE     "FreeBSD"
#define LISP_THREAD_FREEBSD
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS

/* Linux */
#elif defined(LISP_THREAD_LINUX)
#define LISP_THREAD_MODE     "Linux"
#define LISP_THREAD_UNIX
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_WINDOWS

/* Windows */
#elif defined(LISP_THREAD_WINDOWS)
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_REMOVE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#define LISP_THREAD_MODE     "Windows"

/* single thread */
#else
#define LISP_THREAD_MODE     "remove"
#define LISP_THREAD_DISABLE
#define LISP_THREAD_REMOVE
#undef LISP_THREAD_SINGLE
#undef LISP_THREAD_UNIX
#undef LISP_THREAD_FREEBSD
#undef LISP_THREAD_LINUX
#undef LISP_THREAD_WINDOWS
#endif

/* thread enable */
#ifndef LISP_THREAD_DISABLE
#define LISP_THREAD_ENABLE
#endif

/* mode string */
#ifdef LISP_DEBUG
#define LISP_DEBUG_STRING "debug"
#else
#define LISP_DEBUG_STRING "release"
#endif

#ifdef LISP_DEGRADE
#define LISP_DEGRADE_STRING "degrade"
#else
#define LISP_DEGRADE_STRING "release"
#endif

/* memory size */
#ifndef LISP_MEMORY_HEAP
#ifdef LISP_DEBUG
#define LISP_MEMORY_HEAP	(64UL * 1024UL * 1024UL);
#else
#define LISP_MEMORY_HEAP	(1024UL * 1024UL * 1024UL);
#endif
#endif

#ifndef LISP_MEMORY_LOCAL
#ifdef LISP_DEBUG
#define LISP_MEMORY_LOCAL	(16UL * 1024UL * 1024UL);
#else
#define LISP_MEMORY_LOCAL	(256UL * 1024UL * 1024UL);
#endif
#endif

/* memory management */
#if defined(LISP_MEMORY_INIT)
#undef LISP_MEMORY_UNINIT
#elif defined(LISP_MEMORY_UNINIT)
#undef LISP_MEMORY_INIT
#elif defined(LISP_DEBUG)
#define LISP_MEMORY_INIT
#undef LISP_MEMORY_UNINIT
#else
#undef LISP_MEMORY_INIT
#define LISP_MEMORY_UNINIT
#endif

/* garbage collection */
#ifdef LISP_DEBUG
#ifndef LISP_DEBUG_MEMORY
#define LISP_DEBUG_MEMORY
#endif
#endif
/* #define LISP_DEBUG_FORCE_GC 1 */

/* stream */
#ifndef LISP_STREAM_EXTEND
#define LISP_STREAM_EXTEND	3
#endif

/* pointer_table */
#ifndef LISP_POINTER_EXTEND
#define LISP_POINTER_EXTEND	32
#endif

/* long float */
#if defined(LISP_FLOAT_LONG_64)
#define LISP_FLOAT_LONG			64
#undef LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#elif defined(LISP_FLOAT_LONG_80)
#define LISP_FLOAT_LONG			80
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_128
#elif defined(LISP_FLOAT_LONG_128)
#define LISP_FLOAT_LONG			128
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#else
#if (LDBL_MANT_DIG == DBL_MANT_DIG)
/* Visual Studio
 *   long double == double
 */
#define LISP_FLOAT_LONG			64
#define LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#elif (LDBL_MANT_DIG == 64)
/* Intel x86
 *   long double (Intel 80bit) fraction: 63+0 bit (64bit)
 */
#define LISP_FLOAT_LONG			80
#undef LISP_FLOAT_LONG_64
#define LISP_FLOAT_LONG_80
#undef LISP_FLOAT_LONG_128
#else
/* IEEE745
 * long double (IEEE-754 binary128) fraction: 112+1 bit
 */
#define LISP_FLOAT_LONG			128
#undef LISP_FLOAT_LONG_64
#undef LISP_FLOAT_LONG_80
#define LISP_FLOAT_LONG_128
#endif
#endif

/* float (32bit) fraction: 23+1 bit */
#define LISP_FLOAT_SINGLE_FRACTION		FLT_MANT_DIG
/* double (64bit) fraction: 52+1 bit */
#define LISP_FLOAT_DOUBLE_FRACTION		DBL_MANT_DIG
/* long double */
#define LISP_FLOAT_LONG_FRACTION		LDBL_MANT_DIG

/* readline editline */
#if defined(LISP_TERME)
#define LISP_PROMPT_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#undef LISP_STDIN

#elif defined(LISP_EDITLINE)
#define LISP_PROMPT_EDITLINE
#undef LISP_TERME
#undef LISP_READLINE
#undef LISP_STDIN

#elif defined(LISP_READLINE)
#define LISP_PROMPT_READLINE
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_STDIN

#elif defined(LISP_STDIN)
#define LISP_PROMPT_DISABLE
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE

#elif defined(LISP_FREEBSD) || defined(LISP_LINUX)
#define LISP_TERME
#define LISP_PROMPT_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#undef LISP_STDIN

#else
#define LISP_PROMPT_DISABLE
#define LISP_STDIN
#undef LISP_TERME
#undef LISP_EDITLINE
#undef LISP_READLINE
#endif

#if defined(LISP_PROMPT_TERME)
#define LISP_PROMPT_STRING "terme"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_DISABLE)
#define LISP_PROMPT_STRING "stdin"
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_READLINE)
#define LISP_PROMPT_STRING "readline"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_EDITLINE

#elif defined(LISP_PROMPT_EDITLINE)
#define LISP_PROMPT_STRING "editline"
#undef LISP_PROMPT_DISABLE
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE

#else
#define LISP_PROMPT_DISABLE
#define LISP_PROMPT_STRING "stdin"
#undef LISP_PROMPT_TERME
#undef LISP_PROMPT_READLINE
#undef LISP_PROMPT_EDITLINE
#endif

/* terme */
#ifdef LISP_TERME

/* code */
#if defined(LISP_FREEBSD) || defined(LISP_LINUX)
#define LISP_TERME_UNIX
#else
#undef LISP_TERME_UNIX
#endif

#ifdef LISP_WINDOWS
#define LISP_TERME_WINDOWS
#else
#undef LISP_TERME_WINDOWS
#endif

/* bright / dark */
#if defined(LISP_TERME_BRIGHT)
#undef LISP_TERME_DARK
#define LISP_TERME_COLOR1	"bright"
#elif defined(LISP_TERME_DARK)
#undef LISP_TERME_BRIGHT
#define LISP_TERME_COLOR1	"dark"
#else
#define LISP_TERME_BRIGHT
#define LISP_TERME_COLOR1	"bright"
#endif

/* monochrome / color */
#if defined(LISP_TERME_COLOR)
#undef LISP_TERME_MONOCHROME
#define LISP_TERME_COLOR2	"on"
#elif defined(LISP_TERME_MONOCHROME)
#undef LISP_TERME_COLOR
#define LISP_TERME_COLOR2	"off"
#else
#define LISP_TERME_COLOR
#define LISP_TERME_COLOR2	"on"
#endif
#endif

/* show-window [Windows] */
#ifdef LISP_TERME_WINDOWS
#if defined(LISP_TERME_DEFAULT_WINDOW)
#undef LISP_TERME_HIDE_WINDOW
#elif defined(LISP_TERME_HIDE_WINDOW)
#undef LISP_TERME_DEFAULT_WINDOW
#else
#define LISP_TERME_DEFAULT_WINDOW
#endif
#endif

/* Complex math library */
#undef LISP_COMPLEX_INACCURACY
/* Visual C++ */
#if defined(LISP_COMPLEX_WINDOWS)
#undef LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#define LISP_COMPLEX_INACCURACY
/* C++ */
#elif defined(LISP_COMPLEX_CPLUSPLUS)
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_C99
#ifdef __clang__
#define LISP_COMPLEX_INACCURACY
#endif
/* c99 */
#elif defined(LISP_COMPLEX_C99)
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
/* C++ */
#elif defined(__cplusplus)
#undef LISP_COMPLEX_WINDOWS
#define LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#ifdef __clang__
#define LISP_COMPLEX_INACCURACY
#endif
/* Visual C++ */
#elif defined(_MSC_VER)
#define LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
#undef LISP_COMPLEX_C99
#define LISP_COMPLEX_INACCURACY
/* c99 */
#else
#undef LISP_COMPLEX_WINDOWS
#undef LISP_COMPLEX_CPLUSPLUS
#define LISP_COMPLEX_C99
#endif

/* complex-long */
#if defined(LISP_COMPLEX_LONG)
#undef LISP_COMPLEX_DOUBLE
#elif defined(LISP_COMPLEX_DOUBLE)
#undef LISP_COMPLEX_LONG
#elif defined(LISP_FREEBSD)
#define LISP_COMPLEX_DOUBLE
#elif defined(__FreeBSD__)
#define LISP_COMPLEX_DOUBLE
#else
#define LISP_COMPLEX_LONG
#endif

/* windows */
#ifdef _MSC_VER
#pragma warning(disable:4996)
#endif

/* main */
#if defined(LISP_WINMAIN) || defined(LISP_WINDOWS_WIDE)
#define LISP_WINMAIN_WIDE
#else
#undef LISP_WINMAIN_WIDE
#endif

/* setjmp */
#ifdef __cplusplus
#ifdef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#else
#undef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#define LISP_ABORT_EXCEPTION
#endif
#else
#undef LISP_ABORT_SETJMP
#undef LISP_ABORT_EXCEPTION
#define LISP_ABORT_SETJMP
#endif

/* dynamic-link */
#ifdef LISP_WINDOWS
#ifndef LISP_DYNAMIC_LINK
#define LISP_DYNAMIC_LINK
#endif
#endif

/* end of header file */
#endif

