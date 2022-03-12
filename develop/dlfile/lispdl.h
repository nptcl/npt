/*
 *  npt -- ANSI Common Lisp Programming Language.
 *    https://github.com/nptcl/npt
 *    https://github.com/nptcl/npt-amalgamation
 *
 *  File: lispdl.h
 */
#ifndef __LISPDL_HEADER__
#define __LISPDL_HEADER__

#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#endif

#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>


/************************************************************
 *  define.h
 ************************************************************/
#ifndef __LISP_DEFINE_HEADER__
#define __LISP_DEFINE_HEADER__

/*
 *  autoconf
 */
#ifdef HAVE_CONFIG_H
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



/************************************************************
 *  typedef_basic.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_BASIC_HEADER__
#define __LISP_TYPEDEF_BASIC_HEADER__


typedef unsigned char byte;
typedef uint16_t byte16;
typedef uint32_t byte32;
typedef uint64_t byte64;
typedef float short_float;
typedef float single_float;
typedef double double_float;
typedef long double long_float;
typedef uint32_t unicode;
typedef byte *pbyte;
typedef byte *addr;

#endif



/************************************************************
 *  typedef_integer.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_INTEGER_HEADER__
#define __LISP_TYPEDEF_INTEGER_HEADER__


#ifdef LISP_64BIT
#define LISP_INFO           "64bit-code"
#define LISP_INTEGER_BIT	64
#define LISP_INTEGER_MASK	UINT64_MAX
#define FIXNUM_MAX			INT64_MAX
#define FIXNUM_MIN			INT64_MIN
typedef int64_t fixnum;
typedef uint64_t fixed;
#define PRIdF PRId64
#define PRIuF PRIu64
#define PRIxF PRIx64
#define PRIXF PRIX64
#else
#define LISP_INFO           "32bit-code"
#define LISP_INTEGER_BIT	32
#define LISP_INTEGER_MASK	UINT32_MAX
#define FIXNUM_MAX			INT32_MAX
#define FIXNUM_MIN			INT32_MIN
typedef int32_t fixnum;
typedef uint32_t fixed;
#define PRIdF PRId32
#define PRIuF PRIu32
#define PRIxF PRIx32
#define PRIXF PRIX32
#endif

#define FIXNUM_UMIN			((fixed)FIXNUM_MIN)

#endif



/************************************************************
 *  typedef_stream.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_STREAM_HEADER__
#define __LISP_TYPEDEF_STREAM_HEADER__


typedef int (*lisp_streamtype_close)(addr, addr *);
typedef int (*lisp_streamtype_read_byte)(addr, addr *, int *);
typedef int (*lisp_streamtype_unread_byte)(addr, byte);
typedef int (*lisp_streamtype_write_byte)(addr, addr);
typedef int (*lisp_streamtype_read_char)(addr, unicode *, int *);
typedef int (*lisp_streamtype_read_hang)(addr, unicode *, int *, int *);
typedef int (*lisp_streamtype_unread_char)(addr, unicode);
typedef int (*lisp_streamtype_write_char)(addr, unicode);
typedef int (*lisp_streamtype_getleft)(addr, size_t *);
typedef int (*lisp_streamtype_setleft)(addr, size_t);
typedef int (*lisp_streamtype_inputp)(addr, int *);
typedef int (*lisp_streamtype_outputp)(addr, int *);
typedef int (*lisp_streamtype_interactivep)(addr, int *);
typedef int (*lisp_streamtype_characterp)(addr, int *);
typedef int (*lisp_streamtype_binaryp)(addr, int *);
typedef int (*lisp_streamtype_element_type)(addr, addr *);
typedef int (*lisp_streamtype_external_format)(addr, addr *);
typedef int (*lisp_streamtype_file_length)(addr, addr *);
typedef int (*lisp_streamtype_file_position)(addr, size_t *, int *);
typedef int (*lisp_streamtype_file_position_start)(addr, int *);
typedef int (*lisp_streamtype_file_position_end)(addr, int *);
typedef int (*lisp_streamtype_file_position_set)(addr, size_t, int *);
typedef int (*lisp_streamtype_file_charlen)(addr, unicode, size_t *, int *);
typedef int (*lisp_streamtype_file_strlen)(addr, addr, size_t *, int *);
typedef int (*lisp_streamtype_listen)(addr, int *);
typedef int (*lisp_streamtype_clear_input)(addr);
typedef int (*lisp_streamtype_finish_output)(addr);
typedef int (*lisp_streamtype_force_output)(addr);
typedef int (*lisp_streamtype_clear_output)(addr);
typedef int (*lisp_streamtype_exitpoint)(addr);
typedef int (*lisp_streamtype_termsize)(addr, size_t *, int *);

#endif



/************************************************************
 *  typedef.h
 ************************************************************/
#ifndef __LISP_TYPEDEF_HEADER__
#define __LISP_TYPEDEF_HEADER__


/* setjmp */
typedef void (*lisp_abort_calltype)(void);
typedef int (*lisp_equal_calltype)(addr, addr, int *);

/* code */
union CODEVALUE {
	fixnum value;
	unicode character;
	size_t index;
	addr pos;
};
typedef union CODEVALUE CodeValue;

#endif



/************************************************************
 *  define_setjmp.h
 ************************************************************/
#ifndef __LISP_DEFINE_SETJMP__
#define __LISP_DEFINE_SETJMP__


/* abort */
#ifdef LISP_ABORT_SETJMP
extern jmp_buf Lisp_abort_setjmp;
#define Lisp_abort_throw()  longjmp(Lisp_abort_setjmp, 1)
#define Lisp_abort_Begin    if (setjmp(Lisp_abort_setjmp) == 0)
#define Lisp_abort_End      memset(&Lisp_abort_setjmp, 0, sizeof(Lisp_abort_setjmp))
#else
class Lisp_abort_class {};
#define Lisp_abort_throw()  throw Lisp_abort_class()
#define Lisp_abort_Begin    try
#define Lisp_abort_End      catch (Lisp_abort_class) {}
#endif

/* degrade */
#ifdef LISP_ABORT_SETJMP
extern jmp_buf Lisp_degrade_setjmp;
#define Lisp_degrade_throw()	longjmp(Lisp_degrade_setjmp, 1)
#define Lisp_degrade_Begin		if (setjmp(Lisp_degrade_setjmp) == 0)
#define Lisp_degrade_End		memset(&Lisp_degrade_setjmp, 0, sizeof(Lisp_degrade_setjmp))
#else
class Lisp_degrade_class {};
#define Lisp_degrade_throw()	throw Lisp_degrade_class()
#define Lisp_degrade_Begin		try
#define Lisp_degrade_End		catch (Lisp_degrade_class) {}
#endif

#endif



/************************************************************
 *  extern_typedef.h
 ************************************************************/
#ifndef __LISP_EXTERN_TYPEDEF_HEADER__
#define __LISP_EXTERN_TYPEDEF_HEADER__


enum lisp_escape {
	lisp_escape_normal,
	lisp_escape_tagbody,
	lisp_escape_block,
	lisp_escape_catch,
	lisp_escape_handler_case,
	lisp_escape_restart_case
};

enum LispEastAsianType {
	LispEastAsianType_error,
	LispEastAsianType_N,
	LispEastAsianType_A,
	LispEastAsianType_H,
	LispEastAsianType_W,
	LispEastAsianType_F,
	LispEastAsianType_NA
};

typedef int (*lisp_calltype_macro)(addr form, addr env);
typedef int (*lisp_calltype_rest)(addr args);
typedef int (*lisp_calltype_dynamic)(addr args);
typedef int (*lisp_calltype_any)(void);
typedef int (*lisp_calltype_empty)(void);
typedef int (*lisp_calltype_var1)(addr);
typedef int (*lisp_calltype_var2)(addr, addr);
typedef int (*lisp_calltype_var3)(addr, addr, addr);
typedef int (*lisp_calltype_var4)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var5)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var6)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_opt1)(addr);
typedef int (*lisp_calltype_opt2)(addr, addr);
typedef int (*lisp_calltype_opt3)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt1)(addr, addr);
typedef int (*lisp_calltype_var1opt2)(addr, addr, addr);
typedef int (*lisp_calltype_var1opt3)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt1)(addr, addr, addr);
typedef int (*lisp_calltype_var2opt2)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var2opt3)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt1)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt2)(addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var3opt3)(addr, addr, addr, addr, addr, addr);
typedef int (*lisp_calltype_var1rest)(addr, addr);
typedef int (*lisp_calltype_var2rest)(addr, addr, addr);
typedef int (*lisp_calltype_var3rest)(addr, addr, addr, addr);
typedef int (*lisp_calltype_var1dynamic)(addr, addr);
typedef int (*lisp_calltype_var2dynamic)(addr, addr, addr);
typedef int (*lisp_calltype_var3dynamic)(addr, addr, addr, addr);

#endif

enum lisp_dlfile_calltype {
    /* variable */
    lispdl_nil,
    lispdl_t,

    /* extern_type.h */
    lispdl_lisp_hold_p,
    lispdl_lisp_hold_value,
    lispdl_lisp_hold_set,
    lispdl_Lisp_holdv,
    lispdl_lisp_hold,
    lispdl_Lisp_hold,
    lispdl_lisp0_nil,
    lispdl_lisp0_t,
    lispdl_lisp_nil,
    lispdl_lisp_t,
    lispdl_Lisp_nil,
    lispdl_Lisp_t,
    lispdl_lisp_nil_p,
    lispdl_lisp_t_p,
    lispdl_lisp_null_p,
    lispdl_lisp_character_p,
    lispdl_lisp_cons_p,
    lispdl_lisp_list_p,
    lispdl_lisp_string_p,
    lispdl_lisp_strvect_p,
    lispdl_lisp_symbol_p,
    lispdl_lisp_array_p,
    lispdl_lisp_vector_p,
    lispdl_lisp_fixnum_p,
    lispdl_lisp_bignum_p,
    lispdl_lisp_integer_p,
    lispdl_lisp_ratio_p,
    lispdl_lisp_rational_p,
    lispdl_lisp_single_float_p,
    lispdl_lisp_double_float_p,
    lispdl_lisp_long_float_p,
    lispdl_lisp_float_p,
    lispdl_lisp_real_p,
    lispdl_lisp_complex_p,
    lispdl_lisp_number_p,
    lispdl_lisp_clos_p,
    lispdl_lisp_hashtable_p,
    lispdl_lisp_readtable_p,
    lispdl_lisp_control_p,
    lispdl_lisp_callname_p,
    lispdl_lisp_function_p,
    lispdl_lisp_package_p,
    lispdl_lisp_random_state_p,
    lispdl_lisp_pathname_p,
    lispdl_lisp_stream_p,
    lispdl_lisp_restart_p,
    lispdl_lisp_environment_p,
    lispdl_lisp_bitvector_p,
    lispdl_lisp_print_dispatch_p,
    lispdl_lisp_paper_p,

    /* extern_sequence.h */
    lispdl_lisp0_cons,
    lispdl_lisp_cons,
    lispdl_lisp0_vector,
    lispdl_lisp_vector,
    lispdl_lisp0_list_va,
    lispdl_lisp0_lista_va,
    lispdl_lisp0_list,
    lispdl_lisp_list,
    lispdl_lisp0_lista,
    lispdl_lisp_lista,
    lispdl_lisp0_getelt_,
    lispdl_lisp_getelt_,
    lispdl_lisp_setelt_,
    lispdl_lisp_length_,
    lispdl_lisp0_reverse_,
    lispdl_lisp0_nreverse_,
    lispdl_lisp_reverse_,
    lispdl_lisp_nreverse_,
    lispdl_lisp0_car,
    lispdl_lisp0_cdr,
    lispdl_lisp0_carcdr,
    lispdl_lisp_car,
    lispdl_lisp_cdr,
    lispdl_lisp_carcdr,
    lispdl_lisp_setf_car,
    lispdl_lisp_setf_cdr,
    lispdl_lisp_setf_carcdr,
    lispdl_lisp0_string8_,
    lispdl_lisp0_string16_,
    lispdl_lisp0_string32_,
    lispdl_lisp_string8_,
    lispdl_lisp_string16_,
    lispdl_lisp_string32_,
    lispdl_lisp_string_getc_,
    lispdl_lisp_strvect_getc,
    lispdl_lisp_strvect_length,

    /* extern_object.h */
    lispdl_lisp0_character_,
    lispdl_lisp0_fixnum,
    lispdl_lisp0_float_,
    lispdl_lisp0_double_,
    lispdl_lisp0_long_double_,
    lispdl_lisp_character_,
    lispdl_lisp_fixnum,
    lispdl_lisp_float_,
    lispdl_lisp_double_,
    lispdl_lisp_long_double_,
    lispdl_lisp_zero_p,
    lispdl_lisp_plus_p,
    lispdl_lisp_minus_p,
    lispdl_lisp_get_character,
    lispdl_lisp_get_fixnum,
    lispdl_lisp_get_float_,
    lispdl_lisp_get_double_,
    lispdl_lisp_get_long_double_,
    lispdl_lisp0_package_,
    lispdl_lisp0_package8_,
    lispdl_lisp0_package16_,
    lispdl_lisp0_package32_,
    lispdl_lisp_package_,
    lispdl_lisp_package8_,
    lispdl_lisp_package16_,
    lispdl_lisp_package32_,
    lispdl_lisp_in_package_,
    lispdl_lisp_in_package8_,
    lispdl_lisp_in_package16_,
    lispdl_lisp_in_package32_,
    lispdl_lisp_push_and_in_package_,
    lispdl_lisp_push_and_in_package8_,
    lispdl_lisp_push_and_in_package16_,
    lispdl_lisp_push_and_in_package32_,
    lispdl_lisp0_intern_,
    lispdl_lisp0_intern8_,
    lispdl_lisp0_intern16_,
    lispdl_lisp0_intern32_,
    lispdl_lisp_intern_,
    lispdl_lisp_intern8_,
    lispdl_lisp_intern16_,
    lispdl_lisp_intern32_,
    lispdl_lisp0_reader_,
    lispdl_lisp0_reader8_,
    lispdl_lisp0_reader16_,
    lispdl_lisp0_reader32_,
    lispdl_lisp_reader_,
    lispdl_lisp_reader8_,
    lispdl_lisp_reader16_,
    lispdl_lisp_reader32_,
    lispdl_lisp0_pathname_,
    lispdl_lisp0_pathname8_,
    lispdl_lisp0_pathname16_,
    lispdl_lisp0_pathname32_,
    lispdl_lisp0_namestring_,
    lispdl_lisp_pathname_,
    lispdl_lisp_pathname8_,
    lispdl_lisp_pathname16_,
    lispdl_lisp_pathname32_,
    lispdl_lisp_namestring_,
    lispdl_lisp0_paper_,
    lispdl_lisp_paper_,
    lispdl_lisp_paper_gettype_,
    lispdl_lisp_paper_settype_,
    lispdl_lisp_paper_lenarray_,
    lispdl_lisp_paper_lenbody_,
    lispdl_lisp0_paper_getarray_,
    lispdl_lisp_paper_getarray_,
    lispdl_lisp_paper_setarray_,
    lispdl_lisp_paper_getbody_,
    lispdl_lisp_paper_setbody_,
    lispdl_lisp_paper_getmemory_,
    lispdl_lisp_paper_setmemory_,
    lispdl_lisp_paper_body_unsafe_,

    /* extern_init.h */
    lispdl_lisperror_stream,
    lispdl_lisperror_noeol,
    lispdl_lisperror_va,
    lispdl_lisperror,

    /* extern_execute.h */
    lispdl_lisp0_eval_,
    lispdl_lisp0_eval8_,
    lispdl_lisp0_eval16_,
    lispdl_lisp0_eval32_,
    lispdl_lisp_eval_,
    lispdl_lisp_eval8_,
    lispdl_lisp_eval16_,
    lispdl_lisp_eval32_,
    lispdl_lisp0_call_,
    lispdl_lisp_call_,
    lispdl_lisp0_funcall_,
    lispdl_lisp0_funcall8_,
    lispdl_lisp0_funcall16_,
    lispdl_lisp0_funcall32_,
    lispdl_lisp_funcall_,
    lispdl_lisp_funcall8_,
    lispdl_lisp_funcall16_,
    lispdl_lisp_funcall32_,
    lispdl_lisp0_apply_,
    lispdl_lisp0_apply8_,
    lispdl_lisp0_apply16_,
    lispdl_lisp0_apply32_,
    lispdl_lisp_apply_,
    lispdl_lisp_apply8_,
    lispdl_lisp_apply16_,
    lispdl_lisp_apply32_,
    lispdl_lisp_eval_control_,
    lispdl_lisp_eval_string_control_,
    lispdl_lisp_call_control_,
    lispdl_lisp_funcall_control_,
    lispdl_lisp_apply_control_,
    lispdl_lisp0_result_control,
    lispdl_lisp0_result2_control,
    lispdl_lisp0_values_control,
    lispdl_lisp0_nth_value_control,
    lispdl_lisp_result_control,
    lispdl_lisp_result2_control,
    lispdl_lisp_values_control,
    lispdl_lisp_nth_value_control,
    lispdl_lisp_set_result_control,
    lispdl_lisp_set_values_control,
    lispdl_lisp_set_values_nil_control,
    lispdl_lisp_set_values_list_control,
    lispdl_lisp_equal_control,
    lispdl_lisp_break_control,
    lispdl_lisp_escape_control,
    lispdl_lisp_reset_control,
    lispdl_lisp_escape_type_control,
    lispdl_lisp_save_control,
    lispdl_lisp_rollback_control,
    lispdl_lisp_eval_loop_,

    /* extern_control.h */
    lispdl_lisp_push_control,
    lispdl_lisp_pop_control_,
    lispdl_lisp_push_special_,
    lispdl_lisp_push_special8_,
    lispdl_lisp_push_special16_,
    lispdl_lisp_push_special32_,
    lispdl_lisp0_get_special_,
    lispdl_lisp0_get_special8_,
    lispdl_lisp0_get_special16_,
    lispdl_lisp0_get_special32_,
    lispdl_lisp_get_special_,
    lispdl_lisp_get_special8_,
    lispdl_lisp_get_special16_,
    lispdl_lisp_get_special32_,
    lispdl_lisp_set_special_,
    lispdl_lisp_set_special8_,
    lispdl_lisp_set_special16_,
    lispdl_lisp_set_special32_,
    lispdl_lisp_defvar_,
    lispdl_lisp_defvar8_,
    lispdl_lisp_defvar16_,
    lispdl_lisp_defvar32_,
    lispdl_lisp_catch,
    lispdl_lisp_throw_,
    lispdl_lisp_handler_bind_,
    lispdl_lisp_handler_case_,
    lispdl_lisp_handler_reverse,
    lispdl_lisp0_restart_make,
    lispdl_lisp_restart_make,
    lispdl_lisp_restart_interactive,
    lispdl_lisp_restart_report,
    lispdl_lisp_restart_test,
    lispdl_lisp_restart_push,
    lispdl_lisp_restart_reverse,

    /* extern_error.h */
    lispdl_lisp_abort,
    lispdl_lisp_abortf,
    lispdl_lisp_abort8,
    lispdl_lisp_abort16,
    lispdl_lisp_abort32,
    lispdl_lisp_set_abort_handler,
    lispdl_lisp_set_abort_setjmp_handler,
    lispdl_lisp_signal_,
    lispdl_lisp_error_,
    lispdl_lisp_error8_,
    lispdl_lisp_error16_,
    lispdl_lisp_error32_,
    lispdl_lisp_warn8_,
    lispdl_lisp_warn16_,
    lispdl_lisp_warn32_,

    /* extern_function.h */
    lispdl_lisp0_get_function,
    lispdl_lisp0_get_setf,
    lispdl_lisp_get_function,
    lispdl_lisp_get_setf,
    lispdl_lisp0_get_function_,
    lispdl_lisp0_get_function8_,
    lispdl_lisp0_get_function16_,
    lispdl_lisp0_get_function32_,
    lispdl_lisp_get_function_,
    lispdl_lisp_get_function8_,
    lispdl_lisp_get_function16_,
    lispdl_lisp_get_function32_,
    lispdl_lisp0_get_setf_,
    lispdl_lisp0_get_setf8_,
    lispdl_lisp0_get_setf16_,
    lispdl_lisp0_get_setf32_,
    lispdl_lisp_get_setf_,
    lispdl_lisp_get_setf8_,
    lispdl_lisp_get_setf16_,
    lispdl_lisp_get_setf32_,
    lispdl_lisp_compiled_macro,
    lispdl_lisp_compiled_rest,
    lispdl_lisp_compiled_dynamic,
    lispdl_lisp_compiled_any,
    lispdl_lisp_compiled_empty,
    lispdl_lisp_compiled_var1,
    lispdl_lisp_compiled_var2,
    lispdl_lisp_compiled_var3,
    lispdl_lisp_compiled_var4,
    lispdl_lisp_compiled_var5,
    lispdl_lisp_compiled_var6,
    lispdl_lisp_compiled_opt1,
    lispdl_lisp_compiled_opt2,
    lispdl_lisp_compiled_opt3,
    lispdl_lisp_compiled_var1opt1,
    lispdl_lisp_compiled_var1opt2,
    lispdl_lisp_compiled_var1opt3,
    lispdl_lisp_compiled_var2opt1,
    lispdl_lisp_compiled_var2opt2,
    lispdl_lisp_compiled_var2opt3,
    lispdl_lisp_compiled_var3opt1,
    lispdl_lisp_compiled_var3opt2,
    lispdl_lisp_compiled_var3opt3,
    lispdl_lisp_compiled_var1rest,
    lispdl_lisp_compiled_var2rest,
    lispdl_lisp_compiled_var3rest,
    lispdl_lisp_compiled_var1dynamic,
    lispdl_lisp_compiled_var2dynamic,
    lispdl_lisp_compiled_var3dynamic,
    lispdl_lisp0_compiled_function_,
    lispdl_lisp0_compiled_function8_,
    lispdl_lisp0_compiled_function16_,
    lispdl_lisp0_compiled_function32_,
    lispdl_lisp_compiled_function_,
    lispdl_lisp_compiled_function8_,
    lispdl_lisp_compiled_function16_,
    lispdl_lisp_compiled_function32_,
    lispdl_lisp_compiled_defun_,
    lispdl_lisp_compiled_defun8_,
    lispdl_lisp_compiled_defun16_,
    lispdl_lisp_compiled_defun32_,
    lispdl_lisp_compiled_defun_setf_,
    lispdl_lisp_compiled_defun_setf8_,
    lispdl_lisp_compiled_defun_setf16_,
    lispdl_lisp_compiled_defun_setf32_,
    lispdl_lisp_compiled_setvalue,
    lispdl_lisp_compiled_getvalue,

    /* extern_instance.h */
    lispdl_lisp0_find_class,
    lispdl_lisp0_find_class_,
    lispdl_lisp0_find_class8_,
    lispdl_lisp0_find_class16_,
    lispdl_lisp0_find_class32_,
    lispdl_lisp_find_class,
    lispdl_lisp_find_class_,
    lispdl_lisp_find_class8_,
    lispdl_lisp_find_class16_,
    lispdl_lisp_find_class32_,
    lispdl_lisp0_instance_,
    lispdl_lisp0_instance8_,
    lispdl_lisp0_instance16_,
    lispdl_lisp0_instance32_,
    lispdl_lisp_instance_,
    lispdl_lisp_instance8_,
    lispdl_lisp_instance16_,
    lispdl_lisp_instance32_,
    lispdl_lisp_slot_exists_,
    lispdl_lisp_slot_exists8_,
    lispdl_lisp_slot_exists16_,
    lispdl_lisp_slot_exists32_,
    lispdl_lisp_slot_boundp_,
    lispdl_lisp_slot_boundp8_,
    lispdl_lisp_slot_boundp16_,
    lispdl_lisp_slot_boundp32_,
    lispdl_lisp_slot_makunbound_,
    lispdl_lisp_slot_makunbound8_,
    lispdl_lisp_slot_makunbound16_,
    lispdl_lisp_slot_makunbound32_,
    lispdl_lisp0_slot_value_,
    lispdl_lisp0_slot_value8_,
    lispdl_lisp0_slot_value16_,
    lispdl_lisp0_slot_value32_,
    lispdl_lisp_slot_value_,
    lispdl_lisp_slot_value8_,
    lispdl_lisp_slot_value16_,
    lispdl_lisp_slot_value32_,
    lispdl_lisp_slot_setf_,
    lispdl_lisp_slot_setf8_,
    lispdl_lisp_slot_setf16_,
    lispdl_lisp_slot_setf32_,

    /* extern_print.h */
    lispdl_lisp_format8_,
    lispdl_lisp_format16_,
    lispdl_lisp_format32_,
    lispdl_lisp_stdout8_,
    lispdl_lisp_stdout16_,
    lispdl_lisp_stdout32_,
    lispdl_lisp_stderr8_,
    lispdl_lisp_stderr16_,
    lispdl_lisp_stderr32_,
    lispdl_lisp0_stringf8_,
    lispdl_lisp0_stringf16_,
    lispdl_lisp0_stringf32_,
    lispdl_lisp_stringf8_,
    lispdl_lisp_stringf16_,
    lispdl_lisp_stringf32_,

    /* extern_stream.h */
    lispdl_lisp0_stream_define,
    lispdl_lisp_stream_define,
    lispdl_lisp_stream_memory,
    lispdl_lisp0_getinfo_stream,
    lispdl_lisp_getinfo_stream,
    lispdl_lisp_setinfo_stream,
    lispdl_lisp_stream_calltype_close,
    lispdl_lisp_stream_calltype_read_byte,
    lispdl_lisp_stream_calltype_unread_byte,
    lispdl_lisp_stream_calltype_write_byte,
    lispdl_lisp_stream_calltype_read_char,
    lispdl_lisp_stream_calltype_read_hang,
    lispdl_lisp_stream_calltype_unread_char,
    lispdl_lisp_stream_calltype_write_char,
    lispdl_lisp_stream_calltype_getleft,
    lispdl_lisp_stream_calltype_setleft,
    lispdl_lisp_stream_calltype_inputp,
    lispdl_lisp_stream_calltype_outputp,
    lispdl_lisp_stream_calltype_interactivep,
    lispdl_lisp_stream_calltype_characterp,
    lispdl_lisp_stream_calltype_binaryp,
    lispdl_lisp_stream_calltype_element_type,
    lispdl_lisp_stream_calltype_external_format,
    lispdl_lisp_stream_calltype_file_length,
    lispdl_lisp_stream_calltype_file_position,
    lispdl_lisp_stream_calltype_file_position_start,
    lispdl_lisp_stream_calltype_file_position_end,
    lispdl_lisp_stream_calltype_file_position_set,
    lispdl_lisp_stream_calltype_file_charlen,
    lispdl_lisp_stream_calltype_file_strlen,
    lispdl_lisp_stream_calltype_listen,
    lispdl_lisp_stream_calltype_clear_input,
    lispdl_lisp_stream_calltype_finish_output,
    lispdl_lisp_stream_calltype_force_output,
    lispdl_lisp_stream_calltype_clear_output,
    lispdl_lisp_stream_calltype_exitpoint,
    lispdl_lisp_stream_calltype_termsize,
    lispdl_lisp_stream_calltype_error_close,
    lispdl_lisp_stream_calltype_error_read_byte,
    lispdl_lisp_stream_calltype_error_unread_byte,
    lispdl_lisp_stream_calltype_error_write_byte,
    lispdl_lisp_stream_calltype_error_read_char,
    lispdl_lisp_stream_calltype_error_read_hang,
    lispdl_lisp_stream_calltype_error_unread_char,
    lispdl_lisp_stream_calltype_error_write_char,
    lispdl_lisp_stream_calltype_error_getleft,
    lispdl_lisp_stream_calltype_error_setleft,
    lispdl_lisp_stream_calltype_error_inputp,
    lispdl_lisp_stream_calltype_error_outputp,
    lispdl_lisp_stream_calltype_error_interactivep,
    lispdl_lisp_stream_calltype_error_characterp,
    lispdl_lisp_stream_calltype_error_binaryp,
    lispdl_lisp_stream_calltype_error_element_type,
    lispdl_lisp_stream_calltype_error_external_format,
    lispdl_lisp_stream_calltype_error_file_length,
    lispdl_lisp_stream_calltype_error_file_position,
    lispdl_lisp_stream_calltype_error_file_position_start,
    lispdl_lisp_stream_calltype_error_file_position_end,
    lispdl_lisp_stream_calltype_error_file_position_set,
    lispdl_lisp_stream_calltype_error_file_charlen,
    lispdl_lisp_stream_calltype_error_file_strlen,
    lispdl_lisp_stream_calltype_error_listen,
    lispdl_lisp_stream_calltype_error_clear_input,
    lispdl_lisp_stream_calltype_error_finish_output,
    lispdl_lisp_stream_calltype_error_force_output,
    lispdl_lisp_stream_calltype_error_clear_output,
    lispdl_lisp_stream_calltype_error_exitpoint,
    lispdl_lisp_stream_calltype_error_termsize,

    /* extern_unicode.h */
    lispdl_lisp_eastasian_set,
    lispdl_lisp_eastasian_get,
    lispdl_lisp_eastasian_type_unicode,
    lispdl_lisp_eastasian_type_character,
    lispdl_lisp_eastasian_unicode,
    lispdl_lisp_eastasian_character_,
    lispdl_lisp_eastasian_string_,
    lispdl_lisp_eastasian_width_,
    lispdl_lisp_unicode_count,
    lispdl_lisp_utf8_encode,
    lispdl_lisp_utf16_range,
    lispdl_lisp_utf16_high,
    lispdl_lisp_utf16_low,
    lispdl_lisp_utf16_merge,

    /* End */
    lispdl_unbound,
    lispdl_end,
    lispdl_size
};

typedef void *lisp_dlfile_array[lispdl_size];


/*
 *  variables
 */
extern addr lisp_nil_object;
extern addr lisp_t_object;

/* extern_type.h */
extern int (*lisp_hold_p)(addr x);
extern void (*lisp_hold_value)(addr x, addr *ret);
extern void (*lisp_hold_set)(addr x, addr value);
extern addr (*Lisp_holdv)(addr x);
extern void (*lisp_hold)(addr *ret, addr value);
extern addr (*Lisp_hold)(void);
extern void (*lisp0_nil)(addr *ret);
extern void (*lisp0_t)(addr *ret);
extern void (*lisp_nil)(addr x);
extern void (*lisp_t)(addr x);
extern addr (*Lisp_nil)(void);
extern addr (*Lisp_t)(void);
extern int (*lisp_nil_p)(addr x);
extern int (*lisp_t_p)(addr x);
extern int (*lisp_null_p)(addr x);
extern int (*lisp_character_p)(addr x);
extern int (*lisp_cons_p)(addr x);
extern int (*lisp_list_p)(addr x);
extern int (*lisp_string_p)(addr x);
extern int (*lisp_strvect_p)(addr x);
extern int (*lisp_symbol_p)(addr x);
extern int (*lisp_array_p)(addr x);
extern int (*lisp_vector_p)(addr x);
extern int (*lisp_fixnum_p)(addr x);
extern int (*lisp_bignum_p)(addr x);
extern int (*lisp_integer_p)(addr x);
extern int (*lisp_ratio_p)(addr x);
extern int (*lisp_rational_p)(addr x);
extern int (*lisp_single_float_p)(addr x);
extern int (*lisp_double_float_p)(addr x);
extern int (*lisp_long_float_p)(addr x);
extern int (*lisp_float_p)(addr x);
extern int (*lisp_real_p)(addr x);
extern int (*lisp_complex_p)(addr x);
extern int (*lisp_number_p)(addr x);
extern int (*lisp_clos_p)(addr x);
extern int (*lisp_hashtable_p)(addr x);
extern int (*lisp_readtable_p)(addr x);
extern int (*lisp_control_p)(addr x);
extern int (*lisp_callname_p)(addr x);
extern int (*lisp_function_p)(addr x);
extern int (*lisp_package_p)(addr x);
extern int (*lisp_random_state_p)(addr x);
extern int (*lisp_pathname_p)(addr x);
extern int (*lisp_stream_p)(addr x);
extern int (*lisp_restart_p)(addr x);
extern int (*lisp_environment_p)(addr x);
extern int (*lisp_bitvector_p)(addr x);
extern int (*lisp_print_dispatch_p)(addr x);
extern int (*lisp_paper_p)(addr x);

/* extern_sequence.h */
extern void (*lisp0_cons)(addr *ret, addr car, addr cdr);
extern void (*lisp_cons)(addr x, addr car, addr cdr);
extern void (*lisp0_vector)(addr *ret, size_t size);
extern void (*lisp_vector)(addr x, size_t size);
extern void (*lisp0_list_va)(addr *ret, va_list args);
extern void (*lisp0_lista_va)(addr *ret, va_list args);
extern void (*lisp0_list)(addr *ret, ...);
extern void (*lisp_list)(addr x, ...);
extern void (*lisp0_lista)(addr *ret, ...);
extern void (*lisp_lista)(addr x, ...);
extern int (*lisp0_getelt_)(addr *ret, addr pos, size_t index);
extern int (*lisp_getelt_)(addr x, addr pos, size_t index);
extern int (*lisp_setelt_)(addr pos, size_t index, addr value);
extern int (*lisp_length_)(addr pos, size_t *ret);
extern int (*lisp0_reverse_)(addr *ret, addr pos);
extern int (*lisp0_nreverse_)(addr *ret, addr pos);
extern int (*lisp_reverse_)(addr x, addr pos);
extern int (*lisp_nreverse_)(addr x, addr pos);
extern void (*lisp0_car)(addr *ret, addr list);
extern void (*lisp0_cdr)(addr *ret, addr list);
extern void (*lisp0_carcdr)(addr *car, addr *cdr, addr list);
extern void (*lisp_car)(addr x, addr list);
extern void (*lisp_cdr)(addr x, addr list);
extern void (*lisp_carcdr)(addr x, addr y, addr list);
extern void (*lisp_setf_car)(addr cons, addr value);
extern void (*lisp_setf_cdr)(addr cons, addr value);
extern void (*lisp_setf_carcdr)(addr cons, addr car, addr cdr);
extern int (*lisp0_string8_)(addr *ret, const void *str);
extern int (*lisp0_string16_)(addr *ret, const void *str);
extern int (*lisp0_string32_)(addr *ret, const void *str);
extern int (*lisp_string8_)(addr x, const void *str);
extern int (*lisp_string16_)(addr x, const void *str);
extern int (*lisp_string32_)(addr x, const void *str);
extern int (*lisp_string_getc_)(addr pos, size_t i, unicode *c);
extern int (*lisp_strvect_getc)(addr pos, size_t i, unicode *c);
extern int (*lisp_strvect_length)(addr pos, size_t *ret);

/* extern_object.h */
extern int (*lisp0_character_)(addr *ret, unicode value);
extern void (*lisp0_fixnum)(addr *ret, fixnum value);
extern int (*lisp0_float_)(addr *ret, float value);
extern int (*lisp0_double_)(addr *ret, double value);
extern int (*lisp0_long_double_)(addr *ret, long double value);
extern int (*lisp_character_)(addr x, unicode value);
extern void (*lisp_fixnum)(addr x, fixnum value);
extern int (*lisp_float_)(addr x, float value);
extern int (*lisp_double_)(addr x, double value);
extern int (*lisp_long_double_)(addr x, long double value);
extern int (*lisp_zero_p)(addr value);
extern int (*lisp_plus_p)(addr value);
extern int (*lisp_minus_p)(addr value);
extern void (*lisp_get_character)(addr pos, unicode *ret);
extern void (*lisp_get_fixnum)(addr pos, fixnum *ret);
extern int (*lisp_get_float_)(addr pos, float *ret);
extern int (*lisp_get_double_)(addr pos, double *ret);
extern int (*lisp_get_long_double_)(addr pos, long double *ret);
extern int (*lisp0_package_)(addr *ret, addr pos);
extern int (*lisp0_package8_)(addr *ret, const void *str);
extern int (*lisp0_package16_)(addr *ret, const void *str);
extern int (*lisp0_package32_)(addr *ret, const void *str);
extern int (*lisp_package_)(addr x, addr pos);
extern int (*lisp_package8_)(addr x, const void *str);
extern int (*lisp_package16_)(addr x, const void *str);
extern int (*lisp_package32_)(addr x, const void *str);
extern int (*lisp_in_package_)(addr pos);
extern int (*lisp_in_package8_)(const void *str);
extern int (*lisp_in_package16_)(const void *str);
extern int (*lisp_in_package32_)(const void *str);
extern int (*lisp_push_and_in_package_)(addr pos);
extern int (*lisp_push_and_in_package8_)(const void *str);
extern int (*lisp_push_and_in_package16_)(const void *str);
extern int (*lisp_push_and_in_package32_)(const void *str);
extern int (*lisp0_intern_)(addr *ret, addr package, addr name);
extern int (*lisp0_intern8_)(addr *ret, const void *package, const void *name);
extern int (*lisp0_intern16_)(addr *ret, const void *package, const void *name);
extern int (*lisp0_intern32_)(addr *ret, const void *package, const void *name);
extern int (*lisp_intern_)(addr x, addr package, addr name);
extern int (*lisp_intern8_)(addr x, const void *package, const void *name);
extern int (*lisp_intern16_)(addr x, const void *package, const void *name);
extern int (*lisp_intern32_)(addr x, const void *package, const void *name);
extern int (*lisp0_reader_)(addr *ret, addr str);
extern int (*lisp0_reader8_)(addr *ret, const void *str);
extern int (*lisp0_reader16_)(addr *ret, const void *str);
extern int (*lisp0_reader32_)(addr *ret, const void *str);
extern int (*lisp_reader_)(addr x, addr str);
extern int (*lisp_reader8_)(addr x, const void *str);
extern int (*lisp_reader16_)(addr x, const void *str);
extern int (*lisp_reader32_)(addr x, const void *str);
extern int (*lisp0_pathname_)(addr *ret, addr name);
extern int (*lisp0_pathname8_)(addr *ret, const void *str);
extern int (*lisp0_pathname16_)(addr *ret, const void *str);
extern int (*lisp0_pathname32_)(addr *ret, const void *str);
extern int (*lisp0_namestring_)(addr *ret, addr path);
extern int (*lisp_pathname_)(addr x, addr name);
extern int (*lisp_pathname8_)(addr x, const void *str);
extern int (*lisp_pathname16_)(addr x, const void *str);
extern int (*lisp_pathname32_)(addr x, const void *str);
extern int (*lisp_namestring_)(addr x, addr path);
extern int (*lisp0_paper_)(addr *ret, size_t array, size_t body);
extern int (*lisp_paper_)(addr x, size_t array, size_t body);
extern int (*lisp_paper_gettype_)(addr x, byte *ret);
extern int (*lisp_paper_settype_)(addr x, byte value);
extern int (*lisp_paper_lenarray_)(addr x, size_t *ret);
extern int (*lisp_paper_lenbody_)(addr x, size_t *ret);
extern int (*lisp0_paper_getarray_)(addr *ret, addr pos, size_t index);
extern int (*lisp_paper_getarray_)(addr x, addr pos, size_t index);
extern int (*lisp_paper_setarray_)(addr x, size_t index, addr value);
extern int (*lisp_paper_getbody_)(addr x, size_t index, byte *ret);
extern int (*lisp_paper_setbody_)(addr x, size_t index, byte value);
extern int (*lisp_paper_getmemory_)(addr x, size_t a, size_t b, void *output, size_t *ret);
extern int (*lisp_paper_setmemory_)(addr x, size_t a, size_t b, const void *input, size_t *ret);
extern int (*lisp_paper_body_unsafe_)(addr x, byte **ptr, size_t *ret);

/* extern_init.h */
extern FILE *(*lisperror_stream)(void);
extern int (*lisperror_noeol)(const char *fmt, ...);
extern int (*lisperror_va)(const char *fmt, va_list args);
extern int (*lisperror)(const char *fmt, ...);

/* extern_execute.h */
extern int (*lisp0_eval_)(addr *ret, addr pos);
extern int (*lisp0_eval8_)(addr *ret, const void *str);
extern int (*lisp0_eval16_)(addr *ret, const void *str);
extern int (*lisp0_eval32_)(addr *ret, const void *str);
extern int (*lisp_eval_)(addr x, addr pos);
extern int (*lisp_eval8_)(addr x, const void *str);
extern int (*lisp_eval16_)(addr x, const void *str);
extern int (*lisp_eval32_)(addr x, const void *str);
extern int (*lisp0_call_)(addr *ret, addr call, addr args);
extern int (*lisp_call_)(addr x, addr call, addr args);
extern int (*lisp0_funcall_)(addr *ret, addr call, ...);
extern int (*lisp0_funcall8_)(addr *ret, const void *str, ...);
extern int (*lisp0_funcall16_)(addr *ret, const void *str, ...);
extern int (*lisp0_funcall32_)(addr *ret, const void *str, ...);
extern int (*lisp_funcall_)(addr x, addr call, ...);
extern int (*lisp_funcall8_)(addr x, const void *str, ...);
extern int (*lisp_funcall16_)(addr x, const void *str, ...);
extern int (*lisp_funcall32_)(addr x, const void *str, ...);
extern int (*lisp0_apply_)(addr *ret, addr call, ...);
extern int (*lisp0_apply8_)(addr *ret, const void *str, ...);
extern int (*lisp0_apply16_)(addr *ret, const void *str, ...);
extern int (*lisp0_apply32_)(addr *ret, const void *str, ...);
extern int (*lisp_apply_)(addr x, addr call, ...);
extern int (*lisp_apply8_)(addr x, const void *str, ...);
extern int (*lisp_apply16_)(addr x, const void *str, ...);
extern int (*lisp_apply32_)(addr x, const void *str, ...);
extern int (*lisp_eval_control_)(addr eval);
extern int (*lisp_eval_string_control_)(addr eval);
extern int (*lisp_call_control_)(addr call, addr args);
extern int (*lisp_funcall_control_)(addr call, ...);
extern int (*lisp_apply_control_)(addr call, ...);
extern void (*lisp0_result_control)(addr *ret);
extern void (*lisp0_result2_control)(addr *ret1, addr *ret2);
extern void (*lisp0_values_control)(addr *ret);
extern void (*lisp0_nth_value_control)(addr *ret, size_t index);
extern void (*lisp_result_control)(addr x);
extern void (*lisp_result2_control)(addr x, addr y);
extern void (*lisp_values_control)(addr x);
extern void (*lisp_nth_value_control)(addr x, size_t index);
extern void (*lisp_set_result_control)(addr value);
extern void (*lisp_set_values_control)(addr first, ...);
extern void (*lisp_set_values_nil_control)(void);
extern void (*lisp_set_values_list_control)(addr list);
extern int (*lisp_equal_control)(addr control);
extern int (*lisp_break_control)(void);
extern int (*lisp_escape_control)(void);
extern void (*lisp_reset_control)(void);
extern enum lisp_escape (*lisp_escape_type_control)(void);
extern void (*lisp_save_control)(addr *ret);
extern void (*lisp_rollback_control)(addr value);
extern int (*lisp_eval_loop_)(void);

/* extern_control.h */
extern void (*lisp_push_control)(addr *ret);
extern int (*lisp_pop_control_)(addr control);
extern int (*lisp_push_special_)(addr symbol, addr value);
extern int (*lisp_push_special8_)(const void *name, addr value);
extern int (*lisp_push_special16_)(const void *name, addr value);
extern int (*lisp_push_special32_)(const void *name, addr value);
extern int (*lisp0_get_special_)(addr *ret, addr symbol);
extern int (*lisp0_get_special8_)(addr *ret, const void *name);
extern int (*lisp0_get_special16_)(addr *ret, const void *name);
extern int (*lisp0_get_special32_)(addr *ret, const void *name);
extern int (*lisp_get_special_)(addr x, addr symbol);
extern int (*lisp_get_special8_)(addr x, const void *name);
extern int (*lisp_get_special16_)(addr x, const void *name);
extern int (*lisp_get_special32_)(addr x, const void *name);
extern int (*lisp_set_special_)(addr symbol, addr value);
extern int (*lisp_set_special8_)(const void *name, addr value);
extern int (*lisp_set_special16_)(const void *name, addr value);
extern int (*lisp_set_special32_)(const void *name, addr value);
extern int (*lisp_defvar_)(addr symbol);
extern int (*lisp_defvar8_)(const void *str);
extern int (*lisp_defvar16_)(const void *str);
extern int (*lisp_defvar32_)(const void *str);
extern void (*lisp_catch)(addr symbol);
extern int (*lisp_throw_)(addr symbol);
extern int (*lisp_handler_bind_)(addr name, addr call);
extern int (*lisp_handler_case_)(addr name, addr call);
extern void (*lisp_handler_reverse)(void);
extern void (*lisp0_restart_make)(addr *ret, addr name, addr call, int casep);
extern void (*lisp_restart_make)(addr x, addr name, addr call, int casep);
extern void (*lisp_restart_interactive)(addr restart, addr call);
extern void (*lisp_restart_report)(addr restart, addr call);
extern void (*lisp_restart_test)(addr restart, addr call);
extern void (*lisp_restart_push)(addr restart);
extern void (*lisp_restart_reverse)(void);

/* extern_error.h */
extern void (*lisp_abort)(void);
extern void (*lisp_abortf)(const char *fmt, ...);
extern void (*lisp_abort8)(const void *fmt, ...);
extern void (*lisp_abort16)(const void *fmt, ...);
extern void (*lisp_abort32)(const void *fmt, ...);
extern lisp_abort_calltype (*lisp_set_abort_handler)(lisp_abort_calltype call);
extern lisp_abort_calltype (*lisp_set_abort_setjmp_handler)(void);
extern int (*lisp_signal_)(addr condition);
extern int (*lisp_error_)(addr condition);
extern int (*lisp_error8_)(const void *str, ...);
extern int (*lisp_error16_)(const void *str, ...);
extern int (*lisp_error32_)(const void *str, ...);
extern int (*lisp_warn8_)(const void *str, ...);
extern int (*lisp_warn16_)(const void *str, ...);
extern int (*lisp_warn32_)(const void *str, ...);

/* extern_function.h */
extern void (*lisp0_get_function)(addr *ret, addr symbol);
extern void (*lisp0_get_setf)(addr *ret, addr symbol);
extern void (*lisp_get_function)(addr x, addr symbol);
extern void (*lisp_get_setf)(addr x, addr symbol);
extern int (*lisp0_get_function_)(addr *ret, addr value);
extern int (*lisp0_get_function8_)(addr *ret, const void *str);
extern int (*lisp0_get_function16_)(addr *ret, const void *str);
extern int (*lisp0_get_function32_)(addr *ret, const void *str);
extern int (*lisp_get_function_)(addr x, addr value);
extern int (*lisp_get_function8_)(addr x, const void *str);
extern int (*lisp_get_function16_)(addr x, const void *str);
extern int (*lisp_get_function32_)(addr x, const void *str);
extern int (*lisp0_get_setf_)(addr *ret, addr value);
extern int (*lisp0_get_setf8_)(addr *ret, const void *str);
extern int (*lisp0_get_setf16_)(addr *ret, const void *str);
extern int (*lisp0_get_setf32_)(addr *ret, const void *str);
extern int (*lisp_get_setf_)(addr x, addr value);
extern int (*lisp_get_setf8_)(addr x, const void *str);
extern int (*lisp_get_setf16_)(addr x, const void *str);
extern int (*lisp_get_setf32_)(addr x, const void *str);
extern void (*lisp_compiled_macro)(int index, lisp_calltype_macro call);
extern void (*lisp_compiled_rest)(int index, lisp_calltype_rest call);
extern void (*lisp_compiled_dynamic)(int index, lisp_calltype_dynamic call);
extern void (*lisp_compiled_any)(int index, lisp_calltype_any call);
extern void (*lisp_compiled_empty)(int index, lisp_calltype_empty call);
extern void (*lisp_compiled_var1)(int index, lisp_calltype_var1 call);
extern void (*lisp_compiled_var2)(int index, lisp_calltype_var2 call);
extern void (*lisp_compiled_var3)(int index, lisp_calltype_var3 call);
extern void (*lisp_compiled_var4)(int index, lisp_calltype_var4 call);
extern void (*lisp_compiled_var5)(int index, lisp_calltype_var5 call);
extern void (*lisp_compiled_var6)(int index, lisp_calltype_var6 call);
extern void (*lisp_compiled_opt1)(int index, lisp_calltype_opt1 call);
extern void (*lisp_compiled_opt2)(int index, lisp_calltype_opt2 call);
extern void (*lisp_compiled_opt3)(int index, lisp_calltype_opt3 call);
extern void (*lisp_compiled_var1opt1)(int index, lisp_calltype_var1opt1 call);
extern void (*lisp_compiled_var1opt2)(int index, lisp_calltype_var1opt2 call);
extern void (*lisp_compiled_var1opt3)(int index, lisp_calltype_var1opt3 call);
extern void (*lisp_compiled_var2opt1)(int index, lisp_calltype_var2opt1 call);
extern void (*lisp_compiled_var2opt2)(int index, lisp_calltype_var2opt2 call);
extern void (*lisp_compiled_var2opt3)(int index, lisp_calltype_var2opt3 call);
extern void (*lisp_compiled_var3opt1)(int index, lisp_calltype_var3opt1 call);
extern void (*lisp_compiled_var3opt2)(int index, lisp_calltype_var3opt2 call);
extern void (*lisp_compiled_var3opt3)(int index, lisp_calltype_var3opt3 call);
extern void (*lisp_compiled_var1rest)(int index, lisp_calltype_var1rest call);
extern void (*lisp_compiled_var2rest)(int index, lisp_calltype_var2rest call);
extern void (*lisp_compiled_var3rest)(int index, lisp_calltype_var3rest call);
extern void (*lisp_compiled_var1dynamic)(int index, lisp_calltype_var1dynamic call);
extern void (*lisp_compiled_var2dynamic)(int index, lisp_calltype_var2dynamic call);
extern void (*lisp_compiled_var3dynamic)(int index, lisp_calltype_var3dynamic call);
extern int (*lisp0_compiled_function_)(addr *ret, int index, addr symbol);
extern int (*lisp0_compiled_function8_)(addr *ret, int index, const void *str);
extern int (*lisp0_compiled_function16_)(addr *ret, int index, const void *str);
extern int (*lisp0_compiled_function32_)(addr *ret, int index, const void *str);
extern int (*lisp_compiled_function_)(addr x, int index, addr symbol);
extern int (*lisp_compiled_function8_)(addr x, int index, const void *str);
extern int (*lisp_compiled_function16_)(addr x, int index, const void *str);
extern int (*lisp_compiled_function32_)(addr x, int index, const void *str);
extern int (*lisp_compiled_defun_)(int index, addr symbol);
extern int (*lisp_compiled_defun8_)(int index, const void *str);
extern int (*lisp_compiled_defun16_)(int index, const void *str);
extern int (*lisp_compiled_defun32_)(int index, const void *str);
extern int (*lisp_compiled_defun_setf_)(int index, addr symbol);
extern int (*lisp_compiled_defun_setf8_)(int index, const void *str);
extern int (*lisp_compiled_defun_setf16_)(int index, const void *str);
extern int (*lisp_compiled_defun_setf32_)(int index, const void *str);
extern void (*lisp_compiled_setvalue)(addr pos, addr value);
extern void (*lisp_compiled_getvalue)(addr *ret);

/* extern_instance.h */
extern void (*lisp0_find_class)(addr *ret, addr symbol);
extern int (*lisp0_find_class_)(addr *ret, addr symbol);
extern int (*lisp0_find_class8_)(addr *ret, const void *str);
extern int (*lisp0_find_class16_)(addr *ret, const void *str);
extern int (*lisp0_find_class32_)(addr *ret, const void *str);
extern void (*lisp_find_class)(addr x, addr symbol);
extern int (*lisp_find_class_)(addr x, addr symbol);
extern int (*lisp_find_class8_)(addr x, const void *str);
extern int (*lisp_find_class16_)(addr x, const void *str);
extern int (*lisp_find_class32_)(addr x, const void *str);
extern int (*lisp0_instance_)(addr *ret, addr clos, ...);
extern int (*lisp0_instance8_)(addr *ret, const void *clos, ...);
extern int (*lisp0_instance16_)(addr *ret, const void *clos, ...);
extern int (*lisp0_instance32_)(addr *ret, const void *clos, ...);
extern int (*lisp_instance_)(addr x, addr clos, ...);
extern int (*lisp_instance8_)(addr x, const void *clos, ...);
extern int (*lisp_instance16_)(addr x, const void *clos, ...);
extern int (*lisp_instance32_)(addr x, const void *clos, ...);
extern int (*lisp_slot_exists_)(addr instance, addr symbol, int *ret);
extern int (*lisp_slot_exists8_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_exists16_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_exists32_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_boundp_)(addr instance, addr symbol, int *ret);
extern int (*lisp_slot_boundp8_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_boundp16_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_boundp32_)(addr instance, const void *str, int *ret);
extern int (*lisp_slot_makunbound_)(addr instance, addr symbol);
extern int (*lisp_slot_makunbound8_)(addr instance, const void *str);
extern int (*lisp_slot_makunbound16_)(addr instance, const void *str);
extern int (*lisp_slot_makunbound32_)(addr instance, const void *str);
extern int (*lisp0_slot_value_)(addr *ret, addr instance, addr symbol);
extern int (*lisp0_slot_value8_)(addr *ret, addr instance, const void *str);
extern int (*lisp0_slot_value16_)(addr *ret, addr instance, const void *str);
extern int (*lisp0_slot_value32_)(addr *ret, addr instance, const void *str);
extern int (*lisp_slot_value_)(addr x, addr instance, addr symbol);
extern int (*lisp_slot_value8_)(addr x, addr instance, const void *str);
extern int (*lisp_slot_value16_)(addr x, addr instance, const void *str);
extern int (*lisp_slot_value32_)(addr x, addr instance, const void *str);
extern int (*lisp_slot_setf_)(addr instance, addr symbol, addr value);
extern int (*lisp_slot_setf8_)(addr instance, const void *str, addr value);
extern int (*lisp_slot_setf16_)(addr instance, const void *str, addr value);
extern int (*lisp_slot_setf32_)(addr instance, const void *str, addr value);

/* extern_print.h */
extern int (*lisp_format8_)(addr stream, const void *str, ...);
extern int (*lisp_format16_)(addr stream, const void *str, ...);
extern int (*lisp_format32_)(addr stream, const void *str, ...);
extern int (*lisp_stdout8_)(const void *str, ...);
extern int (*lisp_stdout16_)(const void *str, ...);
extern int (*lisp_stdout32_)(const void *str, ...);
extern int (*lisp_stderr8_)(const void *str, ...);
extern int (*lisp_stderr16_)(const void *str, ...);
extern int (*lisp_stderr32_)(const void *str, ...);
extern int (*lisp0_stringf8_)(addr *ret, const void *str, ...);
extern int (*lisp0_stringf16_)(addr *ret, const void *str, ...);
extern int (*lisp0_stringf32_)(addr *ret, const void *str, ...);
extern int (*lisp_stringf8_)(addr x, const void *str, ...);
extern int (*lisp_stringf16_)(addr x, const void *str, ...);
extern int (*lisp_stringf32_)(addr x, const void *str, ...);

/* extern_stream.h */
extern void (*lisp0_stream_define)(addr *ret, int index, size_t size);
extern void (*lisp_stream_define)(addr x, int index, size_t size);
extern void (*lisp_stream_memory)(addr stream, void **ret);
extern void (*lisp0_getinfo_stream)(addr *ret, addr stream);
extern void (*lisp_getinfo_stream)(addr x, addr stream);
extern void (*lisp_setinfo_stream)(addr stream, addr value);
extern void (*lisp_stream_calltype_close)(int, lisp_streamtype_close);
extern void (*lisp_stream_calltype_read_byte)(int, lisp_streamtype_read_byte);
extern void (*lisp_stream_calltype_unread_byte)(int, lisp_streamtype_unread_byte);
extern void (*lisp_stream_calltype_write_byte)(int, lisp_streamtype_write_byte);
extern void (*lisp_stream_calltype_read_char)(int, lisp_streamtype_read_char);
extern void (*lisp_stream_calltype_read_hang)(int, lisp_streamtype_read_hang);
extern void (*lisp_stream_calltype_unread_char)(int, lisp_streamtype_unread_char);
extern void (*lisp_stream_calltype_write_char)(int, lisp_streamtype_write_char);
extern void (*lisp_stream_calltype_getleft)(int, lisp_streamtype_getleft);
extern void (*lisp_stream_calltype_setleft)(int, lisp_streamtype_setleft);
extern void (*lisp_stream_calltype_inputp)(int, lisp_streamtype_inputp);
extern void (*lisp_stream_calltype_outputp)(int, lisp_streamtype_outputp);
extern void (*lisp_stream_calltype_interactivep)(int, lisp_streamtype_interactivep);
extern void (*lisp_stream_calltype_characterp)(int, lisp_streamtype_characterp);
extern void (*lisp_stream_calltype_binaryp)(int, lisp_streamtype_binaryp);
extern void (*lisp_stream_calltype_element_type)(int, lisp_streamtype_element_type);
extern void (*lisp_stream_calltype_external_format)(int, lisp_streamtype_external_format);
extern void (*lisp_stream_calltype_file_length)(int, lisp_streamtype_file_length);
extern void (*lisp_stream_calltype_file_position)(int, lisp_streamtype_file_position);
extern void (*lisp_stream_calltype_file_position_start)(int, lisp_streamtype_file_position_start);
extern void (*lisp_stream_calltype_file_position_end)(int, lisp_streamtype_file_position_end);
extern void (*lisp_stream_calltype_file_position_set)(int, lisp_streamtype_file_position_set);
extern void (*lisp_stream_calltype_file_charlen)(int, lisp_streamtype_file_charlen);
extern void (*lisp_stream_calltype_file_strlen)(int, lisp_streamtype_file_strlen);
extern void (*lisp_stream_calltype_listen)(int, lisp_streamtype_listen);
extern void (*lisp_stream_calltype_clear_input)(int, lisp_streamtype_clear_input);
extern void (*lisp_stream_calltype_finish_output)(int, lisp_streamtype_finish_output);
extern void (*lisp_stream_calltype_force_output)(int, lisp_streamtype_force_output);
extern void (*lisp_stream_calltype_clear_output)(int, lisp_streamtype_clear_output);
extern void (*lisp_stream_calltype_exitpoint)(int, lisp_streamtype_exitpoint);
extern void (*lisp_stream_calltype_termsize)(int, lisp_streamtype_termsize);
extern void (*lisp_stream_calltype_error_close)(int);
extern void (*lisp_stream_calltype_error_read_byte)(int);
extern void (*lisp_stream_calltype_error_unread_byte)(int);
extern void (*lisp_stream_calltype_error_write_byte)(int);
extern void (*lisp_stream_calltype_error_read_char)(int);
extern void (*lisp_stream_calltype_error_read_hang)(int);
extern void (*lisp_stream_calltype_error_unread_char)(int);
extern void (*lisp_stream_calltype_error_write_char)(int);
extern void (*lisp_stream_calltype_error_getleft)(int);
extern void (*lisp_stream_calltype_error_setleft)(int);
extern void (*lisp_stream_calltype_error_inputp)(int);
extern void (*lisp_stream_calltype_error_outputp)(int);
extern void (*lisp_stream_calltype_error_interactivep)(int);
extern void (*lisp_stream_calltype_error_characterp)(int);
extern void (*lisp_stream_calltype_error_binaryp)(int);
extern void (*lisp_stream_calltype_error_element_type)(int);
extern void (*lisp_stream_calltype_error_external_format)(int);
extern void (*lisp_stream_calltype_error_file_length)(int);
extern void (*lisp_stream_calltype_error_file_position)(int);
extern void (*lisp_stream_calltype_error_file_position_start)(int);
extern void (*lisp_stream_calltype_error_file_position_end)(int);
extern void (*lisp_stream_calltype_error_file_position_set)(int);
extern void (*lisp_stream_calltype_error_file_charlen)(int);
extern void (*lisp_stream_calltype_error_file_strlen)(int);
extern void (*lisp_stream_calltype_error_listen)(int);
extern void (*lisp_stream_calltype_error_clear_input)(int);
extern void (*lisp_stream_calltype_error_finish_output)(int);
extern void (*lisp_stream_calltype_error_force_output)(int);
extern void (*lisp_stream_calltype_error_clear_output)(int);
extern void (*lisp_stream_calltype_error_exitpoint)(int);
extern void (*lisp_stream_calltype_error_termsize)(int);

/* extern_unicode.h */
extern int (*lisp_eastasian_set)(enum LispEastAsianType type, unsigned width);
extern int (*lisp_eastasian_get)(enum LispEastAsianType type, unsigned *ret);
extern enum LispEastAsianType (*lisp_eastasian_type_unicode)(unicode c);
extern enum LispEastAsianType (*lisp_eastasian_type_character)(addr value);
extern unsigned (*lisp_eastasian_unicode)(unicode c);
extern int (*lisp_eastasian_character_)(addr value, unsigned *ret);
extern int (*lisp_eastasian_string_)(addr value, size_t *ret);
extern int (*lisp_eastasian_width_)(addr value, size_t *ret);
extern int (*lisp_unicode_count)(void);
extern int (*lisp_utf8_encode)(unicode c, void *ptr, size_t *ret);
extern int (*lisp_utf16_range)(unicode c);
extern int (*lisp_utf16_high)(unicode c);
extern int (*lisp_utf16_low)(unicode c);
extern unicode (*lisp_utf16_merge)(byte16 first, byte16 second);

/* function */
int lisp_dlfile_update(lisp_dlfile_array ptr);

#endif
