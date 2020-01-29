#ifndef __LISP_TYPEDEF_INTEGER_HEADER__
#define __LISP_TYPEDEF_INTEGER_HEADER__

#include "define.h"
#include <stdint.h>
#include <inttypes.h>

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

typedef fixed bigtype;
#define FIXNUM_UMIN			((bigtype)FIXNUM_MIN)

#endif

