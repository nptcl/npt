#include "cmpl_arch.h"

#if defined(LISP_COMPLEX_WINDOWS)
#include "cmpl_windows.h"
#elif defined(LISP_COMPLEX_CPLUSPLUS)
#include "cmpl_cpp.h"
#else
#include "cmpl_c99.h"
#endif

