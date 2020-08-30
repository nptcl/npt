#define _g
#define _s static
#define __extern extern

#ifdef LISP_DEVELOP_NOPREFIX
#define _n(x) x
#else
#define _n(x) lispd_##x
#endif

