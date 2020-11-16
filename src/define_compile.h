#ifdef LISP_DEVELOP_NOPREFIX
#define _n(x) x
#else
#define _n(x) lispd_##x
#endif

