#ifndef __BUILD_DEFINE_HEADER__
#define __BUILD_DEFINE_HEADER__

#define LISP_PACKAGE            LISPNAME
#define LISP_SYSTEM             LISPNAME "-SYSTEM"
#define LISP_CODE               LISPNAME "-CODE"
#define LISP_USER               LISPNAME "-USER"
#define LISP_CLOS               LISPNAME "-CLOS"
#define LISP_RT                 LISPNAME "-RT"
#define LISP_KEYWORD            "KEYWORD"
#define LISP_COMMON             "COMMON-LISP"
#define LISP_COMMON_USER        "COMMON-LISP-USER"
#define Nil                     (lisp_nil_object)
#define T                       (lisp_t_object)
#define Unbound                 ((addr)~(uintptr_t)0)
#define LispRoot(x)             (lisp_root[LISPINDEX_##x])
#define SetLispRoot(x,y)		setlisproot(LISPINDEX_##x, (y));

#define Abort(x)                {Debug(x); abortthis();}
#define Abort2(x,y)             {Debug2(x,y); abortthis();}
#define IfDebug(x,y)            {if (x) { Debug(y); return 1; }}
#define CheckType(x,y)          Check(GetType(x) != (y), "type error")
#define CheckType2(x,y,z)       Check(GetType(x) != (y), (z))
#define CheckReadOnly(x)        Check(GetStatusReadOnly(x), "readonly error");
#define CheckSymbol(x)          Check(! symbolp(x), "type error")
#define CheckLocal(x)           Check((x) == NULL, "local error")
#define CheckLocalType(x,y,z)   { \
	CheckLocal(x); \
	CheckType((y), (z)); \
}
#define Return(x)               {if (x) return 1;}
#define Result(x, y)            ((*(x) = (y)), 0)

#ifdef LISP_DEBUG
#define Info(x)                 info(x)
#define Debug(x)                infoerror(__FILE__, __LINE__, __func__, x)
#define Debug2(x,y)             infoerror(__FILE__, __LINE__, __func__, x, y)
#define Check(x,y)              {if (x) {Abort(y);}}
#define Check2(x,y1,y2)         {if (x) {Abort((y),(z));}}
#define Error(x)                {if (x) {Abort("return error.");}}
#else
#define Info(x)                 ;
#define Debug(x)                ;
#define Debug2(x,y)             ;
#define Check(x,y)              ;
#define Check2(x,y1,y2)         ;
#define Error(x)                (void)(x)
#endif

#endif

