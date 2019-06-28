#ifndef __BUILD_HEADER__
#define __BUILD_HEADER__

#include <setjmp.h>
#include <stddef.h>
#include "alloc.h"
#include "define.h"
#include "execute.h"
#include "file_type.h"
#include "info.h"
#include "typedef.h"

extern int      lisp_init;
extern addr     lisp_root[LISPINDEX_SIZE];
extern addr     lisp_nil;
extern addr     lisp_t;
extern byte32   lisp_property;
/* for debug */
extern int      lisp_info_enable;

#define LISP_PACKAGE            LISPNAME
#define LISP_SYSTEM             LISPNAME "-SYSTEM"
#define LISP_CODE               LISPNAME "-CODE"
#define LISP_USER               LISPNAME "-USER"
#define LISP_CLOS               LISPNAME "-CLOS"
#define LISP_RT                 LISPNAME "-RT"
#define LISP_KEYWORD			"KEYWORD"
#define LISP_COMMON				"COMMON-LISP"
#define LISP_COMMON_USER		"COMMON-LISP-USER"
#define Nil                     (lisp_nil)
#define T                       (lisp_t)
#define Root(i)                 (lisp_root[i])
#define Unbound                 ((addr)~(uintptr_t)0)

#define CHARQUEUESIZE           64
#define PACKAGE_HASHSIZE        16

#ifdef LISP_DEBUG
#define Info(x) info(x)
#define Debug(x) infoerror(__FILE__, __LINE__, __func__, x)
#define Debug2(x,y) infoerror(__FILE__, __LINE__, __func__, x, y)
#define Check(x,y) {if (x) {Debug(y); abortthis();}}
#define Check2(x,y1,y2) {if (x) {Debug(y1,y2); abortthis();}}
#define SetProperty(i,v)  setproperty((i), (v))
#define GetProperty(i)    getproperty(i)
#else
#define Info(x)
#define Debug(x)
#define Debug2(x,y)
#define Check(x,y)
#define Check2(x,y1,y2)
#define SetProperty(i,v)  SetShiftValue(lisp_property,i,v,1UL,byte32)
#define GetProperty(i)    GetShiftValue(lisp_property,i,1UL)
#endif

#define Abort(x)			{Debug(x); abortthis();}
#define Abort2(x,y)			{Debug2(x,y); abortthis();}
#define IfDebug(x,y)		if (x) { Debug(y); return 1; }
#define CheckType(x,y)		Check(GetType(x) != (y), "type error")
#define CheckType2(x,y,z)	Check(GetType(x) != (y), (z))
#define CheckReadOnly(x)	Check(GetStatusReadOnly(x), "readonly error");
#define CheckLocal(x)		Check((x) == NULL, "local error")
#define CheckLocalType(x,y,z) { \
	CheckLocal(x); \
	CheckType((y), (z)); \
}
#define CheckSymbol(x) Check(! symbolp(x), "type error")

#define SetPropertyExecute(p,i,v)	SetShiftValue(p->property,i,v,1UL,byte32)
#define GetPropertyExecute(p,i)		GetShiftValue(p->property,i,1UL)

#define SetRoot(i,n)  (Root(i) = (n))


/*
 *  function
 */
void setproperty(int index, int value);
int getproperty(int index);
void initlisp(void);
int alloclisp(size_t heap, size_t stack);
void freelisp(void);
int degradelisp(int argc, char *argv[], char *env[]);

void build_lisproot(Execute ptr);
void buildlisp(Execute ptr);


/*
 *  core
 */
int save_lisp(struct filememory *fm);
int load_lisp(struct filememory *fm);

#endif

