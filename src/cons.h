#ifndef __CONS_HEADER__
#define __CONS_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

#define Inline_getcons(x,y,z) { \
	if (GetType(x) != LISPTYPE_CONS) { \
		return TypeError_((x), CONS); \
	} \
	GetCons((x), (y), (z)); \
}
#define Inline_getcar(x,y) { \
	if (GetType(x) != LISPTYPE_CONS) { \
		return TypeError_((x), CONS); \
	} \
	GetCar((x), (y)); \
}
#define Inline_getcdr(x,y) { \
	if (GetType(x) != LISPTYPE_CONS) { \
		return TypeError_((x), CONS); \
	} \
	GetCdr((x), (y)); \
}

/* cons */
_g int consp_getcons(addr cons, addr *car, addr *cdr);
_g int consp_getcar(addr cons, addr *car);
_g int consp_getcdr(addr cons, addr *cdr);

_g int getcons_(addr list, addr *car, addr *cdr);
_g int getcar_(addr list, addr *car);
_g int getcdr_(addr list, addr *cdr);
_g int setcons_(addr cons, addr car, addr cdr);
_g int setcar_(addr cons, addr car);
_g int setcdr_(addr cons, addr cdr);
#define Return_getcons(x,y,z) Return(getcons_((x),(y),(z)))
#define Return_getcar(x,y) Return(getcar_((x),(y)))
#define Return_getcdr(x,y) Return(getcdr_((x),(y)))
#define Return_setcons(x,y,z) Return(setcons_((x),(y),(z)))
#define Return_setcar(x,y) Return(setcar_((x),(y)))
#define Return_setcdr(x,y) Return(setcdr_((x),(y)))

/* list */
_g void list_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
_g void list_heap(addr *ret, ...);
_g void list_local(LocalRoot local, addr *ret, ...);
_g void list_alloc(LocalRoot local, addr *ret, ...);
_g void pushva_heap(addr *list, ...);

/* list* */
_g int lista_safe_alloc_(LocalRoot local, addr *ret, addr first, addr cons);
_g int lista_safe_local_(LocalRoot local, addr *ret, addr first, addr cons);
_g int lista_safe_heap_(addr *ret, addr first, addr cons);
_g void lista_stdarg_noerror(LocalRoot local, addr *ret, va_list args);
_g int lista_stdarg_safe_(LocalRoot local, addr *ret, va_list args);
_g void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
_g void lista_alloc(LocalRoot local, addr *ret, ...);
_g void lista_local(LocalRoot local, addr *ret, ...);
_g void lista_heap(addr *ret, ...);

/* bind */
_g void List_bind(addr list, ...);
_g int list_bind_(addr list, ...);
_g void Lista_bind(addr list, ...);
_g int lista_bind_(addr list, ...);

/* copy-tree */
_g void copy_tree_alloc(LocalRoot local, addr *ret, addr list);
_g void copy_tree_local(LocalRoot local, addr *ret, addr list);
_g void copy_tree_heap(addr *ret, addr list);

#endif

