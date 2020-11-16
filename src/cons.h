#ifndef __CONS_HEADER__
#define __CONS_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

#define consp_getcons _n(consp_getcons)
#define consp_getcar _n(consp_getcar)
#define consp_getcdr _n(consp_getcdr)
#define getcons_ _n(getcons_)
#define getcar_ _n(getcar_)
#define getcdr_ _n(getcdr_)
#define setcons_ _n(setcons_)
#define setcar_ _n(setcar_)
#define setcdr_ _n(setcdr_)
#define list_stdarg_alloc _n(list_stdarg_alloc)
#define list_heap _n(list_heap)
#define list_local _n(list_local)
#define list_alloc _n(list_alloc)
#define pushva_heap _n(pushva_heap)
#define lista_safe_alloc_ _n(lista_safe_alloc_)
#define lista_safe_local_ _n(lista_safe_local_)
#define lista_safe_heap_ _n(lista_safe_heap_)
#define lista_stdarg_noerror _n(lista_stdarg_noerror)
#define lista_stdarg_safe_ _n(lista_stdarg_safe_)
#define lista_stdarg_alloc _n(lista_stdarg_alloc)
#define lista_alloc _n(lista_alloc)
#define lista_local _n(lista_local)
#define lista_heap _n(lista_heap)
#define List_bind _n(List_bind)
#define list_bind_ _n(list_bind_)
#define Lista_bind _n(Lista_bind)
#define lista_bind_ _n(lista_bind_)
#define copy_tree_alloc _n(copy_tree_alloc)
#define copy_tree_local _n(copy_tree_local)
#define copy_tree_heap _n(copy_tree_heap)

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
int consp_getcons(addr cons, addr *car, addr *cdr);
int consp_getcar(addr cons, addr *car);
int consp_getcdr(addr cons, addr *cdr);

int getcons_(addr list, addr *car, addr *cdr);
int getcar_(addr list, addr *car);
int getcdr_(addr list, addr *cdr);
int setcons_(addr cons, addr car, addr cdr);
int setcar_(addr cons, addr car);
int setcdr_(addr cons, addr cdr);
#define Return_getcons(x,y,z) Return(getcons_((x),(y),(z)))
#define Return_getcar(x,y) Return(getcar_((x),(y)))
#define Return_getcdr(x,y) Return(getcdr_((x),(y)))
#define Return_setcons(x,y,z) Return(setcons_((x),(y),(z)))
#define Return_setcar(x,y) Return(setcar_((x),(y)))
#define Return_setcdr(x,y) Return(setcdr_((x),(y)))

/* list */
void list_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
void list_heap(addr *ret, ...);
void list_local(LocalRoot local, addr *ret, ...);
void list_alloc(LocalRoot local, addr *ret, ...);
void pushva_heap(addr *list, ...);

/* list* */
int lista_safe_alloc_(LocalRoot local, addr *ret, addr first, addr cons);
int lista_safe_local_(LocalRoot local, addr *ret, addr first, addr cons);
int lista_safe_heap_(addr *ret, addr first, addr cons);
void lista_stdarg_noerror(LocalRoot local, addr *ret, va_list args);
int lista_stdarg_safe_(LocalRoot local, addr *ret, va_list args);
void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
void lista_alloc(LocalRoot local, addr *ret, ...);
void lista_local(LocalRoot local, addr *ret, ...);
void lista_heap(addr *ret, ...);

/* bind */
void List_bind(addr list, ...);
int list_bind_(addr list, ...);
void Lista_bind(addr list, ...);
int lista_bind_(addr list, ...);

/* copy-tree */
void copy_tree_alloc(LocalRoot local, addr *ret, addr list);
void copy_tree_local(LocalRoot local, addr *ret, addr list);
void copy_tree_heap(addr *ret, addr list);

#endif

