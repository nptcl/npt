#ifndef __CONS_HEADER__
#define __CONS_HEADER__

#include <stdarg.h>
#include "local.h"
#include "typedef.h"

/* cons */
_g void getcons(addr cons, addr *left, addr *right);
_g void getcar(addr cons, addr *left);
_g void getcdr(addr cons, addr *right);
_g int consp_getcons(addr cons, addr *left, addr *right);
_g int consp_getcar(addr cons, addr *left);
_g int consp_getcdr(addr cons, addr *right);
_g void setcons(addr cons, addr left, addr right);
_g void setcar(addr cons, addr left);
_g void setcdr(addr cons, addr right);

/* list */
_g void list_alloc_stdarg(LocalRoot local, addr *ret, va_list args);
_g addr list_heapr(addr pos, ...);
_g addr list_localr(LocalRoot local, ...);
_g addr list_allocr(LocalRoot local, ...);
_g void list_heap(addr *ret, ...);
_g void list_local(LocalRoot local, addr *ret, ...);
_g void list_alloc(LocalRoot local, addr *ret, ...);
_g void pushva_heap(addr *list, ...);

/* list* */
_g void lista_alloc_safe(LocalRoot local, addr *ret, addr first, addr cons);
_g void lista_local_safe(LocalRoot local, addr *ret, addr first, addr cons);
_g void lista_heap_safe(addr *ret, addr first, addr cons);
_g void lista_stdarg_safe(LocalRoot local, addr *ret, va_list args);
_g void lista_stdarg_alloc(LocalRoot local, addr *ret, va_list args);
_g addr lista_allocr(LocalRoot local, ...);
_g addr lista_localr(LocalRoot local, ...);
_g addr lista_heapr(addr pos, ...);
_g void lista_alloc(LocalRoot local, addr *ret, ...);
_g void lista_local(LocalRoot local, addr *ret, ...);
_g void lista_heap(addr *ret, ...);

/* bind */
_g void List_bind(addr list, ...);
_g void list_bind(addr list, ...);
_g void Lista_bind(addr list, ...);
_g void lista_bind(addr list, ...);

/* copy-tree */
_g void copy_tree_alloc(LocalRoot local, addr *ret, addr list);
_g void copy_tree_local(LocalRoot local, addr *ret, addr list);
_g void copy_tree_heap(addr *ret, addr list);

#endif

