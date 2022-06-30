#ifndef __CLOS_OBJECT_HEADER__
#define __CLOS_OBJECT_HEADER__

#include "clos_define.h"
#include "execute.h"
#include "local.h"
#include "typedef.h"

/*
 *  clos
 */
struct clos_struct {
	fixnum version;
};

#define GetClosValue_Low				GetArrayA4
#define SetClosValue_Low				SetArrayA4
#define LenClosValue_Low				LenArrayA4
#define PtrClosStruct_Low(x)			PtrBodySSa((x),CLOS_INDEX_SIZE)
#define ClosStruct_Low(x)				((struct clos_struct *)PtrClosStruct_Low(x))
#define GetClassOfClos_Low(x,y)			GetArraySS((x),CLOS_INDEX_CLASS_OF,(y))
#define SetClassOfClos_Low(x,y)			SetArraySS((x),CLOS_INDEX_CLASS_OF,(y))
#define GetSlotClos_Low(x,y)			GetArraySS((x),CLOS_INDEX_SLOT,(y))
#define SetSlotClos_Low(x,y)			SetArraySS((x),CLOS_INDEX_SLOT,(y))
#define GetValueClos_Low(x,y)			GetArraySS((x),CLOS_INDEX_VALUE,(y))
#define SetValueClos_Low(x,y)			SetArraySS((x),CLOS_INDEX_VALUE,(y))
#define GetFuncallClos_Low(x,y)			(*(y) = GetUser((x)))
#define SetFuncallClos_Low(x,y)			(SetUser((x), (y) != 0))
#define GetVersionClos_Low(x,y)			(*(y) = ClosStruct_Low(x)->version)
#define SetVersionClos_Low(x,y)			(ClosStruct_Low(x)->version = (y))

#ifdef LISP_DEBUG
#define GetClosValue			getclosvalue
#define SetClosValue			setclosvalue
#define LenClosValue			lenclosvalue
#define ClosStruct				struct_clos
#define GetClassOfClos			getclassof_clos
#define SetClassOfClos			setclassof_clos
#define GetSlotClos				getslot_clos
#define SetSlotClos				setslot_clos
#define GetValueClos			getvalue_clos
#define SetValueClos			setvalue_clos
#define GetFuncallClos			getfuncall_clos
#define SetFuncallClos			setfuncall_clos
#define GetVersionClos			getversion_clos
#define SetVersionClos			setversion_clos
#else
#define GetClosValue			GetClosValue_Low
#define SetClosValue			SetClosValue_Low
#define LenClosValue			LenClosValue_Low
#define ClosStruct				ClosStruct_Low
#define GetClassOfClos			GetClassOfClos_Low
#define SetClassOfClos			SetClassOfClos_Low
#define GetSlotClos				GetSlotClos_Low
#define SetSlotClos				SetSlotClos_Low
#define GetValueClos			GetValueClos_Low
#define SetValueClos			SetValueClos_Low
#define GetFuncallClos			GetFuncallClos_Low
#define SetFuncallClos			SetFuncallClos_Low
#define GetVersionClos			GetVersionClos_Low
#define SetVersionClos			SetVersionClos_Low
#endif

#define getclosvalue _n(getclosvalue)
#define setclosvalue _n(setclosvalue)
#define lenclosvalue _n(lenclosvalue)
#define clos_value_p _n(clos_value_p)
#define clos_value_alloc _n(clos_value_alloc)
#define clos_value_heap _n(clos_value_heap)

#define struct_clos _n(struct_clos)
#define getclassof_clos _n(getclassof_clos)
#define setclassof_clos _n(setclassof_clos)
#define getslot_clos _n(getslot_clos)
#define setslot_clos _n(setslot_clos)
#define getvalue_clos _n(getvalue_clos)
#define setvalue_clos _n(setvalue_clos)
#define getfuncall_clos _n(getfuncall_clos)
#define setfuncall_clos _n(setfuncall_clos)
#define getversion_clos _n(getversion_clos)
#define setversion_clos _n(setversion_clos)
#define closp _n(closp)
#define clos_alloc _n(clos_alloc)
#define clos_local _n(clos_local)
#define clos_heap _n(clos_heap)
#define clos_destroy _n(clos_destroy)
#define clos_swap _n(clos_swap)
#define clos_copy_alloc _n(clos_copy_alloc)
#define clos_allcopy_alloc_ _n(clos_allcopy_alloc_)
#define clos_getslots_heap_ _n(clos_getslots_heap_)
#define clos_funcall_p _n(clos_funcall_p)
#define clos_set_funcall _n(clos_set_funcall)

void getclosvalue(addr pos, size_t index, addr *ret);
void setclosvalue(addr pos, size_t index, addr value);
void lenclosvalue(addr pos, size_t *ret);
int clos_value_p(addr pos);
void clos_value_alloc(LocalRoot local, addr *ret, size_t size);
void clos_value_heap(addr *ret, size_t size);

struct clos_struct *struct_clos(addr pos);
void getclassof_clos(addr pos, addr *ret);
void setclassof_clos(addr pos, addr value);
void getslot_clos(addr pos, addr *ret);
void setslot_clos(addr pos, addr value);
void getvalue_clos(addr pos, addr *ret);
void setvalue_clos(addr pos, addr value);
void getfuncall_clos(addr pos, int *ret);
void setfuncall_clos(addr pos, int value);
void getversion_clos(addr pos, fixnum *ret);
void setversion_clos(addr pos, fixnum value);
int closp(addr pos);
void clos_alloc(LocalRoot local, addr *ret, addr slots);
void clos_local(LocalRoot local, addr *ret, addr slots);
void clos_heap(addr *ret, addr slots);
void clos_destroy(addr pos);
void clos_swap(addr a, addr b);
void clos_copy_alloc(LocalRoot local, addr pos, addr *ret);
int clos_allcopy_alloc_(Execute ptr, LocalRoot local, addr pos, addr *ret);
int clos_getslots_heap_(Execute ptr, addr pos, addr *ret);
int clos_funcall_p(addr pos);
void clos_set_funcall(addr pos);


/*
 *  slot-vector
 */
#define GetSlotVector_Low				GetArrayA4
#define SetSlotVector_Low				SetArrayA4
#define LenSlotVector_Low				LenArrayA4

#ifdef LISP_DEBUG
#define GetSlotVector			getslotvector
#define SetSlotVector			setslotvector
#define LenSlotVector			lenslotvector
#else
#define GetSlotVector			GetSlotVector_Low
#define SetSlotVector			SetSlotVector_Low
#define LenSlotVector			LenSlotVector_Low
#endif

#define getslotvector _n(getslotvector)
#define setslotvector _n(setslotvector)
#define lenslotvector _n(lenslotvector)
#define slot_vector_p _n(slot_vector_p)
#define slot_vector_alloc _n(slot_vector_alloc)
#define slot_vector_local _n(slot_vector_local)
#define slot_vector_heap _n(slot_vector_heap)
#define slot_vector_copy_alloc_ _n(slot_vector_copy_alloc_)
#define slot_vector_copy_local_ _n(slot_vector_copy_local_)
#define slot_vector_copy_heap_ _n(slot_vector_copy_heap_)
#define slot_vector_copyheap_alloc_ _n(slot_vector_copyheap_alloc_)
#define slot_vector_clear_ _n(slot_vector_clear_)

void getslotvector(addr pos, size_t index, addr *ret);
void setslotvector(addr pos, size_t index, addr value);
void lenslotvector(addr pos, size_t *ret);
int slot_vector_p(addr pos);
void slot_vector_alloc(LocalRoot local, addr *ret, size_t size);
void slot_vector_local(LocalRoot local, addr *ret, size_t size);
void slot_vector_heap(addr *ret, size_t size);
int slot_vector_copy_alloc_(Execute ptr, LocalRoot local, addr *ret, addr pos);
int slot_vector_copy_local_(Execute ptr, addr *ret, addr pos);
int slot_vector_copy_heap_(Execute ptr, addr *ret, addr pos);
int slot_vector_copyheap_alloc_(Execute ptr, LocalRoot local, addr *ret, addr pos);
int slot_vector_clear_(Execute ptr, addr pos);

#endif

