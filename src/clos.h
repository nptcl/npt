#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "clos_define.h"
#include "constant.h"
#include "execute.h"

#define Clos_standard_class _n(Clos_standard_class)
#define Clos_standard_generic _n(Clos_standard_generic)
#define Clos_standard_method _n(Clos_standard_method)
#define Clos_standard_combination _n(Clos_standard_combination)
#define Clos_standard_specializer _n(Clos_standard_specializer)

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
#define getslotvector _n(getslotvector)
#define setslotvector _n(setslotvector)
#define lenslotvector _n(lenslotvector)
#define getclosvalue _n(getclosvalue)
#define setclosvalue _n(setclosvalue)
#define lenclosvalue _n(lenclosvalue)
#define clos_standard_ignore _n(clos_standard_ignore)
#define clos_standard_class_p_debug _n(clos_standard_class_p_debug)
#define clos_standard_generic_p_debug _n(clos_standard_generic_p_debug)
#define clos_standard_method_p_debug _n(clos_standard_method_p_debug)
#define clos_standard_combination_p_debug _n(clos_standard_combination_p_debug)
#define clos_standard_specializer_p_debug _n(clos_standard_specializer_p_debug)

#define slot_vector_alloc _n(slot_vector_alloc)
#define slot_vector_local _n(slot_vector_local)
#define slot_vector_heap _n(slot_vector_heap)
#define slot_vector_copy_alloc _n(slot_vector_copy_alloc)
#define slot_vector_copy_local _n(slot_vector_copy_local)
#define slot_vector_copy_heap _n(slot_vector_copy_heap)
#define slot_vector_copyheap_alloc _n(slot_vector_copyheap_alloc)
#define slot_vector_clear _n(slot_vector_clear)
#define clos_value_heap _n(clos_value_heap)
#define clos_alloc _n(clos_alloc)
#define clos_local _n(clos_local)
#define clos_heap _n(clos_heap)
#define clos_destroy _n(clos_destroy)
#define clos_swap _n(clos_swap)
#define clos_copy_alloc _n(clos_copy_alloc)
#define clos_allcopy_alloc _n(clos_allcopy_alloc)
#define clos_getslots_heap _n(clos_getslots_heap)
#define closp _n(closp)
#define clos_value_p _n(clos_value_p)
#define slot_vector_p _n(slot_vector_p)
#define clos_funcall_p _n(clos_funcall_p)
#define slot_class_p _n(slot_class_p)
#define slot_instance_p _n(slot_instance_p)
#define clos_set_funcall _n(clos_set_funcall)
#define slot_set_class _n(slot_set_class)
#define slot_set_instance _n(slot_set_instance)
#define slot_set_allocation_ _n(slot_set_allocation_)
#define clos_errorp _n(clos_errorp)
#define clos_getp _n(clos_getp)
#define clos_setp _n(clos_setp)
#define clos_checkp_ _n(clos_checkp_)
#define clos_get_ _n(clos_get_)
#define clos_set_ _n(clos_set_)
#define clos_check_ _n(clos_check_)
#define clos_getelt _n(clos_getelt)
#define clos_setelt _n(clos_setelt)
#define clos_checkelt_ _n(clos_checkelt_)
#define clos_getconst_ _n(clos_getconst_)
#define clos_setconst_ _n(clos_setconst_)
#define clos_checkconst_ _n(clos_checkconst_)
#define clos_slot_exists_p _n(clos_slot_exists_p)
#define clos_slot_boundp_nil _n(clos_slot_boundp_nil)
#define clos_slot_boundp_ _n(clos_slot_boundp_)
#define clos_slot_makunbound_nil_ _n(clos_slot_makunbound_nil_)
#define clos_slot_makunbound_ _n(clos_slot_makunbound_)
#define clos_find_class_nil _n(clos_find_class_nil)
#define clos_find_class_ _n(clos_find_class_)
#define clos_define_class _n(clos_define_class)
#define clos_find_generic_nil _n(clos_find_generic_nil)
#define clos_find_generic_ _n(clos_find_generic_)
#define clos_define_generic_ _n(clos_define_generic_)
#define clos_find_combination_nil _n(clos_find_combination_nil)
#define clos_find_combination_ _n(clos_find_combination_)
#define clos_define_combination _n(clos_define_combination)
#define clos_find_specializer_nil_ _n(clos_find_specializer_nil_)
#define clos_find_specializer_ _n(clos_find_specializer_)
#define clos_define_specializer_ _n(clos_define_specializer_)
#define clos_forget_all_specializer_unsafe _n(clos_forget_all_specializer_unsafe)
#define init_clos _n(init_clos)
#define build_clos _n(build_clos)

extern addr Clos_standard_class;
extern addr Clos_standard_generic;
extern addr Clos_standard_method;
extern addr Clos_standard_combination;
extern addr Clos_standard_specializer;

struct clos_struct {
	fixnum version;
};

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

#define GetSlotVector_Low				GetArrayA4
#define SetSlotVector_Low				SetArrayA4
#define LenSlotVector_Low				LenArrayA4
#define GetClosValue_Low				GetArrayA4
#define SetClosValue_Low				SetArrayA4
#define LenClosValue_Low				LenArrayA4

#define clos_standard_class_p_Low(x)		(Clos_standard_class == (x))
#define clos_standard_generic_p_Low(x)		(Clos_standard_generic == (x))
#define clos_standard_method_p_Low(x)		(Clos_standard_method == (x))
#define clos_standard_combination_p_Low(x)	(Clos_standard_combination == (x))
#define clos_standard_specializer_p_Low(x)	(Clos_standard_specializer == (x))

#ifdef LISP_DEBUG
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

#define GetSlotVector			getslotvector
#define SetSlotVector			setslotvector
#define LenSlotVector			lenslotvector
#define GetClosValue			getclosvalue
#define SetClosValue			setclosvalue
#define LenClosValue			lenclosvalue

#define clos_standard_class_p		clos_standard_class_p_debug
#define clos_standard_generic_p		clos_standard_generic_p_debug
#define clos_standard_method_p		clos_standard_method_p_debug
#define clos_standard_combination_p	clos_standard_combination_p_debug
#define clos_standard_specializer_p	clos_standard_specializer_p_debug

#else

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

#define GetSlotVector			GetSlotVector_Low
#define SetSlotVector			SetSlotVector_Low
#define LenSlotVector			LenSlotVector_Low
#define GetClosValue			GetClosValue_Low
#define SetClosValue			SetClosValue_Low
#define LenClosValue			LenClosValue_Low

#define clos_standard_class_p		clos_standard_class_p_Low
#define clos_standard_generic_p		clos_standard_generic_p_Low
#define clos_standard_method_p		clos_standard_method_p_Low
#define clos_standard_combination_p	clos_standard_combination_p_Low
#define clos_standard_specializer_p	clos_standard_specializer_p_Low

#endif

/* access */
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

void getslotvector(addr pos, size_t index, addr *ret);
void setslotvector(addr pos, size_t index, addr value);
void lenslotvector(addr pos, size_t *ret);
void getclosvalue(addr pos, size_t index, addr *ret);
void setclosvalue(addr pos, size_t index, addr value);
void lenclosvalue(addr pos, size_t *ret);

void clos_standard_ignore(int value);
int clos_standard_class_p_debug(addr pos);
int clos_standard_generic_p_debug(addr pos);
int clos_standard_method_p_debug(addr pos);
int clos_standard_combination_p_debug(addr pos);
int clos_standard_specializer_p_debug(addr pos);

/* allocate */
void slot_vector_alloc(LocalRoot local, addr *ret, size_t size);
void slot_vector_local(LocalRoot local, addr *ret, size_t size);
void slot_vector_heap(addr *ret, size_t size);
void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos);
void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos);
void slot_vector_copy_heap(addr *ret, addr pos);
void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos);
void slot_vector_clear(addr pos);

void clos_value_heap(addr *ret, size_t size);
void clos_alloc(LocalRoot local, addr *ret, addr slots);
void clos_local(LocalRoot local, addr *ret, addr slots);
void clos_heap(addr *ret, addr slots);

void clos_destroy(addr pos);
void clos_swap(addr a, addr b);
void clos_copy_alloc(LocalRoot local, addr pos, addr *ret);
void clos_allcopy_alloc(LocalRoot local, addr pos, addr *ret);
void clos_getslots_heap(addr pos, addr *ret);

/* control */
int closp(addr pos);
int clos_value_p(addr pos);
int slot_vector_p(addr pos);
int clos_funcall_p(addr pos);
int slot_class_p(addr pos);
int slot_instance_p(addr pos);
void clos_set_funcall(addr pos);
void slot_set_class(addr pos);
void slot_set_instance(addr pos);
int slot_set_allocation_(addr pos, addr value);

int clos_errorp(addr pos, size_t index, constindex name);
int clos_getp(addr pos, addr key, addr *ret);
int clos_setp(addr pos, addr key, addr value);
int clos_checkp_(addr pos, addr key, addr *value, int *ret);
int clos_get_(addr pos, addr key, addr *ret);
int clos_set_(addr pos, addr key, addr value);
int clos_check_(addr pos, addr key, addr *ret);
void clos_getelt(addr pos, size_t index, addr *ret);
void clos_setelt(addr pos, size_t index, addr value);
int clos_checkelt_(addr pos, size_t index, addr *ret);

int clos_getconst_(addr pos, constindex index, addr *ret);
int clos_setconst_(addr pos, constindex index, addr value);
int clos_checkconst_(addr pos, constindex index, addr *ret);
#define ClosGetConst_(p,n,r) clos_getconst_((p),CONSTANT_##n,(r))
#define ClosSetConst_(p,n,v) clos_setconst_((p),CONSTANT_##n,(v))
#define ClosCheckConst_(p,n,r) clos_checkconst_((p),CONSTANT_##n,(r))

/* check */
int clos_slot_exists_p(addr pos, addr name);
int clos_slot_boundp_nil(addr pos, addr name);
int clos_slot_boundp_(addr pos, addr name, int *ret);
int clos_slot_makunbound_nil_(addr pos, addr name, int *ret);
int clos_slot_makunbound_(addr pos, addr name);

/* talbe */
void clos_find_class_nil(addr name, addr *ret);
int clos_find_class_(addr name, addr *ret);
void clos_define_class(addr name, addr value);

void clos_find_generic_nil(addr name, addr *ret);
int clos_find_generic_(addr name, addr *ret);
int clos_define_generic_(addr name, addr value);

void clos_find_combination_nil(addr name, addr *ret);
int clos_find_combination_(addr name, addr *ret);
void clos_define_combination(addr name, addr value);

int clos_find_specializer_nil_(addr name, addr *ret);
int clos_find_specializer_(addr name, addr *ret);
int clos_define_specializer_(addr name, addr value);
void clos_forget_all_specializer_unsafe(void);

/* build */
void init_clos(void);
void build_clos(Execute ptr);

#endif

