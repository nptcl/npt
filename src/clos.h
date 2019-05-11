#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "clos_define.h"
#include "constant.h"
#include "execute.h"

extern addr Clos_standard_class;
extern addr Clos_standard_generic;
extern addr Clos_standard_method;
extern addr Clos_standard_combination;
extern addr Clos_standard_specializer;

struct slot_struct {
	size_t location;
};

struct clos_struct {
	fixnum version;
};

#define PtrSlotStruct_Low(x)			PtrBodySSa((x),SLOT_INDEX_SIZE)
#define SlotStruct_Low(x)				((struct slot_struct *)PtrSlotStruct_Low(x))
#define GetNameSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_NAME,(y))
#define SetNameSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_NAME,(y))
#define GetTypeSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_TYPE,(y))
#define SetTypeSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_TYPE,(y))
#define GetArgsSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_INITARGS,(y))
#define SetArgsSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_INITARGS,(y))
#define GetFormSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_INITFORM,(y))
#define SetFormSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_INITFORM,(y))
#define GetFunctionSlot_Low(x,y)		GetArraySS((x),SLOT_INDEX_INITFUNCTION,(y))
#define SetFunctionSlot_Low(x,y)		SetArraySS((x),SLOT_INDEX_INITFUNCTION,(y))
#define GetReadersSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_READERS,(y))
#define SetReadersSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_READERS,(y))
#define GetWritersSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_WRITERS,(y))
#define SetWritersSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_WRITERS,(y))
#define GetDocumentSlot_Low(x,y)		GetArraySS((x),SLOT_INDEX_DOCUMENT,(y))
#define SetDocumentSlot_Low(x,y)		SetArraySS((x),SLOT_INDEX_DOCUMENT,(y))
#define GetClassSlot_Low(x,y)			GetArraySS((x),SLOT_INDEX_CLASS,(y))
#define SetClassSlot_Low(x,y)			SetArraySS((x),SLOT_INDEX_CLASS,(y))
#define GetAllocationSlot_Low(x,y)		(*(y) = GetUser((x)))
#define SetAllocationSlot_Low(x,y)		(SetUser((x), (y) != 0))
#define GetLocationSlot_Low(x,y)		(*(y) = SlotStruct_Low(x)->location)
#define SetLocationSlot_Low(x,y)		(SlotStruct_Low(x)->location = (y))

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
#define SlotStruct				struct_slot
#define GetNameSlot				getname_slot
#define SetNameSlot				setname_slot
#define GetTypeSlot				gettype_slot
#define SetTypeSlot				settype_slot
#define GetArgsSlot				getargs_slot
#define SetArgsSlot				setargs_slot
#define GetFormSlot				getform_slot
#define SetFormSlot				setform_slot
#define GetFunctionSlot			getfunction_slot
#define SetFunctionSlot			setfunction_slot
#define GetReadersSlot			getreaders_slot
#define SetReadersSlot			setreaders_slot
#define GetWritersSlot			getwriters_slot
#define SetWritersSlot			setwriters_slot
#define GetDocumentSlot			getdocument_slot
#define SetDocumentSlot			setdocument_slot
#define GetClassSlot			getclass_slot
#define SetClassSlot			setclass_slot
#define GetAllocationSlot		getallocation_slot
#define SetAllocationSlot		setallocation_slot
#define GetLocationSlot			getlocation_slot
#define SetLocationSlot			setlocation_slot

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

#define SlotStruct				SlotStruct_Low
#define GetNameSlot				GetNameSlot_Low
#define SetNameSlot				SetNameSlot_Low
#define GetTypeSlot				GetTypeSlot_Low
#define SetTypeSlot				SetTypeSlot_Low
#define GetArgsSlot				GetArgsSlot_Low
#define SetArgsSlot				SetArgsSlot_Low
#define GetFormSlot				GetFormSlot_Low
#define SetFormSlot				SetFormSlot_Low
#define GetFunctionSlot			GetFunctionSlot_Low
#define SetFunctionSlot			SetFunctionSlot_Low
#define GetReadersSlot			GetReadersSlot_Low
#define SetReadersSlot			SetReadersSlot_Low
#define GetWritersSlot			GetWritersSlot_Low
#define SetWritersSlot			SetWritersSlot_Low
#define GetDocumentSlot			GetDocumentSlot_Low
#define SetDocumentSlot			SetDocumentSlot_Low
#define GetClassSlot			GetClassSlot_Low
#define SetClassSlot			SetClassSlot_Low
#define GetAllocationSlot		GetAllocationSlot_Low
#define SetAllocationSlot		SetAllocationSlot_Low
#define GetLocationSlot			GetLocationSlot_Low
#define SetLocationSlot			SetLocationSlot_Low

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
struct slot_struct *struct_slot(addr pos);
void getname_slot(addr pos, addr *ret);
void setname_slot(addr pos, addr value);
void gettype_slot(addr pos, addr *ret);
void settype_slot(addr pos, addr value);
void getargs_slot(addr pos, addr *ret);
void setargs_slot(addr pos, addr value);
void getform_slot(addr pos, addr *ret);
void setform_slot(addr pos, addr value);
void getfunction_slot(addr pos, addr *ret);
void setfunction_slot(addr pos, addr value);
void getreaders_slot(addr pos, addr *ret);
void setreaders_slot(addr pos, addr value);
void getwriters_slot(addr pos, addr *ret);
void setwriters_slot(addr pos, addr value);
void getdocument_slot(addr pos, addr *ret);
void setdocument_slot(addr pos, addr value);
void getclass_slot(addr pos, addr *ret);
void setclass_slot(addr pos, addr value);
void getallocation_slot(addr pos, int *ret);
void setallocation_slot(addr pos, int value);
void getlocation_slot(addr pos, size_t *ret);
void setlocation_slot(addr pos, size_t value);

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
void slot_alloc(LocalRoot local, addr *ret);
void slot_local(LocalRoot local, addr *ret);
void slot_heap(addr *ret);
void slot_copy_alloc(LocalRoot local, addr *ret, addr slot);
void slot_copy_local(LocalRoot local, addr *ret, addr slot);
void slot_copy_heap(addr *ret, addr slot);

void slot_vector_alloc(LocalRoot local, addr *ret, size_t size);
void slot_vector_local(LocalRoot local, addr *ret, size_t size);
void slot_vector_heap(addr *ret, size_t size);
void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos);
void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos);
void slot_vector_copy_heap(addr *ret, addr pos);
void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos);
void slot_vector_clear(addr pos);

void clos_alloc(LocalRoot local, addr *ret, addr slots);
void clos_local(LocalRoot local, addr *ret, addr slots);
void clos_heap(addr *ret, addr slots);

/* control */
int closp(addr pos);
int slotp(addr pos);
int clos_funcall_p(addr pos);
int slot_class_p(addr pos);
int slot_instance_p(addr pos);
void clos_set_funcall(addr pos);
void slot_set_class(addr pos);
void slot_set_instance(addr pos);
void slot_set_allocation(addr pos, addr value);

int clos_errorp(addr pos, size_t index, constindex name);
int clos_getp(addr pos, addr key, addr *ret);
int clos_setp(addr pos, addr key, addr value);
int clos_checkp(addr pos, addr key, addr *ret);
void clos_get(addr pos, addr key, addr *ret);
void clos_set(addr pos, addr key, addr value);
void clos_check(addr pos, addr key, addr *ret);
void clos_getelt(addr pos, size_t index, addr *ret);
void clos_setelt(addr pos, size_t index, addr value);
void clos_checkelt(addr pos, size_t index, addr *ret);

void clos_getconst(addr pos, constindex index, addr *ret);
void clos_setconst(addr pos, constindex index, addr value);
void clos_checkconst(addr pos, constindex index, addr *ret);
#define ClosGetConst(p,n,r) clos_getconst((p),CONSTANT_##n,(r))
#define ClosSetConst(p,n,v) clos_setconst((p),CONSTANT_##n,(v))
#define ClosCheckConst(p,n,r) clos_checkconst((p),CONSTANT_##n,(r))

/* check */
int clos_slot_exists_p(addr pos, addr name);
int clos_slot_boundp_nil(addr pos, addr name);
int clos_slot_boundp(addr pos, addr name);
int clos_slot_makunbound_nil(addr pos, addr name);
void clos_slot_makunbound(addr pos, addr name);

/* talbe */
void clos_find_class_nil(addr name, addr *ret);
void clos_find_class(addr name, addr *ret);
void clos_define_class(addr name, addr value);
void clos_forget_all_classes_unsafe(void);

void clos_find_generic_nil(addr name, addr *ret);
void clos_find_generic(addr name, addr *ret);
void clos_define_generic(addr name, addr value);

void clos_find_combination_nil(addr name, addr *ret);
void clos_find_combination(addr name, addr *ret);
void clos_define_combination(addr name, addr value);
void clos_forget_all_combination_unsafe(void);

void clos_find_specializer_nil(addr name, addr *ret);
void clos_find_specializer(addr name, addr *ret);
void clos_define_specializer(addr name, addr value);
void clos_forget_all_specializer_unsafe(void);

/* build */
void init_clos(void);
void build_clos(Execute ptr);

#endif

