#ifndef __CLOS_HEADER__
#define __CLOS_HEADER__

#include "clos_define.h"
#include "constant.h"
#include "execute.h"

__extern addr Clos_standard_class;
__extern addr Clos_standard_generic;
__extern addr Clos_standard_method;
__extern addr Clos_standard_combination;
__extern addr Clos_standard_specializer;

struct slot_struct {
	size_t location, access;
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
#define GetReadOnlySlot_Low(x,y)		GetArraySS((x),SLOT_INDEX_READONLY,(y))
#define SetReadOnlySlot_Low(x,y)		SetArraySS((x),SLOT_INDEX_READONLY,(y))
#define GetAllocationSlot_Low(x,y)		(*(y) = GetUser((x)))
#define SetAllocationSlot_Low(x,y)		(SetUser((x), (y) != 0))
#define GetLocationSlot_Low(x,y)		(*(y) = SlotStruct_Low(x)->location)
#define SetLocationSlot_Low(x,y)		(SlotStruct_Low(x)->location = (y))
#define GetAccessSlot_Low(x,y)			(*(y) = SlotStruct_Low(x)->access)
#define SetAccessSlot_Low(x,y)			(SlotStruct_Low(x)->access = (y))

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
#define GetReadOnlySlot			getreadonly_slot
#define SetReadOnlySlot			setreadonly_slot
#define GetAllocationSlot		getallocation_slot
#define SetAllocationSlot		setallocation_slot
#define GetLocationSlot			getlocation_slot
#define SetLocationSlot			setlocation_slot
#define GetAccessSlot			getaccess_slot
#define SetAccessSlot			setaccess_slot

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
#define GetReadOnlySlot			GetReadOnlySlot_Low
#define SetReadOnlySlot			SetReadOnlySlot_Low
#define GetAllocationSlot		GetAllocationSlot_Low
#define SetAllocationSlot		SetAllocationSlot_Low
#define GetLocationSlot			GetLocationSlot_Low
#define SetLocationSlot			SetLocationSlot_Low
#define GetAccessSlot			GetAccessSlot_Low
#define SetAccessSlot			SetAccessSlot_Low

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
_g struct slot_struct *struct_slot(addr pos);
_g void getname_slot(addr pos, addr *ret);
_g void setname_slot(addr pos, addr value);
_g void gettype_slot(addr pos, addr *ret);
_g void settype_slot(addr pos, addr value);
_g void getargs_slot(addr pos, addr *ret);
_g void setargs_slot(addr pos, addr value);
_g void getform_slot(addr pos, addr *ret);
_g void setform_slot(addr pos, addr value);
_g void getfunction_slot(addr pos, addr *ret);
_g void setfunction_slot(addr pos, addr value);
_g void getreaders_slot(addr pos, addr *ret);
_g void setreaders_slot(addr pos, addr value);
_g void getwriters_slot(addr pos, addr *ret);
_g void setwriters_slot(addr pos, addr value);
_g void getdocument_slot(addr pos, addr *ret);
_g void setdocument_slot(addr pos, addr value);
_g void getclass_slot(addr pos, addr *ret);
_g void setclass_slot(addr pos, addr value);
_g void getreadonly_slot(addr pos, addr *ret);
_g void setreadonly_slot(addr pos, addr value);
_g void getallocation_slot(addr pos, int *ret);
_g void setallocation_slot(addr pos, int value);
_g void getlocation_slot(addr pos, size_t *ret);
_g void setlocation_slot(addr pos, size_t value);
_g void getaccess_slot(addr pos, size_t *ret);
_g void setaccess_slot(addr pos, size_t value);

_g struct clos_struct *struct_clos(addr pos);
_g void getclassof_clos(addr pos, addr *ret);
_g void setclassof_clos(addr pos, addr value);
_g void getslot_clos(addr pos, addr *ret);
_g void setslot_clos(addr pos, addr value);
_g void getvalue_clos(addr pos, addr *ret);
_g void setvalue_clos(addr pos, addr value);
_g void getfuncall_clos(addr pos, int *ret);
_g void setfuncall_clos(addr pos, int value);
_g void getversion_clos(addr pos, fixnum *ret);
_g void setversion_clos(addr pos, fixnum value);

_g void getslotvector(addr pos, size_t index, addr *ret);
_g void setslotvector(addr pos, size_t index, addr value);
_g void lenslotvector(addr pos, size_t *ret);
_g void getclosvalue(addr pos, size_t index, addr *ret);
_g void setclosvalue(addr pos, size_t index, addr value);
_g void lenclosvalue(addr pos, size_t *ret);

_g void clos_standard_ignore(int value);
_g int clos_standard_class_p_debug(addr pos);
_g int clos_standard_generic_p_debug(addr pos);
_g int clos_standard_method_p_debug(addr pos);
_g int clos_standard_combination_p_debug(addr pos);
_g int clos_standard_specializer_p_debug(addr pos);

/* allocate */
_g void slot_alloc(LocalRoot local, addr *ret);
_g void slot_local(LocalRoot local, addr *ret);
_g void slot_heap(addr *ret);
_g void slot_copy_alloc(LocalRoot local, addr *ret, addr slot);
_g void slot_copy_local(LocalRoot local, addr *ret, addr slot);
_g void slot_copy_heap(addr *ret, addr slot);

_g void slot_vector_alloc(LocalRoot local, addr *ret, size_t size);
_g void slot_vector_local(LocalRoot local, addr *ret, size_t size);
_g void slot_vector_heap(addr *ret, size_t size);
_g void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos);
_g void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos);
_g void slot_vector_copy_heap(addr *ret, addr pos);
_g void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos);
_g void slot_vector_clear(addr pos);

_g void clos_value_heap(addr *ret, size_t size);
_g void clos_alloc(LocalRoot local, addr *ret, addr slots);
_g void clos_local(LocalRoot local, addr *ret, addr slots);
_g void clos_heap(addr *ret, addr slots);

_g void clos_destroy(addr pos);
_g void clos_swap(addr a, addr b);

/* control */
_g int closp(addr pos);
_g int slotp(addr pos);
_g int clos_value_p(addr pos);
_g int slot_vector_p(addr pos);
_g int clos_funcall_p(addr pos);
_g int slot_class_p(addr pos);
_g int slot_instance_p(addr pos);
_g void clos_set_funcall(addr pos);
_g void slot_set_class(addr pos);
_g void slot_set_instance(addr pos);
_g int slot_set_allocation_(addr pos, addr value);

_g int clos_errorp(addr pos, size_t index, constindex name);
_g int clos_getp(addr pos, addr key, addr *ret);
_g int clos_setp(addr pos, addr key, addr value);
_g int clos_checkp_(addr pos, addr key, addr *value, int *ret);
_g int clos_get_(addr pos, addr key, addr *ret);
_g int clos_set_(addr pos, addr key, addr value);
_g int clos_check_(addr pos, addr key, addr *ret);
_g void clos_getelt(addr pos, size_t index, addr *ret);
_g void clos_setelt(addr pos, size_t index, addr value);
_g int clos_checkelt_(addr pos, size_t index, addr *ret);

_g int clos_getconst_(addr pos, constindex index, addr *ret);
_g int clos_setconst_(addr pos, constindex index, addr value);
_g int clos_checkconst_(addr pos, constindex index, addr *ret);
#define ClosGetConst_(p,n,r) clos_getconst_((p),CONSTANT_##n,(r))
#define ClosSetConst_(p,n,v) clos_setconst_((p),CONSTANT_##n,(v))
#define ClosCheckConst(p,n,r) clos_checkconst_((p),CONSTANT_##n,(r))

/* check */
_g int clos_slot_exists_p(addr pos, addr name);
_g int clos_slot_boundp_nil(addr pos, addr name);
_g int clos_slot_boundp_(addr pos, addr name, int *ret);
_g int clos_slot_makunbound_nil_(addr pos, addr name, int *ret);
_g int clos_slot_makunbound_(addr pos, addr name);

/* talbe */
_g void clos_find_class_nil(addr name, addr *ret);
_g int clos_find_class_(addr name, addr *ret);
_g void clos_define_class(addr name, addr value);

_g void clos_find_generic_nil(addr name, addr *ret);
_g int clos_find_generic_(addr name, addr *ret);
_g int clos_define_generic_(addr name, addr value);

_g void clos_find_combination_nil(addr name, addr *ret);
_g int clos_find_combination_(addr name, addr *ret);
_g void clos_define_combination(addr name, addr value);

_g int clos_find_specializer_nil_(addr name, addr *ret);
_g int clos_find_specializer_(addr name, addr *ret);
_g int clos_define_specializer_(addr name, addr value);
_g void clos_forget_all_specializer_unsafe(void);

/* build */
_g void init_clos(void);
_g void build_clos(Execute ptr);

#endif

