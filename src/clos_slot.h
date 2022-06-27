#ifndef __CLOS_SLOT_HEADER__
#define __CLOS_SLOT_HEADER__

#include "clos_define.h"
#include "execute.h"
#include "local.h"
#include "typedef.h"

struct slot_struct {
	size_t location, access;
};

#define struct_slot _n(struct_slot)
#define getname_slot _n(getname_slot)
#define setname_slot _n(setname_slot)
#define gettype_slot _n(gettype_slot)
#define settype_slot _n(settype_slot)
#define getargs_slot _n(getargs_slot)
#define setargs_slot _n(setargs_slot)
#define getform_slot _n(getform_slot)
#define setform_slot _n(setform_slot)
#define getfunction_slot _n(getfunction_slot)
#define setfunction_slot _n(setfunction_slot)
#define getreaders_slot _n(getreaders_slot)
#define setreaders_slot _n(setreaders_slot)
#define getwriters_slot _n(getwriters_slot)
#define setwriters_slot _n(setwriters_slot)
#define getdocument_slot _n(getdocument_slot)
#define setdocument_slot _n(setdocument_slot)
#define getclass_slot _n(getclass_slot)
#define setclass_slot _n(setclass_slot)
#define getreadonly_slot _n(getreadonly_slot)
#define setreadonly_slot _n(setreadonly_slot)
#define getallocation_slot _n(getallocation_slot)
#define setallocation_slot _n(setallocation_slot)
#define getlocation_slot _n(getlocation_slot)
#define setlocation_slot _n(setlocation_slot)
#define getaccess_slot _n(getaccess_slot)
#define setaccess_slot _n(setaccess_slot)

#define slotp _n(slotp)
#define slot_alloc _n(slot_alloc)
#define slot_local _n(slot_local)
#define slot_heap _n(slot_heap)
#define slot_copy_alloc _n(slot_copy_alloc)
#define slot_copy_local _n(slot_copy_local)
#define slot_copy_heap _n(slot_copy_heap)

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
#endif

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
void getreadonly_slot(addr pos, addr *ret);
void setreadonly_slot(addr pos, addr value);
void getallocation_slot(addr pos, int *ret);
void setallocation_slot(addr pos, int value);
void getlocation_slot(addr pos, size_t *ret);
void setlocation_slot(addr pos, size_t value);
void getaccess_slot(addr pos, size_t *ret);
void setaccess_slot(addr pos, size_t value);

int slotp(addr pos);
void slot_alloc(LocalRoot local, addr *ret);
void slot_local(LocalRoot local, addr *ret);
void slot_heap(addr *ret);
void slot_copy_alloc(LocalRoot local, addr *ret, addr slot);
void slot_copy_local(LocalRoot local, addr *ret, addr slot);
void slot_copy_heap(addr *ret, addr slot);

#endif

