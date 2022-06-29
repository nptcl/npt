#include "clos_define.h"
#include "clos_slot.h"
#include "closget_slot.h"
#include "memory.h"
#include "typedef.h"

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
#define GetLocationSlot_Low(x,y)		(*(y) = struct_slot(x)->location)
#define SetLocationSlot_Low(x,y)		(struct_slot(x)->location = (y))
#define GetAccessSlot_Low(x,y)			(*(y) = struct_slot(x)->access)
#define SetAccessSlot_Low(x,y)			(struct_slot(x)->access = (y))

/*
 *  slot-value
 */
void getname_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetNameSlot_Low(pos, ret);
}

void gettype_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetTypeSlot_Low(pos, ret);
}

void getargs_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetArgsSlot_Low(pos, ret);
}

void getform_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFormSlot_Low(pos, ret);
}

void getfunction_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFunctionSlot_Low(pos, ret);
}

void getreaders_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadersSlot_Low(pos, ret);
}

void getwriters_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetWritersSlot_Low(pos, ret);
}

void getdocument_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetDocumentSlot_Low(pos, ret);
}

void getclass_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetClassSlot_Low(pos, ret);
}

void getreadonly_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadOnlySlot_Low(pos, ret);
}

void getallocation_slot(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, ret);
}

void getlocation_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetLocationSlot_Low(pos, ret);
}

void getaccess_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAccessSlot_Low(pos, ret);
}


/*
 *  (setf slot-value)
 */
void setname_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameSlot_Low(pos, value);
}

void settype_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeSlot_Low(pos, value);
}

void setargs_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArgsSlot_Low(pos, value);
}

void setform_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFormSlot_Low(pos, value);
}

void setfunction_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionSlot_Low(pos, value);
}

void setreaders_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadersSlot_Low(pos, value);
}

void setwriters_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetWritersSlot_Low(pos, value);
}

void setdocument_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDocumentSlot_Low(pos, value);
}

void setclass_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassSlot_Low(pos, value);
}

void setreadonly_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadOnlySlot_Low(pos, value);
}

void setallocation_slot(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAllocationSlot_Low(pos, value);
}

void setlocation_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLocationSlot_Low(pos, value);
}

void setaccess_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAccessSlot_Low(pos, value);
}

