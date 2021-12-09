#ifndef __STRUCTURE_DIRECT_HEADER__
#define __STRUCTURE_DIRECT_HEADER__

#include "memory.h"
#include "structure_defstruct.h"
#include "typedef.h"

struct structure_type_struct {
	unsigned named : 1;
	unsigned errorp : 1;
	enum ARRAY_TYPE type1;
	int type2;
	size_t size, size_all, offset, named_index;
};
enum StructureTypeIndex {
	StructureType_instance,
	StructureType_name,
	StructureType_slot,
	StructureType_vector,
	StructureType_size
};

#define PtrStructureType_Low(x)            \
	((struct structure_type_struct *)PtrBodySSa((x), StructureType_size))
#define GetInstanceStructureType_Low(x,y)   GetArraySS((x),StructureType_instance,(y))
#define SetInstanceStructureType_Low(x,y)   SetArraySS((x),StructureType_instance,(y))
#define GetNameStructureType_Low(x,y)       GetArraySS((x),StructureType_name,(y))
#define SetNameStructureType_Low(x,y)       SetArraySS((x),StructureType_name,(y))
#define GetSlotStructureType_Low(x,y)       GetArraySS((x),StructureType_slot,(y))
#define SetSlotStructureType_Low(x,y)       SetArraySS((x),StructureType_slot,(y))
#define GetVectorStructureType_Low(x,y)     GetArraySS((x),StructureType_vector,(y))
#define SetVectorStructureType_Low(x,y)     SetArraySS((x),StructureType_vector,(y))
#define RefNamedStructureType_Low(x)        (PtrStructureType(x)->named != 0)
#define GetNamedStructureType_Low(x,y)      (*(y) = (PtrStructureType(x)->named != 0))
#define SetNamedStructureType_Low(x,y)      (PtrStructureType(x)->named = (y) != 0)
#define RefErrorpStructureType_Low(x)       (PtrStructureType(x)->errorp != 0)
#define GetErrorpStructureType_Low(x,y)     (*(y) = (PtrStructureType(x)->errorp != 0))
#define SetErrorpStructureType_Low(x,y)     (PtrStructureType(x)->errorp = (y) != 0)

#ifdef LISP_DEBUG
#define PtrStructureType           PtrStructureType_Low
#define GetInstanceStructureType   GetInstanceStructureType_Low
#define SetInstanceStructureType   SetInstanceStructureType_Low
#define GetNameStructureType       GetNameStructureType_Low
#define SetNameStructureType       SetNameStructureType_Low
#define GetSlotStructureType       GetSlotStructureType_Low
#define SetSlotStructureType       SetSlotStructureType_Low
#define GetVectorStructureType     GetVectorStructureType_Low
#define SetVectorStructureType     SetVectorStructureType_Low
#define RefNamedStructureType      RefNamedStructureType_Low
#define GetNamedStructureType      GetNamedStructureType_Low
#define SetNamedStructureType      SetNamedStructureType_Low
#define RefErrorpStructureType     RefErrorpStructureType_Low
#define GetErrorpStructureType     GetErrorpStructureType_Low
#define SetErrorpStructureType     SetErrorpStructureType_Low
#else
#define PtrStructureType           ptrstructuretype
#define GetInstanceStructureType   getinstancestructuretype
#define SetInstanceStructureType   setinstancestructuretype
#define GetNameStructureType       getnamestructuretype
#define SetNameStructureType       setnamestructuretype
#define GetSlotStructureType       getslotstructuretype
#define SetSlotStructureType       setslotstructuretype
#define GetVectorStructureType     getvectorstructuretype
#define SetVectorStructureType     setvectorstructuretype
#define RefNamedStructureType      refnamedstructuretype
#define GetNamedStructureType      getnamedstructuretype
#define SetNamedStructureType      setnamedstructuretype
#define RefErrorpStructureType     referrorpstructuretype
#define GetErrorpStructureType     geterrorpstructuretype
#define SetErrorpStructureType     seterrorpstructuretype
#endif

#define ptrstructuretype _n(ptrstructuretype)
#define getinstancestructuretype _n(getinstancestructuretype)
#define setinstancestructuretype _n(setinstancestructuretype)
#define getnamestructuretype _n(getnamestructuretype)
#define setnamestructuretype _n(setnamestructuretype)
#define getslotstructuretype _n(getslotstructuretype)
#define setslotstructuretype _n(setslotstructuretype)
#define getvectorstructuretype _n(getvectorstructuretype)
#define setvectorstructuretype _n(setvectorstructuretype)
#define refnamedstructuretype _n(refnamedstructuretype)
#define getnamedstructuretype _n(getnamedstructuretype)
#define setnamedstructuretype _n(setnamedstructuretype)
#define referrorpstructuretype _n(referrorpstructuretype)
#define geterrorpstructuretype _n(geterrorpstructuretype)
#define seterrorpstructuretype _n(seterrorpstructuretype)

#define structure_getarray_ _n(structure_getarray_)
#define structure_write1_ _n(structure_write1_)
#define structure_write2_ _n(structure_write2_)
#define structure_write3_ _n(structure_write3_)
#define structure_type_heap _n(structure_type_heap)
#define structure_type _n(structure_type)
#define structure_type_list_p _n(structure_type_list_p)
#define structure_type_vector_p _n(structure_type_vector_p)

struct structure_type_struct *ptrstructuretype(addr pos);
void getinstancestructuretype(addr pos, addr *ret);
void setinstancestructuretype(addr pos, addr value);
void getnamestructuretype(addr pos, addr *ret);
void setnamestructuretype(addr pos, addr value);
void getslotstructuretype(addr pos, addr *ret);
void setslotstructuretype(addr pos, addr value);
void getvectorstructuretype(addr pos, addr *ret);
void setvectorstructuretype(addr pos, addr value);
int refnamedstructuretype(addr pos);
void getnamedstructuretype(addr pos, int *ret);
void setnamedstructuretype(addr pos, int value);
int referrorpstructuretype(addr pos);
void geterrorpstructuretype(addr pos, int *ret);
void seterrorpstructuretype(addr pos, int value);

int structure_getarray_(addr vector, addr slot, addr *ret);
int structure_write1_(Execute ptr, addr instance, addr slot, addr value);
int structure_write2_(Execute ptr, addr list, addr slot, addr value);
int structure_write3_(Execute ptr, addr vector, addr slot, addr type1, addr value);
void structure_type_heap(addr *ret);
void structure_type(struct defstruct *str, addr slot, addr *ret);
int structure_type_list_p(addr type, addr var, int *ret);
int structure_type_vector_p(Execute ptr, addr type, addr var, int *ret);

#endif

