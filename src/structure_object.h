#ifndef __STRUCTURE_OBJECT_HEADER__
#define __STRUCTURE_OBJECT_HEADER__

#include "hold.h"
#include "structure_typedef.h"
#include "typedef.h"

/*
 *  access
 */
#define stdget_structure_name_ _n(stdget_structure_name_)
#define stdset_structure_name_ _n(stdset_structure_name_)
#define stdget_structure_direct_slots_ _n(stdget_structure_direct_slots_)
#define stdset_structure_direct_slots_ _n(stdset_structure_direct_slots_)
#define stdget_structure_slots_ _n(stdget_structure_slots_)
#define stdset_structure_slots_ _n(stdset_structure_slots_)
#define stdget_structure_documentation_ _n(stdget_structure_documentation_)
#define stdset_structure_documentation_ _n(stdset_structure_documentation_)
#define stdget_structure_include_ _n(stdget_structure_include_)
#define stdset_structure_include_ _n(stdset_structure_include_)
#define stdget_structure_precedence_list_ _n(stdget_structure_precedence_list_)
#define stdset_structure_precedence_list_ _n(stdset_structure_precedence_list_)
#define stdget_structure_type_ _n(stdget_structure_type_)
#define stdset_structure_type_ _n(stdset_structure_type_)
#define stdget_structure_vector_ _n(stdget_structure_vector_)
#define stdset_structure_vector_ _n(stdset_structure_vector_)
#define stdget_structure_named_ _n(stdget_structure_named_)
#define stdset_structure_named_ _n(stdset_structure_named_)
#define stdget_structure_named_index_ _n(stdget_structure_named_index_)
#define stdset_structure_named_index_ _n(stdset_structure_named_index_)
#define stdget_structure_value_ _n(stdget_structure_value_)
#define stdset_structure_value_ _n(stdset_structure_value_)
#define stdget_structure_predicate_ _n(stdget_structure_predicate_)
#define stdset_structure_predicate_ _n(stdset_structure_predicate_)
#define stdget_structure_access_ _n(stdget_structure_access_)
#define stdset_structure_access_ _n(stdset_structure_access_)
#define stdget_structure_copier_ _n(stdget_structure_copier_)
#define stdset_structure_copier_ _n(stdset_structure_copier_)
#define stdget_structure_constructor_ _n(stdget_structure_constructor_)
#define stdset_structure_constructor_ _n(stdset_structure_constructor_)
#define stdget_structure_print_ _n(stdget_structure_print_)
#define stdset_structure_print_ _n(stdset_structure_print_)

#define localhold_destruct _n(localhold_destruct)
#define defstruct_clean _n(defstruct_clean)

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

#define structure_getdirect_ _n(structure_getdirect_)
#define structure_setdirect_ _n(structure_setdirect_)
#define structure_getarray_ _n(structure_getarray_)
#define structure_setarray_ _n(structure_setarray_)

#define structure_type_heap _n(structure_type_heap)
#define structure_type _n(structure_type)
#define structure_type_list_p _n(structure_type_list_p)
#define structure_type_vector_p _n(structure_type_vector_p)

int stdget_structure_name_(addr pos, addr *ret);
int stdset_structure_name_(addr pos, addr value);
int stdget_structure_direct_slots_(addr pos, addr *ret);
int stdset_structure_direct_slots_(addr pos, addr value);
int stdget_structure_slots_(addr pos, addr *ret);
int stdset_structure_slots_(addr pos, addr value);
int stdget_structure_documentation_(addr pos, addr *ret);
int stdset_structure_documentation_(addr pos, addr value);
int stdget_structure_include_(addr pos, addr *ret);
int stdset_structure_include_(addr pos, addr value);
int stdget_structure_precedence_list_(addr pos, addr *ret);
int stdset_structure_precedence_list_(addr pos, addr value);
int stdget_structure_type_(addr pos, addr *ret);
int stdset_structure_type_(addr pos, addr value);
int stdget_structure_vector_(addr pos, addr *ret);
int stdset_structure_vector_(addr pos, addr value);
int stdget_structure_named_(addr pos, addr *ret);
int stdset_structure_named_(addr pos, addr value);
int stdget_structure_named_index_(addr pos, addr *ret);
int stdset_structure_named_index_(addr pos, addr value);
int stdget_structure_value_(addr pos, addr *ret);
int stdset_structure_value_(addr pos, addr value);
int stdget_structure_predicate_(addr pos, addr *ret);
int stdset_structure_predicate_(addr pos, addr value);
int stdget_structure_access_(addr pos, addr *ret);
int stdset_structure_access_(addr pos, addr value);
int stdget_structure_copier_(addr pos, addr *ret);
int stdset_structure_copier_(addr pos, addr value);
int stdget_structure_constructor_(addr pos, addr *ret);
int stdset_structure_constructor_(addr pos, addr value);
int stdget_structure_print_(addr pos, addr *ret);
int stdset_structure_print_(addr pos, addr value);

/*
 *  defstruct
 */
void localhold_destruct(LocalHold hold, struct defstruct *str);
void defstruct_clean(struct defstruct *ptr);


/*
 *  structure-type
 */
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

int structure_getdirect_(Execute ptr, addr vector, size_t i, addr type, addr *ret);
int structure_setdirect_(Execute ptr, addr vector, size_t i, addr type, addr value);
int structure_getarray_(Execute ptr, addr vector, addr slot, addr type, addr *ret);
int structure_setarray_(Execute ptr, addr vector, addr slot, addr type, addr value);

void structure_type_heap(addr *ret);
void structure_type(struct defstruct *str, addr slot, addr *ret);
int structure_type_list_p(addr type, addr var, int *ret);
int structure_type_vector_p(Execute ptr, addr type, addr var, int *ret);

#endif

