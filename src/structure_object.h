#ifndef __STRUCTURE_OBJECT_HEADER__
#define __STRUCTURE_OBJECT_HEADER__

#include "memory.h"
#include "typedef.h"

struct structure_struct {
	unsigned named_p : 1;
	unsigned vector_p : 1;
	unsigned list_p : 1;
	enum ARRAY_TYPE type1;
	int type2;
	size_t size, size_all, offset;
};
enum StructureIndex {
	Structure_name,
	Structure_slots,
	Structure_direct,
	Structure_doc,
	Structure_include,
	Structure_precedence,
	Structure_specialized,
	Structure_vector,
	Structure_predicate,
	Structure_access,
	Structure_copier,
	Structure_constructor,
	Structure_size
};

#define PtrStructure_Low(x)            \
	((struct structure_struct *)PtrBodySSa((x), Structure_size))
#define GetNameStructure_Low(x,y)       GetArraySS((x),Structure_name,(y))
#define SetNameStructure_Low(x,y)       SetArraySS((x),Structure_name,(y))
#define GetSlotsStructure_Low(x,y)      GetArraySS((x),Structure_slots,(y))
#define SetSlotsStructure_Low(x,y)      SetArraySS((x),Structure_slots,(y))
#define GetDirectStructure_Low(x,y)     GetArraySS((x),Structure_direct,(y))
#define SetDirectStructure_Low(x,y)     SetArraySS((x),Structure_direct,(y))
#define GetDocStructure_Low(x,y)        GetArraySS((x),Structure_doc,(y))
#define SetDocStructure_Low(x,y)        SetArraySS((x),Structure_doc,(y))
#define GetIncludeStructure_Low(x,y)    GetArraySS((x),Structure_include,(y))
#define SetIncludeStructure_Low(x,y)    SetArraySS((x),Structure_include,(y))
#define GetPrecedenceStructure_Low(x,y) GetArraySS((x),Structure_precedence,(y))
#define SetPrecedenceStructure_Low(x,y) SetArraySS((x),Structure_precedence,(y))
#define GetSpecializedStructure_Low(x,y)    GetArraySS((x),Structure_specialized,(y))
#define SetSpecializedStructure_Low(x,y)    SetArraySS((x),Structure_specialized,(y))
#define GetPredicateStructure_Low(x,y)  GetArraySS((x),Structure_predicate,(y))
#define SetPredicateStructure_Low(x,y)  SetArraySS((x),Structure_predicate,(y))
#define GetAccessStructure_Low(x,y)     GetArraySS((x),Structure_access,(y))
#define SetAccessStructure_Low(x,y)     SetArraySS((x),Structure_access,(y))
#define GetCopierStructure_Low(x,y)     GetArraySS((x),Structure_copier,(y))
#define SetCopierStructure_Low(x,y)     SetArraySS((x),Structure_copier,(y))
#define GetConstructorStructure_Low(x,y)    GetArraySS((x),Structure_constructor,(y))
#define SetConstructorStructure_Low(x,y)    SetArraySS((x),Structure_constructor,(y))

#ifdef LISP_DEBUG
#define PtrStructure                    PtrStructure_Low
#define GetNameStructure                GetNameStructure_Low
#define SetNameStructure                SetNameStructure_Low
#define GetSlotsStructure               GetSlotsStructure_Low
#define SetSlotsStructure               SetSlotsStructure_Low
#define GetDirectStructure              GetDirectStructure_Low
#define SetDirectStructure              SetDirectStructure_Low
#define GetDocStructure                 GetDocStructure_Low
#define SetDocStructure                 SetDocStructure_Low
#define GetIncludeStructure             GetIncludeStructure_Low
#define SetIncludeStructure             SetIncludeStructure_Low
#define GetPrecedenceStructure          GetPrecedenceStructure_Low
#define SetPrecedenceStructure          SetPrecedenceStructure_Low
#define GetSpecializedStructure         GetSpecializedStructure_Low
#define SetSpecializedStructure         SetSpecializedStructure_Low
#define GetPredicateStructure           GetPredicateStructure_Low
#define SetPredicateStructure           SetPredicateStructure_Low
#define GetAccessStructure              GetAccessStructure_Low
#define SetAccessStructure              SetAccessStructure_Low
#define GetCopierStructure              GetCopierStructure_Low
#define SetCopierStructure              SetCopierStructure_Low
#define GetConstructorStructure         GetConstructorStructure_Low
#define SetConstructorStructure         SetConstructorStructure_Low
#else
#define PtrStructure                    ptrstructure
#define GetNameStructure                getnamestructure
#define SetNameStructure                setnamestructure
#define GetSlotsStructure               getslotsstructure
#define SetSlotsStructure               setslotsstructure
#define GetDirectStructure              getdirectstructure
#define SetDirectStructure              setdirectstructure
#define GetDocStructure                 getdocstructure
#define SetDocStructure                 setdocstructure
#define GetIncludeStructure             getincludestructure
#define SetIncludeStructure             setincludestructure
#define GetPrecedenceStructure          getprecedencestructure
#define SetPrecedenceStructure          setprecedencestructure
#define GetSpecializedStructure         getspecializedstructure
#define SetSpecializedStructure         setspecializedstructure
#define GetPredicateStructure           getpredicatestructure
#define SetPredicateStructure           setpredicatestructure
#define GetAccessStructure              getaccessstructure
#define SetAccessStructure              setaccessstructure
#define GetCopierStructure              getcopierstructure
#define SetCopierStructure              setcopierstructure
#define GetConstructorStructure         getconstructorstructure
#define SetConstructorStructure         setconstructorstructure
#endif

#define structure_object_p _n(structure_object_p)
#define ptrstructure _n(ptrstructure)
#define structure_heap _n(structure_heap)
#define structurep _n(structurep)
#define getnamestructure _n(getnamestructure)
#define setnamestructure _n(setnamestructure)
#define getslotsstructure _n(getslotsstructure)
#define setslotsstructure _n(setslotsstructure)
#define getdirectstructure _n(getdirectstructure)
#define setdirectstructure _n(setdirectstructure)
#define getdocstructure _n(getdocstructure)
#define setdocstructure _n(setdocstructure)
#define getincludestructure _n(getincludestructure)
#define setincludestructure _n(setincludestructure)
#define getprecedencestructure _n(getprecedencestructure)
#define setprecedencestructure _n(setprecedencestructure)
#define getspecializedstructure _n(getspecializedstructure)
#define setspecializedstructure _n(setspecializedstructure)
#define getpredicatestructure _n(getpredicatestructure)
#define setpredicatestructure _n(setpredicatestructure)
#define getaccessstructure _n(getaccessstructure)
#define setaccessstructure _n(setaccessstructure)
#define getcopierstructure _n(getcopierstructure)
#define setcopierstructure _n(setcopierstructure)
#define getconstructorstructure _n(getconstructorstructure)
#define setconstructorstructure _n(setconstructorstructure)

#define structure_named_p _n(structure_named_p)
#define structure_list_p _n(structure_list_p)
#define structure_vector_p _n(structure_vector_p)
#define set_named_p_structure _n(set_named_p_structure)
#define set_list_p_structure _n(set_list_p_structure)
#define set_vector_p_structure _n(set_vector_p_structure)
#define get_size_structure _n(get_size_structure)
#define get_size_all_structure _n(get_size_all_structure)
#define get_offset_structure _n(get_offset_structure)
#define set_size_structure _n(set_size_structure)
#define set_size_all_structure _n(set_size_all_structure)
#define set_offset_structure _n(set_offset_structure)
#define gettype_structure _n(gettype_structure)
#define settype_structure _n(settype_structure)
#define structure_swap _n(structure_swap)

int structure_object_p(addr pos);
struct structure_struct *ptrstructure(addr pos);
void getnamestructure(addr pos, addr *ret);
void setnamestructure(addr pos, addr value);
void getslotsstructure(addr pos, addr *ret);
void setslotsstructure(addr pos, addr value);
void getdirectstructure(addr pos, addr *ret);
void setdirectstructure(addr pos, addr value);
void getdocstructure(addr pos, addr *ret);
void setdocstructure(addr pos, addr value);
void getincludestructure(addr pos, addr *ret);
void setincludestructure(addr pos, addr value);
void getprecedencestructure(addr pos, addr *ret);
void setprecedencestructure(addr pos, addr value);
void getspecializedstructure(addr pos, addr *ret);
void setspecializedstructure(addr pos, addr value);
void getpredicatestructure(addr pos, addr *ret);
void setpredicatestructure(addr pos, addr value);
void getaccessstructure(addr pos, addr *ret);
void setaccessstructure(addr pos, addr value);
void getcopierstructure(addr pos, addr *ret);
void setcopierstructure(addr pos, addr value);
void getconstructorstructure(addr pos, addr *ret);
void setconstructorstructure(addr pos, addr value);
void structure_heap(addr *ret);

int structure_named_p(addr pos);
int structure_list_p(addr pos);
int structure_vector_p(addr pos);
void set_named_p_structure(addr pos, int value);
void set_list_p_structure(addr pos, int value);
void set_vector_p_structure(addr pos, int value);
size_t get_size_structure(addr pos);
size_t get_size_all_structure(addr pos);
size_t get_offset_structure(addr pos);
void set_size_structure(addr pos, size_t value);
void set_size_all_structure(addr pos, size_t value);
void set_offset_structure(addr pos, size_t value);
void gettype_structure(addr pos, enum ARRAY_TYPE *rtype1, int *rtype2);
void settype_structure(addr pos, enum ARRAY_TYPE type1, int type2);
void structure_swap(addr x, addr y);

#endif

