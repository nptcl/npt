#ifndef __ARRAY_HEADER__
#define __ARRAY_HEADER__

#include "array_typedef.h"
#include "local.h"
#include "typedef.h"
#include "memory.h"

#define arraygen_set_debug _n(arraygen_set_debug)
#define arraygen_get_debug _n(arraygen_get_debug)
#define arraygen_len_debug _n(arraygen_len_debug)
#define arraygen_lenr_debug _n(arraygen_lenr_debug)
#define arrayspec_pos_debug _n(arrayspec_pos_debug)
#define arrayspec_ptr_debug _n(arrayspec_ptr_debug)
#define arraysize_ptr_debug _n(arraysize_ptr_debug)
#define arrayinfo_struct_debug _n(arrayinfo_struct_debug)
#define getarrayinfo_debug _n(getarrayinfo_debug)
#define setarrayinfo_debug _n(setarrayinfo_debug)
#define lenarrayinfo_debug _n(lenarrayinfo_debug)
#define lenarrayinfor_debug _n(lenarrayinfor_debug)
#define arraygen_alloc _n(arraygen_alloc)
#define arraygen_local _n(arraygen_local)
#define arraygen_heap _n(arraygen_heap)
#define arrayspec_alloc _n(arrayspec_alloc)
#define arrayspec_local _n(arrayspec_local)
#define arrayspec_heap _n(arrayspec_heap)
#define arrayinfo_alloc _n(arrayinfo_alloc)
#define arrayinfo_local _n(arrayinfo_local)
#define arrayinfo_heap _n(arrayinfo_heap)
#define arraysize_alloc_ _n(arraysize_alloc_)
#define arraysize_local_ _n(arraysize_local_)
#define arraysize_heap_ _n(arraysize_heap_)
#define arraysize_copy_alloc_ _n(arraysize_copy_alloc_)
#define arraysize_copy_local_ _n(arraysize_copy_local_)
#define arraysize_copy_heap_ _n(arraysize_copy_heap_)
#define array_empty_alloc _n(array_empty_alloc)
#define array_empty_local _n(array_empty_local)
#define array_empty_heap _n(array_empty_heap)
#define array_alloc_ _n(array_alloc_)
#define array_local_ _n(array_local_)
#define array_heap_ _n(array_heap_)
#define array_va_alloc_ _n(array_va_alloc_)
#define array_va_local_ _n(array_va_local_)
#define array_va_heap_ _n(array_va_heap_)
#define array_system_general_p _n(array_system_general_p)
#define array_system_specialized_p _n(array_system_specialized_p)
#define array_system_p _n(array_system_p)
#define arrayp _n(arrayp)
#define array_simple_p _n(array_simple_p)
#define array_vector_p _n(array_vector_p)
#define array_displaced_p _n(array_displaced_p)
#define array_size_vector_p _n(array_size_vector_p)
#define array_general_p _n(array_general_p)
#define array_specialized_p _n(array_specialized_p)
#define array_simple_vector_p _n(array_simple_vector_p)
#define array_adjustable_p _n(array_adjustable_p)
#define array_fillpointer_p _n(array_fillpointer_p)
#define array_dimension_size _n(array_dimension_size)
#define array_total_size _n(array_total_size)
#define array_fill_size _n(array_fill_size)
#define array_type _n(array_type)
#define array_type_size _n(array_type_size)
#define array_ptrsize _n(array_ptrsize)
#define array_ptrwrite_ _n(array_ptrwrite_)
#define array_ptrread_ _n(array_ptrread_)
#define array_fill_pointer _n(array_fill_pointer)
#define array_setf_fill_pointer_ _n(array_setf_fill_pointer_)
#define array_fill_pointer_start _n(array_fill_pointer_start)
#define array_fill_pointer_end _n(array_fill_pointer_end)
#define array_fill_pointer_set _n(array_fill_pointer_set)

#ifdef LISP_64BIT
#define arraygen_alloc_Low			alloc_array8
#define arrayspec_alloc_Low			alloc_body8
#else
#define arraygen_alloc_Low			alloc_array4
#define arrayspec_alloc_Low			alloc_body4
#endif
#define arrayinfo_alloc_Low			alloc_smallsize
#define arraysize1_alloc_Low		alloc_body4

#ifdef LISP_64BIT
#define arraygen_set_Low			SetArrayA8
#define arraygen_get_Low			GetArrayA8
#define arraygen_len_Low			LenArrayA8
#define arraygen_lenr_Low			LenArrayA8r
#define arrayspec_pos_Low			PosBodyB8
#define arrayspec_ptr_Low			PtrBodyB8
#else
#define arraygen_set_Low			SetArrayA4
#define arraygen_get_Low			GetArrayA4
#define arraygen_len_Low			LenArrayA4
#define arraygen_lenr_Low			LenArrayA4r
#define arrayspec_pos_Low			PosBodyB4
#define arrayspec_ptr_Low			PtrBodyB4
#endif

#define arraysize_ptr_Low(p)		((size_t *)PtrBodyB4(p))
#define arrayinfo_struct_Low(p)		\
	((struct array_struct *)PtrBodySSa((p), ARRAY_INDEX_SIZE))
#define GetArrayInfo_Low			GetArraySS
#define SetArrayInfo_Low			SetArraySS
#define LenArrayInfo_Low			LenArraySS
#define LenArrayInfor_Low			LenArraySSr

#ifdef LISP_DEBUG
#define arraygen_set				arraygen_set_debug
#define arraygen_get				arraygen_get_debug
#define arraygen_len				arraygen_len_debug
#define arraygen_lenr				arraygen_lenr_debug
#define arrayspec_pos				arrayspec_pos_debug
#define arrayspec_ptr				arrayspec_ptr_debug
#define arraysize_ptr				arraysize_ptr_debug
#define ArrayInfoStruct				arrayinfo_struct_debug
#define GetArrayInfo				getarrayinfo_debug
#define SetArrayInfo				setarrayinfo_debug
#define LenArrayInfo				lenarrayinfo_debug
#define LenArrayInfor				lenarrayinfor_debug
#else
#define arraygen_set				arraygen_set_Low
#define arraygen_get				arraygen_get_Low
#define arraygen_len				arraygen_len_Low
#define arraygen_lenr				arraygen_lenr_Low
#define arrayspec_pos				arrayspec_pos_Low
#define arrayspec_ptr				arrayspec_ptr_Low
#define arraysize_ptr				arraysize_ptr_Low
#define ArrayInfoStruct				arrayinfo_struct_Low
#define GetArrayInfo				GetArrayInfo_Low
#define SetArrayInfo				SetArrayInfo_Low
#define LenArrayInfo				LenArrayInfo_Low
#define LenArrayInfor				LenArrayInfor_Low
#endif

/* accessor */
void arraygen_set_debug(addr pos, size_t index, addr value);
void arraygen_get_debug(addr pos, size_t index, addr *ret);
void arraygen_len_debug(addr pos, size_t *ret);
size_t arraygen_lenr_debug(addr pos);
void arrayspec_pos_debug(addr pos, addr *ret);
addr arrayspec_ptr_debug(addr po);
size_t *arraysize_ptr_debug(addr pos);
struct array_struct *arrayinfo_struct_debug(addr pos);
void getarrayinfo_debug(addr pos, size_t index, addr *ret);
void setarrayinfo_debug(addr pos, size_t index, addr value);
void lenarrayinfo_debug(addr pos, size_t *ret);
size_t lenarrayinfor_debug(addr pos);

/* memory allocate */
void arraygen_alloc(LocalRoot local, addr *ret, size_t size);
void arraygen_local(LocalRoot local, addr *ret, size_t size);
void arraygen_heap(addr *ret, size_t size);
void arrayspec_alloc(LocalRoot local, addr *ret, size_t size);
void arrayspec_local(LocalRoot local, addr *ret, size_t size);
void arrayspec_heap(addr *ret, size_t size);
void arrayinfo_alloc(LocalRoot local, addr *ret);
void arrayinfo_local(LocalRoot local, addr *ret);
void arrayinfo_heap(addr *ret);
int arraysize_alloc_(LocalRoot local, addr *ret, size_t index);
int arraysize_local_(LocalRoot local, addr *ret, size_t index);
int arraysize_heap_(addr *ret, size_t index);
int arraysize_copy_alloc_(LocalRoot local, addr *ret, addr pos, size_t size);
int arraysize_copy_local_(LocalRoot local, addr *ret, addr pos, size_t size);
int arraysize_copy_heap_(addr *ret, addr pos, size_t size);
void array_empty_alloc(LocalRoot local, addr *ret);
void array_empty_local(LocalRoot local, addr *ret);
void array_empty_heap(addr *ret);
int array_alloc_(LocalRoot local, addr *ret, size_t index, size_t size);
int array_local_(LocalRoot local, addr *ret, size_t index, size_t size);
int array_heap_(addr *ret, size_t index, size_t size);
int array_va_alloc_(LocalRoot local, addr *ret, ...);
int array_va_local_(LocalRoot local, addr *ret, ...);
int array_va_heap_(addr *ret, ...);

/* type check */
int array_system_general_p(addr pos);
int array_system_specialized_p(addr pos);
int array_system_p(addr pos);
int arrayp(addr pos);
int array_simple_p(addr pos);
int array_vector_p(addr pos);
int array_displaced_p(addr pos);
int array_size_vector_p(addr pos, size_t size);
int array_general_p(addr pos);
int array_specialized_p(addr pos);
int array_simple_vector_p(addr pos);
int array_adjustable_p(addr pos);
int array_fillpointer_p(addr pos);
size_t array_dimension_size(addr pos);
size_t array_total_size(addr pos);
size_t array_fill_size(addr pos);
enum ARRAY_TYPE array_type(addr pos);
unsigned array_type_size(addr pos);

/* memory access */
const size_t *array_ptrsize(addr pos);
int array_ptrwrite_(addr pos, size_t index, void **ret);
int array_ptrread_(addr pos, size_t index, void *const *ret);

/* fill-pointer */
int array_fill_pointer(addr array, addr *ret);
int array_setf_fill_pointer_(addr array, addr value, int *ret);
int array_fill_pointer_start(addr array);
int array_fill_pointer_end(addr array);
int array_fill_pointer_set(addr array, size_t size);

#endif

