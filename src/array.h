#ifndef __ARRAY_HEADER__
#define __ARRAY_HEADER__

#include "array_typedef.h"
#include "local.h"
#include "typedef.h"
#include "memory.h"

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
#define RefArrayInfo_Low			RefArraySS
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
#define RefArrayInfo				refarrayinfo_debug
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
#define RefArrayInfo				RefArrayInfo_Low
#define GetArrayInfo				GetArrayInfo_Low
#define SetArrayInfo				SetArrayInfo_Low
#define LenArrayInfo				LenArrayInfo_Low
#define LenArrayInfor				LenArrayInfor_Low
#endif

/* accessor */
_g void arraygen_set_debug(addr pos, size_t index, addr value);
_g void arraygen_get_debug(addr pos, size_t index, addr *ret);
_g void arraygen_len_debug(addr pos, size_t *ret);
_g size_t arraygen_lenr_debug(addr pos);
_g void arrayspec_pos_debug(addr pos, addr *ret);
_g addr arrayspec_ptr_debug(addr po);
_g size_t *arraysize_ptr_debug(addr pos);
_g struct array_struct *arrayinfo_struct_debug(addr pos);
_g addr refarrayinfo_debug(addr pos, size_t index);
_g void getarrayinfo_debug(addr pos, size_t index, addr *ret);
_g void setarrayinfo_debug(addr pos, size_t index, addr value);
_g void lenarrayinfo_debug(addr pos, size_t *ret);
_g size_t lenarrayinfor_debug(addr pos);

/* memory allocate */
_g void arraygen_alloc(LocalRoot local, addr *ret, size_t size);
_g void arraygen_local(LocalRoot local, addr *ret, size_t size);
_g void arraygen_heap(addr *ret, size_t size);
_g void arrayspec_alloc(LocalRoot local, addr *ret, size_t size);
_g void arrayspec_local(LocalRoot local, addr *ret, size_t size);
_g void arrayspec_heap(addr *ret, size_t size);
_g void arrayinfo_alloc(LocalRoot local, addr *ret);
_g void arrayinfo_local(LocalRoot local, addr *ret);
_g void arrayinfo_heap(addr *ret);
_g void arraysize_alloc(LocalRoot local, addr *ret, size_t index);
_g void arraysize_local(LocalRoot local, addr *ret, size_t index);
_g void arraysize_heap(addr *ret, size_t index);
_g void arraysize_copy_alloc(LocalRoot local, addr *ret, addr pos, size_t size);
_g void arraysize_copy_local(LocalRoot local, addr *ret, addr pos, size_t size);
_g void arraysize_copy_heap(addr *ret, addr pos, size_t size);
_g void array_empty_alloc(LocalRoot local, addr *ret);
_g void array_empty_local(LocalRoot local, addr *ret);
_g void array_empty_heap(addr *ret);
_g void array_alloc(LocalRoot local, addr *ret, size_t index, size_t size);
_g void array_local(LocalRoot local, addr *ret, size_t index, size_t size);
_g void array_heap(addr *ret, size_t index, size_t size);
_g void array_va_alloc(LocalRoot local, addr *ret, ...);
_g void array_va_local(LocalRoot local, addr *ret, ...);
_g void array_va_heap(addr *ret, ...);

/* type check */
_g int arrayp(addr pos);
_g int array_simple_p(addr pos);
_g int array_vector_p(addr pos);
_g int array_size_vector_p(addr pos, size_t size);
_g int array_general_p(addr pos);
_g int array_specialized_p(addr pos);
_g int array_simple_vector_p(addr pos);

/* memory access */
_g const size_t *array_ptrsize(addr pos);
_g void *array_ptrwrite(addr pos, size_t index);
_g const void *array_ptrread(addr pos, size_t index);

#endif

