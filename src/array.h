#ifndef __ARRAY_HEADER__
#define __ARRAY_HEADER__

#include "local.h"
#include "typedef.h"
#include "memory.h"

enum ARRAY_INFO {
	ARRAY_INFO_MEMORY,
	ARRAY_INFO_TYPE,
	ARRAY_INFO_DIMENSION,
	ARRAY_INFO_DISPLACED,
	ARRAY_INFO_SIZE
};

struct array_value {
	enum ARRAY_TYPE type;
	unsigned size;
	union array_value_union {
		addr object;
		int8_t signed8;
		int16_t signed16;
		int32_t signed32;
		uint8_t unsigned8;
		uint16_t unsigned16;
		uint32_t unsigned32;
#ifdef LISP_64BIT
		int64_t signed64;
		uint64_t unsigned64;
#endif
		unsigned bit : 1;
		unicode character;
		single_float single_value;
		double_float double_value;
		long_float long_value;
	} value;
};

struct array_struct {
	unsigned simple : 1;
	unsigned adjustable : 1;
	unsigned fillpointer : 1;
	unsigned displaced : 1;
	enum ARRAY_TYPE type : 5;  /* max 16 (signed) */
	unsigned element : 8;
	unsigned bytesize : 8; /* 8, 16, 32, 64 */
	size_t size, front, dimension, offset, refer;
};

#ifdef LISP_64BIT
#define Low_arraygen_alloc			alloc_array8
#define Low_arrayspec_alloc			alloc_body8
#define Low_set_arraygen			SetArrayA8
#define Low_get_arraygen			GetArrayA8
#define Low_len_arraygen			LenArrayA8
#define Low_lenr_arraygen			LenArrayA8r
#define Low_pos_arraymemory			PosBodyB8
#define Low_ptr_arraymemory			PtrBodyB8
#else
#define Low_arraygen_alloc			alloc_array4
#define Low_arrayspec_alloc			alloc_body4
#define Low_set_arraygen			SetArrayA4
#define Low_get_arraygen			GetArrayA4
#define Low_len_arraygen			LenArrayA4
#define Low_lenr_arraygen			LenArrayA4r
#define Low_pos_arraymemory			PosBodyB4
#define Low_ptr_arraymemory			PtrBodyB4
#endif

#define Low_arraydimension_alloc	alloc_body4
#define Low_arrayinfo_alloc			alloc_smallsize
#define Low_PtrArrayDimension(p)	((size_t *)PtrBodyB4(p))
#define Low_ArrayInfoStruct(p)		\
	((struct array_struct *)PtrBodySSa((p), ARRAY_INFO_SIZE))
#define Low_RefArrayInfo			RefArraySS
#define Low_GetArrayInfo			GetArraySS
#define Low_SetArrayInfo			SetArraySS
#define Low_LenArrayInfo			LenArraySS
#define Low_LenArrayInfor			LenArraySSr

#ifdef LISP_DEBUG
#define set_arraygen				Debug_set_arraygen
#define get_arraygen				Debug_get_arraygen
#define len_arraygen				Debug_len_arraygen
#define lenr_arraygen				Debug_lenr_arraygen
#define pos_arraymemory				Debug_pos_arraymemory
#define ptr_arraymemory				Debug_ptr_arraymemory
#define PtrArrayDimension			Debug_PtrArrayDimension
#define ArrayInfoStruct				Debug_ArrayInfoStruct
#define RefArrayInfo				Debug_RefArrayInfo
#define GetArrayInfo				Debug_GetArrayInfo
#define SetArrayInfo				Debug_SetArrayInfo
#define LenArrayInfo				Debug_LenArrayInfo
#define LenArrayInfor				Debug_LenArrayInfor
#else
#define set_arraygen				Low_set_arraygen
#define get_arraygen				Low_get_arraygen
#define len_arraygen				Low_len_arraygen
#define lenr_arraygen				Low_lenr_arraygen
#define pos_arraymemory				Low_pos_arraymemory
#define ptr_arraymemory				Low_ptr_arraymemory
#define PtrArrayDimension			Low_PtrArrayDimension
#define ArrayInfoStruct				Low_ArrayInfoStruct
#define RefArrayInfo				Low_RefArrayInfo
#define GetArrayInfo				Low_GetArrayInfo
#define SetArrayInfo				Low_SetArrayInfo
#define LenArrayInfo				Low_LenArrayInfo
#define LenArrayInfor				Low_LenArrayInfor
#endif

void Debug_set_arraygen(addr pos, size_t index, addr value);
void Debug_get_arraygen(addr pos, size_t index, addr *ret);
void Debug_len_arraygen(addr pos, size_t *ret);
size_t Debug_lenr_arraygen(addr pos);
void Debug_pos_arraymemory(addr pos, addr *ret);
addr Debug_ptr_arraymemory(addr po);
size_t *Debug_PtrArrayDimension(addr pos);
struct array_struct *Debug_ArrayInfoStruct(addr pos);
addr Debug_RefArrayInfo(addr pos, size_t index);
void Debug_GetArrayInfo(addr pos, size_t index, addr *ret);
void Debug_SetArrayInfo(addr pos, size_t index, addr value);
void Debug_LenArrayInfo(addr pos, size_t *ret);
size_t Debug_LenArrayInfor(addr pos);

void array_alloc(LocalRoot local, addr *ret, size_t index, size_t size);
void array_va_alloc(LocalRoot local, addr *ret, ...);
void array_va_local(LocalRoot local, addr *ret, ...);
void array_va_heap(addr *ret, ...);

void array_make_array(LocalRoot local, addr *ret, addr dimension,
		addr type, addr initial, addr contents,
		addr adjustable, addr fillpointer, addr displaced, addr offset);
void array_contents_alloc(LocalRoot local, addr *ret, addr rank, addr contents);
void array_adjust_array(addr *ret, addr array, addr dimension,
		addr type, addr initial, addr contents,
		addr fillpointer, addr displaced, addr offset);

void array_copy_alloc(LocalRoot local, addr *ret, addr array);
void array_copy_local(LocalRoot local, addr *ret, addr array);
void array_copy_heap(addr *ret, addr array);

int arrayp(addr pos);
int array_simple_p(addr pos);
int array_vector_p(addr pos);
int array_size_vector_p(addr pos, size_t size);
int array_general_p(addr pos);
int array_specialized_p(addr pos);

const size_t *array_dimension_pointer(addr pos);
void *array_write_pointer(addr pos, size_t index);
const void *array_read_pointer(addr pos, size_t index);
void character_array_alloc(LocalRoot local, addr pos);
void allocate_array_alloc(LocalRoot local, addr pos);
void allocate_array_heap(addr pos);

/* array */
void array_element_type(addr pos, addr *ret);
size_t array_vector_length(addr pos, int fill);
void array_rowlength(addr pos, size_t *ret);
int array_dimension_equal(addr left, addr right);
int array_get_t(addr pos, size_t index, addr *ret);
int array_get_bit(addr pos, size_t index, int *ret);
int array_get_unicode(addr pos, size_t index, unicode *ret);
void array_get(LocalRoot local, addr pos, size_t index, addr *ret);
int array_set_bit(LocalRoot local, addr pos, size_t index, int value);
int array_set_character(LocalRoot local, addr pos, size_t index, unicode value);
void array_set(addr pos, size_t index, addr value);
void array_setget(addr p1, size_t s1, addr p2, size_t s2);
void array_get_inplace(addr pos, size_t index, struct array_value *value);
int array_set_inplace(addr pos, size_t index, const struct array_value *value);
void array_aref(LocalRoot local, addr pos, addr args, addr *ret);
void array_setf_aref(addr pos, addr args, addr value);
void array_aref_bit(LocalRoot local, addr pos, addr args, addr *ret);
void array_setf_aref_bit(addr pos, addr args, addr value);
void array_array_dimension(addr array, addr axis, addr *ret);
void array_array_dimensions(addr array, addr *ret);
void array_array_displacement(addr array, addr *displaced, addr *offset);
int array_array_in_bounds_p(addr array, addr rest);
void array_array_row_major_index(addr array, addr rest, addr *ret);
int array_fill_pointer(addr array, addr *ret);
int array_setf_fill_pointer(addr array, addr value);
int array_fill_pointer_start(addr array);
int array_fill_pointer_end(addr array);
int array_fill_pointer_set(addr array, size_t size);
int array_simple_vector_p(addr array);
void array_vector_pop(LocalRoot local, addr pos, addr *ret);
void array_vector_push(LocalRoot local, addr pos, addr value, addr *ret);
void array_vector_push_extend(LocalRoot local,
		addr pos, addr value, addr extension, addr *ret);

/* vector */
void vector_get(addr pos, size_t index, addr *ret);
void vector_set(addr pos, size_t index, addr value);
void vector_aref(addr pos, addr args, addr *ret);
void vector_setf_aref(addr pos, addr args, addr value);
void vector_array_dimension(addr pos, addr arg, size_t size, addr *ret);
void vector_array_dimensions(size_t size, addr *ret);
int vector_array_in_bounds_p(addr rest, size_t size);
void vector_array_row_major_index(addr rest, size_t size, addr *ret);

void signed_make_vector_uninitialize(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, int bytesize);
void float_make_vector_uninitialize(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type);
void signed_make_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, int bytesize, addr value);
void float_make_vector(LocalRoot local, addr *ret,
		size_t size, enum ARRAY_TYPE type, addr value);

void setelt_vector(addr pos, size_t index, addr value);
void vector_adjust(addr *ret, addr array, size_t size, addr value, addr check);
void vector_reverse(LocalRoot local, addr *ret, addr pos);
void vector_nreverse(addr *ret, addr pos);

/* others */
void array_value_alloc(LocalRoot local, addr *ret, const struct array_value *str);
void array_value_local(LocalRoot local, addr *ret, const struct array_value *str);
void array_value_heap(addr *ret, const struct array_value *str);

typedef fixed (*bitcalc_call)(fixed, fixed);
void array_bitcalc(addr *ret, addr pos1, addr pos2, addr opt, bitcalc_call call);
void array_bitnot(addr *ret, addr pos, addr opt);
void array_fill(addr pos, addr item, addr start, addr end);
void array_subseq(addr *ret, addr pos, addr start, addr end);
void array_reverse(LocalRoot local, addr *ret, addr pos);
void array_nreverse(addr *ret, addr pos);

/* coerce */
void array_coerce_bit_heap(addr *ret, addr pos);
void array_coerce_character_heap(addr *ret, addr pos);
void array_coerce_signed8_heap(addr *ret, addr pos);
void array_coerce_signed16_heap(addr *ret, addr pos);
void array_coerce_signed32_heap(addr *ret, addr pos);
void array_coerce_unsigned8_heap(addr *ret, addr pos);
void array_coerce_unsigned16_heap(addr *ret, addr pos);
void array_coerce_unsigned32_heap(addr *ret, addr pos);
#ifdef LISP_64BIT
void array_coerce_signed64_heap(addr *ret, addr pos);
void array_coerce_unsigned64_heap(addr *ret, addr pos);
#endif

#endif

