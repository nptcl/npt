#ifndef __ARRAY_TYPEDEF_HEADER__
#define __ARRAY_TYPEDEF_HEADER__

#include <stddef.h>
#include "define.h"
#include "typedef.h"

enum ARRAY_INDEX {
	ARRAY_INDEX_MEMORY,
	ARRAY_INDEX_TYPE,
	ARRAY_INDEX_DIMENSION,
	ARRAY_INDEX_DISPLACED,
	ARRAY_INDEX_SIZE
};

struct array_value {
	enum ARRAY_TYPE type;
	unsigned size;
	union array_value_union {
		void *voidp;
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
	enum ARRAY_TYPE type : 8;  /* max 16 (signed) */
	unsigned element : 8;
	unsigned bytesize : 8; /* 8, 16, 32, 64 */
	size_t size, front, dimension, offset;
};

typedef fixed (*bitcalc_call)(fixed, fixed);

#endif

