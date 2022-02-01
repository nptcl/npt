#include <stdio.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "compile_faslcode.h"
#include "typedef_object.h"
#include "version.h"

typedef uint32_t unicode;
typedef uint64_t size64;

int BigEndian;
int Arch64;
int Fixnum64;
FILE *Output;

/*
 *  read
 */
int read_sequence(FILE *file, void *dst, size_t size)
{
	size_t r;
	r = fread(dst, 1, size, file);
	return r != size;
}
int read_byte(FILE *file, uint8_t *ret)
{
	int c;

	c = fgetc(file);
	if (c < 0)
		return 1;
	*ret = (uint8_t)c;

	return 0;
}
int read_byte2(FILE *file, uint16_t *ret)
{
	uint8_t a, b;

	if (read_byte(file, &a))
		return 1;
	if (read_byte(file, &b))
		return 1;
	if (BigEndian)
		*ret = (a << 8U) | b;
	else
		*ret = (b << 8U) | a;

	return 0;
}
int read_byte4(FILE *file, uint32_t *ret)
{
	int i;
	uint8_t r;
	uint32_t v;

	v = 0;
	if (BigEndian) {
		for (i = 0; i < 4; i++) {
			if (read_byte(file, &r))
				return 1;
			v <<= 8UL;
			v |= r;
		}
	}
	else {
		for (i = 0; i < 4; i++) {
			if (read_byte(file, &r))
				return 1;
			r <<= i * 8UL;
			v |= r;
		}
	}
	*ret = v;

	return 0;
}
int read_byte8(FILE *file, uint64_t *ret)
{
	int i;
	uint8_t r;
	uint64_t v;

	v = 0;
	if (BigEndian) {
		for (i = 0; i < 8; i++) {
			if (read_byte(file, &r))
				return 1;
			v <<= 8ULL;
			v |= r;
		}
	}
	else {
		for (i = 0; i < 8; i++) {
			if (read_byte(file, &r))
				return 1;
			r <<= i * 8ULL;
			v |= r;
		}
	}
	*ret = v;

	return 0;
}
int read_size(FILE *file, size64 *ret)
{
	uint32_t u32;
	uint64_t u64;

	if (Arch64) {
		if (read_byte8(file, &u64))
			return 1;
		*ret = (size64)u64;
	}
	else {
		if (read_byte4(file, &u32))
			return 1;
		*ret = (size64)u32;
	}

	return 0;
}
int read_faslcode(FILE *file, enum FaslCode *ret)
{
	uint8_t v;

	if (read_byte(file, &v))
		return 1;
	*ret = (enum FaslCode)v;

	return 0;
}


/*
 *  write
 */
int write_sequence(const void *dst, size_t size)
{
	if (Output == NULL)
		return 0;
	return fwrite(dst, 1, size, Output) != size;
}
int write_byte(uint8_t v)
{
	if (Output == NULL)
		return 0;
	return fputc((int)v, Output) < 0;
}
int write_byte2(uint16_t v)
{
	uint8_t a, b;

	if (Output == NULL)
		return 0;
	a = (v >> 8UL);
	b = (v & 0xFF);
	if (BigEndian) {
		if (write_byte(a))
			return 1;
		if (write_byte(b))
			return 1;
	}
	else {
		if (write_byte(b))
			return 1;
		if (write_byte(a))
			return 1;
	}

	return 0;
}
int write_byte4(uint32_t v)
{
	int i;
	uint32_t w;

	if (Output == NULL)
		return 0;
	if (BigEndian) {
		for (i = 0; i < 4; i++) {
			w = v >> ((4UL - i - 1UL) * 8UL);
			if (write_byte(w & 0xFF))
				return 0;
		}
	}
	else {
		for (i = 0; i < 4; i++) {
			if (write_byte(v & 0xFF))
				return 0;
			v >>= 8UL;
		}
	}

	return 0;
}
int write_byte8(uint64_t v)
{
	int i;
	uint64_t w;

	if (Output == NULL)
		return 0;
	if (BigEndian) {
		for (i = 0; i < 8; i++) {
			w = v >> ((8ULL - i - 1ULL) * 8ULL);
			if (write_byte(w & 0xFF))
				return 0;
		}
	}
	else {
		for (i = 0; i < 8; i++) {
			if (write_byte(v & 0xFF))
				return 0;
			v >>= 8ULL;
		}
	}

	return 0;
}
int write_size(size64 v)
{
	if (Arch64) {
		if (write_byte8((uint64_t)v))
			return 1;
	}
	else {
		if (v >> 4) {
			fprintf(stderr, "write_size error.\n");
			return 1;
		}
		if (write_byte4((uint32_t)v))
			return 1;
	}

	return 0;
}
int write_faslcode(enum FaslCode code)
{
	return write_byte((uint8_t)code);
}
int write_times(uint8_t c, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		if (write_byte(c))
			return 1;
	}

	return 0;
}


/*
 *  pipe
 */
int pipe_sequence(FILE *file, void *dst, size_t size)
{
	if (read_sequence(file, dst, size))
		return 1;
	if (write_sequence(dst, size))
		return 1;

	return 0;
}
int pipe_byte(FILE *file, uint8_t *ret)
{
	uint8_t v;

	if (ret == NULL)
		ret = &v;
	if (read_byte(file, ret))
		return 1;
	if (write_byte(*ret))
		return 1;

	return 0;
}
int pipe_size(FILE *file, size64 *ret)
{
	size64 v;

	if (ret == NULL)
		ret = &v;
	if (read_size(file, ret))
		return 1;
	if (write_size(*ret))
		return 1;

	return 0;
}
int pipe_faslcode(FILE *file, enum FaslCode *ret)
{
	enum FaslCode v;

	if (ret == NULL)
		ret = &v;
	if (read_faslcode(file, ret))
		return 1;
	if (write_faslcode(*ret))
		return 1;

	return 0;
}
int pipe_status(FILE *file)
{
	uint8_t s, u;

	if (read_byte(file, &s))
		return 1;
	if (read_byte(file, &u))
		return 1;
	if (write_byte(s))
		return 1;
	if (write_byte(u))
		return 1;

	return 0;
}


/*
 *  fasl
 */
int fasl_value(FILE *file);

/* code */
int fasl_code_operator_p(enum FaslCode code)
{
	return FaslCode_value < code && code < FaslCode_size;
}

int fasl_code_operator(FILE *file)
{
	enum FaslCode code;

	/* code */
	if (pipe_faslcode(file, &code))
		return 1;
	if (! fasl_code_operator_p(code)) {
		fprintf(stderr, "Invalid code-operator, %d.\n", (int)code);
		return 1;
	}

	/* value */
	return fasl_value(file);
}

int fasl_code_loop(FILE *file, size64 size)
{
	size64 i;

	for (i = 0; i < size; i++) {
		if (fasl_code_operator(file))
			return 1;
	}

	return 0;
}

int fasl_code(FILE *file)
{
	size64 size;
	return pipe_status(file)
		|| pipe_size(file, &size)
		|| fasl_code_loop(file, size);
}


/* type */
int fasl_type_array(FILE *file, size64 size)
{
	size64 i;

	for (i = 0; i < size; i++) {
		if (fasl_value(file))
			return 1;
	}

	return 0;
}

int fasl_type(FILE *file)
{
	size64 size;
	return pipe_status(file)
		|| pipe_byte(file, NULL)         /* LispDecl */
		|| pipe_size(file, &size)        /* size */
		|| fasl_type_array(file, size);  /* array */
}


/* clos */
int fasl_clos(FILE *file)
{
	return pipe_status(file)
		|| pipe_size(file, NULL);
}


/* cons */
int fasl_cons(FILE *file)
{
	return pipe_status(file)
		|| fasl_value(file)   /* car */
		|| fasl_value(file);  /* cdr */
}


/* array */
int fasl_array_struct(FILE *file,
		uint8_t *rtype,
		uint8_t *relement,
		size64 *rfront,
		size64 *rdim)
{
	return pipe_byte(file, NULL)      /* bit */
		|| pipe_byte(file, rtype)     /* type */
		|| pipe_byte(file, relement)  /* element */
		|| pipe_byte(file, NULL)      /* bytesize */
		|| pipe_size(file, NULL)      /* size */
		|| pipe_size(file, rfront)    /* front */
		|| pipe_size(file, rdim)      /* dimension */
		|| pipe_size(file, NULL);     /* offset */
}

int fasl_array_dimension(FILE *file, size64 dim)
{
	size64 i;

	if (dim <= 1)
		return 0;

	for (i = 0; i < dim; i++) {
		if (pipe_size(file, NULL))
			return 1;
	}

	return 0;
}

int fasl_array_t(FILE *file, size64 front)
{
	size64 i;

	for (i = 0; i < front; i++) {
		if (fasl_value(file))
			return 1;
	}

	return 1;
}

int fasl_array_bit(FILE *file, size64 front)
{
	return fasl_value(file);
}

int fasl_array_memory(FILE *file, uint8_t element, size64 front)
{
	uint8_t data[0x0100];
	size64 i;

	for (i = 0; i < front; i++) {
		if (pipe_sequence(file, data, (size_t)element))
			return 1;
	}

	return 0;
}

int fasl_array_body(FILE *file, uint8_t type, uint8_t element, size64 front)
{
	switch ((enum ARRAY_TYPE)type) {
		case ARRAY_TYPE_T:
			return fasl_array_t(file, front);

		case ARRAY_TYPE_BIT:
			return fasl_array_bit(file, front);

		case ARRAY_TYPE_CHARACTER:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return fasl_array_memory(file, element, front);

		default:
			fprintf(stderr, "Invalid array type.\n");
			return 1;
	}

	return 0;
}

int fasl_array(FILE *file)
{
	uint8_t type, element;
	size64 front, dim;

	return pipe_status(file)
		|| fasl_array_struct(file, &type, &element, &front, &dim)
		|| fasl_value(file) /* type */
		|| fasl_array_dimension(file, dim)
		|| fasl_array_body(file, type, element, front);
}


/* vector */
int fasl_vector_loop(FILE *file, size64 size)
{
	size64 i;

	for (i = 0; i < size; i++) {
		if (fasl_value(file))
			return 1;
	}

	return 0;
}

int fasl_vector(FILE *file)
{
	size64 size;

	return pipe_status(file)
		|| pipe_size(file, &size)
		|| fasl_vector_loop(file, size);
}


/* character */
int fasl_character(FILE *file)
{
	unicode value;
	return pipe_status(file)
		|| pipe_sequence(file, &value, sizeof(value));
}


/* string */
int fasl_string_loop(FILE *file, size64 size)
{
	unicode value;
	size64 i;

	for (i = 0; i < size; i++) {
		if (pipe_sequence(file, &value, sizeof(value)))
			return 1;
	}

	return 0;
}

int fasl_string(FILE *file)
{
	size64 size;
	return pipe_status(file)
		|| pipe_size(file, &size)
		|| fasl_string_loop(file, size);
}


/* hashtable */
int fasl_hashtable_struct(FILE *file, size64 *rcount)
{
	double dvalue;
	return pipe_byte(file, NULL)    /* resize_float_p */
		|| pipe_byte(file, NULL)    /* expand_p */
		|| pipe_byte(file, NULL)    /* test */
		|| pipe_size(file, rcount)  /* count */
		|| pipe_size(file, NULL)    /* size */
		|| pipe_size(file, NULL)    /* limit */
		|| pipe_size(file, NULL)    /* resize_integer */
		|| pipe_sequence(file, &dvalue, sizeof(dvalue)) /* resize_float */
		|| pipe_sequence(file, &dvalue, sizeof(dvalue)); /* threshold */
}

int fasl_hashtable_loop(FILE *file, size64 count)
{
	size64 i;

	for (i = 0; i < count; i++) {
		/* key */
		if (fasl_value(file))
			return 1;
		/* value */
		if (fasl_value(file))
			return 1;
	}

	return 0;
}

int fasl_hashtable(FILE *file)
{
	size64 count;
	return pipe_status(file)
		|| fasl_hashtable_struct(file, &count)
		|| fasl_hashtable_loop(file, count);
}


/* gensym */
int fasl_gensym(FILE *file)
{
	return pipe_status(file)
		|| pipe_size(file, NULL);
}


/* symbol */
int fasl_symbol(FILE *file)
{
	return fasl_value(file)   /* package */
		|| fasl_value(file);  /* name */
}


/* fixnum */
int fasl_fixnum(FILE *file)
{
	int32_t v32;
	int64_t v64;

	/* status */
	if (pipe_status(file))
		return 1;

	/* value */
	if (Fixnum64) {
		if (pipe_sequence(file, &v64, sizeof(v64)))
			return 1;
	}
	else {
		if (pipe_sequence(file, &v32, sizeof(v32)))
			return 1;
	}

	return 0;
}


/* bignum */
int fasl_bignum_loop(FILE *file, size64 size)
{
	int32_t v32;
	int64_t v64;
	size64 i;

	if (Fixnum64) {
		for (i = 0; i < size; i++) {
			if (pipe_sequence(file, &v64, sizeof(v64)))
				return 1;
		}
	}
	else {
		for (i = 0; i < size; i++) {
			if (pipe_sequence(file, &v32, sizeof(v32)))
				return 1;
		}
	}

	return 0;
}

int fasl_bignum(FILE *file)
{
	size64 size;
	return pipe_status(file)
		|| pipe_byte(file, NULL)
		|| pipe_size(file, &size)
		|| fasl_bignum_loop(file, size);
}


/* ratio */
int fasl_ratio(FILE *file)
{
	return pipe_status(file)
		|| pipe_byte(file, NULL)  /* sign */
		|| fasl_value(file)       /* numer */
		|| fasl_value(file);      /* denom */
}


/* single-float */
int fasl_single_float(FILE *file)
{
	float value;
	return pipe_status(file)
		|| pipe_sequence(file, &value, sizeof(value));
}


/* double-float */
int fasl_double_float(FILE *file)
{
	double value;
	return pipe_status(file)
		|| pipe_sequence(file, &value, sizeof(value));
}


/* long-float */
int fasl_long_float(FILE *file)
{
	long double value;
	return pipe_status(file)
		|| pipe_sequence(file, &value, sizeof(value));
}


/* complex */
int fasl_complex(FILE *file)
{
	return pipe_status(file)
		|| pipe_byte(file, NULL)  /* type */
		|| fasl_value(file)       /* real */
		|| fasl_value(file);      /* imag */
}


/* callname */
int fasl_callname(FILE *file)
{
	return pipe_status(file)
		|| pipe_byte(file, NULL)
		|| fasl_value(file);
}


/* index */
int fasl_index(FILE *file)
{
	return pipe_status(file)
		|| pipe_size(file, NULL);
}


/* package */
int fasl_package(FILE *file)
{
	return fasl_value(file);
}


/* random-state */
int fasl_random_state(FILE *file)
{
	uint8_t data[16];  /* 128bit */
	return pipe_status(file)
		|| pipe_sequence(file, data, 16);
}


/* pathname */
int fasl_pathname(FILE *file)
{
	return pipe_status(file)
		|| pipe_byte(file, NULL)
		|| fasl_value(file)   /* host */
		|| fasl_value(file)   /* device */
		|| fasl_value(file)   /* directory */
		|| fasl_value(file)   /* name */
		|| fasl_value(file)   /* type */
		|| fasl_value(file);  /* version */
}


/* quote */
int fasl_quote(FILE *file)
{
	return pipe_status(file)
		|| pipe_byte(file, NULL)  /* type */
		|| fasl_value(file)       /* value */
		|| fasl_value(file);      /* print */
}


/* bitvector */
int fasl_bitvector_loop(FILE *file, size64 fixed)
{
	uint32_t v32;
	uint64_t v64;
	size64 i;

	if (Fixnum64) {
		for (i = 0; i < fixed; i++) {
			if (pipe_sequence(file, &v64, sizeof(v64)))
				return 1;
		}
	}
	else {
		for (i = 0; i < fixed; i++) {
			if (pipe_sequence(file, &v32, sizeof(v32)))
				return 1;
		}
	}

	return 0;
}

int fasl_bitvector(FILE *file)
{
	size64 fixed;
	return pipe_status(file)
		|| pipe_size(file, NULL)    /* bitsize */
		|| pipe_size(file, &fixed)  /* fixedsize */
		|| fasl_bitvector_loop(file, fixed);
}


/* load-time-value */
int fasl_load(FILE *file)
{
	return pipe_status(file)
		|| pipe_size(file, NULL);
}


/* switch */
int fasl_switch(FILE *file, enum FaslCode code)
{
	switch (code) {
		case FaslCode_code:
			return fasl_code(file);

		case FaslCode_unbound:
		case FaslCode_nil:
		case FaslCode_t:
			return 0; /* do nothing */

		case FaslCode_type:
			return fasl_type(file);

		case FaslCode_clos:
			return fasl_clos(file);

		case FaslCode_cons:
			return fasl_cons(file);

		case FaslCode_array:
			return fasl_array(file);

		case FaslCode_vector2:
		case FaslCode_vector4:
		case FaslCode_vector8:
			return fasl_vector(file);

		case FaslCode_character:
			return fasl_character(file);

		case FaslCode_string:
			return fasl_string(file);

		case FaslCode_hashtable:
			return fasl_hashtable(file);

		case FaslCode_gensym:
			return fasl_gensym(file);

		case FaslCode_symbol:
			return fasl_symbol(file);

		case FaslCode_fixnum:
			return fasl_fixnum(file);

		case FaslCode_bignum:
			return fasl_bignum(file);

		case FaslCode_ratio:
			return fasl_ratio(file);

		case FaslCode_single_float:
			return fasl_single_float(file);

		case FaslCode_double_float:
			return fasl_double_float(file);

		case FaslCode_long_float:
			return fasl_long_float(file);

		case FaslCode_complex:
			return fasl_complex(file);

		case FaslCode_callname:
			return fasl_callname(file);

		case FaslCode_index:
			return fasl_index(file);

		case FaslCode_package:
			return fasl_package(file);

		case FaslCode_random_state:
			return fasl_random_state(file);

		case FaslCode_pathname:
			return fasl_pathname(file);

		case FaslCode_quote:
			return fasl_quote(file);

		case FaslCode_bitvector:
			return fasl_bitvector(file);

		case FaslCode_load:
			return fasl_load(file);

		case FaslCode_break:
			break;

		case FaslCode_error:
		default:
			fprintf(stderr, "Invalid FaslCode, %d.\n", (int)code);
			return 1;
	}

	return 0;
}

int fasl_value(FILE *file)
{
	enum FaslCode code;

	if (read_faslcode(file, &code)) {
		fprintf(stderr, "EOF error.\n");
		return 1;
	}
	if (write_faslcode(code))
		return 1;
	if (fasl_switch(file, code))
		return 1;

	return 0;
}

int fasl_execute(FILE *file)
{
	enum FaslCode code;

	for (;;) {
		if (read_faslcode(file, &code)) {
			fprintf(stderr, "EOF error.\n");
			return 1;
		}
		if (code == FaslCode_eof)
			break;
		if (write_faslcode(code))
			return 1;
		if (fasl_switch(file, code))
			return 1;
	}

	return 0;
}


/*
 *  Header
 *
 *  [0x00] Header   : 8byte  "\0\0\0\0FASL"
 *  [0x08] LispName : 8byte
 *  [0x10] Endian   : 2byte  0x0001
 *  [0x12] VersionA : 2byte
 *  [0x14] VersionB : 2byte
 *  [0x16] VersionC : 2byte
 *  [0x18] Arch     : 1byte  (0:32bit, 1:64bit)
 *  [0x19] Fixnum   : 1byte  (0:32bit, 1:64bit)
 *  [0x1A] Padding  : 6byte
 *  [0x20] body...
 */
uint8_t Head_LispName[8];
uint16_t Head_Endian;
uint16_t Head_VersionA;
uint16_t Head_VersionB;
uint16_t Head_VersionC;
uint8_t Head_Arch;
uint8_t Head_Fixnum;
int Initialize;

int read_header_magic(FILE *file)
{
	uint8_t data[8];

	if (read_sequence(file, data, 8))
		return 1;
	if (memcmp(data, "\0\0\0\0FASL", 8) != 0) {
		fprintf(stderr, "Magic number error.\n");
		return 1;
	}

	return 0;
}

int read_header_lispname(FILE *file)
{
	uint8_t data[8];

	if (read_sequence(file, data, 8))
		return 1;
	if (Initialize == 0) {
		memcpy(Head_LispName, data, 8);
		return 0;
	}
	if (memcmp(data, Head_LispName, 8) != 0) {
		fprintf(stderr, "LispName error.\n");
		return 1;
	}

	return 0;
}

int read_header_endian(FILE *file)
{
	union read_header_endian_union {
		uint16_t v;
		uint8_t b[2];
	} u;

	if (read_sequence(file, u.b, 2))
		return 1;
	if (Initialize == 0) {
		memcpy(&Head_Endian, u.b, 2);
		BigEndian = (u.b[1] != 0);
		return 0;
	}
	if (memcmp(u.b, &Head_Endian, 2) != 0) {
		fprintf(stderr, "Endian error.\n");
		return 1;
	}

	return 0;
}

int read_header_version(FILE *file, uint16_t *version)
{
	uint16_t v;

	if (read_byte2(file, &v))
		return 1;
	if (Initialize == 0) {
		*version = v;
		return 0;
	}
	if (*version != v) {
		fprintf(stderr, "Version error.\n");
		return 1;
	}

	return 0;
}

int read_header_arch(FILE *file)
{
	uint8_t v;

	if (read_byte(file, &v))
		return 1;
	if (Initialize == 0) {
		Head_Arch = v;
		Arch64 = (int)v;
		return 0;
	}
	if (Head_Arch != v) {
		fprintf(stderr, "Arch error.\n");
		return 1;
	}

	return 0;
}

int read_header_fixnum(FILE *file)
{
	uint8_t v;

	if (read_byte(file, &v))
		return 1;
	if (Initialize == 0) {
		Head_Fixnum = v;
		Fixnum64 = (int)v;
		return 0;
	}
	if (Head_Fixnum != v) {
		fprintf(stderr, "Fixnum error.\n");
		return 1;
	}

	return 0;
}

int read_header_padding(FILE *file)
{
	uint8_t data[8];
	return read_sequence(file, data, 6);
}

int write_header(void)
{
	if (Initialize)
		return 0;

	return write_sequence("\0\0\0\0FASL", 8)  /* Magic */
		|| write_sequence(Head_LispName, 8)   /* Name */
		|| write_byte2(Head_Endian)           /* Endian */
		|| write_byte2(Head_VersionA)         /* VersionA */
		|| write_byte2(Head_VersionB)         /* VersionB */
		|| write_byte2(Head_VersionC)         /* VersionC */
		|| write_byte(Head_Arch)              /* Arch */
		|| write_byte(Head_Fixnum)            /* Fixnum */
		|| write_times(0xFF, 6);              /* Padding */
}

int read_header(FILE *file)
{
	return read_header_magic(file)
		|| read_header_lispname(file)
		|| read_header_endian(file)
		|| read_header_version(file, &Head_VersionA)
		|| read_header_version(file, &Head_VersionB)
		|| read_header_version(file, &Head_VersionC)
		|| read_header_arch(file)
		|| read_header_fixnum(file)
		|| read_header_padding(file)
		|| write_header();
}

int read_footer(FILE *file)
{
	uint8_t data[8];

	/* zero */
	if (read_sequence(file, data, 8))
		return 1;
	if (memcmp(data, "\x00\x00\x00\x00\x00\x00\x00\x00", 8) != 0)
		return 1;

	/* fill */
	if (read_sequence(file, data, 8))
		return 1;
	if (memcmp(data, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", 8) != 0)
		return 1;

	return 0;
}

int read_input(FILE *file, const char *str)
{
	if (read_header(file)) {
		fprintf(stderr, "Invalid file header, %s.\n", str);
		return 1;
	}
	Initialize = 1;
	if (fasl_execute(file)) {
		fprintf(stderr, "Fasl read error, %s.\n", str);
		return 1;
	}
	if (read_footer(file)) {
		fprintf(stderr, "Invalid file footer, %s.\n", str);
		return 1;
	}

	return 0;
}


/*
 *  Main Option
 */
int Option_argc;
char **Option_argv;
int Option_f;
int Option_c;
int Option_input;
int Option_stdin;
char *Option_fp;

int main_pipe_file(const char *str)
{
	int check;
	FILE *file;

	file = fopen(str, "rb");
	if (file == NULL) {
		fprintf(stderr, "Cannot open file, %s.\n", str);
		return 1;
	}
	check = read_input(file, str);
	if (fclose(file)) {
		fprintf(stderr, "Close error, %s.\n", str);
		return 1;
	}

	return check;
}

int main_pipe(int i)
{
	char *str;

	if (Option_stdin)
		return read_input(stdin, "STDIN");

	for (; i < Option_argc; i++) {
		str = Option_argv[i];
		if (main_pipe_file(str))
			return 1;
	}

	return 0;
}

int main_output(int i)
{
	return main_pipe(i)
		|| write_faslcode(FaslCode_eof)
		|| write_times(0x00, 8)
		|| write_times(0xFF, 8);
}

int main_check_only(int i)
{
	Output = NULL;
	return main_output(i);
}

int main_option_f(int i)
{
	int check;
	FILE *file;

	file = fopen(Option_fp, "wb");
	if (file == NULL) {
		fprintf(stderr, "File write error, %s.\n", Option_fp);
		return 1;
	}
	Output = file;
	check = main_output(i);
	Output = NULL;
	if (fclose(file)) {
		fprintf(stderr, "File close error, %s.\n", Option_fp);
		return 1;
	}

	return check;
}

int main_stdout(int i)
{
	Output = stdout;
	return main_output(i);
}

int main_help(void)
{
	printf("faslcat -- concatenate fasl files for %s.\n", Lispname);
	printf("\n");
	printf("Usage: faslcat [-h] [-c] [-f OUTPUT] [--] [INPUT ...]\n");
	printf("  -h      Help\n");
	printf("  -c      Do not write output file. (check only)\n");
	printf("  -f      Output file.\n");
	printf("\n");
	printf("Example:\n");
	printf("  faslcat < input.fasl > output.fasl\n");
	printf("  faslcat -f output.fasl input.fasl\n");
	printf("  faslcat -f output.fasl x.fasl y.fasl z.fasl\n");

	return 0;
}

int main(int argc, char *argv[])
{
	int i;
	char *str;

	Initialize = 0;
	BigEndian = 0;
	Arch64 = 0;
	Fixnum64 = 0;
	Output = NULL;

	/* Option */
	Option_argc = argc;
	Option_argv = argv;
	Option_c = 0;
	Option_f = 0;
	Option_input = 0;
	Option_stdin = 0;
	Option_fp = NULL;

	/* help */
	if (argc == 2) {
		if (strcmp(argv[1], "-h") == 0)
			return main_help();
		if (strcmp(argv[1], "--help") == 0)
			return main_help();
	}

	/* arguments */
	i = 1;
loop:
	if (argc <= i)
		goto finish;
	str = argv[i];

	/* -c */
	if (strcmp(str, "-c") == 0) {
		if (Option_f) {
			fprintf(stderr, "Option -f already set.\n");
			return 1;
		}
		Option_c = 1;
		i++;
		goto loop;
	}

	/* -f */
	if (strcmp(str, "-f") == 0) {
		if (Option_c) {
			fprintf(stderr, "Option -c already set.\n");
			return 1;
		}
		if (Option_f) {
			fprintf(stderr, "Option -f already set.\n");
			return 1;
		}
		i++;
		if (argc <= i) {
			fprintf(stderr, "There is no filename after -f.\n");
			return 1;
		}
		Option_fp = argv[i];
		Option_f = 1;
		i++;
		goto loop;
	}

	/* -- */
	if (strcmp(str, "--") == 0) {
		Option_input = (i + 1);
		goto finish;
	}

	/* files */
	Option_input = i;
	goto finish;

	/* output */
finish:
	if (argc == i)
		Option_stdin = 1;

	/* stdout */
	if (Option_c)
		return main_check_only(i);
	else if (Option_f)
		return main_option_f(i);
	else
		return main_stdout(i);
}

