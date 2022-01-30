#include <stdio.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "compile_faslcode.h"

typedef uint64_t size64;

int BigEndian;
int Arch64;
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
	return fputc((int)v, Output) != 1;
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
	if (read_byte(file, ret))
		return 1;
	if (write_byte(*ret))
		return 1;

	return 0;
}
int pipe_size(FILE *file, size64 *ret)
{
	if (read_size(file, ret))
		return 1;
	if (write_size(*ret))
		return 1;

	return 0;
}
int pipe_faslcode(FILE *file, enum FaslCode *ret)
{
	if (read_faslcode(file, ret))
		return 1;
	if (write_faslcode(*ret))
		return 1;

	return 0;
}


/*
 *  fasl
 */
int fasl_execute(FILE *file);


/* code */
int fasl_code_operator_p(enum FaslCode code)
{
	return FaslCode_nop <= code && code < FaslCode_size;
}

int fasl_code_operator(FILE *file)
{
	enum FaslCode code;

	/* code */
	if (pipe_faslcode(file, &code))
		return 1;
	if (! fasl_code_operator_p(code)) {
		fprintf(stderr, "Invalid faslcode, %d.\n", (int)code);
		return 1;
	}

	/* value */
	return fasl_execute(file);
}

int fasl_code(FILE *file)
{
	size64 size, i;

	/* size */
	if (pipe_size(file, &size))
		return 1;

	/* operator */
	for (i = 0; i < size; i++) {
		if (fasl_code_operator(file))
			return 1;
	}

	return 0;
}


/* type */
int fasl_type(FILE *file)
{
	uint8_t v;
	size64 size, i;

	/* LispDecl */
	if (pipe_byte(file, &v))
		return 1;

	/* size */
	if (pipe_size(file, &size))
		return 1;

	/* array */
	for (i = 0; i < size; i++) {
		if (fasl_execute(file))
			return 1;
	}

	return 0;
}


/* index */
int fasl_index(FILE *file)
{
	size64 v;
	return pipe_size(file, &v);
}


/* cons */
int fasl_cons(FILE *file)
{
	/* car */
	if (fasl_execute(file))
		return 1;
	/* cdr */
	if (fasl_execute(file))
		return 1;

	return 0;
}


/* array */
int fasl_array(FILE *file)
{
	uint8_t v;
	size_t size;

	/* struct: bit */
	if (pipe_byte(file, &v))
		return 1;
	/* struct: type */
	if (pipe_byte(file, &v))
		return 1;
	/* struct: element */
	if (pipe_byte(file, &v))
		return 1;
	/* struct: bytesize */
	if (pipe_byte(file, &v))
		return 1;
	/* struct: size */
	if (pipe_size(file, &size))
		return 1;
	/* struct: front */
	if (pipe_size(file, &size))
		return 1;
	/* struct: dimension */
	if (pipe_size(file, &size))
		return 1;
	/* struct: offset */
	if (pipe_size(file, &size))
		return 1;

	return 0;
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
			return fasl_index(file);

		case FaslCode_cons:
			return fasl_cons(file);

		case FaslCode_array:
			return fasl_array(file);

		case FaslCode_vector2:
		case FaslCode_vector4:
		case FaslCode_vector8:
		case FaslCode_character:
		case FaslCode_string:
		case FaslCode_hashtable:
		case FaslCode_gensym:
		case FaslCode_symbol:
		case FaslCode_fixnum:
		case FaslCode_bignum:
		case FaslCode_ratio:
		case FaslCode_single_float:
		case FaslCode_double_float:
		case FaslCode_long_float:
		case FaslCode_complex:
		case FaslCode_callname:
		case FaslCode_index:
		case FaslCode_package:
		case FaslCode_random_state:
		case FaslCode_pathname:
		case FaslCode_quote:
		case FaslCode_bitvector:
		case FaslCode_load:
		case FaslCode_value:
			break;

		case FaslCode_break:
			break;

		case FaslCode_error:
		default:
			break;
	}

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
 *  [0x18] Arch     : 2byte  (0:32bit, 1:64bit)
 *  [0x1A] Padding  : 6byte
 *  [0x20] body...
 */
uint8_t LispName[8];
uint16_t Endian;
uint16_t VersionA;
uint16_t VersionB;
uint16_t VersionC;
uint16_t Arch;
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
		memcpy(LispName, data, 8);
		return 0;
	}
	if (memcmp(data, LispName, 8) != 0) {
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
		memcpy(&Endian, u.b, 2);
		BigEndian = (u.b[0] != 0);
		return 0;
	}
	if (memcmp(u.b, &Endian, 2) != 0) {
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
	uint16_t v;

	if (read_byte2(file, &v))
		return 1;
	if (Initialize == 0) {
		Arch = v;
		Arch64 = (int)Arch;
		return 0;
	}
	if (Arch != v) {
		fprintf(stderr, "Arch error.\n");
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
	uint8_t data[8];

	if (Initialize)
		return 0;

	/* Magic */
	if (write_sequence("\0\0\0\0FASL", 8))
		return 1;
	/* Name */
	if (write_sequence(LispName, 8))
		return 1;
	/* Endian */
	if (write_byte2(Endian))
		return 1;
	/* VersionA */
	if (write_byte2(VersionA))
		return 1;
	/* VersionB */
	if (write_byte2(VersionB))
		return 1;
	/* VersionC */
	if (write_byte2(VersionC))
		return 1;
	/* Arch */
	if (write_byte2(Arch))
		return 1;
	/* Padding */
	memset(data, 0, 8);
	if (write_sequence(data, 6))
		return 1;

	return 0;
}

int read_header(FILE *file)
{
	if (read_header_magic(file))
		return 1;
	if (read_header_lispname(file))
		return 1;
	if (read_header_endian(file))
		return 1;
	if (read_header_version(file, &VersionA))
		return 1;
	if (read_header_version(file, &VersionB))
		return 1;
	if (read_header_version(file, &VersionC))
		return 1;
	if (read_header_arch(file))
		return 1;
	if (read_header_padding(file))
		return 1;
	if (write_header())
		return 1;

	Initialize = 1;
	return 0;
}

int read_input(FILE *file, const char *str)
{
	if (read_header(file)) {
		fprintf(stderr, "Invalid file format, %s.", str);
		return 1;
	}
	if (fasl_execute(file)) {
		fprintf(stderr, "Fasl read error, %s.", str);
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
		fprintf(stderr, "Cannot open file, %s.", str);
		return 1;
	}
	check = read_input(file, str);
	if (fclose(file)) {
		fprintf(stderr, "Close error, %s.", str);
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
	char data[8];

	/* pipe */
	if (main_pipe(i))
		return 1;

	/* footer */
	if (write_faslcode(FaslCode_eof))
		return 1;
	memset(data, 0x00, 8);
	if (write_sequence(data, 8))
		return 1;
	memset(data, 0xFF, 8);
	if (write_sequence(data, 8))
		return 1;

	return 0;
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

int main(int argc, char *argv[])
{
	int i;
	char *str;

	fprintf(stderr, "TODO\n");
	return 1;

	Initialize = 0;
	BigEndian = 0;
	Arch64 = 0;
	Output = NULL;

	/* Option */
	Option_argc = argc;
	Option_argv = argv;
	Option_c = 0;
	Option_f = 0;
	Option_input = 0;
	Option_stdin = 0;
	Option_fp = NULL;
	i = 2;
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

