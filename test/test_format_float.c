#include "format_float.c"
#include "stream_function.h"
#include "degrade.h"

/*
 *  fmtfloat degrade function
 */
#define FMTFLOAT_DEGRADE_SIZE       256
static size_t fmtfloat_degrade_index;
static char fmtfloat_degrade_buffer[FMTFLOAT_DEGRADE_SIZE + 1];

static void fmtfloat_degrade_clear(void)
{
	fmtfloat_degrade_index = 0;
	fmtfloat_degrade_buffer[0] = 0;
}

int fmtfloat_write_char_(addr stream, unicode c)
{
	if (stream == NULL) {
		if (FMTFLOAT_DEGRADE_SIZE <= fmtfloat_degrade_index) {
			fprintf(stderr, "buffer overflow.\n");
			exit(1);
		}
		fmtfloat_degrade_buffer[fmtfloat_degrade_index++] = c;
		fmtfloat_degrade_buffer[fmtfloat_degrade_index] = 0;
		return 0;
	}
	else {
		return write_char_stream_(stream, c);
	}
}

int fmtfloat_print_ascii_(addr stream, const char *ptr)
{
	int c;

	if (stream == NULL) {
		for (c = *(ptr++); c; c = *(ptr++)) {
			Return(fmtfloat_write_char_(NULL, (unicode)c));
		}
		return 0;
	}
	else {
		return print_ascii_stream_(stream, ptr);
	}
}


/*
 *  case-fixed
 */
static int fmtfloat_degrade_count = 1;
#define fmtfloat_degrade_output_ok
#undef fmtfloat_degrade_output_ok

#define fmtfloat_degrade_fixoutput(x, str) { \
	printf("[%d] %s: chk:%-18s !=  ret:%-18s - [%g,%d,%d,%d]\"\n", \
			fmtfloat_degrade_count,x, str, fmtfloat_degrade_buffer, value,w,d,k); \
}

static int fmtfloat_degrade_fixfloat(single_float value,
		const char *str1, const char *str2,
		int w, int d, int k)
{
	int s = (value < 0);

	/* overflowp = 1 */
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_float_(NULL, value,s,  w,d,k,   '*','='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str1) != 0) {
		fmtfloat_degrade_fixoutput("NG", str1);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	fmtfloat_degrade_fixoutput("OK", str1);
#endif

	/* overflowp = 0 */
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_float_(NULL, value,s,  w,d,k,   0,'='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str2) != 0) {
		fmtfloat_degrade_fixoutput("NG", str2);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	fmtfloat_degrade_fixoutput("OK", str2);
#endif
	fmtfloat_degrade_count++;
	return 0;
}

static int fmtfloat_degrade_fixdouble(double_float value,
		const char *str1, const char *str2,
		int w, int d, int k)
{
	int s = (value < 0);

	/* overflowp = 1 */
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_double_(NULL, value,s,  w,d,k,   '*','='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str1) != 0) {
		fmtfloat_degrade_fixoutput("NG", str1);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	fmtfloat_degrade_fixoutput("OK", str1);
#endif

	/* overflowp = 0 */
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_double_(NULL, value,s,  w,d,k,   0,'='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str2) != 0) {
		fmtfloat_degrade_fixoutput("NG", str2);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	fmtfloat_degrade_fixoutput("OK", str2);
#endif
	fmtfloat_degrade_count++;
	return 0;
}


/*
 *  case-exponent
 */
#define test_output_exponent(x, str) { \
	printf("[%d] %s: chk:%-18s !=  ret:%-18s - [%g,%d,%d,%d,%d]\"\n", \
			fmtfloat_degrade_count,x, str, fmtfloat_degrade_buffer, value,w,d,e,k); \
}

static int fmtfloat_degrade_expfloat(single_float value,
		const char *str1, const char *str2,
		int w, int d, int e, int k)
{
	int s = (value < 0);

	/* overflowp = 1 */
	fmtfloat_degrade_clear();
	if (fmtfloat_exponent_float_(NULL, value,s,  w,d,e,k,   '*','=','E'))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str1) != 0) {
		test_output_exponent("NG", str1);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	test_output_exponent("OK", str1);
#endif

	/* overflowp = 0 */
	fmtfloat_degrade_clear();
	if (fmtfloat_exponent_float_(NULL, value,s,  w,d,e,k,   0,'=','E'))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str2) != 0) {
		test_output_exponent("NG", str2);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	test_output_exponent("OK", str2);
#endif
	fmtfloat_degrade_count++;
	return 0;
}

static int fmtfloat_degrade_expdouble(double_float value,
		const char *str1, const char *str2,
		int w, int d, int e, int k)
{
	int s = (value < 0);

	/* overflowp = 1 */
	fmtfloat_degrade_clear();
	if (fmtfloat_exponent_double_(NULL, value,s,  w,d,e,k,   '*','=','E'))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str1) != 0) {
		test_output_exponent("NG", str1);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	test_output_exponent("OK", str1);
#endif

	/* overflowp = 0 */
	fmtfloat_degrade_clear();
	if (fmtfloat_exponent_double_(NULL, value,s,  w,d,e,k,   0,'=','E'))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str2) != 0) {
		test_output_exponent("NG", str2);
		printf("error index = %d\n", fmtfloat_degrade_count);
		return 1;
	}
#ifdef fmtfloat_degrade_output_ok
	test_output_exponent("OK", str2);
#endif
	fmtfloat_degrade_count++;
	return 0;
}

#define fixfloat(v,s1,s2,w,d,k) { \
	if (fmtfloat_degrade_fixfloat(v,s1,s2,w,d,k)) return 1; \
}
#define fixdouble(v,s1,s2,w,d,k) { \
	if (fmtfloat_degrade_fixdouble(v,s1,s2,w,d,k)) return 1; \
}
static int test_case_fixed(void)
{
	fmtfloat_degrade_count = 1;
#include "case_fixed.c"
	test(1, "case-fixed");
	RETURN;
}
#undef fixfloat
#undef fixdouble

#define expfloat(v,s1,s2,w,d,e,k) { \
	if (fmtfloat_degrade_expfloat(v,s1,s2,w,d,e,k)) return 1; \
}
#define expdouble(v,s1,s2,w,d,e,k) { \
	if (fmtfloat_degrade_expdouble(v,s1,s2,w,d,e,k)) return 1; \
}
static int test_case_exponent(void)
{
	fmtfloat_degrade_count = 1;
#include "case_exponent.c"
	test(1, "case-exponent");
	RETURN;
}
#undef expfloat
#undef expdouble


/*
 *  fmtfloat
 */
static int fixed_basic1(double_float v, const char *str)
{
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_double_(NULL, v,0,  -1,-1,0,   0,'='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str) != 0) {
		degrade_printf("ERROR: chk:%s != ret:%s\n", fmtfloat_degrade_buffer, str);
		return 0;
	}
	return 1;
}

#if 0
static int fixed_basic2(double_float v, const char *str)
{
	fmtfloat_degrade_clear();
	if (fmtfloat_fixed_double_(NULL, v,0,  -1,-1,0,   0,'='))
		return 1;
	if (strcmp(fmtfloat_degrade_buffer, str) != 0) {
		degrade_printf("ERROR: chk:%s != ret:%s\n", fmtfloat_degrade_buffer, str);
		return 0;
	}
	return 1;
}
#endif

static int test_fixed_basic(void)
{
	test(fixed_basic1(0, "0.0"), "fixed_basic1");
	test(fixed_basic1(1, "1.0"), "fixed_basic2");
	test(fixed_basic1(-1, "-1.0"), "fixed_basic3");
	test(fixed_basic1(10, "10.0"), "fixed_basic4");
	test(fixed_basic1(-22.3, "-22.3"), "fixed_basic5");
	test(fixed_basic1(-123456789012.3, "-123456789012.3"), "fixed_basic6");
	test(fixed_basic1(9.87654321098, "9.87654321098"), "fixed_basic7");
	test(fixed_basic1(1.23e-5, "0.0000123"), "fixed_basic8");
	RETURN;
}


/*
 *  main
 */
static int testbreak_format_float(void)
{
	TestBreak(test_case_fixed);
	TestBreak(test_case_exponent);
	TestBreak(test_fixed_basic);

	return 0;
}


/*
 *  dummy function
 */
int test_format_float(void)
{
	return testbreak_format_float();
}

