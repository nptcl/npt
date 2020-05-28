#include "c99.c"
#include "degrade.h"
#include <stdarg.h>
#include <string.h>

static int test_setlocale_c(void)
{
	const char *check;

	check = setlocale_c(LC_NUMERIC);
	test(1, "setlocale_c.1");
	if (check) {
		setlocale(LC_NUMERIC, check);
		test(1, "setlocale_c.2");
	}

	RETURN;
}

static void vsnprintc_call(char *buffer, int size, const char *format, ...)
{
	va_list arg;
	va_start(arg, format);
	vsnprintc(buffer, size, format, arg);
	va_end(arg);
}

static int test_vsnprintc(void)
{
	char buffer[100];

	memset(buffer, 0x7A, 100);
	vsnprintc_call(buffer, 3, "%d", 88888);
	test(buffer[0] == '8', "vsnprintc.1");
	test(buffer[1] == '8', "vsnprintc.2");
	test(buffer[2] == 0, "vsnprintc.3");
	test(buffer[3] == 0x7A, "vsnprintc.4");

	memset(buffer, 0x7A, 100);
	vsnprintc_call(buffer, 100, "%4.2f", 12.34);
	test(strcmp(buffer, "12.34") == 0, "vsnprintc.5");

	RETURN;
}

static int test_snprintc(void)
{
	char buffer[100];

	memset(buffer, 0x7A, 100);
	snprintc(buffer, 3, "%d", 99999);
	test(buffer[0] == '9', "snprintc.1");
	test(buffer[1] == '9', "snprintc.2");
	test(buffer[2] == 0, "snprintc.3");
	test(buffer[3] == 0x7A, "snprintc.4");

	memset(buffer, 0x7A, 100);
	snprintc(buffer, 100, "%4.2f", 12.34);
	test(strcmp(buffer, "12.34") == 0, "snprintc5");

	RETURN;
}

static int test_sscanc(void)
{
	int a, b;
	float c;

	test(sscanc("10 20 30 40", "%d %d", &a, &b) == 2, "sscanc.1");
	test(a == 10, "sscanc.2");
	test(b == 20, "sscanc.3");
	test(sscanc("12.34 aaaa", "%f", &c) == 1, "sscanc.4");
	test(c == 12.34f, "sscanc.5");

	RETURN;
}

int test_c99(void)
{
	TITLE;
	TestBreak(test_setlocale_c);
	TestBreak(test_vsnprintc);
	TestBreak(test_snprintc);
	TestBreak(test_sscanc);

	return 0;
}
