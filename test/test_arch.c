#include "arch.c"
#include <string.h>
#include "build.h"
#include "degrade.h"

#define TESTFILE "_debug.txt"

#if 0
static int test_nowtime_string(void)
{
	char mem[100];

	memset(mem, 'A', 100);
	nowtime_string(mem, 5);
	test(mem[3] != 'A', "nowtime_string1");
	test(mem[4] == 0, "nowtime_string2");
	test(mem[5] == 'A', "nowtime_string3");

	memset(mem, 'A', 100);
	nowtime_string(mem, 100);
	test(mem[4] == '/', "nowtime_string4");
	test(strlen(mem) == 4+1+2+1+2 +1+ 2+1+2+1+2, "nowtime_string5");

	RETURN;
}
#endif

static int test_write_file(const char *str)
{
	FILE *file;

	file = fopen(TESTFILE, "wb");
	if (file == NULL) {
		Debug("fopen error.");
		return 1;
	}
	fprintf(file, "%s", str);
	fclose(file);

	return 0;
}

static int test_read_clang(void)
{
	int check;
	char mem[100];
	FILE *file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "read_clang1");
	file = fopen(TESTFILE, "rb");
	test(file, "read_clang2");
	check = read_clang(file, mem, 6, &size);
	fclose(file);
	test(check == 0, "read_clang3");
	test(size == 6, "read_clang4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_clang5");

	RETURN;
}

static int test_readforce_clang(void)
{
	int check;
	char mem[100];
	FILE *file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_clang1");
	file = fopen(TESTFILE, "rb");
	test(file, "readforce_clang2");
	check = readforce_clang(file, mem, 6, &size);
	fclose(file);
	test(check == 0, "readforce_clang3");
	test(size == 6, "readforce_clang4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_clang5");

	RETURN;
}

#ifdef LISP_POSIX
#include <fcntl.h>

static int test_read_posix(void)
{
	int check, file;
	char mem[100];
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "read_posix1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "read_posix2");
	check = read_posix(file, mem, 6, &size);
	close(file);
	test(check == 0, "read_posix3");
	test(size == 6, "read_posix4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_posix5");

	RETURN;
}

static int test_readforce_posix(void)
{
	int check, file;
	char mem[100];
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_posix1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "readforce_posix2");
	check = readforce_posix(file, mem, 6, &size);
	close(file);
	test(check == 0, "readforce_posix3");
	test(size == 6, "readforce_posix4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_posix5");

	RETURN;
}
#endif

#ifdef LISP_WINDOWS
#include <windows.h>

static int test_open_windows(const char *name, HANDLE *ret)
{
	*ret = CreateFileA(
			name,
			GENERIC_READ,
			FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
			NULL,
			OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL,
			NULL);
	return *ret == INVALID_HANDLE_VALUE;
}

static int test_read_windows(void)
{
	int check;
	char mem[100];
	HANDLE file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "read_windows1");
	check = test_open_windows(TESTFILE, &file);
	test(! check, "read_windows2");
	check = read_windows(file, mem, 6, &size);
	CloseHandle(file);
	test(check == 0, "read_windows3");
	test(size == 6, "read_windows4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_windows5");

	RETURN;
}

static int test_readforce_windows(void)
{
	int check;
	char mem[100];
	HANDLE file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_windows1");
	check = test_open_windows(TESTFILE, &file);
	test(! check, "readforce_windows2");
	check = readforce_windows(file, mem, 6, &size);
	CloseHandle(file);
	test(check == 0, "readforce_windows3");
	test(size == 6, "readforce_windows4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_windows5");

	RETURN;
}
#endif


/*
 *  main
 */
int test_arch(void)
{
	TITLE;
	/* TestBreak(test_nowtime_string); */
	TestBreak(test_read_clang);
	TestBreak(test_readforce_clang);
#ifdef LISP_POSIX
	TestBreak(test_read_posix);
	TestBreak(test_readforce_posix);
#endif
#ifdef LISP_WINDOWS
	TestBreak(test_read_windows);
	TestBreak(test_readforce_windows);
#endif

	return 0;
}

