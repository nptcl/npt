#include "arch.c"
#include <string.h>
#include "build.h"
#include "degrade.h"

#define TESTFILE "_debug.txt"

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
	test(check == 0, "read_clang.1");
	file = fopen(TESTFILE, "rb");
	test(file, "read_clang.2");
	check = read_clang(file, mem, 6, &size);
	fclose(file);
	test(check == 0, "read_clang.3");
	test(size == 6, "read_clang.4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_clang.5");

	RETURN;
}

static int test_readforce_clang(void)
{
	int check;
	char mem[100];
	FILE *file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_clang.1");
	file = fopen(TESTFILE, "rb");
	test(file, "readforce_clang.2");
	check = readforce_clang(file, mem, 6, &size);
	fclose(file);
	test(check == 0, "readforce_clang.3");
	test(size == 6, "readforce_clang.4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_clang.5");

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
	test(check == 0, "read_posix.1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "read_posix.2");
	check = read_posix(file, mem, 6, &size);
	close(file);
	test(check == 0, "read_posix.3");
	test(size == 6, "read_posix.4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_posix.5");

	RETURN;
}

static int test_readforce_posix(void)
{
	int check, file;
	char mem[100];
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_posix.1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "readforce_posix.2");
	check = readforce_posix(file, mem, 6, &size);
	close(file);
	test(check == 0, "readforce_posix.3");
	test(size == 6, "readforce_posix.4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_posix.5");

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
	test(check == 0, "read_windows.1");
	check = test_open_windows(TESTFILE, &file);
	test(! check, "read_windows.2");
	check = read_windows(file, mem, 6, &size);
	CloseHandle(file);
	test(check == 0, "read_windows.3");
	test(size == 6, "read_windows.4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_windows.5");

	RETURN;
}

static int test_readforce_windows(void)
{
	int check;
	char mem[100];
	HANDLE file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readforce_windows.1");
	check = test_open_windows(TESTFILE, &file);
	test(! check, "readforce_windows.2");
	check = readforce_windows(file, mem, 6, &size);
	CloseHandle(file);
	test(check == 0, "readforce_windows.3");
	test(size == 6, "readforce_windows.4");
	test(memcmp(mem, "HelloH", 6) == 0, "readforce_windows.5");

	RETURN;
}
#endif


/*
 *  safe
 */
static int test_multisafe_size(void)
{
	size_t size;

	test(multisafe_size(10, 20, &size) == 0, "multisafe_size.1");
	test(size == 200, "multisafe_size.2");
	test(multisafe_size(SIZE_MAX, 10, &size), "multisafe_size.3");

	RETURN;
}

static int test_plussafe_size(void)
{
	size_t size;

	test(plussafe_size(10, 20, &size) == 0, "plussafe_size.1");
	test(size == 30, "plussafe_size.2");
	test(plussafe_size(SIZE_MAX, 10, &size), "plussafe_size.3");

	RETURN;
}


/*
 *  arch
 */
int test_arch(void)
{
	TITLE;

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
	TestBreak(test_multisafe_size);
	TestBreak(test_plussafe_size);

	return 0;
}

