#include "arch.c"
#include <string.h>
#include "build.h"
#include "degrade.h"

#define TESTFILE "_debug.txt"

#if defined(LISP_UNIX) || defined(LISP_WINDOWS)
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
#endif

#ifdef LISP_UNIX
#include <fcntl.h>

static int test_read_unix(void)
{
	int check, file;
	char mem[100];
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "read_unix.1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "read_unix.2");
	check = read_unix(file, mem, 6, &size);
	close(file);
	test(check == 0, "read_unix.3");
	test(size == 6, "read_unix.4");
	test(memcmp(mem, "HelloH", 6) == 0, "read_unix.5");

	RETURN;
}

static int test_readf_unix(void)
{
	int check, file;
	char mem[100];
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readf_unix.1");
	file = open(TESTFILE, O_RDONLY);
	test(0 <= file, "readf_unix.2");
	check = readf_unix(file, mem, 6, &size);
	close(file);
	test(check == 0, "readf_unix.3");
	test(size == 6, "readf_unix.4");
	test(memcmp(mem, "HelloH", 6) == 0, "readf_unix.5");

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

static int test_readf_windows(void)
{
	int check;
	char mem[100];
	HANDLE file;
	size_t size;

	check = test_write_file("HelloHello");
	test(check == 0, "readf_windows.1");
	check = test_open_windows(TESTFILE, &file);
	test(! check, "readf_windows.2");
	check = readf_windows(file, mem, 6, &size);
	CloseHandle(file);
	test(check == 0, "readf_windows.3");
	test(size == 6, "readf_windows.4");
	test(memcmp(mem, "HelloH", 6) == 0, "readf_windows.5");

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
	DegradeTitle;

#ifdef LISP_UNIX
	TestBreak(test_read_unix);
	TestBreak(test_readf_unix);
#endif
#ifdef LISP_WINDOWS
	TestBreak(test_read_windows);
	TestBreak(test_readf_windows);
#endif
	TestBreak(test_multisafe_size);
	TestBreak(test_plussafe_size);

	return 0;
}

