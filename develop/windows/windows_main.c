#include "condition.h"
#include "define_setjmp.h"
#include "extern_error.h"
#include "main_argv.h"
#include "main_init.h"
#include "typedef.h"
#include "windows_error.h"
#include "windows_main.h"
#include "windows_output.h"
#include "windows_window.h"
#include <Windows.h>
#include <string.h>

#define WINDOWS_MAIN_WAIT 3000

static HANDLE Windows_hThread;

static int lisp_windows_call(struct lispargv *args)
{
	int finish;

	lisp_set_abort_setjmp_handler();
	finish = 0;
	Lisp_abort_Begin{
		lisp_argv_run(args);
		finish = 1;
	}
	Lisp_abort_End;

	return finish;
}

static void lisp_windows_output(const char *str)
{
	size_t size;
	
	size = strlen(str);
	(void)windows_output_write(str, size, &size);
	(void)windows_output_flush();
}

static void lisp_windows_loop(struct lispargv *args)
{
	char data[256], *str;
	int finish, code;
	fixnum result;
	
	finish = lisp_windows_call(args);
	code = lisp_code;
	result = lisp_result;
	if (finish == 0) {
		code = 1;
		result = 1;
	}
	lisp_windows_output("[LISP] *** LISP CLOSE ***\r\n");
	snprintf(data, 256, "[LISP] Code = %d\r\n", code);
	lisp_windows_output(data);
	if (code == 0) {
		str = "[LISP] Result = %" PRIdF "\r\n";
		snprintf(data, 256, str, result);
		lisp_windows_output(data);
	}
	lisp_windows_output("[LISP]\r\n");
	lisp_windows_output("[LISP] *** LISP RESTART ***\r\n");
}

static DWORD WINAPI Windows_ThreadProc(LPVOID ptr)
{
	struct lispargv *args;

	args = (struct lispargv *)ptr;
	for (;;) {
		lisp_windows_loop(args);
		lisp_code = 0;
		lisp_result = 0;
	}

	return 0;
}

static int windows_main_thread(struct lispargv *ptr)
{
	DWORD check;
	HANDLE hThread;

	/* thread */
	hThread = CreateThread(NULL, 0, Windows_ThreadProc,
			(LPVOID)ptr, CREATE_SUSPENDED, NULL);
	if (hThread == NULL)
		return windows_error("CreateThread error.");

	/* message loop */
	Windows_hThread = hThread;
	if (windows_window())
		lisp_code = 1;
	Windows_hThread = NULL;

	/* destroy thread */
	check = WaitForSingleObject(hThread, WINDOWS_MAIN_WAIT);
	if (check != WAIT_OBJECT_0) {
		if (TerminateThread(hThread, 1) == 0)
			goto error;
	}
	if (GetExitCodeThread(hThread, &check) == 0)
		goto error;
	(void)CloseHandle(hThread);
	if (check)
		lisp_code = 1;

	return lisp_code;

error:
	(void)CloseHandle(hThread);
	lisp_code = 1;
	return 1;
}

static int windows_main_call(void *ptr)
{
#ifndef LISP_TERME_HIDE
	if (windows_window_show_default())
		return fmte_("windows_window_show_default error.", NULL);
#endif

	return 0;
}

int windows_main(struct lispargv *ptr)
{
	if (ptr->mode_help)
		return lisp_main_help(stdout);
	if (ptr->mode_version)
		return lisp_main_version(ptr, stdout);
	if (ptr->mode_degrade) {
		windows_error("Cannot execute degrade mode.");
		goto error;
	}
	ptr->call = windows_main_call;
	ptr->call_ptr = NULL;
	lisp_argv_init(ptr);

	/* execute */
	if (windows_main_thread(ptr))
		goto error;

	return lisp_code ? 1 : lisp_result;

error:
	lisp_code = 1;
	return 1;
}

int windows_main_start(void)
{
	if (Windows_hThread == NULL)
		return windows_error("Thread handle error.");
	if (ResumeThread(Windows_hThread) < 0)
		return windows_error("ResumeThread error.");

	return 0;
}

