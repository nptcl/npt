#include "main_argv.h"
#include "main_init.h"
#include "windows_error.h"
#include "windows_main.h"
#include "windows_window.h"
#include <Windows.h>

#define WINDOWS_MAIN_WAIT 3000

static HANDLE Windows_hThread;

static DWORD WINAPI Windows_ThreadProc(LPVOID ptr)
{
	struct lispargv *args;
	args = (struct lispargv *)ptr;
	return lisp_argv_run(ptr);
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

