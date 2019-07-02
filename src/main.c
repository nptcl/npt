#include "define.h"
#include "main_argv.h"
#include "main_init.h"

static int main_execute(struct lispargv *ptr)
{
	if (ptr->mode_help)
		return lisp_main_help(stdout);
	if (ptr->mode_version)
		return lisp_main_version(ptr, stdout);
	if (ptr->mode_degrade)
		return lisp_main_degrade(ptr);
	lisp_argv_init(ptr);
	lisp_argv_run(ptr);

	return lisp_code? 1: lisp_result;
}

#ifdef LISP_WINMAIN
int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrev, LPSTR lpCmd, int nShow)
{
	int result;
	struct lispargv *ptr;

	lisp_init();
	ptr = lispargv_windows();
	if (ptr == NULL)
		return 1;
	result = main_execute(ptr);
	free_lispargv(ptr);
	lisp_free();

	return result;
}
#else
int main(int argc, char *argv[], char *env[])
{
	int result;
	struct lispargv *ptr;

	lisp_init();
	ptr = lispargv_main(argc, argv, env);
	if (ptr == NULL)
		return 1;
	result = main_execute(ptr);
	free_lispargv(ptr);
	lisp_free();

	return result;
}
#endif

