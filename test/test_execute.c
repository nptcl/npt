#include "execute.c"
#include "degrade.h"

static int test_begin_code(void)
{
	int finish;
	lispcode result;
	Execute ptr;

	init_execute(0);
	finish = 0;
	ptr = getexecute(0);
	begin_code(ptr, &result);
	if (result == LISPCODE_EXECUTE) {
		exit_code(ptr, LISPCODE_SUCCESS);
		finish = 1;
	}
	else {
		test(result == LISPCODE_SUCCESS, "begin_code1");
		test(GetPropertyExecute(ptr, LISPPROP_JUMP), "begin_code2");
		finish = 2;
		end_code(ptr);
	}
	test(finish == 2, "begin_code3");
	test(GetPropertyExecute(ptr, LISPPROP_JUMP) == 0, "begin_code4");
	free_execute();

	RETURN;
}

static int test_begin_code_thread(void)
{
	int finish;
	lispcode result;
	Execute ptr;

	init_execute(0);
	finish = 0;
	ptr = getexecute(0);
	begin_code_thread(&result);
	if (result == LISPCODE_EXECUTE) {
		exit_code_thread(LISPCODE_SUCCESS);
		finish = 1;
	}
	else {
		test(result == LISPCODE_SUCCESS, "begin_code_thread1");
		test(GetPropertyExecute(ptr, LISPPROP_JUMP), "begin_code_thread2");
		finish = 2;
		end_code_thread();
	}
	test(finish == 2, "begin_code_thread3");
	test(GetPropertyExecute(ptr, LISPPROP_JUMP) == 0, "begin_code_thread4");
	free_execute();

	RETURN;
}

static int call_foreach_execute_check[10];
static void call_foreach_execute(Execute ptr)
{
	int i = (int)ptr->index;
	call_foreach_execute_check[i] = i;
}

static int test_foreach_execute(void)
{
	int i;
	struct execute body[10];
	Execute array[10];

	cleartype(body);
	cleartype(array);
	for (i = 0; i < 10; i++) {
		array[i] = &body[i];
		body[i].index = (size_t)i;
		body[i].state = ThreadState_Run;
		call_foreach_execute_check[i] = 0xAA;
	}
	ExecuteArray = array;
	Size = 10;
	Position = 3;
	body[1].state = ThreadState_Empty;
	foreach_execute(call_foreach_execute);

	test(call_foreach_execute_check[0] == 0, "foreach_execute1");
	test(call_foreach_execute_check[1] == 0xAA, "foreach_execute2");
	test(call_foreach_execute_check[2] == 2, "foreach_execute3");
	ExecuteArray = NULL;

	RETURN;
}


/*
 *  main
 */
int test_execute(void)
{
	TITLE;

	TestBreak(test_begin_code);
	TestBreak(test_begin_code_thread);
	TestBreak(test_foreach_execute);

	return 0;
}


