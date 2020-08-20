#include "gc_execute.c"
#include "degrade.h"

typedef byte testspace[256];

struct execute ***Degrade_execute_Execute();
size_t *Degrade_execute_Size();
size_t *Degrade_execute_Position();

/*
 *  walkthrough
 */
#define resetrecursive_make(mem, type, size) { \
	cleartype(mem); \
	SetType(mem, LISPTYPE_##type); \
	SetStatus(mem, LISPSIZE_##size); \
	SetStatusValue(mem, LISPSTATUS_GC, 0); \
}

#define resetrecursive_body(mem, type) { \
	resetrecursive_make(mem, STRING, type); \
	resetrecursive(mem); \
}

#define resetrecursive_array(mem, type, tail) { \
	resetrecursive_make(mem, VECTOR, type); \
	*PtrLenArray##tail(mem) = 0; \
	resetrecursive(mem); \
}

static int test_resetrecursive(void)
{
	testspace mem, mem1, mem2, mem3, mem4, mem5;

	/* do nothing */
	resetrecursive(Unbound);
	cleartype(mem);
	mem[0] = LISPTYPE_CONS;
	resetrecursive(mem);

	/* recursive */
	resetrecursive_body(mem, BODY2);
	test(GetStatusGc(mem) != 0, "resetrecursive.1");

	resetrecursive_body(mem, BODY4);
	test(GetStatusGc(mem) != 0, "resetrecursive.2");

#ifdef LISP_ARCH_64BIT
	resetrecursive_body(mem, BODY8);
	test(GetStatusGc(mem) != 0, "resetrecursive.3");
#endif

	resetrecursive_array(mem, ARRAY2, A2);
	test(GetStatusGc(mem) != 0, "resetrecursive.4");

	resetrecursive_array(mem, ARRAY4, A4);
	test(GetStatusGc(mem) != 0, "resetrecursive.5");

#ifdef LISP_ARCH_64BIT
	resetrecursive_array(mem, ARRAY8, A8);
	test(GetStatusGc(mem) != 0, "resetrecursive.6");
#endif

	resetrecursive_array(mem, SMALLSIZE, SS);
	test(GetStatusGc(mem) != 0, "resetrecursive.7");

	resetrecursive_array(mem, ARRAYBODY, AB);
	test(GetStatusGc(mem) != 0, "resetrecursive.8");

	/* array4 */
	resetrecursive_make(mem, VECTOR, ARRAY4);
	resetrecursive_make(mem1, CONS, ARRAYBODY);
	resetrecursive_make(mem2, STRING, BODY4);
	resetrecursive_make(mem3, STRING, BODY4);
	resetrecursive_make(mem4, VECTOR, ARRAY4);
	resetrecursive_make(mem5, STRING, BODY4);
	/* mem */
	*PtrLenArrayA4(mem) = 4;
	PtrArrayA4(mem)[0] = Unbound;
	PtrArrayA4(mem)[1] = Unbound;
	PtrArrayA4(mem)[2] = Unbound;
	PtrArrayA4(mem)[3] = Unbound;
	SetArrayA4(mem, 0, Unbound);
	SetArrayA4(mem, 1, (addr)mem1);
	SetArrayA4(mem, 2, (addr)mem2);
	SetArrayA4(mem, 3, (addr)mem4);
	/* mem1 */
	*PtrLenArrayAB(mem1) = 2;
	PtrArrayAB(mem1)[0] = Unbound;
	PtrArrayAB(mem1)[1] = Unbound;
	SetArrayAB(mem1, 0, (addr)mem); /* root */
	SetArrayAB(mem1, 1, (addr)mem3);
	/* mem4 */
	SetStatusValue(mem4, LISPSTATUS_GC, 1);
	*PtrLenArrayA4(mem4) = 1;
	PtrArrayA4(mem4)[0] = Unbound;
	SetArrayA4(mem4, 0, (addr)mem5);
	/* check */
	resetrecursive(mem);
	test(GetStatusGc(mem) != 0, "resetrecursive.9");
	test(GetStatusGc(mem1) != 0, "resetrecursive.10");
	test(GetStatusGc(mem2) != 0, "resetrecursive.11");
	test(GetStatusGc(mem3) != 0, "resetrecursive.12");
	test(GetStatusGc(mem4) != 0, "resetrecursive.13");
	test(GetStatusGc(mem5) == 0, "resetrecursive.14");

#ifdef LISP_ARCH_64BIT
	resetrecursive_make(mem, VECTOR, ARRAY8);
	resetrecursive_make(mem1, CONS, ARRAYBODY);
	resetrecursive_make(mem2, STRING, BODY8);
	resetrecursive_make(mem3, STRING, BODY8);
	resetrecursive_make(mem4, VECTOR, ARRAY8);
	resetrecursive_make(mem5, STRING, BODY8);
	/* mem */
	*PtrLenArrayA8(mem) = 4;
	PtrArrayA8(mem)[0] = Unbound;
	PtrArrayA8(mem)[1] = Unbound;
	PtrArrayA8(mem)[2] = Unbound;
	PtrArrayA8(mem)[3] = Unbound;
	SetArrayA8(mem, 0, Unbound);
	SetArrayA8(mem, 1, (addr)mem1);
	SetArrayA8(mem, 2, (addr)mem2);
	SetArrayA8(mem, 3, (addr)mem4);
	/* mem1 */
	*PtrLenArrayAB(mem1) = 2;
	PtrArrayAB(mem1)[0] = Unbound;
	PtrArrayAB(mem1)[1] = Unbound;
	SetArrayAB(mem1, 0, (addr)mem); /* root */
	SetArrayAB(mem1, 1, (addr)mem3);
	/* mem4 */
	SetStatusValue(mem4, LISPSTATUS_GC, 1);
	*PtrLenArrayA8(mem4) = 1;
	PtrArrayA8(mem4)[0] = Unbound;
	SetArrayA8(mem4, 0, (addr)mem5);
	/* check */
	resetrecursive(mem);
	test(GetStatusGc(mem) != 0, "resetrecursive.15");
	test(GetStatusGc(mem1) != 0, "resetrecursive.16");
	test(GetStatusGc(mem2) != 0, "resetrecursive.17");
	test(GetStatusGc(mem3) != 0, "resetrecursive.18");
	test(GetStatusGc(mem4) != 0, "resetrecursive.19");
	test(GetStatusGc(mem5) == 0, "resetrecursive.20");
#endif

	RETURN;
}

static int test_walkthrough_heap(void)
{
	testspace mem;
	int i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = Unbound;
	resetrecursive_make(mem, STRING, BODY4);
	lisp_root[2] = (addr)mem;
	walkthrough_heap();
	test(GetStatusGc(mem) != 0, "walkthrough_heap.1");
	cleartype(lisp_root);

	RETURN;
}

static int test_walkthrough_local(void)
{
	testspace mem;
	struct execute exec;
	struct localroot local;
	struct localcell cell;

	cleartype(exec);
	cleartype(local);
	cleartype(cell);
	resetrecursive_make(mem, STRING, BODY4);
	exec.local = &local;
	local.cell = &cell;
	cell.count = 1;
	cell.point[0] = mem;
	cell.next = NULL;
	walkthrough_local(&exec);
	test(GetStatusGc(mem) != 0, "walkthrough_local.1");

	RETURN;
}

static struct execute **test_setallobject_init(void)
{
	static struct execute *execute_pointer[10];
	static struct execute execute_buffer[10];
	static struct localroot execute_local[10];
	struct execute ***pexec, **exec, *body, *ptr;
	LocalRoot local;
	size_t i, *psize, *ppos, len;

	pexec = Degrade_execute_Execute();
	psize = Degrade_execute_Size();
	ppos = Degrade_execute_Position();
	cleartype(execute_pointer);
	cleartype(execute_buffer);
	cleartype(execute_local);

	*pexec = exec = execute_pointer;
	*exec = body = execute_buffer;
	local = execute_local;
	len = *psize = *ppos = 10;

	for (i = 0; i < len; i++) {
		exec[i] = ptr = &body[i];
		ptr->local = &local[i];
		ptr->state = ThreadState_Empty;
	}

	return exec;
}

static void test_setallobject_clear(void)
{
	*Degrade_execute_Execute() = NULL;
	*Degrade_execute_Size() = 0;
	*Degrade_execute_Position() = 0;
}

static int test_walkthrough(void)
{
	testspace mem1, mem2;
	int i;
	struct execute **exec, *ptr;
	struct localcell cell;

	/* heap */
	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = Unbound;
	resetrecursive_make(mem1, STRING, BODY4);
	lisp_root[2] = (addr)mem1;

	/* local */
	exec = test_setallobject_init();
	ptr = exec[4];
	ptr->state = ThreadState_Run;
	resetrecursive_make(mem2, CONS, ARRAY4);
	*PtrLenArrayA4(mem2) = 0;
	cleartype(cell);
	cell.point[0] = mem2;
	cell.count = 1;
	ptr->local->cell = &cell;

	/* check */
	walkthrough();
	test(GetStatusGc(mem1) != 0, "walkthrough.1");
	test(GetStatusGc(mem2) != 0, "walkthrough.2");
	test_setallobject_clear();

	RETURN;
}


/*
 *  setallobject
 */
static int test_setallarray(void)
{
	byte mem[10000];
	addr array[20], pos;
	int i, check;

	for (i = 0; i < 20; i++)
		array[i] = mem + i*8;

	/* set value */
	memset(mem, 0, 10000);
	for (i = 0; i < 20; i++) {
		pos = mem + i*8;
		SetType(pos, LISPTYPE_CONS);
		SetStatusValue(pos, LISPSTATUS_GC, 1);
	}
	setallarray(array, 10);
	check = 0;
	for (i = 0; i < 10; i++)
		check = check || GetStatusGc(array[i]);
	test(check == 0, "setallarray.1");

	check = 1;
	for (i = 10; i < 20; i++)
		check = check && GetStatusGc(array[i]);
	test(check != 0, "setallarray.2");

	RETURN;
}

static void localcell_test(struct localcell *cell, int count, struct localcell *next)
{
	int i;
	byte *mem;
	addr pos;

	memset(cell, 0, sizeoft(struct localcell));
	mem = (byte *)&cell->point[LocalCount/2];
	cell->next = next;
	cell->count = count;
	for (i = 0; i < count + 1; i++) {
		pos = mem + i*2;
		SetType(pos, LISPTYPE_STRING);
		SetStatusValue(pos, LISPSTATUS_GC, 1);
		cell->point[i] = pos;
	}
}

static int check_localcell_test(struct localcell *cell, int count)
{
	int i, check;

	check = 0;
	for (i = 0; i < count; i++)
		check = check || GetStatusGc(cell->point[i]);
	check = check || (! GetStatusGc(cell->point[count]));

	return check;
}

static int test_setallobject_local(void)
{
	struct localcell base[10];
	struct execute exec;
	struct localroot local;

	localcell_test(&base[0], 2, &base[1]);
	localcell_test(&base[1], 3, &base[2]);
	localcell_test(&base[2], 1, NULL);
	exec.local = &local;
	local.cell = &base[0];
	setallobject_local(&exec);
	test(! check_localcell_test(&base[0], 2), "setallobject_local.1");
	test(! check_localcell_test(&base[1], 3), "setallobject_local.2");
	test(! check_localcell_test(&base[2], 1), "setallobject_local.3");

	RETURN;
}


/*
 *  gc_execute
 */
int test_gc_execute(void)
{
	DegradeTitle;

	/* walkthrough */
	TestBreak(test_resetrecursive);
	TestBreak(test_walkthrough_heap);
	TestBreak(test_walkthrough_local);
	TestBreak(test_walkthrough);

	/* setallobject */
	TestBreak(test_setallarray);
	TestBreak(test_setallobject_local);

	return 0;
}

