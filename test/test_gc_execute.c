#include "gc_execute.c"
#include "degrade.h"

#if 0

typedef byte testspace[256];

/*
 *  degrade function
 */
struct execute ***Degrade_execute_Execute();
size_t *Degrade_execute_Size();
size_t *Degrade_execute_Position();
struct heapinfo **Degrade_heap_Info();


/*
 *  setallobject
 */
static int test_setallarray(void)
{
	byte mem[10000];
	addr array[20];
	int i, check;

	for (i = 0; i < 20; i++)
		array[i] = mem + i*8;

	/* set value */
	memset(mem, 0, 10000);
	for (i = 0; i < 20; i++)
		mem[i*8] = LISPTYPE_CONS;
	setallarray(array, 10);
	check = 1;
	for (i = 0; i < 10; i++)
		check = check && GetStatusGc(array[i]);
	test(check, "setallarray1");

	check = 0;
	for (i = 10; i < 20; i++)
		check = check || GetStatusGc(array[i]);
	test(check == 0, "setallarray2");

	RETURN;
}

static void heapcell_test(struct heapcell *cell, int count, struct heapcell *next)
{
	int i;
	byte *mem;

	memset(cell, 0, sizeoft(struct heapcell));
	mem = (byte *)&cell->point[HeapCount/2];
	cell->next = next;
	cell->count = count;
	for (i = 0; i < count + 1; i++) {
		mem[i*8] = LISPTYPE_STRING;
		cell->point[i] = mem + i*8;
	}
}

static int check_heapcell_test(struct heapcell *cell, int count)
{
	int i, check;

	check = 1;
	for (i = 0; i < count; i++)
		check = check && GetStatusGc(cell->point[i]);
	check = check && !GetStatusGc(cell->point[count]);

	return check;
}

static int test_setallobject_heap(void)
{
	struct heapcell base[10];
	struct heapinfo info;

	heapcell_test(&base[0], 2, &base[1]);
	heapcell_test(&base[1], 3, &base[2]);
	heapcell_test(&base[2], 1, NULL);
	info.root = &base[0];
	setallobject_heap(&info);
	test(check_heapcell_test(&base[0], 2), "setallobject_heap1");
	test(check_heapcell_test(&base[1], 3), "setallobject_heap2");
	test(check_heapcell_test(&base[2], 1), "setallobject_heap3");

	RETURN;
}

static void localcell_test(struct localcell *cell, int count, struct localcell *next)
{
	int i;
	byte *mem;

	memset(cell, 0, sizeoft(struct localcell));
	mem = (byte *)&cell->point[LocalCount/2];
	cell->next = next;
	cell->count = count;
	for (i = 0; i < count + 1; i++) {
		mem[i*2] = LISPTYPE_STRING;
		cell->point[i] = mem + i*2;
	}
}

static int check_localcell_test(struct localcell *cell, int count)
{
	int i, check;

	check = 1;
	for (i = 0; i < count; i++)
		check = check && GetStatusGc(cell->point[i]);
	check = check && !GetStatusGc(cell->point[count]);

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
	test(check_localcell_test(&base[0], 2), "setallobject_local1");
	test(check_localcell_test(&base[1], 3), "setallobject_local2");
	test(check_localcell_test(&base[2], 1), "setallobject_local3");

	RETURN;
}

static void test_setallobject_makeheap(void)
{
	static struct heapinfo heap_buffer[LISPCLASS_Length];
	static struct heapcell cell_buffer;
	int i;
	struct heapinfo **HeapInfo, *heap, *ptr;
	struct heapcell *cell;
	addr mem;

	HeapInfo = Degrade_heap_Info();
	cleartype(heap_buffer);
	*HeapInfo = heap = heap_buffer;
	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = &heap[i];
		ptr->type = (enum LISPCLASS)i;
		ptr->front = ptr->root = NULL;
	}

	/* testdata */
	cleartype(cell_buffer);
	cell = &cell_buffer;
	ptr = &heap[17];
	ptr->root = ptr->front = cell;
	mem = (addr)&cell->point[30];
	cell->point[0] = (addr)mem;
	cell->count = 1;
	mem[0] = LISPTYPE_STRING;
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

static void test_setallobject_makelocal(void)
{
	addr mem;
	struct execute **exec, *ptr;
	LocalRoot local;
	struct localcell *cell;
	static struct localcell cellbuffer;

	exec = test_setallobject_init();
	ptr = exec[2];
	ptr->state = ThreadState_Run;
	cleartype(cellbuffer);
	local = ptr->local;
	local->cell = cell = &cellbuffer;
	cell->count = 2;
	mem = (addr)&cell->point[30];
	cell->point[0] = (addr)mem;
	mem[0] = LISPTYPE_CONS;
	cell->point[1] = (addr)(mem + 2);
	mem[2] = LISPTYPE_STRING;
}

static void test_setallobject_clear(void)
{
	*Degrade_heap_Info() = NULL;
	*Degrade_execute_Execute() = NULL;
	*Degrade_execute_Size() = 0;
	*Degrade_execute_Position() = 0;
}

static int test_setallobject(void)
{
	struct heapcell *heap;
	struct localcell *local;

	/* make testdata */
	test_setallobject_makeheap();
	test_setallobject_makelocal();
	setallobject();

	/* check */
	heap = (*Degrade_heap_Info())[17].root;
	test(GetStatusGc(heap->point[0]), "setallobject1");
	local = (*Degrade_execute_Execute())[2]->local->cell;
	test(GetStatusGc(local->point[0]), "setallobject2");

	/* free */
	test_setallobject_clear();

	RETURN;
}


/*
 *  walkthrough
 */
#define resetrecursive_make(mem, type, size) { \
	cleartype(mem); \
	SetType(mem, LISPTYPE_##type); \
	SetStatus(mem, LISPSIZE_##size); \
	SetStatusValue(mem, LISPSTATUS_GC, 1); \
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
	test(GetStatusGc(mem) == 0, "resetrecursive1");

	resetrecursive_body(mem, BODY4);
	test(GetStatusGc(mem) == 0, "resetrecursive2");

#ifdef LISP_ARCH_64BIT
	resetrecursive_body(mem, BODY8);
	test(GetStatusGc(mem) == 0, "resetrecursive3");
#endif

	resetrecursive_array(mem, ARRAY2, A2);
	test(GetStatusGc(mem) == 0, "resetrecursive4");

	resetrecursive_array(mem, ARRAY4, A4);
	test(GetStatusGc(mem) == 0, "resetrecursive5");

#ifdef LISP_ARCH_64BIT
	resetrecursive_array(mem, ARRAY8, A8);
	test(GetStatusGc(mem) == 0, "resetrecursive6");
#endif

	resetrecursive_array(mem, SMALLSIZE, SS);
	test(GetStatusGc(mem) == 0, "resetrecursive7");

	resetrecursive_array(mem, ARRAYBODY, AB);
	test(GetStatusGc(mem) == 0, "resetrecursive8");

	/* array4 */
	resetrecursive_make(mem, VECTOR, ARRAY4);
	resetrecursive_make(mem1, CONS, ARRAYBODY);
	resetrecursive_make(mem2, STRING, BODY4);
	resetrecursive_make(mem3, STRING, BODY4);
	resetrecursive_make(mem4, VECTOR, ARRAY4);
	resetrecursive_make(mem5, STRING, BODY4);
	*PtrLenArrayA4(mem) = 4;
	PtrArrayA4(mem)[0] = Unbound;
	PtrArrayA4(mem)[1] = Unbound;
	PtrArrayA4(mem)[2] = Unbound;
	PtrArrayA4(mem)[3] = Unbound;
	SetArrayA4(mem, 0, Unbound);
	SetArrayA4(mem, 1, (addr)mem1);
	SetArrayA4(mem, 2, (addr)mem2);
	SetArrayA4(mem, 3, (addr)mem4);
	*PtrLenArrayAB(mem1) = 2;
	PtrArrayAB(mem1)[0] = Unbound;
	PtrArrayAB(mem1)[1] = Unbound;
	SetArrayAB(mem1, 0, (addr)mem); /* root */
	SetArrayAB(mem1, 1, (addr)mem3);
	*PtrLenArrayA4(mem4) = 1;
	PtrArrayA4(mem4)[0] = Unbound;
	SetStatusValue(mem4, LISPSTATUS_GC, 0);
	SetArrayA4(mem4, 0, (addr)mem5);
	resetrecursive(mem);
	test(GetStatusGc(mem) == 0, "resetrecursive9");
	test(GetStatusGc(mem1) == 0, "resetrecursive10");
	test(GetStatusGc(mem2) == 0, "resetrecursive11");
	test(GetStatusGc(mem3) == 0, "resetrecursive12");
	test(GetStatusGc(mem4) == 0, "resetrecursive13");
	test(GetStatusGc(mem5) != 0, "resetrecursive14");

#ifdef LISP_ARCH_64BIT
	resetrecursive_make(mem, VECTOR, ARRAY8);
	resetrecursive_make(mem1, CONS, ARRAYBODY);
	resetrecursive_make(mem2, STRING, BODY8);
	resetrecursive_make(mem3, STRING, BODY8);
	resetrecursive_make(mem4, VECTOR, ARRAY8);
	resetrecursive_make(mem5, STRING, BODY8);
	*PtrLenArrayA8(mem) = 4;
	PtrArrayA8(mem)[0] = Unbound;
	PtrArrayA8(mem)[1] = Unbound;
	PtrArrayA8(mem)[2] = Unbound;
	PtrArrayA8(mem)[3] = Unbound;
	SetArrayA8(mem, 0, Unbound);
	SetArrayA8(mem, 1, (addr)mem1);
	SetArrayA8(mem, 2, (addr)mem2);
	SetArrayA8(mem, 3, (addr)mem4);
	*PtrLenArrayAB(mem1) = 2;
	PtrArrayAB(mem1)[0] = Unbound;
	PtrArrayAB(mem1)[1] = Unbound;
	SetArrayAB(mem1, 0, (addr)mem); /* root */
	SetArrayAB(mem1, 1, (addr)mem3);
	*PtrLenArrayA8(mem4) = 1;
	PtrArrayA8(mem4)[0] = Unbound;
	SetStatusValue(mem4, LISPSTATUS_GC, 0);
	SetArrayA8(mem4, 0, (addr)mem5);
	resetrecursive(mem);
	test(GetStatusGc(mem) == 0, "resetrecursive9a");
	test(GetStatusGc(mem1) == 0, "resetrecursive10a");
	test(GetStatusGc(mem2) == 0, "resetrecursive11a");
	test(GetStatusGc(mem3) == 0, "resetrecursive12a");
	test(GetStatusGc(mem4) == 0, "resetrecursive13a");
	test(GetStatusGc(mem5) != 0, "resetrecursive14a");
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
	test(GetStatusGc(mem) == 0, "walkthrough_heap1");
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
	test(GetStatusGc(mem) == 0, "walkthrough_local1");

	RETURN;
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
	test(GetStatusGc(mem1) == 0, "walkthrough1");
	test(GetStatusGc(mem2) == 0, "walkthrough2");
	test_setallobject_clear();

	RETURN;
}


/*
 *  replacespace
 */
static int test_replacememory_small(void)
{
	struct heapinfo root;
	struct heapcell cell1, cell2;
	testspace mem1, mem2;
	size_t size;

	/* do nothing */
	cleartype(root);
	cleartype(cell1);
	cleartype(cell2);
	root.root = root.front = &cell1;
	replacememory_small(&root);

	/* replace */
	cell1.count = 1;
	cell1.point[0] = (addr)mem1;
	cleartype(mem1);
	SetType(mem1, LISPTYPE_VECTOR);
	SetStatus(mem1, LISPSIZE_ARRAY2);
	*PtrValue2L(mem1) = 100;
	cell1.next = &cell2;
	cleartype(mem2);
	SetType(mem2, LISPTYPE_VECTOR);
	SetStatus(mem2, LISPSIZE_ARRAY4);
	*PtrValueL(mem2) = 200;
	cell2.point[0] = (addr)mem2;
	cell2.count = 1;
	replacememory_small(&root);
	test(GetType(mem1) != LISPSYSTEM_RESERVED, "replacememory_small1");
	test(GetType(mem2) != LISPSYSTEM_RESERVED, "replacememory_small2");

	SetStatusValue(mem1, LISPSTATUS_GC, 1);
	SetStatusValue(mem2, LISPSTATUS_GC, 1);
	replacememory_small(&root);
	test(GetType(mem1) == LISPSYSTEM_RESERVED, "replacememory_small3");
	test(GetType(mem2) == LISPSYSTEM_RESERVED, "replacememory_small4");
	size = 0;
	GetSizeReserved(cell1.point[0], &size);
	test(size == 100, "replacememory_small5");
	GetSizeReserved(cell2.point[0], &size);
	test(size == 200, "replacememory_small6");

	RETURN;
}

static int test_replacememory_large(void)
{
	struct heapinfo root;
	struct heapcell cell1, cell2;
	testspace mem1, mem2;
	size_t size;

	/* do nothing */
	cleartype(root);
	cleartype(cell1);
	cleartype(cell2);
	root.root = root.front = &cell1;
	replacememory_large(&root);

	/* replace */
	cell1.count = 1;
	cell1.point[0] = (addr)mem1;
	cleartype(mem1);
	SetType(mem1, LISPTYPE_VECTOR);
	SetStatus(mem1, LISPSIZE_ARRAY2);
	*PtrValue2L(mem1) = 100;

	cell1.next = &cell2;
	cleartype(mem2);
	SetType(mem2, LISPTYPE_VECTOR);
	SetStatus(mem2, LISPSIZE_ARRAY4);
	*PtrValueL(mem2) = 200;

	cell2.point[0] = (addr)mem2;
	cell2.count = 1;
	replacememory_large(&root);
	test(GetType(mem1) != LISPSYSTEM_SPACE, "replacememory_large1");
	test(GetType(mem2) != LISPSYSTEM_SPACE, "replacememory_large2");

	SetStatusValue(mem1, LISPSTATUS_GC, 1);
	SetStatusValue(mem2, LISPSTATUS_GC, 1);
	replacememory_large(&root);
	test(GetType(mem1) == LISPSYSTEM_SPACE, "replacememory_large3");
	test(GetType(mem2) == LISPSYSTEM_SPACE, "replacememory_large4");

	size = 0;
	test(cell1.point[0] == Unbound, "replacememory_large5");
	test(cell1.point[1] != Unbound, "replacememory_large5a");
	GetSizeReserved(mem1, &size);
	test(size == 100, "replacememory_large6");
	test(cell2.point[0] == Unbound, "replacememory_large7");
	test(cell2.point[1] != Unbound, "replacememory_large7a");
	GetSizeReserved(mem2, &size);
	test(size == 200, "replacememory_large8");

	RETURN;
}

#define replacetablesmall(x) replacetable((x), replacetable_small)
#define replacetablelarge(x) replacetable((x), replacetable_large)

#define lispclass64byte LISPCLASS_Size14
#define reserved64table_size (HeapCount * 64)
typedef byte reserved64memory[reserved64table_size];
static void make_reserved64table(struct heapcell *cell, addr mem)
{
	size_t i;

	for (i = 0; i < HeapCount; i++) {
		cell->point[i] = mem;
		makereserved(mem, 64);
		mem += 64;
	}
}

static void reserved64table_write(struct heapcell *cell,
		size_t index, enum LISPTYPE type, enum LISPSIZE status)
{
	addr pos;

	pos = cell->point[index];
	SetType(pos, type);
	SetStatus(pos, status);
}

static int test_replacetable_small(void)
{
	struct heapinfo root;
	struct heapcell *cell, cell1;
	reserved64memory table1;
	testspace mem1;

	cleartype(root);
	cell = replacetable(&root, NULL);
	test(cell == NULL, "replacetable_small1");

	cleartype(cell1);
	root.root = root.front = &cell1;
	cell = replacetable(&root, NULL);
	test(cell == NULL, "replacetable_small2");

	make_reserved64table(&cell1, table1);
	resetrecursive_make(mem1, VECTOR, ARRAY4);
	cell1.count = 1;
	cell1.point[0] = (addr)mem1;
	cell = replacetablesmall(&root);
	test(cell == NULL, "replacetable_small3");

	make_reserved64table(&cell1, table1);
	cell1.count = 2;
	reserved64table_write(&cell1, 0, LISPTYPE_CONS, LISPSIZE_ARRAY4);
	cell = replacetablesmall(&root);
	test(cell == NULL, "replacetable_small4");
	test(cell1.count == 1, "replacetable_small5");
	test(GetType(cell1.point[0]) == LISPTYPE_CONS, "replacetable_small6");

	make_reserved64table(&cell1, table1);
	cell1.count = 2;
	reserved64table_write(&cell1, 1, LISPTYPE_CONS, LISPSIZE_ARRAY4);
	cell = replacetablesmall(&root);
	test(cell == NULL, "replacetable_small7");
	test(cell1.count == 1, "replacetable_small8");
	test(GetType(cell1.point[0]) == LISPTYPE_CONS, "replacetable_small9");

	make_reserved64table(&cell1, table1);
	cell1.count = 3;
	reserved64table_write(&cell1, 0, LISPTYPE_CONS, LISPSIZE_ARRAY4);
	reserved64table_write(&cell1, 2, LISPTYPE_STRING, LISPSIZE_ARRAY4);
	cell = replacetablesmall(&root);
	test(cell == NULL, "replacetable_small10");
	test(cell1.count == 2, "replacetable_small11");
	test(GetType(cell1.point[0]) == LISPTYPE_CONS, "replacetable_small12");
	test(GetType(cell1.point[1]) == LISPTYPE_STRING, "replacetable_small13");
	test(GetType(cell1.point[2]) == LISPSYSTEM_RESERVED, "replacetable_small14");

	RETURN;
}

static void fillcelltable(struct heapcell *cell, addr value)
{
	size_t i;
	for (i = 0; i < HeapCount; i++)
		cell->point[i] = value;
}

static int allunboundtable(struct heapcell *cell)
{
	int check;
	size_t i;
	for (check = 1, i = 0; i < HeapCount; i++)
		check = check && cell->point[i] == Unbound;
	return check;
}

static int test_replacetable_large(void)
{
	int i, check;
	struct heapinfo root;
	struct heapcell cell1, cell2;
	addr addr1, addr123, addr999, addr111, addr222, addr333, addr444;

	addr1 = (addr)&addr1;
	addr123 = (addr)&addr123;
	addr999 = (addr)&addr999;
	addr111 = (addr)&addr111;
	addr222 = (addr)&addr222;
	addr333 = (addr)&addr333;
	addr444 = (addr)&addr444;

	cleartype(root);
	cleartype(cell1);
	cleartype(cell2);
	root.root = root.front = &cell1;
	cell1.next = &cell2;

	fillcelltable(&cell1, Unbound);
	cell1.count = 1;
	cell1.point[0] = addr1;
	test(replacetablelarge(&root) == NULL, "replacetable_large1");
	test(cell1.count == 1, "replacetable_large2");
	test(cell1.point[0] == addr1, "replacetable_large3");

	fillcelltable(&cell1, Unbound);
	cell1.count = HeapCount;
	cell1.point[10] = addr123;
	fillcelltable(&cell2, Unbound);
	cell2.count = 1;
	cell2.point[0] = addr999;
	cell1.next = &cell2;
	test(replacetablelarge(&root) == &cell2, "replacetable_large4");
	test(cell1.next == NULL, "replacetable_large5");
	test(cell1.count == 2, "replacetable_large5a");
	test(cell1.point[0] == addr123, "replacetable_large6");
	test(cell1.point[1] == addr999, "replacetable_large7");
	for (check = 1, i = 2; i < HeapCount; i++)
		check = check && cell1.point[i] == Unbound;
	test(check, "replacetable_large8");
	test(allunboundtable(&cell2), "replacetable_large8a");

	fillcelltable(&cell1, addr123);
	cell1.count = HeapCount;
	cell1.point[10] = Unbound;
	cell1.point[11] = Unbound;
	fillcelltable(&cell2, Unbound);
	cell2.count = 10;
	cell2.point[3] = addr111;
	cell2.point[4] = addr222;
	cell1.next = &cell2;
	test(replacetablelarge(&root) == &cell2, "replacetable_large9");
	test(cell1.next == NULL, "replacetable_large10");
	test(cell1.count == HeapCount, "replacetable_large11");
	test(cell1.point[10] == addr123, "replacetable_large12");
	test(cell1.point[11] == addr123, "replacetable_large13");
	test(cell1.point[HeapCount - 2] == addr111, "replacetable_large15");
	test(cell1.point[HeapCount - 1] == addr222, "replacetable_large14");
	test(allunboundtable(&cell2), "replacetable_large14a");

	fillcelltable(&cell1, addr123);
	cell1.count = HeapCount;
	cell1.point[10] = Unbound;
	cell1.point[11] = Unbound;
	fillcelltable(&cell2, Unbound);
	cell2.count = 20;
	cell2.point[3] = addr111;
	cell2.point[4] = addr222;
	cell2.point[8] = addr333;
	cell2.point[9] = addr444;
	cell1.next = &cell2;
	test(replacetablelarge(&root) == NULL, "replacetable_large15");
	test(cell1.next != NULL, "replacetable_large16");
	test(cell1.count == HeapCount, "replacetable_large17");
	test(cell1.point[10] == addr123, "replacetable_large18");
	test(cell1.point[11] == addr123, "replacetable_large19");
	test(cell1.point[HeapCount - 2] == addr111, "replacetable_large20");
	test(cell1.point[HeapCount - 1] == addr222, "replacetable_large21");
	test(cell2.next == NULL, "replacetable_large22");
	test(cell2.count == 2, "replacetable_large22");
	test(cell2.point[0] == addr333, "replacetable_large23");
	test(cell2.point[1] == addr444, "replacetable_large24");

	RETURN;
}

static int allspacetable(addr mem)
{
	size_t i, size;

	for (i = 0; i < HeapCount; i++) {
		if (GetType(mem) != LISPSYSTEM_SPACE) return 0;
		size = 0;
		GetSizeSpace(mem, &size);
		if (size != 64) return 0;
		mem += 64;
	}

	return 1;
}

static int test_deletetable_small(void)
{
	struct heapcell cell1, cell2, cell3;
	reserved64memory table1, table2, table3;

	/* null */
	deletetable_small(NULL);
	/* all reserved */
	cleartype(cell1);
	cleartype(cell2);
	cleartype(cell3);
	make_reserved64table(&cell1, table1);
	cell1.count = 10;
	deletetable_small(&cell1);
	test(cell1.count == 0, "deletetable_small1");
	test(allspacetable(table1), "deletetable_small2");

	cell1.count = HeapCount;
	cell2.count = HeapCount;
	cell3.count = 20;
	cell1.next = &cell2;
	cell2.next = &cell3;
	cell3.next = NULL;
	make_reserved64table(&cell1, table1);
	make_reserved64table(&cell2, table2);
	make_reserved64table(&cell3, table3);
	deletetable_small(&cell1);
	test(cell1.count == 0, "deletetable_small3");
	test(cell1.next == NULL, "deletetable_small4");
	test(allspacetable(table1), "deletetable_small5");
	test(cell2.count == 0, "deletetable_small6");
	test(cell2.next == NULL, "deletetable_small7");
	test(allspacetable(table2), "deletetable_small8");
	test(cell3.count == 0, "deletetable_small9");
	test(cell3.next == NULL, "deletetable_small10");
	test(allspacetable(table3), "deletetable_small11");

	RETURN;
}

static int test_deletetable_large(void)
{
	struct heapcell cell1, cell2, cell3;

	cleartype(cell1);
	cleartype(cell2);
	cleartype(cell3);
	fillcelltable(&cell1, Unbound);
	fillcelltable(&cell2, Unbound);
	fillcelltable(&cell3, Unbound);
	deletetable_large(NULL);
	fillcelltable(&cell1, Unbound);
	fillcelltable(&cell2, Unbound);
	fillcelltable(&cell3, Unbound);
	cell1.count = 100;
	deletetable_large(&cell1);
	test(cell1.count == 0, "deletetable_large1");
	test(cell1.next == NULL, "deletetable_large2");

	fillcelltable(&cell1, Unbound);
	fillcelltable(&cell2, Unbound);
	fillcelltable(&cell3, Unbound);
	cell1.count = HeapCount;
	cell2.count = HeapCount;
	cell3.count = 20;
	cell1.next = &cell2;
	cell2.next = &cell3;
	cell3.next = NULL;
	deletetable_large(&cell1);
	test(cell1.count == 0, "deletetable_large3");
	test(cell1.next == NULL, "deletetable_large4");
	test(cell2.count == 0, "deletetable_large5");
	test(cell2.next == NULL, "deletetable_large6");
	test(cell3.count == 0, "deletetable_large7");
	test(cell3.next == NULL, "deletetable_large8");

	RETURN;
}

static int test_replacespace_heap(void)
{
	struct heapinfo root;
	struct heapcell cell;
	reserved64memory table;
	testspace mem1, mem2, mem3;

	size_t size;

	/* small */
	cleartype(root);
	cleartype(cell);
	root.root = root.front = &cell;
	root.type = lispclass64byte;
	make_reserved64table(&cell, table);
	cell.count = 2;
	SetType(cell.point[0], LISPTYPE_CONS);
	SetType(cell.point[1], LISPTYPE_STRING);
	SetStatus(cell.point[0], LISPSIZE_ARRAY4);
	SetStatus(cell.point[1], LISPSIZE_ARRAY4);
	*PtrValueL(cell.point[0]) = 64;
	*PtrValueL(cell.point[1]) = 64;
	SetStatusValue(cell.point[0], LISPSTATUS_GC, 1);
	replacespace_heap(&root);
	test(cell.count == 1, "replacespace_heap1");
	test(GetType(cell.point[0]) == LISPTYPE_STRING, "replacespace_heap2");
	test(GetType(cell.point[1]) == LISPSYSTEM_RESERVED, "replacespace_heap3");
	size = 0;
	GetSizeReserved(cell.point[1], &size);
	/* test(size == 64, "replacespace_heap4"); */

	/* large */
	cleartype(root);
	cleartype(cell);
	cleartype(mem1);
	cleartype(mem2);
	cleartype(mem3);
	root.root = root.front = &cell;
	root.type = LISPCLASS_SizeM;
	fillcelltable(&cell, Unbound);
	cell.count = 3;
	cell.point[0] = (addr)mem1;
	cell.point[1] = (addr)mem2;
	cell.point[2] = (addr)mem3;
	SetType(cell.point[0], LISPTYPE_CONS);
	SetType(cell.point[1], LISPTYPE_VECTOR);
	SetType(cell.point[2], LISPTYPE_STRING);
	SetStatus(cell.point[0], LISPSIZE_ARRAY4);
	SetStatus(cell.point[1], LISPSIZE_ARRAY4);
	SetStatus(cell.point[2], LISPSIZE_BODY4);
	*PtrValueL(cell.point[0]) = 10;
	*PtrValueL(cell.point[1]) = 20;
	*PtrValueL(cell.point[2]) = 30;
	SetStatusValue(cell.point[1], LISPSTATUS_GC, 1);
	replacespace_heap(&root);

	test(cell.count == 2, "replacespace_heap5");
	test(GetType(cell.point[0]) == LISPTYPE_CONS, "replacespace_heap6");
	test(GetType(cell.point[1]) == LISPTYPE_STRING, "replacespace_heap7");
	test(cell.point[2] == Unbound, "replacespace_heap8");

	test(GetType(mem1) == LISPTYPE_CONS, "replacespace_heap9");
	test(GetType(mem2) == LISPSYSTEM_SPACE, "replacespace_heap10");
	test(GetType(mem3) == LISPTYPE_STRING, "replacespace_heap11");
	size = 0;
	GetSizeSpace(mem2, &size);
	test(size == 20, "replacespace_heap12");

	RETURN;
}

static int test_replacespace_error(void)
{
	/* error */
	struct heapinfo info;
	struct heapcell cell;

	cleartype(info);
	cleartype(cell);
	info.root = info.front = &cell;
	replacespace_heap(&info);

	test(info.root == NULL, "replacespace_error1");
	test(info.front == NULL, "replacespace_error2");

	RETURN;
}

static int test_replacespace(void)
{
	return 0; /* skip */
}


/*
 *  mergespace
 */
static int test_sizeobject(void)
{
	testspace mem;
	size_t size;

	cleartype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 20);
	test(sizeobject(mem, &size) == 0, "sizeobject1");
	test(size == 20, "sizeobject2");

	RETURN;
}

static void write64cons(addr pos)
{
	SetType(pos, LISPTYPE_CONS);
	SetStatus(pos, LISPSIZE_ARRAY2);
	*PtrValue2L(pos) = 64;
}

static int test_mergespace(void)
{
	byte mem[10000];

	/* - */
	aamemory(mem, 10000);
	heap_root = heap_front = (addr)mem;
	heap_pos = 0;
	mergespace();
	test(heap_pos == heap_root, "mergespace1");

	/* s */
	aamemory(mem, 10000);
	makespace(mem, 64);
	heap_front = heap_root + 64;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root, "mergespace2");
	test(heap_pos == heap_root, "mergespace3");

	/* o */
	aamemory(mem, 10000);
	write64cons(mem);
	heap_front = heap_root + 64;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64, "mergespace4");
	test(heap_pos == heap_front, "mergespace5");

	/* ss */
	aamemory(mem, 10000);
	write64cons(mem);
	write64cons(mem + 64);
	heap_front = heap_root + 64*2;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*2, "mergespace2a");
	test(heap_pos == heap_root + 64*2, "mergespace3a");

	/* oo */
	aamemory(mem, 10000);
	makespace(mem, 64);
	makespace(mem + 64, 64);
	heap_front = heap_root + 64*2;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root, "mergespace2b");
	test(heap_pos == heap_root, "mergespace3b");

	/* so */
	aamemory(mem, 10000);
	write64cons(mem);
	makespace(mem + 64, 64);
	heap_front = heap_root + 64*2;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64, "mergespace6");
	test(heap_pos == heap_root + 64, "mergespace7");

	/* os */
	aamemory(mem, 10000);
	makespace(mem, 64);
	write64cons(mem + 64);
	heap_front = heap_root + 64*2;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*2, "mergespace8");
	test(heap_pos == heap_root, "mergespace9");

	/* sso */
	aamemory(mem, 10000);
	write64cons(mem);
	write64cons(mem + 64);
	makespace(mem + 64*2, 64);
	heap_front = heap_root + 64*3;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*2, "mergespace10");
	test(heap_pos == heap_front, "mergespace11");

	/* oos */
	aamemory(mem, 10000);
	makespace(mem + 64*0, 64);
	makespace(mem + 64*1, 64);
	write64cons(mem + 64*2);
	heap_front = heap_root + 64*3;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*3, "mergespace12");
	test(heap_pos == heap_root, "mergespace13");

	/* ssooss */
	aamemory(mem, 10000);
	write64cons(mem + 64*0);
	write64cons(mem + 64*1);
	makespace(mem + 64*2, 64);
	makespace(mem + 64*3, 64);
	write64cons(mem + 64*4);
	write64cons(mem + 64*5);
	heap_front = heap_root + 64*6;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*6, "mergespace14");
	test(heap_pos == heap_root + 64*2, "mergespace15");

	/* oossoo */
	aamemory(mem, 10000);
	makespace(mem + 64*0, 64);
	makespace(mem + 64*1, 64);
	write64cons(mem + 64*2);
	write64cons(mem + 64*3);
	makespace(mem + 64*4, 64);
	makespace(mem + 64*5, 64);
	heap_front = heap_root + 64*6;
	heap_pos = 0;
	mergespace();
	test(heap_front == heap_root + 64*4, "mergespace16");
	test(heap_pos == heap_root, "mergespace17");

	RETURN;
}
#endif


/*
 *  gc_execute
 */
int test_gc_execute(void)
{
	TITLE;

#if 0
	heap_object = 0xFFFFFF;
	heap_count = 0xFFFF;

	/* setallobject */
	TestBreak(test_setallarray);
	TestBreak(test_setallobject_heap);
	TestBreak(test_setallobject_local);
	TestBreak(test_setallobject);
	/* walkthrough */
	TestBreak(test_resetrecursive);
	TestBreak(test_walkthrough_heap);
	TestBreak(test_walkthrough_local);
	TestBreak(test_walkthrough);
	/* replacespace */
	TestBreak(test_replacememory_small);
	TestBreak(test_replacememory_large);
	TestBreak(test_replacetable_small);
	TestBreak(test_replacetable_large);
	TestBreak(test_deletetable_small);
	TestBreak(test_deletetable_large);
	TestBreak(test_replacespace_heap);
	TestBreak(test_replacespace_error);
	TestBreak(test_replacespace);
	/* mergespace */
	TestBreak(test_sizeobject);
	TestBreak(test_mergespace);
#endif

	return 0;
}

