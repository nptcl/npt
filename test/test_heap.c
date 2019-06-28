#include "heap.c"
#include "degrade.h"

static int test_length_space(void)
{
	byte mem[10000];
	size_t size;

	aatype(mem);
	heap_front = mem + 10000;
	SetType(mem, LISPTYPE_CONS);
	size = 0;
	test(length_space(mem, &size) == 0, "length_space1");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 12);
	SetType(mem + 12, LISPTYPE_CONS);
	test(length_space(mem, &size) != 0, "length_space2");
	test(size == 12, "length_space3");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE);
	SetSizeSpace(mem, 20);
	mem[20] = LISPTYPE_CONS;
	test(length_space(mem, &size) != 0, "length_space4");
	test(size == 20, "length_space5");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 80);
	SetType(mem + 80, LISPSYSTEM_SPACE);
	SetSizeSpace(mem + 80, 800);
	mem[80 + 800] = LISPTYPE_CONS;
	test(length_space(mem, &size) != 0, "length_space6");
	test(size == 80 + 800, "length_space7");

	RETURN;
}

static int test_check_spacememory(void)
{
	byte mem[10000];
	size_t size;

	/* space */
	aatype(mem);
	heap_front = mem + 10000;
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 80);
	size = 0;
	test(check_spacememory(mem, &size) != 0, "check_spacememory1");
	test(size == 80, "check_spacememory1a");

	/* object */
	aatype(mem);
	SetType(mem, LISPTYPE_CONS);
	SetStatus(mem, LISPSIZE_ARRAY4);
	SetCheck2(mem, LISPCHECK_ARRAY, LISPCHECK_SIZE4);
	*PtrValueL(mem) = 100;
	size = 0;
	test(check_spacememory(mem, &size) == 0, "check_spacememory2");
	test(size == 100, "check_spacememory3");

	/* reserved */
	aatype(mem);
	SetType(mem, LISPSYSTEM_RESERVED);
	SetSizeReserved(mem, 100);
	size = 0;
	test(check_spacememory(mem, &size) == 0, "check_spacememory4");
	test(size == 100, "check_spacememory5");

	RETURN;
}

static int test_searchmemory(void)
{
	byte mem[10000];
	addr pos;
	size_t size, value;

	/* space */
	aatype(mem);
	heap_front = mem + 10000;
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 80);
	size = 0;
	pos = searchmemory(mem, &size);
	test(pos == mem, "searchmemory1");
	test(size == 80, "searchmemory2");

	value = ConsLength;
	heap_front = mem + value;
	aatype(mem);
	SetType(mem, LISPTYPE_CONS);
	SetCheck2(mem, LISPCHECK_ARRAY, LISPCHECK_SIZE2);
	*PtrValue2L(mem) = ConsLength;
	pos = searchmemory(mem, &size);
	test(pos == NULL, "searchmemory3");

	heap_front = mem + 10000;
	SetType(mem + value, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem + value, 80);
	size = 0;
	pos = searchmemory(mem, &size);
	test(pos == mem + value, "searchmemory4");
	test(size == 80, "searchmemory5");

	RETURN;
}

static int test_makespace(void)
{
	byte mem[1000];
	size_t size, value;

	memset(mem, 0xBB, 1000);
	makespace(mem, 2);
	test(mem[0] == LISPSYSTEM_SPACE1, "makespace2");
	test(mem[1] == 0, "makespace3");
	test(mem[2] == 0xBB, "makespace4");

	memset(mem, 0xAA, 1000);
	size = 8UL + IdxSize - 1;
	makespace(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE1, "makespace5");
	test(mem[1] == size - 2, "makespace6");

	memset(mem, 0xAA, 1000);
	size = 8UL + IdxSize;
	makespace(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE, "makespace7");
	memcpy(&value, mem + 8, IdxSize);
	test(value == 0, "makespace8");

	memset(mem, 0xAA, 1000);
	size = 100;
	makespace(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE, "makespace9");
	memcpy(&value, mem + 8, IdxSize);
	test(value == 100 - 8 - IdxSize, "makespace10");

	RETURN;
}

static int test_makereserved(void)
{
	byte mem[1000];
	size_t size;

	memset(mem, 0xAA, 1000);
	size = 8UL + IdxSize;
	makereserved(mem, size);
	test(mem[0] == LISPSYSTEM_RESERVED, "makereserved1");
	size = 999;
	memcpy(&size, mem + 8UL, IdxSize);
	test(size == 0, "makereserved2");

	memset(mem, 0xAA, 1000);
	makereserved(mem, 100);
	test(mem[0] == LISPSYSTEM_RESERVED, "makereserved3");
	size = 0;
	GetSizeReserved(mem, &size);
	test(size == 100, "makereserved4");

	RETURN;
}

static int test_writereserved(void)
{
	byte mem[10000];
	size_t size;

	heap_front = mem + 10000;
	heap_pos = mem;

	aatype(mem);
	writereserved(mem, 80, 100);
	test(mem[0] == LISPSYSTEM_RESERVED, "writereserved1");
	size = 0;
	GetSizeReserved(mem, &size);
	test(size == 80, "writereserved2");
	test(mem[80] == LISPSYSTEM_SPACE, "writereserved3");
	size = 0;
	GetSizeSpace(mem + 80, &size);
	test(size == 100 - 80, "writereserved4");

	aatype(mem);
	writereserved(mem, 100, 100);
	test(mem[0] == LISPSYSTEM_RESERVED, "writereserved1");
	size = 0;
	GetSizeReserved(mem, &size);
	test(size == 100, "writereserved2");

	RETURN;
}

static int test_expandmemory(void)
{
	byte mem[1000];

	aatype(mem);
	heap_pos = NULL;
	heap_front = mem;
	FrontMax = mem;
	Tail = mem + 1000;
	test(expandmemory(80) == mem, "expandmemory1");
	test(heap_pos == mem + 80, "expandmemory2");
	test(heap_front == mem + 80, "expandmemory3");
	test(FrontMax == mem + 80, "expandmemor4");

	heap_front = mem;
	Tail = mem + 80;
	test(expandmemory(80) == mem, "expandmemory5");

	heap_front = mem;
	Tail = mem + 79;
	lisp_info_enable = 0; /* infoerror */
	test(expandmemory(80) == NULL, "expandmemory6");
	lisp_info_enable = 1;

	RETURN;
}

static int test_allocfront(void)
{
	byte mem[10000];
	addr pos;
	size_t size, spacesize;

	aatype(mem);
	heap_pos = heap_front = mem;
	Tail = mem + 10000;
	pos = allocfront_unlock(800);
	test(pos == mem, "allocfront1");
	test(heap_pos == mem + 800, "allocfront2");
	test(heap_front == mem + 800, "allocfront3");

	aatype(mem);
	heap_pos = mem;
	heap_front = mem + 10000;
	Tail = mem + 10000;
	SetType(mem, LISPTYPE_CONS);
	SetCheck2(mem, LISPCHECK_ARRAY, LISPCHECK_SIZE2);
	*PtrValue2L(mem) = ConsLength;
	size = ConsLength;
	SetType(mem + size, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem + size, 80);
	SetType(mem + size + 80, LISPTYPE_CONS);
	SetCheck2(mem + size + 80, LISPCHECK_ARRAY, LISPCHECK_SIZE2);
	pos = allocfront_unlock(48);
	test(pos == mem + size, "allocfront4");
	test(heap_pos == mem + size + 48, "allocfront5");
	test(mem[size + 48] == LISPSYSTEM_SPACE, "allocfront6");
	spacesize = 0;
	GetSizeSpace(mem + size + 48, &spacesize);
	test(spacesize == 80 - 48, "allocfront7");

	RETURN;
}

static int test_alloctail(void)
{
	byte mem[10000];
	addr pos, ret;

	aatype(mem);
	Align8Front(mem, &pos);
	Tail = pos + 8000;
	heap_front = pos;
	ret = alloctail_unlock(80);
	test(ret == Tail, "alloctail1");
	test(ret == pos + 8000 - 80, "alloctail2");

	Tail = pos + 8000;
	heap_front = pos;
	ret = alloctail_unlock(8000);
	test(ret == Tail, "alloctail3");
	test(ret == pos, "alloctail4");

	RETURN;
}

static int test_fillheapmemory(void)
{
	byte mem[100000];
	addr pos, base;
	struct heapcell *cell;
	size_t size;

	aatype(mem);
	make_mutexlite(&Mutex);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 100000 - 8;
	cell = (struct heapcell *)alloctail(sizeof(struct heapcell));
	base = heap_pos;
	fillheapmemory(cell, 32);
	test(heap_pos == heap_front, "fillheapmemory1");
	test(heap_pos == pos + HeapCount * 32, "fillheapmemory2");

	test(cell->point[0] == base, "fillheapmemory3");
	test(base[0] == LISPSYSTEM_RESERVED, "fillheapmemory4");
	GetSizeReserved(base, &size);
	test(size == 32, "fillheapmemory5");

	base += 32;
	test(cell->point[1] == base, "fillheapmemory3");
	test(base[0] == LISPSYSTEM_RESERVED, "fillheapmemory4");
	GetSizeReserved(base, &size);
	test(size == 32, "fillheapmemory5");

	destroy_mutexlite(&Mutex);

	RETURN;
}

static int test_fillcellunbound(void)
{
	struct heapcell cell;

	aatype(cell);
	fillcellunbound(&cell);
	test(cell.point[0] == Unbound, "fillcellunbound1");
	test(cell.point[HeapCount - 1] == Unbound, "fillcellunbound2");

	RETURN;
}

static int test_cellalloc(void)
{
	byte mem[100000];
	addr pos;
	struct heapcell *cell, *cell2;

	make_mutexlite(&Mutex);
	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 100000 - 8;

	CellRoot = NULL;
	cell = cellalloc();
	test(cell == CellRoot, "cellalloc1");
	test(cell == CellPos, "cellalloc2");
	test(cell->next == NULL, "cellalloc3");
	test(cell->chain == NULL, "cellalloc4");
	test(cell->count == 0, "cellalloc5");

	cell2 = cellalloc();
	test(cell == cell2, "cellalloc6");
	cell->count = 1;
	cell2 = cellalloc();
	test(cell != cell2, "cellalloc7");
	test(cell->next == NULL, "cellalloc8");
	test(cell->chain == cell2, "cellalloc9");
	test(cell2 == CellPos, "cellalloc10");

	destroy_mutexlite(&Mutex);

	RETURN;
}

static int test_cellexpand(void)
{
	byte mem[100000];
	addr pos;
	struct heapinfo root;
	struct heapcell cell;

	make_mutexlite(&Mutex);
	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 100000 - 8;

	CellRoot = NULL;
	cleartype(root);
	cellexpand(&root, NULL);
	test(root.root, "cellexpand1");
	test(root.root == root.front, "cellexpand2");
	root.root->count = 1;

	cellexpand(&root, &cell);
	test(root.root, "cellexpand3");
	test(root.root != root.front, "cellexpand4");
	test(root.root->next == root.front, "cellexpand5");

	destroy_mutexlite(&Mutex);

	RETURN;
}

static int test_allocheap_small(void)
{
	byte mem[100000];
	addr pos, temp1, temp2;
	struct heapinfo root[LISPCLASS_SIZE];
	size_t size;

	make_mutexlite(&Mutex);
	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 100000 - 8;

	CellRoot = NULL;
	cleartype(root);
	Info = root;
	allocheap_small(32, 0, &temp1);
	test(Info[0].root->count == 1, "allocheap_small1");
	test(Info[0].root->point[0] == temp1, "allocheap_small2");
	test(temp1[0] == LISPSYSTEM_RESERVED, "allocheap_small3");
	size = 0;
	GetSizeReserved(temp1, &size);
	test(size == 32, "allocheap_small4");

	allocheap_small(32, 0, &temp2);
	test(Info[0].root->count == 2, "allocheap_small5");
	test(Info[0].root->point[1] == temp2, "allocheap_small6");
	test(temp1 != temp2, "allocheap_small7");
	test(temp2[0] == LISPSYSTEM_RESERVED, "allocheap_small8");
	size = 0;
	GetSizeReserved(temp2, &size);
	test(size == 32, "allocheap_small9");

	Info[0].root->count = HeapCount;
	allocheap_small(32, 0, &temp1);
	test(temp1 != temp2, "allocheap_small10");
	test(Info[0].front->count == 1, "allocheap_small11");
	test(Info[0].front->point[0] == temp1, "allocheap_small12");
	test(Info[0].root->next == Info[0].front, "allocheap_small13");
	test(temp1[0] == LISPSYSTEM_RESERVED, "allocheap_small14");
	size = 0;
	GetSizeReserved(temp1, &size);
	test(size == 32, "allocheap_small15");

	allocheap_small(32, 0, &temp1);
	test(Info[0].front->count == 2, "allocheap_small16");
	test(Info[0].front->point[1] == temp1, "allocheap_small17");
	test(temp1[0] == LISPSYSTEM_RESERVED, "allocheap_small18");
	size = 0;
	GetSizeReserved(temp1, &size);
	test(size == 32, "allocheap_small15");

	destroy_mutexlite(&Mutex);

	RETURN;
}

static int test_allocheap_large(void)
{
	byte mem[100000];
	addr pos, temp1;
	struct heapinfo root[LISPCLASS_SIZE];

	make_mutexlite(&Mutex);
	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 100000 - 8;

	CellRoot = NULL;
	cleartype(root);
	Info = root;
	allocheap_large(8192, 0, &temp1);
	test(Info[0].root->count == 1, "allocheap_large1");
	test(Info[0].root->point[0] == temp1, "allocheap_large2");

	RETURN;
}

static int test_allocheap(void)
{
	byte mem[1000000];
	addr pos, temp;
	struct heapinfo root[LISPCLASS_SIZE];

	make_mutexlite(&Mutex);
	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	Tail = pos + 1000000 - 8;

	CellRoot = NULL;
	cleartype(root);
	Info = root;

	/* size2 */
	allocheap(256, LISPTYPE_CONS, &temp, 1);
	test(temp[0] == LISPTYPE_CONS, "allocheap1");
	test(*PtrValue2L(temp) == 256, "allocheap2");

	allocheap(64, LISPTYPE_SYMBOL, &temp, 1);
	test(temp[0] == LISPTYPE_SYMBOL, "allocheap3");
	test(*PtrValue2L(temp) == 64, "allocheap4");

	/* size */
	allocheap(256, LISPTYPE_CONS, &temp, 0);
	test(temp[0] == LISPTYPE_CONS, "allocheap5");
	test(*PtrValueL(temp) == 256, "allocheap6");

	allocheap(64, LISPTYPE_SYMBOL, &temp, 0);
	test(temp[0] == LISPTYPE_SYMBOL, "allocheap7");
	test(*PtrValueL(temp) == 64, "allocheap8");

	destroy_mutexlite(&Mutex);

	RETURN;
}

static int test_make_mutexheap(void)
{
	int i, a;
	struct heapinfo root[LISPCLASS_SIZE];
	mutexlite *lock;

	memset(root, 0, sizeof(root));
	Info = root;
	test(make_mutexheap() == 0, "make_mutexheap1");

	a = 0;
	for (i = 0; i < LISPCLASS_Length; i++) {
		lock = Info[i].mutex;
		if (lock == NULL) {
			a = 1; break;
		}
	}
	test(a == 0, "make_mutexheap2");

	lock = Info[3].mutex;
	lock_mutexlite(lock);
	unlock_mutexlite(lock);
	free_mutexheap();

	RETURN;
}

static int test_alloc_heap(void)
{
	addr pos;

	lisp_info_enable = 0; /* infoerror */
	test(alloc_heap(10000) == 1, "alloc_heap1");
	lisp_info_enable = 1;
	free_heap();
	test(alloc_heap(1000*1000) == 0, "alloc_heap2");
	free_heap();

	/* error */
	CellRoot = CellPos = (struct heapcell *)Unbound;
	test(alloc_heap(1000*1000) == 0, "alloc_heap3");
	heap_cons(&pos);
	free_heap();

	RETURN;
}

static int call_foreach_heap_check[0xFF];
static void call_foreach_heap(struct heapinfo *heap)
{
	int i = (int)heap->type;
	call_foreach_heap_check[i] = i;
}

static int test_foreach_heap(void)
{
	int i, check;
	struct heapinfo array[LISPCLASS_Length];

	Info = array;
	memset(array, 0, sizeof(array));
	for (i = 0; i < LISPCLASS_Length; i++)
		array[i].type = (enum LISPCLASS)i;
	memset(call_foreach_heap_check, 0, sizeof(call_foreach_heap_check));
	foreach_heap(call_foreach_heap);
	check = 1;
	for (i = 0; i < LISPCLASS_Length; i++)
		check = check && (call_foreach_heap_check[i] == i);
	test(check, "foreach_heap1");

	RETURN;
}

static int test_cellupdate_heap(void)
{
	struct heapcell cell[10];

	CellRoot = &cell[0];
	cell[0].count = 0;
	CellPos = (struct heapcell *)Unbound;
	cellupdate_heap();
	test(CellPos == &cell[0], "cellupdate_heap1");

	cell[0].next = NULL;
	cell[0].chain = &cell[1];
	cell[0].count = 10;
	cell[1].next = NULL;
	cell[1].chain = &cell[2];
	cell[1].count = 20;
	cell[2].next = NULL;
	cell[2].chain = NULL;
	cell[2].count = 30;
	CellPos = (struct heapcell *)Unbound;
	cellupdate_heap();
	test(CellPos == &cell[2], "cellupdate_heap2");

	cell[0].next = NULL;
	cell[0].chain = &cell[1];
	cell[0].count = 10;
	cell[1].next = NULL;
	cell[1].chain = &cell[2];
	cell[1].count = 0;
	cell[2].next = NULL;
	cell[2].chain = NULL;
	cell[2].count = 30;
	CellPos = (struct heapcell *)Unbound;
	cellupdate_heap();
	test(CellPos == &cell[1], "cellupdate_heap3");
	CellRoot = CellPos = NULL;

	RETURN;
}

static int test_heap_cons(void)
{
	int i;
	addr root, cons;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	test(Cons == 0, "heap_cons1a");
	heap_cons(&root);
	test(Cons == 1, "heap_cons1b");
	test(GetType(root) == LISPTYPE_CONS, "heap_cons1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_cons2");
	test(GetCheckSize2(root), "heap_cons2a");
	test(GetCheckArray(root), "heap_cons2b");
	test(*PtrValue2L(root) == ConsLength, "heap_cons2s");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "heap_cons5");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "heap_cons6");

	for (i = 0; i < LISPCLASS_ConsLength; i++)
		heap_cons(&root);
	test(Cons == 1, "heap_cons7");
	free_heap();

	RETURN;
}

static int test_heap_symbol(void)
{
	int i;
	addr root, symbol;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	test(Symbol == 0, "heap_symbol1a");
	heap_symbol(&root);
	test(Symbol == 1, "heap_symbol1b");
	test(GetType(root) == LISPTYPE_SYMBOL, "heap_symbol1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_symbol2");
	test(GetCheckSize2(root), "heap_symbol2a");
	test(GetCheckArray(root), "heap_symbol2b");
	test(*PtrValue2L(root) == SymbolLength, "heap_symbol2s");
	symbol = 0;
	symbol = PtrArrayA2(root)[0];
	test(symbol == Unbound, "heap_symbol5");
	symbol = 0;
	symbol = PtrArrayA2(root)[1];
	test(symbol == Unbound, "heap_symbol6");

	for (i = 0; i < LISPCLASS_SymbolLength; i++)
		heap_symbol(&root);
	test(Symbol == 1, "heap_symbol7");
	free_heap();

	RETURN;
}

static int test_heap_smallsize(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_smallsize(&root, LISPTYPE_VECTOR, 10, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_smallsize1");
	test(GetStatus(root) == LISPSIZE_SMALLSIZE, "heap_smallsize2");
	test(GetCheckSize2(root), "heap_smallsize2a");
	test(GetCheckArray(root), "heap_smallsize2b");
	test(GetCheckBody(root), "heap_smallsize2c");
	test(GetCheckArrayBody(root), "heap_smallsize2d");
	size = MemoryLengthSS(10, 20);
	test(*PtrValue2L(root) == size_split(size), "heap_smallsize2s");
	cons = PtrBodySS(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArraySS(root)[0];
	test(cons == Unbound, "heap_smallsize5");
	cons = 0;
	cons = PtrArraySS(root)[1];
	test(cons == Unbound, "heap_smallsize6");
	cons = 0;
	cons = PtrArraySS(root)[9];
	test(cons == Unbound, "heap_smallsize7");

	size = GetLenArraySS(root);
	test(size == 10, "heap_smallsize8");
	size = GetLenBodySS(root);
	test(size == 20, "heap_smallsize9");
	free_heap();

	RETURN;
}

static int test_heap_array2(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array2(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array2-1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_array2-2");
	test(GetCheckSize2(root), "heap_array2-2a");
	test(GetCheckArray(root), "heap_array2-2b");
	size = MemoryLengthA2(10);
	test(*PtrValue2L(root) == size_split(size), "heap_array2-2s");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "heap_array2-5");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "heap_array2-6");
	cons = 0;
	cons = PtrArrayA2(root)[9];
	test(cons == Unbound, "heap_array2-7");
	size = GetLenArrayA2(root);
	test(size == 10, "heap_array2-8");
	free_heap();

	RETURN;
}

static int test_heap_body2(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body2(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB2(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body2-1");
	test(GetStatus(root) == LISPSIZE_BODY2, "heap_body2-2");
	test(GetCheckSize2(root), "heap_body2-2a");
	test(GetCheckBody(root), "heap_body2-2b");
	size = MemoryLengthB2(20);
	test(*PtrValue2L(root) == size_split(size), "heap_body2-2s");
	size = GetLenBodyB2(root);
	test(size == 20, "heap_body2-8");
	free_heap();

	RETURN;
}

static int test_heap_arraybody(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_arraybody(&root, LISPTYPE_VECTOR, 10, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_arraybody1");
	test(GetStatus(root) == LISPSIZE_ARRAYBODY, "heap_arraybody2");
	test(GetCheckSize4(root), "heap_arraybody2a");
	test(GetCheckArray(root), "heap_arraybody2b");
	test(GetCheckBody(root), "heap_arraybody2c");
	test(GetCheckArrayBody(root), "heap_arraybody2d");
	size = MemoryLengthAB(10, 20);
	test(*PtrValueL(root) == size_split(size), "heap_arraybody2s");
	cons = PtrBodyAB(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArrayAB(root)[0];
	test(cons == Unbound, "heap_arraybody5");
	cons = 0;
	cons = PtrArrayAB(root)[1];
	test(cons == Unbound, "heap_arraybody6");
	cons = 0;
	cons = PtrArrayAB(root)[9];
	test(cons == Unbound, "heap_arraybody7");

	size = GetLenArrayAB(root);
	test(size == 10, "heap_arraybody8");
	size = GetLenBodyAB(root);
	test(size == 20, "heap_arraybody9");
	free_heap();

	RETURN;
}

static int test_heap_array4(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array4(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array4-1");
	test(GetStatus(root) == LISPSIZE_ARRAY4, "heap_array4-2");
	test(GetCheckSize4(root), "heap_array4-2a");
	test(GetCheckArray(root), "heap_array4-2b");
	size = MemoryLengthA4(10);
	test(*PtrValueL(root) == size_split(size), "heap_array4-2s");
	cons = 0;
	cons = PtrArrayA4(root)[0];
	test(cons == Unbound, "heap_array4-5");
	cons = 0;
	cons = PtrArrayA4(root)[1];
	test(cons == Unbound, "heap_array4-6");
	cons = 0;
	cons = PtrArrayA4(root)[9];
	test(cons == Unbound, "heap_array4-7");
	size = GetLenArrayA4(root);
	test(size == 10, "heap_array4-8");
	free_heap();

	RETURN;
}

static int test_heap_body4(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body4(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB4(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body4-1");
	test(GetStatus(root) == LISPSIZE_BODY4, "heap_body4-2");
	test(GetCheckSize4(root), "heap_body4-2a");
	test(GetCheckBody(root), "heap_body4-2b");
	size = MemoryLengthB4(20);
	test(*PtrValueL(root) == size_split(size), "heap_body4-2s");
	size = GetLenBodyB4(root);
	test(size == 20, "heap_body4-8");
	free_heap();

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_heap_array8(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array8(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array8-1");
	test(GetStatus(root) == LISPSIZE_ARRAY8, "heap_array8-2");
	test(GetCheckSize8(root), "heap_array8-2a");
	test(GetCheckArray(root), "heap_array8-2b");
	size = MemoryLengthA8(10);
	test(*PtrValueL(root) == size_split(size), "heap_array8-2s");
	cons = 0;
	cons = PtrArrayA8(root)[0];
	test(cons == Unbound, "heap_array8-5");
	cons = 0;
	cons = PtrArrayA8(root)[1];
	test(cons == Unbound, "heap_array8-6");
	cons = 0;
	cons = PtrArrayA8(root)[9];
	test(cons == Unbound, "heap_array8-7");
	size = GetLenArrayA8(root);
	test(size == 10, "heap_array8-8");
	free_heap();

	RETURN;
}

static int test_heap_body8(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body8(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB8(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body8-1");
	test(GetStatus(root) == LISPSIZE_BODY8, "heap_body8-2");
	test(GetCheckSize8(root), "heap_body8-2a");
	test(GetCheckBody(root), "heap_body8-2b");
	size = MemoryLengthB8(20);
	test(*PtrValueL(root) == size_split(size), "heap_body8-2s");
	size = GetLenBodyB8(root);
	test(size == 20, "heap_body8-8");
	free_heap();

	RETURN;
}
#endif

static int test_heap_array(void)
{
	addr root;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_array2");
	test(GetCheckSize2(root), "heap_array2a");
	test(GetCheckArray(root), "heap_array2b");
	size = GetLenArrayA2(root);
	test(size == 10, "heap_array8");
	free_heap();

	RETURN;
}

static int test_heap_body(void)
{
	addr root;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body(&root, LISPTYPE_VECTOR, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_body1");
	test(GetStatus(root) == LISPSIZE_BODY2, "heap_body2");
	test(GetCheckSize2(root), "heap_body2a");
	test(GetCheckBody(root), "heap_body2b");
	size = GetLenBodyB2(root);
	test(size == 20, "heap_body8");
	free_heap();

	RETURN;
}


/*
 *  main
 */
int test_heap(void)
{
	TITLE;

	TestBreak(test_length_space);
	TestBreak(test_check_spacememory);
	TestBreak(test_searchmemory);
	TestBreak(test_makespace);
	TestBreak(test_makereserved);
	TestBreak(test_writereserved);
	TestBreak(test_expandmemory);
	TestBreak(test_allocfront);
	TestBreak(test_alloctail);
	TestBreak(test_fillheapmemory);
	TestBreak(test_fillcellunbound);
	TestBreak(test_cellalloc);
	TestBreak(test_cellexpand);
	TestBreak(test_allocheap_small);
	TestBreak(test_allocheap_large);
	TestBreak(test_allocheap);
	TestBreak(test_make_mutexheap);
	TestBreak(test_alloc_heap);
	TestBreak(test_foreach_heap);
	TestBreak(test_cellupdate_heap);

	TestBreak(test_heap_cons);
	TestBreak(test_heap_symbol);
	TestBreak(test_heap_smallsize);
	TestBreak(test_heap_array2);
	TestBreak(test_heap_body2);
	TestBreak(test_heap_arraybody);
	TestBreak(test_heap_array4);
	TestBreak(test_heap_body4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_heap_array8);
	TestBreak(test_heap_body8);
#endif
	TestBreak(test_heap_array);
	TestBreak(test_heap_body);

	return 0;
}

