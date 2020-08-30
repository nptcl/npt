#include <stdio.h>
#include <stdlib.h>
#include "core_store.h"
#include "code_object.h"
#include "define.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define LOAD_STORE_SIZE		3
#else
#define LOAD_STORE_SIZE		256
#endif

/*
 *  variable
 */
struct load_store_struct {
	struct load_store_struct *next;
	addr data[LOAD_STORE_SIZE];
	size_t size;
};

static struct load_store_struct *LoadStoreRoot;


/*
 *  function
 */
static struct load_store_struct *load_store_alloc(void)
{
	struct load_store_struct *ptr;

	ptr = (struct load_store_struct *)malloc(sizeoft(struct load_store_struct));
	if (ptr == NULL)
		return NULL;
	ptr->size = 0;

	return ptr;
}

_g int load_store_init(void)
{
	struct load_store_struct *ptr;

	Check(LoadStoreRoot != NULL, "load_store_init error.");
	ptr = load_store_alloc();
	if (ptr == NULL) {
		Debug("malloc error.");
		return 1;
	}
	ptr->next = NULL;
	LoadStoreRoot = ptr;
	return 0;
}

_g int load_store_push(addr pos)
{
	struct load_store_struct *ptr, *root;

	ptr = LoadStoreRoot;
	Check(ptr == NULL, "load_store_push error.");

	/* extend */
	if (LOAD_STORE_SIZE <= ptr->size) {
		root = load_store_alloc();
		if (root == NULL) {
			Debug("malloc error.");
			return 1;
		}
		root->next = ptr;
		LoadStoreRoot = ptr = root;
	}

	/* store */
	ptr->data[ptr->size++] = pos;
	return 0;
}

_g void load_store_error(void)
{
	struct load_store_struct *x, *y;

	for (x = LoadStoreRoot; x; x = y) {
		y = x->next;
		free(x);
	}
	LoadStoreRoot = NULL;
}

static void load_store_addr(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CODE:
			update_code(pos);
			break;

		default:
			break;
	}
}

_g void load_store_exec(void)
{
	addr *data;
	struct load_store_struct *x, *y;
	size_t size, i;

	/* execute */
	for (x = LoadStoreRoot; x; x = y) {
		y = x->next;
		size = x->size;
		data = x->data;
		for (i = 0; i < size; i++)
			load_store_addr(data[i]);
	}

	/* free */
	load_store_error();
}

