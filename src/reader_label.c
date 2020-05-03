#include "array.h"
#include "array_access.h"
#include "condition.h"
#include "cons.h"
#include "equal.h"
#include "heap.h"
#include "integer.h"
#include "symbol.h"
#include "typedef.h"
#include "reader_info.h"

/*
 *  readlabel
 */
enum ReadLabel_Index {
	ReadLabel_Label,
	ReadLabel_Value,
	ReadLabel_List,
	ReadLabel_Size
};

#define RefReadLabel	RefArrayA2
#define GetReadLabel	GetArrayA2
#define SetReadLabel	SetArrayA2


/*
 *  readlabel
 */
static void gensym_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 1);
}

static void normal_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 0);
}

static int gensymp_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	return GetUser(pos);
}

static void readlabel_heap(Execute ptr, addr *ret, addr label)
{
	addr pos, gensym;

	Check(! integerp(label), "label error");
	heap_array2(&pos, LISPSYSTEM_READLABEL, ReadLabel_Size);
	gensym_readlabel(pos);
	make_gensym_char(ptr, "READ-LABEL", label, &gensym);
	SetReadLabel(pos, ReadLabel_Label, label);
	SetReadLabel(pos, ReadLabel_Value, gensym);
	*ret = pos;
}

static int gensym_check_readlabel(addr label, addr check)
{
	addr value;

	if (gensymp_readlabel(label)) {
		GetReadLabel(label, ReadLabel_Value, &value);
		if (value == check)
			return 1;
	}

	return 0;
}

static void push_replace_readlabel(addr label, addr pos)
{
	addr list;

	GetReadLabel(label, ReadLabel_List, &list);
	cons_heap(&list, pos, list);
	SetReadLabel(label, ReadLabel_List, list);
}

static void queue_readlabel(Execute ptr, addr queue, addr pos)
{
	addr list, label, check;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensym_check_readlabel(label, pos)) {
				tailqueue(queue, &check);
				push_replace_readlabel(label, check);
			}
		}
	}
}

_g void dotqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	dotqueue(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

_g void pushqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	pushqueue_heap(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

_g int find_readlabel(addr key, addr list, addr *ret)
{
	addr label, check;

	while (list != Nil) {
		GetCons(list, &label, &list);
		GetReadLabel(label, ReadLabel_Label, &check);
		if (eql_function(key, check)) {
			if (ret)
				GetReadLabel(label, ReadLabel_Value, ret);
			return 1;
		}
	}

	return 0;
}

_g void pushlabel_readinfo(Execute ptr, addr value, addr *ret)
{
	addr cons, label, next;

	getreadinfo(ptr, &cons);
	GetReadInfo(cons, ReadInfo_Label, &cons);
	Check(! consp(cons), "type error");
	GetCar(cons, &next);
	if (find_readlabel(value, next, NULL))
		fmte("The #n= label ~S already exists.", value, NULL);
	readlabel_heap(ptr, &label, value);
	cons_heap(&next, label, next);
	SetCar(cons, next);
	*ret = label;
}

static void replace_cons_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr car, cdr;

	GetCons(pos, &car, &cdr);
	if (car == left)
		SetCar(pos, right);
	if (cdr == left)
		SetCdr(pos, right);
}

static void replace_vector_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	size_t size, i;

	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &check);
		if (check == left)
			setarray(pos, i, right);
	}
}

static void replace_array_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_T) return;
	/* general array */
	size = str->size;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	for (i = 0; i < size; i++) {
		arraygen_get(pos, i, &check);
		if (check == left)
			arraygen_set(pos, i, right);
	}
}

static void replace_readlabel(Execute ptr, addr replace, addr left, addr right)
{
	switch (GetType(replace)) {
		case LISPTYPE_CONS:
			replace_cons_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_VECTOR:
			replace_vector_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_ARRAY:
			replace_array_readlabel(ptr, replace, left, right);
			break;

		default:
			break;
	}
}

_g void closelabel_readlabel(Execute ptr, addr label, addr pos)
{
	addr gensym, list, replace;

	Check(! gensymp_readlabel(label), "gensymp error");
	/* error #n= #n# */
	GetReadLabel(label, ReadLabel_Value, &gensym);
	if (pos == gensym)
		fmte("The #n= value don't replace self value #n#.", NULL);
	/* replace */
	GetReadLabel(label, ReadLabel_List, &list);
	while (list != Nil) {
		GetCons(list, &replace, &list);
		replace_readlabel(ptr, replace, gensym, pos);
	}
	/* result */
	SetReadLabel(label, ReadLabel_List, Nil);
	SetReadLabel(label, ReadLabel_Value, pos);
	normal_readlabel(label);
}


/*
 *  vector
 */
static int vector_find_readlabel(addr key, addr vector)
{
	addr check;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &check);
		if (key == check)
			return 1;
	}

	return 0;
}

_g void vector_readlabel(Execute ptr, addr pos)
{
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				if (vector_find_readlabel(value, pos))
					push_replace_readlabel(label, pos);
			}
		}
	}
}


/*
 *  array
 */
static int array_find_readlabel(addr key, addr array)
{
	addr check;
	size_t size, i;

	array_get_rowlength(array, &size);
	for (i = 0; i < size; i++) {
		(void)array_get_t(array, i, &check);
		if (key == check)
			return 1;
	}

	return 0;
}

_g void array_readlabel(Execute ptr, addr pos)
{
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				if (array_find_readlabel(value, pos))
					push_replace_readlabel(label, pos);
			}
		}
	}
}

