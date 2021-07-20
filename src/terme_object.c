#include "constant.h"
#include "heap.h"
#include "terme_object.h"
#include "symbol.h"
#include "typedef.h"

byte *terme_pointer(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	posbody(pos, &pos);
	return (byte *)pos;
}

void terme_get(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TERME);
	getarray(pos, index, ret);
}

void terme_set(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_TERME);
	setarray(pos, index, value);
}

enum terme_type terme_get_type(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	return (enum terme_type)GetUser(pos);
}

void terme_set_type(addr pos, enum terme_type type)
{
	CheckType(pos, LISPSYSTEM_TERME);
	SetUser(pos, (int)type);
}

int termep(addr pos)
{
	return GetType(pos) == LISPSYSTEM_TERME;
}

int terme_root_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_root);
}

int terme_data_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_data);
}

int terme_string_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_string);
}

int terme_screen_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_screen);
}

int terme_display_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_display);
}

int terme_line_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_line);
}

int terme_history_p(addr pos)
{
	return termep(pos) && (terme_get_type(pos) == terme_type_history);
}


/*
 *  terme-root
 */
struct terme_root_struct {
	unsigned terme_p : 1;
	enum prompt_mode mode;
};

static struct terme_root_struct *terme_root_body(addr pos)
{
	Check(! terme_root_p(pos), "type error");
	return (struct terme_root_struct *)terme_pointer(pos);
}

void terme_root_build(addr *ret)
{
	addr root;
	struct terme_root_struct *str;

	heap_smallsize(&root, LISPSYSTEM_TERME,
			terme_root_size,
			sizeoft(struct terme_root_struct));
	terme_set_type(root, terme_type_root);
	str = terme_root_body(root);
#ifdef LISP_TERME
	str->terme_p = 1;
#else
	str->terme_p = 0;
#endif
	str->mode = prompt_input;
	*ret = root;
}

static int terme_root_special_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_SPECIAL_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int terme_root_get_(Execute ptr, enum terme_root_index index, addr *ret)
{
	addr pos;

	Return(terme_root_special_(ptr, &pos));
	terme_get(pos, (size_t)index, ret);

	return 0;
}

int terme_root_data_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_data, ret);
}

int terme_root_screen_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_screen, ret);
}

int terme_root_display_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_display, ret);
}

int terme_root_history_(Execute ptr, addr *ret)
{
	return terme_root_get_(ptr, terme_root_history, ret);
}

int terme_root_enable_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_root_struct *str;

	Return(terme_root_special_(ptr, &pos));
	str = terme_root_body(pos);

	return Result(ret, str->terme_p);
}


/*
 *  prompt
 */
int terme_prompt_set_(Execute ptr, addr value, enum prompt_mode mode)
{
	addr pos;
	struct terme_root_struct *str;

	Return(terme_root_special_(ptr, &pos));
	terme_set(pos, terme_root_prompt, value);
	str = terme_root_body(pos);
	str->mode = mode;

	return 0;
}

int terme_prompt_get_(Execute ptr, addr *value, enum prompt_mode *mode)
{
	addr pos;
	struct terme_root_struct *str;

	Return(terme_root_special_(ptr, &pos));
	if (value) {
		terme_get(pos, terme_root_prompt, value);
	}
	if (mode) {
		str = terme_root_body(pos);
		*mode = str->mode;
	}

	return 0;
}

