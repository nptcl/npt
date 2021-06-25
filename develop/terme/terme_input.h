#ifndef __TERME_INPUT_HEADER__
#define __TERME_INPUT_HEADER__

#include "typedef.h"

#define terme_input_init _n(terme_input_init)
#define terme_clear_input _n(terme_clear_input)
#define terme_unread_char _n(terme_unread_char)
#define terme_listen _n(terme_listen)
#define terme_hang_char _n(terme_hang_char)
#define terme_read_char _n(terme_read_char)

enum terme_escape {
	terme_escape_error,
	terme_escape_code,
	terme_escape_up,         /* ^P */
	terme_escape_down,       /* ^N */
	terme_escape_left,       /* ^F */
	terme_escape_right,      /* ^B */
	terme_escape_function,   /* Fx, PFx */
	terme_escape_return,     /* ^J, ^M, Enter */
	terme_escape_backspace,  /* ^H, BS */
	terme_escape_first,      /* ^A */
	terme_escape_last,       /* ^E */
	terme_escape_update,     /* ^L */
	terme_escape_delete,     /* ^D */
	terme_escape_rmleft,     /* ^U */
	terme_escape_rmright,    /* ^K */
	terme_escape_tab,        /* ^I */
	terme_escape_size
};
typedef enum terme_escape TermeEscape;

struct terme_keyboard {
	TermeEscape type;
	unicode c;
};
typedef struct terme_keyboard TermeKeyboard;

void terme_input_init(void);
int terme_clear_input(void);
int terme_unread_char(unicode c);
int terme_listen(int *ret);
int terme_hang_char(unicode *value, int *ret);
int terme_read_char(unicode *value, int *ret);
int terme_read_keyboard(TermeKeyboard *ret);

#endif

