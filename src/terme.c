#include "define.h"

#ifdef LISP_TERME
/************************************************************
  terme_call.h
 ************************************************************/
#ifndef __TERME_CALL_HEADER__
#define __TERME_CALL_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_init _n(terme_init)
#define terme_free _n(terme_free)
#define terme_switch_textmode _n(terme_switch_textmode)
#define terme_switch_rawmode _n(terme_switch_rawmode)

int terme_init(void);
int terme_free(void);
int terme_switch_textmode(int *ret);
int terme_switch_rawmode(int *ret);

#endif


/************************************************************
  terme_font.h
 ************************************************************/
#ifndef __TERME_FONT_HEADER__
#define __TERME_FONT_HEADER__

#include "print_font.h"
#include "typedef.h"

#define terme_font _n(terme_font)
#define terme_text_color _n(terme_text_color)
#define terme_back_color _n(terme_back_color)
#define terme_cursor_left _n(terme_cursor_left)
#define terme_cursor_right _n(terme_cursor_right)
#define terme_cursor_move _n(terme_cursor_move)
#define terme_cursor_delete_line_left _n(terme_cursor_delete_line_left)
#define terme_cursor_delete_line_right _n(terme_cursor_delete_line_right)
#define terme_cursor_delete_line _n(terme_cursor_delete_line)
#define terme_cursor_delete_page _n(terme_cursor_delete_page)

int terme_font(PrintFont value);
int terme_text_color(PrintColor value);
int terme_back_color(PrintColor value);
int terme_cursor_left(void);
int terme_cursor_right(void);
int terme_cursor_move(int n);
int terme_cursor_delete_line_left(void);
int terme_cursor_delete_line_right(void);
int terme_cursor_delete_line(void);
int terme_cursor_delete_page(void);

#endif


/************************************************************
  terme_input.h
 ************************************************************/
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


/************************************************************
  terme_output.h
 ************************************************************/
#ifndef __TERME_OUTPUT_HEADER__
#define __TERME_OUTPUT_HEADER__

#include "typedef.h"

#define terme_output_init _n(terme_output_init)
#define terme_finish_output _n(terme_finish_output)
#define terme_write_byte _n(terme_write_byte)
#define terme_write_char _n(terme_write_char)
#define terme_write_ascii _n(terme_write_ascii)
#define terme_write_unicode _n(terme_write_unicode)
#define terme_terpri _n(terme_terpri)
#define terme_fresh_line _n(terme_fresh_line)

void terme_output_init(void);
int terme_finish_output(void);
int terme_write_byte(byte c);
int terme_write_char(unicode c, int *ret);
int terme_write_ascii(const char *str);
int terme_write_unicode(const unicode *str);
int terme_terpri(void);
int terme_fresh_line(void);

#endif


/************************************************************
  terme_prompt.h
 ************************************************************/
#ifndef __TERME_PROMPT_HEADER__
#define __TERME_PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_prompt_ _n(terme_prompt_)
#define terme_readline_ _n(terme_readline_)

int terme_prompt_(Execute ptr, addr pos);
int terme_readline_(Execute ptr, addr *ret);

#endif


/************************************************************
  terme_value.h
 ************************************************************/
#ifndef __TERME_VALUE_HEADER__
#define __TERME_VALUE_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_build _n(terme_build)
#define terme_set_prompt_ _n(terme_set_prompt_)
#define terme_get_prompt_ _n(terme_get_prompt_)

#define terme_data_init_ _n(terme_data_init_)
#define terme_data_push_ _n(terme_data_push_)
#define terme_data_get_ _n(terme_data_get_)
#define terme_data_size_ _n(terme_data_size_)
#define terme_data_delete_ _n(terme_data_delete_)
#define terme_data_delete_left_ _n(terme_data_delete_left_)
#define terme_data_delete_right_ _n(terme_data_delete_right_)
#define terme_data_make_ _n(terme_data_make_)
#define terme_history_save_ _n(terme_history_save_)
#define terme_history_update_ _n(terme_history_update_)

void terme_build(void);
int terme_set_prompt_(Execute ptr, addr value);
int terme_get_prompt_(Execute ptr, addr *ret);

int terme_data_init_(Execute ptr);
int terme_data_push_(Execute ptr, int index, unicode c, int *ret);
int terme_data_get_(Execute ptr, int index, unicode *value, int *ret);
int terme_data_size_(Execute ptr, int *ret);
int terme_data_delete_(Execute ptr, int index, int *ret);
int terme_data_delete_left_(Execute ptr, int index, int *ret);
int terme_data_delete_right_(Execute ptr, int index, int *ret);
int terme_data_make_(Execute ptr, addr *ret);
int terme_history_save_(Execute ptr);
int terme_history_update_(Execute ptr, int index, int *ret);

#endif


/************************************************************
  terme_interface.c
 ************************************************************/
#include "execute.h"
#include "terme.h"
#include "typedef.h"

void build_terme(void)
{
	terme_build();
}

int begin_terme(void)
{
	return terme_init();
}

int end_terme(void)
{
	terme_free();
	return 0;
}

int prompt_terme_(Execute ptr, addr pos)
{
	return terme_prompt_(ptr, pos);
}

int readline_terme_(Execute ptr, addr *ret)
{
	return terme_readline_(ptr, ret);
}

int font_terme(PrintFont value)
{
	return terme_font(value);
}

int text_color_terme(PrintColor value)
{
	return terme_text_color(value);
}

int back_color_terme(PrintColor value)
{
	return terme_back_color(value);
}


/************************************************************
  terme_call.c
 ************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include "typedef.h"

static struct termios terme_textmode_termios;
static struct termios terme_switch_termios;
static int terme_switch_textmode_p;
static int terme_x;
static int terme_y;

/*
 *  terme-init
 */
static int terme_init_get(struct termios *ret)
{
	if (ret == NULL)
		return 0;
	return tcgetattr(STDIN_FILENO, ret);
}

static int terme_init_set(struct termios *ret)
{
	return tcsetattr(STDIN_FILENO, TCSAFLUSH, ret);
}

static int terme_init_termios(void)
{
	int fd;
	struct termios v;

	fd = STDIN_FILENO;

	/* backup */
	if (terme_init_get(&v)) {
		fprintf(stderr, "tcgetattr value error\n");
		return 1;
	}
	terme_textmode_termios = v;

	/* set terminal */
	v.c_iflag &= ~(PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
	v.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
	v.c_oflag &= ~OPOST;
	v.c_cflag &= ~(CSIZE | PARENB);
	v.c_cflag |= CS8;
	v.c_cc[VMIN] = 1;
	v.c_cc[VTIME] = 0;
	if (terme_init_set(&v)) {
		fprintf(stderr, "tcsetattr error.\n");
		return 1;
	}
	terme_switch_termios = v;

	return 0;
}

static int terme_getsize(void)
{
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1)
		return 1;
	terme_x = (int)ws.ws_col;
	terme_y = (int)ws.ws_row;

	return 0;
}

int terme_init(void)
{
	/* terminal size */
	if (terme_getsize())
		return 1;

	/* termios */
	if (terme_init_termios())
		return 1;

	/* switch */
	terme_switch_textmode_p = 1;

	/* output */
	terme_input_init();
	terme_output_init();

	return 0;
}

int terme_free(void)
{
	return terme_init_set(&terme_textmode_termios);
}


/*
 *  terme-switch
 */
int terme_switch_textmode(int *ret)
{
	if (terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_init_get(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 1;
	return terme_init_set(&terme_textmode_termios);
}

int terme_switch_rawmode(int *ret)
{
	if (! terme_switch_textmode_p) {
		if (ret)
			*ret = 0;
		return 0;
	}
	if (terme_init_set(&terme_switch_termios)) {
		if (ret)
			*ret = 0;
		return 1;
	}
	if (ret)
		*ret = 1;
	terme_switch_textmode_p = 0;
	memset(&terme_switch_termios, '\0', sizeof(terme_switch_termios));
	return 0;
}


/************************************************************
  terme_font.c
 ************************************************************/
#include <stdio.h>
#include "typedef.h"

static int terme_font_operator(const char *str)
{
	byte c;
	int i;

	for (i = 0; ; i++) {
		c = (byte)str[i];
		if (c == 0)
			break;
		if (terme_write_byte(c))
			return 1;
	}
	return terme_finish_output();
}

int terme_font(PrintFont value)
{
	const char *str;

	switch (value) {
		case print_font_reset:      str = "\x1B[0m"; break;
		case print_font_bold:       str = "\x1B[1m"; break;
		case print_font_faint:      str = "\x1B[2m"; break;
		case print_font_italic:     str = "\x1B[3m"; break;
		case print_font_underline:  str = "\x1B[4m"; break;
		case print_font_blink1:     str = "\x1B[5m"; break;
		case print_font_blink2:     str = "\x1B[6m"; break;
		case print_font_reverse:    str = "\x1B[7m"; break;
		case print_font_hide_in:    str = "\x1B[8m"; break;
		case print_font_hide_out:   str = "\x1B[9m"; break;
		default: return 1;
	}

	return terme_font_operator(str);
}

int terme_text_color(PrintColor value)
{
	const char *str;

	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[30m"; break;
		case print_color_red:             str = "\x1B[31m"; break;
		case print_color_green:           str = "\x1B[32m"; break;
		case print_color_yellow:          str = "\x1B[33m"; break;
		case print_color_blue:            str = "\x1B[34m"; break;
		case print_color_magenta:         str = "\x1B[35m"; break;
		case print_color_cyan:            str = "\x1B[36m"; break;
		case print_color_white:           str = "\x1B[37m"; break;
		case print_color_bright_black:    str = "\x1B[90m"; break;
		case print_color_bright_red:      str = "\x1B[91m"; break;
		case print_color_bright_green:    str = "\x1B[92m"; break;
		case print_color_bright_yellow:   str = "\x1B[93m"; break;
		case print_color_bright_blue:     str = "\x1B[94m"; break;
		case print_color_bright_magenta:  str = "\x1B[95m"; break;
		case print_color_bright_cyan:     str = "\x1B[96m"; break;
		case print_color_bright_white:    str = "\x1B[97m"; break;
		default: return 1;
	}

	return terme_font_operator(str);
}

int terme_back_color(PrintColor value)
{
	const char *str;

	switch (value) {
		case print_color_reset:           str = "\x1B[0m"; break;
		case print_color_black:           str = "\x1B[40m"; break;
		case print_color_red:             str = "\x1B[41m"; break;
		case print_color_green:           str = "\x1B[42m"; break;
		case print_color_yellow:          str = "\x1B[43m"; break;
		case print_color_blue:            str = "\x1B[44m"; break;
		case print_color_magenta:         str = "\x1B[45m"; break;
		case print_color_cyan:            str = "\x1B[46m"; break;
		case print_color_white:           str = "\x1B[47m"; break;
		case print_color_bright_black:    str = "\x1B[100m"; break;
		case print_color_bright_red:      str = "\x1B[101m"; break;
		case print_color_bright_green:    str = "\x1B[102m"; break;
		case print_color_bright_yellow:   str = "\x1B[103m"; break;
		case print_color_bright_blue:     str = "\x1B[104m"; break;
		case print_color_bright_magenta:  str = "\x1B[105m"; break;
		case print_color_bright_cyan:     str = "\x1B[106m"; break;
		case print_color_bright_white:    str = "\x1B[107m"; break;
		default: return 1;
	}

	return terme_font_operator(str);
}

int terme_cursor_left(void)
{
	return terme_font_operator("\x1B[D");
}

int terme_cursor_right(void)
{
	return terme_font_operator("\x1B[C");
}

int terme_cursor_move(int n)
{
	char data[64];
	snprintf(data, 64, "\x1B[%dG", n + 1);
	return terme_font_operator(data);
}

int terme_cursor_delete_line_left(void)
{
	return terme_font_operator("\x1B[1K");
}

int terme_cursor_delete_line_right(void)
{
	return terme_font_operator("\x1B[K");
}

int terme_cursor_delete_line(void)
{
	return terme_font_operator("\x1B[2K");
}

int terme_cursor_delete_page(void)
{
	return terme_font_operator("\x1B[2J")
		|| terme_font_operator("\x1B[1;1H");
}


/************************************************************
  terme_input.c
 ************************************************************/
#include <stdio.h>
#include <unistd.h>
#include <sys/select.h>
#include "character_check.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define TERME_INPUT_SIZE		3
#else
#define TERME_INPUT_SIZE		4096
#endif

#define TERME_INPUT_UNBYTE		64
#define TERME_INPUT_UNREAD		8

/* buffer */
static byte terme_input_buffer[TERME_INPUT_SIZE];
static size_t terme_input_size;
static size_t terme_input_now;
/* unicode */
static byte terme_input_unbyte[TERME_INPUT_UNBYTE];
static int terme_input_unbyte_size;
static int terme_input_unbyte_base;
static int terme_input_unbyte_now;
/* unread */
static unicode terme_input_unread[TERME_INPUT_UNREAD];
static int terme_input_unread_size;
static int terme_input_unread_base;
static int terme_input_unread_now;

void terme_input_init(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
}

static int terme_input_select(int *ret)
{
	int fd, reti;
	fd_set fdset;
	struct timeval tm;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	tm.tv_sec = 0;
	tm.tv_usec = 0;
	reti = select(fd + 1, &fdset, NULL, NULL, &tm);
	if (reti < 0) {
		*ret = 0;
		return 1; /* error */
	}
	if (reti == 0) {
		/* empty */
		*ret = 0;
		return 0;
	}
	else {
		/* can read */
		*ret = 1;
		return 0;
	}
}

#define TERME_CLEAR_INPUT_STDIN		4096
static int terme_clear_input_stdin(void)
{
	byte data[TERME_CLEAR_INPUT_STDIN];
	int check;
	ssize_t rets;

	for (;;) {
		if (terme_input_select(&check))
			return 1;
		if (! check)
			break;
		rets = read(STDIN_FILENO, data, TERME_CLEAR_INPUT_STDIN);
		if (rets < 0)
			return 1;
	}

	return 0;
}

int terme_clear_input(void)
{
	terme_input_size = 0;
	terme_input_now = 0;
	terme_input_unbyte_size = 0;
	terme_input_unbyte_base = 0;
	terme_input_unbyte_now = 0;
	terme_input_unread_size = 0;
	terme_input_unread_base = 0;
	terme_input_unread_now = 0;
	return terme_clear_input_stdin();
}


/*
 *  unbyte
 */
static int terme_unbyte_push(byte c)
{
	if (TERME_INPUT_UNBYTE <= terme_input_unbyte_size)
		return 1; /* error */

	terme_input_unbyte_size++;
	terme_input_unbyte[terme_input_unbyte_base] = c;
	terme_input_unbyte_base++;
	terme_input_unbyte_base %= TERME_INPUT_UNBYTE;

	return 0;
}

static void terme_unbyte_pop(byte *value, int *ret)
{
	if (terme_input_unbyte_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unbyte[terme_input_unbyte_now];
	terme_input_unbyte_size--;
	if (terme_input_unbyte_size) {
		terme_input_unbyte_now++;
		terme_input_unbyte_now %= TERME_INPUT_UNBYTE;
	}
	else {
		terme_input_unbyte_base = 0;
		terme_input_unbyte_now = 0;
	}
	*ret = 1;
}


/*
 *  getc
 */
static int terme_input_wait(void)
{
	int fd, reti;
	fd_set fdset;

	fd = STDIN_FILENO;
	FD_ZERO(&fdset);
	FD_SET(fd, &fdset);
	reti = select(fd + 1, &fdset, NULL, NULL, NULL);
	return reti < 0;
}

static int terme_getc_buffering(void)
{
	ssize_t size;

	terme_input_size = 0; /* for error */
	terme_input_now = 0;
	size = read(STDIN_FILENO, terme_input_buffer, TERME_INPUT_SIZE);
	if (size < 0)
		return 1;
	terme_input_size = (size_t)size;

	return 0;
}

static int terme_getc_hang(byte *value, int *ret)
{
	byte c;
	int check;

	/* unbyte */
	terme_unbyte_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* input buffer */
	if (terme_input_size <= terme_input_now) {
		if (terme_input_select(&check))
			goto error;
		if (! check) { /* empty */
			*value = 0;
			*ret = 0;
			return 0;
		}
		if (terme_getc_buffering())
			goto error;
	}
	*ret = 1;
	*value = terme_input_buffer[terme_input_now];
	terme_input_now++;
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}

static int terme_getc(byte *ret)
{
	byte c;
	int check;

	for (;;) {
		if (terme_getc_hang(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
		return 0;
	}
	*ret = c;
	return 0;

error:
	*ret = 0;
	return 1;
}


/*
 *  unread-char
 */
int terme_unread_char(unicode c)
{
	if (TERME_INPUT_UNREAD <= terme_input_unread_size)
		return 1; /* error */

	terme_input_unread_size++;
	terme_input_unread[terme_input_unread_base] = c;
	terme_input_unread_base++;
	terme_input_unread_base %= TERME_INPUT_UNREAD;

	return 0;
}

static void terme_unread_pop(unicode *value, int *ret)
{
	if (terme_input_unread_size == 0) {
		*value = 0;
		*ret = 0;
		return;
	}

	*value = terme_input_unread[terme_input_unread_now];
	terme_input_unread_size--;
	if (terme_input_unread_size) {
		terme_input_unread_now++;
		terme_input_unread_now %= TERME_INPUT_UNREAD;
	}
	else {
		terme_input_unread_base = 0;
		terme_input_unread_now = 0;
	}
	*ret = 1;
}


/*
 *  read-char
 */
static int terme_listen_rollback(byte *data, int size)
{
	while (size) {
		size--;
		if (terme_unbyte_push(data[size]))
			return 1;
	}

	return 0;
}

#define terme_listen_utf16(x) (0xD800 <= (x) && (x) < 0xE000)

#define terme_listen_getc() { \
	if (terme_getc_hang(&c, &check)) goto error; \
	if (! check) goto hang; \
	if (c == 0x1B) goto escape; \
	data[i++] = c; \
}
static int terme_listen_unicode(void)
{
	byte data[8], c;
	int i, check;
	unicode value;

	i = 0;
	terme_listen_getc();

	/* encode */
	if (0x00 <= c && c <= 0x7F)
		goto sequence1;
	if (0xC2 <= c && c <= 0xDF)
		goto sequence2;
	if (0xE0 <= c && c <= 0xEF)
		goto sequence3;
	if (0xF0 <= c && c <= 0xF7)
		goto sequence4;
	goto invalid;

sequence1:
	value = (unicode)c;
	goto normal;

sequence2:
	value = (0x1F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x80)
		goto invalid;
	goto normal;

sequence3:
	value = (0x0F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x0800)
		goto invalid;
	if (terme_listen_utf16(value))
		goto invalid;
	goto normal;

sequence4:
	value = (0x07 & c) << 18;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 12;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= (0x3F & c) << 6;
	terme_listen_getc();
	if (c < 0x80 || 0xBF < c)
		goto invalid;
	value |= 0x3F & c;
	if (value < 0x010000)
		goto invalid;
	if (UnicodeCount <= value)
		goto invalid;
	goto normal;

normal:
	if (terme_unread_char(value))
		goto error;
	return 0;

invalid:
	return 1;

error:
	(void)terme_listen_rollback(data, i);
	return 1;

hang:
	return terme_listen_rollback(data, i);

escape:
	if (terme_listen_rollback(data, i))
		return 1;
	if (terme_unread_char(0x1B))
		goto error;
	return 0;
}

int terme_listen(int *ret)
{
	/* unread */
	if (terme_input_unread_size) {
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	*ret = (terme_input_unread_size != 0);
	return 0;
}

int terme_hang_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	/* unread */
	terme_unread_pop(&c, &check);
	if (check) {
		*value = c;
		*ret = 1;
		return 0;
	}

	/* listen */
	if (terme_listen_unicode()) {
		*value = 0;
		*ret = 0;
		return 1; /* error */
	}

	/* unread */
	terme_unread_pop(value, ret);
	return 0;
}

int terme_read_char(unicode *value, int *ret)
{
	int check;
	unicode c;

	for (;;) {
		if (terme_hang_char(&c, &check))
			goto error;
		if (check)
			break;
		if (terme_input_wait())
			goto error;
	}
	*value = c;
	*ret = 0; /* normal */
	return 0;

error:
	*value = 0;
	*ret = 0;
	return 1;
}


/*
 *  read-keyboard
 *
 *  Up       ^[OA
 *  Down     ^[OB
 *  Right    ^[OC
 *  Left     ^[OD
 *  PF1      ^[OP
 *  PF2      ^[OQ
 *  PF3      ^[OR
 *  PF4      ^[OS
 *  F1       ^[[11~  ^[OP
 *  F2       ^[[12~  ^[OQ
 *  F3       ^[[13~  ^[OR
 *  F4       ^[[14~  ^[OS
 *  F5       ^[[15~  ^[Ot
 *  F6       ^[[17~  ^[Ou
 *  F7       ^[[18~  ^[Ov
 *  F8       ^[[19~  ^[Ol
 *  F9       ^[[20~  ^[Ow
 *  F10      ^[[21~  ^[Ox
 *  F11      ^[[23~
 *  F12      ^[[24~
 */
static void terme_read_escape(TermeKeyboard *ret)
{
	byte c;

	if (terme_getc(&c))
		goto error;

	if (c == 0x4F)
		goto third_4F;
	if (c == 0x5B)
		goto third_5B;
	goto error;

third_4F:
	if (terme_getc(&c))
		goto error;
	if (0x50 <= c && c <= 0x53) /* PF1 - PF4 */
		goto program;
	goto error;

third_5B:
	if (terme_getc(&c))
		goto error;
	if (c == 0x31)
		goto forth_31;
	if (c == 0x43)
		goto escape_right;
	if (c == 0x44)
		goto escape_left;
	goto error;

forth_31:
	if (terme_getc(&c))
		goto error;
	if (0x31 <= c && c <= 0x39) { /* F1 - F9 */
		if (terme_getc(&c))
			goto error;
		if (c == 0x7E) /* \E[[11~: F1 */
			goto function1;
	}
	goto error;

escape_right: /* 0x1B 0x5B 0x43 */
	ret->type = terme_escape_right;
	return;

escape_left: /* 0x1B 0x5B 0x44 */
	ret->type = terme_escape_left;
	return;

program:
	ret->type = terme_escape_function;
	ret->c = (c - 0x50) + 1; /* PF1 -> 1 */
	return;

function1:
	ret->type = terme_escape_function;
	ret->c = (c - 0x31) + 1; /* F1 -> 1 */
	return;

error:
	ret->type = terme_escape_error;
	return;
}

int terme_read_keyboard(TermeKeyboard *ret)
{
	int check;
	unicode c;

	if (terme_read_char(&c, &check))
		return 1;
	if (c != 0x1B) {
		ret->type = terme_escape_code;
		ret->c = c;
		return 0;
	}

	/* escape sequence */
	terme_read_escape(ret);
	return 0;
}


/************************************************************
  terme_output.c
 ************************************************************/
#include <stdio.h>
#include <unistd.h>
#include "character_check.h"
#include "eastasian_unicode.h"
#include "typedef.h"

#ifdef LISP_DEBUG
#define TERME_OUTPUT_SIZE	3
#else
#define TERME_OUTPUT_SIZE	4096
#endif

static byte terme_output_buffer[TERME_OUTPUT_SIZE];
size_t terme_output_size;
size_t terme_output_x;

void terme_output_init(void)
{
	terme_output_size = 0;
	terme_output_x = 0;
}

int terme_finish_output(void)
{
	byte *data;
	size_t size, retu;
	ssize_t rets;

	size = terme_output_size;
	data = terme_output_buffer;
	while (size) {
		rets = write(STDOUT_FILENO, data, size);
		if (rets < 0)
			return 1; /* error */
		retu = (size_t)rets;
		if (size <= retu)
			break;
		size -= retu;
		data += retu;
	}
	terme_output_size = 0;

	return 0;
}

int terme_write_byte(byte c)
{
	if (TERME_OUTPUT_SIZE <= terme_output_size) {
		if (terme_finish_output())
			return 1;
	}

	terme_output_buffer[terme_output_size] = c;
	terme_output_size++;

	return 0;
}

static int terme_write_memory(const byte *data, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		if (terme_write_byte(data[i]))
			return 1;
	}

	return 0;
}

static int terme_write_utf8(unicode u, byte *dst, size_t *ret)
{
	size_t w;

	w = 0;
	/* 1 byte */
	if (u < 0x80) {
		dst[w++] = u;
		goto normal;
	}
	/* 2 byte */
	if (u < 0x0800) {
		dst[w++] = 0xC2 | (u >> 6);
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 3 byte */
	if (u < 0xD800) {
		goto sequence3;
	}
	/* surrogate pair */
	if (u < 0xE000) {
		goto error;
	}
	/* 3 byte */
	if (u < 0x010000) {
sequence3:
		dst[w++] = 0xE0 | (u >> 12);
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* 4 byte */
	if (u < UnicodeCount) {
		dst[w++] = 0xF0 | (u >> 18);
		dst[w++] = 0x80 | (0x3F & (u >> 12));
		dst[w++] = 0x80 | (0x3F & (u >> 6));
		dst[w++] = 0x80 | (0x3F & u);
		goto normal;
	}
	/* error */

error:
	return 1;

normal:
	*ret = w;
	return 0;
}

int terme_write_char(unicode c, int *ret)
{
	byte data[8];
	size_t size;

	if (terme_write_utf8(c, data, &size)) {
		/* encode error */
		if (ret)
			*ret = 1;
		return 0;
	}

	/* eastasian width */
	if (c == 0x0D)
		terme_output_x = 0;
	else
		terme_output_x += (size_t)eastasian_width(c);

	/* output */
	if (ret)
		*ret = 0;
	return terme_write_memory(data, size);
}

int terme_write_ascii(const char *str)
{
	int i;
	unicode c;

	for (i = 0; ; i++) {
		c = (unicode)str[i];
		if (c == 0)
			break;
		if (0x80 <= c)
			continue;
		if (terme_write_char(c, NULL))
			return 1;
	}

	return 0;
}

int terme_write_unicode(const unicode *str)
{
	int i;
	unicode c;

	for (i = 0; ; i++) {
		c = str[i];
		if (c == 0)
			break;
		if (terme_write_char(c, NULL))
			return 1;
	}

	return 0;
}


/*
 *  call
 */
int terme_terpri(void)
{
	return terme_write_byte(0x0D) || terme_write_byte(0x0A);
}

int terme_fresh_line(void)
{
	return terme_output_x? terme_terpri(): 0;
}


/************************************************************
  terme_prompt.c
 ************************************************************/
#include "condition.h"
#include "copy.h"
#include "print_font.h"
#include "stream.h"
#include "stream_common.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

static int terme_prompt_size;
static int terme_prompt_now;
static int terme_prompt_history;

/*
 *  error
 */
static int terme_fmte_va_(addr format, addr args)
{
	int mode, check;

	if (terme_switch_textmode(&mode)) {
		Abort("terme_switch_textmode error.");
		return 0;
	}
	check = call_simple_error_(NULL, format, args);
	if (mode && terme_switch_rawmode(NULL)) {
		Abort("terme_switch_rawmode error.");
		return 0;
	}

	return check;
}

static int terme_fmte_(const char *str, ...)
{
	addr format, args;
	va_list va;

	if (terme_fresh_line()) {
		Abort("terme_fresh_line error.");
		return 0;
	}
	if (terme_finish_output()) {
		Abort("terme_finish_output error.");
		return 0;
	}
	strvect_char_heap(&format, str);
	va_start(va, str);
	copylocal_list_stdarg(NULL, &args, va);
	va_end(va);
	return terme_fmte_va_(format, args);
}


/*
 *  prompt
 */
int terme_prompt_(Execute ptr, addr pos)
{
	Check(! stringp(pos), "type error");
	return terme_set_prompt_(ptr, pos);
}

static int terme_prompt_string_(addr pos)
{
	int ignore;
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (terme_write_char(c, &ignore))
			return terme_fmte_("terme_write_char error.", NULL);
	}

	return 0;
}

static int terme_prompt_output_(Execute ptr)
{
	int check;
	addr pos, io;
	size_t size;

	/* special */
	Return(terme_get_prompt_(ptr, &pos));
	if (pos == Nil)
		return 0;

	/* prompt */
	string_length(pos, &size);
	terme_prompt_size = (int)size;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &io));
	Return(fresh_line_stream_(io, &check));
	if (terme_font(print_font_reset))
		goto error;
	if (terme_text_color(print_color_bright_green))
		goto error;
	Return(terme_prompt_string_(pos));
	if (terme_font(print_font_reset))
		goto error;
	if (terme_finish_output())
		goto error;
	return 0;

error:
	return terme_fmte_("terme output error.", NULL);
}


/*
 *  readline
 */
static int terme_readline_loop_(Execute ptr, TermeKeyboard *, addr *, int *);

static int terme_readline_control_(Execute ptr,
		TermeKeyboard *str, addr *value, int *ret)
{
	switch (str->c) {
		case 0x03:	/* C */
			return terme_fmte_("Ctrl + C", NULL);

		case 0x04:  /* D, delete */
			str->type = terme_escape_delete;
			break;

		case 0x0A:  /* J */
		case 0x0D:  /* Return, Enter, Ctrl+M */
			str->type = terme_escape_return;
			break;

		case 0x10:  /* P, up */
			str->type = terme_escape_up;
			break;

		case 0x0E:  /* N, down */
			str->type = terme_escape_down;
			break;

		case 0x06:  /* F, left */
			str->type = terme_escape_right;
			break;

		case 0x02:  /* B, right */
			str->type = terme_escape_left;
			break;

		case 0x01:  /* A */
			str->type = terme_escape_first;
			break;

		case 0x05:  /* E */
			str->type = terme_escape_last;
			break;

		case 0x0C:  /* L */
			str->type = terme_escape_update;
			break;

		case 0x08:  /* H, backspace */
			str->type = terme_escape_backspace;
			break;

		case 0x15:  /* U, rmleft */
			str->type = terme_escape_rmleft;
			break;

		case 0x0B:  /* K, rmright */
			str->type = terme_escape_rmright;
			break;

		case 0x09:  /* I, tab */
			str->type = terme_escape_tab;
			break;

		default:
			fprintf(stderr, "Ctrl + %x\n", (int)str->c);
			str->type = terme_escape_error;
			break;
	}

	return terme_readline_loop_(ptr, str, value, ret);
}

static int terme_readline_write_line_(Execute ptr, int first)
{
	int i, check;
	unicode c;

	for (i = first; ; i++) {
		Return(terme_data_get_(ptr, i, &c, &check));
		if (! check)
			break;
		if (terme_write_char(c, &check))
			return terme_fmte_("terme_write_char error.", NULL);
	}

	return 0;
}

static int terme_readline_output_(Execute ptr)
{
	int base;

	base = terme_prompt_size + terme_prompt_now + 1;
	Return(terme_readline_write_line_(ptr, terme_prompt_now));
	if (terme_cursor_move(base))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_code_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	int check;
	unicode c;

	c = str->c;
	if (c < 0x20)
		return terme_readline_control_(ptr, str, value, ret);

	/* output */
	Return(terme_data_push_(ptr, terme_prompt_now, c, &check));
	if (! check)  /* buffer overflow */
		return Result(ret, 0);
	Return(terme_readline_output_(ptr));
	terme_prompt_now++;

	return Result(ret, 0);
}

static int terme_readline_up_down_(Execute ptr, int diff)
{
	int index, check;

	if (terme_prompt_history == 0) {
		Return(terme_history_save_(ptr));
	}
	index = terme_prompt_history + diff;
	Return(terme_history_update_(ptr, index, &check));
	if (! check)
		return 0;
	terme_prompt_history = index;

	/* output */
	Return(terme_data_size_(ptr, &terme_prompt_now));
	if (terme_cursor_delete_line())
		return terme_fmte_("terme_cursor_delete_line error.", NULL);
	if (terme_cursor_move(0))
		return terme_fmte_("terme_cursor_move error.", NULL);
	Return(terme_prompt_output_(ptr));
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_finish_output())
		return terme_fmte_("terme_finish_output error.", NULL);

	return 0;
}

static int terme_readline_up_(Execute ptr)
{
	return terme_readline_up_down_(ptr, 1);
}

static int terme_readline_down_(Execute ptr)
{
	return terme_readline_up_down_(ptr, -1);
}

static int terme_readline_left_(Execute ptr)
{
	if (terme_prompt_now == 0)
		return 0;
	if (terme_cursor_left())
		return terme_fmte_("terme_cursor_left error.", NULL);
	terme_prompt_now--;

	return 0;
}

static int terme_readline_right_(Execute ptr)
{
	int size;

	Return(terme_data_size_(ptr, &size));
	if (size <= terme_prompt_now)
		return 0;
	if (terme_cursor_right())
		return terme_fmte_("terme_cursor_right error.", NULL);
	terme_prompt_now++;

	return 0;
}

static int terme_readline_return(Execute ptr, addr *value, int *ret)
{
	terme_prompt_history = 0;
	Return(terme_data_make_(ptr, value));
	return Result(ret, 1);
}

static int terme_readline_backspace_(Execute ptr)
{
	int check;

	if (terme_prompt_now <= 0)
		return 0;
	/* backspace */
	Return(terme_data_delete_(ptr, terme_prompt_now - 1, &check));
	if (! check)
		return 0;
	terme_prompt_now--;

	/* output */
	if (terme_cursor_left())
		return terme_fmte_("terme_cursor_left error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_prompt_now));
	if (terme_cursor_move(terme_prompt_size + terme_prompt_now))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_cursor_(Execute ptr, int n)
{
	int base;

	terme_prompt_now = n;
	base = terme_prompt_size + terme_prompt_now;
	if (terme_cursor_move(base))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_first_(Execute ptr)
{
	return terme_readline_cursor_(ptr, 0);
}

static int terme_readline_last_(Execute ptr)
{
	int size;
	Return(terme_data_size_(ptr, &size));
	return terme_readline_cursor_(ptr, size);
}

static int terme_readline_update_(Execute ptr)
{
	int base;

	/* cursor position */
	base = terme_prompt_size + terme_prompt_now;

	/* all clean */
	if (terme_cursor_delete_page())
		return terme_fmte_("terme_cursor_delete_page error.", NULL);

	/* output prompt */
	Return(terme_prompt_output_(ptr));

	/* output text */
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(base))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_delete_(Execute ptr, addr *value, int *ret)
{
	int check;

	/* exit */
	Return(terme_data_size_(ptr, &check));
	if (check == 0) {
		*value = Nil;
		return Result(ret, 1);
	}

	/* delete */
	Return(terme_data_delete_(ptr, terme_prompt_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, terme_prompt_now));
	if (terme_cursor_move(terme_prompt_size + terme_prompt_now))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmleft_(Execute ptr)
{
	int check;

	/* data */
	Return(terme_data_delete_left_(ptr, terme_prompt_now, &check));
	if (! check)
		return 0;
	terme_prompt_now = 0;

	/* cursor */
	if (terme_cursor_move(terme_prompt_size))
		return terme_fmte_("terme_cursor_move error.", NULL);
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);
	Return(terme_readline_write_line_(ptr, 0));
	if (terme_cursor_move(terme_prompt_size))
		return terme_fmte_("terme_cursor_move error.", NULL);

	return 0;
}

static int terme_readline_rmright_(Execute ptr)
{
	int check;

	Return(terme_data_delete_right_(ptr, terme_prompt_now, &check));
	if (! check)
		return 0;
	if (terme_cursor_delete_line_right())
		return terme_fmte_("terme_cursor_delete_line_right error.", NULL);

	return 0;
}

static int terme_readline_loop_(Execute ptr, TermeKeyboard *str, addr *value, int *ret)
{
	*ret = 0;
	switch (str->type) {
		case terme_escape_error:
			break;

		case terme_escape_code:
			return terme_readline_code_(ptr, str, value, ret);

		case terme_escape_up:
			return terme_readline_up_(ptr);

		case terme_escape_down:
			return terme_readline_down_(ptr);

		case terme_escape_left:
			return terme_readline_left_(ptr);

		case terme_escape_right:
			return terme_readline_right_(ptr);

		case terme_escape_return:
			return terme_readline_return(ptr, value, ret);

		case terme_escape_backspace:
			return terme_readline_backspace_(ptr);

		case terme_escape_first:
			return terme_readline_first_(ptr);

		case terme_escape_last:
			return terme_readline_last_(ptr);

		case terme_escape_update:
			return terme_readline_update_(ptr);

		case terme_escape_delete:
			return terme_readline_delete_(ptr, value, ret);

		case terme_escape_rmleft:
			return terme_readline_rmleft_(ptr);

		case terme_escape_rmright:
			return terme_readline_rmright_(ptr);

		case terme_escape_function:
		case terme_escape_tab:
			break; /* ignore */

		default:
			return terme_fmte_("terme_readline error.", NULL);
	}

	return 0;
}

static int terme_readline_call_(Execute ptr, addr *ret)
{
	int check;
	addr pos;
	TermeKeyboard str;

	Return(terme_prompt_output_(ptr));
	pos = Nil;
	for (;;) {
		if (terme_read_keyboard(&str)) {
			*ret = Nil;
			return terme_fmte_("terme_read_keyboard error.", NULL);
		}
		Return(terme_readline_loop_(ptr, &str, &pos, &check));
		if (check)
			break;
	}
	if (terme_fresh_line())
		goto error;
	if (terme_finish_output())
		goto error;
	return Result(ret, pos);

error:
	*ret = Nil;
	return terme_fmte_("terme output error.", NULL);
}

int terme_readline_(Execute ptr, addr *ret)
{
	int mode, check;

	/* initialize */
	terme_prompt_size = 0;
	terme_prompt_now = 0;
	terme_prompt_history = 0;
	Return(terme_data_init_(ptr));

	/* readline */
	if (terme_switch_rawmode(&mode)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_rawmode error.", NULL);
	}
	check = terme_readline_call_(ptr, ret);
	if (mode && terme_switch_textmode(NULL)) {
		*ret = Nil;
		return terme_fmte_("terme_switch_textmode error.", NULL);
	}

	return check;
}


/************************************************************
  terme_value.c
 ************************************************************/
#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "constant.h"
#include "heap.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

#define TERME_VALUE_SIZE		4096
#define TERME_VALUE_HISTORY		5

static int terme_history_index = 0;

enum terme_value {
	terme_value_prompt,
	terme_value_data,
	terme_value_history,
	terme_value_size
};

/*
 *  build
 */
static int terme_value_array_(addr *ret)
{
	addr pos;
	struct array_struct *str;

	Return(array_heap_(&pos, 1, TERME_VALUE_SIZE));
	str = ArrayInfoStruct(pos);
	str->fillpointer = 1;
	Return(array_character_alloc_(NULL, pos));
	str->front = 0;

	return Result(ret, pos);
}

static int terme_value_heap_(addr *ret)
{
	addr pos, x;

	/* object */
	vector2_heap(&pos, terme_value_size);

	/* array */
	Return(terme_value_array_(&x));
	SetArrayA2(pos, terme_value_data, x);

	/* queue */
	vector_heap(&x, TERME_VALUE_HISTORY);
	setarray(pos, terme_value_history, x);

	return Result(ret, pos);
}

void terme_build(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_TERME, &symbol);
	Error(terme_value_heap_(&pos));
	SetValueSymbol(symbol, pos);
}

static int terme_get_value_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}


/*
 *  prompt
 */
int terme_set_prompt_(Execute ptr, addr value)
{
	addr array;

	Return(terme_get_value_(ptr, &array));
	SetArrayA2(array, terme_value_prompt, value);

	return 0;
}

int terme_get_prompt_(Execute ptr, addr *ret)
{
	addr array;

	Return(terme_get_value_(ptr, &array));
	GetArrayA2(array, terme_value_prompt, ret);

	return 0;
}


/*
 *  data
 */
static int terme_data_array_(Execute ptr, addr *ret)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_data, ret);

	return 0;
}

int terme_data_init_(Execute ptr)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	str->front = 0;

	return 0;
}

static int terme_data_shift_right_(addr pos, size_t x, size_t y)
{
	unicode c;
	size_t i, size;

	if (x == y)
		return 0;
	size = y - x;
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, y - i - 1, &c));
		Return(array_set_character_(pos, y - i, c));
	}

	return 0;
}

int terme_data_push_(Execute ptr, int index, unicode c, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (str->size <= str->front)
		return Result(ret, 0);
	if (index < 0 || str->front < index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_right_(pos, (size_t)index, str->front));

	/* set */
	Return(array_set_character_(pos, index, c));
	str->front++;

	return Result(ret, 1);
}

int terme_data_get_(Execute ptr, int index, unicode *value, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);

	*ret = 1;
	return array_get_unicode_(pos, (size_t)index, value);
}

int terme_data_size_(Execute ptr, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);

	return Result(ret, (int)str->front);
}

static int terme_data_shift_left_(addr pos, size_t x, size_t y)
{
	unicode c;

	for (; x < y; x++) {
		Return(array_get_unicode_(pos, x + 1, &c));
		Return(array_set_character_(pos, x, c));
	}

	return 0;
}

int terme_data_delete_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_left_(pos, (size_t)index, str->front));
	str->front--;

	return Result(ret, 1);
}

int terme_data_delete_left_(Execute ptr, int index, int *ret)
{
	int i, size;
	unicode c;
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	/* do nothing */
	if (index == 0)
		return Result(ret, 0);

	/* all delete */
	if (index == str->front) {
		str->front = 0;
		return Result(ret, 1);
	}

	/* invalid */
	if (index < 0 || str->front < index)
		return Result(ret, 0);

	/* shift */
	size = str->front - index;
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i + index, &c));
		Return(array_set_character_(pos, i, c));
	}
	str->front = size;

	return Result(ret, 1);
}

int terme_data_delete_right_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	str->front = index;

	return Result(ret, 1);
}

static int terme_data_heap_(Execute ptr, addr *ret, int eol)
{
	addr pos, make;
	unicode c;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	size = str->front;
	strvect_heap(&make, size + (eol? 1: 0));

	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i, &c));
		Return(strvect_setc_(make, i, c));
	}
	if (eol) {
		Return(strvect_setc_(make, i, 0x0A));
	}

	return Result(ret, make);
}

static int terme_history_set_(Execute ptr, addr value)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	setarray(pos, terme_history_index, value);

	return 0;
}

static int terme_history_next_(Execute ptr)
{
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	terme_history_index = (terme_history_index + 1) % sizei;

	return 0;
}

int terme_data_make_(Execute ptr, addr *ret)
{
	addr pos;

	/* history */
	Return(terme_data_heap_(ptr, &pos, 0));
	Return(terme_history_set_(ptr, pos));
	Return(terme_history_next_(ptr));

	/* result */
	return terme_data_heap_(ptr, ret, 1);
}

int terme_history_save_(Execute ptr)
{
	addr pos;
	Return(terme_data_heap_(ptr, &pos, 0));
	return terme_history_set_(ptr, pos);
}

static int terme_history_get_(Execute ptr, int index, addr *value, int *ret)
{
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	if (index < 0 || sizei <= index) {
		*value = Nil;
		return Result(ret, 0);
	}
	index = (sizei + terme_history_index - index) % sizei;
	getarray(pos, index, value);

	return Result(ret, 1);
}

static int terme_history_copy_(Execute ptr, addr pos)
{
	unicode c;
	addr array;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &array));
	str = ArrayInfoStruct(array);
	strvect_length(pos, &size);
	if (str->size < size)
		size = str->size;

	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(array_set_character_(array, i, c));
	}
	str->front = size;

	return 0;
}

int terme_history_update_(Execute ptr, int index, int *ret)
{
	int check;
	addr pos;

	Return(terme_history_get_(ptr, index, &pos, &check));
	if ((! check) || pos == Nil)
		return Result(ret, 0);
	Return(terme_history_copy_(ptr, pos));

	return Result(ret, 1);
}


#else
/************************************************************
  terme_disable.c
 ************************************************************/
#include "constant.h"
#include "print_write.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "terme.h"

void build_terme(void)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	SetValueSymbol(symbol, Nil);
}

int begin_terme(void)
{
	return 0;
}

int end_terme(void)
{
	return 0;
}

int prompt_terme_(Execute ptr, addr pos)
{
	addr symbol;

	Check(! stringp(pos), "type error");
	GetConst(SYSTEM_TERME, &symbol);
	setspecial_local(ptr, symbol, pos);

	return 0;
}

static int readline_terme_append_newline_(addr pos, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_heap(&value, size + 1UL);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(strvect_setc_(value, i, c));
	}
	Return(strvect_setc_(value, i, 0x0A));

	return Result(ret, value);
}

int readline_terme_(Execute ptr, addr *ret)
{
	int check;
	addr input, output, prompt, pos;

	GetConst(STREAM_STDIN, &input);
	GetConst(STREAM_STDOUT, &output);
	GetConst(SYSTEM_TERME, &prompt);
	Return(getspecialcheck_local_(ptr, prompt, &prompt));
	Return(fresh_line_stream_(output, &check));
	if (prompt != Nil) {
		Return(princ_print(ptr, output, prompt));
	}
	Return(finish_output_stream_(output));
	Return(clear_input_stream_(input));
	Return(read_line_stream_(ptr, &pos, &check, input, 0, Nil, 0));
	if (pos == Nil)
		return Result(ret, Nil);
	Return(readline_terme_append_newline_(pos, &pos));
	return Result(ret, pos);
}

int font_terme(PrintFont value)
{
	return 0;
}

int text_color_terme(PrintColor value)
{
	return 0;
}

int back_color_terme(PrintColor value)
{
	return 0;
}


#endif

