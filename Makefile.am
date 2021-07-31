if LISP_DEGRADE
CFLAGS += -Isrc -Itest
include src/source.mk
else
include src/release.mk
endif

CFLAGS += $(lisp_optimize)
CFLAGS += -I/usr/incldue -I/usr/local/include
LDFLAGS += -L/usr/lib -L/usr/local/lib
VPATH = src:test
bin_PROGRAMS = npt
npt_SOURCES = $(source)

