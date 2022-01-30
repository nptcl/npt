source = faslcat.c
object = $(source:.c=.o)
a.out = faslcat
CFLAGS = -g -std=c99 -Wall -fstack-protector -I.. -I../../src

$(a.out) : $(object)
	$(CC) $(CFLAGS) -o $(a.out) $(object)

.PHONY : clean
clean :
	-rm -f $(a.out) $(object)

