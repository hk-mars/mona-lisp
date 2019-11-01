

# Makefile for building monalisp


# Environment settings

PLATFORM = macosx


CC = gcc -std=gnu99
CFLAGS = -O2 -Wall -Wextra $(SYSCFLAGS) $(MYCFLAGS)
LDFLAGS = $(SYSCFLAGS) $(MYCFLAGS)
LIBS = -lm $(SYSLIBS) $(MYLIBS)

AR = ar rcu

SYSCFLAGS =
SYSLDFLAGS =
SYSLIBS =

MYCFLAGS =
MYLDFLAGS =
MYLIBS =

# Environment settings, done


PLATFORMS = aix bsd c89 freebsd generic linux macosx mingw posix solaris


ALL_APPS = monalisp

ALL_OBJS = monalisp.o lex.o syntax.o reader.o mem.o util.o error.o stack.o obj.o \
	list.o form.o token.o eval.o variable.o hsearch.o bin_tree.o tree.o \
	parser.o ast_tree.o asg_graph.o printer.o chars.o function.o macro.o \
	gc.o system.o rules.o


ALL_LIBS = lib_monalisp.a


default:	$(PLATFORM)

all:		$(ALL_APPS)

o:		$(ALL_OBJS)

a:		$(ALL_LIBS)


$(ALL_APPS):	$(ALL_OBJS)
		$(CC) -o $@ $(LDFLAGS) $(ALL_OBJS)

clean:
		rm -rf $(ALL_APPS) $(ALL_OBJS) *.*~ log.txt

depend:
		@$(CC) $(CFLAGS) -MM l*.c

echo:
		@echo "PLATFORM = $(PLATFORM)"
		@echo "CC = $(CC)"
		@echo "CFLAGS = $(CFLAGS)"
		@echo "LDFLAGS = $(SYSLDFLAGS)"
		@echo "LIBS = $(LIBS)"
		@echo "AR = $(AR)"


ALL = all

me:
		make clean
		make all

none:
		@echo "Please do 'make PLATFORM' where PLATFORM is one of these:"
		@echo "   $(PLATFORMS)"

macosx:
		$(MAKE) $(ALL) SYSCFLAGS="-DLUA_USE_MACOSX" SYSLIBS="-lreadline"


.PHONY:		all $(PLATFORMS) default o a clean depend echo none


monalisp.o:	monalisp.c lex.c syntax.c reader.c mem.c util.c error.c stack.c obj.c \
	list.c form.c token.c eval.c variable.c hsearch.c bin_tree.c tree.c \
	parser.c ast_tree.c asg_graph.c printer.c chars.c function.c macro.c \
	gc.c system.c rules.c
















