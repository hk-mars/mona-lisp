

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

ALL_OBJS = monalisp.o lexer.o

ALL_LIBS = lib_monalisp.a


default:	$(PLATFORM)

all:		$(ALL_APPS)

o:		$(ALL_OBJS)

a:		$(ALL_LIBS)


$(ALL_APPS):	$(ALL_OBJS)
		$(CC) -o $@ $(LDFLAGS) $(ALL_OBJS)


clean:
		rm -rf $(ALL_APPS) $(ALL_OBJS) *.*~

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

none:
		@echo "Please do 'make PLATFORM' where PLATFORM is one of these:"
		@echo "   $(PLATFORMS)"

macosx:
		$(MAKE) $(ALL) SYSCFLAGS="-DLUA_USE_MACOSX" SYSLIBS="-lreadline"


.PHONY:		all $(PLATFORMS) default o a clean depend echo none


monalisp.o:	monalisp.c lexer.c


