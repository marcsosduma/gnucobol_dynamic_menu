#===========================================================
#               C O M M A N D   A L I A S E S
#===========================================================

COBC        = cobc
RMFIL       = rm -f
COPY        = cp -p
CFLAGS      = -lpdcurses
CLEAN       = *.dll *.exe
MODEXT      = .dll
PROGRAM     = test.exe
PROGRAM1    = main.exe

uname_S    := $(shell uname -s 2>/dev/null)

#
# Windows
#
 ifneq (,$(findstring windows32,$(uname_S)))
     OSTYPE  = WINDOWS
     FixPath = $(subst /,\,$1)
     EXEEXT  = .exe
     MODEXT  = .dll
     OBJEXT  = .o
     WINLIB  = -lws2_32.lib
     CFLAGS  =-lpdcurses
     CLEAN   = *.dll *.exe
 endif
#
# Linux
#
 ifneq (,$(findstring Linux,$(uname_S)))
     OSTYPE  = LINUX
     FixPath = $1
     EXEEXT  =
     MODEXT  = .so
     OBJEXT  = .o
     WINLIB  =
     CFLAGS  =-lncursesw
     CLEAN   = *.so 
     PROGRAM = test
     PROGRAM1 = main
 endif

SRCS1=read_user_text_file.cob
SRCS2=read_profile_text_file.cob 
SRCS3=read_menu_text_file.cob 
SRCS4=read_menu_profile_text_file.cob
SRCS5=read_user_profile_text_file.cob

all: read_user_text_file$(EXEEXT) read_profile_text_file$(EXEEXT) read_menu_text_file$(EXEEXT) read_user_profile_text_file$(EXEEXT) read_menu_profile_text_file$(EXEEXT)

read_user_text_file$(EXEEXT): $(SRCS1)
	$(COBC) -x $(SRCS1) $(CFLAGS)

read_profile_text_file$(EXEEXT): $(SRCS2)
	$(COBC) -x $(SRCS2) $(CFLAGS)

read_menu_text_file$(EXEEXT): $(SRCS3)
	$(COBC) -x $(SRCS3) $(CFLAGS)

read_menu_profile_text_file$(EXEEXT): $(SRCS4)
	$(COBC) -x $(SRCS4) $(CFLAGS)

read_user_profile_text_file$(EXEEXT): $(SRCS4)
	$(COBC) -x $(SRCS5) $(CFLAGS)

