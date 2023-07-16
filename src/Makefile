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
PROGRAM1    = main_program.exe

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
     PROGRAM1 = main_program
 endif

SRCS1=menu.cob
SRCS2=makebox.cob 
SRCS3=validate_usr.cob 
SRCS4=makemenu.cob
SRCS5=test.cob
SRCS6=main_program.cob
SRCS7=moduser.cob

all: menu$(MODEXT) makebox$(MODEXT) validate_usr$(MODEXT) makemenu$(MODEXT) $(PROGRAM) $(PROGRAM1) moduser$(EXEEXT)

menu$(MODEXT): $(SRCS1)
	$(COBC) -m $(SRCS1) $(CFLAGS)

makebox$(MODEXT): $(SRCS2)
	$(COBC) -m $(SRCS2) $(CFLAGS)

validate_usr$(MODEXT): $(SRCS3)
	$(COBC) -m $(SRCS3) $(CFLAGS)

makemenu$(MODEXT): $(SRCS4)
	$(COBC) -m $(SRCS4) $(CFLAGS)
     
$(PROGRAM): $(SRCS5)
	  $(COBC) -x $(SRCS5) $(CFLAGS)
     
$(PROGRAM1): $(SRCS6)
	  $(COBC) -x $(SRCS6) $(CFLAGS)

moduser$(EXEEXT): $(SRCS7)
	  $(COBC) -x $(SRCS7) $(CFLAGS)
