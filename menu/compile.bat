del *.exe
del *.dll
cobc -m makebox.cob 
cobc -m menu.cob  -lpdcurses
cobc -x teste.cob -lpdcurses
teste
