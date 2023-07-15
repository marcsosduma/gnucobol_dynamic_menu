rm teste
rm menu.so
rm makebox.so
cobc -m makebox.cob
cobc -m menu.cob -lncursesw
cobc -x teste.cob -lncursesw
./teste
