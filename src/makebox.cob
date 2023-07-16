       >>SOURCE FORMAT IS FREE
       *>****************************************************************
       *> Author: MARCOS DUMA
       *> Date: 23/04/2023
       *> Purpose: MENU FOR GNUCOBOL PROGRAMS - LINUX AND WINDOWS
       *> Tectonics: cobc
       *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. makebox.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
	   SOURCE-COMPUTER.      
             GNUCOBOL.
           OBJECT-COMPUTER.      
             GNUCOBOL
             CLASSIFICATION brazil.
       SPECIAL-NAMES.
           CRT STATUS IS WK-CRT-STATUS.
           CURSOR     IS WK-CursorRowCol.
	       LOCALE brazil "pt_BR.UTF-8".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WK-NUML              PIC 999.
       77  WK-NUMC              PIC 999.
       01  WK-CRT-STATUS         PIC 9(04) VALUE 9999.
       01  WK-CursorRowCol       PIC 9(06) value 0000.
       01  redefines WK-CursorRowCol .
           05 wCursorRow       Pic 9(03).
           05 wCursorCol       Pic 9(03).    
       01  box1-dos-line-horizontal        pic x(01) value x"cd". 
       01  box1-dos-line-vertical          pic x(01) value x"ba". 
       01  box1-dos-top-left               pic x(01) value x"c9". 
       01  box1-dos-top-right              pic x(01) value x"bb". 
       01  box1-dos-bottom-left            pic x(01) value x"c8". 
       01  box1-dos-bottom-right           pic x(01) value x"bc". 
       01  box1-dos-line-sep-left          pic x(01) value x"cc".
       01  box1-dos-line-sep-right         pic x(01) value x"b9".
       
       01  box2-dos-line-horizontal        pic x(01) value x"c4". 
       01  box2-dos-line-vertical          pic x(01) value x"b3". 
       01  box2-dos-top-left               pic x(01) value x"da". 
       01  box2-dos-top-right              pic x(01) value x"bf". 
       01  box2-dos-bottom-left            pic x(01) value x"c0". 
       01  box2-dos-bottom-right           pic x(01) value x"d9". 
       01  box2-dos-line-sep-left          pic x(01) value x"c3".
       01  box2-dos-line-sep-right         pic x(01) value x"b4".
       
       01  box1-utf8-line-horizontal       pic x(03) value x"e29590". 
       01  box1-utf8-line-vertical         pic x(03) value x"e29591". 
       01  box1-utf8-top-left              pic x(03) value x"e29594". 
       01  box1-utf8-top-right             pic x(03) value x"e29597". 
       01  box1-utf8-bottom-left           pic x(03) value x"e2959a". 
       01  box1-utf8-bottom-right          pic x(03) value x"e2959d". 
       01  box1-utf8-line-sep-left         pic x(03) value x"e295a0". 
       01  box1-utf8-line-sep-right        pic x(03) value x"e295a3". 
       
       01  box2-utf8-line-horizontal       pic x(03) value x"e29480". 
       01  box2-utf8-line-vertical         pic x(03) value x"e29482". 
       01  box2-utf8-top-left              pic x(03) value x"e2948c". 
       01  box2-utf8-top-right             pic x(03) value x"e29490". 
       01  box2-utf8-bottom-left           pic x(03) value x"e29494". 
       01  box2-utf8-bottom-right          pic x(03) value x"e29498". 
       01  box2-utf8-line-sep-left         pic x(03) value x"e2949c". 
       01  box2-utf8-line-sep-right        pic x(03) value x"e294a4". 

       01  box-line-horizontal            pic x(03) value x"e29590". 
       01  box-line-vertical              pic x(03) value x"e29591". 
       01  box-top-left                   pic x(03) value x"e29594". 
       01  box-top-right                  pic x(03) value x"e29597". 
       01  box-bottom-left                pic x(03) value x"e2959a". 
       01  box-bottom-right               pic x(03) value x"e2959d". 
       01  box-line-sep-left              pic x(03) value x"e2959d". 
       01  box-line-sep-right             pic x(03) value x"e2959d". 

       01  OS PIC X(20) VALUE SPACES.
       01  WK-POS-X                       PIC 9(03).
       01  WK-POS-Y                       PIC 9(03).
       01  WK-KEY                         PIC X(01).

       LINKAGE SECTION.
       01  LK-TYPE                        PIC X(01) VALUE "B".
       01  LK-LINE-TYPE                   PIC 9(01) VALUE 2.
       01  LK-POS_X1                      PIC 9(03) VALUE 1.
       01  LK-POS_Y1                      PIC 9(03) VALUE 1.
       01  LK-POS_X2                      PIC 9(03) VALUE 80.
       01  LK-POS_Y2                      PIC 9(03) VALUE 24.
       01  LK-BK-COLOR                    PIC 9(03) VALUE 0.
       01  LK-TEXT-COLOR                  PIC 9(03) VALUE 7.

       PROCEDURE DIVISION USING LK-TYPE LK-LINE-TYPE LK-POS_X1 LK-POS_Y1 LK-POS_X2
                                LK-POS_Y2 LK-BK-COLOR LK-TEXT-COLOR.
       DISP-MENU-INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT OS FROM ENVIRONMENT "OS".
           
           IF  OS = "Windows_NT"
               IF LK-LINE-TYPE = 1
                      MOVE box1-dos-line-horizontal TO box-line-horizontal
                      MOVE box1-dos-line-vertical TO box-line-vertical
                      MOVE box1-dos-top-left TO box-top-left
                      MOVE box1-dos-top-right TO box-top-right
                      MOVE box1-dos-bottom-left TO box-bottom-left
                      MOVE box1-dos-bottom-right TO box-bottom-right
                      MOVE box1-dos-line-sep-left TO box-line-sep-left
                      MOVE box1-dos-line-sep-right TO box-line-sep-right
               ELSE
                      MOVE box2-dos-line-horizontal TO box-line-horizontal
                      MOVE box2-dos-line-vertical TO box-line-vertical
                      MOVE box2-dos-top-left TO box-top-left
                      MOVE box2-dos-top-right TO box-top-right
                      MOVE box2-dos-bottom-left TO box-bottom-left
                      MOVE box2-dos-bottom-right TO box-bottom-right
                      MOVE box2-dos-line-sep-left TO box-line-sep-left
                      MOVE box2-dos-line-sep-right TO box-line-sep-right
               END-IF       
               MOVE X'00' TO box-line-horizontal(2:1)
               MOVE X"00" TO box-line-vertical(2:1)
               MOVE X"00" TO box-top-left(2:1)
               MOVE X'00' TO box-top-right(2:1)
               MOVE X"00" TO box-bottom-left(2:1)
               MOVE X"00" TO box-bottom-right(2:1)
               MOVE X"00" TO box-line-sep-left(2:1)
               MOVE X"00" TO box-line-sep-right(2:1)
           ELSE
               *> linux
               MOVE "LINUX" TO OS
               IF LK-LINE-TYPE = 1
                      MOVE box1-utf8-line-horizontal TO box-line-horizontal
                      MOVE box1-utf8-line-vertical TO box-line-vertical
                      MOVE box1-utf8-top-left TO box-top-left
                      MOVE box1-utf8-top-right TO box-top-right
                      MOVE box1-utf8-bottom-left TO box-bottom-left
                      MOVE box1-utf8-bottom-right TO box-bottom-right
                      MOVE box1-utf8-line-sep-left TO box-line-sep-left
                      MOVE box1-utf8-line-sep-right TO box-line-sep-right
              ELSE
                      MOVE box2-utf8-line-horizontal TO box-line-horizontal
                      MOVE box2-utf8-line-vertical TO box-line-vertical
                      MOVE box2-utf8-top-left TO box-top-left
                      MOVE box2-utf8-top-right TO box-top-right
                      MOVE box2-utf8-bottom-left TO box-bottom-left
                      MOVE box2-utf8-bottom-right TO box-bottom-right
                      MOVE box2-utf8-line-sep-left TO box-line-sep-left
                      MOVE box2-utf8-line-sep-right TO box-line-sep-right
              END-IF 
           END-IF.

           IF LK-TYPE = "B" *> VERTICAL MENU - PULLDOWN
                  PERFORM  VARYING WK-POS-X FROM LK-POS_X1 BY 1 UNTIL  WK-POS-X > LK-POS_X2
                             display box-line-horizontal at line LK-POS_Y1 col WK-POS-X  with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                             display box-line-horizontal at line LK-POS_Y2 col WK-POS-X  with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  END-PERFORM                          
                  PERFORM  VARYING WK-POS-Y FROM LK-POS_Y1 BY 1 UNTIL  WK-POS-Y > LK-POS_Y2
                             display box-line-vertical at line WK-POS-Y col LK-POS_X1 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                             display box-line-vertical at line WK-POS-Y col LK-POS_X2 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  END-PERFORM                          
                  display box-top-left at line LK-POS_Y1 col LK-POS_X1 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  display box-top-right at line LK-POS_Y1 col LK-POS_X2 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  display box-bottom-left at line LK-POS_Y2 col LK-POS_X1  with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  display box-bottom-right at line LK-POS_Y2 col LK-POS_X2  with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
           END-IF
           IF LK-TYPE = "L" *> LINE
                  PERFORM  VARYING WK-POS-X FROM LK-POS_X1 BY 1 UNTIL  WK-POS-X > LK-POS_X2
                            display box-line-horizontal at line LK-POS_Y1 col WK-POS-X  with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  END-PERFORM                          
                  display box-line-sep-left at line LK-POS_Y1 col LK-POS_X1 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
                  display box-line-sep-right at line LK-POS_Y1 col LK-POS_X2 with  Background-Color LK-BK-COLOR Foreground-Color LK-TEXT-COLOR end-display
           END-IF.       
       DISP-MENU-FIM.
           EXIT PROGRAM.         
