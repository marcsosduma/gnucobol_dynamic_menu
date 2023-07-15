       >>SOURCE FORMAT IS FREE
*>----------------------------------------------------------------------
*> Purpose: Main screen example of dynamic menu
*>
*>----------------------------------------------------------------------
*>  CHANGES HISTORY
*>  REQUEST      DATE     AUTHOR  DESCRIPTION
*>  DYNAMIC MENU 06/15/23 DUMA    Automatic menu in GNU COBOL.
*>----------------------------------------------------------------------
IDENTIFICATION DIVISION.
PROGRAM-ID.    makemenu.
AUTHOR.        Duma.
ENVIRONMENT DIVISION.
CONFIGURATION    SECTION.
SOURCE-COMPUTER.      
             GNUCOBOL.
           OBJECT-COMPUTER.      
             GNUCOBOL
             CLASSIFICATION brazil.
SPECIAL-NAMES.
               LOCALE brazil "pt_BR.UTF-8".
               DECIMAL-POINT IS COMMA.
DATA DIVISION.
WORKING-STORAGE SECTION.
*> Variables -----------------------------------------------------------
77  WK-NUML                       PIC  9(003).
77  WK-NUMC                       PIC  9(003).
77  WK-IDX                        PIC  9(003) VALUE ZEROS.
77  WK-COUNT                      PIC  9(003) VALUE ZEROS.
77  WK-OS                         PIC  X(020) VALUE SPACES.
01  WK-KEY                        PIC X(01).
*> VARIABLES TO SAVE THE SCREEN
77  WK-SCR-NAME                   PIC  X(050) OCCURS 10 TIMES VALUE SPACES.
77  WK-SCR-SAVED-OK              BINARY-LONG.
77  WK-NUMBER-4-NAME              PIC  9(005).
01  WK-FILE-INFO.
    05  WK-File-Size-In-Bytes     PIC 9(18) COMP.
    05  WK-Mod-DD                 PIC 9(02) COMP.
    05  WK-Mod-MO                 PIC 9(02) COMP.
    05  WK-Mod-YYYY               PIC 9(04) COMP.
    05  WK-Mod-HH                 PIC 9(02) COMP.
    05  WK-Mod-MM                 PIC 9(02) COMP.
    05  WK-Mod-SS                 PIC 9(02) COMP.
    05  FILLER                    PIC 9(02) COMP.
*> Variaveis Montagem do Menu ------------------------------------------
01  WK-MENU-NUM-ITEM              PIC 9(03).
01  WK-AUX                        PIC 9(03).
01  WM-PARM.
    05 WM-ITENS occurs 20 times   PIC X(20).
01  WM-QTD-ITENS                  PIC 9(03).
01  WM-ITEM-SELECTED              PIC 9(03).
01  WM-COLORM-BACK                PIC 9(03).
01  WM-COLORM-TEXT                PIC 9(03).
01  WM-COLORM-SEL-BACK            PIC 9(03).
01  WM-COLORM-SEL-TEXT            PIC 9(03).
01  WM-ITEM-SEL-X                 PIC 9(03).
01  WM-ITEM-SEL-Y                 PIC 9(03).
01  WM-ITEM-MENU-TOP              PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-START-POS                  PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-POS_X                      PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-POS_Y                      PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-MENU-TYPE                  PIC X(1).
01  WM-CHOSEN-MENU                  PIC X(15) VALUE SPACES.
01  WK-IDX-MENU                   PIC  9(003) VALUE ZEROS.
01  WK-LEVEL-NAME                 PIC  9(001).
       88 WK-NIVEL1               VALUE 1. 
01  WK-LEVEL-NAME-1               PIC  X(015).
01  WK-SIZE-X-MENU                PIC  9(003) VALUE ZEROS.
01  WK-MENU-LOADED.
    03 LOAD-FATHER                   PIC X(15).
    03 LOAD-TYPE                  PIC X(01).
    03 LOAD-NAME                  PIC X(15).
    03 LOAD-ORDER                 PIC X(03).
    03 LOAD-DISPLAY               PIC 9(20).
    03 LOAD-ACTION                  PIC 9(15).  
*> Constantes das Cores ------------------------------------------------
01 black   constant as 0.
01 blue    constant as 1.
01 green   constant as 2.
01 cyan    constant as 3.
01 red     constant as 4.
01 magenta constant as 5.
01 yellow  constant as 6.
01 white   constant as 7.
*>----------------------------------------------------------------------
LINKAGE SECTION.
01  PARM.
    03  WK-MENU occurs 100 times.
       10 MENU-FATHER        PIC X(15).
       10 MENU-TYPE          PIC X(01).
       10 MENU-NAME          PIC X(15).
       10 MENU-ORDER         PIC X(03).
       10 MENU-DISPLAY       PIC 9(20).
       10 MENU-ACTION        PIC 9(15).  
01  QUANTITY-ITEMS           PIC 9(03).
01  POS_X                    PIC 9(03).
01  POS_Y                    PIC 9(03).
01  RET-ITEM-SELECTED        PIC 9(03).
01  SELECTED-ACTION          PIC X(15).
*>----------------------------------------------------------------------
PROCEDURE DIVISION USING PARM QUANTITY-ITEMS POS_X POS_Y 
                         RET-ITEM-SELECTED SELECTED-ACTION.
010-STARTING.
    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
    SET ENVIRONMENT 'ESCDELAY' TO '25'
    ACCEPT WK-OS FROM ENVIRONMENT "OS".

    ACCEPT WK-NUML FROM LINES
    ACCEPT WK-NUMC FROM COLUMNS

    MOVE WK-NUMC TO WK-SIZE-X-MENU.
    ADD -2 TO WK-SIZE-X-MENU.
    PERFORM 050-SHOW-MENU THRU 050-END-SHOW-MENU.   
010-END-STARTING.
    STOP RUN.
*>----------------------------------------------------------------------
*> ASSEMBLING THE MAIN MENU
*>----------------------------------------------------------------------
050-SHOW-MENU.
    MOVE 1 TO WM-ITEM-SEL-X WM-ITEM-SEL-Y *> RETURNS THE COLUMN OF THE SELECTED ITEM X AND Y
    MOVE 1 TO WM-ITEM-MENU-TOP(1)
    MOVE 1 TO WM-ITEM-SELECTED.           *> SELECTED ITEM - 0 ESC THE FIRST TIME AND 1
    MOVE WK-MENU(1) TO WK-MENU-LOADED
    MOVE LOAD-FATHER TO WK-LEVEL-NAME-1 WM-CHOSEN-MENU.
050-LOOP-MENU.
    IF WM-CHOSEN-MENU = WK-LEVEL-NAME-1
       MOVE 2 TO WM-POS_X(1)              *> COLUMN WHERE THE MAIN MENU WILL BEGIN
       MOVE 4 TO WM-POS_Y(1)              *> LINE WHERE THE MAIN MENU WILL BEGIN
       MOVE 1 TO WK-LEVEL-NAME
       MOVE WK-LEVEL-NAME-1 TO WK-SCR-NAME(WK-LEVEL-NAME)
    END-IF
    *> Finds the first menu item
    MOVE 1 TO WK-IDX
    MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    PERFORM UNTIL LOAD-FATHER=WM-CHOSEN-MENU
       ADD 1 TO WK-IDX
       MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    END-PERFORM
    MOVE WK-IDX TO WM-START-POS(WK-LEVEL-NAME)  *> Saves which is the first menu item
    MOVE LOAD-TYPE TO WM-MENU-TYPE              *> MENU TYPE - HORIZONTAL OR PULLDOWN
    *> Load the items to be displayed
    MOVE 1 TO WK-IDX-MENU 
    MOVE 0 TO WK-COUNT
    PERFORM UNTIL LOAD-FATHER<>WM-CHOSEN-MENU
       STRING " " LOAD-DISPLAY INTO WM-ITENS(WK-IDX-MENU)
       ADD 1 TO WK-IDX WK-IDX-MENU WK-COUNT
       MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    END-PERFORM    

    MOVE WK-COUNT TO WM-QTD-ITENS           *> NUMBER OF ITEMS
    IF WK-NIVEL1
        MOVE blue  TO  WM-COLORM-BACK       *> BACKGROUND COLOR
        MOVE white TO  WM-COLORM-TEXT       *> TEXT COLOR
        MOVE white TO  WM-COLORM-SEL-BACK   *> BACKGROUND COLOR
        MOVE blue  TO  WM-COLORM-SEL-TEXT   *> TEXT COLOR
    ELSE
        MOVE white  TO  WM-COLORM-BACK       *> BACKGROUND COLOR
        MOVE blue   TO  WM-COLORM-TEXT       *> TEXT COLOR
        MOVE green  TO  WM-COLORM-SEL-BACK   *> BACKGROUND COLOR
        MOVE white  TO  WM-COLORM-SEL-TEXT   *> TEXT COLOR
    END-IF
    MOVE WM-ITEM-MENU-TOP(WK-LEVEL-NAME) TO WM-ITEM-SELECTED     *> RETURNS THE INDEX OF THE SELECTED ITEM - 0 ESC

    CALL 'menu' USING        BY CONTENT WM-MENU-TYPE
                             BY REFERENCE WM-PARM 
                             BY CONTENT WM-QTD-ITENS 
                             BY CONTENT WM-POS_X(WK-LEVEL-NAME)
                             BY CONTENT WM-POS_Y(WK-LEVEL-NAME)
                             BY CONTENT WM-COLORM-BACK
                             BY CONTENT WM-COLORM-TEXT
                             BY CONTENT WM-COLORM-SEL-BACK
                             BY CONTENT WM-COLORM-SEL-TEXT
                             BY REFERENCE WM-ITEM-SELECTED
                             BY REFERENCE WM-ITEM-SEL-X
                             BY REFERENCE WM-ITEM-SEL-Y
                             BY CONTENT WK-SIZE-X-MENU
    END-CALL

    MOVE WK-MENU(WM-START-POS(WK-LEVEL-NAME)) TO WK-MENU-LOADED
    IF WM-ITEM-SELECTED=0
       IF WK-NIVEL1
            GO 050-END-SHOW-MENU
       ELSE
            IF WK-SCR-NAME(WK-LEVEL-NAME) <> WK-LEVEL-NAME-1
                PERFORM ScreenRestore THRU ScreenRestoreEx
            END-IF
            MOVE 1 TO WM-ITEM-SEL-X WM-ITEM-SEL-Y
            ADD -1 TO WK-LEVEL-NAME
            MOVE WM-ITEM-MENU-TOP(WK-LEVEL-NAME) TO WM-ITEM-SELECTED     *> RETURNS THE INDEX OF THE SELECTED ITEM - 0 ESC
            COMPUTE WK-IDX = WM-ITEM-SELECTED + WM-START-POS(WK-LEVEL-NAME) - 1 END-COMPUTE
            MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
            MOVE LOAD-FATHER TO WM-CHOSEN-MENU
            GO 050-LOOP-MENU
       END-IF
    ELSE
        MOVE WM-ITEM-SELECTED TO WM-ITEM-MENU-TOP(WK-LEVEL-NAME) *> RETURNS TO THE SAME PREVIOUS ITEM
        COMPUTE WK-IDX = WM-ITEM-SELECTED + WM-START-POS(WK-LEVEL-NAME) - 1 END-COMPUTE
        MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
        IF LOAD-ACTION="SAIR"
            GO 050-END-SHOW-MENU
        END-IF
        IF LOAD-ACTION = SPACES 
           MOVE LOAD-NAME TO WM-CHOSEN-MENU
           IF WM-ITEM-SELECTED <> 0
                ADD 1 TO WK-LEVEL-NAME 
                MOVE 1 TO WM-ITEM-MENU-TOP(WK-LEVEL-NAME)
                MOVE SPACES TO WK-SCR-NAME(WK-LEVEL-NAME)
                perform ScreenSave thru ScreenSaveEx
                MOVE WM-ITEM-SEL-X TO WM-POS_X(WK-LEVEL-NAME)
                IF WK-LEVEL-NAME>2
                    COMPUTE WM-POS_X(WK-LEVEL-NAME) = WM-ITEM-SEL-X + 10 END-COMPUTE
                    COMPUTE WK-AUX = WM-POS_X(WK-LEVEL-NAME) + 22 END-COMPUTE
                    IF WM-POS_X(WK-LEVEL-NAME) > WK-NUMC
                        COMPUTE WM-POS_X(WK-LEVEL-NAME) = WM-ITEM-SEL-X - 10 END-COMPUTE
                    END-IF
                END-IF
                COMPUTE WM-POS_Y(WK-LEVEL-NAME) = WM-ITEM-SEL-Y + 1 END-COMPUTE
           END-IF
        ELSE
            PERFORM 050-DISP-TRANS
            MOVE WM-ITEM-SELECTED TO RET-ITEM-SELECTED
            MOVE LOAD-DISPLAY TO SELECTED-ACTION            
            PERFORM until WK-LEVEL-NAME < 2
                IF WK-SCR-NAME(WK-LEVEL-NAME) <> WK-LEVEL-NAME-1
                    PERFORM ScreenRestore THRU ScreenRestoreEx
                END-IF
                ADD -1 TO WK-LEVEL-NAME
            END-PERFORM
            MOVE WK-LEVEL-NAME-1 TO WM-CHOSEN-MENU
        END-IF
    END-IF
    GO 050-LOOP-MENU.
050-END-SHOW-MENU.
    EXIT PROGRAM.
*>----------------------------------------------------------------------
050-DISP-TRANS.
    DISPLAY "SELECTED FUNCTIONALITY: "  at line 19 column 20 with HIGHLIGHT FOREGROUND-COLOR yellow LOAD-DISPLAY
    ACCEPT WK-KEY at line 19 column 79 with HIGHLIGHT FOREGROUND-COLOR yellow.
050-END-DISP-TRANS.
    EXIT.
*>----------------------------------------------------------------------
ScreenSave.
   *>move Z'MENUTESTE.SCR' to WK-SCR-NAME
    MOVE 1 TO WK-COUNT.
File-Exists.
   COMPUTE WK-NUMBER-4-NAME = FUNCTION RANDOM( FUNCTION SECONDS-PAST-MIDNIGHT ) * 100000  END-COMPUTE
   STRING "TMP-N" WK-LEVEL-NAME "-"  WK-NUMBER-4-NAME  ".SCR"  x"00" INTO WK-SCR-NAME(WK-LEVEL-NAME).
   Call 'CBL_CHECK_FILE_EXIST' USING WK-SCR-NAME(WK-LEVEL-NAME) WK-FILE-INFO end-call
   IF Return-Code = 0 and WK-COUNT<10
        GO File-Exists
   END-IF
   call static 'scr_dump' using by reference WK-SCR-NAME(WK-LEVEL-NAME) returning WK-SCR-SAVED-OK end-call.
ScreenSaveEx.

ScreenRestore.
   IF  WK-OS = "Windows_NT"
        call static 'clear' end-call
        call static 'refresh' end-call
   END-IF
   call static 'scr_restore' using by reference WK-SCR-NAME(WK-LEVEL-NAME) returning WK-SCR-SAVED-OK end-call
   CALL 'CBL_DELETE_FILE' USING WK-SCR-NAME(WK-LEVEL-NAME).
ScreenRestoreEx.
