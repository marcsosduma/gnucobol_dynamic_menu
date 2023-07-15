       >>SOURCE FORMAT IS FREE
*>----------------------------------------------------------------------
*> Purpose: Main screen dynamic menu example
*>
*>----------------------------------------------------------------------
*> DYNAMIC MENU 05/26/23 DUMA Automatic menu in GNU COBOL.
*>----------------------------------------------------------------------
IDENTIFICATION DIVISION.
PROGRAM-ID.    test.
AUTHOR.        Duma.
ENVIRONMENT DIVISION.
CONFIGURATION    SECTION.
SOURCE-COMPUTER. GNUCOBOL.
OBJECT-COMPUTER. 
             GNUCOBOL
             CLASSIFICATION brazil.
SPECIAL-NAMES.
               LOCALE brazil "pt_BR.UTF-8".
               DECIMAL-POINT IS COMMA.
INPUT-OUTPUT   SECTION.
FILE-CONTROL.
COPY "../cpy/user_se.cpy".
COPY "../cpy/user_profile_se.cpy".
COPY "../cpy/profile_se.cpy".
COPY "../cpy/menu_profile_se.cpy".
COPY "../cpy/menu_se.cpy".
*>----------------------------------------------------------------------

DATA DIVISION.
FILE SECTION.
COPY "../cpy/user_fd.cpy".
COPY "../cpy/user_profile_fd.cpy".
COPY "../cpy/profile_fd.cpy".
COPY "../cpy/menu_profile_fd.cpy".
COPY "../cpy/menu_fd.cpy".
*>----------------------------------------------------------------------

WORKING-STORAGE SECTION.
*> Variaveis -----------------------------------------------------------
77  WK-USER-NAME                  PIC  X(010) VALUE SPACES.
77  WK-OS                         PIC  X(020) VALUE SPACES.
77  WK-NUML                       PIC  9(003).
77  WK-NUMC                       PIC  9(003).
77  WK-BKG-COLOR                  PIC  9(001) VALUE 1.
77  WK-FRG-COLOR                  PIC  9(001) VALUE 6.
77  WK-CONF                       PIC  X(001) VALUE SPACES.
77  WK-READS                      PIC  9(008) VALUE ZEROS.
77  ST-USR                        PIC  9(002).
    88  FLT-OK                                VALUE ZEROS.
    88  FLT-CANCEL                           VALUE 99.
    88  FLT-NOT-EXIST                        VALUE 35.
77  ST-PFU                        PIC  9(002).
    88  FLT-OK                                VALUE ZEROS.
    88  FLT-CANCEL                           VALUE 99.
    88  FLT-NOT-EXIST                        VALUE 35.
77  ST-PRF                        PIC  9(002).
    88  FLT-OK                                VALUE ZEROS.
    88  FLT-CANCEL                           VALUE 99.
    88  FLT-NOT-EXIST                        VALUE 35.
77  ST-PFM                        PIC  9(002).
    88  FLT-OK                               VALUE ZEROS.
    88  FLT-CANCEL                           VALUE 99.
    88  FLT-NOT-EXIST                        VALUE 35.
77  ST-MNU                        PIC  9(002).
    88  FLT-OK                               VALUE ZEROS.
    88  FLT-CANCEL                           VALUE 99.
    88  FLT-NOT-EXIST                        VALUE 35.
77  WK-NUM-PROF                   PIC  9(003) VALUE ZEROS.
77  WK-PROFILES                   PIC  9(004) OCCURS 15 TIMES.
77  WK-LIN                        PIC  9(002) VALUE ZEROS.
77  WK-LIN-FIX                    PIC  9(002) VALUE ZEROS.
77  WK-IDX                        PIC  9(003) VALUE ZEROS.
77  WK-COUNT                      PIC  9(003) VALUE ZEROS.
*> VARIAVEIS PARA SALVAR A SCREEN
77  WK-SRC-NAME                   PIC  X(050) OCCURS 10 TIMES VALUE SPACES.
77  WK-SRC-SAVED-OK               BINARY-LONG.
77  WK-NUMBER-4-NAME              PIC  9(005).
*> VARIAVEIS PARA SALVAR A SCREEN
01  WK-USR-ITENS                  PIC  X(600) VALUE SPACES. *> ITENS DO MENU DO USR
01  WK-NUM-ITENS                  PIC  9(003) VALUE ZEROS.
01  WK-ITEN-PERM.
    03 WK-IT-PERM                 PIC  9(005) VALUE ZEROS.
    03 WK-SEP                     PIC  X VALUE "*".
01  WK-ITEN-FIND.
    03 WK-SEP1                    PIC  X VALUE "*".
    03 WK-IT-FIND                 PIC  9(005) VALUE ZEROS.
    03 WK-SEP2                    PIC  X VALUE "*".
01  WK-MODULE                     PIC  X(015) VALUE SPACES.
01  WK-NAME-LEVEL-1               PIC  X(015).
01  WK-MENU occurs 100 times.
    03 MENU-FATHER                PIC X(15).
    03 MENU-TYPE                  PIC X(01).
    03 MENU-NAME                  PIC X(15).
    03 MENU-ORDER                 PIC X(03).
    03 MENU-DISPLAY               PIC 9(20).
    03 MENU-ACTION                PIC 9(15).  
01  WK-MENU-LOADED.
    03 LOAD-FATHER                PIC X(15).
    03 LOAD-TYPE                  PIC X(01).
    03 LOAD-NAME                  PIC X(15).
    03 LOAD-ORDER                 PIC X(03).
    03 LOAD-DISPLAY               PIC 9(20).
    03 LOAD-ACTION                PIC 9(15).  

01  WK-MENU-NUM-ITEM              PIC 9(03).
01  WK-M-SUM-X                    PIC 9(03).
01  WK-M-SUM-Y                    PIC 9(03).
01  WK-AUX                        PIC 9(03).

01  WM-PARM.
    05 WM-ITENS occurs 20 times   PIC X(20).
01  WM-NUMBER-ITENS               PIC 9(03).
01  WM-ITEM-SELECTED              PIC 9(03).
01  WM-COLORM-BACK                PIC 9(03).
01  WM-COLORM-TEXT                PIC 9(03).
01  WM-COLORM-SEL-BACK            PIC 9(03).
01  WM-COLORM-SEL-TEXT            PIC 9(03).
01  WM-ITEM-SEL-X                 PIC 9(03).
01  WM-ITEM-SEL-Y                 PIC 9(03).
01  WM-ITEM-MENU-TOP              PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-POS-START                  PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-POS_X                      PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-POS_Y                      PIC 9(03) OCCURS 10 TIMES VALUE ZEROS.
01  WM-TYPE-MENU                  PIC X(1).
01  WM-QUESTION                   PIC X VALUE 'N'.
01  WM-CHOOSE-MENU                PIC X(15) VALUE SPACES.
01  WK-IDX-MENU                   PIC  9(003) VALUE ZEROS.
01  WK-WK-FRG-COLOR               PIC  9(003) VALUE 1.
01  WK-WK-BKG-COLOR               PIC  9(003) VALUE 6.
01  WK-LEVEL-MENU                 PIC  9(001).
       88 WK-LEVEL1               VALUE 1. 
*> Constantes-----------------------------------------------------------
01 black   constant as 0.
01 blue    constant as 1.
01 green   constant as 2.
01 cyan    constant as 3.
01 red     constant as 4.
01 magenta constant as 5.
01 yellow  constant as 6.
01 white   constant as 7.
01  WK-DATE-HOUR.
    03  WK-DATE.
        05  WK-YEAR               PIC  9(004) VALUE ZEROS.
        05  WK-MNT                PIC  9(002) VALUE ZEROS.
        05  WK-DAY                PIC  9(002) VALUE ZEROS.
    03  WK-HORA.
        05  WK-HOR                PIC  9(002) VALUE ZEROS.
        05  WK-MIN                PIC  9(002) VALUE ZEROS.
        05  WK-SEC                PIC  9(002) VALUE ZEROS.
        05  WK-CSE                PIC  9(002) VALUE ZEROS.
        05 DIF-GREENWICH          PIC  X(005) VALUE SPACES.
01  FLG-EOF                       PIC  X(001) VALUE "F".
    88  EOF                                   VALUE "T".
01  FLG-FAIL                      PIC  X(001) VALUE "F".
    88  FAIL                                  VALUE "T".
01  FLG-BREAK-LINE                PIC  X(001) VALUE "F".
    88  BREAK-LINE                            VALUE "T".
01  WK-SCR01-1                    PIC  X(057) VALUE
    "DYNAMIC MENU SYSTEM".
01  WK-SCR01-2.
    03  WK-SCR-DATA.
        05  WK-DAY-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-MNT-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-YEAR-T              PIC  X(004).
    03  FILLER                    PIC  X(002) VALUE SPACES.
    03  WK-SCR-HOUR.
        05  WK-HOR-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE ":".
        05  WK-MIN-T              PIC  X(002).
01  WK-SCR24-1                    PIC  X(009) VALUE "MESSAGE:".
01  WK-MSG                        PIC  X(080) VALUE SPACES.
01  WK-TAM-X-MENU                 PIC  9(003) VALUE ZEROS.
*> draw tho box
01  WK-BOX-TYPE-BOX               PIC X(01) VALUE "B".
01  WK-BOX-TYPE-SEP               PIC X(01) VALUE "L".
01  WK-BOX-TYPE-LINE              PIC 9(01) VALUE 2.
01  WK-BOX-POS_X1                 PIC 9(03) VALUE 1.
01  WK-BOX-POS_Y1                 PIC 9(03) VALUE 3.
01  WK-BOX-POS_X2                 PIC 9(03) VALUE 80.
01  WK-BOX-POS_Y2                 PIC 9(03) VALUE 22.
01  WK-BOX-WK-BKG-COLOR           PIC 9(03) VALUE black.
01  WK-BOX-COR-TEXTO              PIC 9(03) VALUE white.
01  WK-BOX-LINE-POS_Y1            PIC 9(03) VALUE 5.
01  WK-KEY                        PIC X(01).
01  file-info.
           05  File-Size-In-Bytes  PIC 9(18) COMP.
           05  Mod-DD              PIC 9(2) COMP.
           05  Mod-MO              PIC 9(2) COMP.
           05  Mod-YYYY            PIC 9(4) COMP.
           05  Mod-HH              PIC 9(2) COMP.
           05  Mod-MM              PIC 9(2) COMP.
           05  Mod-SS              PIC 9(2) COMP.
           05  FILLER              PIC 9(2) COMP.
01  WK-TMPDIR                      PIC X(256).

COPY screenio.

SCREEN SECTION.
01  SS-CLS.
    03  SS-FILLER01-1.
        05  BLANK SCREEN.
        05  LINE 01 COLUMN 01 PIC X(80)
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  SS-FILLER01-2.
        05  LINE 01 COLUMN 01 PIC X(57) FROM WK-SCR01-1
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  SS-FILLER01-3.
        05  LINE 01 COLUMN 64 PIC X(17) FROM WK-SCR01-2
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  SS-FILLER02-1 FOREGROUND-COLOR 2.
        05  LINE 02 COLUMN 01 VALUE "user:".
        05  COLUMN PLUS 2  PIC x(010) USING WK-USER-NAME. 

    03  SS-FILLER24-1.
        05  LINE 24 COLUMN 01 PIC X(17) FROM WK-SCR24-1
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR ERASE EOL.
01  SS-SCREEN-LOGIN.
    03  SS-LOGIN FOREGROUND-COLOR 2.
        05  LINE 02 COLUMN 01 VALUE "user:".
        05  COLUMN PLUS 2  PIC x(010) USING WK-USER-NAME. 

 01  SS-MSG.
     03  LINE 24 COLUMN 11 PIC X(70)
         BACKGROUND-COLOR WK-BKG-COLOR.
     03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
         05  LINE 24 COLUMN 11    PIC  X(070) FROM WK-MSG.
01  SS-CONF.
    03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
        05  LINE 24 COLUMN PLUS 44 PIC  X(001) USING WK-CONF.
01  SS-SCREEN-CON.
    03  LINE 01 COLUMN 01 PIC X(80)
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  LINE 01 COLUMN 01 PIC X(57) FROM WK-SCR01-1
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  SS-FILLER01-5.
        05  LINE 01 COLUMN 64 PIC X(17) FROM WK-SCR01-2
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR.
    03  SS-FILLER02-6 FOREGROUND-COLOR 2.
        05  LINE 02 COLUMN 01 VALUE "user:".
        05  COLUMN PLUS 2  PIC x(010) USING WK-USER-NAME. 
    03  SS-FILLER002-4 FOREGROUND-COLOR 2.
        05  LINE 06 COLUMN 03 VALUE "Select the Menu item...".
    03  SS-FILLER24-7.
        05  LINE 24 COLUMN 01 PIC X(17) FROM WK-SCR24-1
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR ERASE EOL.
    03  LINE 23 COLUMN 01 PIC X(080) VALUE 
            " F2 = Return | PAGE UP = Page Previous | PAGE DOWN = Page the Front | <ARROWS>"
            HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR WK-BKG-COLOR ERASE EOL.
    03  LINE 24 COLUMN 01 ERASE EOL.
    03  LINE 24 COLUMN 02 VALUE "MESSAGE: "
            FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR black.
    03  LINE 24 COLUMN 11 PIC  X(070) FROM WK-MSG
            FOREGROUND-COLOR WK-FRG-COLOR
            BACKGROUND-COLOR black.

*>----------------------------------------------------------------------
*>                    Main Program Module                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION.

000-START.
    
    PERFORM 010-INITIALIZES

    PERFORM 020-PROCESSING

    PERFORM 250-ENDS.

000-EXIT-START.
    EXIT.
*>----------------------------------------------------------------------
010-INITIALIZES.

    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
    SET ENVIRONMENT 'ESCDELAY' TO '25'
    ACCEPT WK-USER-NAME FROM USER NAME.
    *> move "admin" to WK-USER-NAME.
    ACCEPT WK-OS FROM ENVIRONMENT "OS".

    IF  WK-OS = "Windows_NT"
           *> windows
           CALL "SYSTEM" USING "chcp 437"
           CALL "SYSTEM" USING "mode con: lines=24 cols=80"
    ELSE
           *> linux
           CALL "SYSTEM" USING "resize -s 24 80"
    END-IF.
    ACCEPT WK-NUML FROM LINES
    ACCEPT WK-NUMC FROM COLUMNS
    ACCEPT WK-TMPDIR FROM ENVIRONMENT "TMPDIR"
    MOVE WK-NUMC TO WK-BOX-POS_X2.
    MOVE   WK-NUMC TO WK-BOX-POS_X2
    COMPUTE WK-BOX-POS_Y2 = WK-NUML - 2 END-COMPUTE
    MOVE WK-NUMC TO WK-TAM-X-MENU.
    ADD -2 TO WK-TAM-X-MENU.

    *> BROWSE THE MAIN MENU ONLY
    MOVE "PRINCIPAL" TO WK-MODULE.

    PERFORM 900-DATA-HORA.
010-END-INITIALIZES.
    EXIT.
*>----------------------------------------------------------------------
020-PROCESSING.
    DISPLAY SS-CLS
    ACCEPT SS-SCREEN-LOGIN
    IF COB-CRT-STATUS = COB-SCR-ESC
       GO 250-ENDS
    END-IF 
    PERFORM 030-FIND-USER

    IF WK-READS = 1
       PERFORM 040-LOCALIZE-PROFILE-USER
    END-IF 

    IF WK-NUM-PROF = 0
       MOVE "Access denied. log out? [Y/N]: " TO WK-MSG
    ELSE
       PERFORM 060-LOCALIZA-PROFILE-MENU
    END-IF

    IF WK-NUM-ITENS>0
       PERFORM 070-CARREGA-MENU
    else
       MOVE "Access denied. log out? [Y/N]: " TO WK-MSG
    END-IF
   
    IF WK-MENU-NUM-ITEM>0 
       DISPLAY SS-MSG
       MOVE SPACES TO WK-CONF
       ACCEPT SS-CONF
       PERFORM 025-MOSTRA-MENU THRU 025-END-MOSTRA-MENU
       MOVE "Exit the system? [Y/N]: " TO WK-MSG
    else
       MOVE "Access denied. Exit the system? [Y/N]: " TO WK-MSG
    END-IF

    DISPLAY SS-MSG
    MOVE SPACES TO WK-CONF
    ACCEPT SS-CONF
    IF WK-CONF = "Y" OR "y"
       STOP RUN
    END-IF
    go 020-PROCESSING.
020-EXIT-PROCESSING.
    EXIT.
*>----------------------------------------------------------------------
025-MOSTRA-MENU.
    *> MAIN MENU
    MOVE SPACES TO WK-MSG
    MOVE black TO WK-WK-FRG-COLOR
    MOVE WHITE TO WK-WK-BKG-COLOR
    DISPLAY SS-CLS
    DISPLAY SS-SCREEN-CON
    CALL 'makebox' using BY REFERENCE WK-BOX-TYPE-BOX      *> box
                         BY REFERENCE WK-BOX-TYPE-LINE    *> LINE simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2
                         BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                         BY REFERENCE WK-BOX-WK-BKG-COLOR     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL
    CALL 'makebox' using BY REFERENCE WK-BOX-TYPE-SEP      *> Lina de separACTION
                         BY REFERENCE WK-BOX-TYPE-LINE    *> LINE simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-LINE-POS_Y1  *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2 
                         BY REFERENCE WK-NUML              *> lin 2 (nao usado no separador)
                         BY REFERENCE WK-BOX-WK-BKG-COLOR     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL
    MOVE 1 TO WM-ITEM-SEL-X WM-ITEM-SEL-Y *> RETORNA A COLUNA DO ITEM SELECIONADO X E Y
    MOVE 1 TO WM-ITEM-MENU-TOP(1)
    MOVE 1 TO WM-ITEM-SELECTED       *> RETORNA O INDICE DO ITEM SELECIONADO - 0 ESC NA PRIMEIRA VEZ E 1
    MOVE WK-MENU(1) TO WK-MENU-LOADED
    MOVE LOAD-FATHER TO WK-NAME-LEVEL-1 WM-CHOOSE-MENU.
025-LOOP-MENU.
    IF WM-CHOOSE-MENU = WK-NAME-LEVEL-1
       MOVE 2 TO WM-POS_X(1)             *> COLUNA ONDE INICIARA O MENU
       MOVE 4 TO WM-POS_Y(1)             *> LINE ONDE INICIARA O MENU
       MOVE 1 TO WK-LEVEL-MENU
       MOVE WK-NAME-LEVEL-1 TO WK-SRC-NAME(WK-LEVEL-MENU)
    END-IF
    *> Encontra o primeiro item do menu
    MOVE 1 TO WK-IDX
    MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    PERFORM UNTIL LOAD-FATHER=WM-CHOOSE-MENU
       ADD 1 TO WK-IDX
       MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    END-PERFORM
    MOVE WK-IDX TO WM-POS-START(WK-LEVEL-MENU)  *> Salva qual e o primeiro item do menu
    MOVE LOAD-TYPE TO WM-TYPE-MENU               *> TYPE DO MENU - HORIZONTAL OU PULLDOWN
    *> Carrega os itens que serao apresentados
    MOVE 1 TO WK-IDX-MENU 
    MOVE 0 TO WK-COUNT
    PERFORM UNTIL LOAD-FATHER<>WM-CHOOSE-MENU
       STRING " " LOAD-DISPLAY INTO WM-ITENS(WK-IDX-MENU)
       ADD 1 TO WK-IDX WK-IDX-MENU WK-COUNT
       MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
    END-PERFORM    

    MOVE WK-COUNT TO WM-NUMBER-ITENS       *> NUMERO DE ITENS
    IF WK-LEVEL1
        MOVE blue  TO  WM-COLORM-BACK       *> COR DE FUNDO
        MOVE white TO  WM-COLORM-TEXT       *> COR DO TEXTO
        MOVE white TO  WM-COLORM-SEL-BACK   *> COR DE FUNDO
        MOVE blue  TO  WM-COLORM-SEL-TEXT   *> COR DO TEXTO
    ELSE
        MOVE white  TO  WM-COLORM-BACK       *> COR DE FUNDO
        MOVE blue   TO  WM-COLORM-TEXT       *> COR DO TEXTO
        MOVE green  TO  WM-COLORM-SEL-BACK   *> COR DE FUNDO
        MOVE white  TO  WM-COLORM-SEL-TEXT   *> COR DO TEXTO
    END-IF
    MOVE WM-ITEM-MENU-TOP(WK-LEVEL-MENU) TO WM-ITEM-SELECTED     *> RETORNA O INDICE DO ITEM SELECIONADO - 0 ESC

    CALL 'menu' USING        BY CONTENT WM-TYPE-MENU
                             BY REFERENCE WM-PARM 
                             BY CONTENT WM-NUMBER-ITENS 
                             BY CONTENT WM-POS_X(WK-LEVEL-MENU)
                             BY CONTENT WM-POS_Y(WK-LEVEL-MENU)
                             BY CONTENT WM-COLORM-BACK
                             BY CONTENT WM-COLORM-TEXT
                             BY CONTENT WM-COLORM-SEL-BACK
                             BY CONTENT WM-COLORM-SEL-TEXT
                             BY REFERENCE WM-ITEM-SELECTED
                             BY REFERENCE WM-ITEM-SEL-X
                             BY REFERENCE WM-ITEM-SEL-Y
                             BY CONTENT WK-TAM-X-MENU
    END-CALL

    MOVE WK-MENU(WM-POS-START(WK-LEVEL-MENU)) TO WK-MENU-LOADED
    IF WM-ITEM-SELECTED=0
       IF WK-LEVEL1
            GO 025-END-MOSTRA-MENU
       ELSE
            IF WK-SRC-NAME(WK-LEVEL-MENU) <> WK-NAME-LEVEL-1
                PERFORM ScreenRestore THRU ScreenRestoreEx
            END-IF
            MOVE 1 TO WM-ITEM-SEL-X WM-ITEM-SEL-Y
            ADD -1 TO WK-LEVEL-MENU
            MOVE WM-ITEM-MENU-TOP(WK-LEVEL-MENU) TO WM-ITEM-SELECTED     *> RETORNA O INDICE DO ITEM SELECIONADO - 0 ESC
            COMPUTE WK-IDX = WM-ITEM-SELECTED + WM-POS-START(WK-LEVEL-MENU) - 1 END-COMPUTE
            MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
            MOVE LOAD-FATHER TO WM-CHOOSE-MENU
            GO 025-LOOP-MENU
       END-IF
    ELSE
        MOVE WM-ITEM-SELECTED TO WM-ITEM-MENU-TOP(WK-LEVEL-MENU) *> RETORNA PARA O MESMO ITEM ANTERIOR
        COMPUTE WK-IDX = WM-ITEM-SELECTED + WM-POS-START(WK-LEVEL-MENU) - 1 END-COMPUTE
        MOVE WK-MENU(WK-IDX) TO WK-MENU-LOADED
        IF LOAD-ACTION="SAIR"
            GO 025-END-MOSTRA-MENU
        END-IF
        IF LOAD-ACTION = SPACES 
           MOVE LOAD-NAME TO WM-CHOOSE-MENU
           IF WM-ITEM-SELECTED <> 0
                ADD 1 TO WK-LEVEL-MENU 
                MOVE 1 TO WM-ITEM-MENU-TOP(WK-LEVEL-MENU)
                MOVE SPACES TO WK-SRC-NAME(WK-LEVEL-MENU)
                perform ScreenSave thru ScreenSaveEx
                MOVE WM-ITEM-SEL-X TO WM-POS_X(WK-LEVEL-MENU)
                IF WK-LEVEL-MENU>2
                    COMPUTE WM-POS_X(WK-LEVEL-MENU) = WM-ITEM-SEL-X + 10 END-COMPUTE
                    COMPUTE WK-AUX = WM-POS_X(WK-LEVEL-MENU) + 22 END-COMPUTE
                    IF WM-POS_X(WK-LEVEL-MENU) > WK-NUMC
                        COMPUTE WM-POS_X(WK-LEVEL-MENU) = WM-ITEM-SEL-X - 10 END-COMPUTE
                    END-IF
                END-IF
                COMPUTE WM-POS_Y(WK-LEVEL-MENU) = WM-ITEM-SEL-Y + 1 END-COMPUTE
           END-IF
        ELSE
            PERFORM 027-DISP-TRANS
            PERFORM until WK-LEVEL-MENU < 2
                IF WK-SRC-NAME(WK-LEVEL-MENU) <> WK-NAME-LEVEL-1
                    PERFORM ScreenRestore THRU ScreenRestoreEx
                END-IF
                ADD -1 TO WK-LEVEL-MENU
            END-PERFORM
            MOVE WK-NAME-LEVEL-1 TO WM-CHOOSE-MENU
        END-IF
    END-IF
    GO 025-LOOP-MENU.
025-END-MOSTRA-MENU.
    EXIT.
027-DISP-TRANS.
    DISPLAY "SELECTED FUNCTIONALITY: "  at line 19 column 20 with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR LOAD-DISPLAY
    ACCEPT WK-KEY at line 19 column 79 with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR.
027-END-DISP-TRANS.
    EXIT.
*>----------------------------------------------------------------------
030-FIND-USER.
    OPEN INPUT FD-USER 
    INITIALIZE REC-USR
    MOVE 0 TO  WK-READS
    MOVE WK-USER-NAME TO U-LOGIN
    DISPLAY "VALIDATING USER..."  at line 04 column 01 with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR
    START FD-USER
       KEY IS = KEY2-USR
       INVALID KEY
           DISPLAY "USER NOT FOUND: " line 05 column 01 WK-USER-NAME 
       NOT INVALID KEY
           READ FD-USER NEXT
           DISPLAY "USER FOUND:"  line 05 column 01 WK-USER-NAME
           ADD 1 TO WK-READS
    END-START
    CLOSE FD-USER.
030-END-FIND-USER.
    EXIT.
*>----------------------------------------------------------------------
040-LOCALIZE-PROFILE-USER.
    OPEN INPUT FD-PFU
    INITIALIZE REC-PFU
    MOVE 0 TO  WK-READS WK-NUM-PROF
    MOVE U-ID-USR TO PFU-ID-USR
    MOVE 7 TO WK-LIN
    DISPLAY "SEARCHING FOR USER PROFILES..." with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR at line 06 column 01
    START FD-PFU
       KEY IS >= KEY1-PFU
       INVALID KEY           
           DISPLAY "NO PROFILES FOUND" line WK-LIN column 01
       NOT INVALID KEY
           MOVE "F" TO FLG-EOF
           PERFORM UNTIL EOF            
               READ FD-PFU NEXT
                 AT END  
                    MOVE "T" TO FLG-EOF
                 NOT AT END
                   IF U-ID-USR = PFU-ID-USR
                       DISPLAY "PROFILE FOUND: "  line WK-LIN column 01
                       ADD 1 TO WK-READS
                       ADD 1 TO WK-NUM-PROF
                       MOVE PFU-ID-PRF TO WK-PROFILES(WK-NUM-PROF)
                       PERFORM 050-LOCALIZA-PROFILE
                       ADD 1 TO WK-LIN
                   ELSE
                       MOVE "T" TO FLG-EOF
                       IF WK-READS=0
                           DISPLAY "NO PROFILE FOUND" line WK-LIN column 01
                           CLOSE FD-PFU
                           GO 040-END-LOCALIZE-PROFILE-USER
                       END-IF                        
                   END-IF
               END-READ
           END-PERFORM
    END-START
    CLOSE FD-PFU.
040-END-LOCALIZE-PROFILE-USER.
    EXIT.
*>----------------------------------------------------------------------
050-LOCALIZA-PROFILE.
    OPEN INPUT FD-PRF 
    INITIALIZE REC-PRF
    MOVE PFU-ID-PRF TO PRF-ID
    START FD-PRF
       KEY IS = KEY1-PRF
       INVALID KEY
           DISPLAY "PROFILE NOT FOUND" AT line WK-LIN column 19 
       NOT INVALID KEY
           READ FD-PRF NEXT
           DISPLAY PRF-NAME AT line WK-LIN column 19 
    END-START
    CLOSE FD-PRF.
050-END-LOCALIZA-PROFILE.
    EXIT.
*>----------------------------------------------------------------------
060-LOCALIZA-PROFILE-MENU.
    OPEN INPUT FD-PFM
    DISPLAY "LOOKING FOR USER FEATURES..." with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR at line WK-LIN column 01
    ADD 1 TO WK-LIN
    MOVE WK-LIN TO WK-LIN-FIX
    MOVE "*" TO WK-USR-ITENS
    MOVE 0 TO WK-NUM-ITENS
    PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > WK-NUM-PROF
       INITIALIZE REC-PFM
       MOVE 0 TO  WK-READS
       MOVE WK-PROFILES(WK-IDX) TO PFM-ID-PRF
       START FD-PFM
              KEY IS >= PFM-ID-PRF
              INVALID KEY
                  ADD 1 TO WK-LIN           
                  DISPLAY "NO USER FEATURES " line WK-LIN-FIX column 01 WK-PROFILES(WK-IDX) 
              NOT INVALID KEY
                  MOVE "F" TO FLG-EOF
                  PERFORM UNTIL EOF            
                      READ FD-PFM NEXT
                        AT END  
                           MOVE "T" TO FLG-EOF
                        NOT AT END
                          IF WK-PROFILES(WK-IDX) = PFM-ID-PRF
                              MOVE 0 TO WK-COUNT
                              MOVE PFM-ID-MENU TO WK-IT-FIND
                              INSPECT WK-USR-ITENS TALLYING WK-COUNT FOR ALL WK-ITEN-FIND
                              IF WK-COUNT = 0
                                  ADD 1 TO WK-READS
                                  ADD 1 TO WK-NUM-ITENS
                                  MOVE PFM-ID-MENU TO WK-IT-PERM
                                  STRING  WK-USR-ITENS WK-ITEN-PERM DELIMITED BY SPACE INTO WK-USR-ITENS
                                  DISPLAY "USER FEATURES: "  line WK-LIN-FIX column 01 WK-READS " " WK-USR-ITENS
                             END-IF
                          ELSE
                              MOVE "T" TO FLG-EOF
                          END-IF
                      END-READ
                  END-PERFORM
           END-START
    END-PERFORM 
    CLOSE FD-PFM.
060-END-LOCALIZA-PROFILE-MENU.
    EXIT.
*>----------------------------------------------------------------------
070-CARREGA-MENU.
    OPEN INPUT FD-MENU
    ADD 1 TO WK-LIN
    DISPLAY "READING ITEMS FROM THE USER MENU..." with HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR at line WK-LIN column 01
    ADD 1 TO WK-LIN
    MOVE WK-LIN TO WK-LIN-FIX
    INITIALIZE REC-MENU
    MOVE 0 TO  WK-READS
    MOVE 0 TO WK-MENU-NUM-ITEM
    MOVE WK-MODULE TO M-MODULE 
    MOVE SPACES TO M-ITEM-FATHER
    START FD-MENU
       KEY IS >= KEY2-MENU
       INVALID KEY
           ADD 1 TO WK-LIN           
           DISPLAY "NO MENU ITEM FOUND " line WK-LIN-FIX column 01
       NOT INVALID KEY
           MOVE "F" TO FLG-EOF
           PERFORM UNTIL EOF            
               READ FD-MENU NEXT
                 AT END  
                    MOVE "T" TO FLG-EOF
                 NOT AT END
                    IF WK-MODULE = M-MODULE
                        MOVE 0 TO WK-COUNT
                        MOVE M-ID TO WK-IT-FIND
                        INSPECT WK-USR-ITENS TALLYING WK-COUNT FOR ALL WK-ITEN-FIND
                        IF WK-COUNT > 0
                            ADD 1 TO WK-READS
                            MOVE PFM-ID-MENU TO WK-IT-PERM
                            ADD 1 TO WK-MENU-NUM-ITEM
                            MOVE M-ITEM-FATHER TO LOAD-FATHER
                            MOVE M-TYPE TO LOAD-TYPE
                            MOVE M-NAME TO LOAD-NAME
                            MOVE M-ORDER TO LOAD-ORDER
                            MOVE M-DISPLAY-TEXT TO LOAD-DISPLAY
                            MOVE M-ACTION TO LOAD-ACTION
                            MOVE WK-MENU-LOADED TO WK-MENU(WK-MENU-NUM-ITEM)
                        END-IF
                    ELSE
                        MOVE "T" TO FLG-EOF
                    END-IF
               END-READ
           END-PERFORM
           DISPLAY "FOUND MENU ITEMS: "  line WK-LIN-FIX column 01 WK-MENU-NUM-ITEM
           COMPUTE WK-COUNT = WK-MENU-NUM-ITEM + 1
           MOVE SPACES TO WK-MENU(WK-COUNT)
    END-START
    CLOSE FD-MENU.
070-END-CARREGA-MENU.
    EXIT.
*>----------------------------------------------------------------------
250-ENDS.
    PERFORM 900-DATA-HORA
    DISPLAY SS-FILLER02-1
    DISPLAY WK-SCR01-2 LINE 02 COLUMN 64 HIGHLIGHT FOREGROUND-COLOR WK-FRG-COLOR BACKGROUND-COLOR WK-BKG-COLOR
    MOVE "*** END OF PROCESSING ***" TO WK-MSG
    DISPLAY SS-MSG
    STOP RUN.
250-EXIT-ENDS.
    EXIT.
*>----------------------------------------------------------------------
900-DATA-HORA.
    ACCEPT WK-HORA FROM TIME
    MOVE   WK-HOR  TO   WK-HOR-T 
    MOVE   WK-MIN  TO   WK-MIN-T    
    ACCEPT WK-DATE FROM DATE YYYYMMDD
    MOVE   WK-YEAR  TO   WK-YEAR-T
    MOVE   WK-MNT  TO   WK-MNT-T
    MOVE   WK-DAY  TO   WK-DAY-T.
900-EXIT-DATA-HORA.
    EXIT.
*>----------------------------------------------------------------------
ScreenSave.
   *>move Z'MENUTESTE.SCR' to WK-SRC-NAME
    MOVE 1 TO WK-COUNT.
File-Exists.
   COMPUTE WK-NUMBER-4-NAME = FUNCTION RANDOM( FUNCTION SECONDS-PAST-MIDNIGHT ) * 100000  END-COMPUTE
   STRING "TMP-N" WK-LEVEL-MENU "-"  WK-NUMBER-4-NAME  ".SCR"  x"00" INTO WK-SRC-NAME(WK-LEVEL-MENU).
   Call 'CBL_CHECK_FILE_EXIST' USING WK-SRC-NAME(WK-LEVEL-MENU) file-info end-call
   IF Return-Code = 0 and WK-COUNT<10
    GO File-Exists
   END-IF
   call static 'scr_dump' using by reference WK-SRC-NAME(WK-LEVEL-MENU) returning WK-SRC-SAVED-OK end-call.
ScreenSaveEx.

ScreenRestore.
   IF  WK-OS = "Windows_NT"
        call static 'clear' end-call
        call static 'refresh' end-call
   END-IF
   call static 'scr_restore' using by reference WK-SRC-NAME(WK-LEVEL-MENU) returning WK-SRC-SAVED-OK end-call
   CALL 'CBL_DELETE_FILE' USING WK-SRC-NAME(WK-LEVEL-MENU).
ScreenRestoreEx.
