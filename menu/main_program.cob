       >>SOURCE FORMAT IS FREE
*>****************************************************************
*> Author: MARCOS DUMA
*> Date: 23/04/2023
*> Purpose: MENU FOR GNUCOBOL PROGRAMS - LINUX AND WINDOWS
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID.    main_program.
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
DATA DIVISION.
WORKING-STORAGE SECTION.
*> Variables -----------------------------------------------------------
77 WK-VALID PIC X.
    88 WK-USER-YES VALUES ARE "Y" "y".
    88 WK-USER-NOT VALUES ARE "N".
01  WK-CONF                       PIC  X(001) VALUE SPACES.
*> Constants-----------------------------------------------------------
01 ct-black   constant as 0.
01 ct-blue    constant as 1.
01 ct-green   constant as 2.
01 ct-cyan    constant as 3.
01 ct-red     constant as 4.
01 ct-magenta constant as 5.
01 ct-yellow  constant as 6.
01 ct-white   constant as 7.
*> Screen Adjustment Variables -----------------------------------------
77 WS-MSG PIC X.
    88 IS-YES VALUES ARE "Y" "y".
77 WS-NUML                        PIC 999.
77 WS-NUMC                        PIC 999.
77 WS-STATUS                      PIC X(30).
77 WS-ERROR-MSG                   PIC X(80).
77 WS-USR-LOGGED                  PIC X(20).
77 WS-MODULE                      PIC  X(015).
*> Screen Colors -------------------------------------------------------
77  BACKG-COLOR                   PIC  9(001) VALUE 1.
77  FOREG-COLOR                   PIC  9(001) VALUE 6.
01 WS-SCR.
    05 FILLER                     PIC X(50) VALUE "DYNAMIC MENU EXAMPLE ".
    05 WS-OP                      PIC X(20) VALUE SPACES.
*> Environment variables -----------------------------------------------
77  WK-USER-NAME                  PIC  X(010) VALUE SPACES.
77  WK-OS                         PIC  X(020) VALUE SPACES.
77  WK-NUML                       PIC  9(003).
77  WK-NUMC                       PIC  9(003).
*> Menu variables ------------------------------------------------------
01  WK-ME.
   03 WK-MENU occurs 100 times.
       10 MENU-FATHER             PIC X(15).
       10 MENU-TYPE               PIC X(01).
       10 MENU-NAME               PIC X(15).
       10 MENU-ORDER              PIC X(03).
       10 MENU-DISPLAY            PIC 9(20).
       10 MENU-ACTION             PIC 9(15).  
01  WK-MENU-NUM-ITEM              PIC 9(03).
01  WK-POS-X                      PIC 9(03).
01  WK-POS-Y                      PIC 9(03).
01  WK-SELECTED-ITEM              PIC 9(03).
01  WK-SELECTED-ACTION            PIC X(15).
*> Draw the box --------------------------------------------------------
01  WK-BOX-TYPE-BOX               PIC X(01) VALUE "B".
01  WK-BOX-TYPE-SEP               PIC X(01) VALUE "L".
01  WK-BOX-TYPE-LINE              PIC 9(01) VALUE 2.
01  WK-BOX-POS_X1                 PIC 9(03) VALUE 1.
01  WK-BOX-POS_Y1                 PIC 9(03) VALUE 3.
01  WK-BOX-POS_X2                 PIC 9(03) VALUE 80.
01  WK-BOX-POS_Y2                 PIC 9(03) VALUE 22.
01  WK-BOX-BACKG-COLOR            PIC 9(03) VALUE ct-black.
01  WK-BOX-COLOR-TEXT            PIC 9(03) VALUE ct-white.
01  WK-BOX-LINE-POS_Y1            PIC 9(03) VALUE 5.

*> System Date and Time ------------------------------------------------
01  WK-DATE.
    03  WK-YEAR                   PIC  9(002) VALUE ZEROS.
    03  WK-MNT                    PIC  9(002) VALUE ZEROS.
    03  WK-DAY                    PIC  9(002) VALUE ZEROS.
01  WK-TIME.
    03  WK-HOR                    PIC  9(002) VALUE ZEROS.
    03  WK-MIN                    PIC  9(002) VALUE ZEROS.
    03  WK-SEG                    PIC  9(002) VALUE ZEROS.
    03  WK-CSE                    PIC  9(002) VALUE ZEROS.
01  WK-SCR01-2.
    03  WK-SCR-TIME.
        05  WK-DAY-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-MNT-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-YEAR-T             PIC  X(004).
    03  FILLER                    PIC  X(002) VALUE SPACES.
    03  WK-SCR-TIME.
        05  WK-HOR-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE ":".
        05  WK-MIN-T              PIC  X(002).
*> Secao das Telas------------------------------------------------------
COPY screenio.

SCREEN SECTION.
01 SS-CLS.
       05 SS-FILLER.
           10 BLANK SCREEN.
           10 LINE 01 COLUMN 01 ERASE EOL
              BACKGROUND-COLOR BACKG-COLOR.
           10 LINE WS-NUML COLUMN 01 ERASE EOL
              BACKGROUND-COLOR BACKG-COLOR.
       05 SS-HEAD.
           10 LINE 01 COLUMN 02 PIC X(31) FROM WS-SCR
              HIGHLIGHT FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR BACKG-COLOR.
           10 LINE 01 COLUMN 59 PIC X(20) FROM WS-USR-LOGGED
              HIGHLIGHT FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR BACKG-COLOR.
       05 SS-STATUS.
           10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
              FROM WS-STATUS HIGHLIGHT
              FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR BACKG-COLOR.
01 SS-LOGIN-SCR.
    05 SS-CHAVE FOREGROUND-COLOR 2.
        10 LINE 10 COLUMN 10 VALUE "USER......:"
              HIGHLIGHT FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR ct-black.
        10 COLUMN PLUS 2 PIC X(20) USING WS-USR-LOGGED
              BACKGROUND-COLOR ct-black.              
        10 LINE 11 COLUMN 10 VALUE "MODULE...:"
              HIGHLIGHT FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR ct-black.
        10 COLUMN PLUS 2 PIC X(20) USING WS-MODULE.
01 SS-END.
       05 SS-FILLER.
           10 LINE 02 COLUMN 01 ERASE EOL
              BACKGROUND-COLOR BACKG-COLOR.
           10 LINE 02 COLUMN 02 ERASE EOL PIC X(31) FROM WK-SCR01-2
              HIGHLIGHT FOREGROUND-COLOR FOREG-COLOR
              BACKGROUND-COLOR BACKG-COLOR.
01 SS-MSG.
    05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
        10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-ERROR-MSG BELL.
        10 COLUMN PLUS 2 TO WS-MSG.
01  SS-CONF.
    03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
        05  LINE WS-NUML COLUMN PLUS 70 PIC  X(001) USING WK-CONF.

*>----------------------------------------------------------------------
*>                    Modulo Principal do Programa                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION.

000-START.
    
    PERFORM 010-STARTING

    PERFORM 020-PROCESSING

    PERFORM 900-ENDS.

000-EXIT-START.
    EXIT.
*>----------------------------------------------------------------------
010-STARTING.

    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
    SET ENVIRONMENT 'ESCDELAY' TO '25'
    ACCEPT WK-USER-NAME FROM USER NAME
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
    MOVE   WK-NUML TO WS-NUML
    *> MENU POSITION
    MOVE   4 TO WK-POS-Y
    MOVE   WK-NUMC TO WK-POS-Y
    MOVE   WK-NUMC TO WK-BOX-POS_X2
    COMPUTE WK-BOX-POS_Y2 = WK-NUML - 2 END-COMPUTE
    ADD -1 TO WK-POS-Y

    *> SUGGEST THE MAIN MENU
    MOVE "MAIN" TO WS-MODULE
    
    PERFORM 950-DATA-HORA.   
010-END-STARTING.
    EXIT.
*>----------------------------------------------------------------------
020-PROCESSING.
    PERFORM 030-LOGIN

    MOVE "Confirm end of processing? [Y/N]: " TO WS-ERROR-MSG
    DISPLAY SS-MSG
    MOVE SPACES TO WK-CONF
    PERFORM UNTIL (WK-CONF = "Y" OR "y" OR "N" OR "n")
       ACCEPT SS-CONF
    END-PERFORM
    IF WK-CONF = "N" OR WK-CONF = "n"
        GO 020-PROCESSING
    END-IF.
020-EXIT-PROCESSING.
    EXIT.
*>----------------------------------------------------------------------
030-LOGIN.
    DISPLAY SS-CLS
    ACCEPT  SS-LOGIN-SCR.
    DISPLAY SS-CLS
    CALL "validate_usr" USING BY REFERENCE WS-USR-LOGGED
                              BY REFERENCE WS-MODULE
                              BY REFERENCE WK-VALID
                              BY REFERENCE WK-ME
                              BY REFERENCE WK-MENU-NUM-ITEM
    END-CALL.

    DISPLAY SS-CLS
    PERFORM 100-DESENHA-BOX
    IF WK-USER-YES
       CALL "makemenu" USING BY REFERENCE WK-ME
                             BY REFERENCE WS-MODULE
                             BY REFERENCE WK-MENU-NUM-ITEM
                             BY REFERENCE WK-POS-X
                             BY REFERENCE WK-POS-Y
                             BY REFERENCE WK-SELECTED-ITEM
                             BY REFERENCE WK-SELECTED-ACTION
    END-IF.
030-END-LOGIN.
    EXIT.
*>----------------------------------------------------------------------
100-DESENHA-BOX.    
    CALL 'makebox' using BY REFERENCE WK-BOX-TYPE-BOX      *> box
                         BY REFERENCE WK-BOX-TYPE-LINE     *> single line
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2
                         BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                         BY REFERENCE WK-BOX-BACKG-COLOR   *> background color
                         BY REFERENCE WK-BOX-COLOR-TEXT   *> foreground color
    END-CALL
    CALL 'makebox' using BY REFERENCE WK-BOX-TYPE-SEP      *> separation line
                         BY REFERENCE WK-BOX-TYPE-LINE     *> single line
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-LINE-POS_Y1   *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2 
                         BY REFERENCE WK-NUML              *> lin 2 (not used)
                         BY REFERENCE WK-BOX-BACKG-COLOR   *> background color
                         BY REFERENCE WK-BOX-COLOR-TEXT    *> foreground color
    END-CALL.
100-END-DESENHA-BOX.    
    EXIT.
*>----------------------------------------------------------------------
900-ENDS.
    PERFORM 950-DATA-HORA
    DISPLAY SS-END
    MOVE "*** END DO PROCESSING ***" TO WS-ERROR-MSG
    DISPLAY SS-MSG
    STOP RUN.
900-EXIT-ENDS.
    EXIT.
*>----------------------------------------------------------------------
950-DATA-HORA.
    ACCEPT WK-TIME FROM TIME
    ACCEPT WK-DATE FROM DATE
   
   *>Data e hora formatada
    MOVE   WK-HOR  TO   WK-HOR-T 
    MOVE   WK-MIN  TO   WK-MIN-T    
    MOVE   WK-YEAR  TO   WK-YEAR-T
    MOVE   WK-MNT  TO   WK-MNT-T
    MOVE   WK-DAY  TO   WK-DAY-T.
950-EXIT-DATA-HORA.
    EXIT.
*>----------------------------------------------------------------------
