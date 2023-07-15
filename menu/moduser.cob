       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERS.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
              SOURCE-COMPUTER.      
                    GNUCOBOL.
                  OBJECT-COMPUTER.      
                    GNUCOBOL
                    CLASSIFICATION brazil.
              SPECIAL-NAMES.
	       LOCALE brazil "pt_BR.UTF8".
           DECIMAL-POINT IS COMMA.    
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            COPY "../cpy/user_se.cpy".

       DATA DIVISION.
       FILE SECTION.
            COPY "../cpy/user_fd.cpy".

       WORKING-STORAGE SECTION.

       01 WS-MODULO.
           05 FILLER PIC X(29) VALUE "USER ADMINISTRATION MODULE - ".
           05 WS-OP PIC X(20) VALUE SPACES.

       77 WS-OPTION PIC X.
           88 E-INSERT   VALUE IS "1".
           88 E-CONSULT  VALUE IS "2".
           88 E-UPDATE   VALUE IS "3".
           88 E-REMOVE   VALUE IS "4".
           88 E-END      VALUE IS "X" "x".
       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".
       77 ST-USR                        PIC  9(002).
           88  FS-OK                     VALUE ZEROS.
           88  FS-CANCEL                 VALUE 99.
           88  FS-NOT-EXIST              VALUE 35.
       77 WS-NUML PIC 999.
       77 WS-NUMC PIC 999.
       77 WS-BACK-COLOR PIC 9 VALUE 1.
       77 WS-FOR-COLOR  PIC 9 VALUE 6.

       77 WS-STATUS  PIC X(30).
       77 WS-MSGERROR PIC X(79).
       77 WS-ID-USR   PIC 9(04).
       *>  Colors ------------------------------------------------------
       01  BLACK                                     CONSTANT AS 0.
       01  BLUE                                      CONSTANT AS 1.
       01  GREEN                                     CONSTANT AS 2.
       01  CYAN                                      CONSTANT AS 3.
       01  RED                                       CONSTANT AS 4.
       01  MAGENTA                                   CONSTANT AS 5.
       01  YELLOW                                    CONSTANT AS 6.
       01  WHITE                                     CONSTANT AS 7.
       *> box variables ------------------------------------------------
       01  WK-BOX-TIPO-BOX               PIC X(01) VALUE "B".
       01  WK-BOX-TIPO-SEP               PIC X(01) VALUE "L".
       01  WK-BOX-TIPO-LINHA             PIC 9(01) VALUE 2.
       01  WK-BOX-POS_X1                 PIC 9(03) VALUE 1.
       01  WK-BOX-POS_Y1                 PIC 9(03) VALUE 3.
       01  WK-BOX-POS_X2                 PIC 9(03) VALUE 80.
       01  WK-BOX-POS_Y2                 PIC 9(03) VALUE 22.
       01  WK-BOX-COR-FUNDO              PIC 9(03) VALUE black.
       01  WK-BOX-COR-TEXTO              PIC 9(03) VALUE white.
       01  WK-BOX-LINHA-POS_Y1           PIC 9(03) VALUE 5.
       *>---------------------------------------------------------------

       COPY screenio.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR WS-BACK-COLOR.
               10 LINE 23 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR WS-BACK-COLOR.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(35) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR WS-FOR-COLOR
                  BACKGROUND-COLOR WS-BACK-COLOR.
           05 SS-STATUS.
               10 LINE 23 COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR WS-FOR-COLOR
                  BACKGROUND-COLOR WS-BACK-COLOR.

       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "1 - INSERT".
           05 LINE 08 COLUMN 15 VALUE "2 - CONSULT".
           05 LINE 09 COLUMN 15 VALUE "3 - UPDATE".
           05 LINE 10 COLUMN 15 VALUE "4 - REMOVE".
           05 LINE 11 COLUMN 15 VALUE "X - END".
           05 LINE 13 COLUMN 15 VALUE "OPTION: ".
           05 LINE 13 COL PLUS 1 USING WS-OPTION AUTO.

       01 SS-SCR-RECORD.
           05 SS-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE "ID:".
               10 COLUMN PLUS 2 PIC 9(04) USING U-ID-USR
                  BLANK WHEN ZEROS.
           05 SS-DATA.
               10 LINE 11 COLUMN 10 VALUE "LOGIN:".
               10 COLUMN PLUS 2 PIC X(10) USING U-LOGIN.
               10 LINE 12 COLUMN 10 VALUE "PASSWD:".
               10 COLUMN PLUS 2 PIC X(10) USING U-PASSWD NO-ECHO.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE 23 COLUMN 2 PIC X(78) FROM WS-MSGERROR.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION.
       001-INIT-PROC.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           CALL "SYSTEM" USING "chcp 437"
           CALL "SYSTEM" USING "mode con: lines=24 cols=80"
           ACCEPT WS-NUML FROM LINES
           ACCEPT WS-NUMC FROM COLUMNS
           PERFORM PROC-OPEN-FILES
           PERFORM UNTIL E-END
               MOVE "MENU" TO WS-OP
               MOVE "CHOOSE THE OPTION" TO WS-STATUS
               MOVE SPACES TO WS-OPTION
               DISPLAY SS-CLS
               PERFORM PROC-SHOW-BOX
               ACCEPT SS-MENU
               EVALUATE TRUE
                   WHEN E-INSERT
                       PERFORM INSERT THRU INSERT-END
                   WHEN E-CONSULT
                       PERFORM CONSULT THRU CONSULT-END
                   WHEN E-UPDATE
                       PERFORM PROC-UPDATE THRU PROC-UPDATE-END
                   WHEN E-REMOVE
                       PERFORM REMOVE THRU REMOVE-END
               END-EVALUATE
           END-PERFORM.
       001-INIT-PROC-END.
           CLOSE FD-USER.
           STOP RUN.
      *> -----------------------------------
       INSERT.
           MOVE "INSERT" TO WS-OP.
           MOVE "ESC TO GO BACK" TO WS-STATUS.
           DISPLAY SS-CLS.
           PERFORM PROC-SHOW-BOX.
           MOVE SPACES TO REC-USR.
       INSERT-LOOP.
           ACCEPT SS-SCR-RECORD.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO INSERT-END
           END-IF
           IF U-LOGIN EQUAL SPACES OR U-PASSWD EQUAL SPACES
               MOVE "LOGIN AND PASSWORD IS REQUIRED" TO WS-MSGERROR
               PERFORM PROC-SHOW-ERROS
               GO INSERT-LOOP
           END-IF
           WRITE REC-USR
           INVALID KEY
               MOVE "User has already been registered" TO WS-MSGERROR
               PERFORM PROC-SHOW-ERROS
               MOVE ZEROS TO REC-USR
           END-WRITE.
           GO INSERT.
       INSERT-END.

      *> -----------------------------------
       CONSULT.
           MOVE "CONSULT" TO WS-OP.
           MOVE "ESC TO GO BACK" TO WS-STATUS.
           DISPLAY SS-CLS.
           PERFORM PROC-SHOW-BOX.
       CONSULT-LOOP.
           INITIALIZE REC-USR.
           DISPLAY SS-SCR-RECORD.
           PERFORM PROC-READ-USER THRU PROC-READ-USER-END.
           IF FS-CANCEL
               GO CONSULT-END
           END-IF
           IF FS-OK
               DISPLAY SS-DATA
               MOVE "ENTER TO CONTINUE" TO WS-MSGERROR
               PERFORM PROC-SHOW-ERROS
           END-IF.
           GO CONSULT-LOOP.
       CONSULT-END.

      *> -----------------------------------
       PROC-UPDATE.
           MOVE "UPDATE" TO WS-OP.
           MOVE "ESC TO GO BACK" TO WS-STATUS.
           DISPLAY SS-CLS.
           PERFORM PROC-SHOW-BOX.
       PROC-UPDATE-LOOP.
           MOVE SPACES TO REC-USR.
           DISPLAY SS-SCR-RECORD.
           PERFORM PROC-READ-USER THRU PROC-READ-USER-END.
           IF FS-CANCEL
               GO TO PROC-UPDATE-END
           END-IF
           IF FS-OK
               ACCEPT SS-DATA
               IF COB-CRT-STATUS = COB-SCR-ESC
                   GO PROC-UPDATE-LOOP
               END-IF
           ELSE
               GO PROC-UPDATE-LOOP
            END-IF
            REWRITE REC-USR
                INVALID KEY
                    MOVE "ERROR ON WRITE RECORD" TO WS-MSGERROR
                    PERFORM PROC-SHOW-ERROS
                NOT INVALID KEY
                    CONTINUE
            END-REWRITE.
            GO PROC-UPDATE-LOOP.
       PROC-UPDATE-END.

      *> -----------------------------------
       REMOVE.
           MOVE "REMOVE" TO WS-OP.
           MOVE "ESC TO GO BACK" TO WS-STATUS.
           DISPLAY SS-CLS.
           PERFORM PROC-SHOW-BOX.
           MOVE SPACES TO REC-USR.
           DISPLAY SS-SCR-RECORD.
           PERFORM PROC-READ-USER THRU PROC-READ-USER-END.
           IF FS-CANCEL
               GO REMOVE-END
           END-IF
           IF NOT FS-OK
               GO REMOVE
           END-IF
           DISPLAY SS-DATA.
           MOVE "N" TO WS-ERRO.
           MOVE "CONFIRMS THE DELETION OF THE USER (Y/N)?" TO WS-MSGERROR.
           ACCEPT SS-ERRO.
           IF NOT E-SIM
               GO REMOVE-END
           END-IF
           DELETE FD-USER
               INVALID KEY
                   MOVE "ERROR ON REMOVE RECORD" TO WS-MSGERROR
                   PERFORM PROC-SHOW-ERROS
           END-DELETE.
       REMOVE-END.

      *> -----------------------------------
      *> READ RECORD AND SHOW ERROR MESSAGE
       PROC-READ-USER.
           ACCEPT SS-CHAVE
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
                MOVE U-ID-USR TO WS-ID-USR
                START FD-USER
                    KEY IS >= KEY1-USR
                INVALID KEY
                    MOVE "USER NOT FOUND" TO WS-MSGERROR
                    PERFORM PROC-SHOW-ERROS
                NOT INVALID KEY
                    READ FD-USER NEXT
                    AT END  
                        MOVE 25 TO ST-USR
                        MOVE "USER NOT FOUND" TO WS-MSGERROR
                        PERFORM PROC-SHOW-ERROS
                    NOT AT END
                        IF U-ID-USR <> WS-ID-USR THEN
                            MOVE 25 TO ST-USR
                            MOVE "USER NOT FOUND" TO WS-MSGERROR
                            PERFORM PROC-SHOW-ERROS
                        END-IF
                    END-READ
                END-START
           ELSE
                MOVE 99 to ST-USR
           END-IF.
       PROC-READ-USER-END.

      *> -----------------------------------
      *> OPEN FILES TO READ AND WRITE
       PROC-OPEN-FILES.
           OPEN I-O FD-USER
           IF FS-NOT-EXIST THEN
               OPEN OUTPUT FD-USER
               CLOSE FD-USER
               OPEN I-O FD-USER
           END-IF.

      *> -----------------------------------
      *> SHOW MESSAGE, ACCEPT INFORMATION AND DISPLAY STATUS
       PROC-SHOW-ERROS.
           DISPLAY SS-ERRO
           ACCEPT SS-ERRO
           DISPLAY SS-STATUS.
       PROC-SHOW-ERROS-END.
       *> -----------------------------------
       *> DRAW BOX
       PROC-SHOW-BOX.        
            MOVE 1       TO WK-BOX-POS_X1
            MOVE 3       TO WK-BOX-POS_Y1
            MOVE WS-NUMC TO WK-BOX-POS_X2
            MOVE 22      TO WK-BOX-POS_Y2
            *> BOX
            CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-BOX     *> type is box
                                BY REFERENCE WK-BOX-TIPO-LINHA    *> single line
                                BY REFERENCE WK-BOX-POS_X1        *> col 1
                                BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                                BY REFERENCE WK-BOX-POS_X2        *> col 2
                                BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                                BY REFERENCE WK-BOX-COR-FUNDO     *> background color
                                BY REFERENCE WK-BOX-COR-TEXTO     *> foreground color
            END-CALL
            *> LINE
            MOVE 5       TO WK-BOX-LINHA-POS_Y1
            CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-SEP     *> type is line
                                BY REFERENCE WK-BOX-TIPO-LINHA    *> single line
                                BY REFERENCE WK-BOX-POS_X1        *> col 1
                                BY REFERENCE WK-BOX-LINHA-POS_Y1  *> lin 1
                                BY REFERENCE WS-NUMC              *> col 2 
                                BY REFERENCE WS-NUML              *> lin 2 (not used for line)
                                BY REFERENCE WK-BOX-COR-FUNDO     *> background color
                                BY REFERENCE WK-BOX-COR-TEXTO     *> foreground color
            END-CALL.
       PROC-SHOW-BOX-END.
