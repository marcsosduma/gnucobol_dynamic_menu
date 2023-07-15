       >>SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID.    read_profile_text_file.
AUTHOR.        Duma.
*>----------------------------------------------------------------------
*> Object: Generate profile file from text file
*>
*> Read...: profile.txt
*>
*> Out....: profile.dat
*>
*>----------------------------------------------------------------------
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

COPY "../cpy/profile_se.cpy".

     SELECT FILE_TXT ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL
            FILE  STATUS IS ST-TXT.

*>----------------------------------------------------------------------
DATA DIVISION.
FILE SECTION.

COPY "../cpy/profile_fd.cpy".

FD  FILE_TXT
    VALUE OF FILE-ID IS  "../txt/profile.txt".
01  rec-TXT                       PIC  X(100).

*>----------------------------------------------------------------------
WORKING-STORAGE SECTION.
*> Variaveis -----------------------------------------------------------
77  WK-NUML                       PIC  9(003).
77  WK-NUMC                       PIC  9(003).
77  BACK-COLOR                    PIC  9(001) VALUE 1.
77  FOR-COLOR                     PIC  9(001) VALUE 6.
77  ST-TXT                        PIC  X(002) VALUE ZEROS.
77  ST-PRF                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.

01  WK-END-LOOP                   PIC  X(001).
01  I                             PIC  9(003) VALUE ZEROS.
01  J                             PIC  9(003) VALUE ZEROS.
01  WK-READS                      PIC  9(008) VALUE ZEROS.
01  WK-ERROR                      PIC  9(007) VALUE ZEROS.
01  WK-CONF                       PIC  X(001) VALUE SPACES.
01  WK-CLEAR                      PIC  X(080) VALUE SPACES.
01  DT-SCREEN                     PIC  X(017) VALUE SPACES.
01  WK-DATE.
    03  WK-YEAR                    PIC  9(002) VALUE ZEROS.
    03  WK-MNT                    PIC  9(002) VALUE ZEROS.
    03  WK-DAY                    PIC  9(002) VALUE ZEROS.
01  WK-HORA.
    03  WK-HOR                    PIC  9(002) VALUE ZEROS.
    03  WK-MIN                    PIC  9(002) VALUE ZEROS.
    03  WK-SEC                    PIC  9(002) VALUE ZEROS.
    03  WK-CSE                    PIC  9(002) VALUE ZEROS.
01  FLG-EOF                       PIC  X(001) VALUE "F".
    88  EOF                                   VALUE "T".
01  FLG-FAIL                      PIC  X(001) VALUE "F".
    88  FALHA                                 VALUE "T".
01  FLG-LINE-BREAK              PIC  X(001) VALUE "F".
    88  LINE-BREAK                          VALUE "T".
01  WK-SCR01-1                    PIC  X(057) VALUE
    "GENERATE PROFILE FILE FROM FILE IN TXT FORMAT".
01  WK-SCR01-2.
    03  WK-SCR-DATE.
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

COPY screenio.

SCREEN SECTION.
01  SS-CLS.
    03  SS-FILLER01-1.
        05  BLANK SCREEN.
        05  LINE 01 COLUMN 01 PIC X(80)
            BACKGROUND-COLOR BACK-COLOR.
    03  SS-FILLER01-2.
        05  LINE 01 COLUMN 01 PIC X(57) FROM WK-SCR01-1
            HIGHLIGHT FOREGROUND-COLOR FOR-COLOR
            BACKGROUND-COLOR BACK-COLOR.
    03  SS-FILLER01-3.
        05  LINE 01 COLUMN 64 PIC X(17) FROM WK-SCR01-2
            HIGHLIGHT FOREGROUND-COLOR FOR-COLOR
            BACKGROUND-COLOR BACK-COLOR.
    03  SS-FILLER02-1 FOREGROUND-COLOR 2.
        05  LINE 02 COLUMN 01 VALUE "READS:".
        05  COLUMN PLUS 2  PIC  9(008) USING WK-READS
                   BLANK WHEN ZEROS. 
    03  SS-FILLER24-1.
        05  LINE 24 COLUMN 01 PIC X(17) FROM WK-SCR24-1
            HIGHLIGHT FOREGROUND-COLOR FOR-COLOR
            BACKGROUND-COLOR BACK-COLOR.
 01  SS-MSG.
     03  LINE 24 COLUMN 11 PIC X(70)
*>     03  LINE 24 COLUMN 11 ERASE EOL
         BACKGROUND-COLOR BACK-COLOR.
     03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
         05  LINE 24 COLUMN 11    PIC  X(070) FROM WK-MSG.
01  SS-CONF.
    03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
        05  LINE 24 COLUMN PLUS 44 PIC  X(001) USING WK-CONF.
*>----------------------------------------------------------------------
*>                    Main Program Module                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION.

000-FIRST-PROC.
    
    PERFORM 010-INITIALIZES

    PERFORM 020-PROCESSING

    PERFORM 030-ENDS.

000-FIRST-PROC-END.
    EXIT.
*>----------------------------------------------------------------------
010-INITIALIZES.

    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
    SET ENVIRONMENT 'ESCDELAY' TO '25'
    ACCEPT WK-NUML FROM LINES
    ACCEPT WK-NUMC FROM COLUMNS
    PERFORM 900-DATA-HORA
    
    DISPLAY SS-CLS
    MOVE "Confirm processing? [Y/N]: " TO WK-MSG
    DISPLAY SS-MSG

    MOVE SPACES TO WK-CONF
    PERFORM UNTIL (WK-CONF = "Y" OR "y" OR "N" OR "n")
       ACCEPT SS-CONF
    END-PERFORM

    IF WK-CONF = "N" OR "n"
       STOP RUN
    END-IF

    OPEN I-O FD-PRF
    IF FSL-OK
       MOVE "ERROR! FILE ALREADY GENERATED. The program will be closed" TO WK-MSG
       DISPLAY SS-MSG
       STOP RUN
    END-IF
    OPEN OUTPUT FD-PRF
    CLOSE  FD-PRF
    OPEN I-O FD-PRF
    OPEN INPUT  FILE_TXT

    MOVE "Please wait, Processing..." TO WK-MSG
    DISPLAY SS-MSG.

010-END-INITIALIZES.
    EXIT.
*>----------------------------------------------------------------------
020-PROCESSING.
       INITIALIZE REC-PRF
                  REC-TXT
       MOVE "N" TO FLG-EOF
       PERFORM UNTIL EOF
          display WK-READS line 05 column 01
          READ FILE_TXT
             AT END
                MOVE "T" TO FLG-EOF
             NOT AT END
                ADD 1 TO WK-READS
                DISPLAY SS-FILLER02-1
                IF REC-TXT(1:5)<>SPACES
                    WRITE REC-PRF FROM REC-TXT
                    INVALID KEY 
                        display WK-READS line 02 column 08
                        display REC-TXT line 03 column 01
                        MOVE "ERROR WRITING THE REGISTRATION. The program will exit - FS: " TO WK-MSG
                        MOVE ST-PRF TO WK-MSG(61:02)
                        DISPLAY SS-MSG
                        CLOSE FILE_TXT FD-PRF
                        STOP RUN
                    NOT INVALID KEY
                        display WK-READS line 02 column 08
                        display REC-TXT line 20 column 01
                 END-WRITE
                END-IF
          END-READ
       END-PERFORM.

020-EXIT-PROCESSING.
    EXIT.
*>----------------------------------------------------------------------
030-ENDS.

    CLOSE FD-PRF
          FILE_TXT

    PERFORM 900-DATA-HORA
    DISPLAY SS-FILLER02-1
    DISPLAY WK-SCR01-2 LINE 02 COLUMN 64 HIGHLIGHT FOREGROUND-COLOR FOR-COLOR BACKGROUND-COLOR BACK-COLOR
    MOVE "*** END OF PROCESSING ***" TO WK-MSG
    DISPLAY SS-MSG
    STOP RUN.

030-EXIT-ENDS.
    EXIT.
*>----------------------------------------------------------------------
900-DATA-HORA.

    ACCEPT WK-HORA FROM TIME
    MOVE   WK-HOR  TO   WK-HOR-T 
    MOVE   WK-MIN  TO   WK-MIN-T    
    ACCEPT WK-DATE FROM DATE
    MOVE   WK-YEAR  TO   WK-YEAR-T
    MOVE   WK-MNT  TO   WK-MNT-T
    MOVE   WK-DAY  TO   WK-DAY-T.

900-EXIT-DATA-HORA.
    EXIT.
*>----------------------------------------------------------------------
