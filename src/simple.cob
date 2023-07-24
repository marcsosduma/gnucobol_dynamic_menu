       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. simple.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 ONE    PICTURE 9999  VALUE 0.
       77 wk-key PIC X(1).
       PROCEDURE DIVISION.
       001-INIT.
       PERFORM 002-SHOW.
       ACCEPT WK-KEY.
       001-END-PRG.
            STOP RUN.
       002-SHOW.
            DISPLAY "Demonstration of step over"
            MOVE 1 TO ONE.
            DISPLAY ONE.
       END PROGRAM simple.
       