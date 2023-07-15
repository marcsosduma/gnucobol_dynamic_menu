       >>SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID.    carrega_perfil_usuario.
AUTHOR.        Duma.
*>----------------------------------------------------------------------
*> Objetivo: Gerar arquivo perfil_menu a partir do arquivo texto
*>
*> Entrada...: perfil_menu.txt
*>
*> Saida.....: perfil_menu.dat
*>
*>----------------------------------------------------------------------
*>  HISTORICO DE ALTERACOES
*>  SOLICITACAO    DATA      AUTOR     DESCRICAO
*>  MENU DINAMICO  26/05/23  DUMA      Menu automatico no GNU COBOL.
*>----------------------------------------------------------------------
ENVIRONMENT DIVISION.
CONFIGURATION    SECTION.
SOURCE-COMPUTER. FAPESP.
OBJECT-COMPUTER. 
             GNUCOBOL
             CLASSIFICATION brazil.
SPECIAL-NAMES.
               LOCALE brazil "pt_BR.UTF-8".
               DECIMAL-POINT IS COMMA.
INPUT-OUTPUT   SECTION.
FILE-CONTROL.

COPY "../share/perfil_menu_se.cpy".

     SELECT ARQUIVO_TXT ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL
            FILE  STATUS IS ST-TXT.

*>----------------------------------------------------------------------
DATA DIVISION.
FILE SECTION.

COPY "../share/perfil_menu_fd.cpy".

FD  ARQUIVO_TXT
    VALUE OF FILE-ID IS  "../txt/perfil_menu.txt".
01  REG-TXT                       PIC  X(30).

*>----------------------------------------------------------------------
WORKING-STORAGE SECTION.
*> Variaveis -----------------------------------------------------------
77  WK-NUML                       PIC  9(003).
77  WK-NUMC                       PIC  9(003).
77  COR-FUNDO                     PIC  9(001) VALUE 1.
77  COR-FRENTE                    PIC  9(001) VALUE 6.
77  ST-TXT                        PIC  X(002) VALUE ZEROS.
77  ST-PFM                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCELA                           VALUE 99.
    88  FSL-NAO-EXISTE                        VALUE 35.

01  WK-SAI-LOOP                   PIC  X(001).
01  I                             PIC  9(003) VALUE ZEROS.
01  J                             PIC  9(003) VALUE ZEROS.
01  WK-LIDO                       PIC  9(008) VALUE ZEROS.
01  WK-ERRO                       PIC  9(007) VALUE ZEROS.
01  WK-CONF                       PIC  X(001) VALUE SPACES.
01  WK-LIMPA                      PIC  X(080) VALUE SPACES.
01  DT-TELA                       PIC  X(017) VALUE SPACES.
01  WK-DATA.
    03  WK-ANO                    PIC  9(002) VALUE ZEROS.
    03  WK-MES                    PIC  9(002) VALUE ZEROS.
    03  WK-DIA                    PIC  9(002) VALUE ZEROS.
01  WK-HORA.
    03  WK-HOR                    PIC  9(002) VALUE ZEROS.
    03  WK-MIN                    PIC  9(002) VALUE ZEROS.
    03  WK-SEG                    PIC  9(002) VALUE ZEROS.
    03  WK-CSE                    PIC  9(002) VALUE ZEROS.
01  FLG-EOF                       PIC  X(001) VALUE "F".
    88  EOF                                   VALUE "T".
01  FLG-FALHA                     PIC  X(001) VALUE "F".
    88  FALHA                                 VALUE "T".
01  FLG-QUEBRA-LINHA              PIC  X(001) VALUE "F".
    88  QUEBRA-LINHA                          VALUE "T".
01  WK-TEL01-1                    PIC  X(057) VALUE
    "GERAR ARQ PRF. DO MENU A PARTIR DO ARQUIVO TXT".
01  WK-TEL01-2.
    03  WK-TEL-DATA.
        05  WK-DIA-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-MES-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE "/".
        05  WK-ANO-T              PIC  X(004).
    03  FILLER                    PIC  X(002) VALUE SPACES.
    03  WK-TEL-HORA.
        05  WK-HOR-T              PIC  X(002).
        05  FILLER                PIC  X(001) VALUE ":".
        05  WK-MIN-T              PIC  X(002).
01  WK-TEL24-1                    PIC  X(009) VALUE "MENSAGEM:".
01  WK-MSG                        PIC  X(080) VALUE SPACES.

COPY screenio.

SCREEN SECTION.
01  SS-CLS.
    03  SS-FILLER01-1.
        05  BLANK SCREEN.
        05  LINE 01 COLUMN 01 PIC X(80)
            BACKGROUND-COLOR COR-FUNDO.
    03  SS-FILLER01-2.
        05  LINE 01 COLUMN 01 PIC X(57) FROM WK-TEL01-1
            HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
            BACKGROUND-COLOR COR-FUNDO.
    03  SS-FILLER01-3.
        05  LINE 01 COLUMN 64 PIC X(17) FROM WK-TEL01-2
            HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
            BACKGROUND-COLOR COR-FUNDO.
    03  SS-FILLER02-1 FOREGROUND-COLOR 2.
        05  LINE 02 COLUMN 01 VALUE "LIDOS:".
        05  COLUMN PLUS 2  PIC  9(008) USING WK-LIDO
                   BLANK WHEN ZEROS. 
    03  SS-FILLER24-1.
        05  LINE 24 COLUMN 01 PIC X(17) FROM WK-TEL24-1
            HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
            BACKGROUND-COLOR COR-FUNDO.
 01  SS-MSG.
     03  LINE 24 COLUMN 11 PIC X(70)
*>     03  LINE 24 COLUMN 11 ERASE EOL
         BACKGROUND-COLOR COR-FUNDO.
     03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
         05  LINE 24 COLUMN 11    PIC  X(070) FROM WK-MSG.
01  SS-CONF.
    03  FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
        05  LINE 24 COLUMN PLUS 44 PIC  X(001) USING WK-CONF.
*>----------------------------------------------------------------------
*>                    Modulo Principal do Programa                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION.

000-INICIO.
    
    PERFORM 010-INICIALIZA

    PERFORM 020-PROCESSAMENTO

    PERFORM 030-FINALIZA.

000-EXIT-INICIO.
    EXIT.
*>----------------------------------------------------------------------
010-INICIALIZA.

    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'
    SET ENVIRONMENT 'ESCDELAY' TO '25'
    ACCEPT WK-NUML FROM LINES
    ACCEPT WK-NUMC FROM COLUMNS
    PERFORM 900-DATA-HORA
    
    DISPLAY SS-CLS
    MOVE "Confirma o processamento? [S/N]: " TO WK-MSG
    DISPLAY SS-MSG

    MOVE SPACES TO WK-CONF
    PERFORM UNTIL (WK-CONF = "S" OR "s" OR "N" OR "n")
       ACCEPT SS-CONF
    END-PERFORM

    IF WK-CONF = "N" OR "n"
       STOP RUN
    END-IF

    OPEN I-O PERFIL_MENU
    IF FSL-OK
       MOVE "ERRO! ARQUIVO JA GERADO. O programa sera encerrado" TO WK-MSG
       DISPLAY SS-MSG
       STOP RUN
    END-IF
    OPEN OUTPUT PERFIL_MENU
    CLOSE  PERFIL_MENU
    OPEN I-O PERFIL_MENU
    OPEN INPUT  ARQUIVO_TXT

    MOVE "Aguarde, Processando..." TO WK-MSG
    DISPLAY SS-MSG.

010-FIM-INICIALIZA.
    EXIT.
*>----------------------------------------------------------------------
020-PROCESSAMENTO.

       INITIALIZE REG-PFM
                  REG-TXT
       MOVE "N" TO FLG-EOF
       PERFORM UNTIL EOF
                display WK-LIDO line 05 column 01
          READ ARQUIVO_TXT
             AT END
                MOVE "T" TO FLG-EOF
             NOT AT END
                ADD 1 TO WK-LIDO
                DISPLAY SS-FILLER02-1
                IF REG-TXT(1:5)<>SPACES
                    WRITE REG-PFM FROM REG-TXT
                    INVALID KEY 
                        display WK-LIDO line 02 column 08
                        display REG-TXT line 03 column 01
                        MOVE "ERRO AO GRAVAR O REGISTRO. O programa sera encerrado - FS: " TO WK-MSG
                        MOVE ST-PFM TO WK-MSG(61:02)
                        DISPLAY SS-MSG
                        CLOSE ARQUIVO_TXT PERFIL_MENU
                        STOP RUN
                    NOT INVALID KEY
                        display WK-LIDO line 02 column 08
                        display REG-TXT line 20 column 01
                 END-WRITE
                END-IF
          END-READ
       END-PERFORM.

020-EXIT-PROCESSAMENTO.
    EXIT.
*>----------------------------------------------------------------------
030-FINALIZA.

    CLOSE PERFIL_MENU
          ARQUIVO_TXT

    PERFORM 900-DATA-HORA
    DISPLAY SS-FILLER02-1
    DISPLAY WK-TEL01-2 LINE 02 COLUMN 64 HIGHLIGHT FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO
    MOVE "*** FIM DO PROCESSAMENTO ***" TO WK-MSG
    DISPLAY SS-MSG
    STOP RUN.

030-EXIT-FINALIZA.
    EXIT.
*>----------------------------------------------------------------------
900-DATA-HORA.

    ACCEPT WK-HORA FROM TIME
    MOVE   WK-HOR  TO   WK-HOR-T 
    MOVE   WK-MIN  TO   WK-MIN-T    
    ACCEPT WK-DATA FROM DATE
    MOVE   WK-ANO  TO   WK-ANO-T
    MOVE   WK-MES  TO   WK-MES-T
    MOVE   WK-DIA  TO   WK-DIA-T.

900-EXIT-DATA-HORA.
    EXIT.
*>----------------------------------------------------------------------
