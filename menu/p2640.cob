       >>SOURCE FORMAT IS FREE
*>----------------------------------------------------------------------
IDENTIFICATION      DIVISION.
*>----------------------------------------------------------------------
PROGRAM-ID.         PDAP2640.
AUTHOR.             FGALDINO.
*>----------------------------------------------------------------------
*>  Objetivo: Manutengco Cadastro Objeto.
*>
*>  Entrada...: OBJETOS_PPA      -  Objeto PPA
*>              ESTRATEGIAS_PPA  -  Estratigia PPA
*>
*>----------------------------------------------------------------------
*>  HISTSRICO DE ALTERAGUES
*>  SOLICIT     DATA     AUTOR      DESCRICAO
*>            04/10/19   FERNANDO   Implementagco.
*>
*>----------------------------------------------------------------------
ENVIRONMENT         DIVISION.
*>----------------------------------------------------------------------
CONFIGURATION SECTION.
SOURCE-COMPUTER.      
        GNUCOBOL.
    OBJECT-COMPUTER.      
        GNUCOBOL.
SPECIAL-NAMES.
       CRT STATUS IS wCRT-STATUS.
       CURSOR     IS wCursorRowCol.
	  *> LOCALE brazil "pt_BR.UTF-8".
       DECIMAL-POINT IS COMMA.
INPUT-OUTPUT        SECTION.
FILE-CONTROL.

*>COPY "OBJETOS_PPA_SE"       IN DAP$FDS.

*>COPY "ESTRATEGIAS_PPA_SE"   IN DAP$FDS.

*>----------------------------------------------------------------------
DATA DIVISION.
*>----------------------------------------------------------------------
FILE SECTION.

*>COPY "OBJETOS_PPA_FD"       IN DAP$FDS.

*>COPY "ESTRATEGIAS_PPA_FD"   IN DAP$FDS.

WORKING-STORAGE     SECTION.
*> Variaveis --------------------------------
77  K                             PIC  9(002) VALUE ZEROS.
77  WK-ACESSO                     PIC  9(001) VALUE ZEROS.
77  WK-CONF                       PIC  X(001) VALUE SPACES.
77  WK-OPER                       PIC  X(001) VALUE SPACES.
77  WK-NOMUSUARIO                 PIC  X(012).
77  WK-CODUSUARIO                 PIC  9(003).
77  WK-NIVELUSU                   PIC  9(002).
77  WK-LPTUSU                     PIC  X(025).
77  PROG-ANT                      PIC  X(030).
77  DELTA-ELE                     COMP-1 VALUE 2.

01  WK-DATA                       PIC  9(008).
01  WK-DATA-RED   REDEFINES  WK-DATA.
    03  WK-SS                     PIC  9(002).
    03  WK-AAMMDD                 PIC  9(006).
01  WK-DATA-RED-2 REDEFINES  WK-DATA.
    03  WK-AAAAMM                 PIC  9(006).
    03  FILLER                    PIC  9(002).
01  AAMMDD.
    03  AA                        PIC  9(002).
    03  MM                        PIC  9(002).
    03  DD                        PIC  9(002).
01  DATA-TELA                     PIC  X(017) VALUE SPACES.
01  TB                            PIC  9(003).
01  I                             PIC  9(003).
01  J                             PIC  9(003).
01  LIN-ANT                       PIC  9(002).
01  LIN                           PIC  9(002).
01  LIN-DEP                       PIC  9(002).

01  WK-ESPERA                     USAGE COMP-1 VALUE 1.
01  WK-CODIGO                     PIC  9(002)  VALUE ZEROS.
01  WK-NOME                       PIC  X(060)  VALUE SPACES.
01  WK-CONT1                      PIC  9(002)  VALUE ZEROS.
*>
01  BOX-LINE-HORIZONTAL           PIC  X(003) VALUE x"e29590". 
01  BOX-LINE-VERTICAL             PIC  X(003) VALUE x"e29591". 
01  BOX-TOP-LEFT                  PIC  X(003) VALUE x"e29594". 
01  BOX-TOP-RIGHT                 PIC  X(003) VALUE x"e29597". 
01  BOX-BOTTOM-LEFT               PIC  X(003) VALUE x"e2959a". 
01  BOX-BOTTOM-RIGHT              PIC  X(003) VALUE x"e2959d". 
01  OS                            PIC  X(020) VALUE "LINUS".

01  C-LINE-HORIZONTAL         PIC  X(003) VALUE "═" .
01  C-LINE-VERTICAL           PIC  X(003) VALUE "║".
01  C-TOP-LEFT                PIC  X(003) VALUE "╔".
01  C-TOP-RIGHT               PIC  X(003) VALUE "╗".
01  C-BOTTOM-LEFT             PIC  X(003) VALUE "╚".
01  C-BOTTOM-RIGHT            PIC  X(003) VALUE "╝".

*>  Cores
01  BLACK                                     CONSTANT AS 0.
01  BLUE                                      CONSTANT AS 1.
01  GREEN                                     CONSTANT AS 2.
01  CYAN                                      CONSTANT AS 3.
01  RED                                       CONSTANT AS 4.
01  MAGENTA                                   CONSTANT AS 5.
01  YELLOW                                    CONSTANT AS 6.
01  WHITE                                     CONSTANT AS 7.
*> Teclas
78  K-ENTER                                   VALUE 0000.
78  K-UP                                      VALUE 2003.
78  K-DOWN                                    VALUE 2004.
78  K-LEFT                                    VALUE 2009.
78  K-RIGHT                                   VALUE 2010.
78  K-TAB                                     VALUE 2007.
78  K-BACKTAB                                 VALUE 2008.
78  K-PAGEUP                                  VALUE 2001.
78  K-PAGEDOWN                                VALUE 2002.
78  K-ESCAPE                                  VALUE 2005.
*> Mouse
77  K-MOUSE-MOVE                  PIC  9(004) VALUE 2040.
77  K-LEFT-PRESSED                PIC  9(004) VALUE 2041.
77  K-LEFT-RELEASED               PIC  9(004) VALUE 2042.
77  K-LEFT-DBL-CLICK              PIC  9(004) VALUE 2043.
77  K-MID-PRESSED                 PIC  9(004) VALUE 2044.
77  K-MID-RELEASED                PIC  9(004) VALUE 2045.
77  K-MID-DBL-CLICK               PIC  9(004) VALUE 2046.
77  K-RIGHT-PRESSED               PIC  9(004) VALUE 2047.
77  K-RIGHT-RELEASED              PIC  9(004) VALUE 2048.
77  K-RIGHT-DBL-CLICK             PIC  9(004) VALUE 2049.

*> Dados da Tabela -------------------------
*>      CODIGO  NOME
*>......--99----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX......
01  TABELA.
*>  ** 4 paginas de 14 **
    03  TAB-LINHA OCCURS 56.
        05  TAB-F1           PIC  X(002).
        05  TAB-CODIGO       PIC  X(002).
        05  TAB-F2           PIC  X(004).
        05  TAB-NOME         PIC  X(060).

*> FLAGS -----------------------------------
01  FLG-EOF                  PIC  X(001) VALUE "F".
    88  EOF                              VALUE "T".
01  FLG-ACHOU                PIC  X(001) VALUE "F".
    88  ACHOU                            VALUE "T".
01  FLG-SAIDA                PIC  X(001) VALUE "F".
    88  SAIDA                            VALUE "T".
01  FLG-OPC-INCLUIR          PIC  X(001) VALUE "F".
    88  OPC-INCLUIR                      VALUE "T".
01  FLG-VALIDO               PIC  X(001) VALUE "F".
    88  VALIDO                           VALUE "T".
01  FLG-TAB-VAZIA            PIC  X(001) VALUE "F".
    88  TAB-VAZIA                        VALUE "T".
01  FLG-PERMISSAO-EXCLUIR    PIC  X(001) VALUE "F".
    88  PERMISSAO-EXCLUIR                VALUE "T".
*> Variaveis de Tela ------------------------------
01  wCRT-STATUS                   PIC  9(004).
01  wCursorRowCol                 PIC  9(006).
01  REDefines wCursorRowCol .
    03  wCursorRow                Pic  9(003).
    03  wCursorCol                Pic  9(003).    
01  wINT                               BINARY-SHORT SIGNED.
77  WK-NUML                       PIC  9(003) VALUE ZEROS.
77  WK-NUMC                       PIC  9(003) VALUE ZEROS.
77  COR-FUNDO                     PIC  9(001) VALUE 2.
77  COR-FRENTE                    PIC  9(001) VALUE 0.
01  WK-DATA-HORA.
    03  WK-DATA.
        05  WK-ANO                PIC  9(004) VALUE ZEROS.
        05  WK-MES                PIC  9(002) VALUE ZEROS.
        05  WK-DIA                PIC  9(002) VALUE ZEROS.
    03  WK-HORA.
        05  WK-HOR                PIC  9(002) VALUE ZEROS.
        05  WK-MIN                PIC  9(002) VALUE ZEROS.
        05  WK-SEG                PIC  9(002) VALUE ZEROS.
        05  WK-CSE                PIC  9(002) VALUE ZEROS.
        05 DIFERENCA-GREENWICH    PIC  X(005) VALUE SPACES.
01  WK-TEL01-CON.
    03  WK-TEL01C-1                PIC  X(063) VALUE 
            "PDAP2640             MANTER CADASTRO - OBJETOS FOMENTO".
    03  WK-TEL01C-2.
        05  WK-TEL-DATA.
            07  WK-DIA-T          PIC  X(002).
            07  FILLER            PIC  X(001) VALUE "-".
            07  WK-MES-T          PIC  X(003).
            07  FILLER            PIC  X(001) VALUE "-".
            07  WK-ANO-T          PIC  X(004).
        05  FILLER                PIC  X(001) VALUE SPACES.
        05  WK-TEL-HORA.
            07  WK-HOR-T          PIC  X(002).
            07  FILLER            PIC  X(001) VALUE ":".
            07  WK-MIN-T          PIC  X(002).
01  WK-TEL01-INC.
    03  WK-TEL01I-1               PIC  X(063) VALUE 
            "PDAP2640             MANTER CADASTRO - OBJETOS FOMENTO".
    03  WK-TEL01I-2               PIC  X(017) VALUE SPACES.
01  WK-TEL24-1                    PIC  X(080) VALUE "MENSAGEM:".
01  WK-MSG                        PIC  X(080) VALUE SPACES.
01  WK-TECLA                      PIC  X(001) VALUE SPACES.
*> desenhar o box
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
*>
*>----------------------------------------------------------------------
COPY screenio.
SCREEN SECTION.
01  SS-TELA-CON.
    03  BLANK SCREEN.
    03  LINE 01 COLUMN 01 FROM WK-TEL01-CON
             FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 04 COLUMN 02 PIC X(078) VALUE "      CODIGO  NOME"
             FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 23 COLUMN 01 PIC X(080) VALUE 
            " F2 = Retornar | PAGE UP = Pag. Anterior | PAGE DOWN = Pag. a Frente | <SETAS>"
            FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 24 COLUMN 01 PIC X(080) VALUE SPACES.
    03  LINE 24 COLUMN 01 VALUE "MENSAGEM: ".

01  SS-TELA-INC.
    03  BLANK SCREEN.
    03  LINE 01 COLUMN 01 FROM WK-TEL01-INC
            FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 07 COLUMN 05 PIC X(072) VALUE "                               INCLUSAO"
            FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 09 COLUMN 06 VALUE "56789012345678" 
             HIGHLIGHT FOREGROUND-COLOR COR-FUNDO BACKGROUND-COLOR COR-FRENTE.
    03  GET-CODIGO.
        05  LINE 10 COLUMN 08 VALUE "Codigo:".
        05  LINE 10 COL plus 1 USING WK-CODIGO AUTO
                 BLANK WHEN ZEROS. 
        05  LINE 24 COL 11 VALUE "Informe o código a ser incluido                                       "
                 FOREGROUND-COLOR COR-FUNDO. 
    03  GET-NOME.
        05  LINE 12 COLUMN 08 VALUE "Nome..:".
        05  LINE 12 COL plus 1 USING WK-NOME AUTO.
        05  LINE 24 COL 11 VALUE "Informe a descrição                                                   "
                 FOREGROUND-COLOR COR-FUNDO. 
    03  LINE 23 COLUMN 01 PIC X(080) VALUE 
            "                                  F2 = Retornar"
            FOREGROUND-COLOR COR-FRENTE BACKGROUND-COLOR COR-FUNDO.
    03  LINE 24 COLUMN 01 PIC X(080) VALUE SPACES.
    03  LINE 24 COLUMN 01 VALUE "MENSAGEM: ".

*>----------------------------------------------------------------------
*>                     Modulo Principal do Programa                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION.
000-INICIO.

    SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'
    SET ENVIRONMENT 'COB_SCREEN_ESC'        TO 'Y'
    SET ENVIRONMENT 'ESCDELAY'              TO '25'
    *> CALL static "resize_term" USING by value 25 by value 80
    CALL "SYSTEM" USING "nohup resize -s 25 80 1>/dev/null 2>/dev/null && sleep 1" 
    DISPLAY " " ERASE EOS *> to start extended screenio
    CALL "SYSTEM" USING "chcp 437"
    CALL "SYSTEM" USING "mode con: lines=25 cols=80"
    ACCEPT WK-NUML FROM LINES
    ACCEPT WK-NUMC FROM COLUMNS
    MOVE WK-NUMC TO WK-BOX-POS_X2.

    PERFORM 900-DATA-HORA
    MOVE 1 TO DELTA-ELE
    DISPLAY SS-TELA-CON
    *> DESENHO DO BOX
    MOVE 1       TO WK-BOX-POS_X1
    MOVE 3       TO WK-BOX-POS_Y1
    MOVE WK-NUMC TO WK-BOX-POS_X2
    MOVE 22      TO WK-BOX-POS_Y2
    CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-BOX      *> box
                         BY REFERENCE WK-BOX-TIPO-LINHA    *> linha simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2
                         BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                         BY REFERENCE WK-BOX-COR-FUNDO     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL
    *> LINHA DE SEPARACAO DAS SECOES
    MOVE 5       TO WK-BOX-LINHA-POS_Y1
    CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-SEP      *> Lina de separacao
                         BY REFERENCE WK-BOX-TIPO-LINHA    *> linha simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-LINHA-POS_Y1  *> lin 1
                         BY REFERENCE WK-NUMC              *> col 2 
                         BY REFERENCE WK-NUML              *> lin 2 (nao usado)
                         BY REFERENCE WK-BOX-COR-FUNDO     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL

    ACCEPT WK-TECLA LINE 24 COLUMN 79.
    display SS-TELA-INC
    *> DESENHO DO BOX
    MOVE 5       TO WK-BOX-POS_X1
    MOVE 5       TO WK-BOX-POS_Y1
    COMPUTE WK-BOX-POS_X2 =  WK-NUMC - 3
    MOVE 14      TO WK-BOX-POS_Y2
    CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-BOX      *> box
                         BY REFERENCE WK-BOX-TIPO-LINHA    *> linha simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2
                         BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                         BY REFERENCE WK-BOX-COR-FUNDO     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL
    *> LINHA DE SEPARACAO DAS SECOES
    MOVE 8       TO WK-BOX-LINHA-POS_Y1
    CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-SEP      *> Lina de separacao
                         BY REFERENCE WK-BOX-TIPO-LINHA    *> linha simples
                         BY REFERENCE WK-BOX-POS_X1        *> col 1
                         BY REFERENCE WK-BOX-LINHA-POS_Y1  *> lin 1
                         BY REFERENCE WK-BOX-POS_X2        *> col 2 
                         BY REFERENCE WK-NUML              *> lin 2 (nao usado)
                         BY REFERENCE WK-BOX-COR-FUNDO     *> cor fundo
                         BY REFERENCE WK-BOX-COR-TEXTO     *> col frente
    END-CALL
    accept GET-CODIGO
    DISPLAY WK-CODIGO LINE 10 COLUMN 15
    accept GET-NOME
    DISPLAY WK-NOME LINE 12 COLUMN 15


    ACCEPT WK-TECLA LINE 24 COLUMN 79.
    STOP RUN.

*>----------------------------------------------------------------------
900-DATA-HORA.

    MOVE FUNCTION CURRENT-DATE TO WK-DATA-HORA
    MOVE   WK-ANO              TO   WK-ANO-T
    EVALUATE WK-MES
        WHEN 01    MOVE "JAN"  TO   WK-MES-T
        WHEN 02    MOVE "FEV"  TO   WK-MES-T
        WHEN 03    MOVE "MAR"  TO   WK-MES-T
        WHEN 04    MOVE "ABR"  TO   WK-MES-T
        WHEN 05    MOVE "MAI"  TO   WK-MES-T
        WHEN 06    MOVE "JUN"  TO   WK-MES-T
        WHEN 07    MOVE "JUL"  TO   WK-MES-T
        WHEN 08    MOVE "AGO"  TO   WK-MES-T
        WHEN 09    MOVE "SET"  TO   WK-MES-T
        WHEN 10    MOVE "OUT"  TO   WK-MES-T
        WHEN 11    MOVE "NOV"  TO   WK-MES-T
        WHEN 12    MOVE "DEZ"  TO   WK-MES-T
        WHEN OTHER MOVE "JAN"  TO   WK-MES-T
    END-EVALUATE
    MOVE   WK-DIA              TO   WK-DIA-T
    MOVE   WK-HOR              TO   WK-HOR-T 
    MOVE   WK-MIN              TO   WK-MIN-T
    MOVE   WK-TEL01C-2         TO   WK-TEL01I-2.

900-EXIT-DATA-HORA.
    EXIT.
*>----------------------------------------------------------------------
