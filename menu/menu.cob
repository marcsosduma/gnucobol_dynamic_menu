       >>SOURCE FORMAT IS FREE
       *>****************************************************************
       *> Author: MARCOS DUMA
       *> Date: 23/04/2023
       *> Purpose: MENU PARA PROGRAMAS EM GNUCOBOL - LINUX E WINDOWS
       *> Este programa foi baseado na implementacao TUI do
       *> projeto GNUCOBOL DE Eugenio Di Lorenzo 
       *> Tectonics: cobc
       *>****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. menu.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
	   SOURCE-COMPUTER.      
             GNUCOBOL.
           OBJECT-COMPUTER.      
             GNUCOBOL
             CLASSIFICATION brazil.
       SPECIAL-NAMES.
           CRT STATUS IS wCRT-STATUS.
           CURSOR     IS wCursorRowCol.
	       LOCALE brazil "pt_BR.UTF-8".
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARRAY    PIC X(20) OCCURS 20 TIMES.
       01  TAM-ITEM PIC 9(03) OCCURS 20 TIMES.
       01  TEXTO-MENU PIC X(30).
       01  Item-Num PIC 99.
       01  K PIC 99.
       01  SELECAO PIC 99.
       01  wDummy  PIC X(01) VALUE SPACE.
       01  wPrompt PIC X(01) VALUE SPACE.
       01  wCRT-STATUS         PIC 9(04) VALUE 9999.
       01  wCursorRowCol       PIC 9(06) value 0000.
       01  redefines wCursorRowCol .
           05 wCursorRow       Pic 9(03).
           05 wCursorCol       Pic 9(03).    
       01  wInt        BINARY-SHORT SIGNED.
       01  box-pos-x PIC 9(03).
       01  box-pos-y PIC 9(03).
       01  box-width PIC 9(03).
       01  calc-x    PIC 9(03).
       01  calc-y    PIC 9(03).
       01  menu-bg   PIC 9(03).
       01  menu-fg   PIC 9(03).
       01  one-click pic 9(03).
       01  doble-left-click pic 9(03).
       01  end-time    pic 9(09).
       01  atual-time  pic 9(09).
       01  diference   PIC 9(09).
       01  wTimeX.
          03 wTimehh         pic  9(2).
          03 wTimemm         pic  9(2).
          03 wTimess         pic  9(2).
          03 wTimecc         pic  9(2).
       01  OS PIC X(20) VALUE SPACES.

       01 black   constant as 0.
       01 blue    constant as 1.
       01 green   constant as 2.
       01 cyan    constant as 3.
       01 red     constant as 4.
       01 magenta constant as 5.
       01 yellow  constant as 6.
       01 WHITE   constant as 7.

       78 K-ENTER       VALUE 0000.
       78 K-UP          VALUE 2003.
       78 K-DOWN        VALUE 2004.
       78 K-LEFT        VALUE 2009.
       78 K-RIGHT       VALUE 2010.
       78 K-TAB         VALUE 2007.
       78 K-BACKTAB     VALUE 2008.
       78 K-PAGEUP      VALUE 2001.
       78 K-PAGEDOWN    VALUE 2002.
       78 K-ESCAPE      VALUE 2005.

       77 K-MOUSE-MOVE          PIC 9(04) VALUE 2040.
       77 K-LEFT-PRESSED        PIC 9(04) VALUE 2041.
       77 K-LEFT-RELEASED       PIC 9(04) VALUE 2042.
       77 K-LEFT-DBL-CLICK      PIC 9(04) VALUE 2043.
       77 K-MID-PRESSED         PIC 9(04) VALUE 2044.
       77 K-MID-RELEASED        PIC 9(04) VALUE 2045.
       77 K-MID-DBL-CLICK       PIC 9(04) VALUE 2046.
       77 K-RIGHT-PRESSED       PIC 9(04) VALUE 2047.
       77 K-RIGHT-RELEASED      PIC 9(04) VALUE 2048.
       77 K-RIGHT-DBL-CLICK     PIC 9(04) VALUE 2049.
       *>  mouse mask, apply to COB_MOUSE_FLAGS
       78  COB-AUTO-MOUSE-HANDLING          VALUE 1.
       78  COB-ALLOW-LEFT-DOWN              VALUE 2.
       78  COB-ALLOW-LEFT-UP                VALUE 4.
       78  COB-ALLOW-LEFT-DOUBLE            VALUE 8.
       78  COB-ALLOW-MIDDLE-DOWN            VALUE 16.
       78  COB-ALLOW-MIDDLE-UP              VALUE 32.
       78  COB-ALLOW-MIDDLE-DOUBLE          VALUE 64.
       78  COB-ALLOW-RIGHT-DOWN             VALUE 128.
       78  COB-ALLOW-RIGHT-UP               VALUE 256.
       78  COB-ALLOW-RIGHT-DOUBLE           VALUE 512.
       78  COB-ALLOW-MOUSE-MOVE             VALUE 1024.
       01  COB-MOUSE-FLAGS      PIC 9(04).
       77  WS-NUML              PIC 999.
       77  WS-NUMC              PIC 999.
       01  WS-TIPO-MENU         PIC X(1).
       01  WS-IDX               PIC 9(03).
       01  WK-SPACES            PIC X(150) VALUE SPACES.
       *> desenhar o box
       01  WK-BOX-TIPO-BOX               PIC X(01) VALUE "B".
       01  WK-BOX-TIPO-LINHA             PIC 9(01) VALUE 1.
       01  WK-BOX-POS_X1                 PIC 9(03) VALUE 1.
       01  WK-BOX-POS_Y1                 PIC 9(03) VALUE 3.
       01  WK-BOX-POS_X2                 PIC 9(03) VALUE 80.
       01  WK-BOX-POS_Y2                 PIC 9(03) VALUE 22.
       
       LINKAGE SECTION.
       01  TIPO-MENU PIC X(1).
       01  PARM.
           05 ITENS occurs 20 times pic x(20). 
       01  NUMERO_ITENS             PIC 9(03).
       01  POS_X                    PIC 9(03).
       01  POS_Y                    PIC 9(03).
       01  COR-FUNDO                PIC 9(03).
       01  COR-TEXTO                PIC 9(03).
       01  COR-SEL-FUNDO            PIC 9(03).
       01  COR-SEL-TEXTO            PIC 9(03).
       01  ITEM-SELECIONADO         PIC 9(03).
       01  POS-ITEM-SEL-X           PIC 9(03).
       01  POS-ITEM-SEL-Y           PIC 9(03).
       01  TAM-MENU-X               PIC 9(03).

       PROCEDURE DIVISION USING TIPO-MENU PARM NUMERO_ITENS POS_X POS_Y COR-FUNDO 
                                COR-TEXTO COR-SEL-FUNDO COR-SEL-TEXTO ITEM-SELECIONADO 
                                POS-ITEM-SEL-X POS-ITEM-SEL-Y TAM-MENU-X.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT OS FROM ENVIRONMENT "OS".
           ACCEPT WS-NUML FROM LINES
           ACCEPT WS-NUMC FROM COLUMNS
           IF  OS <> "Windows_NT"
              MOVE "LINUX" TO OS
           END-IF
           MOVE COR-FUNDO TO menu-bg.
           MOVE COR-TEXTO TO MENU-FG.
           perform varying k from 1 by 1 until k > NUMERO_ITENS
               MOVE ITENS(k) to ARRAY(k)
           end-perform.
           MOVE NUMERO_ITENS TO Item-Num.
           IF ITEM-SELECIONADO<=0 
               MOVE 1 TO SELECAO
           else 
               MOVE ITEM-SELECIONADO TO SELECAO
           END-IF
           MOVE 0 TO wInt.
           MOVE POS_Y TO box-pos-y.
           MOVE POS_X TO box-pos-x.
           MOVE 20 TO box-width.
           MOVE TIPO-MENU TO WS-TIPO-MENU.
           MOVE 0 TO doble-left-click.
           MOVE 0 TO atual-time.
           call static "curs_set" using by value wInt end-call.
           *> make mouse active
           COMPUTE COB-MOUSE-FLAGS = COB-AUTO-MOUSE-HANDLING
                      + COB-ALLOW-LEFT-DOWN   + COB-ALLOW-MIDDLE-DOWN   + COB-ALLOW-RIGHT-DOWN
                      + COB-ALLOW-LEFT-UP     + COB-ALLOW-MIDDLE-UP     + COB-ALLOW-RIGHT-UP
                      + COB-ALLOW-LEFT-DOUBLE + COB-ALLOW-MIDDLE-DOUBLE + COB-ALLOW-RIGHT-DOUBLE
                      + COB-ALLOW-MOUSE-MOVE
           SET environment "COB_MOUSE_FLAGS"  to COB-MOUSE-FLAGS.
           perform varying k from 1 by 1 until k > Item-Num  
               IF WS-TIPO-MENU = "H"
                   MOVE SPACES TO TEXTO-MENU
                   MOVE ARRAY(K) TO TEXTO-MENU
                   MOVE FUNCTION LENGTH(FUNCTION TRIM(TEXTO-MENU TRAILING)) TO WS-IDX
                   COMPUTE WS-IDX = WS-IDX + 1 END-COMPUTE
                   MOVE WS-IDX TO TAM-ITEM(K)
               ELSE
                   MOVE LENGTH OF ARRAY(1) TO TAM-ITEM(K)
               END-IF
           end-perform.
           PERFORM DISP-MENU THRU DISP-MENU-FIM.         
       STOP RUN.

       DISP-MENU. 
           IF WS-TIPO-MENU = "H" *> HORIZONTAL - PRINCIPAL
                  display WK-SPACES(POS_X:TAM-MENU-X) at line box-pos-y col box-pos-x with BACKGROUND-COLOR menu-bg end-display
                  *>display SPACES at line box-pos-y col box-pos-x with  Background-Color menu-bg Foreground-Color menu-fg end-display
                  compute calc-x = box-pos-x + 1  end-compute
                  MOVE box-pos-y TO calc-y
                  MOVE box-pos-y TO POS-ITEM-SEL-Y
                  perform varying k from 1 by 1 until k > Item-Num
                      MOVE SPACES TO TEXTO-MENU
                      MOVE ARRAY(K) TO TEXTO-MENU
                      IF SELECAO = K
                             display TEXTO-MENU(1:TAM-ITEM(K)) at line calc-y col calc-x  with  Background-Color COR-SEL-FUNDO Foreground-Color COR-SEL-TEXTO highlight end-display
                             MOVE calc-x TO POS-ITEM-SEL-X
                      else 
                             display TEXTO-MENU(1:TAM-ITEM(K)) at line calc-y col calc-x  with  Background-Color menu-bg Foreground-Color menu-fg end-display
                      end-if
                      compute calc-x = calc-x + TAM-ITEM(K) end-compute
                  end-perform
                  *> POSICAO DO CURSOR PARA RECEBER A INFORMACAO (JUNTO COM O ITEM)
                  compute calc-y = box-pos-y end-compute
                  compute calc-x = box-pos-x + 1 end-compute
           END-IF

           IF WS-TIPO-MENU = "V" *> MENU VERTICAL - PULLDOWN
                  MOVE box-pos-x TO WK-BOX-POS_X1
                  MOVE box-pos-y TO WK-BOX-POS_Y1
                  compute WK-BOX-POS_X2 = box-pos-x + box-width + 1 end-compute
                  compute WK-BOX-POS_Y2 = box-pos-y + Item-Num + 1 end-compute
                  CALL 'makebox' using BY REFERENCE WK-BOX-TIPO-BOX      *> box
                                       BY REFERENCE WK-BOX-TIPO-LINHA    *> linha simples
                                       BY REFERENCE WK-BOX-POS_X1        *> col 1
                                       BY REFERENCE WK-BOX-POS_Y1        *> lin 1
                                       BY REFERENCE WK-BOX-POS_X2        *> col 2
                                       BY REFERENCE WK-BOX-POS_Y2        *> lin 2
                                       BY REFERENCE menu-bg              *> cor fundo
                                       BY REFERENCE MENU-FG              *> cor frente
                  END-CALL
                  perform varying k from 1 by 1 until k > Item-Num
                      compute calc-y = box-pos-y + k end-compute
                      compute calc-x = box-pos-x + 1 end-compute
                      IF SELECAO = K
                             display ARRAY(K) at line calc-y col calc-x  with  Background-Color COR-SEL-FUNDO Foreground-Color COR-SEL-TEXTO  highlight end-display
                             MOVE calc-y TO POS-ITEM-SEL-Y
                      else 
                             display ARRAY(K) at line calc-y col calc-x  with  Background-Color menu-bg Foreground-Color menu-fg end-display
                      end-if
                  end-perform
                  *> POSICAO DO CURSOR PARA RECEBER A INFORMACAO (JUNTO COM O ITEM)
                  compute calc-y = box-pos-y + 1 end-compute
                  compute calc-x = box-pos-x + 1 end-compute
           END-IF
           move ARRAY(1) (1:1) to wPrompt
           move space to wDummy
           IF SELECAO = 1
               accept wDummy at line calc-y col calc-x 
               with auto-skip prompt character is wPrompt 
               with  Background-Color COR-SEL-FUNDO Foreground-Color COR-SEL-TEXTO  highlight end-accept
           else 
               accept wDummy at line calc-y col calc-x 
               with auto-skip prompt character is wPrompt 
               with  Background-Color menu-bg Foreground-Color menu-fg end-accept
           END-IF
           If wDummy = space
              IF wCRT-STATUS = K-ENTER  or doble-left-click>0
                 go DISP-MENU-FIM
              END-IF
              IF wCRT-STATUS = K-LEFT-DBL-CLICK or wCRT-STATUS = K-RIGHT-PRESSED
                   IF WS-TIPO-MENU='V'
                       COMPUTE calc-x = POS_X + LENGTH OF ARRAY(1) end-compute
                       COMPUTE calc-y = POS_Y + Item-Num + 1 end-compute
                       IF (wCursorCol > POS_X and wCursorCol<calc-x
                           and wCursorRow > POS_Y and wCursorRow < calc-y)
                           COMPUTE k = wCursorRow - POS_Y end-compute
                           if k = SELECAO
                                  if wCRT-STATUS = K-RIGHT-PRESSED
                                         accept wDummy at line CALC-Y col CALC-X with prompt character is wPrompt with  Background-Color menu-bg Foreground-Color menu-fg end-accept
                                  end-if
                            go DISP-MENU-FIM
                           end-if                           
                       END-IF
                    else 
                       MOVE POS_X TO WS-IDX
                       MOVE POS_Y TO calc-y
                       perform varying k from 1 by 1 until k > Item-Num
                              COMPUTE calc-x = WS-IDX + TAM-ITEM(K) end-compute
                              IF (wCursorCol >= WS-IDX and wCursorCol <= calc-x
                                  and wCursorRow >= POS_Y and wCursorRow <= calc-y)
                                  if k = SELECAO
                                         if wCRT-STATUS = K-RIGHT-PRESSED
                                             accept wDummy at line POS_Y col POS_X with prompt character is wPrompt with  Background-Color menu-bg Foreground-Color menu-fg end-accept
                                         end-if
                                         go DISP-MENU-FIM
                                 END-IF
                              END-IF
                              COMPUTE WS-IDX = WS-IDX + TAM-ITEM(K) END-COMPUTE
                       end-perform
                    END-If
              END-IF
              *> Escape or a Mouse Right Button = exit
              IF WCRT-STATUS = K-ESCAPE
                MOVE 0 TO SELECAO
                go DISP-MENU-FIM
              end-if
              EVALUATE TRUE
                     when wCRT-STATUS = K-RIGHT  *> Cursor Key right or down
                           or wCRT-STATUS = K-DOWN
                         IF SELECAO < Item-Num
                              compute SELECAO = SELECAO + 1 end-compute
                         END-IF
                     when wCRT-STATUS = K-LEFT
                          or wCRT-STATUS = K-UP
                         IF SELECAO > 1
                              compute SELECAO = SELECAO - 1 end-compute
                         END-IF
                     WHEN wCRT-STATUS = K-LEFT-PRESSED or wCRT-STATUS = K-LEFT-DBL-CLICK
                          IF WS-TIPO-MENU='V'
                              COMPUTE calc-x = POS_X + LENGTH OF ARRAY(1) end-compute
                              COMPUTE calc-y = POS_Y + Item-Num + 1 end-compute
                              IF (wCursorCol > POS_X and wCursorCol<calc-x
                                  and wCursorRow > POS_Y and wCursorRow < calc-y)
                                  COMPUTE SELECAO = wCursorRow - POS_Y end-compute
                                  IF one-click = SELECAO AND OS="LINUX"
                                   ACCEPT end-time FROM TIME
                                   compute diference = atual-time - end-time end-compute
                                   if diference<100 then 
                                       MOVE one-click TO doble-left-click
                                   else 
                                       move end-time to atual-time
                                   end-if
                                  else 
                                   ACCEPT atual-time FROM TIME
                                   MOVE 0 TO doble-left-click
                                  END-IF
                                  MOVE SELECAO TO one-click
                              END-IF
                              *>display "mouse: "wCursorRow" "wCursorCol  at line calc-y col calc-x  with  Background-Color menu-bg Foreground-Color menu-fg end-display
                              if wCRT-STATUS = K-LEFT-DBL-CLICK
                                  *> AGUARDA ALGUNS MILESEGUNDOS...
                                  call "CBL_GC_NANOSLEEP" USING 500000000 end-call
                              end-if
                           ELSE 
                              MOVE POS_X TO  WS-IDX
                              MOVE POS_Y TO calc-y
                              perform varying k from 1 by 1 until k > Item-Num
                                     COMPUTE calc-x = WS-IDX + TAM-ITEM(K) end-compute
                                     IF (wCursorCol >= WS-IDX and wCursorCol <= calc-x
                                         and wCursorRow >= POS_Y and wCursorRow <= calc-y)
                                         MOVE K TO SELECAO
                                         IF one-click = SELECAO AND OS="LINUX"
                                          ACCEPT end-time FROM TIME
                                          compute diference = atual-time - end-time end-compute
                                          if diference<100 then 
                                              MOVE one-click TO doble-left-click
                                          else 
                                              move end-time to atual-time
                                          end-if
                                         else 
                                          ACCEPT atual-time FROM TIME
                                          MOVE 0 TO doble-left-click
                                         END-IF
                                         MOVE SELECAO TO one-click
                                     end-if
                                     *>display "mouse: "wCursorRow" "wCursorCol  at line calc-y col calc-x  with  Background-Color menu-bg Foreground-Color menu-fg end-display
                                     if wCRT-STATUS = K-LEFT-DBL-CLICK
                                         *> AGUARDA ALGUNS MILESEGUNDOS...
                                         call "CBL_GC_NANOSLEEP" USING 500000000 end-call
                                     end-if
                                     COMPUTE WS-IDX = WS-IDX + TAM-ITEM(K) END-COMPUTE
                              end-perform
                           END-If
                    end-evaluate
           end-if
           go to DISP-MENU.
       DISP-MENU-FIM.
           move SELECAO to ITEM-SELECIONADO.
           move 2 to wInt.
           call static "curs_set" using by value wInt end-call.
           EXIT PROGRAM.
