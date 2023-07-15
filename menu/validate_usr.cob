       >>SOURCE FORMAT IS FREE
*>----------------------------------------------------------------------
*> Purpose: Dynamic menu example validation routine
*>
*>----------------------------------------------------------------------
*>  CHANGES HISTORY
*>  REQUEST      DATE     AUTHOR   DESCRIPTION
*>  DYNAMIC MENU 05/26/23 DUMA     Automatic menu in GNU COBOL.
*>----------------------------------------------------------------------
IDENTIFICATION DIVISION.
PROGRAM-ID.    validate_usr.
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
*>----------------------------------------------------------------------
77 WK-VALID PIC X.
    88 WK-USER-YES VALUES ARE "Y".
    88 WK-USER-NOT-OK VALUES ARE "N".
*> Variables for locating profiles -------------------------------------
77  WK-PROFILE_NUM                 PIC  9(003) VALUE ZEROS.
77  WK-PROFILE                     PIC  9(004) OCCURS 15 TIMES.
*> Variables for locating menu items -----------------------------------
77  WK-READ                       PIC  9(008) VALUE ZEROS.
77  WK-COUNT                      PIC  9(003) VALUE ZEROS.
77  WK-IDX                        PIC  9(003) VALUE ZEROS.
*> Buffer variables of user menu items ---------------------------------
77  WK-USR-ITENS                  PIC  X(600) VALUE SPACES. *> USR MENU ITEMS
77  WK-NUM-ITENS                  PIC  9(003) VALUE ZEROS.
*> Variables for locating menu items in the String Buffer --------------
01  WK-ITEN-PERM.
    03 WK-IT-PERM                 PIC  9(005) VALUE ZEROS.
    03 WK-SEP                     PIC  X VALUE "*".
01  WK-ITEN-SEARCH.
    03 WK-SEP1                    PIC  X VALUE "*".
    03 WK-IT-SEARCH               PIC  9(005) VALUE ZEROS.
    03 WK-SEP2                    PIC  X VALUE "*".
*> Variables to load menu items ----------------------------------------
01  WK-MENU-LOADED.
    03 LOADED-FATHER              PIC X(15).
    03 LOADED-TYPE                PIC X(01).
    03 LOADED-NAME                PIC X(15).
    03 LOADED-ORDER               PIC X(03).
    03 LOADED-DISPLAY             PIC 9(20).
    03 LOADED-ACTION              PIC 9(15).  

*> Lines for displaying the results ----------------------------------
77  WK-LIN                        PIC  9(002) VALUE ZEROS.
77  WK-LIN-FIX                    PIC  9(002) VALUE ZEROS.
*> File-status of files --------------------------------------------
77  FLG-EOF                       PIC  X(001) VALUE "F".
    88  EOF                                   VALUE "T".
77  ST-USR                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.
77  ST-PFU                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.
77  ST-PRF                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.
77  ST-PFM                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.
77  ST-MNU                        PIC  9(002).
    88  FSL-OK                                VALUE ZEROS.
    88  FSL-CANCEL                            VALUE 99.
    88  FSL-NOT-EXIST                         VALUE 35.
*> Constants -----------------------------------------------------------
01 ct-black   constant as 0.
01 ct-blue    constant as 1.
01 ct-green   constant as 2.
01 ct-cyan    constant as 3.
01 ct-red     constant as 4.
01 ct-magenta constant as 5.
01 ct-yellow  constant as 6.
01 ct-white   constant as 7.
*> Constants -----------------------------------------------------------

LINKAGE SECTION.
01  LK-USER    PIC X(20).
01  LK-MODULE  PIC X(15). *> DEFINE IF IT IS THE MAIN MENU OR ANOTHER MENU
01  LK-VALID   PIC X(01).
01  WK-ME.
   03 WK-MENU occurs 100 times.
       10 MENU-FATHER                   PIC X(15).
       10 MENU-TYPE                  PIC X(01).
       10 MENU-NAME                  PIC X(15).
       10 MENU-ORDER                 PIC X(03).
       10 MENU-DISPLAY               PIC 9(20).
       10 MENU-ACTION                PIC 9(15).  
01  WK-MENU-NUM-ITEM                 PIC 9(03).

*>----------------------------------------------------------------------
*>                    Main Program Module                     
*>----------------------------------------------------------------------
PROCEDURE DIVISION USING LK-USER LK-MODULE LK-VALID WK-ME WK-MENU-NUM-ITEM.

010-PROCESSING.

    MOVE "N" TO LK-VALID
    PERFORM 020-SEARCH-USER
    IF WK-USER-NOT-OK
       GO 010-EXIT-PROCESSING
    END-IF

    PERFORM 030-SEARCH-USER-PROFILE 
    IF WK-USER-NOT-OK
       GO 010-EXIT-PROCESSING
    END-IF

    PERFORM 050-SEARCH-PROFILE-MENU 
    IF WK-USER-NOT-OK
       GO 010-EXIT-PROCESSING
    END-IF

    PERFORM 060-LOAD-MENU 
    IF WK-USER-NOT-OK
       GO 010-EXIT-PROCESSING
    END-IF
    
    MOVE "Y" TO LK-VALID.
010-EXIT-PROCESSING.
    EXIT PROGRAM.
*>----------------------------------------------------------------------
020-SEARCH-USER.
    OPEN INPUT FD-USER 
    INITIALIZE REC-USR
    MOVE LK-USER TO U-LOGIN
    DISPLAY "VALIDATING USER..."  at line 04 column 01 with HIGHLIGHT FOREGROUND-COLOR ct-yellow
    START FD-USER
       KEY IS = KEY2-USR
       INVALID KEY
           DISPLAY "USER NOT FOUND: " line 05 column 01 LK-USER
           MOVE "N" TO WK-VALID
       NOT INVALID KEY
           READ FD-USER NEXT
           DISPLAY "USER FOUND:"  line 05 column 01 LK-USER
           MOVE "Y" TO WK-VALID
    END-START
    CLOSE FD-USER.
020-FIM-SEARCH-USER.
    EXIT.
*>----------------------------------------------------------------------
030-SEARCH-USER-PROFILE.
    OPEN INPUT FD-PFU 
    INITIALIZE REC-PFU
    MOVE 0 TO  WK-PROFILE_NUM
    MOVE U-ID-USR TO PFU-ID-USR
    MOVE 7 TO WK-LIN
    MOVE "N" TO WK-VALID
    DISPLAY "SEARCHING FOR USER PROFILES..." with HIGHLIGHT FOREGROUND-COLOR ct-yellow at line 06 column 01
    START FD-PFU
       KEY IS >= KEY1-PFU
       INVALID KEY           
           DISPLAY "NO USER PROFILES FOUND" line WK-LIN column 01
           MOVE "N" TO WK-VALID
       NOT INVALID KEY
           MOVE "F" TO FLG-EOF
           PERFORM UNTIL EOF            
               READ FD-PFU NEXT
                 AT END  
                    MOVE "T" TO FLG-EOF
                 NOT AT END
                   IF U-ID-USR = PFU-ID-USR
                       ADD 1 TO WK-PROFILE_NUM
                       IF WK-PROFILE_NUM=1 
                           DISPLAY "USER PROFILE FOUND: "  line WK-LIN column 01
                       END-IF 
                       MOVE PFU-ID-PRF TO WK-PROFILE(WK-PROFILE_NUM)
                       PERFORM 040-SEARCH-PROFILE
                       ADD 1 TO WK-LIN
                       MOVE "Y" TO WK-VALID
                   ELSE
                       MOVE "T" TO FLG-EOF
                       IF WK-USER-NOT-OK
                           DISPLAY "NO USER PROFILES FOUND" line WK-LIN column 01
                           CLOSE FD-PFU
                           GO 030-FIM-SEARCH-USER-PROFILE
                       END-IF                        
                   END-IF
               END-READ
           END-PERFORM
    END-START
    CLOSE FD-PFU.
030-FIM-SEARCH-USER-PROFILE.
    EXIT.
*>----------------------------------------------------------------------
040-SEARCH-PROFILE.
    OPEN INPUT FD-PRF 
    INITIALIZE REC-PRF
    MOVE PFU-ID-PRF TO PRF-ID
    START FD-PRF
       KEY IS = KEY1-PRF
       INVALID KEY
           DISPLAY "NO PROFILES FOUND" AT line WK-LIN column 19 
       NOT INVALID KEY
           READ FD-PRF NEXT
           DISPLAY PRF-NAME AT line WK-LIN column 19 
    END-START
    CLOSE FD-PRF.
040-FIM-SEARCH-PROFILE.
    EXIT.
*>----------------------------------------------------------------------
050-SEARCH-PROFILE-MENU.
    OPEN INPUT FD-PFM
    DISPLAY "LOOKING FOR USER FEATURES..." with HIGHLIGHT FOREGROUND-COLOR ct-yellow at line WK-LIN column 01
    ADD 1 TO WK-LIN
    MOVE WK-LIN TO WK-LIN-FIX
    MOVE "*" TO WK-USR-ITENS
    MOVE 0 TO WK-NUM-ITENS
    PERFORM VARYING WK-IDX FROM 1 BY 1 UNTIL WK-IDX > WK-PROFILE_NUM
       INITIALIZE REC-PFM
       MOVE 0 TO  WK-READ
       MOVE WK-PROFILE(WK-IDX) TO PFM-ID-PRF
       START FD-PFM
              KEY IS >= PFM-ID-PRF
              INVALID KEY
                  ADD 1 TO WK-LIN           
                  DISPLAY "NO FUNCTIONALITY FOUND " line WK-LIN-FIX column 01 WK-PROFILE(WK-IDX) 
                  MOVE "N" TO WK-VALID
              NOT INVALID KEY
                  MOVE "F" TO FLG-EOF
                  PERFORM UNTIL EOF            
                      READ FD-PFM NEXT
                        AT END  
                           MOVE "T" TO FLG-EOF
                        NOT AT END
                          IF WK-PROFILE(WK-IDX) = PFM-ID-PRF
                              MOVE 0 TO WK-COUNT
                              MOVE PFM-ID-MENU TO WK-IT-SEARCH
                              INSPECT WK-USR-ITENS TALLYING WK-COUNT FOR ALL WK-ITEN-SEARCH
                              IF WK-COUNT = 0
                                  MOVE "Y" TO WK-VALID
                                  ADD 1 TO WK-READ
                                  ADD 1 TO WK-NUM-ITENS
                                  MOVE PFM-ID-MENU TO WK-IT-PERM
                                  STRING  WK-USR-ITENS WK-ITEN-PERM DELIMITED BY SPACE INTO WK-USR-ITENS
                                  DISPLAY "FUNCTIONALITY FOUND: "  line WK-LIN-FIX column 01 WK-READ
                             END-IF
                          ELSE
                              MOVE "T" TO FLG-EOF
                          END-IF
                      END-READ
                  END-PERFORM
           END-START
    END-PERFORM 
    CLOSE FD-PFM.
050-FIM-SEARCH-PROFILE-MENU.
    EXIT.
*>----------------------------------------------------------------------
060-LOAD-MENU.
    OPEN INPUT FD-MENU
    ADD 1 TO WK-LIN
    DISPLAY "READING ITEMS FROM THE USER MENU..." with HIGHLIGHT FOREGROUND-COLOR ct-yellow at line WK-LIN column 01
    ADD 1 TO WK-LIN
    MOVE WK-LIN TO WK-LIN-FIX
    INITIALIZE REC-MENU
    MOVE 0 TO WK-MENU-NUM-ITEM
    MOVE LK-MODULE TO M-MODULE 
    MOVE SPACES TO M-ITEM-FATHER
    MOVE "N" TO WK-VALID
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
                    IF LK-MODULE = M-MODULE
                        MOVE 0 TO WK-COUNT
                        MOVE M-ID TO WK-IT-SEARCH
                        INSPECT WK-USR-ITENS TALLYING WK-COUNT FOR ALL WK-ITEN-SEARCH
                        IF WK-COUNT > 0
                            MOVE "Y" TO WK-VALID
                            MOVE PFM-ID-MENU TO WK-IT-PERM
                            ADD 1 TO WK-MENU-NUM-ITEM
                            MOVE M-ITEM-FATHER TO LOADED-FATHER
                            MOVE M-TYPE TO LOADED-TYPE
                            MOVE M-NAME TO LOADED-NAME
                            MOVE M-ORDER TO LOADED-ORDER
                            MOVE M-DISPLAY-TEXT TO LOADED-DISPLAY
                            MOVE M-ACTION TO LOADED-ACTION
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
060-FIM-LOAD-MENU.
    EXIT.
*>----------------------------------------------------------------------
