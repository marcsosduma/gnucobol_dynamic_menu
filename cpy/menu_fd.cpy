*>----------------------------------------------------------------
*>                FILE DINAMIC_MENU.DAT                   
*>                                                                
*> Object.: Manage Menu                              
*>                                                                
*>----------------------------------------------------------------
*> Record.......: REC-MENU                       Record Length: 92
*> Primary Key..: KEY1-MENU (  5 Bytes )                       
*> Secundary Key: KEY2-MENU ( 52 Bytes ) WITH NO DUPLICATES    
*>----------------------------------------------------------------

FD  FD-MENU
    VALUE OF FILE-ID IS "../data/DINAMIC_MENU.DAT".

01  REC-MENU.
    05 KEY1-MENU.
       10 M-ID                 PIC 9(05).
       10 KEY2-MENU.
          15 M-MODULE          PIC X(15).
          15 M-NUMB-MENU       PIC 9(03). 
          15 M-ITEM-FATHER     PIC X(15).
          15 M-ORDER           PIC 9(03). 
          15 M-TYPE            PIC X(01).
          15 M-NAME            PIC X(15).
    05 M-DISPLAY-TEXT          PIC 9(20).
    05 M-ACTION                PIC 9(15).
*>----------------------------------------------------------------------
