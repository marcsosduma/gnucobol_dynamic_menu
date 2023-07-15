*>----------------------------------------------------------------
*>                FILE USUARIO.DAT                   
*>                                                                
*> Objetivo.: Manage User                              
*>                                                                
*>----------------------------------------------------------------
*> Record.: REC-USR                   Record length: 24
*> Primary Key  : KEY1-USR ( 04 Bytes )                       
*> Secundary Key: KEY2-USR ( 10 Bytes ) WITH NO DUPLICATES    
*>----------------------------------------------------------------

FD  FD-USER
    VALUE OF FILE-ID IS "../data/USER.DAT".

01  REC-USR.
    05 KEY1-USR.
       10 U-ID-USR             PIC 9(04).
       10 KEY2-USR.
          15 U-LOGIN           PIC X(10).
    05 U-PASSWD                PIC X(10).
*>----------------------------------------------------------------------
