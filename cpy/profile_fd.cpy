*>----------------------------------------------------------------
*>                FILE PROFILE.DAT                   
*>                                                                
*> Object.: MANAGE PROFILE                              
*>                                                                
*>----------------------------------------------------------------
*> Record.: 01  REC-PRF.                 Record Length: 24
*> Primary Key  : KEY1-PRF     ( 04 Bytes )                       
*> Secundary Key: KEY2-PRF     ( 20 Bytes ) WITH NO DUPLICATES    
*>----------------------------------------------------------------

FD  FD-PRF
    VALUE OF FILE-ID IS "../data/PROFILE.DAT".

01  REC-PRF.
    05 KEY1-PRF.
       10 PRF-ID                  PIC 9(04).
    05 KEY2-PRF.
       15 PRF-NAME                PIC X(20).
*>----------------------------------------------------------------------
