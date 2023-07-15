*>----------------------------------------------------------------
*>                FILE USER_PROFILE.DAT                   
*>                                                                
*> Object.: MANAGE USER PROFILE                              
*>                                                                
*>----------------------------------------------------------------
*> Record.: 01  REC-PFU.                 Record Length: 08
*> Primary Key.: KEY1-PFU     ( 08 Bytes )                       
*>----------------------------------------------------------------

FD  FD-PFU
    VALUE OF FILE-ID IS "../data/USER_PROFILE.DAT".

01  REC-PFU.
    05 KEY1-PFU.
       10 PFU-ID-USR              PIC 9(04).
       10 PFU-ID-PRF              PIC 9(04).
*>----------------------------------------------------------------------
