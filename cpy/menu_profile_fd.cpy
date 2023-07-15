*>----------------------------------------------------------------
*>                FILE MENU_PROFILE.DAT                   
*>                                                                
*> Object.: Manage the relationship table MENU PROFILE                         
*>                                                                
*>----------------------------------------------------------------
*> Record.: 01  REC-PFM.                 Record length: 09
*> Primary Key: KEY1-PFM                 ( 09 Bytes )                       
*>----------------------------------------------------------------

FD  FD-PFM
    VALUE OF FILE-ID IS "../data/MENU_PROFILE.DAT".

01  REC-PFM.
    05 KEY1-PFM.
       10 PFM-ID-PRF              PIC 9(04).
       10 PFM-ID-MENU             PIC 9(05).
*>----------------------------------------------------------------------
