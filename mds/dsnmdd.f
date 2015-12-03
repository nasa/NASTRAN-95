      SUBROUTINE DSNMDD                                                         
      INCLUDE   'DSIOF.COM'                                                     
      INCLUDE   'NASNAMES.COM'                                                  
      CHARACTER*12 FILNAM(100)                                                  
C                                                                               
      COMMON /SYSTEM/ SYSBUF, IWR                                               
C                                                                               
      DATA FILNAM/'PUNCH','LINK' ,'LOG      ' ,'RDICT   ' , 'INPUT   '          
     *,'OUTPUT   ' , 'OPTP'    ,'NPTP.ZAP' ,'XPDT.ZAP' , 'PLOT    '          
     *,'UT1'      , 'UT2'     ,'UT3'      ,'UT4'      , 'UT5'              
     *,'INPT'     , 'INP1'    ,'INP2'     ,'INP3'     , 'INP4'             
     *,'INP5'     , 'POOL.ZAP','SCR23.ZAP','SCR24.ZAP','SCR25.ZAP'         
     *,'SCR26.ZAP','SCR27.ZAP','SCR28.ZAP','SCR29.ZAP','SCR30.ZAP'         
     *,'SCR31.ZAP','SCR32.ZAP','SCR33.ZAP','SCR34.ZAP','SCR35.ZAP'         
     *,'SCR36.ZAP','SCR37.ZAP','SCR38.ZAP','SCR39.ZAP','SCR40.ZAP'         
     *,'SCR41.ZAP','SCR42.ZAP','SCR43.ZAP','SCR44.ZAP','SCR45.ZAP'         
     *,'SCR46.ZAP','SCR47.ZAP','SCR48.ZAP','SCR49.ZAP','SCR50.ZAP'         
     *,'SCR51.ZAP','SCR52.ZAP','SCR53.ZAP','SCR54.ZAP','SCR55.ZAP'         
     *,'SCR56.ZAP','SCR57.ZAP','SCR58.ZAP','SCR59.ZAP','SCR60.ZAP'         
     *,'SCR61.ZAP','SCR62.ZAP','SCR63.ZAP','SCR64.ZAP','SCR65.ZAP'         
     *,'SCR66.ZAP','SCR67.ZAP','SCR68.ZAP','SCR69.ZAP','SCR70.ZAP'         
     *,'SCR71.ZAP','SCR72.ZAP','SCR73.ZAP','SCR74.ZAP','SCR75.ZAP'         
     *,'SCR76.ZAP','SCR77.ZAP','SCR78.ZAP','SCR79.ZAP','SCR80.ZAP'         
     *,'SCR81.ZAP','SCR82.ZAP','SCR83.ZAP','SCR84.ZAP','SCR85.ZAP'         
     *,'SCR86.ZAP','SCR87.ZAP','SCR88.ZAP','SCR89.ZAP','SCR90.ZAP'         
     *,'SCR91.ZAP','SCR92.ZAP','SCR93.ZAP','SCR94.ZAP','SCR95.ZAP'         
     *,'SCR96.ZAP','SCR97.ZAP','SCR98.ZAP','SCR99.ZAP','SCR00.ZAP'/        
      LENDIR = INDEX( DIRTRY, ' ' ) - 1
      DO 15 K = 1, 21                                                           
      MDSNAM( K ) = FILNAM(K)                                                   
15    CONTINUE                                                                  
      MDSNAM( 8 ) = DIRTRY(1:LENDIR) // '/' // FILNAM( 8 )                      
      MDSNAM( 9 ) = DIRTRY(1:LENDIR) // '/' // FILNAM( 9 )                      
      DO 20 K = 22, MAXFCB                                                      
      MDSNAM( K ) = DIRTRY(1:LENDIR) // '/' // FILNAM( K )                      
20    CONTINUE                                                                  
700   RETURN                                                                    
      END                                                                       

