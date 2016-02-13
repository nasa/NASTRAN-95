      SUBROUTINE DSNMRD (IUNITU)                                                
      INCLUDE  'DSIOF.COM'                                                      
      COMMON /GNDATE/ IGNDAT(2)                                                 
      READ (IUNITU) FCB, NUMDEV, DEV, MDSNAM, IGNDAT, MAXBLK, MAXDSK            
      READ (IUNITU) NUMOPN, NUMCLS, NUMWRI, NUMREA                              
      DO 10 I = 1, 80                                                           
      FCB( 9, I ) = 0                                                           
      FCB(10, I ) = 0                                                           
      FCB(11, I ) = 0                                                           
10    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
