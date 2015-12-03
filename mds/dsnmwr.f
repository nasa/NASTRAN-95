          SUBROUTINE DSNMWR (IUNITU)                                            
C                                                                               
C         THIS SUBROUTINE IS CALLED BY ENDSYS                                   
C                                                                               
      INCLUDE 'DSIOF.COM'                                                       
      COMMON /GNDATE/ IGNDAT(2)                                                 
      WRITE (IUNITU) FCB, NUMDEV, DEV, MDSNAM, IGNDAT,MAXBLK,MAXDSK             
      WRITE (IUNITU) NUMOPN, NUMCLS, NUMWRI, NUMREA                             
      RETURN                                                                    
      END                                                                       
