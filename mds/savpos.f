      SUBROUTINE SAVPOS ( FILE, IPOS )                                          
      INCLUDE 'DSIOF.COM'                                                       
      COMMON / DDIOSV / IFLPOS( 2,80 )                                          
      INTEGER          FILE                                                     
      NAME = FILE                                                               
      CALL DSGEFL                                                               
      IPOS = IFLPOS( 1,IFILEX )*MULQ2 + IFLPOS( 2, IFILEX )                     
      IF (IPRVOP .EQ. 0)                                                        
     &        IPOS = FCB(3,IFILEX)*MULQ2 + FCB(4,IFILEX)                        
      RETURN                                                                    
        END                                                                     
