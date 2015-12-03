        SUBROUTINE DSRLSE                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
      INEXT  = IFILEX                                                           
10    NEXDSN = IAND( MDSFCB( 3,INEXT ), MASKH2 )                                
      IF ( NEXDSN .EQ. 0 ) GO TO 20                                             
      MDSFCB( 1, INEXT ) = IAND( MDSFCB( 1,INEXT ), MASKH1 )                    
      MDSFCB( 2, INEXT ) = 0                                                    
      MDSFCB( 3, INEXT ) = 0                                                    
C                                                                               
C OPEN AND CLOSE FILE TO DELETE SPACE ALLOCATION                                
C                                                                               
      CALL DSOPEN ( MDSNAM(NEXDSN), NEXDSN, 1 )                                 
      CALL DSCLOS ( NEXDSN )                                                    
      INEXT  = NEXDSN                                                           
      GO TO 10                                                                  
20    RETURN                                                                    
        END                                                                     
