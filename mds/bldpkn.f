      SUBROUTINE BLDPKN ( FILE, BLOCK,  MCB )                                   
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'DSIOF.COM'                                                       
      INTEGER FILE, BLOCK( 15 ), MCB( 7 )                                       
      NAME = FILE                                                               
      IF ( BLOCK( 1) .EQ. 0 ) GO TO 10                                          
      CALL DSBPNK ( BLOCK, MCB )                                                
      GO TO 20                                                                  
10    CALL DSBPNK ( IBLKA, MCB )                                                
20    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
