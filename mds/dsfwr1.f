      SUBROUTINE DSFWR1                                                         
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      CALL DSSKRC                                                               
      ID     = IAND( IBASE( INDCLR-1 ), MASKQ1 )                                
      IF ( ID .EQ. IDSEF ) GO TO 10                                             
      IRETRN = 0                                                                
      GO TO 7000                                                                
10    IRETRN = 1                                                                
7000  RETURN                                                                    
      END                                                                       
