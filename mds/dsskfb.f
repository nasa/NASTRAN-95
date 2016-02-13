        SUBROUTINE DSSKFB( NN )                                                 
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        N = NN                                                                  
10      IF ( N .EQ. 0 ) GO TO 7000                                              
20      CALL DSBRC1                                                             
        ID = IAND( IBASE( INDCLR ), MASKQ1 )                                    
        IF ( ID .EQ. IDSEF ) GO TO 30                                           
        IF ( NBLOCK .NE. 1 ) GO TO 20                                           
        IF ( ( INDCLR-INDBAS ) .LE. 5 ) GO TO 7000                              
        GO TO 20                                                                
30      N = N + 1                                                               
        GO TO 10                                                                
7000    RETURN                                                                  
        END                                                                     
