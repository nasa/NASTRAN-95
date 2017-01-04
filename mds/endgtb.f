        SUBROUTINE ENDGTB ( BLOCK )                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 )                                                     
        NAME = BLOCK( 1)                                                        
        CALL DSGEFL                                                             
        ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
        IF ( ID .NE. IDSST ) CALL DSMSG ( 117 )                                 
        LEN = IAND ( IBASE( INDCBP ), MASKH2 ) * BLOCK( 11 )                    
        INDCBP = INDCBP - LEN - 2                                               
        CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
