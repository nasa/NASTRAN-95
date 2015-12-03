        SUBROUTINE ENDGET( BLOCK )                                              
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 )                                                     
        NAME = BLOCK( 1 )                                                       
        CALL DSGEFL                                                             
        NWORDS = BLOCK( 11 )                                                    
        NELM = IAND( IBASE( INDCBP-2 ), MASKH2 )                                
        INDCBP = INDCBP + NELM*NWORDS + BLOCK(3)*2                              
        CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
