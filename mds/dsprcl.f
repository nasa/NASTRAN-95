        SUBROUTINE DSPRCL ( BLOCK )                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 )                                                     
        INTEGER IDIV( 4 )                                                       
        DATA    IDIV / 1, 2, 1, 2 /                                             
        BLOCK(  2 ) = IAND( IBASE( INDCBP ), MASKQ4 )                           
        BLOCK(  3 ) = IAND( IBASE( INDCBP ), MASKQ3 )                           
        BLOCK(  3 ) = BLOCK( 3 ) / MULQ3                                        
        BLOCK( 11 ) = NWRDEL( BLOCK( 2 ) )                                      
        BLOCK( 12 ) = IBASE( INDCBP+1 )                                         
        BLOCK( 14 ) = IDIV( BLOCK( 2 ) )                                        
        RETURN                                                                  
        END                                                                     
