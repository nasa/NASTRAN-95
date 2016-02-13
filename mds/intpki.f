        SUBROUTINE INTPKI ( A, I, FILE, BLOCK, IEOL )                           
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER A(4), BLOCK(15), FILE                                           
        NAME   = FILE                                                           
        IRETRN = 0                                                              
        I = BLOCK( 4 )                                                          
        INDEX  = ( BLOCK(5)-1 )*BLOCK(14) + 1 + BLOCK(7)*BLOCK(11)              
        ITYPOT = BLOCK( 13 )                                                    
        IF ( BLOCK(2) .NE. ITYPOT ) GO TO 50                                    
        NUM = NWRDEL( ITYPOT )                                                  
CDIR$ NOVECTOR                                                                  
        DO 40 KK = 1, NUM                                                       
        A( KK ) = IBASE( INDEX + KK - 1 )                                       
40      CONTINUE                                                                
CDIR$ VECTOR                                                                    
        GO TO 60                                                                
50      CALL DSUPKC( BLOCK(2), ITYPOT, IBASE( INDEX ), A )                      
60      CONTINUE                                                                
        BLOCK( 4 ) = BLOCK( 4 ) + 1                                             
        BLOCK( 7 ) = BLOCK( 7 ) + 1                                             
        BLOCK(10 ) = BLOCK( 4 )                                                 
        IF ( BLOCK( 7 ) .LT. BLOCK( 6 ) ) GO TO 200                             
        CALL ENDGET( BLOCK )                                                    
        CALL GETSTR( *100, BLOCK )                                              
100     BLOCK( 7 ) = 0                                                          
200     CONTINUE                                                                
        IF ( IRETRN .NE. 0 ) GO TO 300                                          
        IEOL = 0                                                                
        GO TO 700                                                               
300     IEOL = 1                                                                
700     RETURN                                                                  
        END                                                                     
