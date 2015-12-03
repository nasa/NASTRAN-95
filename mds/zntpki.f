        SUBROUTINE ZNTPKI                                                       
      INCLUDE 'DSIOF.COM'                                                       
        COMMON / ZNTPKX / A(4), I, IEOL, IENDRC                                 
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER            A                                                    
        IRETRN = 0                                                              
        I      = IBLKB( 4 )                                                     
        INDEX  = ( IBLKB(5)-1 )*IBLKB(14) + 1 + IBLKB(7)*IBLKB(11)              
        ITYPOT = IBLKB( 13 )                                                    
CDIR$ NOVECTOR                                                                  
        IF ( ITYPOT .NE. IBLKB(2) ) GO TO 50                                    
        NUM = NWRDEL( ITYPOT )                                                  
        DO 40 KK = 1, NUM                                                       
        A( KK ) = IBASE( INDEX + KK - 1 )                                       
40      CONTINUE                                                                
CDIR$ VECTOR                                                                    
        GO TO 70                                                                
50      CALL DSUPKC( IBLKB(2), ITYPOT, IBASE( INDEX ), A )                      
70      CONTINUE                                                                
        IBLKB( 4 ) = IBLKB( 4 ) + 1                                             
        IBLKB( 7 ) = IBLKB( 7 ) + 1                                             
        IBLKB(10 ) = IBLKB( 4 )                                                 
        IF ( IBLKB( 7 ) .LT. IBLKB( 6 ) ) GO TO 200                             
        CALL ENDGET( IBLKB )                                                    
        CALL GETSTR( *100, IBLKB )                                              
100     IBLKB( 7 ) = 0                                                          
200     CONTINUE                                                                
        IF ( IRETRN .NE. 0 ) GO TO 300                                          
        IEOL    = 0                                                             
        IENDRC  = 0                                                             
        GO TO 700                                                               
300     IEOL    = 1                                                             
        IENDRC  = 1                                                             
700     RETURN                                                                  
        END                                                                     
