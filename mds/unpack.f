        SUBROUTINE UNPACK ( *, FILE, A )                                        
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        COMMON / UNPAKX / ITYPOT, IROBGN, LASROW, INCR                          
        INTEGER           FILE                                                  
        REAL              A(4), IBASE                                           
        DATA              LARGE / 65536 /                                       
        NAME = FILE                                                             
        NUM    = NWRDEL( IABS( ITYPOT ) )                                       
        CALL DSIPK1( IBLKD, ITYPOT )                                            
        IF ( IRETRN .EQ. 1 ) GO TO 7700                                         
        IF ( IROBGN .LE. 0 .OR. LASROW .LE. 0 ) GO TO 10                        
        IROW   = IROBGN                                                         
        ILSROW = LASROW                                                         
        GO TO 20                                                                
10      IROBGN = IBLKD( 4 )                                                     
        IROW   = IROBGN                                                         
        ILSROW = LARGE                                                          
20      CONTINUE                                                                
        INDEX2 = 1                                                              
        ITYPE  = IBLKD( 13 )                                                    
        INDEX1 = ( IBLKD( 5 ) - 1 ) * IBLKD( 14 ) + 1                           
        NUMINC = NUM * INCR                                                     
90      IF ( IBLKD( 4 ) .GT. ILSROW ) GO TO 200                                 
        IF ( ( IBLKD( 4 ) + IBLKD( 6 ) - 1 ) .LT. IROBGN ) GO TO 145            
100     IDIFF = ( IBLKD( 4 ) + IBLKD( 7 ) ) - IROW                              
        IBLKD( 7 ) = IBLKD( 7 ) + 1                                             
        IF ( IDIFF .EQ. 0 ) GO TO 140                                           
        IF ( IDIFF .LT. 0 ) GO TO 142                                           
        DO 130 K = 1, NUM                                                       
        DO 110   KKK = 1, IDIFF                                                 
        A( INDEX2 + K - 1 + (KKK-1)*NUMINC ) = 0.                               
110     CONTINUE                                                                
130     CONTINUE                                                                
        INDEX2 = INDEX2 + IDIFF*NUMINC                                          
        IROW   = IROW + IDIFF                                                   
140     IF ( IBLKD(2) .NE. ITYPE ) GO TO 1400                                   
CDIR$ NOVECTOR                                                                  
        DO 141 K = 1, NUM                                                       
        A( INDEX2 + K - 1 ) = IBASE( INDEX1 + K - 1 )                           
141     CONTINUE                                                                
CDIR$ VECTOR                                                                    
        GO TO 1401                                                              
1400    CALL DSUPKC( IBLKD( 2 ), ITYPE, IBASE( INDEX1 ), A( INDEX2 ) )          
1401    CONTINUE                                                                
        IF ( IROW .GE. ILSROW ) GO TO 225                                       
        IROW = IROW + 1                                                         
        INDEX2 = INDEX2 + NUMINC                                                
142     INDEX1 = INDEX1 + IBLKD( 11 )                                           
        IF ( IBLKD( 7 ) .NE. IBLKD( 6 ) ) GO TO 100                             
145     CALL ENDGET( IBLKD )                                                    
        CALL GETSTR( *200, IBLKD )                                              
        INDEX1 = ( IBLKD( 5 ) - 1 ) * IBLKD( 14 ) + 1                           
        IBLKD( 7 ) = 0                                                          
150     IF ( IBLKD( 8 ) .LT. 1 ) GO TO 90                                       
200     IF ( ILSROW .EQ. LARGE ) GO TO 230                                      
        NUMLEF = LASROW - IROW + 1                                              
        IF ( NUMLEF .LE. 0 ) GO TO 225                                          
        DO 220 KK = 1, NUM                                                      
        DO 210 K  = 1, NUMLEF                                                   
        A( INDEX2 + KK - 1 + (K-1)*NUMINC ) = 0.                                
210     CONTINUE                                                                
220     CONTINUE                                                                
225     IF ( IBLKD( 8 ) .GE. 1 ) GO TO 240                                      
        CALL DSSKRC                                                             
        CALL DSSDCB                                                             
        GO TO 240                                                               
230     LASROW = IROW - 1                                                       
240     RETURN                                                                  
7700    RETURN 1                                                                
        END                                                                     
