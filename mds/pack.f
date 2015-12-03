        SUBROUTINE PACK ( A, FILE, MCB )                                        
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'XNSTRN.COM'                                                      
        COMMON / PACKX  / ITYPIN, ITYPOT, IROBGN, LASROW, INCR                  
        COMMON / DDIOSV / IFLPOS( 2,80 )                                        
        INTEGER           FILE,   MCB(7)                                        
        INTEGER           A(4)                                                  
        NAME       = FILE                                                       
        IBLKC( 1 ) = NAME                                                       
        IBLKC( 2 ) = ITYPOT                                                     
        IBLKC( 3 ) = 0                                                          
        IBLKC( 4 ) = 0                                                          
        IBLKC( 7 ) = 0                                                          
        IBLKC( 8 ) = -1                                                         
        IBLKC( 9 ) = ITYPIN                                                     
        IBLKC(10 ) = 0                                                          
        IF ( ITYPIN .LE. 0 .OR. ITYPIN .GT. 4 ) GO TO 10                        
        IF ( ITYPOT .LE. 0 .OR. ITYPOT .GT. 4 ) GO TO 10                        
        GO TO 20                                                                
10      CALL DSMSG1( IBLKC )                                                    
        CALL DSMSG( 118 )                                                       
20      NWDIN      = NWRDEL( ITYPIN )                                           
        IBLKC( 12) = MCB( 2 ) + 1                                               
        CALL DSGEFL                                                             
        IFLPOS( 1,IFILEX ) = FCB( 3, IFILEX )                                   
        IFLPOS( 2,IFILEX ) = FCB( 4, IFILEX )                                   
        CALL PUTSTR( IBLKC )                                                    
        IEOR       = 0                                                          
        INDEXA     = 0                                                          
        IROW       = IROBGN                                                     
        INDEXB     = ( IBLKC( 5 ) - 1 ) * IBLKC( 14 ) + 1                       
CDIR$ NOVECTOR                                                                  
100     DO 110 K = 1, NWDIN                                                     
        IF ( A( INDEXA+K ) .NE. 0 ) GO TO 120                                   
110     CONTINUE                                                                
CDIR$ VECTOR                                                                    
        LASIND = (LASROW-IROW+1)*INCR*NWDIN                                     
        KLIM   = LASIND + INCR                                                  
        KLAST  = KLIM                                                           
        INCRR = INCR*NWDIN                                                      
        DO 116 KK = 1, NWDIN                                                    
        INDEA1 = INDEXA - 1 + KK                                                
        DO 115 K  = 1, LASIND, INCRR                                            
        IF ( A(INDEA1 + K) .EQ. 0 ) GO TO 115                                   
        IF ( K .LT. KLAST ) KLAST = K                                           
        GO TO 116                                                               
115     CONTINUE                                                                
116     CONTINUE                                                                
        NCNT = (( KLAST-1 ) / INCRR) - 1                                        
        IF ( KLAST .EQ. KLIM ) NCNT = LASROW - IROW                             
        IROW = IROW + NCNT                                                      
        INDEXA= INDEXA + NCNT*(NWDIN*INCR)                                      
        IEOR       = 1                                                          
        GO TO 150                                                               
120     IF ( IBLKC( 7 ) .EQ. 0 ) GO TO 130                                      
        IF ( IEOR .EQ. 0 ) GO TO 140                                            
        CALL ENDPUT( IBLKC )                                                    
        CALL PUTSTR( IBLKC )                                                    
        IBLKC( 7 ) = 0                                                          
        INDEXB     = ( IBLKC( 5 ) - 1 ) * IBLKC( 14 ) + 1                       
130     IBLKC( 4 ) = IROW                                                       
140     IF ( ITYPIN .NE. ITYPOT ) GO TO 1400                                    
CDIR$ NOVECTOR                                                                  
        DO 141 K = 1, NWDIN                                                     
        IBASE( INDEXB + K - 1 ) = A( INDEXA + K )                               
141     CONTINUE                                                                
CDIR$ VECTOR                                                                    
        GO TO 1401                                                              
1400    CALL DSUPKC( ITYPIN, ITYPOT, A( INDEXA+1 ), IBASE( INDEXB ))            
1401    CONTINUE                                                                
        IEOR       = 0                                                          
        INDEXB     = INDEXB + IBLKC( 11 )                                       
        IBLKC( 7 ) = IBLKC( 7 ) + 1                                             
        IBLKC(10 ) = IBLKC( 10 ) + IBLKC( 11 )                                  
        IF ( IBLKC( 7 ) .LT. IBLKC( 6 ) ) GO TO 150                             
        CALL ENDPUT( IBLKC )                                                    
        CALL PUTSTR( IBLKC )                                                    
        IBLKC( 7 ) = 0                                                          
        INDEXB = ( IBLKC( 5 ) - 1 ) * IBLKC( 14 ) + 1                           
150     INDEXA = INDEXA + ( INCR*NWDIN )                                        
        IROW   = IROW + 1                                                       
        IF ( IROW .LE. LASROW ) GO TO 100                                       
        CALL DSBPNK( IBLKC, MCB )                                               
        RETURN                                                                  
        END                                                                     
