        SUBROUTINE ZBLPKI                                                       
        INTEGER A                                                               
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'XNSTRN.COM'                                                      
        COMMON / ZBLPKX / A(4), I                                               
        IBLKA(15) = I                                                           
        ITYPIN = IBLKA( 13 )                                                    
        NWORDS = NWRDEL( ITYPIN )                                               
        IF ( IBLKA( 2 ) .GE. 3 ) GO TO 5                                        
        INCCNT = 1                                                              
        GO TO 8                                                                 
5       INCCNT = 2                                                              
8       CONTINUE                                                                
        DO 10 K = 1, NWORDS                                                     
        IF ( A( K ) .NE. 0 ) GO TO 20                                           
10      CONTINUE                                                                
        GO TO 7000                                                              
20      IF ( IBLKA( 4 ) .EQ. 0 ) GO TO 35                                       
        NEXROW = IBLKA( 4 ) + IBLKA( 7 )                                        
        ICROW  = IBLKA( 15 )                                                    
        IF ( ICROW .GE. NEXROW ) GO TO 30                                       
        CALL DSMSG1( IBLKA )                                                    
        CALL DSMSG( 119 )                                                       
30      IF ( ICROW .EQ. NEXROW ) GO TO 40                                       
        CALL ENDPUT( IBLKA )                                                    
        CALL PUTSTR( IBLKA )                                                    
        IBLKA( 7 ) = 0                                                          
35      ICROW = IBLKA( 15 )                                                     
        IBLKA( 4 ) = ICROW                                                      
40      INDEX  = ( IBLKA( 5 ) - 1  ) * IBLKA( 14 ) + 1                          
        IF ( ITYPIN .NE. IBLKA(2) ) GO TO 100                                   
CDIR$ NOVECTOR                                                                  
        DO 50 KK = 1, NWORDS                                                    
        IBASE( INDEX + KK - 1 ) = A( KK )                                       
50      CONTINUE                                                                
CDIR$ VECTOR                                                                    
        GO TO 200                                                               
100     CALL DSUPKC ( ITYPIN, IBLKA( 2 ), A, IBASE( INDEX ) )                   
200     CONTINUE                                                                
        IBLKA( 5 ) = IBLKA( 5 ) + INCCNT                                        
        IBLKA( 7 ) = IBLKA( 7 ) + 1                                             
        IBLKA(10 ) = IBLKA(10 ) + IBLKA( 11 )                                   
        IF ( IBLKA( 6 ) .GT. IBLKA( 7 ) ) GO TO 7000                            
        CALL ENDPUT( IBLKA )                                                    
        CALL PUTSTR( IBLKA )                                                    
        IBLKA( 4 ) = 0                                                          
        IBLKA( 7 ) = 0                                                          
7000    RETURN                                                                  
        END                                                                     
