        SUBROUTINE BLDPKI ( A, I, FILE, BLOCK )                                 
      INTEGER BLOCK(15), A(4), FILE                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      NAME   = FILE                                                             
      BLOCK( 15 ) = I                                                           
      ITYPIN = BLOCK( 13 )                                                      
      NWORDS = NWRDEL( ITYPIN )                                                 
      IF ( BLOCK( 2 ) .GE. 3 ) GO TO 5                                          
      INCCNT = 1                                                                
      GO TO 8                                                                   
5     INCCNT = 2                                                                
8     CONTINUE                                                                  
      DO 10 K = 1, NWORDS                                                       
      IF ( A( K ) .NE. 0 ) GO TO 20                                             
10    CONTINUE                                                                  
      GO TO 7000                                                                
20    IF ( BLOCK( 4 ) .EQ. 0 ) GO TO 35                                         
      NEXROW = BLOCK( 4 ) + BLOCK( 7 )                                          
      ICROW  = BLOCK( 15 )                                                      
      IF ( ICROW .GE. NEXROW ) GO TO 30                                         
      CALL DSMSG1( BLOCK )                                                      
      CALL DSMSG( 119 )                                                         
30    IF ( ICROW .EQ. NEXROW ) GO TO 40                                         
      CALL ENDPUT( BLOCK )                                                      
      CALL PUTSTR( BLOCK )                                                      
      BLOCK( 7 ) = 0                                                            
35    ICROW = BLOCK( 15 )                                                       
      BLOCK( 4 ) = ICROW                                                        
40    INDEX  = ( BLOCK( 5 ) - 1  ) * BLOCK( 14 ) + 1                            
      IF ( ITYPIN .NE. BLOCK( 2 ) ) GO TO 100                                   
CDIR$ NOVECTOR                                                                  
      DO 70 KK = 1, NWORDS                                                      
      IBASE( INDEX + KK - 1 ) = A( KK )                                         
70    CONTINUE                                                                  
CDIR$ VECTOR                                                                    
      GO TO 200                                                                 
100   CALL DSUPKC ( ITYPIN, BLOCK( 2 ), A, IBASE( INDEX ) )                     
200   CONTINUE                                                                  
      BLOCK( 5 ) = BLOCK( 5 ) + INCCNT                                          
      BLOCK( 7 ) = BLOCK( 7 ) + 1                                               
      BLOCK(10 ) = BLOCK(10 ) + BLOCK( 11 )                                     
      IF ( BLOCK( 6 ) .GT. BLOCK( 7 ) ) GO TO 7000                              
      CALL ENDPUT( BLOCK )                                                      
      CALL PUTSTR( BLOCK )                                                      
      BLOCK( 4 ) = 0                                                            
      BLOCK( 7 ) = 0                                                            
7000  RETURN                                                                    
        END                                                                     
