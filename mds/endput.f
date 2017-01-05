      SUBROUTINE ENDPUT ( BLOCK )                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      INTEGER BLOCK( 15 )                                                     
      LIM = NBUFF - BLOCK( 3 )*2                                              
      NAME = BLOCK( 1 )                                                       
      CALL DSGEFL                                                             
      IF ( BLOCK( 7 ) .LE. 0 ) GO TO 10                                       
      IF ( BLOCK( 6 ) .GE. BLOCK( 7 ) ) GO TO 5                               
      CALL DSMSG1( BLOCK )                                                    
      CALL DSMSG ( 109 )                                                      
5     IBASE( INDCBP+1 ) = IDSSH  + BLOCK(7)                                   
      IBASE( INDCBP+2 ) = BLOCK(4)                                            
      NWORDS = BLOCK( 11 )                                                    
      INDCBP = INDCBP + ( BLOCK( 7 ) * NWORDS ) + 2                           
      IF ( ( INDCBP-INDBAS ) .GT. LIM ) CALL DSMSG( 108 )                     
      IF ( BLOCK( 3 ) .EQ. 0 ) GO TO 10                                       
      IBASE( INDCBP+1 ) = IDSST + BLOCK( 7 )                                  
      IBASE( INDCBP+2 ) = BLOCK(4) + BLOCK(7) - 1                             
      INDCBP = INDCBP + 2                                                     
10    IF ( BLOCK( 8 ) .NE. 1 ) GO TO 20                                       
      IBASE( INDCBP+1 ) = IDSCT +  BLOCK(3)*MULQ3 + BLOCK(2)                  
      IBASE( INDCBP+2 ) = BLOCK( 12 )                                         
      IBASE( INDCBP+3 ) = IDSRT + IDSC + ( INDCLR-INDBAS+1 )                  
      IBASE( INDCLR )   = IDSSB + BLOCK( 9 ) + INDCBP-INDCLR+2                
      INDCBP = INDCBP + 4                                                     
      INDCLR = INDCBP                                                         
20    IF ( BLOCK( 6 ) .NE. BLOCK( 7 ) ) GO TO 50                              
      IF ( BLOCK ( 8 ) .NE. 1 ) GO TO 30                                      
      IBASE( INDCBP ) = IDSEB                                                 
      GO TO 40                                                                
30    IFLG = BLOCK( 9 )                                                       
      IF ( IFLG .EQ. IDSX ) GO TO 45                                          
      IFLG = IDSP                                                             
      BLOCK( 9 ) = IDSX                                                       
45    IBASE( INDCLR ) = IDSSB + IFLG + ( INDCBP-INDCLR )                      
      IBASE( INDCBP + 1 ) = IDSRT + IFLG + ( INDCLR-INDBAS+1 )                
      IBASE( INDCBP + 2 ) = IDSEB                                             
      INDCLR = INDCBP + 2                                                     
      INDCBP = INDCLR                                                         
40    CALL DSWRNB                                                             
CWKBD  NCL93007 11/94  
C  50 CALL DSSDCB
CWKBNB NCL93007 11/94
C ACCUMULATE THE TOTAL NUMBER OF TERMS AND STRINGS
50    FCB( 16, IFILEX ) = FCB( 16, IFILEX ) + 1
      FCB( 17, IFILEX ) = FCB( 17, IFILEX ) + BLOCK( 7 )
      CALL DSSDCB                                                             
CWKBNE NCL93007 11/94
      RETURN                                                                  
      END                                                                     
