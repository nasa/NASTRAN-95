        SUBROUTINE DSBRC1                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
C        PRINT *,' DSBRC1-1,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS         
        IF ( INDCLR .NE. INDCBP ) GO TO 20                                      
        IF ( ( INDCLR-INDBAS ) .NE. 5 ) GO TO 10                                
        IF ( NBLOCK .EQ. 1 ) GO TO 100                                          
        CALL DSRDPB                                                             
        INDCBP = INDCBP - 1                                                     
        GO TO 100                                                               
10      INDCBP = INDCBP - 1                                                     
        GO TO 100                                                               
20      INDCBP = INDCLR                                                         
100     IF ( NBLOCK .NE. 1 ) GO TO 110                                          
        IF ( ( INDCLR-INDBAS ) .NE. 5 ) GO TO 110                               
        GO TO 7000                                                              
110     ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
        IF ( ID .EQ. IDSEF ) GO TO 7000                                         
        IF ( ID .NE. IDSRT ) GO TO 120                                          
        INDCBP = INDBAS + ( IAND( IBASE( INDCBP ), MASKH2 ) ) - 1               
        ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
120     IF ( ID .EQ. IDSRH ) GO TO 140                                          
        IF ( ID .EQ. IDSSB ) GO TO 140                                          
        IF ( ID .EQ. IDSEB ) GO TO 130                                          
        CALL DSMSG( 106 )                                                       
130     INDCBP = INDCBP - 1                                                     
        GO TO 100                                                               
140     IFLAG = IAND( IBASE( INDCBP ), MASKQ2 )                                 
        IF ( IFLAG .EQ. IDSC ) GO TO 7000                                       
        IF ( IFLAG .EQ. IDSP ) GO TO 7000                                       
        IF ( ( INDCBP-INDBAS ) .LE. 5 ) GO TO 150                               
        INDCBP = INDCBP - 1                                                     
        GO TO 100                                                               
150     IF ( NBLOCK .EQ. 1 ) GO TO 7000                                         
        CALL DSRDPB                                                             
        INDCBP = INDCBP - 1                                                     
        GO TO 100                                                               
7000    INDCLR = INDCBP                                                         
C        PRINT *,' DSBRC1-2,NBLOCK,INDCLR,INDBAS=',NBLOCK,INDCLR,INDBAS         
        RETURN                                                                  
        END                                                                     
