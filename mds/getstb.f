        SUBROUTINE GETSTB ( *, BLOCK )                                          
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 )                                                     
        IRETRN = 0                                                              
        NAME   = BLOCK( 1 )                                                     
        CALL DSGEFL                                                             
        IF ( BLOCK( 8 ) .NE. -1 ) GO TO 100                                     
        IF ( ( INDCLR-INDBAS ) .GT. 5 ) GO TO 10                                
        CALL DSRDPB                                                             
10      INDCBP = INDCBP - 1                                                     
        ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
        IF ( ID .NE. IDSRT ) CALL DSMSG( 114 )                                  
        INDCBP = INDCBP - 2                                                     
        ID     = IAND( IBASE( INDCBP ), MASKQ1 )                                
        IF ( ID .NE. IDSCT ) CALL DSMSG ( 115 )                                 
        CALL DSPRCL( BLOCK )                                                    
        BLOCK( 8 ) = 0                                                          
100     INDCBP = INDCBP - 2                                                     
        IF ( ( INDCBP-INDBAS ) .GT. 5 ) GO TO 110                               
        CALL DSRDPB                                                             
        INDCBP = INDCBP + 1                                                     
        GO TO 100                                                               
110     ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
        IF ( ID .EQ. IDSCH ) GO TO 130                                          
        IF ( ID .EQ. IDSST ) GO TO 120                                          
        IF ( ID .EQ. IDSSH ) GO TO 100                                          
        IF ( ID .EQ. IDSRT ) GO TO 100                                          
        IF ( ID .EQ. IDSSD ) GO TO 100                                          
        IF ( ID .EQ. IDSSE ) GO TO 100                                          
CWKBNB 1/94                                                                     
        ID = IAND( IBASE( INDCBP+1 ), MASKQ1 )                                  
        IF ( ID .NE. IDSSD ) GO TO 116                                          
        INDCBP = INDCBP + 1                                                     
        GO TO 100                                                               
CWKBNE 1/94                                                                     
CWKBR 1/94  CALL DSMSG ( 116 )                                                  
116     CALL DSMSG ( 116 )                                                      
120     BLOCK( 4 ) = IBASE( INDCBP+1 )                                          
        BLOCK( 6 ) = IAND( IBASE( INDCBP ), MASKH2 )                            
        IDIV   = MIN0( 2, BLOCK( 11 ) )                                         
        BLOCK( 5 ) = INDCBP-1                                                   
        IF ( BLOCK( 2 ) .EQ. 2 ) BLOCK( 5 ) = ( INDCBP-1 ) / IDIV               
        IF ( BLOCK( 2 ) .EQ. 3 ) BLOCK( 5 ) =   INDCBP-2                        
        IF ( BLOCK( 2 ) .EQ. 4 ) BLOCK( 5 ) = ( INDCBP-3 ) / IDIV               
        GO TO 7000                                                              
130     INDCBP = INDCBP - 1                                                     
        INDCLR = INDCBP                                                         
        BLOCK( 8 ) = 1                                                          
        IRETRN = 1                                                              
7000    CALL DSSDCB                                                             
        IF ( IRETRN .EQ. 1 ) RETURN 1                                           
        RETURN                                                                  
        END                                                                     
