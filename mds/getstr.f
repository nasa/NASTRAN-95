        SUBROUTINE GETSTR ( *, BLOCK )                                          
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER BLOCK( 15 )                                                     
        IRETRN = 0                                                              
        NAME   = BLOCK( 1 )                                                     
        CALL DSGEFL                                                             
        IF ( BLOCK( 8 ) .NE. -1 ) GO TO 100                                     
10      IF ( ( INDCLR-INDBAS+1 ) .GT. LCW ) CALL DSMSG( 113 )                   
        ID = IAND( IBASE( INDCLR ), MASKQ1 )                                    
        IF ( ID .EQ. IDSSB ) GO TO 30                                           
        IF ( ID .EQ. IDSEB ) GO TO 20                                           
        CALL DSMSG ( 110 )                                                      
20      CALL DSRDNB                                                             
        GO TO 10                                                                
30      ID = IAND( IBASE( INDCBP+1 ), MASKQ1 )                                  
        IF ( ID .EQ. IDSCH ) GO TO 40                                           
        CALL DSMSG1( BLOCK )                                                    
        CALL DSMSG( 111 )                                                       
40      CONTINUE                                                                
        INDCBP = INDCBP + 1                                                     
        CALL DSPRCL( BLOCK )                                                    
        INDCBP = INDCBP + 2                                                     
        BLOCK( 8 ) = 0                                                          
100     ID = IAND( IBASE( INDCBP ), MASKQ1 )                                    
        INDCBP = INDCBP + 1                                                     
        IF ( ID .EQ. IDSSH ) GO TO 130                                          
        IF ( ID .EQ. IDSSD ) GO TO 100                                          
        IF ( ID .EQ. IDSCT ) GO TO 120                                          
        IF ( ID .EQ. IDSSE ) GO TO 110                                          
        IF ( ID .EQ. IDSRT ) GO TO 110                                          
        CALL DSMSG ( 112 )                                                      
110     CALL DSRDNB                                                             
        INDCBP = INDCBP + 1                                                     
        GO TO 100                                                               
120     CALL DSSKRC                                                             
        BLOCK( 6 ) = 0                                                          
        BLOCK( 8 ) = 1                                                          
        IRETRN     = 1                                                          
        GO TO 7000                                                              
130     BLOCK( 4 ) = IBASE( INDCBP )                                            
        BLOCK( 6 ) = IAND( IBASE( INDCBP-1 ), MASKH2 )                          
        INDCBP     = INDCBP + 1                                                 
        BLOCK( 5 ) = ( INDCBP-1 ) / BLOCK( 14 ) + 1                             
7000    CALL DSSDCB                                                             
        IF ( IRETRN .EQ. 1 ) RETURN 1                                           
        RETURN                                                                  
        END                                                                     
