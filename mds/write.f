      SUBROUTINE WRITE ( FILE, IDATA, N, EORFLG )                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      COMMON / DDIOSV / IFLPOS( 2,80 )                                        
      INTEGER  FILE, EORFLG, IDATA( 2 )                                       
      IEOR   = EORFLG                                                         
      NAME   = FILE                                                           
      NWORDS = N                                                              
      IF ( NWORDS .GE. 0 ) GO TO 10                                           
      CALL DSMSG( 6 )                                                         
10    CALL DSGEFL                                                             
      IF ( NWORDS .EQ. 0 ) GO TO 25                                           
      IF ( IPRVOP .NE. 0 ) GO TO 20                                           
      CALL DSMSG( 7 )                                                         
20    IF ( INDCLR .NE. INDCBP ) GO TO 30                                      
      IFLPOS( 1,IFILEX ) = FCB( 3, IFILEX )                                   
      IFLPOS( 2,IFILEX ) = FCB( 4, IFILEX )                                   
      IBASE( INDCLR ) = IDSRH + IDSC                                          
      IBLOCK = NBLOCK                                                         
      IBASE( INDBAS + NBUFF + 2 ) = NBLOCK                                    
      GO TO 40                                                                
25    IF ( INDCBP .NE. INDCLR ) GO TO 70                                      
      LWORDS = NBUFF - ( INDCLR - INDBAS ) - 2                                
      IF ( LWORDS .GT. 0 ) GO TO 70                                           
      IBASE( INDCLR ) = IDSEB                                                 
      CALL DSWRNB                                                             
      IBASE( INDCLR ) = IDSRH + IDSC                                          
      IBASE( INDCLR + 1 ) = IDSRT + IDSC + ( INDCLR - INDBAS + 1 )            
      INDCBP = INDCBP + 2                                                     
      INDCLR = INDCBP                                                         
      GO TO 80                                                                
30    IBLOCK = IBASE( INDBAS + NBUFF + 2 )                                    
40    LWORDS = NBUFF - ( INDCBP-INDBAS )                                      
      IF ( LWORDS .GE. NWORDS ) GO TO 50                                      
      CALL DSWRT1( IDATA )                                                    
CWKBI SPR94013 11/94
      IBASE( INDBAS + NBUFF + 2 ) = IBLOCK 
      GO TO 80                                                                
50    DO 60 I = 1, NWORDS                                                     
      IBASE( INDCBP + I  ) = IDATA( I )                                       
60    CONTINUE                                                                
      IBASE( INDCLR ) = IBASE( INDCLR ) + NWORDS                              
      INDCBP = INDCBP + NWORDS                                                
70    IF ( IEOR .EQ. 0 ) GO TO 80                                             
      IF ( INDCBP .NE. INDCLR ) GO TO 75                                      
      IBASE( INDCBP ) = IDSRH + IDSC                                          
      IFLPOS( 1,IFILEX ) = FCB( 3, IFILEX )                                   
      IFLPOS( 2,IFILEX ) = FCB( 4, IFILEX )                                   
75    IBASE( INDCBP+1 ) = IDSRT + IDSC + ( INDCLR-INDBAS+1 )                  
      INDCBP = INDCBP + 2                                                     
      INDCLR = INDCBP                                                         
80    CALL DSSDCB                                                             
      RETURN                                                                  
      END                                                                     
