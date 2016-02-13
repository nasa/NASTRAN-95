        SUBROUTINE DSEFWR                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        IF ( IPRVOP .NE. 0 ) GO TO 10                                           
        CALL DSMSG ( 7 )                                                        
10      IF ( INDCLR .EQ. INDCBP ) GO TO 20                                      
        IBASE( INDCBP+1 ) = IDSRT + IDSC + ( INDCLR-INDBAS+1 )                  
        INDCLR = INDCBP + 2                                                     
        INDCBP = INDCLR                                                         
20      IF ( ( INDCLR-INDBAS-2 ) .LT. NBUFF ) GO TO 30                          
        IBASE( INDCLR ) = IDSEB                                                 
        CALL DSWRNB                                                             
30      IBASE( INDCLR   ) = IDSEF                                               
        IBASE( INDCLR+1 ) = IDSEB                                               
        INDCLR = INDCLR + 1                                                     
        INDCBP = INDCLR                                                         
        IF ( ( INDCLR-INDBAS ) .LE. NBUFF ) GO TO 40                            
        CALL DSWRNB                                                             
40      RETURN                                                                  
        END                                                                     
