        SUBROUTINE DSSKRC                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
10      ID = IAND( IBASE( INDCLR ), MASKQ1 )                                    
        IF ( ID .EQ. IDSRH ) GO TO 40                                           
        IF ( ID .EQ. IDSSB ) GO TO 40                                           
        IF ( ID .EQ. IDSEF ) GO TO 30                                           
        IF ( ID .EQ. IDSEB ) GO TO 20                                           
        CALL DSMSG ( 103 )                                                      
20      CALL DSRDNB                                                             
        GO TO 10                                                                
30      INDCLR = INDCLR + 1                                                     
        INDCBP = INDCLR                                                         
        GO TO 7000                                                              
40      LEN  = IAND( IBASE( INDCLR ), MASKH2 )                                  
        ICLR = INDCLR + LEN + 1                                                 
        ID   = IAND( IBASE( ICLR ), MASKQ1 )                                    
        IF ( ID .EQ. IDSRT ) GO TO 50                                           
        CALL DSMSG ( 104 )                                                      
50      IFLG = IAND( IBASE( ICLR ), MASKQ2 )                                    
        IF ( IFLG .EQ. IDSC ) GO TO 60                                          
        CALL DSRDNB                                                             
        GO TO 10                                                                
60      INDCLR = ICLR +  1                                                      
        INDCBP = INDCLR                                                         
7000    RETURN                                                                  
        END                                                                     
