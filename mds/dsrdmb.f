        SUBROUTINE DSRDMB ( IDATA, M )                                          
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER           IDATA(2)                                              
        IRWORD = 0                                                              
10      LEN     = IAND( IBASE( INDCLR ) , MASKH2 )                              
        IF ( LEN .EQ. 0 ) GO TO 40                                              
        IDIFF   = INDCBP - INDCLR                                               
        IWORDS  = LEN - IDIFF                                                   
        IREQ    = IABS( NWORDS )                                                
        IF ( IREQ .GT. ( IWORDS+IRWORD ) ) GO TO 40                             
        INUM    = IREQ - IRWORD                                                 
        IF ( INUM .EQ. 0 ) GO TO 7000                                           
        IF ( NWORDS .LT. 0 ) GO TO 30                                           
        DO 20 K = 1, INUM                                                       
        IDATA( IRWORD+K ) = IBASE( INDCBP+K )                                   
20      CONTINUE                                                                
30      INDCBP = INDCBP + INUM                                                  
        GO TO 7000                                                              
40      ID = IAND( IBASE( INDCLR+LEN+1 ), MASKQ2 )                              
        IF ( LEN .EQ. 0 ) GO TO 65                                              
        IF ( NWORDS .LT. 0 ) GO TO 60                                           
        DO 50 K = 1, IWORDS                                                     
        IDATA( IRWORD+K ) = IBASE( INDCBP+K )                                   
50      CONTINUE                                                                
60      IRWORD = IRWORD + IWORDS                                                
65      IF ( ID .EQ. IDSC ) GO TO 70                                            
        CALL DSRDNB                                                             
        GO TO 10                                                                
70      IRETRN = 2                                                              
        IEOR   = 1                                                              
        M      = IRWORD                                                         
7000    RETURN                                                                  
        END                                                                     
