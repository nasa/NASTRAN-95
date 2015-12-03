        SUBROUTINE DSWRT1 ( IDATA )                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER IDATA( 2 )                                                      
        INEXT = 0                                                               
        IF ( LWORDS .LE. -1 ) GO TO 50                                          
        IF ( NBLOCK .EQ. IBLOCK ) GO TO 10                                      
        IFLG = IDSX                                                             
        GO TO 20                                                                
10      IFLG = IDSP                                                             
20      IF ( LWORDS .LE. 0 ) GO TO 40                                           
        ICOUNT = IAND( IBASE( INDCLR ), MASKH2 )                                
        IBASE( INDCLR ) = IDSRH + IFLG + ICOUNT + LWORDS                        
        DO 30 I = 1, LWORDS                                                     
        IBASE( INDCBP + I ) = IDATA( I )                                        
30      CONTINUE                                                                
        INDCBP = INDCBP + LWORDS + 1                                            
        IBASE( INDCBP ) = IDSRT + IFLG + ( INDCLR-INDBAS+1 )                    
        INDCLR = INDCBP + 1                                                     
        IBASE( INDCBP+1 ) = IDSEB                                               
        IFLG   = IDSX                                                           
        GO TO 60                                                                
40      IBASE( INDCLR ) = IAND( IBASE( INDCLR ), NOT( MASKQ2 ) )                
        IBASE( INDCLR ) = IOR( IFLG, IBASE( INDCLR ) )                          
        IBASE( INDCBP+1 ) = IDSRT + IFLG + ( INDCLR-INDBAS+1 )                  
        IBASE( INDCBP+2 ) = IDSEB                                               
        LWORDS = 0                                                              
        INDCLR = INDCBP + 2                                                     
        INDCBP = INDCLR                                                         
        IFLG   = IDSX                                                           
        GO TO 60                                                                
50      IBASE( INDCLR ) = IDSEB                                                 
        LWORDS = 0                                                              
        IFLG    = IDSX                                                          
        IF ( IBLOCK .EQ. NBLOCK ) IFLG = IDSC                                   
60      CALL DSWRNB                                                             
        IRWORDS = NWORDS - LWORDS                                               
        INEXT   = INEXT + LWORDS                                                
70      IF ( IRWORDS .GT. ( NBUFF-5 ) ) GO TO 80                                
        IFIN    = 1                                                             
        NWORDS  = IRWORDS                                                       
        GO TO 90                                                                
80      IFIN    = 0                                                             
        IF ( IFLG .EQ. IDSC ) IFLG = IDSP                                       
        NWORDS  = NBUFF - 5                                                     
90      IBASE( INDCLR ) = IDSRH + IFLG + NWORDS                                 
        DO 100 I = 1, NWORDS                                                    
        IBASE( INDCBP+I ) = IDATA( INEXT+I )                                    
100     CONTINUE                                                                
        INDCBP = INDCBP + NWORDS                                                
        IF ( IFIN .EQ. 1 ) GO TO 110                                            
        INEXT  = INEXT + NWORDS                                                 
        IBASE( INDCBP+1 ) = IDSRT + IFLG + ( INDCLR-INDBAS+1 )                  
        IBASE( INDCBP+2 ) = IDSEB                                               
        IRWORDS = IRWORDS - NWORDS                                              
        IFLG   = IDSX                                                           
        INDCLR  = INDCLR + NWORDS + 2                                           
        CALL DSWRNB                                                             
        GO TO 70                                                                
110     IF ( IEOR .EQ. 0 ) GO TO 120                                            
        IBASE( INDCBP+1 ) = IDSRT + IDSC + ( INDCLR-INDBAS+1 )                  
        INDCLR = INDCBP + 2                                                     
        INDCBP = INDCLR                                                         
120     RETURN                                                                  
        END                                                                     
