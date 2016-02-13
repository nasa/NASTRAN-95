        SUBROUTINE READ ( *, *, FILE, IDATA, N, IEORFL, M )                     
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER           FILE, IDATA( 2 )                                      
        NAME   = FILE                                                           
        NWORDS = N                                                              
        IEOR   = IEORFL                                                         
        IRETRN = 0                                                              
        CALL DSGEFL                                                             
        IF ( IPRVOP .EQ. 0 ) GO TO 10                                           
        CALL DSMSG ( 4 )                                                        
10      ID = IAND( IBASE( INDCLR ), MASKQ1 )                                    
        IF ( ID .NE. IDSEB ) GO TO 30                                           
        CALL DBMLBK( LASBLK )                                                   
        IF ( LASBLK .GT. NBLOCK ) GO TO 20                                      
        IRETRN = 1                                                              
        GO TO 7000                                                              
20      CALL DSRDNB                                                             
        ID = IAND( IBASE( INDCLR ), MASKQ1 )                                    
30      IF ( ID .EQ. IDSRH ) GO TO 50                                           
        IF ( ID .EQ. IDSEF ) GO TO 40                                           
        CALL DSMSG ( 105 )                                                      
40      INDCLR = INDCLR + 1                                                     
        INDCBP = INDCLR                                                         
        IRETRN = 1                                                              
        GO TO 7000                                                              
50      IWORDS = IAND( IBASE( INDCLR ), MASKH2 )                                
        IDIFF  = INDCBP - INDCLR                                                
        IWORDS = IWORDS - IDIFF                                                 
        IREQ   = IABS( NWORDS )                                                 
        IF ( IREQ .GT. IWORDS ) GO TO 80                                        
        IF ( NWORDS .LE. 0 ) GO TO 70                                           
        L = 1                                                                   
        ILIM = INDCBP + NWORDS - 1                                              
        DO 60 K = INDCBP, ILIM                                                  
        IDATA( L ) = IBASE( K+1 )                                               
        L = L + 1                                                               
60      CONTINUE                                                                
70      INDCBP = INDCBP + IREQ                                                  
        GO TO 90                                                                
80      CALL DSRDMB ( IDATA, M )                                                
90      IF ( IEOR .EQ. 0 ) GO TO 7000                                           
        CALL DSSKRC                                                             
7000    CALL DSSDCB                                                             
        IF ( IRETRN .EQ. 2 ) RETURN 2                                           
        IF ( IRETRN .EQ. 1 ) RETURN 1                                           
        RETURN                                                                  
        END                                                                     
