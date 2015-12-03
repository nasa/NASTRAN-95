        SUBROUTINE DSUPKC ( ITIN, ITOUT, A, B )                                 
        COMMON / SYSTEM / ISYSBF, IWR                                           
        REAL               A(4), B(4), AA(4), BB(4)                             
        INTEGER           NWORDS(4)                                             
        REAL              RS1, RS2                                              
        DOUBLE PRECISION  RD1, RD2, RDI1, RDI2                                  
        EQUIVALENCE       (AA,RS1,RD1), (BB,RS2,RD2)                            
        EQUIVALENCE       ( AA(3), RDI1 ), ( BB(3), RDI2 )                      
        DATA              NWORDS / 1,2,2,4/                                     
        IWRD1   = NWORDS( ITIN )                                                
        IF ( ITIN .NE. ITOUT ) GO TO 20                                         
CDIR$ NEXTSCALAR                                                                
        DO 10 K = 1, IWRD1                                                      
        B( K )  = A( K )                                                        
10      CONTINUE                                                                
        GO TO 7777                                                              
20      IF ( ITOUT .GT. 64 ) GO TO 30                                           
        ITOUT2 = ITOUT                                                          
        IWRD2  = NWORDS( ITOUT )                                                
        SSIGN  = 1.0                                                            
        GO TO 40                                                                
30      ITOUT2 = ITOUT - 64                                                     
        IWRD2  = NWORDS( ITOUT2 )                                               
        SSIGN  = -1.0                                                           
CDIR$ NEXTSCALAR                                                                
40      DO 50 K = 1, IWRD1                                                      
        AA( K ) = A( K )                                                        
50      CONTINUE                                                                
        GO TO ( 1000, 2000, 3000, 4000 ), ITIN                                  
1000    GO TO ( 1100, 1200, 1300, 1400 ), ITOUT2                                
1100    RS2 = SSIGN * RS1                                                       
        GO TO 7000                                                              
1200    RD2 = SSIGN * RS1                                                       
        GO TO 7000                                                              
1300    BB( 1 ) = SSIGN * RS1                                                   
        BB( 2 ) = 0.                                                            
        GO TO 7000                                                              
1400    RD2 = SSIGN * RS1                                                       
        RDI2 = 0.                                                               
        GO TO 7000                                                              
2000    GO TO ( 2100, 2200, 2300, 2400 ), ITOUT2                                
2100    RS2 = SSIGN * RD1                                                       
        GO TO 7000                                                              
2200    RD2 = SSIGN * RD1                                                       
        GO TO 7000                                                              
2300    BB( 1 ) = SSIGN * RD1                                                   
        BB( 2 ) = 0.                                                            
        GO TO 7000                                                              
2400    RD2 = SSIGN * RD1                                                       
        RDI2 = 0.                                                               
        GO TO 7000                                                              
3000    GO TO ( 3100, 3200, 3300, 3400 ), ITOUT2                                
3100    RS2 = SSIGN * AA( 1 )                                                   
        GO TO 7000                                                              
3200    RD2 = SSIGN * AA( 1 )                                                   
        GO TO 7000                                                              
3300    BB( 1 ) = SSIGN * AA( 1 )                                               
        BB( 2 ) = SSIGN * AA( 2 )                                               
        GO TO 7000                                                              
3400    RD2 = SSIGN * AA( 1 )                                                   
        RDI2 = SSIGN * AA( 2 )                                                  
        GO TO 7000                                                              
4000    GO TO ( 4100, 4200, 4300, 4400 ), ITOUT2                                
4100    RS2 = SSIGN * RD1                                                       
        GO TO 7000                                                              
4200    RD2 = SSIGN * RD1                                                       
        GO TO 7000                                                              
4300    BB( 1 ) = SSIGN * RD1                                                   
        BB( 2 ) = SSIGN * RDI1                                                  
        GO TO 7000                                                              
4400    RD2 = SSIGN * RD1                                                       
        RDI2 = SSIGN * RDI1                                                     
        GO TO 7000                                                              
CDIR$ NEXTSCALAR                                                                
7000    DO 7200 K = 1, IWRD2                                                    
        B( K ) = BB( K )                                                        
7200    CONTINUE                                                                
7777    RETURN                                                                  
        END                                                                     
