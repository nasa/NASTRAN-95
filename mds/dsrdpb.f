        SUBROUTINE DSRDPB                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        NBLOCK = NBLOCK - 1                                                     
        CALL DBMMGR( 6 )                                                        
        INDCLR = IBASE( INDBAS+4 ) + INDBAS - 1                                 
        INDCBP = INDCLR                                                         
        IBLK   = IBASE( INDBAS+3 )                                              
        IF ( IBLK .EQ. NBLOCK ) GO TO 10                                        
        CALL DSMSG( 102 )                                                       
10      RETURN                                                                  
        END                                                                     
