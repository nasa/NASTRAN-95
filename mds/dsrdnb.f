        SUBROUTINE DSRDNB                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        CALL DBMMGR( 5 )                                                        
        NBLOCK = FCB( 4, IFILEX )                                               
        INDCLR = INDBAS + 5                                                     
        INDCBP = INDCLR                                                         
        LCW    = IBASE( INDBAS+4 )                                              
        IBLK   = IBASE( INDBAS+3 )                                              
        IF ( IBLK .EQ. NBLOCK ) GO TO 10                                        
        CALL DSMSG ( 102 )                                                      
10      RETURN                                                                  
        END                                                                     
