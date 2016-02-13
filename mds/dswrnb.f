        SUBROUTINE DSWRNB                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        IBASE( INDBAS+4 ) = INDCLR - INDBAS + 1                                 
        CALL DBMMGR( 4 )                                                        
        NBLOCK  = FCB( 4, IFILEX )                                              
        INDCLR  = INDBAS + 5                                                    
        IBASE( INDBAS+3 ) = NBLOCK                                              
        INDCBP  = INDCLR                                                        
        RETURN                                                                  
        END                                                                     
