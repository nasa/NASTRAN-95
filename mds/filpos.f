        SUBROUTINE FILPOS ( FILE, IPOS )                                        
        INCLUDE 'DSIOF.COM'                                                     
        INCLUDE 'XNSTRN.COM'                                                    
        INTEGER    FILE                                                         
        NAME  = FILE                                                            
        CALL DSGEFL                                                             
        NBLOCK = IAND( IPOS, MASKH2 )                                           
        ICBLK  = FCB( 4, IFILEX )                                               
        IF ( ICBLK .EQ. NBLOCK ) GO TO 10                                       
        CALL DBMMGR( 6 )                                                        
10      CONTINUE                                                                
        INDCLR = IPOS/MULQ2 + INDBAS - 1                                        
        INDCBP = INDCLR                                                         
        CALL DSSDCB                                                             
        RETURN                                                                  
        END                                                                     
