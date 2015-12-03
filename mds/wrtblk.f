        SUBROUTINE WRTBLK ( FILE, IEND )                                        
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER FILE                                                            
        NAME = FILE                                                             
        CALL DSGEFL                                                             
C        PRINT *,' WRTBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS               
C        WRITE(6,40646)(IBASE(INDBAS+K),K=0,7)                                  
40646   FORMAT(' WRTBLK,BUFFER=',8(1X,Z8))                                      
        CALL DBMMGR( 8 )                                                        
        INDCLR = IBASE( INDBAS+4 )                                              
        NBLOCK = IBASE( INDBAS+3 )                                              
        IBASE( INDBAS+1 ) = IBASE( INDBAS+4 )                                   
        IBASE( INDBAS+2 ) = IBASE( INDBAS+4 )                                   
        FCB( 3,IFILEX ) = INDCLR                                                
        FCB( 4,IFILEX ) = NBLOCK                                                
C       INNN = FCB(12, IFILEX)                                                  
C       PRINT *,' WRTBLK-2,IFILEX,INNN=',IFILEX,INNN                            
C       WRITE(6,40646)(IBASE(INNN+K),K=0,7)                                     
        IF ( IEND .EQ. 1 ) GO TO 700                                            
        CALL DBMMGR( 4 )                                                        
700     RETURN                                                                  
        END                                                                     
