        SUBROUTINE RDBLK ( *, FILE, IFIRST, LEFT )                              
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        INTEGER FILE                                                            
        NAME = FILE                                                             
        CALL    DSGEFL                                                          
C        PRINT *,' RDBLK,NAME,IFILEX,INDBAS=',NAME,IFILEX,INDBAS                
C        WRITE(6,40646)(IBASE(INDBAS+K),K=1,8)                                  
40646   FORMAT(' BUFFER=',8(1X,Z8))                                             
        IF ( IPRVOP .NE. 0 ) CALL DSMSG( 4 )                                    
        IF ( IFIRST .NE. 0 ) GO TO 10                                           
        CALL DSRDNB                                                             
10      CALL DBMMGR( 9 )                                                        
C      WRITE(6,44771)(FCB(K,IFILEX),K=1,15)                                     
44771 FORMAT(' RDBLK,FCB=',/,2(5I8,/),2I8,4X,2A4,4X,I8)                         
C      INNN = FCB( 12, IFILEX )                                                 
C       PRINT *,' RDBLK-2,IFILEX,INNN=',IFILEX,INNN                             
C       WRITE(6,40646)(IBASE(INNN+K),K=0,7)                                     
        IBASE( INDBAS+1 ) = IBASE( INDBAS+4 )                                   
        IBASE( INDBAS+2 ) = IBASE( INDBAS+4 )                                   
        LEFT = NBUFF + 3 - LCW                                                  
        IF ( IBASE( INDBAS+LCW-2) .EQ. IDSEF ) RETURN 1                         
        RETURN                                                                  
        END                                                                     
