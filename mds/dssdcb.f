        SUBROUTINE DSSDCB                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
        ICLR  =  INDCLR - INDBAS + 1                                            
        FCB( 3, IFILEX )  = ICLR                                                
        FCB( 4, IFILEX )  = NBLOCK                                              
        IBASE( INDBAS+1 ) = INDCBP - INDBAS + 1                                 
        IBASE( INDBAS+2 ) = ICLR                                                
        LASNAM = NAME                                                           
C        WRITE(6,40646)IFILEX,NBLOCK,ICLR,INDBAS,INDCLR                         
40646   FORMAT(' DSSDCB,IFILEX,NBLOCK,ICLR,BAS,CLR=',I3,I5,6I7)                 
        RETURN                                                                  
        END                                                                     
