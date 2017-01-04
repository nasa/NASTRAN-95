      SUBROUTINE DSHXDD ( II,IARR, LEN)                                         
      INTEGER    IARR( 10000)                                                   
      COMMON / DSBUFF / IIBUFF(2048)                                            
      DO 10 K = 1, LEN                                                          
10    IIBUFF(K) = IARR(K)                                                       
      WRITE ( 6, 901 ) II,(IIBUFF(I),I=1,LEN )                                  
901   FORMAT(I5,200(8(1X,Z8),/))                                                
      RETURN                                                                    
      END                                                                       
