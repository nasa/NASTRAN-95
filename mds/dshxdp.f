      SUBROUTINE DSHXDP ( IARR, LEN )                                           
      INTEGER    IARR( 10000)                                                   
      WRITE ( 6, 901 ) (IARR(I),I=1,LEN )                                       
901   FORMAT(200(8(1X,Z8),/))                                                   
      RETURN                                                                    
      END                                                                       
