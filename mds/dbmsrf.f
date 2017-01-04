      SUBROUTINE DBMSRF (  NAME, IFILEX )                                
C********************************************************************           
C  DBMNAM RETURNS THE UNIT NUMBER ASSOCIATED WITH A DMAP FILE NAME            
C     ARGUMENTS                                                                 
C       NAME    (INPUT) (2A4) DMAP FILE NAME                                 
C       IFILEX  (OUTPUT) (I)  UNIT ASSOCIATED WITH FILE NAME IN FIAT
C********************************************************************           
      INTEGER FIST(100), FIAT(100), NAME(2)
      COMMON / XFIST / FIST                                                     
      COMMON / XFIAT / FIAT                                                     
      LIM  = FIAT(2)*11 + 3                                                      
      DO 10 I = 4, LIM, 11                                                       
      IF ( NAME( 1 ) .NE. FIAT( I+1 ) .OR. NAME( 2 ) .NE. FIAT( I+2 ) )
     &   GO TO 10
      IFILEX = IAND( FIAT( I ), 32767 )
      GO TO 700
10    CONTINUE
      IFILEX = 0
700   RETURN
      END                                                                       
