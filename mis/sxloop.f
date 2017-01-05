      SUBROUTINE SXLOOP (X,Y,N)
C
C     SINGLE PRECISION VERSION OF CXLOOP, CALLED ONLY BY CDCMPS
C
      REAL       X(1),   Y(1)
      REAL       XX(2) , YY(2), MPY(2)
      NN = N + N
      DO 10 I = 1,NN
   10 X(I) = Y(I)
      RETURN
C
      ENTRY SLOOP (XX,YY,MPY,M)
      MM = M + M
      DO 20 I = 1,MM,2
      XX(I  ) = XX(I  ) - MPY(1)*YY(I) + MPY(2)*YY(I+1)
   20 XX(I+1) = XX(I+1) - MPY(2)*YY(I) - MPY(1)*YY(I+1)
      RETURN
      END
