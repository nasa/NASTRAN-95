      SUBROUTINE CSUB (X,Y,Z,A,B)
C*******
C     CSUB WILL FORM Z = A*X - B*Y WHERE A AND B ARE SCALAR
C     MULTIPLIERS FOR THE COMPLEX VECTORS X AND Y
C*******
      DOUBLE PRECISION   X(2)      ,Y(2)     ,A(2)     ,B(2)
     1                  ,Z(1)      ,DUM
      COMMON   /CINVPX/  AAA       ,NCOL
      NCOL2 = NCOL+NCOL
      DO 10 I = 1,NCOL2,2
      DUM  = X(I)*A(1) - X(I+1)*A(2) - Y(I)*B(1) + Y(I+1)*B(2)
      Z(I+1) = X(I)*A(2) + X(I+1)*A(1) - Y(I+1)*B(1) - Y(I)*B(2)
   10 Z(I) = DUM
      RETURN
      END
