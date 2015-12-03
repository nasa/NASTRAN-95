      SUBROUTINE SUB(X,Y,A,B)
C*******
C     SUB WILL FORM Y = A*X - B*Y  WHERE A AND B ARE SCALAR MULTIPLIERS
C     FOR THE VECTORS X AND Y
C*******
      DOUBLE PRECISION   X(1)      ,Y(1)     ,A        ,B
      COMMON   /INVPWX/  XX        ,NCOL
      DO 10 I = 1,NCOL
   10 Y(I) = X(I)*A - Y(I)*B
      RETURN
      END
