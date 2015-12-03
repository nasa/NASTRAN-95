      SUBROUTINE SUB1(X,Y,A,B)
C     SUBROUTINE SUB(X,Y,A,B)
C*******
C     SUB WILL FORM Y = A*X - B*Y  WHERE A AND B ARE SCALAR MULTIPLIERS
C     FOR THE VECTORS X AND Y
C*******
C     DOUBLE PRECISION   X(1)      ,Y(1)     ,A        ,B
      DOUBLE PRECISION A,B
      REAL X(1),Y(1)
      COMMON   /INVPWX/  XX        ,NCOL
      A1 = A
      B1 = B
      DO 10 I = 1,NCOL
   10 Y(I) = X(I)*A1- Y(I)*B1
      RETURN
      END
