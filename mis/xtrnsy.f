      SUBROUTINE X TRNS Y (X,Y,ALPHA)
C*******
C     X TRNS Y  FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA
C*******
      DOUBLE PRECISION   X(1)      ,Y(1)     ,ALPHA
      COMMON   /INVPWX/  AAA       ,NCOL
      ALPHA = 0.D0
      DO 10 I=1,NCOL
   10 ALPHA = ALPHA + X(I)*Y(I)
      RETURN
      END
