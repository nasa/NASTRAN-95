      SUBROUTINE CX TRN Y (X,Y,ALPHA)
C*******
C     CX TRN Y FORMS THE DOT PRODUCT X TRANSPOSE * Y = ALPHA WHERE
C     X AND Y ARE COMPLEX
C*******
      COMMON   /CINVPX/  AAA       ,NCOL
      DOUBLE PRECISION   X(1)      ,Y(1)     ,ALPHA(2)
      NCOL2 = NCOL+NCOL
      ALPHA(1) = 0.D0
      ALPHA(2) = 0.D0
      DO 10 I = 1,NCOL2,2
      ALPHA(1) = ALPHA(1)+X(I)*Y(I)-X(I+1)*Y(I+1)
   10 ALPHA(2) = ALPHA(2)+X(I)*Y(I+1)+X(I+1)*Y(I)
      RETURN
      END
