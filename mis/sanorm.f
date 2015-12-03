      SUBROUTINE SANORM (*,A)
      DIMENSION  A(3)
C
C     VECTOR NORMALIZATION AND VECTOR LENGTH
C
      XL=A(1)*A(1) + A(2)*A(2) + A(3)*A(3)
      IF (XL .LE. 0.0) RETURN 1
      XL   = SQRT(XL)
      A(1) = A(1)/XL
      A(2) = A(2)/XL
      A(3) = A(3)/XL
      RETURN
      END
