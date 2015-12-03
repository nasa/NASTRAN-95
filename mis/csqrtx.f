      SUBROUTINE CSQRTX(XX,Y)
C*******
C     ROUTINE TO FIND THE COMPLEX SQUARE ROOT OF X AND STORE IT IN Y
C*******
      DOUBLE PRECISION XX(2),X(2),Y(2),R
      X(1) = XX(1)
      X(2) = XX(2)
      R = DSQRT(X(1)**2+X(2)**2)
      Y(1) = DSQRT(DABS(X(1)+R)/2.)
      Y(2) = DSQRT(DABS(-X(1)+R)/2.)
      IF(X(2) .EQ. 0.0D0) RETURN
      Y(2) = DSIGN(Y(2),X(2))
      RETURN
      END
