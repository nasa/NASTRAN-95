      SUBROUTINE DNORM(X,MAG)
C
C     DOUBLE PRECISION NORMALIZATION
C
      DOUBLE PRECISION        X(3)     ,MAG      ,A
C
      MAG= 0.D0
      A= X(1)*X(1) + X(2)*X(2) +X(3)*X(3)
      IF (A .GT. 0.D0) MAG= DSQRT(A)
      IF(MAG .EQ. 0.0D0) RETURN
      X(1) = X(1) / MAG
      X(2) = X(2) / MAG
      X(3) = X(3) / MAG
      RETURN
      END
