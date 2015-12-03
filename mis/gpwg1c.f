      SUBROUTINE GPWG1C  (B,E,EIG,IFLAG)
C
C     DOUBLE PRECISION VERSION, BY G.CHAN/SPERRY    8/86
C
C     IFLAG=0 MEANS RUN OK
C     IFLAG=1 MEANS NO SOLUTION IN 20 ITERATIONS
C
      DOUBLE PRECISION  E(3,3),EP(3,3),B(3,3),BP(3,3),EIG(3)
      DOUBLE PRECISION  DETB,EPSIL,BMAX,R,S,C,T
C
      DETB = 0.0D0
      DO 5 I = 1,3
      DO 5 J = 1,3
      DETB = DETB+B(I,J)*B(I,J)
    5 CONTINUE
      EPSIL = DSQRT(DETB)*1.0D-5
      IFLAG =0
      II = 1
      DO 10 I=1,3
      DO 10 J=1,3
      E(I,J) = 0.0D0
      IF (I .EQ. J) E(I,J) = 1.0D0
   10 CONTINUE
      IF (DETB .EQ. 0.0D0) GO TO 100
   15 BMAX = DMAX1(DABS(B(1,2)),DABS(B(1,3)),DABS(B(2,3)))
      IF (DABS(BMAX)  .LT. EPSIL) GO TO 100
      IF (BMAX .NE. DABS(B(1,2))) GO TO 20
      I = 1
      J = 2
      K = 3
      GO TO 40
   20 IF (BMAX .NE. DABS(B(1,3))) GO TO 30
      I = 1
      J = 3
      K = 2
      GO TO 40
   30 I = 2
      J = 3
      K = 1
   40 R = (B(J,J)-B(I,I))/B(I,J)
      IF (DABS(R) .LT. 1.0D-6) GO TO 50
      IF (DABS(R) .GT. 1.0D+6) GO TO 60
      T = DSQRT((R*R)/4.0D0+1.0D0)-0.5D0*R
      C = DSQRT(1.0D0+T*T)
      S = T/C
      C = 1.0D0/C
      GO TO 70
   50 S = DSQRT(.5D0)
      C = S
      GO TO 70
   60 S = 0.0D0
      C = 1.0D0
   70 BP(I,I) = B(I,I)*C*C+B(J,J)*S*S-2.0D0*B(I,J)*S*C
      BP(J,J) = B(I,I)*S*S+B(J,J)*C*C+2.0D0*B(I,J)*S*C
      BP(K,K) = B(K,K)
      BP(J,I) = 0.0D0
      BP(I,J) = 0.0D0
      BP(K,I) = B(I,K)*C-B(J,K)*S
      BP(I,K) = BP(K,I)
      BP(K,J) = B(J,K)*C+B(I,K)*S
      BP(J,K) = BP(K,J)
      EP(I,1) = E(I,1)*C-E(J,1)*S
      EP(J,1) = E(I,1)*S+E(J,1)*C
      EP(K,1) = E(K,1)
      EP(I,2) = E(I,2)*C-E(J,2)*S
      EP(J,2) = E(I,2)*S+E(J,2)*C
      EP(K,2) = E(K,2)
      EP(I,3) = E(I,3)*C-E(J,3)*S
      EP(J,3) = E(I,3)*S+E(J,3)*C
      EP(K,3) = E(K,3)
      DO 80  I=1,3
      DO 80  J=1,3
      B(I,J) = BP(I,J)
      E(I,J) = EP(I,J)
   80 CONTINUE
      IF (II .GE. 21) GO TO 90
      II = II+1
      GO TO 15
   90 IFLAG=1
      GO TO 120
  100 DO 110  I=1,3
  110 EIG(I) = B(I,I)
  120 RETURN
      END
