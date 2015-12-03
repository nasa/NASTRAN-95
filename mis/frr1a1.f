      SUBROUTINE FRR1A1 (RZ,CZ,IB,REB,CEB)
C
      COMPLEX   Z,SUM,ZK,TERM
C
      Z  = CMPLX(RZ,CZ)
      IF (CABS(Z) .LT. .1) GO TO 100
      ZK  = CMPLX(1.,0.)
      N   = IB
      BF  = 1.
      BF1 = 0.
      SUM = CMPLX(0.,0.)
      DO 10 I = 1,N
      SUM = SUM + ZK/CMPLX(BF,0.)
      ZK  = ZK*Z
      BF1 = BF1 + 1.
      BF  = BF*BF1
   10 CONTINUE
      ZK  = CMPLX(BF,0.)/ZK*(CEXP(Z)-SUM)
      REB = REAL(ZK)
      CEB = AIMAG(ZK)
      RETURN
C
  100 CONTINUE
      ZK  = Z
      DEN = FLOAT(IB) + 1.
      SUM = CMPLX(1.,0.)
      DO 20 I = 1,30
      TERM= ZK/DEN
      SUM = SUM + TERM
      IF (CABS(TERM) .LT. 1.E-9) GO TO 200
      ZK  = ZK*Z
      DEN = DEN*(FLOAT(IB)+ FLOAT(I+1))
   20 CONTINUE
  200 REB = REAL(SUM)
      CEB = AIMAG(SUM)
      RETURN
      END
