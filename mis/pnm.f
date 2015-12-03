      SUBROUTINE PNM(M,N,X,IR,V)
      DIMENSION GAMMA(81)
      IF(N.LT.M) GO TO 2
      IF(N.EQ.0) GO TO 1
      GO TO 3
    1 V=1.0
      RETURN
    2 V=0.0
      RETURN
    3 Z=1.0
      W=Z
      IF(N.EQ.M) GO TO 4
      NMM=N-M
      DO 5 I=1,NMM
    5 Z=X*Z
    4 GAMMA(1)=1.0
      NPNN1=N+N+1
      DO 6 I=2,NPNN1
      GAMMA(I)=W*GAMMA(I-1)
    6 W=W+1.0
      W=1.0
      ABXX=ABS(X)
      IF(ABXX.LT.0.001) GO TO 7
      GO TO 8
    7 I=(N-M)/2
      I2=2*I
      NMM=N-M
      IF(I2.NE.NMM) GO TO 9
      V=GAMMA(M+N+1)/(GAMMA(I+1)*GAMMA(M+I+1))
      IF(IR.NE.0) GO TO 100
      V=V*(-1.0)**I
      GO TO 100
    9 V=0.0
      RETURN
    8 Y=W/(X*X)
      IF(IR.EQ.0) GO TO 11
      GO TO 12
   11 Y=-Y
      W=-W
   12 J=3
      V=0.0
      DO 13 I=1,22
      II=(N-M+2)/2
      IF(II.LT.I) GO TO 100
      V=V+GAMMA(N+N-I-I+3)*Z/(GAMMA(I)*GAMMA(N-I+2)*GAMMA(N-I-I-M+J))
   13 Z=Z*Y
  100 Z=1.0
      DO 101 I=1,N
  101 Z=Z+Z
      V=V/Z
      IF(IR.NE.0) GO TO 102
      GO TO 103
  102 II=N/4
      I=N-4*II
      IF(I.GT.1) GO TO 104
      GO TO 103
  104  V=-V
  103  IF(M.EQ.0) RETURN
      J=M/2
      CF=W+X*X
      Z=ABS(CF)
      J2=J+J
      IF(M.NE.J2) GO TO 107
      GO TO 105
  107 Z=SQRT(Z)
      J=M
  105 IF(J.LT.1) J=1
      DO 106 I=1,J
  106 V=V*Z
      RETURN
      END
