      SUBROUTINE PERMUT(IA,ID,N,ISW)
      DIMENSION IA(1),IB(32),IC(32),ID(10)
      DO 10 I=1,N
      IC(I)=IA(I)
   10 IB(I)=I
      N1=N-1
      DO 20 I=1,N1
      I1=I+1
      DO 30 J=I1,N
      IF(IC(J)-IC(I))40,30,30
   40 IS1=IB(J)
      IB(J)=IB(I)
      IB(I)=IS1
      IS1 = IC(J)
      IC(J)=IC(I)
      IC(I) = IS1
   30 CONTINUE
   20 CONTINUE
      DO 50 I = 1,N
      IF(IC(I)-ISW)50,60,60
   50 CONTINUE
      K=1
      GO TO 71
   60 DO 70 J=I,N
      K=J-I+1
   70 ID(K)=IB(J)
      IF(K .EQ. N) GO TO 90
      K=K+1
   71 DO 80 J=K,N
      L=J-K+1
   80 ID(J)=IB(L)
   90 RETURN
      END
