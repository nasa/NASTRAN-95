      SUBROUTINE ALG30 (A,B)
C
      DIMENSION A(9,9),B(9),INDEX(9)
C
      N=9
      DO 100 J=1,N
100   INDEX(J)=0
110   AMAX=-1.0
      DO 120 J=1,N
      IF(INDEX(J).NE.0)GO TO 120
      DO 115 L=1,N
      IF(INDEX(L).NE.0)GO TO 115
      PV=ABS(A(J,L))
      IF(PV.LE.AMAX)GO TO 115
      IR=J
      IC=L
      AMAX=PV
115   CONTINUE
120   CONTINUE
      IF(AMAX.LE.0.0)RETURN
      INDEX(IC)=IR
      IF(IC.EQ.IR)GO TO 150
      DO 140 L=1,N
      PV=A(IR,L)
      A(IR,L)=A(IC,L)
      A(IC,L)=PV
      IF(L.GT.1)GO TO 140
      PV=B(IR)
      B(IR)=B(IC)
      B(IC)=PV
140   CONTINUE
150   PV=1.0/A(IC,IC)
      A(IC,IC)=1.0
      DO 160 L=1,N
      A(IC,L)=A(IC,L)*PV
      IF(L.GT.1)GO TO 160
      B(IC)=B(IC)*PV
160   CONTINUE
      DO 180 L1=1,N
      IF(L1.EQ.IC)GO TO 180
      PV=A(L1,IC)
      A(L1,IC)=0.0
      DO 170 L=1,N
170   A(L1,L)=A(L1,L)-A(IC,L)*PV
      B(L1)=B(L1)-B(IC)*PV
180   CONTINUE
      GO TO 110
      END
