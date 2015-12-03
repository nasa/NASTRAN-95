      SUBROUTINE CDETM2 (P,D,IP,PR,PI,DR,DI,IPS1)
C
C     ARRANGES  P,D,IP  IN ORDER BY MAGNITUDE OF DETERMINANT
C
      INTEGER          IP(6),IPS(3),IPS1(3)
      DOUBLE PRECISION P(6),D(6),PR(3),PI(3),DR(3),DI(3),D1,D2,D3,DD(3),
     1                 D4
      EQUIVALENCE      (D1,DD(1)),(D2,DD(2)),(D3,DD(3))
C
      D1 = D(1)*D(1) + D(2)*D(2)
      D2 = D(3)*D(3) + D(4)*D(4)
      D3 = D(5)*D(5) + D(6)*D(6)
      DO 10 I = 1,3
      DD(I) = DSQRT(DD(I))
   10 CONTINUE
      DO 20 I = 2,6,2
      K = I/2
      IPS(K)  = IP(I)
      IPS1(K) = IP(I)
   20 CONTINUE
C
C     SAVE STUFF IN OUTPUT AREAS
C
      DO 30 I = 1,3
      PR(I) = P(2*I-1)
      PI(I) = P(2*I  )
      DR(I) = D(2*I-1)
      DI(I) = D(2*I  )
   30 CONTINUE
C
C     SCALE  MAGNITUDES
C
      DO 80 I = 1,3
   40 IF (DD(I) .GT. 10.0D0) GO TO 60
   50 IF (DD(I) .LT.  1.0D0) GO TO 70
      GO TO 80
   60 DD(I)  = DD(I)*0.1D0
      IPS(I) = IPS(I) + 1
      GO TO 40
   70 DD(I)  = DD(I)*10.0D0
      IPS(I) = IPS(I) - 1
      GO TO 50
   80 CONTINUE
C
C     START COMPARISON TESTS
C
      IF (IPS(1).GT.IPS(2) .AND. IPS(2).GT.IPS(3)) GO TO 190
      IF (IPS(1).GT.IPS(2) .AND. IPS(1).GT.IPS(3)) GO TO 160
      IF (IPS(2)-IPS(3)) 100,90,130
   90 IF (D2 .GE. D3) GO TO 130
  100 IF (IPS(1)-IPS(3)) 120,110,160
  110 IF (D1 .GE. D3) GO TO 160
  120 IS1 = 1
      IS2 = 3
      ASSIGN 160 TO ISRET
      GO TO 200
  130 IF (IPS(1)-IPS(2)) 150,140,160
  140 IF (D1 .GE. D2) GO TO 160
  150 IS1 = 1
      IS2 = 2
      ASSIGN 160 TO ISRET
      GO TO 200
  160 IF (IPS(2)-IPS(3)) 180,170,190
  170 IF (D2 .GE. D3) GO TO 190
  180 IS1 = 2
      IS2 = 3
      ASSIGN 190 TO ISRET
      GO TO 200
  190 RETURN
C
C      SWITCHES VALUES ON IS1, IS2
C
  200 NX = IPS(IS1)
      IPS(IS1) = IPS(IS2)
      IPS(IS2) = NX
      NX       = IPS1(IS1)
      IPS1(IS1)= IPS1(IS2)
      IPS1(IS2)= NX
      D4       = PR(IS1)
      PR(IS1)  = PR(IS2)
      PR(IS2)  = D4
      D4       = PI(IS1)
      PI(IS1)  = PI(IS2)
      PI(IS2)  = D4
      D4       = DR(IS1)
      DR(IS1)  = DR(IS2)
      DR(IS2)  = D4
      D4       = DI(IS1)
      DI(IS1)  = DI(IS2)
      DI(IS2)  = D4
      D4       = DD(IS1)
      DD(IS1)  = DD(IS2)
      DD(IS2)  = D4
      GO TO ISRET, (160,190)
      END
