      DOUBLE PRECISION FUNCTION DKL (NP,I,L,R,Z)
C-----
C   THIS ROUTINE CALCULATES THE DOUBLE PRECISION  DELTA(IJ)  INTEGRALS
C FOR AXISYMMETRIC SOLIDS IN SMA1, EMG.
C
C   INPUT
C     NP = NUMBER OF POINTS (3 OR 4)
C     I,L= THE INTEGRAL DESIRED (I SERIES STARTS WITH -1)
C     R  = RADIUS ARRAY (NP LONG)
C     Z  = Z-CORD ARRAY (NP LONG)
C
C   OUTPUT
C     DKL = DESIRED INTEGRAL
C
C-----
      INTEGER NAM(2)
      DOUBLE PRECISION A         ,AJ       ,AR
     1,       BETA
     2,       DR       ,DZ       ,DFACT    ,DZJ      ,DL1
     3,       EPS
     4,       FACT
     5,       GKL
     6,       ONE
     7,       PR
     8,       RA       ,RB       ,R(1)     ,RAK      ,RBK
     9,       TWO      ,THREE
     *,       ZA       ,ZB       ,ZERO     ,Z(1)
C
      DATA EPS / .01D0 /
      DATA ZERO,ONE,TWO,THREE / 0.0D0, 1.0D0, 2.0D0, 3.0D0 /
      DATA NAM / 4HDKL , 1H  /
C
      DKL = ZERO
      L1 = L+1
      L2 = L + 2
      DL1 = L1
      K  = I+1
C
C  . LOOP ON NUMBER OF POINTS...
      IF (R(1).LE.ZERO) GO TO 300
       DO 200 M = 1,NP
      J = M+1
      IF (M.EQ.NP) J = 1
      RA = R(M)
      RB = R(J)
      ZA = Z(M)
      ZB = Z(J)
      DR = RB-RA
      DZ = ZB-ZA
C
C  . TEST IF RADIUS IS .LE. 0 (DRIVER SHOULD FIND THIS)...
      IF (RB.LE.ZERO) GO TO 300
      GKL = ZERO
      PR = RA+RB
      AR = PR / TWO
C
C  . CHECK FOR APPROXIMATION, DR/AVE(R)...
      IF ( DABS ( DR/AR ) .LT. EPS ) GO TO 70
C
      A = ZA*DR - RA*DZ
      BETA = A/DR
C
C  . CHECK FOR BETA .EQ. 0 CASE...
      IF (DABS (BETA / AR ) .GT. EPS ) GO TO 10
C
      IF (DZ.EQ.ZERO) GO TO 200
      LK = L + K + 1
      AR = LK
      GKL = (DZ/DR)**L1 * (RA**LK-RB**LK) / (DL1*AR)
       GO TO 200
C
C  . GENERAL CASE...
   10 RAK = RA**K
      RBK = RB**K
      IF ( K ) 300,20,30
C
C  . GENERAL CASE, K.EQ.0, CONSTANT TERM...
   20 GKL = DLOG (RA/RB) / DL1
       GO TO 40
C
C  . GENERAL CASE, CONSTANT TERM...
   30 AR = K * L1
      GKL = (RAK - RBK) / AR
C
C  . GENERAL CASE, SUMMATION...
   40 IF (DZ.EQ.ZERO) GO TO 65
      LFACT = 1
C  . CALCULATE FACTORIAL (L+1)...
       DO 50 J = 2,L
   50 LFACT = LFACT * J
      FACTL = LFACT
      JFACT = 1
      AJ  = ONE
      DZJ = ONE
      LMJF= LFACT * L1
       DO 60 J = 1,L1
      JFACT = JFACT * J
C  . CALCULATE (L+1-J) FACTORIAL IN LMJF...
      LMJF = LMJF / (L2-J)
      FACT = FACTL / FLOAT (JFACT*LMJF)
      DFACT = K + J
      DFACT = FACT / DFACT
      AJ  = AJ * A
      RAK = RAK * RA
      RBK = RBK * RB
      DZJ = DZJ * DZ
   60 GKL = GKL + (DFACT * DZJ * (RAK-RBK)) / AJ
C-----
   65 GKL = GKL * BETA**L1
       GO TO 200
C
C  . APPROXIMATE CODE...
   70 CONTINUE
      IF (DR.EQ.ZERO) GO TO 200
      DZJ = L1 * L2
      RBK = ZB**L1
      J = K - 1
      GKL = -DR * AR**J * RBK / DL1
C
      IF (DZ.EQ.ZERO) GO TO 200
      GKL = GKL + (( (TWO*RA+RB) / THREE)**J * DR * DABS(ZA**L2-RBK*ZB))
     1               / (DZJ * DZ)
C
  200 DKL = DKL + GKL
C-----
C
C  . ALL DONE
C
  210 CONTINUE
      RETURN
C
C  . ERROR...
C
  300 CALL MESAGE (-7,K,NAM)
       GO TO 210
      END
