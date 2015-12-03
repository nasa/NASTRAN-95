      SUBROUTINE SSPLIN(NI,XYI,ND,XYD,KX,KY,KD,KT,DZ,G,NCORE,ISNG)
      LOGICAL LX,LY,LONE,IKD,IKT
      DIMENSION    G(1),XYI(1),XYD(1),NAME(2)
      REAL DET
      DATA NAME/4HSSPL,4HIN  /
      LONE = .TRUE.
      LX = .TRUE.
      LY = .TRUE.
      IKT = .FALSE.
      IKD = .FALSE.
      IF(KY.LT.0.OR.KX.LT.0) LONE = .FALSE.
      IF(KY.LT.0.OR.KX.GT.0) LX   = .FALSE.
      IF(KY.GT.0.OR.KX.LT.0) LY   = .FALSE.
      N = NI
      IF(LONE) N=N+1
      IF(LX  ) N=N+1
      IF(LY  ) N=N+1
      EX = FLOAT(KX)
      EY = FLOAT(KY)
      IF(KT.EQ.1) IKT = .TRUE.
      IF(KD.EQ.1)  IKD = .TRUE.
      NB = ND*(1+KD)
C
C     CORE NEEDED
C
C                A          G         INVERS
      NEEDED =           NB*NI      + 3*N
C                               B         C
      IF(IKT) NEEDED = NEEDED + NB*N  + NI*N
C                                      C          A OR B
      IF(.NOT.IKT) NEEDED = NEEDED + NI*N + MAX0(N*N,NB*N)
      IF(NEEDED.GT.NCORE) CALL MESAGE(-8,0,NAME)
      IS = NCORE - 3*N -1
      IG = 1
C
C     IF  KT = 1 COMPUTE B THEN A THEN C IN A SPACE
C
C     IF KT = 0 COMPUTE C THEN A THEN B IN A SPACE
C
      NT = 2*NI
      IF(.NOT.IKT) GO TO 65
      GO TO 95
C
C     COMPUTE TO A MATRIX
C
    1 K = IA
C
C     ZERO A
C
      II = K+1
      IK = II + N*N
      DO 5 I = II,IK
    5 G(I) = 0.0
      II = 1
      IK = 0
      DO 60 I = 1,NT,2
      K = K+IK
      JJ = I/2
      DO 20 J = I,NT,2
      K = K+1
      JJ = JJ +1
      SUM = 0.0
      XM = (XYI(I) - XYI(J)) **2
      XP = (XYI(I) + XYI(J)) **2
      YM = (XYI(I+1) - XYI(J+1)) **2
      YP = (XYI(I+1) + XYI(J+1)) **2
      T1 = XM+YM
      T2 = XP+YM
      T3 = XM+YP
      T4 = XP+YP
      IF(T1.NE.0.0) SUM = T1 * ALOG(T1)
      IF(T2.NE.0.0.AND.KX.NE.0)SUM = SUM + (T2*ALOG(T2)*EX)
      IF(T3.NE.0.0.AND.KY.NE.0) SUM = SUM + (T3 * ALOG(T3) * EY)
      IF(T4.NE.0.0.AND.KY.NE.0.AND.KX.NE.0)SUM=SUM+(T4*ALOG(T4)*EX*EY)
      IF(J.EQ.I) GO TO 10
      G(K) = SUM
C
C     SYMETRY TERM
C
      KK = K + (N-1)*(JJ-II)
      G(KK) = SUM
      GO TO 20
   10 G(K) = SUM + DZ
      KK = K
   20 CONTINUE
      INR = 0
      IF(.NOT.LONE) GO TO 30
      INR = INR +1
      G(K+INR) = 1.0
      G(KK+INR*N) = 1.0
   30 IF(.NOT.LX) GO TO 40
      INR = INR +1
      G(K+INR) = XYI(I)
      G(KK+INR*N) = XYI(I)
   40 IF(.NOT.LY) GO TO 50
      INR = INR +1
      G(K+INR) = XYI(I+1)
      G(KK+INR*N) = XYI(I+1)
   50 IK = II + INR
      II = II +1
   60 CONTINUE
C
C     CALL INVERS FOR A-1 C  OR A-1 B
C
C     REPLACE CALLS TO INVAER WITH CALLS TO INVERS
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISNG = -1
      CALL INVERS(N,G(IA+1),N,G(MP),NC,DET,ISNG,G(IS))
      IF(ISNG.EQ.2) GO TO 1000
      IF(.NOT.IKT) GO TO 100
      IC = IA
      K = IC+1
      GO TO 70
C
C     C MATRIX COLUMN STORED
C
   65 IC = NB*NI
      MP = IC+1
   70 DO 90 I = 1,NI
      DO 80 J = 1,N
      IC = IC+1
      G(IC) = 0.0
      IF(I.EQ.J) G(IC) = 1.0
   80 CONTINUE
   90 CONTINUE
      IF(IKT) GO TO 170
      NC = NI
      IA = IC
      GO TO 1
C
C     B MATRIX COLUMN STORED
C
   95 IB = NB*NI
      MP = IB +1
      GO TO 110
  100 IB = IA
  110 NR = 2*ND
      K = IB +1
      DO 160 J = 1,NR,2
      DO 120 I = 1,NT,2
      IB = IB+1
      ALT1 = 0.0
      ALT2 = 0.0
      ALT3 = 0.0
      ALT4 = 0.0
      XM = XYD(J)- XYI(I)
      XP = XYI(I) + XYD(J)
      YM = XYD(J+1) - XYI(I+1)
      YP = XYI(I+1) + XYD(J+1)
      T1 = XM*XM + YM*YM
      T2 = XP*XP + YM*YM
      T3 = XM*XM + YP*YP
      T4 = XP*XP +YP*YP
      IF(T1.NE.0.0) ALT1 = ALOG(T1)
      IF(T2.NE.0.0.AND.KX.NE.0)ALT2 = ALOG(T2)
      IF(T3.NE.0.0.AND.KY.NE.0)ALT3 = ALOG(T3)
      IF(T4.NE.0.0.AND.KX.NE.0.AND.KY.NE.0) ALT4 = ALOG(T4)
      G(IB) = T1*ALT1 + T2*ALT2*EX + T3*ALT3*EY + T4*ALT4*EX*EY
      IF(.NOT.IKD) GO TO 120
      IK = IB + N
      G(IK) = 2.0*( XM*(1.0+ALT1) + XP*(1.0+ALT2)*EX +
     *        XM*(1.0+ALT3)*EY + XP*(1.0+ALT4)*EX*EY)
  120 CONTINUE
      INR = 0
      IF(.NOT.LONE) GO TO 130
      INR = INR +1
      G(IB+INR) = 1.0
      IF(IKD) G(IB+INR+N) = 0.0
  130 IF(.NOT.LX) GO TO 140
      INR = INR +1
      G(IB+INR) = XYD(J)
      IF(IKD) G(IB+INR+N) = 1.0
  140 IF(.NOT.LY) GO TO 150
      INR = INR +1
      G(IB+INR) = XYD(J+1)
      IF(IKD) G(IB+INR+N) = 0.0
  150 IB = IB+INR + N*KD
  160 CONTINUE
      IF(.NOT.IKT) GO TO 180
      IA = IB
      NC = NB
      GO TO 1
  170 CONTINUE
C
C     GMMATS WANTS ROW STORED SO INVERT ROWS AND COLUMNS AND INVERT
C     MULTIPLICATION ORDER
C
      CALL GMMATS(G(MP),NB,N,0,G(K),NI,N,1,G(IG))
      GO TO 1000
  180 CONTINUE
      CALL GMMATS(G(MP),NI,N,0,G(K),NB,N,1,G(IG))
 1000 RETURN
      END
