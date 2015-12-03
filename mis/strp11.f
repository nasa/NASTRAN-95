      SUBROUTINE STRP11
C
C     PHASE 1 STRESS DATA RECOVERY FOR CTRPLT1 - HIGHER ORDER PLATE
C     ELEMENT
C
C     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING
C
C     1) ELEMENT ID              WORDS    1     STORAGE IN PH1OUT  1
C     2) SIX SILS                WORDS    6                      2-7
C     3) BENDING THICKNESSES     WORDS    3                      8-10
C     4) STRESS POINTS           WORDS    8                     11-18
C     5) 4 NOS. 6 5X6 S MATRICES WORDS    720                   19-738
C     6) 3X1 S SUB T MATRIX      WORDS    3                    739-741
C
C     ECPT ENTRIES
C     AS IN STIFFNESS ROUTINE KTRPL1
C
      LOGICAL          NOTS
      REAL             J11,J12,J22,NSM,IVECT,JVECT,KVECT
      DOUBLE PRECISION DETERM
      DIMENSION        NAME(2),INDEX(20,3),ICS(6),NL(6),Q(6,6),IND(6,3),
     1                 EMOD(9),XC(6),YC(6),ZC(6),QQQ(20,20),QQQINV(360),
     2                 TS6(40),TS7(59),IEST(42),IVECT(3),JVECT(3),
     3                 KVECT(3),E(18),V1(3),V2(3),V3(3),E1(18),
     4                 PH1BEN(9),PH1SHR(6),PH2(18),PH3(12),PH4(90),
     5                 TRANS(9),BALOTR(36),D(9),DPH1(9),G(4),GPH1(6),
     6                 NPH1OU(990)
      COMMON /SDR2X5/  EST(100),PH1OUT(990),FORVEC(24),
     1                 X,Y,Z,DISTA,DISTB,DISTC,A1,A2,A3,B1,B2,B3,
     2                 QQQINV,TS6,TS7,PH2,PH3,PH4,Q,E,E1,TRANS,BALOTR
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN /  MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/  EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                 RJ11,RJ12,RJ22
C
C
C     EQUIVALENCE IECPT WITH ECPT IN COMMON BLOCK /SMA1ET/ SINCE ECPT IS
C     A MIXED INTEGER AND REAL ARRAY
C
      EQUIVALENCE     (A,DISTA), (B,DISTB), (C,DISTC),
     1                (V1(1),EST(19)),(V2(1),EST(23)),(V3(1),EST(27)),
     2                (IEST(1),EST(1)),
     3                (D11,EM(1)),(D12,EM(2)), (D13,EM(3)),
     4                (D22,EM(4)),(D23,EM(5)), (D33,EM(6))
      EQUIVALENCE     (NPH1OU(1),PH1OUT(1))
      EQUIVALENCE     (PH1OUT(401),INDEX(1,1),IND(1,1))
      EQUIVALENCE     (PH1OUT(1),QQQ(1,1))
      DATA  DEGRA  /  0.0174532925            /
      DATA  BLANK  ,  NAME / 4H    , 4HCTRP, 4HLT1   /
C
      NOTS  =.FALSE.
      IDELE = IEST(1)
      DO 109 I = 1,6
      NL(I) = IEST(I+1)
  109 CONTINUE
      THETAM = EST(8)
      MATID1 = IEST(9)
      TMEM1  = (EST(10)*12.0)**0.333333333333
      TMEM3  = (EST(11)*12.0)**0.333333333333
      TMEM5  = (EST(12)*12.0)**0.333333333333
      MATID2 = IEST(13)
      TSHR1  = EST(14)
      TSHR3  = EST(15)
      TSHR5  = EST(16)
      NSM    = EST(17)
      J      = 0
      DO 120 I = 24,44,4
      J      = J + 1
      ICS(J) = IEST(I)
      XC(J)  = EST(I+1)
      YC(J)  = EST(I+2)
      ZC(J)  = EST(I+3)
  120 CONTINUE
C
C     IF TMEM3 OR TMEM5 IS ZERO OR BLANK, THEY WILL BE SET EQUAL TO
C     TMEM1
C     SO ALSO FOR TEMP3 OR TEMP5
C
      IF (TMEM3.EQ.0.0 .OR. TMEM3.EQ.BLANK) TMEM3 = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5.EQ.BLANK) TMEM5 = TMEM1
      IF (TSHR3.EQ.0.0 .OR. TSHR3.EQ.BLANK) TSHR3 = TSHR1
      IF (TSHR5.EQ.0.0 .OR. TSHR5.EQ.BLANK) TSHR5 = TSHR1
      IF (TSHR1 .EQ. 0.0) NOTS = .TRUE.
      ELTEMP = EST(48)
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     EVALUATE MATERIAL PROPERTIES
C
      MATFLG = 2
      MATID  = MATID1
      CALL MAT (IDELE)
C
      EMOD(1) = D11
      EMOD(2) = D12
      EMOD(3) = D13
      EMOD(4) = D12
      EMOD(5) = D22
      EMOD(6) = D23
      EMOD(7) = D13
      EMOD(8) = D23
      EMOD(9) = D33
      MATID   = MATID2
      MATFLG  = 3
      J11     = 0.0
      J12     = 0.0
      J22     = 0.0
      IF (NOTS) GO TO 146
      CALL MAT (IDELE)
  146 CONTINUE
C
C     CALCULATIONS FOR THE TRIANGLE
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
      CALL AF (F,1,A,B,C,A1,A2,A3,TMEM1,TMEM3,TMEM5,1)
      CALL AF (F,1,A,B,C,B1,B2,B3,TSHR1,TSHR3,TSHR5,1)
C
C     FILL E-MATRIX
C
      DO 177 I = 1,18
  177 E( I) = 0.0
      E( 1) = KVECT(1)
      E( 4) = KVECT(2)
      E( 7) = KVECT(3)
      E(11) = IVECT(1)
      E(14) = IVECT(2)
      E(17) = IVECT(3)
      E(12) = JVECT(1)
      E(15) = JVECT(2)
      E(18) = JVECT(3)
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
C
C     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
C
      DO 110 I = 1,20
      DO 110 J = 1,20
  110 QQQ(I,J) = 0.0
      DO 115 I = 1,6
      I1 = (I-1)*3 + 1
      I2 = (I-1)*3 + 2
      I3 = (I-1)*3 + 3
      QQQ(I1, 1) = 1.0
      QQQ(I1, 2) = XC(I)
      QQQ(I1, 3) = YC(I)
      QQQ(I1, 4) = XC(I)*XC(I)
      QQQ(I1, 5) = XC(I)*YC(I)
      QQQ(I1, 6) = YC(I)*YC(I)
      QQQ(I1, 7) = QQQ(I1, 4)*XC(I)
      QQQ(I1, 8) = QQQ(I1, 4)*YC(I)
      QQQ(I1, 9) = QQQ(I1, 5)*YC(I)
      QQQ(I1,10) = QQQ(I1, 6)*YC(I)
      QQQ(I1,11) = QQQ(I1, 7)*XC(I)
      QQQ(I1,12) = QQQ(I1, 7)*YC(I)
      QQQ(I1,13) = QQQ(I1, 8)*YC(I)
      QQQ(I1,14) = QQQ(I1, 9)*YC(I)
      QQQ(I1,15) = QQQ(I1,10)*YC(I)
      QQQ(I1,16) = QQQ(I1,11)*XC(I)
      QQQ(I1,17) = QQQ(I1,12)*YC(I)
      QQQ(I1,18) = QQQ(I1,13)*YC(I)
      QQQ(I1,19) = QQQ(I1,14)*YC(I)
      QQQ(I1,20) = QQQ(I1,15)*YC(I)
      QQQ(I2, 3) = 1.0
      QQQ(I2, 5) = XC(I)
      QQQ(I2, 6) = YC(I)*2.0
      QQQ(I2, 8) = QQQ(I1, 4)
      QQQ(I2, 9) = QQQ(I1, 5)*2.0
      QQQ(I2,10) = QQQ(I1, 6)*3.0
      QQQ(I2,12) = QQQ(I1, 7)
      QQQ(I2,13) = QQQ(I1, 8)*2.0
      QQQ(I2,14) = QQQ(I1, 9)*3.0
      QQQ(I2,15) = QQQ(I1,10)*4.0
      QQQ(I2,17) = QQQ(I1,12)*2.0
      QQQ(I2,18) = QQQ(I1,13)*3.0
      QQQ(I2,19) = QQQ(I1,14)*4.0
      QQQ(I2,20) = QQQ(I1,15)*5.0
      QQQ(I3, 2) =-1.0
      QQQ(I3, 4) =-2.0*XC(I)
      QQQ(I3, 5) =-YC(I)
      QQQ(I3, 7) =-QQQ(I1, 4)*3.0
      QQQ(I3, 8) =-QQQ(I1, 5)*2.0
      QQQ(I3, 9) =-QQQ(I1, 6)
      QQQ(I3,11) =-QQQ(I1, 7)*4.0
      QQQ(I3,12) =-QQQ(I1, 8)*3.0
      QQQ(I3,13) =-QQQ(I1, 9)*2.0
      QQQ(I3,14) =-QQQ(I1,10)
      QQQ(I3,16) =-QQQ(I1,11)*5.0
      QQQ(I3,17) =-QQQ(I1,13)*3.0
      QQQ(I3,18) =-QQQ(I1,14)*2.0
      QQQ(I3,19) =-QQQ(I1,15)
C
C     IF NO TRANSVERSE SHEAR GO TO 113
C
      IF (NOTS) GO TO 1137
      X = XC(I)
      Y = YC(I)
      CALL STRPTS (TS6,NOTS)
      DO 113 JJ = 1,20
      QQQ(I2,JJ) = QQQ(I2,JJ) - TS6(20+JJ)
      QQQ(I3,JJ) = QQQ(I3,JJ) + TS6(   JJ)
  113 CONTINUE
 1137 CONTINUE
  115 CONTINUE
      QQQ(19,16) = 5.0*A**4*C
      QQQ(19,17) = 3.0*A**2*C**3 - 2.0*A**4*C
      QQQ(19,18) =-2.0*A*C**4 + 3.0*A**3*C**2
      QQQ(19,19) = C**5 - 4.0*A**2*C**3
      QQQ(19,20) = 5.0*A*C**4
      QQQ(20,16) = 5.0*B**4*C
      QQQ(20,17) = 3.0*B**2*C**3 - 2.0*B**4*C
      QQQ(20,18) = 2.0*B*C**4 - 3.0*B**3*C**2
      QQQ(20,19) = C**5 - 4.0*B**2*C**3
      QQQ(20,20) =-5.0*B*C**4
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
C     IS U
C
      CALL INVERS (20,QQQ,20,TS6(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      DO 152 I = 1,20
      DO 152 J = 1,18
      IJ = (I-1)*18 + J
      QQQINV(IJ) = QQQ(I,J)
  152 CONTINUE
      DO 154 I = 1,36
  154 BALOTR(I) = 0.0
C
      DO 102 I = 1,7
      PH1OUT(I) = EST(I)
  102 CONTINUE
      PH1OUT( 8) = TMEM1
      PH1OUT( 9) = TMEM3
      PH1OUT(10) = TMEM5
      PH1OUT(11) = EST(18)
      PH1OUT(12) = EST(19)
      PH1OUT(13) = EST(20)
      PH1OUT(14) = EST(21)
      PH1OUT(15) = EST(22)
      PH1OUT(16) = EST(23)
      DO 700 JJ = 1,4
      JJ1 = JJ*2 - 1
      IF (JJ .NE. 4) X = XC(JJ1)
      IF (JJ .NE. 4) Y = YC(JJ1)
      IF (JJ .EQ. 4) X = (XC(1)+XC(3)+XC(5))/3.0
      IF (JJ .EQ. 4) Y = (YC(1)+YC(3)+YC(5))/3.0
      IF (JJ .EQ. 4) PH1OUT(17) = (A1+A2*X+A3*Y)/2.0
      IF( JJ .EQ. 4) PH1OUT(18) = -PH1OUT(17)
      DO 105 I = 1,60
      TS7(I) = 0.0
  105 CONTINUE
      AI = PH1OUT(7+JJ)**3/12.0
      IF (JJ .EQ. 4) AI = PH1OUT(17)**3/1.5
      DO 107 I = 1,9
  107 D(I) = EMOD(I)*AI
      X2  = X*X
      XY  = X*Y
      Y2  = Y*Y
      X3  = X2*X
      X2Y = X2*Y
      XY2 = X*Y2
      Y3  = Y2*Y
      TS7( 4) = 2.0
      TS7( 7) = 6.0*X
      TS7( 8) = 2.0*Y
      TS7(11) = 12.0*X2
      TS7(12) = 6.0*XY
      TS7(13) = 2.0*Y2
      TS7(16) = 20.0*X3
      TS7(17) = 6.0*XY2
      TS7(18) = 2.0*Y3
      TS7(26) = 2.0
      TS7(29) = 2.0*X
      TS7(30) = 6.0*Y
      TS7(33) = 2.0*X2
      TS7(34) = TS7(12)
      TS7(35) = 12.0*Y2
      TS7(37) = 2.0*X3
      TS7(38) = 6.0*X2Y
      TS7(39) = 12.0*XY2
      TS7(40) = 20.0*Y3
      TS7(45) = 2.0
      TS7(48) = 4.0*X
      TS7(49) = 4.0*Y
      TS7(52) = 6.0*X2
      TS7(53) = 8.0*XY
      TS7(54) = 6.0*Y2
      TS7(57) = 12.0*X2Y
      TS7(58) = TS7(39)
      TS7(59) = 8.0*Y3
      CALL GMMATS (TS7,3,20,0, QQQINV,20,18,0, PH4(1))
      CALL STRPTS (TS6,NOTS)
      CALL GMMATS (TS6,2,20,0, QQQINV,20,18,0, PH4(55))
      DO 600 II = 1,6
      IF (ICS(II) .EQ. 0) GO TO 130
      J = 4*II + 20
      CALL TRANSS (IEST(J),TRANS)
      DO 124 J = 1,3
      L = 6*(J-1) + 1
      M = 3*(J-1) + 1
      BALOTR(L   ) = TRANS(M  )
      BALOTR(L+ 1) = TRANS(M+1)
      BALOTR(L+ 2) = TRANS(M+2)
      BALOTR(L+21) = TRANS(M  )
      BALOTR(L+22) = TRANS(M+1)
      BALOTR(L+23) = TRANS(M+2)
  124 CONTINUE
      CALL GMMATS (E,6,3,+1, BALOTR,6,6,0, E1)
      GO TO 133
  130 CONTINUE
      DO 132 I = 1,3
      DO 132 J = 1,6
      I1 = (I-1)*6 + J
      J1 = (J-1)*3 + I
      E1(I1) = E(J1)
  132 CONTINUE
  133 CONTINUE
      KZ = (II-1)*3 + 1
      PH1BEN(1) = PH4(KZ   )
      PH1BEN(2) = PH4(KZ+ 1)
      PH1BEN(3) = PH4(KZ+ 2)
      PH1BEN(4) = PH4(KZ+18)
      PH1BEN(5) = PH4(KZ+19)
      PH1BEN(6) = PH4(KZ+20)
      PH1BEN(7) = PH4(KZ+36)
      PH1BEN(8) = PH4(KZ+37)
      PH1BEN(9) = PH4(KZ+38)
      CALL GMMATS (D,3,3,0, PH1BEN,3,3,0, DPH1)
      CALL GMMATS (DPH1,3,3,0, E1,3,6,0, PH2)
      MZ = (II-1)*3 + 55
      PH1SHR(1) = PH4(MZ   )
      PH1SHR(2) = PH4(MZ+ 1)
      PH1SHR(3) = PH4(MZ+ 2)
      PH1SHR(4) = PH4(MZ+18)
      PH1SHR(5) = PH4(MZ+19)
      PH1SHR(6) = PH4(MZ+20)
      IF (NOTS) GO TO 166
      THK  = B1 + B2*X + B3*Y
      G(1) = EM(6)*THK
      G(2) = 0.0
      G(3) = 0.0
      G(4) = G(1)
      CALL GMMATS (G,2,2,0, PH1SHR,2,3,0, GPH1)
      GO TO 168
  166 CONTINUE
      GPH1(1) = PH1SHR(1)
      GPH1(2) = PH1SHR(2)
      GPH1(3) = PH1SHR(3)
      GPH1(4) = PH1SHR(4)
      GPH1(5) = PH1SHR(5)
      GPH1(6) = PH1SHR(6)
  168 CONTINUE
      CALL GMMATS (GPH1,2,3,0, E1,3,6,0, PH3)
      DO 148 I = 1,3
      DO 148 J = 1,6
      I1 = (I-1)*6 + J
      I2 = I1 + 18
      J1 = (II-1)*30 + (JJ-1)*180 + I1 + 18
      J2 = J1 + 18
      PH1OUT(J1) = PH2(I1)
      IF (I .NE. 3) PH1OUT(J2) = PH3(I1)
  148 CONTINUE
  600 CONTINUE
      JJ1 = (JJ-1)*3 + 1
      CALL GMMATS (D,3,3,0, ALF,3,1,0, PH1OUT(738+JJ1))
  700 CONTINUE
      RETURN
      END
