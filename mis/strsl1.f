      SUBROUTINE STRSL1
C
C     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING
C
C     1)  ELEMENT ID              WORDS    1    STORAGE IN PH1OUT  1
C     2)  SIX SILS                WORDS    6                     2-7
C     3)  3 MEMBRANE  THICKNESES  WORDS    3                     8-10
C     4)  3 BENDING  THICKNESES   WORDS    3                    11-13
C     5)  8  STRESS DATA POINTS   WORDS    8                    14-21
C     6)  4 NOS. STRESS MATRICES (6-5X6 EACH) WORDS 720         22-741
C     7)  S SUB T MATRIX          WORDS  4X3                   742-753
C     8)  ELEMENT ID              WORD     1                   754
C     9)  SIX SILS                WORDS    6                   755-760
C     10) ELEMENT TEMPERATURE     WORD     1                   761
C     11) 4 NOS. MEMBRANE STRESS MATRICES 4(6-3X3)             762-1193
C
C     ECPT LISTS
C
C     ECPT ( 1) = ELEMENT ID                                    INTEGER
C     ECPT ( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1          INTEGER
C     ECPT ( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2          INTEGER
C     ECPT ( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3          INTEGER
C     ECPT ( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4          INTEGER
C     ECPT ( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5          INTEGER
C     ECPT ( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6          INTEGER
C     ECPT ( 8) = THETA                                         REAL
C     ECPT ( 9) = MATERIAL ID 1                                 INTEGER
C     ECPT (10) = THICKNESS T1 AT GRID POINT G1
C     ECPT (11) = THICKNESS T3 AT GRID POINT G3
C     ECPT (12) = THICKNESS T5 AT GRID POINT G5
C     ECPT (13) = MATERIAL ID 2                                 INTEGER
C     ECPT (14) = THICKNESS TBEND1 FOR BENDING AT GRID POINT G1
C     ECPT (15) = THICKNESS TBEND3 FOR BENDING AT GRID POINT G3
C     ECPT (16) = THICKNESS TBEND5 FOR BENDING AT GRID POINT G5
C     ECPT (17) = MATERIAL ID 3                                 INTEGER
C     ECPT (18) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID POINT G1
C     ECPT (19) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID POINT G3
C     ECPT (20) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID POINT G5
C     ECPT (21) = NON-STRUCTURAL MASS                           REAL
C     ECPT (22) = DISTANCE Z11 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT (23) = DISTANCE Z21 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT (24) = DISTANCE Z13 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT (25) = DISTANCE Z23 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT (26) = DISTANCE 015 FOR STRESS CALCULATION  AT GRID POINT G5
C     ECPT (27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
C
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
C
C     ECPT (28) = CO-ORDINATE SYSTEM ID FOR GRID A              INTEGER
C     ECPT (29) = CO-ORDINATE X1                                REAL
C     ECPT (30) = CO-ORDINATE Y1                                REAL
C     ECPT (31) = CO-ORDINATE Z1                                REAL
C     ECPT (32) = CO-ORDINATE SYSTEM ID FOR GRID B              INTEGER
C     ECPT (33) = CO-ORDINATE X1                                REAL
C     ECPT (34) = CO-ORDINATE Y1                                REAL
C     ECPT (35) = CO-ORDINATE Z1                                REAL
C     ECPT (36) = CO-ORDINATE SYSTEM ID FOR GRID C              INTEGE9
C     ECPT (37) = CO-ORDINATE X1                                REAL
C     ECPT (38) = CO-ORDINATE Y1                                REAL
C     ECPT (39) = CO-ORDINATE Z1                                REAL
C     ECPT (40) = CO-ORDINATE SYSTEM ID FOR GRID D              INTEGER
C     ECPT (41) = CO-ORDINATE X1                                REAL
C     ECPT (42) = CO-ORDINATE Y1                                REAL
C     ECPT (43) = CO-ORDINATE Z1                                REAL
C     ECPT (44) = CO-ORDINATE SYSTEM ID FOR GRID E              INTEGER
C     ECPT (45) = CO-ORDINATE X1                                REAL
C     ECPT (46) = CO-ORDINATE Y1                                REAL
C     ECPT (47) = CO-ORDINATE Z1                                REAL
C     ECPT (48) = CO-ORDINATE SYSTEM ID FOR GRID F              INTEGER
C     ECPT (49) = CO-ORDINATE X1                                REAL
C     ECPT (50) = CO-ORDINATE Y1                                REAL
C     ECPT (51) = CO-ORDINATE Z1                                REAL
C     ECPT (52) = ELEMENT TEMPERATURE  AT GRID POINTS G1        REAL
C     ECPT (53) = ELEMENT TEMPERATURE  AT GRID POINTS G2        REAL
C     ECPT (54) = ELEMENT TEMPERATURE  AT GRID POINTS G3        REAL
C     ECPT (55) = ELEMENT TEMPERATURE  AT GRID POINTS G4        REAL
C     ECPT (56) = ELEMENT TEMPERATURE  AT GRID POINTS G5        REAL
C     ECPT (57) = ELEMENT TEMPERATURE  AT GRID POINTS G6        REAL
C
      LOGICAL         NOTS
      REAL            J11,J12,J22,NSM,IVECT(3),JVECT(3),KVECT(3)
      DIMENSION       NAME(2),INDEX(20,3),ICS(6),NL(6),XC(6),YC(6),
     1                ZC(6),QQQ(20,20),QQQINV(360),TS6(40),TS7(60),
     2                E(18),V1(3),V2(3),V3(3),IEST(42),E1(18),PH1BEN(9),
     3                PH1SHR(6),PH2(18),PH3(12),PH4(90),TMMM(36),
     4                Q(6,6),IND(6,3),CAB(3),EE( 30),PH1MEM(6),EPH1(15),
     5                SI(9),EMOD(9),D(9),DPH1(9),G(4),GPH1(6),
     6                NPH1OU(990),TM(96),TMQQ(90),EE1(5,6),TMM(3,12),
     7                TMB(60),TMBQ(54),TRANS(9),BALOTR(36)
      COMMON /MATIN / MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/ EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                RJ11,RJ12,RJ22
      COMMON /SDR2X5/ EST(100),PH1OUT(1200),FORVEC(24),X,Y,Z,DISTA,
     1                DISTB,DISTC,A1,A2,A3,AA1,AA2,AA3,QQQINV,QQ,TM,
     2                TMQQ,TS6,TS7,Q,EE,EE1,PH2,PH3,PH4,E,E1,XC,YC,ZC,
     3                PH1MEM,PH1BEN,PH1SHR,DPH1,EPH1,GPH1,G,D,ICS,NL,
     4                CAB,TRANS,BALOTR,EMOD,SI
      EQUIVALENCE     (A,DISTA),(B,DISTB),(C,DISTC),(V1(1),EST(29)),
     1                (V2(1),EST(37)),(V3(1),EST(45)),(IEST(1),EST(1)),
     2                (TMMM(1),TMM(1,1)),(PH1OUT(1),QQQ(1,1)),
     3                (PH1OUT(401),INDEX(1,1),IND(1,1)),
     4                (NPH1OU(1),PH1OUT(1))
      DATA   DEGRA  / 0.0174532925 /
      DATA   BLANK  , NAME / 4H    , 4HTRSH, 4HL     /
C
      NOTS   = .TRUE.
      IDELE  = IEST(1)
      DO 10 I = 1,6
      NL(I)  = IEST(I+1)
   10 CONTINUE
      THETAM = EST(8)
      MATID1 = IEST(9)
      TMEM1  = EST(10)
      TMEM3  = EST(11)
      TMEM5  = EST(12)
      MATID2 = IEST(13)
      TBEND1 = (EST(14)*12.0)**0.3333333333
      TBEND3 = (EST(15)*12.0)**0.3333333333
      TBEND5 = (EST(16)*12.0)**0.3333333333
      MATID3 = IEST(17)
      TSHR1  = EST(18)
      TSHR3  = EST(19)
      TSHR5  = EST(20)
      NSM    = EST(21)
      J      = 0
      IF (TMEM3.EQ.0.0 .OR. TMEM3.EQ.BLANK) TMEM3 = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5.EQ.BLANK) TMEM5 = TMEM1
      IF (TSHR3.EQ.0.0 .OR. TSHR3.EQ.BLANK) TSHR3 = TSHR1
      IF (TSHR5.EQ.0.0 .OR. TSHR5.EQ.BLANK) TSHR5 = TSHR1
      IF (TSHR1 .EQ. 0.0) NOTS =.TRUE.
      IF (TBEND3.EQ.0.0 .OR. TBEND3.EQ.BLANK) TBEND3 = TBEND1
      IF (TBEND5.EQ.0.0 .OR. TBEND5.EQ.BLANK) TBEND5 = TBEND1
      DO 20 I = 28,48,4
      J = J + 1
      ICS(J) = IEST(I )
      XC(J)  = EST(I+1)
      YC(J)  = EST(I+2)
      ZC(J)  = EST(I+3)
   20 CONTINUE
      ELTEMP = EST(52)
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     EVALUATE MATERIAL PROPERTIES
C
      MATFLG = 2
      MATID  = MATID1
      IF (MATID1 .EQ. 0) GO TO 30
      CALL MAT (IDELE)
      G11 = EM(1)
      G12 = EM(2)
      G13 = EM(3)
      G22 = EM(4)
      G23 = EM(5)
      G33 = EM(6)
C
   30 MATID = MATID2
      IF (MATID2 .EQ. 0) GO TO 40
      MATFLG = 2
      CALL MAT (IDELE)
      D11  = EM(1)
      D12  = EM(2)
      D13  = EM(3)
      D21  = D12
      D22  = EM(4)
      D23  = EM(5)
      D31  = D13
      D32  = D23
      D33  = EM(6)
      D(1) = D11
      D(2) = D12
      D(3) = D13
      D(4) = D21
      D(5) = D22
      D(6) = D23
      D(7) = D13
      D(8) = D23
      D(9) = D33
      D334 = D33*4.0
      D132 = D13*2.0
      D232 = D23*2.0
      J11  = 0.0
      J12  = 0.0
      J22  = 0.0
      IF (NOTS) GO TO 40
      CALL MAT (IDELE)
C
C     CALCULATIONS FOR THE TRIANGLE
C
   40 CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
C     THICKNESS VARIATION - MEMBRANE
C
      CALL AF (F,1,A,B,C,C1,C2,C3,TMEM1,TMEM2,TMEM3,1)
      CAB(1) = C1
      CAB(2) = C2
      CAB(3) = C3
C
C     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
C     OF BENDING THICKNESSES
C
      CALL AF (F,1,A,B,C,A1,A2,A3,TBEND1,TBEND3,TBEND5,1)
      A1SQ = A1*A1
      A2SQ = A2*A2
      A3SQ = A3*A3
      C1   = A1SQ*A1
      C2   = 3.0*A1SQ*A2
      C3   = 3.0*A1SQ*A3
      C4   = 3.0*A1*A2SQ
      C5   = 6.0*A1*A2*A3
      C6   = 3.0*A3SQ*A1
      C7   = A2SQ*A2
      C8   = 3.0*A2SQ*A3
      C9   = 3.0*A2*A3SQ
      C10  = A3*A3SQ
C
C     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
C     TRANSVERSE SHEAR
C
      CALL AF (F,1,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,1)
C
C
C     FILL E-MATRIX
C
      DO 50 I = 1,18
   50 E(I)  = 0.0
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
C     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
C
      DO 60 I = 1,400
   60 QQQ(I,1) = 0.0
      DO 70 I = 1,6
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
   70 CONTINUE
C
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
      DO 80 I = 1,6
      DO 80 J = 1,6
      I1 = 3*(I-1) + 1
   80 Q(I,J) = QQQ(I1,J)
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (6,Q,6,TS6(1),0,DET,ISING,IND)
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
C     IS U
C
C     SET ISING = -1 AGAIN.
      ISING = -1
      CALL INVERS (20,QQQ,20,TS6(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      H4 = Q(4,1)*ZC(1) + Q(4,2)*ZC(2) + Q(4,3)*ZC(3) + Q(4,4)*ZC(4) +
     1     Q(4,5)*ZC(5) + Q(4,6)*ZC(6)
      H5 = Q(5,1)*ZC(1) + Q(5,2)*ZC(2) + Q(5,3)*ZC(3) + Q(5,4)*ZC(4) +
     1     Q(5,5)*ZC(5) + Q(5,6)*ZC(6)
      H6 = Q(6,1)*ZC(1) + Q(6,2)*ZC(2) + Q(6,3)*ZC(3) + Q(6,4)*ZC(4) +
     1     Q(6,5)*ZC(5) + Q(6,6)*ZC(6)
      H4 = H4*2.0
      H6 = H6*2.0
C
C     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY * W
C
      H5 = H5*2.0
      DO 90 I = 1,20
      DO 90 J = 1,18
      IJ = (I-1)*18 + J
      QQQINV(IJ) = QQQ(I,J)
   90 CONTINUE
      DO 100 I = 1,36
  100 BALOTR(I) = 0.0
C
      DO 110 I = 1,7
      NPH1OU (I) = IEST(I)
  110 CONTINUE
      IF (MATID2.EQ.0) GO TO 120
      GO TO 140
  120 DO 130 I = 2,7
      NPH1OU (I) = 0
  130 CONTINUE
  140 PH1OUT( 8) = TMEM1
      PH1OUT( 9) = TMEM3
      PH1OUT(10) = TMEM5
      PH1OUT(11) = TBEND1
      PH1OUT(12) = TBEND3
      PH1OUT(13) = TBEND5
      PH1OUT(14) = EST(22)
      PH1OUT(15) = EST(23)
      PH1OUT(16) = EST(24)
      PH1OUT(17) = EST(25)
      PH1OUT(18) = EST(26)
      PH1OUT(19) = EST(27)
      EMOD(1) = G11
      EMOD(2) = G12
      EMOD(3) = G13
      EMOD(4) = G12
      EMOD(5) = G22
      EMOD(6) = G23
      EMOD(7) = G13
      EMOD(8) = G23
      EMOD(9) = G33
      DO 150 I = 1,30
  150 EE(I)   = 0.0
      EE( 1)  = IVECT(1)
      EE( 2)  = JVECT(1)
      EE( 3)  = KVECT(1)
      EE( 6)  = IVECT(2)
      EE( 7)  = JVECT(2)
      EE( 8)  = KVECT(2)
      EE(11)  = IVECT(3)
      EE(12)  = JVECT(3)
      EE(13)  = KVECT(3)
      EE(19)  = IVECT(1)
      EE(20)  = JVECT(1)
      EE(24)  = IVECT(2)
      EE(25)  = JVECT(2)
      EE(29)  = IVECT(3)
      EE(30)  = JVECT(3)
      DO 340 JJ = 1,4
      J = 2*JJ - 1
      IF (JJ .EQ. 4) GO TO 160
      X = XC(J)
      Y = YC(J)
      GO TO 170
  160 X = (XC(1) + XC(3) + XC(5))/3.0
      Y = (YC(1) + YC(3) + YC(5))/3.0
      PH1OUT(20) = (A1 + A2*X + A3*Y)/2.0
      PH1OUT(21) =-PH1OUT(20)
  170 IF (MATID2 .EQ. 0) GO TO 190
      DO 180 I = 1,60
      TS7(I) = 0.0
  180 CONTINUE
      THK  = A1 + A2*X + A3*Y
      THK1 = THK**3/12.0
      D(1) = D11*THK1
      D(2) = D12*THK1
      D(3) = D13*THK1
      D(4) = D(2)
      D(5) = D22*THK1
      D(6) = D23*THK1
      D(7) = D(3)
      D(8) = D(6)
      D(9) = D33*THK1
      X2   = X*X
      XY   = X*Y
      Y2   = Y*Y
      X3   = X2*X
      X2Y  = X2*Y
      XY2  = X*Y2
      Y3   = Y2*Y
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
      CALL GMMATS (TS7,3,20,0,QQQINV,20,18,0,PH4(1))
      CALL STRSLV (TS6,NOTS)
      CALL GMMATS (TS6,2,20,0,QQQINV,20,18,0,PH4(55))
C
  190 IF (MATID1 .EQ. 0) GO TO 220
      DO 200 I = 1,36
      TMMM(I) = 0.0
  200 CONTINUE
      DO 210 J = 1,6
      J1 = (J-1)*2 + 1
      J2 = J1 + 1
      TMM(1,J1) = Q(2,J) + 2.0*X*Q(4,J) + Y*Q(5,J)
      TMM(2,J2) = Q(3,J) + X*Q(5,J) + 2.0*Y*Q(6,J)
      TMM(3,J1) = TMM(2,J2)
      TMM(3,J2) = TMM(1,J1)
  210 CONTINUE
      X4   = X3*X
      X3Y  = X3*Y
      X2Y2 = X2*Y2
      XY3  = X*Y3
      Y4   = Y*Y3
      X5   = X4*X
      X3Y2 = X3*Y2
      X2Y3 = X2*Y3
      XY4  = X*Y4
      Y5   = Y*Y4
      TMB( 1) = -H4
      TMB( 2) = -H4*X
      TMB( 3) = -H4*Y
      TMB( 4) = -H4*X2
      TMB( 5) = -H4*XY
      TMB( 6) = -H4*Y2
      TMB( 7) = -H4*X3
      TMB( 8) = -H4*X2Y
      TMB( 9) = -H4*XY2
      TMB(10) = -H4*Y3
      TMB(11) = -H4*X4
      TMB(12) = -H4*X3Y
      TMB(13) = -H4*X2Y2
      TMB(14) = -H4*XY3
      TMB(15) = -H4*Y4
      TMB(16) = -H4*X5
      TMB(17) = -H4*X3Y2
      TMB(18) = -H4*X2Y3
      TMB(19) = -H4*XY4
      TMB(20) = -H4*Y5
      TMB(21) = -H6
      TMB(22) = -H6*X
      TMB(23) = -H6*Y
      TMB(24) = -H6*X2
      TMB(25) = -H6*XY
      TMB(26) = -H6*Y2
      TMB(27) = -H6*X3
      TMB(28) = -H6*X2Y
      TMB(29) = -H6*XY2
      TMB(30) = -H6*Y3
      TMB(31) = -H6*X4
      TMB(32) = -H6*X3Y
      TMB(33) = -H6*X2Y2
      TMB(34) = -H6*XY3
      TMB(35) = -H6*Y4
      TMB(36) = -H6*X5
      TMB(37) = -H6*X3Y2
      TMB(38) = -H6*X2Y3
      TMB(39) = -H6*XY4
      TMB(40) = -H6*Y5
      TMB(41) = -H5
      TMB(42) = -H5*X
      TMB(43) = -H5*Y
      TMB(44) = -H5*X2
      TMB(45) = -H5*XY
      TMB(46) = -H5*Y2
      TMB(47) = -H5*X3
      TMB(48) = -H5*X2Y
      TMB(49) = -H5*XY2
      TMB(50) = -H5*Y3
      TMB(51) = -H5*X4
      TMB(52) = -H5*X3Y
      TMB(53) = -H5*X2Y2
      TMB(54) = -H5*XY3
      TMB(55) = -H5*Y4
      TMB(56) = -H5*X5
      TMB(57) = -H5*X3Y2
      TMB(58) = -H5*X2Y3
      TMB(59) = -H5*XY4
      TMB(60) = -H5*Y5
      CALL GMMATS (TMB,3,20,0, QQQINV,20,18,0, TMBQ)
C
  220 DO 330 II = 1,6
      IF (ICS(II) .EQ. 0) GO TO 240
      CALL TRANSS (IEST(4*II+24),TRANS)
      DO 230 J = 1,3
      L = 6*(J-1) + 1
      M = 3*(J-1) + 1
      BALOTR(L   ) = TRANS(M  )
      BALOTR(L+1 ) = TRANS(M+1)
      BALOTR(L+2 ) = TRANS(M+2)
      BALOTR(L+21) = TRANS(M  )
      BALOTR(L+22) = TRANS(M+1)
      BALOTR(L+23) = TRANS(M+2)
  230 CONTINUE
      CALL GMMATS (E,6,3,+1, BALOTR,6,6,0, E1)
      GO TO 260
  240 DO 250 I = 1,3
      DO 250 J = 1,6
      I1 = (I-1)*6 + J
      J1 = (J-1)*3 + I
      E1(I1) = E(J1)
  250 CONTINUE
  260 IF (MATID2 .EQ. 0) GO TO 300
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
      IF (NOTS) GO TO 270
      THK  = AA1 + AA2*X + AA3*Y
      G(1) = EM(6)*THK
      G(2) = 0.0
      G(3) = 0.0
      G(4) = G(1)
      CALL GMMATS (G,2,2,0, PH1SHR,2,3,0, GPH1)
      GO TO 280
  270 GPH1(1) = PH1SHR(1)
      GPH1(2) = PH1SHR(2)
      GPH1(3) = PH1SHR(3)
      GPH1(4) = PH1SHR(4)
      GPH1(5) = PH1SHR(5)
      GPH1(6) = PH1SHR(6)
  280 CALL GMMATS (GPH1,2,3,0, E1,3,6,0, PH3)
      DO 290 I = 1,3
      DO 290 J = 1,6
      I1 = (I-1)*6 + J
      I2 = I1 + 18
      J1 = (II-1)*30 + (JJ-1)*180 + I1 + 21
      J2 = J1 + 18
      PH1OUT(J1) = PH2(I1)
      IF (I .NE. 3) PH1OUT(J2) = PH3(I1)
  290 CONTINUE
C
  300 IF (MATID1 .EQ. 0) GO TO 330
      DO 310 I = 1,3
      DO 310 J = 1,2
      JI = (I-1)*5 + J
      IJ = (J-1)*3 + I + (II-1)*6
      TM(JI) = TMMM(IJ)
  310 CONTINUE
      DO 320 I = 1,3
      DO 320 J = 1,3
      JI = (I-1)*5  + J + 2
      IJ = (I-1)*18 + J + (II-1)*3
      TM(JI) = TMBQ(IJ)
  320 CONTINUE
      IF (ICS(II) .NE. 0) CALL GMMATS (EE,6,5,+1, BALOTR,6,6,0, EE1)
      IJ1 = (JJ-1)*108 + (II-1)*18 + 762
      CALL  GMMATS (EMOD,3,3,0, TM(1),3,5,0, EPH1)
      IF (ICS(II) .EQ. 0) CALL GMMATS (EPH1,3,5,0,EE,6,5,+1,PH1OUT(IJ1))
      IF (ICS(II) .NE. 0) CALL GMMATS (EPH1,3,5,0,EE1,5,6,0,PH1OUT(IJ1))
  330 CONTINUE
  340 CONTINUE
C
      JST = 742 + (JJ-1)*3
      IF (MATID2 .NE. 0) CALL GMMATS (D,3,3,0,ALF(1),3,1,0,PH1OUT(JST))
      IF (MATID1 .NE. 0) CALL GMMATS (EMOD,3,3,0,ALF(1),3,1,0,
     1    PH1OUT(1194))
      IF (MATID1 .EQ. 0) GO TO 360
      DO 350 I = 1,7
  350 NPH1OU(753+I) = IEST(I)
      GO TO 380
  360 DO 370 I = 1,7
      NPH1OU(753+I) = 0
  370 CONTINUE
  380 PH1OUT(761) = TREF
      RETURN
      END
