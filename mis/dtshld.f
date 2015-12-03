      SUBROUTINE DTSHLD
C
C     ECPT ENTRIES
C
C     ECPT( 1) = ELEMENT ID                                    INTEGER
C     ECPT( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1          INTEGER
C     ECPT( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2          INTEGER
C     ECPT( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3          INTEGER
C     ECPT( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4          INTEGER
C     ECPT( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5          INTEGER
C     ECPT( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6          INTEGER
C     ECPT( 8) = THETA                                         REAL
C     ECPT( 9) = MATERIAL  ID 1                                INTEGER
C     ECPT(10) = THICKNESS T1 AT GRID POINT G1
C     ECPT(11) = THICKNESS T3 AT GRID POINT G3
C     ECPT(12) = THICKNESS T5 AT GRID POINT G5
C     ECPT(13) = MATERIAL  ID 2                                INTEGER
C     ECPT(14) = THICKNESS TBEND1 FOR BENDING AT GRID POINT G1
C     ECPT(15) = THICKNESS TBEND3 FOR BENDING AT GRID POINT G3
C     ECPT(16) = THICKNESS TBEND5 FOR BENDING AT GRID POINT G5
C     ECPT(17) = MATERIAL  ID 3                                INTEGER
C     ECPT(18) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID POINT G1
C     ECPT(19) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID POINT G3
C     ECPT(20) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID POINT G5
C     ECPT(21) = NON-STRUCTURAL MASS                           REAL
C     ECPT(22) = DISTANCE Z11 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT(23) = DISTANCE Z21 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT(24) = DISTANCE Z13 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT(25) = DISTANCE Z23 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT(26) = DISTANCE Z15 FOR STRESS CALCULATION  AT GRID POINT G5
C     ECPT(27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
C
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE  IN NASTRAN BASIC SYSTEM
C
C     ECPT(28) = COORDINATE SYSTEM ID FOR GRID A               INTEGER
C     ECPT(29) = COORDINATE X1                                 REAL
C     ECPT(30) = COORDINATE Y1                                 REAL
C     ECPT(31) = COORDINATE Z1                                 REAL
C     ECPT(32) = COORDINATE SYSTEM ID FOR GRID B               INTEGER
C     ECPT(33) = COORDINATE X1                                 REAL
C     ECPT(34) = COORDINATE Y1                                 REAL
C     ECPT(35) = COORDINATE Z1                                 REAL
C     ECPT(36) = COORDINATE SYSTEM ID FOR GRID C               INTEGER
C     ECPT(37) = COORDINATE X1                                 REAL
C     ECPT(38) = COORDINATE Y1                                 REAL
C     ECPT(39) = COORDINATE Z1                                 REAL
C     ECPT(40) = COORDINATE SYSTEM ID FOR GRID D               INTEGER
C     ECPT(41) = COORDINATE X1                                 REAL
C     ECPT(42) = COORDINATE Y1                                 REAL
C     ECPT(43) = COORDINATE Z1                                 REAL
C     ECPT(44) = COORDINATE SYSTEM ID FOR GRID E               INTEGER
C     ECPT(45) = COORDINATE X1                                 REAL
C     ECPT(46) = COORDINATE Y1                                 REAL
C     ECPT(47) = COORDINATE Z1                                 REAL
C     ECPT(48) = COORDINATE SYSTEM ID FOR GRID F               INTEGER
C     ECPT(49) = COORDINATE X1                                 REAL
C     ECPT(50) = COORDINATE Y1                                 REAL
C     ECPT(51) = COORDINATE Z1                                 REAL
C     EST (52) = ELEMENT  TEMPERATURE
C     EST (53) = ENFORCED ELEMENT DEFORMATION (NOT USED)
C     EST (54) = LOADING  TEMPERATURE
C     EST (55) TO EST (90) = GLOBAL DISPLACEMENT VECTOR
C                REPLACES ECPT(65) TO ECPT(100) DESCRIBED BELOW
C     ECPT(65) = U1-DISP FOR X1
C     ECPT(66) = V1-DISP FOR Y1
C     ECPT(67) = W1-DISP FOR Z1
C     ECPT(68) = ALFA1-ROTATION FOR X1
C     ECPT(69) = BETA1-ROTATION FOR Y1
C     ECPT(70) = GAMA1-ROTATION FOR Z1
C     ECPT(71) = U2-DISP FOR X2
C     ECPT(72) = V2-DISP FOR Y2
C     ECPT(73) = W2-DISP FOR Z2
C     ECPT(74) = ALFA2-ROTATION FOR X2
C     ECPT(75) = BETA2-ROTATION FOR Y2
C     ECPT(76) = GAMA2-ROTATION FOR Z2
C     ECPT(77) = U3-DISP FOR X3
C     ECPT(78) = V3-DISP FOR Y3
C     ECPT(79) = W3-DISP FOR Z3
C     ECPT(80) = ALFA3-ROTATION FOR X3
C     ECPT(81) = BETA3-ROTATION FOR Y3
C     ECPT(82) = GAMA3-ROTATION FOR Z3
C     ECPT(83) = U4-DISP FOR X4
C     ECPT(84) = V4-DISP FOR Y4
C     ECPT(85) = W4-DISP FOR Z4
C     ECPT(86) = ALFA4-ROTATION FOR X4
C     ECPT(87) = BETA4-ROTATION FOR Y4
C     ECPT(88) = GAMA4-ROTATION FOR Z4
C     ECPT(89) = U5-DISP FOR X5
C     ECPT(90) = V5-DISP FOR Y5
C     ECPT(91) = W5-DISP FOR Z5
C     ECPT(92) = ALFA5-ROTATION FOR X5
C     ECPT(93) = BETA5-ROTATION FOR Y5
C     ECPT(94) = GAMA5-ROTATION FOR Z5
C     ECPT(95) = U6-DISP FOR X6
C     ECPT(96) = V6-DISP FOR Y6
C     ECPT(97) = W6-DISP FOR Z6
C     ECPT(98) = ALFA6-ROTATION FOR X6
C     ECPT(99) = BETA6-ROTATION FOR Y6
C     ECPT(100)= GAMA6-ROTATION FOR Z6
C
C     RK AND SK ARE EXPONENTS IN THICKNESS VARIATION
C
      LOGICAL          NOTS,UNIMEM,UNIBEN,NOGO
      INTEGER          RK(3),SK(3),RL(3),SL(3),XU(32),YU(32),XV(32),
     1                 YV(32),XW(32),YW(32),SIL(6),SIL1,SIL2,
     2                 RR,RR0,RR1,SS,SS0,SS1
      REAL             J11,J12,J22,NSM,IVECT(3),JVECT(3),KVECT(3),XC(6),
     1                 YC(6),ZC(6),F(18,18)
CWKBI 9/93
      DOUBLE PRECISION DETERM
      DOUBLE PRECISION TRAND(9),BALOTR(36),KSUB( 36),KSUBT( 36)
      DOUBLE PRECISION D334,D132,D232,RMX,RNX,RMNX,RMX1,RNX1,RMY,RNY,
     1                 RMNY,RMY1,RNY1,X,Y,QQQ(20,20),CMT(1296),
     2                 CTM(36,36),CMS(900),CM1(30,30),CAB(3),CSUB(5,5),
     3                 CSUBT(6,5),C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,
     4                 H4,H5,H6,SB1,SB2,SB3,SB4,SB5,SB6,SB7,SB8,SB9,
     5                 RIX,RIY,RJX,RJY,RKX,RKY,RLX,RLY,EE(30),Q(6,6),
     6                 QQQINV(360),QKS(960),KSHL(1024),MSHL(1024)
      DOUBLE PRECISION SB10,SB11,SB12,SB13,SB14,SB15,SB16,SB17,SB18,SB19
     1,                SB20,SB21,SB22,SB23,SB24,SB25,SB26,SB27,SB28,SB29
     2,                SB30,SB31,SB32,SB33,SB34,SB35,SB36,SB37,SB38,SB39
     3,                SB40,CC(10),ST
      DIMENSION        IND(6,3),EL(3),FL(3),GL(3),NAME(2),INDEX(20,3),
     1                 ICS(6),IEST(100),NL(6),SIGX(3),SIGY(3),SIGXY(3),
     2                 ES(6),STRESS(3),STR(3),VEC(3),PH1OUT(250),
     3                 TM(3,12),EMOD(9),TMMM(36),TRANS(9),EPH1(6),
     4                 EE1(6),NSIL(6),TI(1)
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG /  UFM,UWM,UIM,SFM
      COMMON /SYSTEM/  IBUF,IOUTPT
      COMMON /DS1AET/  EST(100)
      COMMON /DS1AAA/  NPVT,ICSTM,NCSTM
      COMMON /DS1ADP/  F
      COMMON /MATIN /  MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/  EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                 RJ11,RJ12,RJ22
      EQUIVALENCE      (C1,CC(1)),(C2,CC(2)),(C3,CC(3)),(C4,CC(4)),
     1                 (C5,CC(5)),(C6,CC(6)),(C7,CC(7)),(C8,CC(8)),
     2                 (C9,CC(9)),(C10,CC(10)),
     3                 (NSIL(1),PH1OUT(2)),(TM(1,1),TMMM(1)),(A,DISTA),
     4                 (B,DISTB),(C,DISTC),(IEST(1),EST(1)),
     5                 (CM1(1,1),CMS(1)),(THK1,TBEND1),(THK2,TBEND3),
     6                 (THK3,TBEND5),(CMT(1025),QQQINV(1)),
     7                 (CTM(1,1),CMT(1),KSHL(1),MSHL(1),QQQ(1,1)),
     8                 (CMT(437),PH1OUT(1)),(CMT(687),INDEX(1,1)),
     9                 (CMT(747),IND(1,1)),(TI(1),EST(65))
      DATA    RK    /  0,1,0 /, RL / 0,1,0 /, SK / 0,0,1 /, SL / 0,0,1/,
     1        XU    /  0,1,0,2,1,0,26*0    /, YU / 0,0,1,0,1,2,26*0   /,
     2        XV    /  6*0,0,1,0,2,1,0,20*0/, YV /6*0,0,0,1,0,1,2,20*0/,
     3        XW    /  12*0,0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,3,2,1,0   /,
     4        YW    /  12*0,0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,0,2,3,4,5   /,
     5        BLANK ,  NAME / 4H    ,4HDTSH,4HLD    /,
     6        DEGRA /  0.0174532925 /
C
      NOTS   =.FALSE.
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
      TBEND1 = (EST(14)*12.0)**0.333333333333
      TBEND3 = (EST(15)*12.0)**0.333333333333
      TBEND5 = (EST(16)*12.0)**0.333333333333
      MATID3 = IEST(17)
      TSHR1  = EST(18)
      TSHR3  = EST(19)
      TSHR5  = EST(20)
      NSM    = EST(21)
      J      = 0
      DO 20 I = 28,48,4
      J      = J + 1
      ICS(J) = IEST(I)
      XC(J)  = EST(I+1)
      YC(J)  = EST(I+2)
      ZC(J)  = EST(I+3)
   20 CONTINUE
C
C     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK, THEY WILL BE
C     SET EQUAL TO TMEM1 SO ALSO FOR TSHR3,TSHR5,TBEND3 AND TBEND5
C
      IF (TMEM3.EQ.0.0 .OR. TMEM3.EQ.BLANK) TMEM3 = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5.EQ.BLANK) TMEM5 = TMEM1
      IF (TSHR3.EQ.0.0 .OR. TSHR3.EQ.BLANK) TSHR3 = TSHR1
      IF (TSHR5.EQ.0.0 .OR. TSHR5.EQ.BLANK) TSHR5 = TSHR1
      TSHR   = (TSHR1+TSHR3+TSHR5)/3.0
      IF (TSHR1 .EQ. 0.0) NOTS =.TRUE.
      IF (TBEND3.EQ.0.0 .OR. TBEND3.EQ.BLANK) TBEND3 = TBEND1
      IF (TBEND5.EQ.0.0 .OR. TBEND5.EQ.BLANK) TBEND5 = TBEND1
      ELTEMP = EST(52)
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     EVALUTE MATERIAL PROPERTIES
C
      MATFLG = 2
      MATID  = MATID1
      IF (MATID1 .LE. 0) GO TO 670
      CALL MAT (IDELE)
C
      MATFLG = 2
      MATID  = MATID2
      CALL MAT (IDELE)
      D13    = EM(3)
      D23    = EM(5)
      D33    = EM(6)
      J11    = 0.0
      J12    = 0.0
      J22    = 0.0
      IF (NOTS) GO TO 30
      MATFLG = 3
      MATID  = MATID3
      CALL MAT (IDELE)
      J11    = 1.0/(RJ11*TSHR)
      J12    = 0.0
      J22    = 1.0/(RJ22*TSHR)
   30 CONTINUE
C
C     CALCULATIONS FOR THE TRIANGLE
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
C
      DO 40 I = 1,20
      DO 40 J = 1,20
   40 QQQ(I,J) = 0.0D0
      DO 50 I = 1,6
      I1 = (I-1)*3 + 1
      I2 = (I-1)*3 + 2
      I3 = (I-1)*3 + 3
      QQQ(I1, 1) = 1.0D0
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
      QQQ(I2, 3) = 1.0D0
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
      QQQ(I3, 2) =-1.0D0
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
   50 CONTINUE
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
      DO 60 I = 1,6
      I1 = (I-1)*3 + 1
      DO 60 J = 1,6
      Q(I,J) = QQQ(I1,J)
   60 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (6,Q,6,QQQINV(1),0,DETERM,ISING,IND)
      IF (ISING .EQ. 2) GO TO 660
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
C     IS U
C
C     AGAIN RESET ISING TO -1
C
      ISING = -1
      CALL INVERD (20,QQQ,20,QQQINV(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
      IF (ISING .EQ. 2) GO TO 660
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MA CALCULATIONS
C
      DO 70 I = 1,20
      DO 70 J = 1,18
      IJ = (I-1)*18 + J
      QQQINV(IJ) = QQQ(I,J)
   70 CONTINUE
C
C     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
C
C     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
C
C     OBTAIN MEMBRANE STRESSES
C
C     RELEVANT PORTION OF STRESS ROUTINE OF TRIM6 IS CODED HERE
C
C     TRANSFORMATION MATRIX BETWEEN ELEMENT AND BASIC COORDINATES
C
      ES(1) = IVECT(1)
      ES(2) = JVECT(1)
      ES(3) = IVECT(2)
      ES(4) = JVECT(2)
      ES(5) = IVECT(3)
      ES(6) = JVECT(3)
      DO 90 I = 1,9
      BALOTR(I) = 0.0
   90 CONTINUE
C
      DO 100 I = 1,7
      PH1OUT(I) = EST(I)
  100 CONTINUE
      PH1OUT( 8) = EST(10)
      PH1OUT( 9) = EST(11)
      PH1OUT(10) = EST(12)
      PH1OUT(11) = TREF
      EMOD(1) = EM(1)
      EMOD(2) = EM(2)
      EMOD(3) = EM(3)
      EMOD(4) = EM(2)
      EMOD(5) = EM(4)
      EMOD(6) = EM(5)
      EMOD(7) = EM(3)
      EMOD(8) = EM(5)
      EMOD(9) = EM(6)
C
      CALL GMMATS (EMOD,3,3,0,ALF(1),3,1,0,PH1OUT(228))
      DO 210 JJ = 1,3
      J = 2*JJ - 1
      X = XC(J)
      Y = YC(J)
      DO 110 I = 1,36
      TMMM(I) = 0.0
  110 CONTINUE
C
C     TM MATRIX IS THE PRODUCT OF B AND QINVERSE MATRICES
C
      DO 120 J = 1,6
      J1 = (J-1)*2 + 1
      J2 = J1 + 1
      TM(1,J1) = Q(2,J) + 2.0*X*Q(4,J) + Y*Q(5,J)
      TM(2,J2) = Q(3,J) + X*Q(5,J) + 2.0*Y*Q(6,J)
      TM(3,J1) = TM(2,J2)
      TM(3,J2) = TM(1,J1)
  120 CONTINUE
C
C     ZERO STRESS VECTOR STORAGE
C
      DO 130 I = 1,3
      STRESS(I) = 0.0
  130 CONTINUE
C
      DO 180 II = 1,6
      IJ1 = (JJ-1)*54 + (II-1)*9 + 12
      IF (ICS(II) .EQ. 0) GO TO 140
      CALL TRANSS (IEST(4*II+24),TRANS)
      CALL GMMATS (ES,3,2,+1,TRANS,3,3,0,EE1)
      GO TO 160
  140 CONTINUE
      DO 150 I = 1,3
      DO 150 J = 1,2
      I1 = (I-1)*2 + J
      J1 = (J-1)*3 + I
      EE1(J1) = ES(I1)
  150 CONTINUE
  160 CONTINUE
      MZ = (II-1)*6 + 1
      CALL GMMATS (EMOD,3,3,0,TMMM(MZ),2,3,+1,EPH1)
      CALL GMMATS (EPH1,3,2,0,EE1,2,3,0,PH1OUT(IJ1))
C
C     POINTER TO I-TH SIL IN PH1OUT
C
      NPOINT = 55 + (II-1)*6
C
C     POINTER TO  3X3 S SUB I MATRIX
C
      NPT1 = 12 + (II-1)*9 + (JJ-1)*54
C
      CALL GMMATS (PH1OUT(NPT1),3,3,0,EST(NPOINT),3,1,0,VEC(1))
      DO 170 J = 1,3
      STRESS(J) = STRESS(J) + VEC(J)
      STR(J) = STRESS(J)
  170 CONTINUE
  180 CONTINUE
      IF (IEST(54) .EQ. -1) GO TO 200
      TEM = EST(54) - PH1OUT(11)
      DO 190 I = 1,3
      STRESS(I) = STRESS(I) - PH1OUT(227+I)*TEM
      STR(I) = STRESS(I)
  190 CONTINUE
  200 CONTINUE
      SIGX(JJ)  = STRESS(1)
      SIGY(JJ)  = STRESS(2)
      SIGXY(JJ) = STRESS(3)
  210 CONTINUE
C
C     EL, FL, GL ARE COEFFICIENTS IN LINEAR VARIATION OF SIGX, SIGY,
C     SIGXY RESPECTIVELY OVER THE ELEMENT
C
C
      EL(1) = (SIGX(1)*A + SIGX(2)*B)/(A+B)
      EL(2) = (SIGX(2) - SIGX(1))/(A+B)
      EL(3) = (SIGX(3) - EL(1))/C
      FL(1) = (SIGY(1)*A + SIGY(2)*B)/(A+B)
      FL(2) = (SIGY(2) - SIGY(1))/(A+B)
      FL(3) = (SIGY(3) - FL(1))/C
      GL(1) = (SIGXY(1)*A + SIGXY(2)*B)/(A+B)
      GL(2) = (SIGXY(2) - SIGXY(1))/(A+B)
      GL(3) = (SIGXY(3) - GL(1))/C
C
C     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
C     THICKNESS VARIATION
C
      CALL AF (F,18,A,B,C,CAB1,CAB2,CAB3,TMEM1,TMEM3,TMEM5,0)
      CAB(1) = CAB1
      CAB(2) = CAB2
      CAB(3) = CAB3
      UNIMEM =.FALSE.
      UNIBEN =.FALSE.
C
      D334 = D33*4.0D0
      D132 = D13*2.0D0
      D232 = D23*2.0D0
C
C     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
C     OF BENDING THICKNESSES
C
      CALL AF (F,18,A,B,C,A1,A2,A3,THK1,THK2,THK3,0)
      IF (ABS(CAB2).LE.1.E-6 .AND. ABS(CAB3).LE.1.E-6) UNIMEM =.TRUE.
      IF (ABS(A2).LE.1.0E-06 .AND. ABS(A3).LE.1.0E-06) UNIBEN =.TRUE.
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
C     CALL AF (F,18,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,0)
      H4   = Q(4,1)*ZC(1) + Q(4,2)*ZC(2) + Q(4,3)*ZC(3) + Q(4,4)*ZC(4) +
     1       Q(4,5)*ZC(5) + Q(4,6)*ZC(6)
      H5   = Q(5,1)*ZC(1) + Q(5,2)*ZC(2) + Q(5,3)*ZC(3) + Q(5,4)*ZC(4) +
     1       Q(5,5)*ZC(5) + Q(5,6)*ZC(6)
      H6   = Q(6,1)*ZC(1) + Q(6,2)*ZC(2) + Q(6,3)*ZC(3) + Q(6,4)*ZC(4) +
     1       Q(6,5)*ZC(5) + Q(6,6)*ZC(6)
      H4   = H4*2.0D0
      H6   = H6*2.0D0
C
C     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY*W
C
      H5   = H5*2.0D0
C
      DO 260 I = 1,32
      IX   = XU(I)
      RIX  = IX
      JX   = YU(I)
      RJX  = JX
      KX   = XV(I)
      RKX  = KX
      LX   = YV(I)
      RLX  = LX
      MX   = XW(I)
      RMX  = MX
      NX   = YW(I)
      RNX  = NX
      RMNX = RMX*RNX
      RMX1 = RMX*(RMX-1.0D0)
      RNX1 = RNX*(RNX-1.0D0)
C
      DO 250 J = I,32
      IJ   = (I-1)*32 + J
      JI   = (J-1)*32 + I
      IY   = XU(J)
      RIY  = IY
      JY   = YU(J)
      RJY  = JY
      KY   = XV(J)
      RKY  = KY
      LY   = YV(J)
      RLY  = LY
      MY   = XW(J)
      RMY  = MY
      NY   = YW(J)
      RNY  = NY
      RMNY = RMY*RNY
      RMY1 = RMY*(RMY-1.0)
      RNY1 = RNY*(RNY-1.0)
      ST   = 0.0D0
      DO 230 K = 1,3
      DO 220 L = 1,3
      RR    = RK(K) + RL(L)
      RR0   = RK(K) + RL(L) - 1
      RR1   = RK(K) + RL(L) + 1
      SS    = SK(K) + SL(L)
      SS0   = SK(K) + SL(L) - 1
      SS1   = SK(K) + SL(L) + 1
      MM    = MX + MY
      MMRR0 = MM + RR0
      MMRR1 = MM + RR1
      NN    = NX + NY
      NNSS1 = NN + SS1
      NNSS0 = NN + SS0
      MMRR  = MM + RR
      NNSS  = NN + SS
      KK    = KX + KY
      KKRR0 = KK + RR0
      LL    = LX + LY
      LLSS1 = LL + SS1
      II    = IX + IY
      JJ    = JX + JY
      IIRR1 = II + RR1
      JJSS0 = JJ + SS0
      KI    = KX + IY
      KIRR  = KI + RR
      LJ    = LX + JY
      LJSS  = LJ + SS
      IK    = IX + KY
      IKRR  = IK + RR
      JL    = JX + LY
      JLSS  = JL + SS
      KM    = KX + MY
      KMRR  = KM + RR
      LN    = LX + NY
      LNSS1 = LN + SS1
      IM    = IX + MY
      IMRR1 = IM + RR1
      JN    = JX + NY
      JNSS  = JN + SS
      KKRR  = KK + RR
      LLSS  = LL + SS
      KIRR1 = KI + RR1
      LJSS0 = LJ + SS0
      MK    = MX + KY
      MKRR  = MK + RR
      NLSS1 = NX + LY + SS1
      MI    = MX + IY
      MIRR1 = MI + RR1
      NJ    = NX + JY
      NJSS  = NJ + SS
      IKRR0 = IK + RR0
      JLSS1 = JL + SS1
      IIRR  = II + RR
      JJSS  = JJ + SS
      IKRR1 = IK + RR1
      JLSS0 = JL + SS0
      LNSS1 = LN + SS1
      KIRR0 = KI + RR0
      LJSS1 = LJ + SS1
      SB1   = 0.0D0
      SB2   = 0.0D0
      SB3   = 0.0D0
      SB4   = 0.0D0
      SB5   = 0.0D0
      SB6   = 0.0D0
      SB7   = 0.0D0
      SB8   = 0.0D0
      SB9   = 0.0D0
      SB10  = 0.0D0
      SB11  = 0.0D0
      SB12  = 0.0D0
      SB13  = 0.0D0
      SB14  = 0.0D0
      SB15  = 0.0D0
      SB16  = 0.0D0
      SB17  = 0.0D0
      SB18  = 0.0D0
      SB19  = 0.0D0
      SB20  = 0.0D0
      SB21  = 0.0D0
      SB22  = 0.0D0
      SB23  = 0.0D0
      SB24  = 0.0D0
      SB25  = 0.0D0
      SB26  = 0.0D0
      SB27  = 0.0D0
      SB28  = 0.0D0
      SB29  = 0.0D0
      SB30  = 0.0D0
      SB31  = 0.0D0
      SB32  = 0.0D0
      SB33  = 0.0D0
      SB34  = 0.0D0
      SB35  = 0.0D0
      SB36  = 0.0D0
      SB37  = 0.0D0
      SB38  = 0.0D0
      SB39  = 0.0D0
      SB40  = 0.0D0
      IF (MMRR0 .GT. 0) SB1 = CAB(K)*EL(L)*RMX*RMY*F(MMRR0,NNSS1)
      IF (NNSS0 .GT. 0) SB2 = CAB(K)*FL(L)*RNX*RNY*F(MMRR1,NNSS0)
      IF (MMRR.GT.0 .AND. NNSS.GT.0) SB3 = CAB(K)*GL(L)*RNX*RMY*
     1                                     F(MMRR,NNSS)
      IF (MMRR.GT.0 .AND. NNSS.GT.0) SB4 = CAB(K)*GL(L)*RMX*RNY*
     1                                     F(MMRR,NNSS)
      IF (KKRR0 .GT. 0) SB5 = CAB(K)*EL(L)*RKX*RKY*F(KKRR0,LLSS1)
      IF (JJSS0 .GT. 0) SB6 = CAB(K)*EL(L)*RJX*RJY*F(IIRR1,JJSS0)
      IF (KIRR.GT.0 .AND. LJSS.GT.0) SB7 = CAB(K)*EL(L)*RKX*RJY*
     1                                     F(KIRR,LJSS)
      IF (IKRR.GT.0 .AND. JLSS.GT.0) SB8 = CAB(K)*EL(L)*RJX*RKY*
     1                                     F(IKRR,JLSS)
      IF (KIRR.GT.0 .AND. LJSS.GT.0) SB9 = CAB(K)*EL(L)*RKX*RJY*
     1                                     F(KIRR,LJSS)
      IF (KKRR0 .GT. 0) SB10 = CAB(K)*EL(L)*RKX*RKY*F(KKRR0,LLSS1)
      IF (KMRR  .GT. 0) SB11 = CAB(K)*EL(L)*RKX*H5*F(KMRR,LNSS1)
      IF (JJSS0 .GT. 0) SB12 = CAB(K)*EL(L)*RJX*RJY*F(IIRR1,JJSS0)
      IF (IKRR.GT.0 .AND. JLSS.GT.0) SB13 = CAB(K)*EL(L)*RJX*RKY*
     1                                      F(IKRR,JLSS)
      IF (JNSS  .GT. 0) SB14 = CAB(K)*EL(L)*RJX*H5*F(IMRR1,JNSS)
      IF (KKRR0 .GT. 0) SB15 = CAB(K)*FL(L)*RKX*RKY*F(KKRR0,LLSS1)
      IF (KIRR.GT.0 .AND. LJSS.GT.0) SB16 = CAB(K)*FL(L)*RKX*RJY*
     1                                      F(KIRR,LJSS)
      IF (JJSS0 .GT. 0) SB17 = CAB(K)*FL(L)*RJX*RJY*F(IIRR1,JJSS0)
      IF (IKRR.GT.0 .AND. JLSS.GT.0) SB18 = CAB(K)*FL(L)*RJX*RKY*
     1                                      F(IKRR,JLSS)
      IF (KIRR.GT.0 .AND. LJSS.GT.0) SB19 = CAB(K)*FL(L)*RKX*RJY*
     1                                      F(KIRR,LJSS)
      IF (KKRR0 .GT. 0) SB20 = CAB(K)*FL(L)*RKX*RKY*F(KKRR0,LLSS1)
      IF (KMRR  .GT. 0) SB21 = CAB(K)*FL(L)*RKX*H5*F(KMRR,LNSS1)
      IF (JJSS0 .GT. 0) SB22 = CAB(K)*FL(L)*RJX*RJY*F(IIRR1,JJSS0)
      IF (IKRR.GT.0 .AND. JLSS.GT.0) SB23 = CAB(K)*FL(L)*RJX*RKY*
     1                                      F(IKRR,JLSS)
      IF (JNSS  .GT. 0) SB24 = CAB(K)*FL(L)*RJX*H5*F(IMRR1,JNSS)
      IF (KKRR.GT.0 .AND. LLSS.GT.0) SB25 = CAB(K)*GL(L)*RLX*RKY*
     1                                      F(KKRR,LLSS)
      IF (KKRR.GT.0 .AND. LLSS.GT.0) SB26 = CAB(K)*GL(L)*RKX*RLY*
     1                                      F(KKRR,LLSS)
      IF (LJSS0 .GT. 0) SB27 = CAB(K)*GL(L)*RLX*RJY*F(KIRR1,LJSS0)
      IF (JLSS0 .GT. 0) SB28 = CAB(K)*GL(L)*RJX*RLY*F(IKRR1,JLSS0)
      IF (MKRR  .GT. 0) SB29 = CAB(K)*GL(L)*RKY*H6*F(MKRR,NLSS1)
      IF (KMRR  .GT. 0) SB30 = CAB(K)*GL(L)*RKX*H6*F(KMRR,LNSS1)
      IF (NJSS  .GT. 0) SB31 = CAB(K)*GL(L)*RJY*H6*F(MIRR1,NJSS)
      IF (JNSS  .GT. 0) SB32 = CAB(K)*GL(L)*RJX*H6*F(IMRR1,JNSS)
      IF (IKRR0 .GT. 0) SB33 = CAB(K)*GL(L)*RIX*RKY*F(IKRR0,JLSS1)
      IF (KIRR0 .GT. 0) SB34 = CAB(K)*GL(L)*RKX*RIY*F(KIRR0,LJSS1)
      IF (IIRR.GT.0 .AND. JJSS.GT.0) SB35 = CAB(K)*GL(L)*RIX*RJY*
     1                                      F(IIRR,JJSS)
      IF (IIRR.GT.0 .AND. JJSS.GT.0) SB36 = CAB(K)*GL(L)*RJX*RIY*
     1                                      F(IIRR,JJSS)
      IF (MKRR  .GT. 0) SB37 = CAB(K)*GL(L)*RKY*H4*F(MKRR,NLSS1)
      IF (KMRR  .GT. 0) SB38 = CAB(K)*GL(L)*RKX*H4*F(KMRR,LNSS1)
      IF (NJSS  .GT. 0) SB39 = CAB(K)*GL(L)*RJY*H4*F(MIRR1,NJSS)
      IF (JNSS  .GT. 0) SB40 = CAB(K)*GL(L)*RJX*H4*F(IMRR1,JNSS)
      ST = ST + SB1 + SB2 + SB3 + SB4 +
     1     0.25*(SB5+SB6-SB7-SB8) + (SB9+SB10-SB11-SB12-SB13+SB14) +
     2     0.25*(SB15-SB16+SB17-SB18) + (-SB19-SB20+SB21+SB22+SB23-SB24)
     3   + 0.5*(SB25+SB26-SB27-SB28-SB29-SB30+SB31+SB32) +
     4     0.5*(-SB33-SB34+SB35+SB36+SB37+SB38-SB39-SB40)
  220 CONTINUE
      IF (UNIMEM) GO TO 240
  230 CONTINUE
  240 CONTINUE
      KSHL(IJ) = ST
      KSHL(JI) = KSHL(IJ)
  250 CONTINUE
  260 CONTINUE
C
C     IF NO TRANSVERSE SHEAR GO TO 230
C
C     IF TSHR EQUAL TO ZERO OR MATID3 EQUAL TO ZERO , SKIP THESE
C     CALCULATION
C
      IF (NOTS) GO TO 270
C
C     CURRENTLY, TRANSVERSE SHEAR CALCULATIONS ARE NOT CODED FOR SHELL
C     ELEMENT WHEN IT IS CODED, CALL THE ROUTINE HERE
C
  270 CONTINUE
      CALL GMMATD (Q,6,6,0,KSHL(1),6,32,0,QKS(1))
      CALL GMMATD (Q,6,6,0,KSHL(193),6,32,0,QKS(193))
      CALL GMMATD (QQQINV,20,18,+1,KSHL(385),20,32,0,QKS(385))
      DO 290 I = 1,30
      DO 280 J = 1,6
      IJ = (I-1)*32 + J
      JI = (I-1)*6  + J
      KSHL(    JI) = QKS(  IJ)
      KSHL(180+JI) = QKS(6+IJ)
  280 CONTINUE
  290 CONTINUE
      DO 310 I = 1,30
      DO 300 J = 1,20
      IJ = (I-1)*32 + J + 12
      JI = (I-1)*20 + J + 360
      KSHL(JI) = QKS(IJ)
  300 CONTINUE
  310 CONTINUE
      CALL GMMATD (KSHL(  1),30,6 ,0,Q,6,6,1 ,QKS(  1))
      CALL GMMATD (KSHL(181),30,6 ,0,Q,6,6,1 ,QKS(181))
      CALL GMMATD (KSHL(361),30,20,0,QQQINV,20,18,0,QKS(361))
      DO 330 I = 1,30
      DO 320 J = 1,6
      IJ = (I-1)*30 + J
      JI = (I-1)*6  + J
      CMS(IJ  ) = QKS(JI    )
      CMS(IJ+6) = QKS(JI+180)
  320 CONTINUE
  330 CONTINUE
      DO 350 I = 1,30
      DO 340 J = 1,18
      IJ = (I-1)*30 + J + 12
      JI = (I-1)*18 + J + 360
      CMS(IJ) = QKS(JI)
  340 CONTINUE
  350 CONTINUE
      DO 360 I = 1,30
      EE(I) = 0.0D0
  360 CONTINUE
      EE( 1) = IVECT(1)
      EE( 2) = JVECT(1)
      EE( 3) = KVECT(1)
      EE( 6) = IVECT(2)
      EE( 7) = JVECT(2)
      EE( 8) = KVECT(2)
      EE(11) = IVECT(3)
      EE(12) = JVECT(3)
      EE(13) = KVECT(3)
      EE(19) = IVECT(1)
      EE(20) = JVECT(1)
      EE(24) = IVECT(2)
      EE(25) = JVECT(2)
      EE(29) = IVECT(3)
      EE(30) = JVECT(3)
      DO 390 K = 1,6
      DO 380 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 5*(K-1) + I
      DO 370 J = 1,30
      CTM (I1,J) = CM1(K1,J)
  370 CONTINUE
  380 CONTINUE
  390 CONTINUE
      DO 420 K = 1,6
      DO 410 I = 1,3
      I2 = 5*(K-1)  + I + 2
      K2 = 12 + (K-1)*3 + I
      DO 400 J = 1,30
      CTM (I2,J) = CM1(K2,J)
  400 CONTINUE
  410 CONTINUE
  420 CONTINUE
      DO 450 K = 1,6
      DO 440 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 5*(K-1) + I
      DO 430 J = 1,30
      CM1(J,I1) = CTM (J,K1)
  430 CONTINUE
  440 CONTINUE
  450 CONTINUE
      DO 480 K = 1,6
      DO 470 I = 1,3
      I2 = 5*(K-1)  + I + 2
      K2 = 12 + (K-1)*3 + I
      DO 460 J = 1,30
      CM1(J,I2) = CTM(J,K2)
  460 CONTINUE
  470 CONTINUE
  480 CONTINUE
C
C     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
C     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
C     ARE R
C     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
C
C     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
C     COORDINATE
C
C     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINATES TO GLOBAL (DISP)
C     COORDINATES
C
C     INSERT THE 6X6 SUBMATRIX  INTO KGG MATRIX
C
      DO 490 I = 1,1296
      CMT(I) = 0.0D0
  490 CONTINUE
      DO 500 I = 1,6
      SIL(I) = I
  500 CONTINUE
      DO 510 I = 1,6
      IF (NPVT .NE. IEST(I+1)) GO TO 510
      NPIVOT = I
      GO TO 520
  510 CONTINUE
      NOGO = .TRUE.
      WRITE (IOUTPT,720) SFM,IEST(1)
      RETURN
C
  520 CONTINUE
      I    = NPIVOT
      SIL1 = SIL(NPIVOT)
      DO 650 J = 1,6
      SIL2 = SIL(J)
      DO 530 II = 1,36
      BALOTR(II) = 0.0D0
      KSUB(II)   = 0.0D0
  530 CONTINUE
      DO 550 K = 1,5
      K1 = (SIL1-1)*5 + K
      DO 540 L = 1,5
      L1 = (SIL2-1)*5 + L
      CSUB(K,L) = CM1(K1,L1)
  540 CONTINUE
  550 CONTINUE
      CALL GMMATD (EE,6,5,0,CSUB,5,5,0,CSUBT)
      CALL GMMATD (CSUBT,6,5,0,EE,6,5,+1,KSUBT)
      DO 560 K = 1,6
      DO 560 L = 1,6
      K1 = (K-1)*6 + L
      L1 = (L-1)*6 + K
      KSUB(L1) = KSUBT(K1)
  560 CONTINUE
C
C     TRANSFORM THE KSUB(36) FROM BASIC TO DISPLACEMENT COORDINATES
C
      IF (NL(SIL1).EQ.0 .OR. ICS(SIL1).EQ.0) GO TO 590
      CALL TRANSD (IEST(4*SIL1+24),TRAND)
      DO 570 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  570 CONTINUE
      CALL GMMATD (BALOTR(1),6,6,1,KSUB(1),6,6,0,KSUBT)
      DO 580 K = 1,36
      KSUB(K) = KSUBT(K)
  580 CONTINUE
  590 CONTINUE
      IF (NL(SIL2).EQ.0 .OR. ICS(SIL2).EQ.0) GO TO 630
      IF (J .EQ. I) GO TO 610
      CALL TRANSD (IEST(4*SIL2+24),TRAND)
      DO 600 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  600 CONTINUE
  610 CONTINUE
      CALL GMMATD (KSUB(1),6,6,0,BALOTR(1),6,6,0,KSUBT)
      DO 620 K = 1,36
      KSUB(K) = KSUBT(K)
  620 CONTINUE
  630 CONTINUE
      CALL DS1B (KSUB(1),IEST(J+1))
  650 CONTINUE
      GO TO 730
  660 CONTINUE
      NOGO =.TRUE.
      WRITE (IOUTPT,700) UFM,IEST(1)
      RETURN
C
  670 CONTINUE
      NOGO =.TRUE.
      WRITE (IOUTPT,710) UFM,IEST(1)
      RETURN
C
  700 FORMAT (A23,' 2416, MATRIX RELATING GENERALIZED PARAMETERS AND ',
     1       'GRID POINT DISPLACEMENTS IS SINGULAR.', /26X,
     2       'CHECK COORDINATES OF ELEMENT  TRSHL WITH ID =',I9,1H.)
  710 FORMAT (A23,' 2418, MATERIAL ID FOR MEMBRANE EFFECTS IS LESS ',
     1       'THAN OR EQUAL TO ZERO FOR TRSHL ELEMENT WITH ID =',I9,1H.)
  720 FORMAT (A25,' 2419, PIVOT POINT IS NOT EQUAL TO TRSHL ELEMENT ',
     1       'GRID POINTS FOR ELEMENT ID =',I9,1H.)
  730 CONTINUE
      RETURN
      END
