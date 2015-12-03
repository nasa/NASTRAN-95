      SUBROUTINE KTSHLD
C
C     ECPT ENTRIES
C
C     ECPT( 1) = ELEMENT ID                                     INTEGER
C     ECPT( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1           INTEGER
C     ECPT( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2           INTEGER
C     ECPT( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3           INTEGER
C     ECPT( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4           INTEGER
C     ECPT( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5           INTEGER
C     ECPT( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6           INTEGER
C     ECPT( 8) = THETA                                          REAL
C     ECPT( 9) = MATERIAL ID 1                                  INTEGER
C     ECPT(10) = THICKNESS T1 AT GRID POINT G1
C     ECPT(11) = THICKNESS T3 AT GRID POINT G3
C     ECPT(12) = THICKNESS T5 AT GRID POINT G5
C     ECPT(13) = MATERIAL ID 2                                  INTEGER
C     ECPT(14) = THICKNESS TBEND1 FOR BENDING AT GRID POINT G1
C     ECPT(15) = THICKNESS TBEND3 FOR BENDING AT GRID POINT G3
C     ECPT(16) = THICKNESS TBEND5 FOR BENDING AT GRID POINT G5
C     ECPT(17) = MATERIAL ID 3                                  INTEGER
C     ECPT(18) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID POINT G1
C     ECPT(19) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID POINT G3
C     ECPT(20) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID POINT G5
C     ECPT(21) = NON-STRUCTURAL MASS                            REAL
C     ECPT(22) = DISTANCE Z11 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT(23) = DISTANCE Z21 FOR STRESS CALCULATION  AT GRID POINT G1
C     ECPT(24) = DISTANCE Z13 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT(25) = DISTANCE Z23 FOR STRESS CALCULATION  AT GRID POINT G3
C     ECPT(26) = DISTANCE Z15 FOR STRESS CALCULATION  AT GRID POINT G5
C     ECPT(27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
C
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE  IN NASTRAN BASIC SYSTEM
C
C     ECPT(28) = COORDINATE SYSTEM ID FOR GRID A                INTEGER
C     ECPT(29) = COORDINATE X1                                  REAL
C     ECPT(30) = COORDINATE Y1                                  REAL
C     ECPT(31) = COORDINATE Z1                                  REAL
C     ECPT(32) = COORDINATE SYSTEM ID FOR GRID B                INTEGER
C     ECPT(33) = COORDINATE X1                                  REAL
C     ECPT(34) = COORDINATE Y1                                  REAL
C     ECPT(35) = COORDINATE Z1                                  REAL
C     ECPT(36) = COORDINATE SYSTEM ID FOR GRID C                INTEGER
C     ECPT(37) = COORDINATE X1                                  REAL
C     ECPT(38) = COORDINATE Y1                                  REAL
C     ECPT(39) = COORDINATE Z1                                  REAL
C     ECPT(40) = COORDINATE SYSTEM ID FOR GRID D                INTEGER
C     ECPT(41) = COORDINATE X1                                  REAL
C     ECPT(42) = COORDINATE Y1                                  REAL
C     ECPT(43) = COORDINATE Z1                                  REAL
C     ECPT(44) = COORDINATE SYSTEM ID FOR GRID E                INTEGER
C     ECPT(45) = COORDINATE X1                                  REAL
C     ECPT(46) = COORDINATE Y1                                  REAL
C     ECPT(47) = COORDINATE Z1                                  REAL
C     ECPT(48) = COORDINATE SYSTEM ID FOR GRID F                INTEGER
C     ECPT(49) = COORDINATE X1                                  REAL
C     ECPT(50) = COORDINATE Y1                                  REAL
C     ECPT(51) = COORDINATE Z1                                  REAL
C     EST (52) = ELEMENT TEMPERATURE
C
      LOGICAL          IMASS,NOTS,NOGO,UNIMEM,UNIBEN
      INTEGER          XU(32),YU(32),XV(32),YV(32),XW(32),YW(32),
     1                 RK(3),SK(3),ELTYPE,ELID,ESTID,DICT(15),SIL(6),
     2                 SIL1,SIL2,SAVE(6),XTHK(10),YTHK(10),SMALL(6)
C
C     RK AND SK ARE EXPONENTS IN THICKNESS VARIATION
C
      REAL             J11,J12,J22,NSM,XC(6),YC(6),ZC(6),IVECT(3),
     1                 JVECT(3),KVECT(3),CC(10),NAME(2)
C
C     LOCAL DOUBLE PRECISION VARIABLES
C
      DOUBLE PRECISION S11,S22,S13,S23,D334,D132,D232,S33,MSHL(1024),
     1                 RMX,RNX,RMNX,RMX1,RNX1,RMY,RNY,RMNY,RMY1,RNY1,
     2                 CMT(1296),CTM(36,36),CMS(900),CM1(30,30),
     3                 QKS(960),CAB(3),H4,H5,H6,QQQ(20,20),
     4                 SB1,SB2,SB3,SB4,SB5,SB6,SB7,SB8,SB9,RIX,RIY,RJX,
     5                 RJY,RKX,RKY,RLX,RLY,G11,G22,KSHL(1024),KSUP(36),
     6                 KSUPT(36),QQQINV(360),Q,EE,CSUB,CSUBT
      DOUBLE PRECISION SB10,SB11,SB12,SB13,SB14,SB15,SB16,SB17,SB18,SB19
     1,                SB20,SB21,SB22,SB23,SB24,SB25,SB26,SB27,SB28,SB29
     2,                SB30,SB31,SB32,SB33,SB34,SB35,SB36,SB37,SB38,SB39
     3,                SB40,SB41,DEGRA,DETERM
      DOUBLE PRECISION TRAND(9),BALOTR(36),KSUB(6,6),KSUBT(6,6),
     1                 ST,D13,D22,D11,D12,D23,D33,
     2                 G13,G23,G33,G12,RHO,ST1,A2SQ,A3SQ,AREA,VOL,A1SQ,
     3                 ST11,ST22,ST121,ST122,ST131,ST132,ST133,ST231,
     4                 ST232,ST233,ST331,ST332
      DIMENSION        IND(6,3),INDEX(20,3),ICS(6),IEST(42),NL(6)
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /BLANK /  NOK,NOM,NOB
      COMMON /EMGEST/  EST(100)
      COMMON /EMGDIC/  ELTYPE,LDICT,NLOCS,ELID,ESTID
      COMMON /SMA1DP/  F(14,14),Q(6,6),EE(30),CSUBT(6,5),CSUB(5,5)
      COMMON /SMA2DP/  TRAND,BALOTR,KSUB,KSUBT,FAC,XC,YC,ZC,IVECT,JVECT,
     1                 KVECT,CC,CAB,DICT,SIL,SAVE,SMALL,INDEX,ICS,NL
      COMMON /SMA1CL/  KDUMMY(22), KNOGO
      COMMON /EMGPRM/  IXTRA,IZR,NZR,DUMY(12),KMBGG(3),IPREC,NOGO
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /MATIN /  MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/  EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                 RJ11,RJ12,RJ22
      EQUIVALENCE     (C1,CC(1)), (C2 ,CC(2)), (C3,CC(3)), (C4,CC(4)),
     1                (C5,CC(5)), (C6 ,CC(6)), (C7,CC(7)), (C8,CC(8)),
     2                (C9,CC(9)), (C10,CC(10))
      EQUIVALENCE     (A ,DISTA), (B  ,DISTB), (C ,DISTC),
     1                (CMT(1),CTM(1,1))      , (IEST(1),EST(1))
      EQUIVALENCE     (CMT(1),KSHL(1),MSHL(1), QQQ(1,1)),
     1                (KSUB(1,1),KSUP(1))    , (KSUBT(1,1),KSUPT(1)),
     2                (QKS(1),CMT(1025))
      EQUIVALENCE     (THK1,TBEND1), (THK2,TBEND3), (THK3,TBEND5),
     1                (CM1(1,1),CMS(1)), (KSYSTM(2),IOUTPT),
     2                (IND(1,1),INDEX(1,1))
      DATA    XU    / 0,1,0,2,1,0,26*0     /,
     1        YU    / 0,0,1,0,1,2,26*0     /,
     2        XV    / 6*0,0,1,0,2,1,0,20*0 /,
     3        YV    / 6*0,0,0,1,0,1,2,20*0 /,
     4        XW    / 12*0,0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,3,2,1,0/,
     5        YW    / 12*0,0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,0,2,3,4,5/
      DATA    BLANK , NAME  / 4H    , 4HTRSH, 4HL     /
      DATA    RK    / 0,1,0 /
      DATA    SK    / 0,0,1 /
      DATA    DEGRA / 0.0174532925D0       /
      DATA    XTHK  / 0,1,0,2,1,0,3,2,1,0  /
      DATA    YTHK  / 0,0,1,0,1,2,0,1,2,3  /
C
C
      DICT(1) = ESTID
C
C     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
C
      ICODE  = 63
      NDOF   = 36
      NSQ    = NDOF**2
      DICT(2)= 1
      DICT(3)= NDOF
      DICT(4)= ICODE
      DICT(5)= GSUBE
      NOTS   =.FALSE.
      IMASS  =.FALSE.
      IF (NOM .GT. 0) IMASS =.TRUE.
      IPASS  = 1
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
      DO 20 I = 28,48,4
      J      = J + 1
      ICS(J) = IEST(I )
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
      IF (TSHR1 .EQ. 0.0) NOTS =.TRUE.
      TSHR = (TSHR1+TSHR3+TSHR5)/3.0
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
      IF  (MATID1 .EQ. 0) GO TO 30
      CALL MAT (IDELE)
C
      G11 = EM(1)
      G12 = EM(2)
      G13 = EM(3)
      G22 = EM(4)
      G23 = EM(5)
      G33 = EM(6)
   30 CONTINUE
      MATFLG = 2
      MATID  = MATID2
      IF  (MATID2 .EQ. 0) GO TO 40
      CALL MAT (IDELE)
      D11 = EM(1)
      D12 = EM(2)
      D13 = EM(3)
      D22 = EM(4)
      D23 = EM(5)
      D33 = EM(6)
      J11 = 0.0
      J12 = 0.0
      J22 = 0.0
      IF (NOTS) GO TO 40
      MATFLG = 3
      MATID  = MATID3
      CALL MAT (IDELE)
      J11 = 1.0/(RJ11*TSHR)
      J12 = 0.0
      J22 = 1.0/(RJ22*TSHR)
   40 CONTINUE
C
C     CALCULATIONS FOR THE TRIANGLE
C
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     COMPUTE THE AREA INTEGRATION FUNCTION F
C
      CALL AF (F,14,A,B,C,0,0,0,0,0,0,-1)
C
C     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
C
      DO 50 I = 1,20
      DO 50 J = 1,20
   50 QQQ(I,J) = 0.0D0
      DO 60 I = 1,6
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
   60 CONTINUE
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
      DO 70 I = 1,6
      DO 70 J = 1,6
      I1 = (I-1)*3 + 1
      Q(I,J) = QQQ(I1,J)
   70 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (6,Q,6,BALOTR(1),0,DETERM,ISING,IND)
      IF (ISING .EQ. 2) GO TO 700
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
C     IS U
C
C     AGAIN RESET ISING TO -1
C
      ISING = -1
      CALL INVERD (20,QQQ,20,BALOTR(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
      IF (ISING .EQ. 2) GO TO 700
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      DO 80 I = 1,20
      DO 80 J = 1,18
      IJK = (I-1)*18 + J
      QQQINV(IJK) = QQQ(I,J)
   80 CONTINUE
C
C     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
C
C     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
C
   90 CONTINUE
C
C     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
C     THICKNESS VARIATION - MEMBRANE
C
      CALL AF (F,14,A,B,C,C1,C2,C3,TMEM1,TMEM3,TMEM5,1)
      CAB(1) = C1
      CAB(2) = C2
      CAB(3) = C3
      AREA   = F(1,1)
      VOL    = C1*F(1,1) + C2*F(2,1) + C3*F(1,2)
C
C
      D334 = D33*4.0D0
      D132 = D13*2.0D0
      D232 = D23*2.0D0
C
C     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
C     OF BENDING THICKNESSES
C
      CALL AF (F,14,A,B,C,A1,A2,A3,THK1,THK2,THK3,1)
      UNIMEM =.FALSE.
      UNIBEN =.FALSE.
      IF (ABS(A2).LE.1.0D-06 .AND. ABS(A3).LE.1.0D-06) UNIBEN =.TRUE.
      IF (ABS(C2).LE.1.0D-06 .AND. ABS(C3).LE.1.0D-06) UNIMEM =.TRUE.
      A1SQ = A1*A1
      A2SQ = A2*A2
      A3SQ = A3*A3
      C1 = A1SQ*A1
      C2 = 3.0*A1SQ*A2
      C3 = 3.0*A1SQ*A3
      C4 = 3.0*A1*A2SQ
      C5 = 6.0*A1*A2*A3
      C6 = 3.0*A3SQ*A1
      C7 = A2SQ*A2
      C8 = 3.0*A2SQ*A3
      C9 = 3.0*A2*A3SQ
      C10= A3*A3SQ
C
C     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
C     TRANSVERSE SHEAR
C
C    (POSSIBLY AN ERROR HERE - AA1,AA2, AND AA3 ARE NOT USED IN PROGRAM)
C     CALL AF (F,14,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,1)
C
      H4 = Q(4,1)*ZC(1) + Q(4,2)*ZC(2) + Q(4,3)*ZC(3) + Q(4,4)*ZC(4) +
     1     Q(4,5)*ZC(5) + Q(4,6)*ZC(6)
      H5 = Q(5,1)*ZC(1) + Q(5,2)*ZC(2) + Q(5,3)*ZC(3) + Q(5,4)*ZC(4) +
     1     Q(5,5)*ZC(5) + Q(5,6)*ZC(6)
      H6 = Q(6,1)*ZC(1) + Q(6,2)*ZC(2) + Q(6,3)*ZC(3) + Q(6,4)*ZC(4) +
     1     Q(6,5)*ZC(5) + Q(6,6)*ZC(6)
      H4 = H4*2.0D0
      H6 = H6*2.0D0
C
C     H5 IS MULTIPLIED BY 2.0, SO THAT EXY = DU/DY + DV/DX - ZXY*W
C
      H5 = H5*2.0D0
C
      DO 230 I = 1,32
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
      IXP1 = IX + 1
      JXP1 = JX + 1
      KXP1 = KX + 1
      LXP1 = LX + 1
      MXP1 = MX + 1
      NXP1 = NX + 1
      DO 220 J = I,32
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
      RMY1 = RMY*(RMY-1.0D0)
      RNY1 = RNY*(RNY-1.0D0)
      MX0  = MX + MY
      MX1  = MX + MY - 1
      MX2  = MX + MY - 2
      MX3  = MX + MY - 3
      NX0  = NX + NY
      NX1  = NX + NY - 1
      NX2  = NX + NY - 2
      NX3  = NX + NY - 3
      MY1  = MX + MY + 1
      NY1  = NX + NY + 1
      IX0  = IX + IY
      IX1  = IX0 - 1
      IX01 = IX0 + 1
      JX0  = JX  + JY
      JX1  = JX0 - 1
      JX01 = JX0 + 1
      KX0  = KX  + KY
      KX1  = KX0 - 1
      KX01 = KX0 + 1
      LX0  = LX  + LY
      LX1  = LX0 - 1
      LX01 = LX0 + 1
      IF (IPASS .EQ. 1) GO TO 110
      IX011= IX01 + 1
      JX011= JX01 + 1
      RHO  = RHOY*1.0D0
      IF (J .GT. 12) GO TO 100
      MSHL(IJ) = RHO*(CAB(1)*F(IX01,JX01) + CAB(2)*F(IX011,JX01) +
     1           CAB(3)*F(IX01,JX011)) + NSM*F(IX01,JX01)
      MSHL(JI) = MSHL (IJ)
  100 CONTINUE
      MX01  = MX0  + 1
      NX01  = NX0  + 1
      MX011 = MX01 + 1
      NX011 = NX01 + 1
      MSHL(IJ) = RHO*(A1*F(MX01,NX01) + A2*F(MX011,NX01) +
     1           A3*F(MX01,NX011)) + NSM*F(MX01,NX01)
      MSHL(JI) = MSHL(IJ)
      GO TO 210
  110 CONTINUE
      ST = 0.0D0
      IF (I.LE.12 .AND. J.GT.12) GO TO 160
      IF (I .GT. 12) GO TO 140
      DO 120 K = 1,3
      IXR1  = IX1 + RK(K)
      JXS01 = JX01+ SK(K)
      LXS1  = LX1 + SK(K)
      KXR01 = KX01+ RK(K)
      IXR01 = IX01+ RK(K)
      JXS1  = JX1 + SK(K)
      KXR1  = KX1 + RK(K)
      LXS01 = LX01+ SK(K)
      IYKX1 = IY + KX + RK(K)
      JYLX1 = JY + LX + SK(K)
      IXKY1 = IX + KY + RK(K)
      JXLY1 = JX + LY + SK(K)
      IXIY0 = IX + IY + RK(K)
      JXJY0 = JX + JY + SK(K)
      IYKX2 = IYKX1 - 1
      JYLX0 = JYLX1 + 1
      IXKY2 = IXKY1 - 1
      JXLY0 = JXLY1 + 1
      KXKY0 = KX + KY + RK(K)
      LXLY0 = LX + LY + SK(K)
      IXKY0 = IX + KY + RK(K) + 1
      JXLY2 = JXLY1 - 1
      IYKX0 = IY + KX + RK(K) + 1
      JYLX2 = JYLX1 - 1
      ST11  = 0.0D0
      ST22  = 0.0D0
      ST331 = 0.0D0
      ST332 = 0.0D0
      ST121 = 0.0D0
      ST122 = 0.0D0
      ST131 = 0.0D0
      ST132 = 0.0D0
      ST133 = 0.0D0
      ST231 = 0.0D0
      ST232 = 0.0D0
      ST233 = 0.0D0
      IF (IXR1 .GT. 0) ST11  = G11*RIX*RIY*F(IXR1,JXS01)
      IF (LXS1 .GT. 0) ST22  = G22*RLX*RLY*F(KXR01,LXS1)
      IF (JXS1 .GT. 0) ST331 = G33*RJX*RJY*F(IXR01,JXS1)
      IF (KXR1 .GT. 0) ST332 = G33*RKX*RKY*F(KXR1,LXS01)
      IF (IXKY1.GT.0 .AND. JXLY1.GT.0) ST121 = (G33*RJX*RKY +
     1                                 G12*RIX*RLY)*F(IXKY1,JXLY1)
      IF (IYKX1.GT.0 .AND. JYLX1.GT.0) ST122 = (G33*RJY*RKX +
     1                                 G12*RIY*RLX)*F(IYKX1,JYLX1)
      IF (IXIY0.GT.0 .AND. JXJY0.GT.0) ST131 = G13*(RIY*RJX +
     1                                 RIX*RJY)*F(IXIY0,JXJY0)
      IF (IYKX2 .GT. 0) ST132 = G13*RIY*RKX*F(IYKX2,JYLX0)
      IF (IXKY2 .GT. 0) ST133 = G13*RIX*RKY*F(IXKY2,JXLY0)
      IF (KXKY0.GT.0 .AND. LXLY0.GT.0) ST231 = G23*(RKX*RLY +
     1                                 RKY*RLX)*F(KXKY0,LXLY0)
      IF (JXLY2 .GT. 0) ST232 = G23*RJX*RLY*F(IXKY0,JXLY2)
      IF (JYLX2 .GT. 0) ST233 = G23*RJY*RLX*F(IYKX0,JYLX2)
C
      ST1 = (ST11  + ST22  + ST331 + ST332 + ST121 + ST122 + ST131 +
     1       ST132 + ST133 + ST231 + ST232 + ST233)* CAB(K)
      ST  = ST + ST1
      IF (UNIMEM) GO TO 130
  120 CONTINUE
  130 CONTINUE
      GO TO 200
  140 CONTINUE
      ST = 0.0D0
      DO 150 K = 1,10
      MX3X = MX3 + XTHK(K)
      NY1Y = NY1 + YTHK(K)
      MY1X = MY1 + XTHK(K)
      NX3Y = NX3 + YTHK(K)
      MX1X = MX1 + XTHK(K)
      NX1Y = NX1 + YTHK(K)
      MX2X = MX2 + XTHK(K)
      NX0Y = NX0 + YTHK(K)
      MX0X = MX0 + XTHK(K)
      NX2Y = NX2 + YTHK(K)
      S11  = 0.0D0
      S22  = 0.0D0
      S33  = 0.0D0
      S13  = 0.0D0
      S23  = 0.0D0
      IF (MX3X .GT. 0) S11 = D11*RMX1*RMY1*CC(K)*F(MX3X,NY1Y)
      IF (NX3Y .GT. 0) S22 = D22*RNX1*RNY1*CC(K)*F(MY1X,NX3Y)
      IF (MX1X.GT.0 .AND. NX1Y.GT.0) S33 = (D334*RMNX*RMNY +
     1    D12*(RMX1*RNY1+RMY1*RNX1))*CC(K)*F(MX1X,NX1Y)
      IF (MX2X.GT.0 .AND. NX0Y.GT.0) S13 = D132*(RMX1*RMNY +
     1    RMNX*RMY1)*CC(K)*F(MX2X,NX0Y)
      IF (MX0X.GT.0 .AND. NX2Y.GT.0) S23 = D232*(RMNX*RNY1 +
     1    RNX1*RMNY)*CC(K)*F(MX0X,NX2Y)
      ST = ST + (S11 + S22 + S33 + S13 + S23)/12.0D0
      IF (UNIBEN) GO TO 160
  150 CONTINUE
  160 CONTINUE
      SB7  = 0.0D0
      SB9  = 0.0D0
      SB10 = 0.0D0
      SB18 = 0.0D0
      SB21 = 0.0D0
      SB26 = 0.0D0
      SB28 = 0.0D0
      SB31 = 0.0D0
      SB36 = 0.0D0
      SB38 = 0.0D0
      DO 180 K = 1,3
      IXMYR = IX + MY + RK(K)
      JXNYS1= JX + NY + SK(K) + 1
      SB1  = 0.0D0
      SB2  = 0.0D0
      SB3  = 0.0D0
      SB4  = 0.0D0
      SB5  = 0.0D0
      SB6  = 0.0D0
      SB8  = 0.0D0
      SB11 = 0.0D0
      SB12 = 0.0D0
      SB13 = 0.0D0
      SB14 = 0.0D0
      SB15 = 0.0D0
      SB16 = 0.0D0
      SB17 = 0.0D0
      SB19 = 0.0D0
      SB20 = 0.0D0
      SB22 = 0.0D0
      SB23 = 0.0D0
      SB24 = 0.0D0
      SB25 = 0.0D0
      SB27 = 0.0D0
      SB29 = 0.0D0
      SB30 = 0.0D0
      SB32 = 0.0D0
      SB33 = 0.0D0
      SB34 = 0.0D0
      SB35 = 0.0D0
      SB37 = 0.0D0
      SB39 = 0.0D0
      SB40 = 0.0D0
      IF (IXMYR .GT. 0)  SB 1 =-G11*RIX*H4*CAB(K)*F(IXMYR,JXNYS1)
      IYMXR  = IY + MX + RK(K)
      JYNXS1 = JY + NX + SK(K) + 1
      IF (IYMXR .GT. 0)  SB 2 =-G11*RIY*H4*CAB(K)*F(IYMXR,JYNXS1)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB 3 = G11*H4**2*CAB(K)*F(MXMYR1,NXNYS1)
      KXMYR1 = KX + MY + RK(K) + 1
      LXNYS  = LX + NY + SK(K)
      IF (LXNYS .GT. 0)  SB 4 =-G22*RLX*H6*CAB(K)*F(KXMYR1,LXNYS)
      MXKYR1 = MX + KY + RK(K) + 1
      NXLYS  = NX + LY + SK(K)
      IF (NXLYS .GT. 0)  SB 5 =-G22*RLY*H6*CAB(K)*F(MXKYR1,NXLYS)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB 6 = G22*H6**2*CAB(K)*F(MXMYR1,NXNYS1)
      IXMYR1 = IX + MY + RK(K) + 1
      JXNYS  = JX + NY + SK(K)
      IF (JXNYS .GT. 0)  SB 8 =-G33*RJX*H5*CAB(K)*F(IXMYR1,JXNYS)
      KXMYR  = KX + MY + RK(K)
      LXNYS1 = LX + NY + SK(K) + 1
      IF (KXMYR .GT. 0)  SB11 =-G33*RKX*H5*CAB(K)*F(KXMYR,LXNYS1)
      MXIYR1 = MX + IY + RK(K) + 1
      NXJYS  = NX + JY + SK(K)
      IF (NXJYS .GT. 0)  SB12 =-G33*RJY*H5*CAB(K)*F(MXIYR1,NXJYS)
      MXKYR  = MX + KY + RK(K)
      NXLYS1 = NX + LY + SK(K) + 1
      IF (MXKYR .GT. 0)  SB13 =-G33*RKY*H5*CAB(K)*F(MXKYR,NXLYS1)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB14 = G33*H5**2*CAB(K)*F(MXMYR1,NXNYS1)
      IXMYR  = IX + MY + RK(K)
      JXNYS1 = JX + NY + SK(K) + 1
      IF (IXMYR .GT. 0)  SB15 =-G12*RIX*H6*CAB(K)*F(IXMYR,JXNYS1)
      MXKYR1 = MX + KY + RK(K) + 1
      NXLYS  = NX + LY + SK(K)
      IF (NXLYS .GT. 0)  SB16 =-G12*RLY*H4*CAB(K)*F(MXKYR1,NXLYS)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB17 = 2*G12*H4*H6*CAB(K)*F(MXMYR1,NXNYS1)
      KXMYR1 = KX + MY + RK(K) + 1
      LXNYS  = LX + NY + SK(K)
      IF (LXNYS .GT. 0)  SB19 =-G12*RLX*H4*CAB(K)*F(KXMYR1,LXNYS)
      MXIYR  = MX + IY + RK(K)
      NXJYS1 = NX + JY + SK(K) + 1
      IF (MXIYR .GT. 0)  SB20 =-G12*RIY*H6*CAB(K)*F(MXIYR,NXJYS1)
      IXMYR  = IX + MY + RK(K)
      JXNYS1 = JX + NY + SK(K) + 1
      IF (IXMYR .GT. 0)  SB22 =-G13*RIX*H5*CAB(K)*F(IXMYR,JXNYS1)
      MXIYR1 = MX + IY + RK(K) + 1
      NXJYS  = NX + JY + SK(K)
      IF (NXJYS .GT. 0)  SB23 =-G13*RJY*H4*CAB(K)*F(MXIYR1,NXJYS)
      MXKYR  = MX + KY + RK(K)
      NXLYS1 = NX + LY + SK(K) + 1
      IF (MXKYR .GT. 0)  SB24 =-G13*RKY*H4*CAB(K)*F(MXKYR,NXLYS1)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB25 = 2*G13*H4*H5*CAB(K)*F(MXMYR1,NXNYS1)
      IXMYR1 = IX + MY + RK(K) + 1
      JXNYS  = JX + NY + SK(K)
      IF (JXNYS .GT. 0)  SB27 =-G13*RJX*H4*CAB(K)*F(IXMYR1,JXNYS)
      KXMYR  = KX + MY + RK(K)
      LXNYS1 = LX + NY + SK(K) + 1
      IF (KXMYR .GT. 0)  SB29 =-G13*RKX*H4*CAB(K)*F(KXMYR,LXNYS1)
      MXIYR  = MX + IY + RK(K)
      NXJYS1 = NX + JY + SK(K) + 1
      IF (MXIYR .GT. 0)  SB30 =-G13*RIY*H5*CAB(K)*F(MXIYR,NXJYS1)
      KXMYR1 = KX + MY + RK(K) + 1
      LXNYS  = LX + NY + SK(K)
      IF (LXNYS .GT. 0)  SB32 =-G23*RLX*H5*CAB(K)*F(KXMYR1,LXNYS)
      MXIYR1 = MX + IY + RK(K) + 1
      NXJYS  = NX + JY + SK(K)
      IF (NXJYS .GT. 0)  SB33 =-G23*RJY*H6*CAB(K)*F(MXIYR1,NXJYS)
      MXKYR  = MX + KY + RK(K)
      NXLYS1 = NX + LY + SK(K) + 1
      IF (MXKYR .GT. 0)  SB34 =-G23*RKY*H6*CAB(K)*F(MXKYR,NXLYS1)
      MXMYR1 = MX + MY + RK(K) + 1
      NXNYS1 = NX + NY + SK(K) + 1
                         SB35 = 2*G23*H5*H6*CAB(K)*F(MXMYR1,NXNYS1)
      IXMYR1 = IX + MY + RK(K) + 1
      JXNYS  = JX + NY + SK(K)
      IF (JXNYS .GT. 0)  SB37 =-G23*RJX*H6*CAB(K)*F(IXMYR1,JXNYS)
      KXMYR  = KX + MY + RK(K)
      LXNYS1 = LX + NY + SK(K) + 1
      IF (KXMYR .GT. 0)  SB39 =-G23*RKX*H6*CAB(K)*F(KXMYR,LXNYS1)
      MXKYR1 = MX + KY + RK(K) + 1
      NXLYS  = NX + LY + SK(K)
      IF (NXLYS .GT. 0)  SB40 =-G23*RLY*H5*CAB(K)*F(MXKYR1,NXLYS)
      SB41 = SB3 + SB6 + SB14 + SB17 + SB25 + SB35
      IF (I .LE. 12) SB41 = 0.0D0
      ST = ST   + SB1  + SB2  + SB4  + SB5  + SB7  + SB8  + SB9  + SB10
     1   + SB11 + SB12 + SB13 + SB15 + SB16 + SB18 + SB19 + SB20 + SB21
     2   + SB22 + SB23 + SB24 + SB26 + SB27 + SB28 + SB29 + SB30 + SB31
     3   + SB32 + SB33 + SB34 + SB36 + SB37 + SB38 + SB39 + SB40 + SB41
      IF (UNIMEM) GO TO 190
  180 CONTINUE
  190 CONTINUE
  200 CONTINUE
      KSHL(IJ) = ST
      KSHL(JI) = KSHL(IJ)
  210 CONTINUE
  220 CONTINUE
  230 CONTINUE
      IF (IPASS .EQ. 2) GO TO 240
C
C     CURRENTLY,TRANSVERSE SHEAR CALCULATIONS ARE NOT CODED FOR SHELL
C     ELEMENT WHEN IT IS CODED,CALL THE ROUTINE HERE
C
  240 CONTINUE
C
C     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
C
      CALL GMMATD (Q,6,6,0, KSHL(  1),6,32,0, QKS(1))
      CALL GMMATD (Q,6,6,0, KSHL(193),6,32,0, QKS(193))
      CALL GMMATD (QQQINV,20,18,+1, KSHL(385),20,32,0, QKS(385))
      DO 260 I = 1,30
      DO 250 J = 1,6
      IJ = (I-1)*32 + J
      JI = (I-1)*6 + J
      KSHL(JI) = QKS(IJ)
      KSHL(180+JI) = QKS(6+IJ)
  250 CONTINUE
  260 CONTINUE
      DO 280 I = 1,30
      DO 270 J = 1,20
      IJ = (I-1)*32 + J + 12
      JI = (I-1)*20 + J + 360
      KSHL(JI) = QKS(IJ)
  270 CONTINUE
  280 CONTINUE
      CALL GMMATD (KSHL(1  ),30,6 ,0, Q,6,6,1 , QKS(1  ))
      CALL GMMATD (KSHL(181),30,6 ,0, Q,6,6,1 , QKS(181))
      CALL GMMATD (KSHL(361),30,20,0, QQQINV,20,18,0, QKS(361))
      DO 300 I = 1,30
      DO 290 J = 1,6
      IJ = (I-1)*30 + J
      JI = (I-1)*6  + J
      CMS(IJ  ) = QKS(JI    )
      CMS(IJ+6) = QKS(JI+180)
  290 CONTINUE
  300 CONTINUE
      DO 320 I = 1,30
      DO 310 J = 1,18
      IJ = (I-1)*30 + J + 12
      JI = (I-1)*18 + J + 360
      CMS(IJ) = QKS(JI)
  310 CONTINUE
  320 CONTINUE
      DO 330 I = 1,30
      EE(I) = 0.0D0
  330 CONTINUE
      EE(1)  =  IVECT(1)
      EE(2)  =  JVECT(1)
      EE(3)  =  KVECT(1)
      EE(6)  =  IVECT(2)
      EE(7)  =  JVECT(2)
      EE(8)  =  KVECT(2)
      EE(11) =  IVECT(3)
      EE(12) =  JVECT(3)
      EE(13) =  KVECT(3)
      EE(19) =  IVECT(1)
      EE(20) =  JVECT(1)
      EE(24) =  IVECT(2)
      EE(25) =  JVECT(2)
      EE(29) =  IVECT(3)
      EE(30) =  JVECT(3)
      DO 360 K = 1,6
      DO 350 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 5*(K-1) + I
      DO 340 J = 1,30
      CTM (I1,J) = CM1(K1,J)
  340 CONTINUE
  350 CONTINUE
  360 CONTINUE
      DO 390 K = 1,6
      DO 380 I = 1,3
      I2 = 5*(K-1) + I + 2
      K2 = 12 + (K-1)*3 + I
      DO 370 J = 1,30
      CTM (I2,J) = CM1(K2,J)
  370 CONTINUE
  380 CONTINUE
  390 CONTINUE
      DO 420 K = 1,6
      DO 410 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 5*(K-1) + I
      DO 400 J = 1,30
      CM1(J,I1) = CTM(J,K1)
  400 CONTINUE
  410 CONTINUE
  420 CONTINUE
      DO 450 K = 1,6
      DO 440 I = 1,3
      I2 = 5*(K-1) + I + 2
      K2 = 12 + (K-1)*3 + I
      DO 430 J = 1,30
      CM1(J,I2) = CTM(J,K2)
  430 CONTINUE
  440 CONTINUE
  450 CONTINUE
      DO 460 I = 1,1296
      CMT(I) = 0.0D0
  460 CONTINUE
C
C     LUMPED MASS COMPUTATION
C
      IF (IPASS .NE. 2) GO TO 490
  470 AMASS = (RHOY*VOL + NSM*AREA)/6.
      DO 480 I = 1,1296,37
      CMT(I) = AMASS
  480 CONTINUE
      IPASS = 2
      GO TO 690
C
C     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
C     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
C     ARE R
C     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
C
C     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
C     COORDINATES
C
C     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINAYES TO GLOBAL (DISP)
C     COORDINATES
C
C     INSERT THE 6X6 SUBMATRIX  INTO KGG MATRIX
C
  490 DO 500 I = 1,6
      SAVE(I) = NL(I)
  500 CONTINUE
      DO 530 I = 1,6
      SMALL(I) = I
      ISMALL = NL(I)
      DO 520 J = 1,6
      IF (ISMALL .LE. NL(J)) GO TO 510
      SMALL(I) = J
      ISMALL = NL(J)
  510 CONTINUE
  520 CONTINUE
      ISM = SMALL(I)
      NL(ISM) = 1000000
  530 CONTINUE
      DO 540 I = 1,6
      NL(I) = SAVE(I)
  540 CONTINUE
      DO 680 I = 1,6
      SIL1 = SMALL(I)
      DO 670 J = I,6
      SIL2 = SMALL(J)
      DO 550 II = 1,36
      BALOTR(II) = 0.0D0
      KSUP(II)   = 0.0D0
  550 CONTINUE
      DO 570 K = 1,5
      K1 = (SIL1-1)*5 + K
      DO 560 L = 1,5
      L1 = (SIL2-1)*5 + L
      CSUB(K,L)=CM1(K1,L1)
  560 CONTINUE
  570 CONTINUE
      CALL GMMATD (EE,6,5,0, CSUB,5,5,0, CSUBT)
      CALL GMMATD (CSUBT,6,5,0, EE,6,5,+1, KSUPT)
      DO 580 K = 1,6
      DO 580 L = 1,6
      K1 = (K-1)*6 + L
      L1 = (L-1)*6 + K
      KSUP(L1) = KSUPT(K1)
  580 CONTINUE
C
C     TRANSFORM THE KSUP(36) FROM BASIC TO DISPLACEMENT COORDINATES
C
      IF (NL(SIL1).EQ.0 .OR. ICS(SIL1).EQ.0) GO TO 610
      JJ = 4*SIL1 + 24
      CALL TRANSD (IEST(JJ),TRAND)
      DO 590 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  590 CONTINUE
      CALL GMMATD (BALOTR(1),6,6,1, KSUP(1),6,6,0, KSUPT)
      DO 600 K = 1,36
      KSUP(K) = KSUPT(K)
  600 CONTINUE
  610 CONTINUE
      IF (NL(SIL2).EQ.0 .OR. ICS(SIL2).EQ.0) GO TO 650
      IF (J .EQ. I) GO TO 630
      JJ = 4*SIL2 + 24
      CALL TRANSD (IEST(JJ),TRAND)
      DO 620 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  620 CONTINUE
  630 CONTINUE
      CALL GMMATD (KSUP(1),6,6,0, BALOTR(1),6,6,0, KSUPT)
      DO 640 K = 1,36
      KSUP(K) = KSUPT(K)
  640 CONTINUE
  650 CONTINUE
      DO 660 II = 1,6
      DO 660 JJ = 1,6
      I1 = (I-1)*6 + II
      J1 = (J-1)*6 + JJ
      CTM(I1,J1) = KSUB(JJ,II)
      CTM(J1,I1) = KSUB(JJ,II)
  660 CONTINUE
  670 CONTINUE
  680 CONTINUE
  690 CALL EMGOUT (CMT(1),CMT(1),1296,1,DICT,IPASS,IPREC)
      IF (.NOT.IMASS .OR. IPASS.GE.2) RETURN
C
C     TO TO 295 TO COMPUTE LUMPED MASS MATRIX
C     GO TO 211 TO COMPUTE CONSIST. MASS MATRIX (THIS PATH DOES NOT
C     WROK)
C
      IPASS = 3
      CALL SSWTCH (46,J)
      GO TO (720,90,470), IPASS
C
C     ERROR
C
  700 CONTINUE
      NOGO  =.TRUE.
      KNOGO = 1
      WRITE  (IOUTPT,710) UFM,IEST(1)
  710 FORMAT (A23,' 2416, MATRIX RELATING GENERALIZED PARAMETERS AND ',
     1       'GRID POINT DISPLACEMENTS IS SINGULAR.', //26X,
     2       'CHECK COORDINATES OF ELEMENT  TRSHL WITH ID',I9,1H.)
  720 CONTINUE
      RETURN
      END
