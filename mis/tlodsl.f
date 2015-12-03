      SUBROUTINE TLODSL (TREAL,TINT)
C
C     ECPT ENTRIES
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
C     ECPT (26) = DISTANCE Z15 FOR STRESS CALCULATION  AT GRID POINT G5
C     ECPT (27) = DISTANCE Z25 FOR STRESS CALCULATION  AT GRID POINT G5
C
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE  IN NASTRAN BASIC SYSTEM
C
C     ECPT (28) = CO-ORDINATE SYSTEM ID FOR GRID A              INTEGER
C     ECPT (29) = CO-ORDINATE X1                                REAL
C     ECPT (30) = CO-ORDINATE Y1                                REAL
C     ECPT (31) = CO-ORDINATE Z1                                REAL
C     ECPT (32) = CO-ORDINATE SYSTEM ID FOR GRID B              INTEGER
C     ECPT (33) = CO-ORDINATE X1                                REAL
C     ECPT (34) = CO-ORDINATE Y1                                REAL
C     ECPT (35) = CO-ORDINATE Z1                                REAL
C     ECPT (36) = CO-ORDINATE SYSTEM ID FOR GRID C              INTEGER
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
C     EST  (52) = ELEMENT TEMPERATURE
C
      LOGICAL         NOGO,     NOTS,     UNIBEN,   UNITEM,   UNIMEM
      INTEGER         XU(32),   YU(32),   XV(32),   YV(32),   XW(32),
     1                YW(32),   RK(3),    SK(3),    ELID,     ESTID,
     2                TL(3),    UL(3),    QT(3),    PT(3),    XTHK(10),
     3                YTHK(10), SMALL(6), TINT(6),  ICS(6),   IEST(42),
     4                NL(6),    IND(6,3), ELTYPE,   SIL1,    INDEX(20,3)
      REAL            TREAL(6), G(9),     G1(3),    NAME(2),  F(14,14),
     1                XC(6),    YC(6),    ZC(6),    TS6(40),  EE(30),
     2                Q(6,6),   QQ(960),  CC(10),   CAB(3),   P3(30),
     3                P6(32),   P7(30),   P8(5),    P9(6),    PL(3),
     4                DD(3),    TRAND(9), PTEM(32), GE1(9),   EL(3),
     5                IVECT(3), JVECT(3), KVECT(3),
     6                QQQ(20,20),         QQQINV(360),        BALOTR(36)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /TRIMEX/ EST(100)
      COMMON /ZZZZZZ/ PG(1)
      COMMON /SSGWRK/ IND,FAC,F,P3,P6,P7
      COMMON /EMGDIC/ ELTYPE,LDICT,NLOCS,ELID,ESTID
      COMMON /MATIN / MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/ EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                RJ11,RJ12,RJ22
      COMMON /SYSTEM/ IBUF,NOUT
      EQUIVALENCE     (C1,CC(1)),(C2,CC(2)),(C3,CC(3)),(C4,CC(4)),
     1                (C5,CC(5)),(C6,CC(6)),(C7,CC(7)),(C8,CC(8)),
     2                (C9,CC(9)),(C10,CC(10)),(IEST(1),EST(1)),
     3                (A,DISTA),(B,DISTB),(C,DISTC),(THK1,TBEND1),
     4                (THK2,TBEND3),(THK3,TBEND5)
      DATA    XU    / 0,1,0,2,1,0,26*0/,      YU/0,0,1,0,1,2,26*0    /,
     1        XV    / 6*0,0,1,0,2,1,0,20*0/ , YV/6*0,0,0,1,0,1,2,20*0/,
     2        XW    / 12*0,0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,3,2,1,0   /,
     3        YW    / 12*0,0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,0,2,3,4,5   /
      DATA    BLANK , NAME / 4H    ,  4HTRSH,4HL          /
      DATA    RK,SK / 0,1,0,  0,0,1/, DEGRA /0.0174532925 /
      DATA    XTHK  / 0,1,0,2,1,0,3,2,1,0/,  YTHK/ 0,0,1,0,1,2,0,1,2,3/
      DATA    TL    / 0,1,0/, UL/0,0,1/, PT/0,1,0/, QT/0,0,1/
C
C     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
C
      NOTS   = .FALSE.
      IDELE  = IEST(1)
      DO 109 I = 1,6
      NL(I)  = IEST(I+1)
  109 CONTINUE
      THETAM = EST (8)
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
      DO 120 I = 28,48,4
      J      = J + 1
      ICS(J) = IEST(I)
      XC(J)  = EST(I+1)
      YC(J)  = EST(I+2)
      ZC(J)  = EST(I+3)
  120 CONTINUE
C
C     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK, THEY WILL BE
C     SET EQUAL TO TMEM1 SO ALSO FOR TSHR3,TSHR5,TBEND3 AND TBEND5
C
      T1PRIM = -TREAL(2)
      T3PRIM = -TREAL(2)
      T5PRIM = -TREAL(2)
      IF (TMEM3.EQ.0.0 .OR. TMEM3.EQ.BLANK) TMEM3 = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5.EQ.BLANK) TMEM5 = TMEM1
      IF (TSHR3.EQ.0.0 .OR. TSHR3.EQ.BLANK) TSHR3 = TSHR1
      IF (TSHR5.EQ.0.0 .OR. TSHR5.EQ.BLANK) TSHR5 = TSHR1
      IF (TSHR1 .EQ. 0.0) NOTS = .TRUE.
      IF (TBEND3.EQ.0.0 .OR. TBEND3.EQ.BLANK) TBEND3 = TBEND1
      IF (TBEND5.EQ.0.0 .OR. TBEND5.EQ.BLANK) TBEND5 = TBEND1
      IF (T3PRIM.EQ.0.0 .OR. T3PRIM.EQ.BLANK) T3PRIM = T1PRIM
      IF (T5PRIM.EQ.0.0 .OR. T5PRIM.EQ.BLANK) T5PRIM = T1PRIM
      ELTEMP = EST(52)
      AVTHK  = (TBEND1 + TBEND3 + TBEND5)/3.0
      AVINER = AVTHK**3/12.0
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     EVALUTE MATERIAL PROPERTIES
C
      MATFLG = 2
      MATID  = MATID1
      IF (MATID .LE. 0) GO TO 122
      CALL MAT (IDELE)
C
      G11 = EM(1)
      G12 = EM(2)
      G13 = EM(3)
      G22 = EM(4)
      G23 = EM(5)
      G33 = EM(6)
      GM1 = EM(1)*ALF(1) + EM(2)*ALF(2) + EM(3)*ALF(3)
      GM2 = EM(2)*ALF(1) + EM(4)*ALF(2) + EM(5)*ALF(3)
      GM3 = EM(3)*ALF(1) + EM(5)*ALF(2) + EM(6)*ALF(3)
      G11PR = 0.0
      G22PR = 0.0
      G33PR = 0.0
  122 CONTINUE
      MATFLG = 2
      MATID  = MATID2
      IF (MATID .LE. 0) GO TO 149
      CALL MAT (IDELE)
      D11  = EM(1)
      D12  = EM(2)
      D13  = EM(3)
      D22  = EM(4)
      D23  = EM(5)
      D33  = EM(6)
      G(1) = EM(1)
      G(2) = EM(2)
      G(3) = EM(3)
      G(4) = EM(2)
      G(5) = EM(4)
      G(6) = EM(5)
      G(7) = EM(3)
      G(8) = EM(5)
      G(9) = EM(6)
C
C     IF  TINT(6).NE.1,G1 IS G AND T1PRIME IS ALPHA TIMES T1PRIME
C     IF  TINT(6).EQ.1,G1 IS G TIMES ALPHA AND T1PRIME IS T1PRIME
C
      IF  (TINT(6) .NE. 1) GO TO 147
C
C     G1 IS G TIMES ALFA
C
      CALL GMMATS (G,3,3,0, ALF,3,1,0, G1)
C
      G11PR = G1(1)
      G22PR = G1(2)
      G33PR = G1(3)
      GO TO 149
  147 CONTINUE
      DO 148 I = 1,9
  148 GE1(I) = G(I)*AVINER
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY
C
      ISING = -1
      CALL INVERS (3,GE1(1),3,TS6(1),0,DETERM,ISING,INDEX)
      IF (ISING .EQ. 2) GO TO 905
      CALL GMMATS (GE1,3,3,0, TREAL(2),3,1,0, PL(1))
  149 CONTINUE
C
C     CALCULATIONS FOR THE TRIANGLE
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     COMPUTE THE AREA INTEGRATION FUNCTION F
C
      CALL AF (F,14,A,B,C,0,0,0,0,0,0,-1)
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
      DO 128 I = 1,6
      DO 128 J = 1,6
      I1 = (I-1)*3 + 1
      Q(I,J) = QQQ(I1,J)
  128 CONTINUE
C
C     SET ISING = -1
C
      ISING = -1
      CALL INVERS (6,Q,6,TS6(1),0,DETERM,ISING,IND)
      IF (ISING .EQ. 2) GO TO 904
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1) I
C
C     AGAIN SET ISING = -1
C
      ISING = -1
      CALL INVERS (20,QQQ,20,TS6(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
      IF (ISING .EQ. 2) GO TO 905
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      DO  153 I = 1,960
      QQ(I) = 0.0
  153 CONTINUE
      DO 154 I = 1,6
      DO 154 J = 1,6
      IJ = (I-1)*30 + J
      IK = (I+5)*30 + J + 6
      QQ(IJ) = Q(I,J)
      QQ(IK) = Q(I,J)
  154 CONTINUE
      DO 156 I = 1,20
      DO 156 J = 1,18
      IJ = 372 + (I-1)*30 + J
      QQ (IJ) = QQQ(I,J)
      IJ1 = (I-1)*18 + J
      QQQINV(IJ1) = QQQ(I,J)
  156 CONTINUE
C
C     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
C
C     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
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
      UNIBEN =.FALSE.
      UNIMEM =.FALSE.
      UNITEM =.FALSE.
      IF (ABS(C2).LE.1.0E-06 .AND. ABS(C3).LE.1.0E-06) UNIMEM =.TRUE.
C
      D334 = D33*4.0
      D132 = D13*2.0
      D232 = D23*2.0
C
C     DD(1) TO DD(3) ARE THE CONSTANTS IN LINEAR EQUATION FOR TEMP
C     GRADIENT. CURRENTLY ONLY UNIFORM TEMP GRADIENT IS PERMITTED IN
C     THE ELEMENT, THOUGH THE CODE IS WRITTEN FOR LINEAR VARIATION
C
      CALL AF (F,14,A,B,C,DD(1),DD(2),DD(3),T1PRIM,T3PRIM,T5PRIM,1)
C
C     EL(1) TO EL(3) ARE THE CONSTANTS IN THE LINEAR EQUATION FOR
C     MEAN TEMP VARIATION
C
      EL(1) = TREAL(1) - TREF
      EL(2) = 0.0
      EL(3) = 0.0
C
C     A1,A2,A3 ARE THE COEFFICIENTS OF LINEAR EQUATION FOR VARIATION
C     OF BENDING THICKNESSES
C
      CALL AF (F,14,A,B,C,A1,A2,A3,THK1,THK2,THK3,1)
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
      IF (ABS(A2).LE.1.0E-06 .AND. ABS(A3).LE.1.0E-06) UNIBEN =.TRUE.
      EL2  = EL(2)
      EL3  = EL(3)
      IF (ABS(EL2).LE.1.E-06 .AND. ABS(EL3).LE.1.E-06) UNITEM =.TRUE.
C
C     AA1, AA2, AA3  ARE COEFFICIENTS IN THICKNESS VARIATION FOR
C     TRANSVERSE SHEAR
C
      CALL AF (F,14,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,1)
      H4 = Q(4,1)*ZC(1) + Q(4,2)*ZC(2) + Q(4,3)*ZC(3) + Q(4,4)*ZC(4) +
     1     Q(4,5)*ZC(5) + Q(4,6)*ZC(6)
      H5 = Q(5,1)*ZC(1) + Q(5,2)*ZC(2) + Q(5,3)*ZC(3) + Q(5,4)*ZC(4) +
     1     Q(5,5)*ZC(5) + Q(5,6)*ZC(6)
      H6 = Q(6,1)*ZC(1) + Q(6,2)*ZC(2) + Q(6,3)*ZC(3) + Q(6,4)*ZC(4) +
     1     Q(6,5)*ZC(5) + Q(6,6)*ZC(6)
      H4 = H4*2.0
      H6 = H6*2.0
C
C     H5 IS MULTIPLIED BY 2.0, SO THAT EXY=DU/DY + DV/DX - ZXY*W
C
      H5 = H5*2.0
C
C     CALCULATION OF THERMAL LOAD VECTOR
C
      DO 670 I = 1,32
      PTEM(I) = 0.0
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
      RMX1 = RMX*(RMX-1.0)
      RNX1 = RNX*(RNX-1.0)
      IXP1 = IX + 1
      JXP1 = JX + 1
      KXP1 = KX + 1
      LXP1 = LX + 1
      MXP1 = MX + 1
      NXP1 = NX + 1
      MX01 = MX - 1
      MX1  = MX + 1
      NX01 = NX - 1
      NX1  = NX + 1
      PTEMP= 0.0
      IF (I .LE. 12) GO TO 225
      DO 215 K = 1,10
      MX01X= MX01+ XTHK(K)
      NX1Y = NX1 + YTHK(K)
      MX1X = MX1 + XTHK(K)
      NX01Y= NX01+ YTHK(K)
      MXX  = MX  + XTHK(K)
      NXY  = NX  + YTHK(K)
      IF (TINT(6) .NE. 1) GO TO 213
      DO 212 L = 1,3
      MX01XP= MX01X+ PT(L)
      NX1YQ = NX1Y + QT(L)
      MX1XP = MX1X + PT(L)
      NX01YQ= NX01Y+ QT(L)
      MXXP  = MXX  + PT(L)
      NXYQ  = NXY  + QT(L)
      IF (MX01XP.GT.0 .AND. NX1YQ.GT.0)
     1    PTEMP = PTEMP + CC(K)*DD(L)*G1(1)*RMX1*F(MX01XP,NX1YQ)
      IF (MX1XP.GT.0 .AND. NX01YQ.GT.0)
     1    PTEMP = PTEMP + CC(K)*DD(L)*G1(2)*RNX1*F(MX1XP,NX01YQ)
      IF (MXXP.GT.0 .AND. NXYQ.GT.0)
     1    PTEMP = PTEMP + CC(K)*DD(L)*G1(3)*RMNX*F(MXXP,NXYQ)
      IF (UNITEM) GO TO 213
  212 CONTINUE
  213 CONTINUE
      IF (TINT(6) .EQ. 1) GO TO 214
      IF (MX01X .GT. 0) PTEMP = PTEMP + CC(K)*RMX1*(PL(1)*G(1)
     1                        + PL(2)*G(2) + PL(3)*G(3))*F(MX01X,NX1Y)
      IF (NX01Y .GT. 0) PTEMP = PTEMP + CC(K)*RNX1*(PL(1)*G(4)
     1                        + PL(2)*G(5) + PL(3)*G(6))*F(MX1X,NX01Y)
      IF (MXX.GT.0 .AND. NXY.GT.0) PTEMP = PTEMP + CC(K)*RMNX*(PL(1)*
     1                        G(7)+PL(2)*G(8)+PL(3)*G(9))*F(MXX,NXY)
  214 CONTINUE
      IF (UNIBEN) GO TO 2150
  215 CONTINUE
 2150 CONTINUE
      PTEM(I) = PTEMP/12.0
      GO TO 235
  225 CONTINUE
      DO 263 K = 1,3
      IXR = IX + RK(K)
      JXS = JX + SK(K)
      KXR = KX + RK(K)
      LXS = LX + SK(K)
      DO 262 L = 1,3
      IXRT  = IXR + TL(L)
      JXSU1 = JXS + UL(L) + 1
      KXRT1 = KXR + TL(L) + 1
      LXSU  = LXS + UL(L)
      IXRT1 = IXRT  + 1
      JXSU  = JXSU1 - 1
      KXRT  = KXRT1 - 1
      LXSU1 = LXSU  + 1
      MKR1  = MX + KX + RK(K) - 1
      NLS1  = NX + LX + SK(K) - 1
      IF (IXRT .GT. 0) PTEMP = PTEMP +CAB(K)*EL(L)*GM1*RIX*F(IXRT,JXSU1)
      IF (LXSU .GT. 0) PTEMP = PTEMP +CAB(K)*EL(L)*GM2*RLX*F(KXRT1,LXSU)
      IF (JXSU .GT. 0) PTEMP = PTEMP +CAB(K)*EL(L)*GM3*RJX*F(IXRT1,JXSU)
      IF (KXRT .GT. 0) PTEMP = PTEMP +CAB(K)*EL(L)*GM3*RKX*F(KXRT,LXSU1)
      IF (MKR1.GT.0 .AND. NLS1.GT.0) PTEMP = PTEMP -(G11PR*H4*EL(L)*
     1    CAB(K)*F(MKR1,NLS1))-(G22PR*H4*EL(L)*CAB(K)*F(MKR1,NLS1))-
     2    (G33PR*H4*EL(L)*CAB(K)*F(MKR1,NLS1))
      IF (UNITEM) GO TO 2620
  262 CONTINUE
 2620 CONTINUE
      IF (UNIMEM) GO TO 2630
  263 CONTINUE
 2630 CONTINUE
      PTEM(I) = PTEMP
  235 CONTINUE
  670 CONTINUE
      CALL GMMATS (QQ,32,30,+1,PTEM,32,1,0,P6)
      DO 179 I = 1,30
      EE(I)  = 0.0
  179 CONTINUE
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
      DO 278 K = 1,6
      DO 277 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 5*(K-1) + I
      P7(I1) = P6(K1)
  277 CONTINUE
  278 CONTINUE
      DO 283 K = 1,6
      DO 282 I = 1,3
      I2 = 5*(K-1) + I + 2
      K2 = 12 + (K-1)*3 + I
      P7(I2) = P6(K2)
  282 CONTINUE
  283 CONTINUE
C
C     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
C     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
C     ARE ROTATED
C     - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
C
C     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
C     COORDINATES
C
C     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINAYES TO GLOBAL (DISP)
C     COORDINATES
C
      DO 302 I = 1,6
      SMALL(I) = I
  302 CONTINUE
      DO 308 I = 1,6
      SIL1 = SMALL(I)
      DO 310 II = 1,36
      BALOTR(II) = 0.0
  310 CONTINUE
      DO 304 K = 1,5
      K1 = (SIL1-1)*5 + K
      P8(K) = P7(K1)
  304 CONTINUE
      CALL GMMATS (EE,6,5,0,P8,5,1,0,P9)
C
C     TRANSFORM THE KSUB(36) FROM BASIC TO DISPLACEMENT COORDINATES
C
      IF (NL(SIL1).EQ.0 .OR. ICS(SIL1).EQ.0) GO TO 330
      JJ = 4*SIL1 + 24
      CALL TRANSS (IEST(JJ),TRAND)
      DO 320 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+ 1) = TRAND(M+1)
      BALOTR(L+ 2) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  320 CONTINUE
      CALL GMMATS (BALOTR(1),6,6,1, P9(1),6,1,0, P6(1))
      DO 350 K = 1,6
  350 P9(K) = P6(K)
  330 CONTINUE
      DO 370 II = 1,6
      I2 = IEST(I+1) + II - 1
      PG(I2) = PG(I2) + P9(II)
  370 CONTINUE
  308 CONTINUE
      GO TO 999
  904 WRITE (NOUT,2416) UFM,IEST(1)
      GO TO 910
  905 WRITE (NOUT,2417) UFM,IEST(1)
  910 NOGO = .TRUE.
      GO TO 999
C
 2416 FORMAT (A23,' 2416, MATRIX RELATING GENERALIZED PARAMETERS AND ',
     1       'GRID POINT DISPLACEMENTS IS SINGULAR.',/26X,
     2       'CHECK COORDINATES OF ELEMENT  TRSHL WITH ID =',I9,1H.)
 2417 FORMAT (A23,' 2417, A SINGULAR MATERIAL MATRIX FOR ELEMENT ID =',
     1       I9,' HAS BEEN DETECTED BY SUBROUTINE TLODSL', /26X,'WHILE',
     2       ' TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
  999 RETURN
      END
