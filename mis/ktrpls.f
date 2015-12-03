      SUBROUTINE KTRPLS
C
C     STIFFNESS SUBROUTINE FOR HIGHER ORDER PLATE ELEMENT CTRPLT1
C
C     ECPT ENTRIES
C
C     ECPT( 1) = ELEMENT ID                               INTEGER
C     ECPT( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1     INTEGER
C     ECPT( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2     INTEGER
C     ECPT( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3     INTEGER
C     ECPT( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4     INTEGER
C     ECPT( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5     INTEGER
C     ECPT( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6     INTEGER
C     ECPT( 8) = THETA                                    REAL
C     ECPT( 9) = MATERIAL ID 1                            INTEGER
C     ECPT(10) = THICKNESS T1 AT GRID POINT G1
C     ECPT(11) = THICKNESS T3 AT GRID POINT G3
C     ECPT(12) = THICKNESS T5 AT GRID POINT G5
C     ECPT(13) = MATERIAL ID 2                            INTEGER
C     ECPT(14) = THICKNESS TSHR1 FOR TRANSVERSE SHEAR AT GRID
C     ECPT(15) = THICKNESS TSHR3 FOR TRANSVERSE SHEAR AT GRID
C     ECPT(16) = THICKNESS TSHR5 FOR TRANSVERSE SHEAR AT GRID
C     ECPT(17) = NON-STRUCTURAL MASS                      REAL
C     ECPT(18) = DISTANCE Z11 FOR STRESS CALCULATION AT GRID 1
C     ECPT(19) = DISTANCE Z21 FOR STRESS CALCULATION AT GRID 1
C     ECPT(20) = DISTANCE Z13 FOR STRESS CALCULATION AT GRID 3
C     ECPT(21) = DISTANCE Z23 FOR STRESS CALCULATION AT GRID 3
C     ECPT(22) = DISTANCE Z15 FOR STRESS CALCULATION AT GRID 5
C     ECPT(23) = DISTANCE Z25 FOR STRESS CALCULATION AT GRID 5
C
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
C
C     ECPT(24) = COORDINATE SYSTEM ID FOR GRID A          INTEGER
C     ECPT(25) = COORDINATE X1                            REAL
C     ECPT(26) = COORDINATE Y1                            REAL
C     ECPT(27) = COORDINATE Z1                            REAL
C     ECPT(28) = COORDINATE SYSTEM ID FOR GRID B          INTEGER
C     ECPT(29) = COORDINATE X1                            REAL
C     ECPT(30) = COORDINATE Y1                            REAL
C     ECPT(31) = COORDINATE Z1                            REAL
C     ECPT(32) = COORDINATE SYSTEM ID FOR GRID C          INTEGER
C     ECPT(33) = COORDINATE X1                            REAL
C     ECPT(34) = COORDINATE Y1                            REAL
C     ECPT(35) = COORDINATE Z1                            REAL
C     ECPT(36) = COORDINATE SYSTEM ID FOR GRID D          INTEGER
C     ECPT(37) = COORDINATE X1                            REAL
C     ECPT(38) = COORDINATE Y1                            REAL
C     ECPT(39) = COORDINATE Z1                            REAL
C     ECPT(40) = COORDINATE SYSTEM ID FOR GRID E          INTEGER
C     ECPT(41) = COORDINATE X1                            REAL
C     ECPT(42) = COORDINATE Y1                            REAL
C     ECPT(43) = COORDINATE Z1                            REAL
C     ECPT(44) = COORDINATE SYSTEM ID FOR GRID F          INTEGER
C     ECPT(45) = COORDINATE X1                            REAL
C     ECPT(46) = COORDINATE Y1                            REAL
C     ECPT(47) = COORDINATE Z1                            REAL
C     ECPT(48) = ELEMENT TEMPERATURE                      REAL
C
      LOGICAL         IMASS,NOTS,NOGO,UNIBEN
      INTEGER         NAME(2),INDEX(20,3),XPOWER(20),YPOWER(20),ICS(6),
     1                NL(6),IEST(42),XTHK(10),YTHK(10),SAVE(6),SMALL(6),
     2                DICT(11),FLAGS,ELTYPE,ELID,ESTID,PRECIS,SIL1,SIL2
      REAL            NSM,IVECT(3),JVECT(3),KVECT(3),F(14,14),
     1                XC(6),YC(6),ZC(6),CC(10),KSUP(36),KSUPT(36),
     2                E(18),CMT(1296),QQQ(20,20),QQQINV(360),MTR3(400),
     3                KTR3(400),CSUB(3,3),CSUBT(6,3),TS6(40),TS1(60),
     4                TS6S(40),TS2(60),TS7(60),TRAND(9),BALOTR(36),
     5                KSUB(6,6),KSUBT(6,6)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / NOK,NOM
      COMMON /SMA1DP/ CM1(18,18)
      COMMON /EMGEST/ EST(100)
      COMMON /EMGDIC/ ELTYPE,LDICT,NLOCS,ELID,ESTID
      COMMON /EMGPRM/ ICORE,JCORE,NCORE,DUM(12),FLAGS(3),PRECIS
      COMMON /SMA1IO/ X,Y,Z,DISTA,DISTB,DISTC,A1,A2,A3,AA1,AA2,AA3
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /MATIN / MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/ EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                RJ11,RJ12,RJ22
      EQUIVALENCE     (KSYSTM(2),IOUTPT),(KSUB(1,1),KSUP(1)),
     1                (KSUBT(1,1),KSUPT(1)),(C1,CC(1)),(C2,CC(2)),
     2                (C3,CC(3)),(C4,CC(4)),(C5,CC(5)),(C6,CC(6)),
     3                (C7,CC(7)),(C8,CC(8)),(C9,CC(9)),(C10,CC(10)),
     4                (THK1,TMEM1),(THK2,TMEM3),(THK3,TMEM5),
     5                (A,DISTA),(B,DISTB),(C,DISTC),(IEST(1),EST(1)),
     6                (CMT(1),KTR3(1),MTR3(1),QQQ(1,1)),
     7                (CM1(1,1),TS6(1)),(CM1(5,3),TS1(1)),
     8                (CM1(11,6),TS6S(1)),(CM1(15,8),TS2(1)),
     9                (CM1(3,12),TS7(1))
      DATA    XPOWER/ 0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,3,2,1,0/
      DATA    YPOWER/ 0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,0,2,3,4,5/
      DATA    XTHK  / 0,1,0,2,1,0,3,2,1,0 /
      DATA    YTHK  / 0,0,1,0,1,2,0,1,2,3 /
      DATA    DEGRA / 0.0174532925/
      DATA    BLANK , NAME / 4H    , 4HTRPL, 4HT1    /
C
C     COMPONENT CODE,ICODE,IS  111111  AND HAS A VALUE OF 63
C
      ICODE   = 63
      NDOF    = 36
      IPREC   = PRECIS
      NLOCS   = 6
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = NDOF
      DICT(4) = ICODE
      DICT(5) = GSUBE
      NOTS    = .FALSE.
      IMASS   = .FALSE.
      IF (NOM .GT. 0) IMASS = .TRUE.
      IPASS  = 1
      IDELE  = IEST(1)
      DO 109 I = 1,6
      NL(I)  = IEST(I+1)
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
C     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK,THEY WILL BE SET EQUAL TO
C     SO ALSO FOR TEMP3 AND TEMP5
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
      MATID  = MATID2
      MATFLG = 3
      J11 = 0.0
      J12 = 0.0
      J22 = 0.0
      IF (NOTS) GO TO 146
      CALL MAT (IDELE)
  146 CONTINUE
      D11 = EM(1)
      D12 = EM(2)
      D13 = EM(3)
      D22 = EM(4)
      D23 = EM(5)
      D33 = EM(6)
C
C     CALCULATIONS FOR THE TRIANGLE
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     FILL E-MATRIX
C
      DO 177 I = 1,18
  177 E(I)  = 0.0
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
      D334  = D33*4.0
      D132  = D13*2.0
      D232  = D23*2.0
      CALL AF (F,14,A,B,C,A1,A2,A3,THK1,THK2,THK3,1)
      A1SQ  = A1*A1
      A2SQ  = A2*A2
      A3SQ  = A3*A3
      C1  = A1SQ*A1
      C2  = 3.0*A1SQ*A2
      C3  = 3.0*A1SQ*A3
      C4  = 3.0*A1*A2SQ
      C5  = 6.0*A1*A2*A3
      C6  = 3.0*A3SQ*A1
      C7  = A2SQ*A2
      C8  = 3.0*A2SQ*A3
      C9  = 3.0*A2*A3SQ
      C10 = A3*A3SQ
      CALL AF (F,14,A,B,C,AA1,AA2,AA3,TSHR1,TSHR3,TSHR5,1)
      UNIBEN = .FALSE.
      IF (ABS(A2).LE.1.0E-06 .AND. ABS(A3).LE.1.0E-06) UNIBEN = .TRUE.
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
C
C     IF NO TRANSVERSE SHEAR GO TO 113
C
      IF (NOTS) GO TO 1137
      X  = XC(I)
      Y  = YC(I)
      CALL TSPL3S (TS6)
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
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (20,QQQ,20,TS6(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
      IF (ISING .EQ. 2) GO TO 904
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      DO 152 I = 1,20
      DO 152 J = 1,18
      IJ = (I-1)*18 + J
      QQQINV(IJ) = QQQ(I,J)
  152 CONTINUE
C
C     START EXECUTION FOR STIFFNESS MATRIX CALCULATION
C
C     CM IS STIFFNESS MATRIX IN ELEMENT COORDINATES
C
  211 CONTINUE
      DO 212 I = 1,400
      KTR3(I) = 0.0
  212 CONTINUE
      DO 220 I = 1,20
      MX   = XPOWER(I)
      RMX  = MX
      NX   = YPOWER(I)
      RNX  = NX
      RMNX = RMX*RNX
      RMX1 = RMX*(RMX-1.0)
      RNX1 = RNX*(RNX-1.0)
      DO 218 J = I,20
      IJ   = (I-1)*20 + J
      JI   = (J-1)*20 + I
      MY   = XPOWER(J)
      RMY  = MY
      NY   = YPOWER(J)
      RNY  = NY
      RMNY = RMY*RNY
      RMY1 = RMY*(RMY-1.0)
      RNY1 = RNY*(RNY-1.0)
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
      IF (IPASS .EQ. 1) GO TO 214
      MX01 = MX0  + 1
      NX01 = NX0  + 1
      MX011= MX01 + 1
      NX011= NX01 + 1
      RHO  = RHOY*1.0
      MTR3(IJ) = RHO*(A1*F(MX01,NX01) + A2*F(MX011,NX01) +
     1           A3*F(MX01,NX011)) + NSM*F(MX01,NX01)
      MTR3(JI) = MTR3(IJ)
      GO TO 216
  214 CONTINUE
      ST   = 0.0
      DO 215 K = 1,10
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
      S11  = 0.0
      S22  = 0.0
      S33  = 0.0
      S13  = 0.0
      S23  = 0.0
      IF (MX3X .GT. 0) S11 = D11*RMX1*RMY1*CC(K)*F(MX3X,NY1Y)
      IF (NX3Y .GT. 0) S22 = D22*RNX1*RNY1*CC(K)*F(MY1X,NX3Y)
      IF (MX1X.GT.0 .AND. NX1Y.GT.0) S33 = (D334*RMNX*RMNY+
     1    D12*(RMX1*RNY1+RMY1*RNX1))*CC(K)*F(MX1X,NX1Y)
      IF (MX2X.GT.0 .AND. NX0Y.GT.0) S13 = D132*(RMX1*RMNY+
     1    RMNX*RMY1)*CC(K)*F(MX2X,NX0Y)
      IF (MX0X.GT.0 .AND. NX2Y.GT.0) S23 = D232*(RMNX*RNY1+
     1    RNX1*RMNY)*CC(K)*F(MX0X,NX2Y)
      ST = ST + S11 + S22 + S33 + S13 + S23
      IF (UNIBEN) GO TO 2150
  215 CONTINUE
 2150 CONTINUE
      KTR3(IJ) = ST/12.0
      KTR3(JI) = KTR3(IJ)
  216 CONTINUE
  218 CONTINUE
  220 CONTINUE
      IF (IPASS .EQ. 2) GO TO 230
C
C     IF NO TRANSVERSE SHEAR GO TO 230
C
C     IF TSHR EQUAL TO ZERO OR MATID3 EQUAL TO ZERO , SKIP THESE
C     CALCULATIONS
C
      IF (NOTS) GO TO 230
      CALL TSPL1S (TS1,TS2,TS6,TS6S,TS7,KTR3,CMT(761))
C
  230 CONTINUE
C
C     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
C
      CALL GMMATS (QQQINV,20,18,+1,KTR3,20,20,0,CMT(761))
      CALL GMMATS (CMT(761),18,20,0,QQQINV,20,18,0,CM1)
C
  290 DO 300 I = 1,1296
      CMT(I) = 0.0
  300 CONTINUE
      IF (IPASS .LE. 2) GO TO 305
C
C     LUMPED MASS MATRIX
C
      CALL AF (F,14,A,B,C,T1,T2,T3,EST(10),EST(11),EST(12),1)
      AREA  = F(1,1)
      VOL   = T1*F(1,1) + T2*F(2,1) + T3*F(1,2)
      AMASS = (RHOY*VOL+NSM*AREA)/6.
      DO 303 I = 1,1296,37
      CMT(I) = AMASS
  303 CONTINUE
      IPASS = 2
      GO TO 400
C
C     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
C     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
C     ARE REQUESTED  - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
C
C     TRANSFORM STIFFNESS MATRIX FROM ELEMENT COORDINATES TO BASIC
C     COORDINATES
C
C     TRANSFORM STIFFNESS MATRIX FROM BASIC COORDINAYES TO GLOBAL (DISP)
C     COORDINATES
C
C     INSERT THE 6X6 SUBMATRIX  INTO KGG MATRIX
C
  305 DO 310 I = 1,6
      SAVE(I) = NL(I)
  310 CONTINUE
      DO 314 I = 1,6
      SMALL(I) = I
      ISMALL = NL(I)
      DO 313 J = 1,6
      IF (ISMALL .LE.NL(J)) GO TO 312
      SMALL(I) = J
      ISMALL = NL(J)
  312 CONTINUE
  313 CONTINUE
      ISM = SMALL(I)
      NL(ISM) = 1000000
  314 CONTINUE
      DO 316 I = 1,6
      NL(I) = SAVE(I)
  316 CONTINUE
      DO 390 I  = 1,6
      DO 385 J  = I,6
      DO 320 II = 1,36
      BALOTR(II) = 0.0
      KSUP(II) = 0.0
  320 CONTINUE
      DO 324 K = 1,3
      SIL1 = SMALL(I)
      K1 = (SIL1-1)*3 + K
      DO 323 L = 1,3
      SIL2 = SMALL(J)
      L1 = (SIL2-1)*3 + L
      CSUB(K,L) = CM1(K1,L1)
  323 CONTINUE
  324 CONTINUE
      CALL GMMATS (E,6,3,0,CSUB,3,3,0,CSUBT)
      CALL GMMATS (CSUBT,6,3,0,E,6,3,+1,KSUPT)
      DO 325 K = 1,6
      DO 325 L = 1,6
      K1 = (K-1)*6 + L
      L1 = (L-1)*6 + K
      KSUP(L1) = KSUPT(K1)
  325 CONTINUE
C
C     TRANSFORM THE KSUP(36) FROM BASIC TO DISPLACEMENT COORDINATES
C
      IF (NL(SIL1).EQ.0 .OR. ICS(SIL1).EQ.0) GO TO 340
      JJ = 4*I + 20
      CALL TRANSS (IEST(JJ),TRAND)
      DO 327 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  327 CONTINUE
      CALL GMMATS (BALOTR(1),6,6,1,KSUP(1),6,6,0,KSUPT)
      DO 330 K = 1,36
      KSUP(K) = KSUPT(K)
  330 CONTINUE
  340 CONTINUE
      IF (NL(SIL2).EQ.0 .OR. ICS(SIL2).EQ.0) GO TO 375
      IF (J .EQ. I) GO TO 365
      CALL TRANSS (IEST(4*J+20),TRAND)
      DO 360 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  360 CONTINUE
  365 CONTINUE
      CALL GMMATS (KSUP(1),6,6,0,BALOTR( 1 ),6,6,0,KSUPT)
      DO 370 K = 1,36
      KSUP(K) = KSUPT(K)
  370 CONTINUE
  375 CONTINUE
      DO 380 II = 1,6
      DO 380 JJ = 1,6
      I1   = (I-1)*6 + II
      J1   = (J-1)*6 + JJ
      I1J1 = (I1-1)*36 + J1
      J1I1 = (J1-1)*36 + I1
      CMT(I1J1) = KSUB(JJ,II)
      CMT(J1I1) = KSUB(JJ,II)
  380 CONTINUE
  385 CONTINUE
  390 CONTINUE
C
C     CALL INSERTION ROUTINE
C
  400 CALL EMGOUT (CMT(1),CMT(1),1296,1,DICT,IPASS,IPREC)
      IF (.NOT.IMASS .OR. IPASS.GE.2) RETURN
C
C     GO TO 290 TO COMPUTE LUMPED MASS MATRIX
C     GO TO 211 TO COMPUTE CONSIST. MASS MATRIX (THIS PATH MAY NOT WORK)
C
      IPASS = 3
      CALL SSWTCH (46,J)
      IF (J .EQ. 1) IPASS = 2
      GO TO (999,211,290), IPASS
C
C     ERRORS
C
  904 CONTINUE
      NOGO = .TRUE.
      WRITE  (IOUTPT,2411) UFM,IEST(1)
 2411 FORMAT (A23,' 2411, MATRIX RELATING GENERALIZED PARAMETERS AND ',
     1       'GRID POINT DISPLACEMENTS IS SINGULAR.', //26X,
     2       'CHECK COORDINATES OF ELEMENT  TRPLT1 WITH ID',I9,1H.)
  999 CONTINUE
      RETURN
      END
