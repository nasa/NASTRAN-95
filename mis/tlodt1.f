      SUBROUTINE TLODT1 (TREAL,TINT)
C
C     THERMAL LOAD VECTOR FOR TRPLT1 (HIGHER ORDER PLATE BENDING ELEMENT
C
C     ECPT ENTRIES
C     AS IN STIFFNESS ROUTINE KTRPL1
C
      LOGICAL         NOGO,NOTS,UNIBEN,UNITEM
      INTEGER         XPOWER(20),YPOWER(20),XTHK(10),YTHK(10),PT(3),
     1                QT(3),SIL(6),SIL1,TINT(6)
      REAL            IVECT,JVECT,KVECT
      DIMENSION       F(10,10),XC(6),YC(6),ZC(6),QQQ(20,20),QQINV(360),
     1                TS1(60),TS2(60),IEST(42),TREAL(6),TRAND(9),DD(3),
     2                ICS(6),GE1(9),NAM(2)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SSGWRK/ X,Y,Z,DISTA,DISTB,DISTC,A1,A2,A3,B1,B2,B3,G1(3),
     1                D(3),E(18),IVECT(3),JVECT(3),KVECT(3),CC(10),G(9),
     2                PTEM(20),PTELE(18),PTGLB(36),PSUB(3),PSUBT(6),
     3                PSUBT1(6),TS6(40),NAME(2),INDEX(20,3),NL(6),TL(3),
     4                BALOTR(36)
      COMMON /SYSTEM/ SYSBUF,IOUT
      COMMON /TRIMEX/ EST(100)
      COMMON /ZZZZZZ/ PG(1)
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN / MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/ EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                RJ11,RJ12,RJ22
C
C     EQUIVALENCE IECPT WITH ECPT IN COMMON BLOCK /SMA1ET/ SINCE ECPT IS
C     A MIXED INTEGER AND REAL ARRAY
C
      EQUIVALENCE     (THK1,TMEM1),(THK2,TMEM3),(THK3,TMEM5),
     1                (A,DISTA),(B,DISTB),(C,DISTC),(IEST(1),EST(1)),
     2                (C1,CC(1)),(C2,CC(2)),(C3,CC(3)),(C4,CC(4)),
     3                (C5,CC(5)),(C6,CC(6)),(C7,CC(7)),(C8,CC(8)),
     4                (C9,CC(9)),(C10,CC(10)),(D(1),D1),(D(2),D2),
     5                (D(3),D3),(DD(1),D(1))
      DATA    BLANK , NAM  / 4H    , 4HTRPL, 4HT1    /
      DATA    XPOWER/ 0,1,0,2,1,0,3,2,1,0,4,3,2,1,0,5,3,2,1,0/
      DATA    YPOWER/ 0,0,1,0,1,2,0,1,2,3,0,1,2,3,4,0,2,3,4,5/
      DATA    XTHK  / 0,1,0,2,1,0,3,2,1,0 /
      DATA    YTHK  / 0,0,1,0,1,2,0,1,2,3 /
      DATA    PT    / 0,1,0 /,  QT / 0,0,1/
      DATA    DEGRA / 0.0174532925 /
C
C
      NOTS  = .FALSE.
      IDELE = IEST(1)
      DO 109 I = 1,6
      NL(I) = IEST(I+1)
  109 CONTINUE
      THETAM = EST(8)
      MATID1 = IEST(9)
      TMEM1  = (EST(10)*12.0)**0.333333333333
      TMEM3  = (EST(11)*12.0)**0.333333333333
      TMEM5  = (EST(12)*12.0)**0.333333333333
      TSHR1  = EST(14)
      TSHR3  = EST(15)
      TSHR5  = EST(16)
      J      = 0
      DO 120 I = 24,44,4
      J      = J + 1
      ICS(J) = IEST(I)
      XC(J)  = EST(I+1)
      YC(J)  = EST(I+2)
      ZC(J)  = EST(I+3)
  120 CONTINUE
      TEMP1  = TREAL(1)
      TEMP3  = TREAL(1)
      TEMP5  = TREAL(1)
      T1PRIM =-TREAL(2)
      T3PRIM =-TREAL(2)
      T5PRIM =-TREAL(2)
C
C     IF TMEM3 OR TMEM5 EQUAL TO ZERO OR BLANK,THEY WILL BE SET EQUAL TO
C     SO ALSO FOR TEMP3 AND TEMP5
C
      IF (TMEM3.EQ.0.0 .OR. TMEM3 .EQ.BLANK) TMEM3  = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5 .EQ.BLANK) TMEM5  = TMEM1
      IF (TEMP3.EQ.0.0 .OR. TEMP3 .EQ.BLANK) TEMP3  = TEMP1
      IF (TEMP5.EQ.0.0 .OR. TEMP5 .EQ.BLANK) TEMP5  = TEMP1
      IF (T3PRIM.EQ..0 .OR. T3PRIM.EQ.BLANK) T3PRIM = T1PRIM
      IF (T5PRIM.EQ..0 .OR. T5PRIM.EQ.BLANK) T5PRIM = T1PRIM
      IF (TSHR3.EQ.0.0 .OR. TSHR3 .EQ.BLANK) TSHR3  = TSHR1
      IF (TSHR5.EQ.0.0 .OR. TSHR5 .EQ.BLANK) TSHR5  = TSHR1
      ELTEMP = EST(48)
      AVTHK  = (TMEM1+TMEM3+TMEM5)/3.0
      AVINER = AVTHK**3/12.0
      IF (TSHR1 .EQ. 0.0) NOTS = .TRUE.
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     EVALUATE MATERIAL PROPERTIES
C
      MATFLG = 2
      MATID = MATID1
      CALL MAT (IDELE)
C
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
C     IF TINT(6).NE.1,G1 IS G AND T1PRIME IS ALPHA TIMES T1PRIME
C     IF TINT(6).EQ.1,G1 IS G TIMES ALPHA AND T1PRIME IS T1PRIME
C
      IF (TINT(6) .NE. 1) GO TO 147
C
C     G1 IS G TIMES ALPHA
C
      CALL GMMATS (G,3,3,0, ALF,3,1,0, G1)
      GO TO 149
  147 CONTINUE
      DO 148 I = 1,9
  148 GE1(I) = G(I)*AVINER
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,GE1(1),3,TS1(1),0,DETERM,ISING,INDEX)
      IF (ISING .EQ. 2) GO TO 901
      CALL GMMATS (GE1,3,3,0, TREAL(2),3,1,0, TL(1))
C
C     CALCULATIONS FOR THE TRIANGLE
C
  149 CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAM )
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
C     EVALUATE CONSTANTS D1,D2,D3 IN THE LINEAR EQUATION FOR TEMPERATURE
C     GRADIENT VARIATION OVER THE ELEMENT
C
      CALL AF (F,10,A,B,C,D1,D2,D3,THK1,THK2,THK3,1)
      UNITEM = .FALSE.
      IF (ABS(D2).LE.1.0E-06 .AND. ABS(D3).LE.1.0E-06) UNITEM =.TRUE.
C
      DISTAB = DISTA + DISTB
      A1   = (THK1*DISTA+THK2*DISTB)/DISTAB
      A2   = (THK2-THK1)/DISTAB
      A3   = (THK3-A1)/DISTC
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
      CALL AF (F,10,A,B,C,B1,B2,B3,TSHR1,TSHR3,TSHR5,1)
      UNIBEN =.FALSE.
      IF (ABS(A2).LE.1.0E-06 .AND. ABS(A3).LE.1.0E-06) UNIBEN =.TRUE.
C
C     COMPUTE THE AREA INTEGRATION FUNCTION F
C
      CALL AF (F,10,A,B,C,0,0,0,0,0,0,-1)
C
C     CALCULATIONS FOR QMATRIX (QQQ) AND ITS INVERSE
C
      DO 110 I = 1,400
  110 QQQ(I,1) = 0.0
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
      IF (NOTS) GO TO 115
      X = XC(I)
      Y = YC(I)
      CALL TLODT3 (TS6,NOTS)
      DO 113 JJ = 1,20
      QQQ(I2,JJ) = QQQ(I2,JJ) - TS6(20+JJ)
      QQQ(I3,JJ) = QQQ(I3,JJ) + TS6(   JJ)
  113 CONTINUE
  115 CONTINUE
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
C
C     FOURTH ARGUMENT IS A DUMMY LOCATION FOR INVERSE AND HENCE TS1(1)
C     IS U
C
C     AGAIN SET ISING = -1
C
      ISING = -1
      CALL  INVERS (20,QQQ,20,TS1(1),0,DETERM,ISING,INDEX)
C
C     ISING EQUAL TO 2 IMPLIES THAT QQQ IS SINGULAR
C
C     FIRST 18 COLUMNS OF QQQ INVERSE IS THE QQQINV FOR USE IN STIFFNESS
C     MATRIX CALCULATIONS
C
      DO 152 I = 1,20
      DO 152 J = 1,18
      IJ = (I-1)*18 + J
      QQINV (IJ) = QQQ(I,J)
  152 CONTINUE
C
      DO 220 I = 1,20
      MX   = XPOWER(I)
      RMX  = MX
      NX   = YPOWER(I)
      RNX  = NX
      RMNX = RMX*RNX
      RMX1 = RMX*(RMX-1.0D0)
      RNX1 = RNX*(RNX-1.0D0)
      PTEMP= 0.0
      MX01 = MX - 1
      MX1  = MX + 1
      NX01 = NX - 1
      NX1  = NX + 1
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
     1    PTEMP = PTEMP+CC(K)*DD(L)*G1(1)*RMX1*F(MX01XP,NX1YQ)
      IF (MX1XP.GT.0 .AND. NX01YQ.GT.0)
     1    PTEMP = PTEMP+CC(K)*DD(L)*G1(2)*RNX1*F(MX1XP,NX01YQ)
      IF (MXXP.GT.0 .AND. NXYQ.GT.0)
     1    PTEMP = PTEMP+CC(K)*DD(L)*G1(3)*RMNX*F(MXXP,NXYQ)
      IF (UNITEM) GO TO 213
  212 CONTINUE
C
  213 IF (TINT(6) .EQ. 1) GO TO 214
      IF (MX01X .GT. 0) PTEMP = PTEMP + CC(K)*RMX1*(TL(1)*G(1)
     1                        + TL(2)*G(2) + TL(3)*G(3))*F(MX01X,NX1Y)
      IF (NX01Y .GT. 0) PTEMP = PTEMP + CC(K)*RNX1*(TL(1)*G(4)
     1                        + TL(2)*G(5) + TL(3)*G(6))*F(MX1X,NX01Y)
      IF (MXX.GT.0 .AND. NXY.GT.0) PTEMP = PTEMP + CC(K)*RMNX*
     1                   (TL(1)*G(7)+TL(2)*G(8)+TL(3)*G(9))*F(MXX,NXY)
  214 IF (UNIBEN) GO TO 216
  215 CONTINUE
C
  216 PTEM(I) = PTEMP/12.0
  220 CONTINUE
C
C     IF NO TRANSVERSE SHEAR GO TO 230
C
C     IF TSHR EQUAL TO ZERO OR MATID3 EQUAL TO ZERO, SKIP THESE
C     CALCULATIONS
C
      IF (NOTS) GO TO 230
C
      CALL TLODT2 (TS1,TS2)
      DO 226 I = 1,20
      PTEM(I) = PTEM(I) + TS2(I)
  226 CONTINUE
C
C     (QQQINV) TRANSPOSE (KTR3)  (QQQINV)
C
  230 CALL  GMMATS (QQINV,20,18,+1, PTEM,20,1,0, PTELE)
C
C     LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL (THAT IS
C     COORDINATE AT ANY GRID POINT IN WHICH DISPLACEMENT AND STRESSES
C     ARE R - NOT NEEDED IF FIELD 7 IN GRID CARD IS ZERO)
C
      DO 301 I = 1,36
      PTGLB(I) = 0.0
  301 CONTINUE
      DO 302 I = 1,6
      SIL(I) = I
  302 CONTINUE
      DO 380 I = 1,6
      DO 310 II = 1,36
      BALOTR(II) = 0.0D0
  310 CONTINUE
      SIL1 = SIL(I)
      DO 304 K = 1,3
      K1 = (SIL1-1)*3 + K
      PSUB(K) = PTELE(K1)
  304 CONTINUE
      CALL GMMATS (E,6,3,0, PSUB,3,1,0, PSUBT)
C
C     TRANSFORM THE PSUBT(6) FROM BASIC TO DISPLACEMENT COORDINATES
C
      IF (NL(I).EQ.0 .OR. ICS(I).EQ.0) GO TO 330
      JJ = 4*I + 20
      CALL TRANSS (IEST(JJ),TRAND)
      DO 320 JJ = 1,3
      L = 6*(JJ-1) + 1
      M = 3*(JJ-1) + 1
      BALOTR(L   ) = TRAND(M  )
      BALOTR(L+1 ) = TRAND(M+1)
      BALOTR(L+2 ) = TRAND(M+2)
      BALOTR(L+21) = TRAND(M  )
      BALOTR(L+22) = TRAND(M+1)
      BALOTR(L+23) = TRAND(M+2)
  320 CONTINUE
      CALL GMMATS (BALOTR(1),6,6,1, PSUBT,6,1,0, PSUBT1)
      DO 350 K = 1,6
      PSUBT(K) = PSUBT1(K)
  350 CONTINUE
C
C     INSERT PTGLB IN PG
C
  330 DO 370 II = 1,6
      I1 = (I-1)*6 + II
      I2 = IEST(I+1) + II - 1
      PTGLB(I1) = PSUBT(II)
      PG(I2) = PG(I2) + PSUBT(II)
  370 CONTINUE
  380 CONTINUE
      GO TO 999
C
  901 WRITE  (IOUT,905) UFM,IEST(1)
  905 FORMAT (A23,' 2412, A SINGULAR MATERIAL MATRIX FOR ELEMENT ID =',
     1       I9,' HAS BEEN DETECTED BY SUBROUTINE TLODT1', /26X,'WHILE',
     2       ' TRYING TO COMPUTE THERMAL LOADS WITH TEMPP2 CARD DATA.')
      NOGO=.TRUE.
  999 RETURN
      END
