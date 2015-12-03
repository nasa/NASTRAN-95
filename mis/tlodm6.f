      SUBROUTINE TLODM6 (TI)
C
C     THERMAL LOAD VECTOR FOR TRIM6 (LINEAR STRAIN MEMBRANE TRIANGLE)
C     ELEMENT
C
C     EST ENTRIES
C
C     EST ( 1) = ELEMENT ID                              INTEGER
C     EST ( 2) = SCALAR INDEX NUMBER FOR GRID POINT 1    INTEGER
C     EST ( 3) = SCALAR INDEX NUMBER FOR GRID POINT 2    INTEGER
C     EST ( 4) = SCALAR INDEX NUMBER FOR GRID POINT 3    INTEGER
C     EST ( 5) = SCALAR INDEX NUMBER FOR GRID POINT 4    INTEGER
C     EST ( 6) = SCALAR INDEX NUMBER FOR GRID POINT 5    INTEGER
C     EST ( 7) = SCALAR INDEX NUMBER FOR GRID POINT 6    INTEGER
C     EST ( 8) = THETA                                   REAL
C     EST ( 9) = MATERIAL IDENTIFICATION NUMBER          INTEGER
C     EST (10) = THICKNESS T1 AT GRID POINT 1            REAL
C     EST (11) = THICKNESS T3 AT GRID POINT 3            REAL
C     EST (12) = THICKNESS T5 AT GRID POINT 5            REAL
C     EST (13) = NON-STRUCTURAL MASS                     REAL
C     X1,Y1,Z1 FOR ALL SIX POINTS ARE IN NASTRAN BASIC SYSTEM
C
C     EST (14) = CO-ORDINATE SYSTEM ID FOR GRID POINT 1  INTEGER
C     EST (15) = CO-ORDINATE X1                          REAL
C     EST (16) = CO-ORDINATE Y1                          REAL
C     EST (17) = CO-ORDINATE Z1                          REAL
C     EST (18) = CO-ORDINATE SYSTEM ID FOR GRID POINT 2  INTEGER
C     EST (19) = CO-ORDINATE X2                          REAL
C     EST (20) = CO-ORDINATE Y2                          REAL
C     EST (21) = CO-ORDINATE Z2                          REAL
C     EST (22) = CO-ORDINATE SYSTEM ID FOR GRID POINT 3  INTEGER
C     EST (23) = CO-ORDINATE X3                          REAL
C     EST (24) = CO-ORDINATE Y3                          REAL
C     EST (25) = CO-ORDINATE Z3                          REAL
C     EST (26) = CO-ORDINATE SYSTEM ID FOR GRID POINT 4  INTEGER
C     EST (27) = CO-ORDINATE X4                          REAL
C     EST (28) = CO-ORDINATE Y4                          REAL
C     EST (29) = CO-ORDINATE Z4                          REAL
C     EST (30) = CO-ORDINATE SYSTEM ID FOR GRID POINT 5  INTEGER
C     EST (31) = CO-ORDINATE X5                          REAL
C     EST (32) = CO-ORDINATE Y5                          REAL
C     EST (33) = CO-ORDINATE Z5                          REAL
C     EST (34) = CO-ORDINATE SYSTEM ID FOR GRID POINT 6  INTEGER
C     EST (35) = CO-ORDINATE X6                          REAL
C     EST (36) = CO-ORDINATE Y6                          REAL
C     EST (37) = CO-ORDINATE Z6                          REAL
C     EST (38) TO EST (43)  -  ELEMENT TEMPERATURES AT SIX GRID POINTS
C
      LOGICAL         UNIMEM,   UNITEM
      REAL            IVECT(3), JVECT(3), KVECT(3), CC(3),    DD(3),
     1                G(9),     G1(3),    NAME(2),  F(5,5),   NSM,
     2                XC(6),    YC(6),    ZC(6),    Q(6,6),   E(6),
     3                TRANS(9), QINV(36), PTEM(12), PTELE(12),PTGLB(18),
     4                PSUB(2),  PSUBT(3), PSUBT1(3),TI(6)
      INTEGER         XU(12),   YU(12),   XV(12),   YV(12),   SIL(6),
     1                SIL1,     RK(3),    SK(3),    TL(3),    UL(3),
     2                IND(6,3), ICS(6),   IEST(45), NL(6)
      COMMON /TRIMEX/ EST(100)
      COMMON /ZZZZZZ/ PG(1)
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN / MATID,MATFLG,ELTEMP,PLA34,SINTH,COSTH
      COMMON /MATOUT/ EM(6),RHOY,ALF(3),TREF,GSUBE,SIGTY,SIGCY,SIGSY,
     1                RJ11,RJ12,RJ22
C
C     EQUIVALENCE  IEST WITH EST IN COMMON BLOCK /EMGEST/ SINCE EST IS
C     A MIXED INTEGER AND REAL ARRAY
C
      EQUIVALENCE   (IEST(1),EST(1))
      EQUIVALENCE   (A,DISTA),(B,DISTB),(C,DISTC),(CC(1),C1),(CC(2),C2),
     1              (CC(3),C3),(DD(1),D1),(DD(2),D2),(DD(3),D3)
      DATA   XU   / 0,1,0,2,1,0,6*0/ ,   YU / 0,0,1,0,1,2,6*0/
      DATA   XV   / 6*0,0,1,0,2,1,0/ ,   YV / 6*0,0,0,1,0,1,2/
      DATA   RK   / 0,1,0          / ,   SK / 0,0,1          /
      DATA   TL   / 0,1,0          / ,   UL / 0,0,1          /
      DATA   BLANK/ 4H             / ,  NAME/ 4HTRIM, 4H6    /
      DATA   DEGRA/ 0.0174532925   /
C
C     ALLOCATE EST VALUES TO RESPECTIVE  LOCAL  VARIABLES
C
      IDELE = IEST(1)
      DO 10 I = 1,6
      NL(I)  = IEST(I+1)
   10 CONTINUE
      THETAM = EST(8)
      MATID1 = IEST(9)
      TMEM1  = EST(10)
      TMEM3  = EST(11)
      TMEM5  = EST(12)
C
C     IF TMEM3 OR TMEM5 IS 0.0 OR BLANK,IT WILL BE SET EQUAL TO TMEM1
C
      IF (TMEM3.EQ.0.0 .OR. TMEM3.EQ.BLANK) TMEM3 = TMEM1
      IF (TMEM5.EQ.0.0 .OR. TMEM5.EQ.BLANK) TMEM5 = TMEM1
C
      NSM = EST(13)
C
      J = 0
      DO 20 I = 14,34,4
      J = J + 1
      ICS(J) = IEST(I )
      XC (J) = EST(I+1)
      YC (J) = EST(I+2)
      ZC (J) = EST(I+3)
   20 CONTINUE
C
C     TEMPERATURE AT THE THREE GRID POINTS ARE  DENOTED BY TO1,TO3 AND
C     TO5
C
      TO1 = TI(1)
      TO3 = TI(3)
      TO5 = TI(5)
C
      ELTEMP = (EST(38)+EST(39)+EST(40)+EST(41)+EST(42)+EST(43))/6.0
      THETA1 = THETAM*DEGRA
      SINTH  = SIN(THETA1)
      COSTH  = COS(THETA1)
      IF (ABS(SINTH) .LE. 1.0E-06) SINTH = 0.0
C
C     CALCULATIONS FOR THE  TRIANGLE
C
      CALL TRIF (XC,YC,ZC,IVECT,JVECT,KVECT,A,B,C,IEST(1),NAME)
C
C     COMPUTE THE AREA INTEGRATION FUNCTION F, AND
C     EVALUATE THE CONSTANTS C1,C2,AND C3 IN THE LINEAR EQUATION FOR
C     THICKNESS VARIATION
C
      CALL AF (F,5,A,B,C,C1,C2,C3,TMEM1,TMEM3,TMEM5,0)
      UNIMEM = .FALSE.
      IF (ABS(C2).LE.1.0E-06 .AND. ABS(C3).LE.1.0E-06) UNIMEM = .TRUE.
C
C     CALCULATIONS FOR  Q MATRIX AND ITS INVERSE
C
      DO 30 I = 1,6
      DO 30 J = 1,6
      Q(I,J) = 0.0
   30 CONTINUE
      DO 40 I = 1,6
      Q(I,1) = 1.0
      Q(I,2) = XC(I)
      Q(I,3) = YC(I)
      Q(I,4) = XC(I)*XC(I)
      Q(I,5) = XC(I)*YC(I)
      Q(I,6) = YC(I)*YC(I)
   40 CONTINUE
C
C     FIND INVERSE OF Q MATRIX
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (6,Q,6,QINV(1),0,DETERM,ISING,IND)
C
C     ISING EQUAL TO 2 IMPLIES THAT Q MATRIX IS SINGULAR
C
C     EVALUATE  MATERIAL PROPERTIES AND FILL IN G MATRIX
C
      MATFLG = 2
      MATID  = MATID1
      CALL MAT (IDELE)
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
C     G1 IS G TIMES ALFA
C
      CALL GMMATS (G,3,3,0,ALF,3,1,0,G1)
C
C     CALCULATION OF THERMAL LOAD VECTOR
C
C     EVALUATE THE CONSTANTS D1,D2,D3 IN THE LINEAR EQUATION FOR
C     TEMPERATURE VARIATION OVER THE ELEMENT
C
      T1BAR = TO1 - TREF
      T3BAR = TO3 - TREF
      T5BAR = TO5 - TREF
C
      CALL AF (F,5,A,B,C,D1,D2,D3,T1BAR,T2BAR,T3BAR,1)
      UNITEM = .FALSE.
      IF (ABS(D2).LE.1.0E-06 .AND. ABS(D3).LE.1.0E-06) UNITEM = .TRUE.
      DO 90 I = 1,12
      IX  = XU(I)
      RIX = IX
      JX  = YU(I)
      RJX = JX
      KX  = XV(I)
      RKX = KX
      LX  = YV(I)
      RLX = LX
      PTEMP = 0.0
      DO 70 K = 1,3
      IXR = IX + RK(K)
      JXS = JX + SK(K)
      KXR = KX + RK(K)
      LXS = LX + SK(K)
      DO 50 L = 1,3
      IXRT  = IXR + TL(L)
      JXSU1 = JXS + UL(L) + 1
      KXRT1 = KXR + TL(L) + 1
      LXSU  = LXS + UL(L)
      IXRT1 = IXRT+ 1
      JXSU  = JXSU1 - 1
      KXRT  = KXRT1 - 1
      LXSU1 = LXSU + 1
      IF (IXRT .GT.0) PTEMP = PTEMP+CC(K)*DD(L)*G1(1)*RIX*F(IXRT ,JXSU1)
      IF (LXSU .GT.0) PTEMP = PTEMP+CC(K)*DD(L)*G1(2)*RLX*F(KXRT1,LXSU )
      IF (JXSU .GT.0) PTEMP = PTEMP+CC(K)*DD(L)*G1(3)*RJX*F(IXRT1,JXSU )
      IF (KXRT .GT.0) PTEMP = PTEMP+CC(K)*DD(L)*G1(3)*RKX*F(KXRT ,LXSU1)
      IF (UNITEM) GO TO 60
   50 CONTINUE
   60 CONTINUE
      IF (UNIMEM) GO TO 80
   70 CONTINUE
   80 CONTINUE
      PTEM(I) = PTEMP
   90 CONTINUE
C
      CALL GMMATS (Q,6,6,0,PTEM(1),6,1,0,PTELE(1))
      CALL GMMATS (Q,6,6,0,PTEM(7),6,1,0,PTELE(7))
C
C     REORDER THE THERMAL LOAD VEC SO THAT THE DISPLACEMENTS OF A GRID
C     POINT ARE ARRANGED CONSECUTIVELY
C
      DO 110 K = 1,6
      DO 100 I = 1,2
      K1 = 6*(I-1) + K
      I1 = 2*(K-1) + I
      PTEM(I1) = PTELE(K1)
  100 CONTINUE
  110 CONTINUE
C
C     TRANSFORM THE THERMAL LOAD VECTOR PTEM FROM ELEMENT CO-ORDINATES
C     TO BASIC CO-ORDINATES
C
      E(1) = IVECT(1)
      E(2) = JVECT(1)
      E(3) = IVECT(2)
      E(4) = JVECT(2)
      E(5) = IVECT(3)
      E(6) = JVECT(3)
      DO 120 I = 1,18
      PTGLB(I) = 0.0
  120 CONTINUE
      DO 130 I = 1,6
      SIL(I) = I
  130 CONTINUE
      DO 200 I = 1,6
      SIL1 = SIL(I)
      DO 140 K = 1,2
      K1 = (SIL1-1)*2 + K
      PSUB(K) = PTEM(K1)
  140 CONTINUE
      CALL GMMATS (E,3,2,0,PSUB,2,1,0,PSUBT)
C
C     TRANSFORM THE PSUBT ROM BASIC TO GLOBAL CO-ORDINATES
C
      IF (NL(SIL1).EQ.0 .OR. ICS(SIL1).EQ.0) GO TO 160
      K = 4*SIL1 + 10
      CALL TRANSS (IEST(K),TRANS)
      CALL GMMATS (TRANS(1),3,3,1,PSUBT,3,1,0,PSUBT1)
      DO 150 K = 1,3
      PSUBT(K) = PSUBT1(K)
  150 CONTINUE
  160 CONTINUE
C
C     INSERT PTGLB IN GLOBAL LOAD VECTOR PG
C
      DO 180 II = 1,3
      I1 = (I-1)*3 + II
      I2 = IEST(I+1) + II - 1
      PTGLB(I1) = PSUBT(II)
      PG(I2) = PG(I2) + PSUBT(II)
  180 CONTINUE
  200 CONTINUE
      RETURN
      END
