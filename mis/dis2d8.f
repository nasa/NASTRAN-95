      SUBROUTINE DIS2D8
C
C     2-D, 8 GRID POINT ISOPARAMETRIC STRUCTURAL ELEMENT
C     DIFFERENTIAL STIFFNESS MATRIX ROUTINE
C
      REAL             KX,KY
      DOUBLE PRECISION KIJ,G,B,XI,ETA,DNXI,DNETA,XX,TB,DNL,PT,H,XJB,
     1                 XXJB,DETERM,DNC,GSUBE,DUMARG,
     2                 BT,TEMPAR,TEMP,DNX,DNY,SAVE,TSAVE,
     3                 KWD(36),CID(18),CJD(18),KMULT(18),DHH,
     4                 THICK,PSTMUL(9),PREMUL(9),E1T
      DIMENSION        SIG(3),ALPHAS(3),ST(3),SEMP(9),TTB(9),STB(9),
     1                 BB(72),DB(72),S(6),R(9),SE1T(6),DN(8),
     2                 G(9),QQ(15),XI(8),ETA(8),TB(9),XY1(3),XY2(3),
     3                 B(12),BT(12),ECPT(1),TEMP(9),TEMPAR(1),DNX(1),
     4                 DNY(1),DNXI(1),DNETA(1),SAVE(72),TSAVE(72),
     5                 VEC(3),VVEC(3),VECI(3),VECJ(3),VECK(3),E1T(9),
     6                 IWS(2,3)
      COMMON /DS1AAA/  NPVT,ICSTM,NCSTM
      COMMON /DS1AET/  NECPT(1),NGRID(8),ID1,TH,MATID1,T,ISYS1,X1,Y1,Z1,
     1                 ISYS2,X2,Y2,Z2,ISYS3,X3,Y3,Z3,ISYS4,X4,Y4,Z4,
     2                 ISYS5,X5,Y5,Z5,ISYS6,X6,Y6,Z6,ISYS7,X7,Y7,Z7,
     3                 ISYS8,X8,Y8,Z8,TTEMP,EDT,ISETNO,TGRID(8),DISP(24)
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                 TREF,GE,KX,KY,C
      COMMON /DS1ADP/  KIJ(36),XX(16),DNC(16),DNL(16),XXJB(2,2),XJB(4),
     1                 PT(3),H(3),G,B,BT,TB,DETERM,GSUBE,DUMARG,TSAVE
      EQUIVALENCE      (ALPHAS(1),ALPHA1),
     1                 (ECPT(1),NECPT(1)),(TEMP(1),B(1)),
     2                 (DNC(1),DNXI(1)),(DNC(9),DNETA(1)),
     3                 (DNL(1),DNX(1)),(DNL(9),DNY(1)),(QQ(1),G11),
     4                 (TEMPAR(1),BT(1)),(XY1(1),X1),(XY2(1),X2)
      DATA     XI   /  -1.D0, 1.D0, 1.D0,-1.D0, 0.D0, 1.D0, 0.D0,-1.D0/
      DATA     ETA  /  -1.D0,-1.D0, 1.D0, 1.D0,-1.D0, 0.D0, 1.D0, 0.D0/
C
C     ECPT LIST
C                                      IN
C                                      THIS
C     ECPT       DESCRIPTION           ROUTINE        TYPE
C     ******************************************************************
C     ECPT( 1) = ELEMENT ID            NECPT(1)       INTEGER
C     ECPT( 2) = GRID POINT 1          NGRID(1)       INTEGER
C     ECPT( 3) = GRID POINT 2          NGRID(2)       INTEGER
C     ECPT( 4) = GRID POINT 3          NGRID(3)       INTEGER
C     ECPT( 5) = GRID POINT 4          NGRID(4)       INTEGER
C     ECPT( 6) = GRID POINT 5          NGRID(5)       INTEGER
C     ECPT( 7) = GRID POINT 6          NGRID(6)       INTEGER
C     ECPT( 8) = GRID POINT 7          NGRID(7)       INTEGER
C     ECPT( 9) = GRID POINT 8          NGRID(8)       INTEGER
C     ECPT(10) = COORD SYS ID-STRESS   ID1            INTEGER
C     ECPT(11) = ANIS. MATERIAL ANGLE  TH             REAL
C     ECPT(12) = MATERIAL ID           MATID1         INTEGER
C     ECPT(13) = THICKNESS             T              REAL
C     ECPT(14) = COORD SYS ID 1        ISYS1          INTEGER
C     ECPT(15) = X1                    X1             REAL
C     ECPT(16) = Y1                    Y1             REAL
C     ECPT(17) = Z1                    Z1             REAL
C     ECPT(18) = COORD SYS ID 2        ISYS2          INTEGER
C     ECPT(19) = X2                    X2             REAL
C     ECPT(20) = Y2                    Y2             REAL
C     ECPT(21) = Z2                    Z2             REAL
C     ECPT(22) = COORD SYS ID 3        ISYS3          INTEGER
C     ECPT(23) = X3                    X3             REAL
C     ECPT(24) = Y3                    Y3             REAL
C     ECPT(25) = Z3                    Z3             REAL
C     ECPT(26) = COORD SYS ID 4        ISYS4          INTEGER
C     ECPT(27) = X4                    X4             REAL
C     ECPT(28) = Y4                    Y4             REAL
C     ECPT(29) = Z4                    Z4             REAL
C     ECPT(30) = COORD SYS ID 5        ISYS5          INTEGER
C     ECPT(31) = X5                    X5             REAL
C     ECPT(32) = Y5                    Y5             REAL
C     ECPT(33) = Z5                    Z5             REAL
C     ECPT(34) = COORD SYS ID 6        ISYS6          INTEGER
C     ECPT(35) = X6                    XL             REAL
C     ECPT(36) = Y6                    Y6             REAL
C     ECPT(37) = Z6                    Z6             REAL
C     ECPT(38) = COORD SYS ID 7        ISYS7          INTEGER
C     ECPT(39) = X7                    X7             REAL
C     ECPT(40) = Y7                    Y7             REAL
C     ECPT(41) = Z7                    Z7             REAL
C     ECPT(42) = COORD SYS ID 8        ISYS8          INTEGER
C     ECPT(43) = X8                    X8             REAL
C     ECPT(44) = Y8                    Y8             REAL
C     ECPT(45) = Z8                    Z8             REAL
C     ECPT(46) = ELEMENT TEMP          TTEMP          REAL
C     ECPT(47) = 0.                    EDT            REAL
C     ECPT(48) = TEMPERATURE SET       ISETNO         INTEGER
C     ECPT(49) = *
C     ECPT(. ) = *  GRID POINT TEMPERATURES
C     ECPT(56) = *
C     ECPT(57) = *
C     ECPT(. ) = *  TRANSLATIONAL DOF-S OF GRIDS FOR THIS ELEMENT
C     ECPT(80) = *
C
C
C     TEST FOR PIVOT POINT
C
      DO 10 KK = 1,8
      IF (NGRID(KK) .EQ. NPVT) GO TO 20
   10 CONTINUE
C
C     IF FALL HERE NO ELEMENT GRID POINT IS THE PIVOT POINT
C
      CALL MESAGE (-30,34,ECPT(1))
C
C     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
C
   20 DO 30 I = 1,3
      VECI(I) = XY2(I) - XY1(I)
   30 CONTINUE
      VECIL = SQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      IF (VECIL .EQ. 0.0) GO TO 60
      VECI(1) = VECI(1)/VECIL
      VECI(2) = VECI(2)/VECIL
      VECI(3) = VECI(3)/VECIL
C
C     K VECTOR IS OBTAINED BY CROSSING I INTO VECTOR FROM GRID PT. 1 TO
C     GRID
C
      VECK(1) = VECI(2)*(Z4-Z1) - VECI(3)*(Y4-Y1)
      VECK(2) = VECI(3)*(X4-X1) - VECI(1)*(Z4-Z1)
      VECK(3) = VECI(1)*(Y4-Y1) - VECI(2)*(X4-X1)
      VECKL=SQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      IF (VECKL .EQ. 0.0) GO TO 60
      VECK(1) = VECK(1)/VECKL
      VECK(2) = VECK(2)/VECKL
      VECK(3) = VECK(3)/VECKL
C
C     J VECTOR IS OBTAINED BY CROSSING K INTO I
C
      VECJ(1) = VECK(2)*VECI(3) - VECK(3)*VECI(2)
      VECJ(2) = VECK(3)*VECI(1) - VECK(1)*VECI(3)
      VECJ(3) = VECK(1)*VECI(2) - VECK(2)*VECI(1)
C
      E1T(1) = VECI(1)
      E1T(2) = VECI(2)
      E1T(3) = VECI(3)
      E1T(4) = VECJ(1)
      E1T(5) = VECJ(2)
      E1T(6) = VECJ(3)
      E1T(7) = VECK(1)
      E1T(8) = VECK(2)
      E1T(9) = VECK(3)
      DO 40 I = 1,6
   40 SE1T(I) = E1T(I)
C
C     STORE ELEMENT COORDS FOR GRIDS 1 AND 2
C
      XX(1) = 0.
      XX(2) = 0.
      XX(3) = VECIL
      XX(4) = 0.
C
C     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
C     FROM GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD.
C     IS THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO
C     THE GRID POINT.
C
      DO 50 I = 3,8
      IXX  = 2*I - 1
      ISUB = 4*I + 11
      VEC(1)  = ECPT(ISUB  ) - X1
      VEC(2)  = ECPT(ISUB+1) - Y1
      VEC(3)  = ECPT(ISUB+2) - Z1
      XX(IXX) = VEC(1)*VECI(1) + VEC(2)*VECI(2) + VEC(3)*VECI(3)
      VVEC(1) = VECI(2)*VEC(3) - VECI(3)*VEC(2)
      VVEC(2) = VECI(3)*VEC(1) - VECI(1)*VEC(3)
      VVEC(3) = VECI(1)*VEC(2) - VECI(2)*VEC(1)
      XX(IXX+1) = SQRT(VVEC(1)**2 + VVEC(2)**2 + VVEC(3)**2)
   50 CONTINUE
      GO TO 70
C
C     INAPPROPRIATE GEOMETRY
C
   60 CALL MESAGE (30,31,ECPT(1))
      NOGO = 1
      RETURN
C
C     COMPUTE MATERIAL PROPERTIES
C
   70 TTH   = TH*3.1415927/180.
      SINTH = SIN(TTH)
      COSTH = COS(TTH)
      ELTEMP= TTEMP
      INFLAG= 2
      MATID = MATID1
      CALL MAT (ECPT(1))
      DO 80 I = 1,3
   80 G(I)  = QQ(I)
      G(4)  = QQ(2)
      G(5)  = QQ(4)
      G(6)  = QQ(5)
      G(7)  = QQ(3)
      G(8)  = QQ(5)
      G(9)  = QQ(6)
      THICK = T
      DO 90 I = 1,9
   90 R(I)  = G(I)
      IF (ISETNO .NE. 0) CALL GMMATS (R,3,3,0,ALPHAS,3,1,0,ST)
C
C     ZERO OUT THE KIJ AND SAVE MATRICES
C
      DO 100 I = 1,36
      KWD(I) = 0.D0
  100 KIJ(I) = 0.D0
      DO 110 I = 1,72
  110 SAVE(I) = 0.D0
C
      PT(1) =-0.57735027D0
      PT(2) =-PT(1)
      H(1)  = 1.D0
      H(2)  = 1.D0
      IF (ID1 .EQ. 2) GO TO 120
      PT(1) =-0.77459667D0
      PT(2) = 0.D0
      PT(3) =-PT(1)
      H(1)  = 5.D0/9.D0
      H(2)  = 8.D0/9.D0
      H(3)  = H(1)
C
C     2 OR 3 QUADRATURE POINTS
C
  120 DO 410 III = 1,ID1
      DO 410 JJJ = 1,ID1
C
C     COMPUTE GAUSS POINT STRESSES
C
C
C     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
C     EACH GRID POINT
C
      DO 130 N = 1,4
      DNXI(N)  = .25D0*XI(N)*(1.D0+PT(JJJ)*ETA(N))*
     1           (2.D0*PT(III)*XI(N)+PT(JJJ)*ETA(N))
      DNETA(N) = .25D0*ETA(N)*(1.D0+PT(III)*XI(N))*
     1           (PT(III)*XI(N)+2.D0*PT(JJJ)*ETA(N))
  130 CONTINUE
      DO 140 N = 5,7,2
C
      DNXI(N)  = -PT(III)*(1.D0+PT(JJJ)*ETA(N))
      DNETA(N) = .5D0*(1.D0-PT(III)*PT(III))*ETA(N)
  140 CONTINUE
C
      DO 150 N = 6,8,2
      DNXI(N)  = .5D0*XI(N)*(1.D0-PT(JJJ)*PT(JJJ))
      DNETA(N) = -PT(JJJ)*(1.D0+PT(III)*XI(N))
  150 CONTINUE
C
C     COMPUTE JACOBEAN
C
C           N1XI   N2XI   N3XI   N4XI   N5XI   N6XI   N7XI   N8XI
C     DNC = N1ETA  N2ETA  N3ETA  N4ETA  N5ETA  N6ETA  N7ETA  N8ETA
C
C          X1  Y1
C          X2  Y2
C          X3  Y3
C     XX = X4  Y4
C          X5  Y5
C          X6  Y6
C          X7  Y7
C          X8  Y8
C
      CALL GMMATD (DNC,2,8,0,XX,8,2,0,XJB)
C
C     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
C     FOR INVERSION
C
      K = 0
      DO 160 I = 1,2
      DO 160 J = 1,2
      K = K + 1
  160 XXJB(I,J) = XJB(K)
C
C     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
C
      CALL INVERD (2,XXJB,2,DUMARG,0,DETERM,ISING,IWS)
      IF (ISING .EQ. 2) CALL MESAGE (-30,143,ECPT(1))
      DHH = DETERM*H(III)*H(JJJ)
C
C     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
C
      K = 0
      DO 170 I = 1,2
      DO 170 J = 1,2
      K = K + 1
  170 XJB(K) = XXJB(I,J)
      CALL GMMATD (XJB,2,2,0,DNC,2,8,0,DNL)
C
C           N1X N2X N3X N4X N5X N6X N7X N8X
C     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
C
      DO 180 I = 1,72
  180 BB(I) = 0.
C
C     SET UP INDICATOR FOR GRID POINT TEMPERATURES
C
      IDTEMP = 0
      DO 190 I = 1,8
      IF (TGRID(I) .NE. 0.) GO TO 200
  190 CONTINUE
      GO TO 210
  200 IDTEMP = 1
C
  210 DO 270 N = 1,8
C
      DO 220 I = 1,9
  220 SEMP(I) = 0.
      DO 230 I = 1,6
  230 S(I) = 0.
      S(1) = DNX(N)
      S(4) = DNY(N)
      S(5) = DNY(N)
      S(6) = DNX(N)
C
C     TRANSFORM TO ELEMENT COORDINATES
C
      IF (NECPT(4*N+10) .EQ. 0) GO TO 240
      CALL TRANSS (NECPT(4*N+10),TTB)
      CALL GMMATS (SE1T,2,3,0,TTB,3,3,0,STB)
      GO TO 260
  240 DO 250 I = 1,6
  250 STB(I) = SE1T(I)
  260 CALL GMMATS (S,3,2,0,STB,2,3,0,SEMP(1))
      N3 = 3*N
      BB(N3- 2) = SEMP(1)
      BB(N3- 1) = SEMP(2)
      BB(N3   ) = SEMP(3)
      BB(N3+22) = SEMP(4)
      BB(N3+23) = SEMP(5)
      BB(N3+24) = SEMP(6)
      BB(N3+46) = SEMP(7)
      BB(N3+47) = SEMP(8)
      BB(N3+48) = SEMP(9)
  270 CONTINUE
C
C     BRING IN G MATRIX
C
      CALL GMMATS (R,3,3,0,BB,3,24,0,DB)
C
C     COMPUTE STRESSES
C
      CALL GMMATS (DB,3,24,0,DISP,24,1,0,SIG)
C
C
C     COMPUTE GAUSS POINT  TEMPERATURES
C
      IF (ISETNO .EQ. 0) GO TO 350
      IF (IDTEMP .EQ. 1) GO TO 280
      RGTEMP = ELTEMP - TREF
      GO TO 330
C
C     ALL TEMPERATURES ARE DEFAULT VALUE
C
  280 DO 290 N = 1,4
      DN(N) = .25*(1.+PT(III)*XI(N))*(1.+PT(JJJ)*ETA(N))
     1        *(PT(III)*XI(N)+PT(JJJ)*ETA(N)-1.)
  290 CONTINUE
      DO 300 N = 5,7,2
      DN(N) = .5*(1.-PT(III)*PT(III))*(1.+PT(JJJ)*ETA(N))
  300 CONTINUE
      DO 310 N = 6,8,2
      DN(N) = .5*(1.+PT(III)*XI(N))*(1.-PT(JJJ)*PT(JJJ))
  310 CONTINUE
      GSTEMP = 0.
      DO 320 N = 1,8
      GSTEMP = GSTEMP + DN(N)*TGRID(N)
  320 CONTINUE
      RGTEMP = GSTEMP - TREF
  330 DO 340 I = 1,3
      SIG(I) = SIG(I) - ST(I)*RGTEMP
  340 CONTINUE
C
  350 CONTINUE
C
C     FORM KWD MATRIX
C
      KWD( 1) = SIG(2)
      KWD( 2) =-SIG(3)
      KWD( 7) =-SIG(3)
      KWD( 8) = SIG(1)
      KWD(15) = SIG(1) + SIG(2)
      KWD(16) =-SIG(3)
      KWD(17) = SIG(3)
      KWD(18) = SIG(1) - SIG(2)
      KWD(21) =-SIG(3)
      KWD(27) = SIG(3)
      KWD(33) = SIG(1) - SIG(2)
C
C     FORM CID FOR I = NPVT
C
      DO 360 I = 1,18
  360 CID( I) = 0.D0
      CID( 3) = DNY(KK)
      CID( 6) =-DNX(KK)
      CID( 7) =-.5*DNY(KK)
      CID( 8) = .5*DNX(KK)
      CID(10) = DNX(KK)
      CID(14) = DNY(KK)
      CID(16) =.5*DNY(KK)
      CID(17) =.5*DNX(KK)
C
      CALL GMMATD (CID,6,3,1,KWD,6,6,0,KMULT)
C
C     LOOP FOR THE 8 6X6 PARTITIONS CORRESPONDING TO THE PRESENT
C     PIVOT POINT
C
      DO 400 N = 1,8
C
      DO 370 I = 1,18
  370 CJD(I) = 0.D0
C
      CJD( 3) = DNY(N)
      CJD( 6) =-DNX(N)
      CJD( 7) =-.5*DNY(N)
      CJD( 8) = .5*DNX(N)
      CJD(10) = DNX(N)
      CJD(14) = DNY(N)
      CJD(16) =.5*DNY(N)
      CJD(17) =.5*DNX(N)
C
      CALL GMMATD (KMULT,3,6,0,CJD,6,3,0,TEMPAR(1))
C
C     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
C
      DO 380 I = 1,9
      TEMPAR(I) = TEMPAR(I)*DHH
  380 CONTINUE
C
C     ADD THE RESULTS OF THIS INTEGRATION TO THE PREVIOUS RESULTS
C
      LL = 9*(N-1)
      DO 390 I = 1,9
      L = LL + I
      SAVE(L) = SAVE(L) + TEMPAR(I)
  390 CONTINUE
C
C     LOOP FOR MORE PARTITIONS
C
  400 CONTINUE
C
C     LOOP FOR MORE GAUSS POINTS
C
  410 CONTINUE
C
C     CHECK ON NECESSITY OF PRE-MULTIPLYING COORDINATE TRANSFORMATIONS
C
      ISUB = 4*KK + 10
      IF (NECPT(ISUB) .EQ. 0) GO TO 420
C
C     ELEMENT TO GLOBAL
C
      CALL TRANSD (NECPT(ISUB),TB)
      CALL GMMATD (E1T,3,3,0,TB,3,3,0,PREMUL)
      GO TO 440
  420 DO 430 I = 1,9
      PREMUL(I) = E1T(I)
  430 CONTINUE
  440 DO 460 N = 1,8
      LL = 9*N - 8
      CALL GMMATD (PREMUL,3,3,1,SAVE(LL),3,3,0,TEMP)
C
C     STORE THE 3 X 3 IN TSAVE
C
      DO 450 I = 1,9
      L = 9*N + I - 9
  450 TSAVE(L) = TEMP(I)
C
  460 CONTINUE
C
C     NOW CHECK ON THE NECESSITY FOR POST-MULTIPLYING TRANSFORMATIONS
C
      DO 500 N = 1,8
      ISUB = 4*N + 10
      LL = 9*N - 8
      IF (NECPT(ISUB) .EQ. 0) GO TO 470
C
C     GLOBAL TO ELEMENT
C
      CALL TRANSD (NECPT(ISUB),TB)
      CALL GMMATD (E1T,3,3,0,TB,3,3,0,PSTMUL)
      GO TO 490
  470 DO 480 I = 1,9
      PSTMUL(I) = E1T(I)
  480 CONTINUE
C
C     POST-MULTIPLY
C
  490 CALL GMMATD (TSAVE(LL),3,3,0,PSTMUL,3,3,0,TEMP)
C
C     FILL OUT THE 6 X 6 PARTITION
C
      KIJ( 1) = TEMP(1)*THICK
      KIJ( 2) = TEMP(2)*THICK
      KIJ( 3) = TEMP(3)*THICK
      KIJ( 7) = TEMP(4)*THICK
      KIJ( 8) = TEMP(5)*THICK
      KIJ( 9) = TEMP(6)*THICK
      KIJ(13) = TEMP(7)*THICK
      KIJ(14) = TEMP(8)*THICK
      KIJ(15) = TEMP(9)*THICK
C
C     INSERT INTO THE OVERALL STIFFNESS MATRIX
C
      CALL DS1B (KIJ,NECPT(N+1))
C
C     LOOP FOR MORE PARTITIONS
C
  500 CONTINUE
C
      RETURN
      END
