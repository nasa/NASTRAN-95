      SUBROUTINE TIS2D8 (TEMP,PG)
C
C     THIS ROUTINE COMPUTES EQUIVALENT LOADS DUE TO GRID POINT
C     TEMPERATURES FOR THE 2-D, 8 GRID POINT ISOPARAMETRIC ELEMENT
C
C     ECPT LIST                        IN
C                                      THIS
C     ECPT       DESCRIPTION           ROUTINE         TYPE
C     --------   --------------------  --------       -------
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
C
      REAL            KX,KY
      DIMENSION       G(9),QQ(15),XI(8),ETA(8),XY1(3),XY2(3),BT(12),
     1                ECPT(1),TEMPAR(8),DNX(1),DNY(1),IZ(1),DNXI(1),
     2                DNETA(1),TEMP(8),PG(1),DN(8),IWS(2,3),
     3                VEC(3),VVEC(3),VECI(3),VECJ(3),VECK(3),E1T(6)
      COMMON /TRANX / Z(14)
      COMMON /TRIMEX/ NECPT(1),NGRID(8),ID1,TH,MATID1,T,ISYS1,X1,Y1,Z1,
     1                ISYS2,X2,Y2,Z2,ISYS3,X3,Y3,Z3,ISYS4,X4,Y4,Z4,
     2                ISYS5,X5,Y5,Z5,ISYS6,X6,Y6,Z6,ISYS7,X7,Y7,Z7,
     3                ISYS8,X8,Y8,Z8,TTEMP,SAVE(16),RTSIDE(3),TEMPAV,
     4                ALPHAS(3),XX(16),DNC(16),DNL(16),XXJB(2,2),
     5                XJB(4),PT(3),H(3),G,BT,DETERM,DUMARG
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                TREF,GE,KX,KY,C
      EQUIVALENCE     (ECPT(1),NECPT(1)),(Z(1),IZ(1)),(QQ(1),G11),
     1                (DNC(1),DNXI(1)),(DNC(9),DNETA(1)),
     2                (DNL(1),DNX(1)),(DNL(9),DNY(1)),
     3                (TEMPAR(1),BT(1)),(XY1(1),X1),(XY2(1),X2)
      DATA    XI    / -1., 1., 1., -1., 0., 1., 0., -1./
      DATA    ETA   / -1.,-1., 1.,  1.,-1., 0., 1.,  0./
C
C     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
C
      DO 20 I = 1,3
      VECI(I) = XY2(I) - XY1(I)
   20 CONTINUE
      VECIL = SQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      IF (VECIL .EQ. 0.0) GO TO 40
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
      VECKL   = SQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      IF (VECKL .EQ. 0.0) GO TO 40
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
C
C     STORE ELEMENT COORDS FOR GRIDS 1 AND 2
C
      XX(1) = 0.
      XX(2) = 0.
      XX(3) = VECIL
      XX(4) = 0.
C
C     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
C     FROM THE GRID POINT TO
C     GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD. IS
C     THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO THE
C     GRID POINT.
C
      DO 30 I = 3,8
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
   30 CONTINUE
      GO TO 150
C
C     INAPPROPRIATE GEOMETRY
C
   40 CALL MESAGE (-30,31,ECPT(1))
C
C     COMPUTE MATERIAL PROPERTIES
C
  150 TTH   = TH*3.1415927/180.
      SINTH = SIN(TTH)
      COSTH = COS(TTH)
      INFLAG= 2
      MATID = MATID1
C
C     ZERO OUT SOME MATRICES
C
      DO 225 I = 1,16
  225 SAVE(I) = 0.
C
      PT(1) =-0.57735027
      PT(2) =-PT(1)
      H(1)  = 1.
      H(2)  = 1.
      IF (ID1 .EQ. 2) GO TO 226
      PT(1) =-0.77459667D0
      PT(2) = 0.D0
      PT(3) =-PT(1)
      H(1)  = 5.0/9.0
      H(2)  = 8.0/9.0
      H(3)  = H(1)
C
C     2 OR 3 QUADRATURE POINTS
C
  226 DO 255 III = 1,ID1
      DO 255 JJJ = 1,ID1
C
C     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
C     EACH GRID POINT
C
      DO 230 N = 1,4
      DN(N)   = 0.25*(1.+PT(III)*XI(N))*(1.+PT(JJJ)*ETA(N))*
     1          (PT(III)*XI(N)+PT(JJJ)*ETA(N)-1.)
      DNXI(N) = 0.25*XI(N)*(1.+PT(JJJ)*ETA(N))*
     1          (2.*PT(III)*XI(N)+PT(JJJ)*ETA(N))
      DNETA(N)= 0.25*ETA(N)*(1.+PT(III)*XI(N))*
     1          (PT(III)*XI(N)+2.*PT(JJJ)*ETA(N))
  230 CONTINUE
C
      DO 231 N = 5,7,2
      DN(N)   = 0.5*(1.-PT(III)*PT(III))*(1.+PT(JJJ)*ETA(N))
      DNXI(N) =-PT(III)*(1.+PT(JJJ)*ETA(N))
      DNETA(N)= 0.5*(1.-PT(III)*PT(III))*ETA(N)
  231 CONTINUE
C
      DO 232 N = 6,8,2
      DN(N)   = 0.5*(1.+PT(III)*XI(N))*(1.-PT(JJJ)*PT(JJJ))
      DNXI(N) = 0.5*XI(N)*(1.-PT(JJJ)*PT(JJJ))
      DNETA(N)=-PT(JJJ)*(1.+PT(III)*XI(N))
  232 CONTINUE
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
      CALL GMMATS (DNC,2,8,0, XX,8,2,0, XJB)
C
C     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
C     FOR INVERSION
C
      K = 0
      DO 240 I = 1,2
      DO 240 J = 1,2
      K = K + 1
  240 XXJB(I,J) = XJB(K)
C
C     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
C
      CALL INVERS (2,XXJB,2,DUMARG,0,DETERM,ISING,IWS)
      IF (ISING .EQ. 2) CALL MESAGE (-30,143,ECPT(1))
C
C     COMPUTE DERIVATIVES WITH RESPECT TO X,Y,AND Z
C
      K = 0
      DO 245 I = 1,2
      DO 245 J = 1,2
      K = K + 1
  245 XJB(K) = XXJB(I,J)
      CALL GMMATS (XJB,2,2,0, DNC,2,8,0, DNL)
C
C           N1X N2X N3X N4X N5X N6X N7X N8X
C     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
C
      COEF = DETERM*H(III)*H(JJJ)
C
C     COMPUTE GAUSS POINT TEMPERATURE
C
      GSTEMP = 0.
      DO 234 N = 1,8
      GSTEMP = GSTEMP + DN(N)*(TEMP(N))
  234 CONTINUE
C
C     GSTEMP IS THE GAUSS POINT TEMPERATURE. FIND MATERIAL PROPERTIES
C     BASED ON THIS TEMPERATURE. IF SAME AS PREVIOUS TEMPERATURE,DO NOT
C     RECOMPUTE.
C
      LLL = III*JJJ
      IF (LLL .EQ. 1) GO TO 236
      IF (GSTEMP .EQ. ELTEMP) GO TO 237
  236 ELTEMP = GSTEMP
      CALL MAT (ECPT(1))
      DO 160 I = 1,3
  160 G(I) = QQ(I)
      G(4) = QQ(2)
      G(5) = QQ(4)
      G(6) = QQ(5)
      G(7) = QQ(3)
      G(8) = QQ(5)
      G(9) = QQ(6)
      ALPHAS(1) = ALPHA1
      ALPHAS(2) = ALPHA2
      ALPHAS(3) = ALP12
C
      CALL GMMATS (G,3,3,0, ALPHAS,3,1,0, RTSIDE)
C
C     COMPUTE RELATIVE GAUSS POINT TEMPERATURE
C
      RGTEMP = GSTEMP - TREF
C
  237 CONTINUE
C
      COEF = COEF*RGTEMP
C
C     SET UP BT
C
      DO 239 KK = 1,8
C
      DO 238 I = 1,6
  238 BT(I) = 0.
C
      BT(1) = DNX(KK)
      BT(3) = DNY(KK)
      BT(5) = BT(3)
      BT(6) = BT(1)
C
      CALL GMMATS (BT,2,3,0, RTSIDE,3,1,0, TEMPAR(7))
C
C     ADD TO PREVIOUS RESULTS
C
      SAVE(2*KK-1) = SAVE(2*KK-1) + TEMPAR(7)*COEF
      SAVE(2*KK  ) = SAVE(2*KK  ) + TEMPAR(8)*COEF
C
C     CONTINUE FOR MORE GRID POINTS
C
  239 CONTINUE
C
C     CONTINUE FOR MORE GAUSS POINTS
C
  255 CONTINUE
C
C     TRANSFORMATIONS AND ADD TO OVERALL VECTOR
C
      DO 350 KK = 1,8
      TEMPAR(7) = SAVE(2*KK-1)
      TEMPAR(8) = SAVE(2*KK  )
C
C     CONVERT FROM ELEMENT COORDINATES TO BASIC
C
      CALL GMMATS (E1T,2,3,1, TEMPAR(7),2,1,0, TEMPAR(1))
      ISUB = 4*KK + 10
      IF (NECPT(ISUB) .EQ. 0) GO TO 300
C
C     MUST TRANSFORM FROM BASIC COORDS TO GLOBAL
C
      CALL BASGLB (TEMPAR(1),TEMPAR(1),ECPT(ISUB+1),NECPT(ISUB))
C
C     ADD THIS VECTOR TO OVERALL LOAD VECTOR
C
  300 DO 310 I = 1,3
      L = NGRID(KK) + I - 1
  310 PG(L) = PG(L) + TEMPAR(I)*T
C
C     GET ANOTHER PARTITION
C
  350 CONTINUE
      RETURN
      END
