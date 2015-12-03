      SUBROUTINE IS2D8S
C
C     2-D, 8 GRID POINT ISOPARAMETRIC STRUCTURAL  ELEMENT STIFFNESS,
C     MASS, CONDUCTIVITY, AND CAPACITANCE ROUTINE
C
C     SINGLE PRECISION VERSION
C
      LOGICAL         ERROR,    HEAT
      INTEGER         GE,       DICT(13), IND6(36), ISIL(8),  NAM(2),
     1                SCR4,     BCD(2)
      REAL            KX,       KY,       KXY,      MIJ,      KIJ
      DIMENSION       VEC(3),   VVEC(3),  VECI(3),  VECJ(3),  VECK(3),
     1                XY1(3),   XY2(3),   IZ(1),    ECPT(46), QQ(15),
     2                IWS(2,3)
      DIMENSION       G(9),     XI(8),    ETA(8),   TB(9),
     1                B(12),    BT(12),   TEMP(9),  TEMPAR(7),DNX(1),
     2                DNY(1),   DNXI(1),  DNETA(1), SAVE(144),TSAVE(216)
     3,               E1T(6),   TEMP1(9), TEMP2(9), TEMP3(9), PSTMUL(9),
     4                PREMUL(6),DN(8),    XX(16),   DNC(16),  DNL(16),
     5                XJB(4),   XXJB(2,2),PT(3),    H(3),     SAVM(36)
      COMMON /BLANK / SKIP(16), VOLUME,   SURFAC
      COMMON /EMGEST/ NECPT(1), NGRID(8), ID1,      TH,       MATID1,
     1                T,        ISYS1,    X1,       Y1,       Z1,
     2                ISYS2,    X2,       Y2,       Z2,       ISYS3,
     3                X3,       Y3,       Z3,       ISYS4,    X4,
     4                Y4,       Z4,       ISYS5,    X5,       Y5,
     5                Z5,       ISYS6,    X6,       Y6,       Z6,
     6                ISYS7,    X7,       Y7,       Z7,       ISYS8,
     7                X8,       Y8,       Z8,       TTEMP,    DUMB(119)
      COMMON /MATIN / MATID,    INFLAG,   ELTEMP,   STRESS,   SINTH,
     1                COSTH
      COMMON/MATOUT / G11,      G12,      G13,      G22,      G23,
     1                G33,      RHO,      ALPHA1,   ALPHA2,   ALPH12,
     2                TREF,     GE,       DUM3(3)
      COMMON /HMTOUT/ KX,       KXY,      KY,       C
      COMMON /EMGDIC/ DUM2(2),  NLOCS,    ELID,     IESTID
      COMMON /ZZZZZZ/ Z(1)
      COMMON /EMGPRM/ IDUM,     JCORE,    NCORE,    DUM12(12),KMBGG(3),
     1                IPREC,    ERROR,    HEAT,     COUP
      EQUIVALENCE     (ECPT(1),NECPT(1)), (Z(1),IZ(1)), (TEMP(1),B(1)),
     1                (DNC(1),DNXI(1))  , (DNC(9),DNETA(1)),
     2                (DNL(1),DNX(1))   , (DNL(9),DNY(1)), (QQ(1),G11),
     3                (TEMPAR(1),BT(1)) , (XY1(1),X1)    , (XY2(1),X2),
     4                (ISIL(1),NGRID(1))
      DATA    XI    / -1.00, 1.00, 1.00,-1.00, 0.00, 1.00, 0.00,-1.00/
      DATA    ETA   / -1.00,-1.00, 1.00, 1.00,-1.00, 0.00, 1.00, 0.00/
      DATA    IND6  / 1,7,49,13,55,91,19,61,97,127,25,67,103,133,157,31,
     1                73,109,139,163,181,37,79,115,145,169,187,199,43,
     2                85,121,151,175,193,205,211/
      DATA    NAM   , BCD/ 4HIS2D,4H8S  , 4HCIS2,4HD8   /
      DATA    SCR4  / 304 /
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
C     ECPT(10) = NO. OF GAUSS POINTS   ID1            INTEGER
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
      IF (JCORE+576 .GT. NCORE) CALL MESAGE (-8,0,NAM)
      DICT(1) = IESTID
      DICT(2) = 1
      IF (HEAT) GO TO 1
      DICT(3) = 24
      DICT(4) = 7
      NSQ     = 576
      GO TO 2
    1 DICT(3) = 8
      DICT(4) = 1
      NSQ     = 64
C
C     SAVE NGRID IN DUMB
C
    2 DO 3 I = 1,9
    3 DUMB(I) = ECPT(I)
      AREA = 0.0
C
C     SET UP SIL ARRAY SO THAT MATRICES ARE SET UP IN INCREASING SIL
C     ORDER SIL(I)=PARTITION NUMBER OF ITH GRID POINT
C
      I =-8
    5 J = 0
      DO 6 K = 1,8
      IF (ISIL(K) .LT. J) GO TO 6
      J = ISIL(K)
      L = K
    6 CONTINUE
      ISIL(L) = I
      I = I + 1
      IF (I .LT. 0) GO TO 5
      DO 7 I = 1,8
    7 ISIL(I) =-ISIL(I)
C
      DO 10 I = 1,NSQ
   10 Z(JCORE+I) = 0.0
C
C     UNIT I VECTOR IS FROM GRID POINT 1 TO GRID POINT 2
C
      DO 20 I = 1,3
      VECI(I) = XY2(I)-XY1(I)
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
      E1T(1)  = VECI(1)
      E1T(2)  = VECI(2)
      E1T(3)  = VECI(3)
      E1T(4)  = VECJ(1)
      E1T(5)  = VECJ(2)
      E1T(6)  = VECJ(3)
C
C     STORE ELEMENT COORDS FOR GRIDS 1 AND 2
C
      XX(1) = 0.0
      XX(2) = 0.0
      XX(3) = VECIL
      XX(4) = 0.0
C
C     FOR GRIDS 3-8, THE X COORDINATE IS THE DOT PRODUCT OF HTE VECTOR
C     FROM GRID POINT 1 TO THE GRID POINT AND THE I VECTOR. THE Y COORD.
C     IS THE L OF THE I VECTOR CROSSED INTO THE VECTOR FROM GRID 1 TO
C     THE GRID POINT.
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
   40 CALL MESAGE (30,31,ECPT(1))
      ERROR = .TRUE.
C
  150 IF (ERROR) RETURN
C
C     SET UP QUADRATURE POINTS AND WEIGHTS
C
      PT(1) =-0.57735027
      PT(2) =-PT(1)
      H(1)  = 1.0
      H(2)  = 1.0
      IF (ID1 .EQ. 2) GO TO 155
      PT(1) =-0.77459667
      PT(2) = 0.0
      PT(3) =-PT(1)
      H(1)  = 5.0/9.0
      H(2)  = 8.0/9.0
      H(3)  = H(1)
C
  155 IF (HEAT) GO TO 700
C
C     COMPUTE MATERIAL PROPERTIES
C
      TTH   = TH*3.1415927/180.
      SINTH = SIN(TTH)
      COSTH = COS(TTH)
      ELTEMP= TTEMP
      INFLAG= 2
      MATID = MATID1
      CALL MAT (ECPT(1))
      DO 160 I = 1,3
  160 G(I)  = QQ(I)
      G(4)  = QQ(2)
      G(5)  = QQ(4)
      G(6)  = QQ(5)
      G(7)  = QQ(3)
      G(8)  = QQ(5)
      G(9)  = QQ(6)
      THICK = T
      DRHO  = RHO*T
C
C     ZERO THE SAVE MATRICES TO COLLECT INTEGRATIONS
C
      DO 210 I = 1,36
  210 SAVM(I) = 0.0
      DO 220 I = 1,144
  220 SAVE(I) = 0.0
C
C     2 OR 3 QUADRATURE POINTS
C
      DO 300 III = 1,ID1
      DO 300 JJJ = 1,ID1
C
C     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
C     EACH GRID POINT
C
      DO 230 N = 1,4
      IF (KMBGG(2) .NE. 0) DN(N) = .25*(1.0+PT(III)*XI(N))*
     1   (1.0+PT(JJJ)*ETA(N))*(PT(III)*XI(N)+PT(JJJ)*ETA(N)-1.0)
      DNXI(N)  = .25*XI(N)*(1.0+PT(JJJ)*ETA(N))*
     1           (2.0*PT(III)*XI(N)+PT(JJJ)*ETA(N))
      DNETA(N) = .25*ETA(N)*(1.0+PT(III)*XI(N))*
     1           (PT(III)*XI(N)+2.0*PT(JJJ)*ETA(N))
  230 CONTINUE
C
      DO 231 N = 5,7,2
      IF (KMBGG(2) .NE. 0) DN(N) = .50*(1.0-PT(III)*PT(III))*
     1   (1.0+PT(JJJ)*ETA(N))
      DNXI(N)  = -PT(III)*(1.0+PT(JJJ)*ETA(N))
      DNETA(N) = .50*(1.0-PT(III)*PT(III))*ETA(N)
  231 CONTINUE
C
      DO 232 N = 6,8,2
      IF (KMBGG(2) .NE. 0) DN(N) = .50*(1.0+PT(III)*XI(N))*
     1   (1.0-PT(JJJ)*PT(JJJ))
      DNXI(N)  = .50*XI(N)*(1.0-PT(JJJ)*PT(JJJ))
      DNETA(N) =-PT(JJJ)*(1.0+PT(III)*XI(N))
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
      CALL GMMATS (DNC,2,8,0,XX,8,2,0,XJB)
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
      IF (ISING .NE. 2) GO TO 241
      CALL MESAGE (30,143,ECPT(1))
      ERROR =.TRUE.
      RETURN
C
  241 CONTINUE
      DHH  = DETERM*H(III)*H(JJJ)
      AREA = AREA + DHH
C
C     COMPUTE DERIVATIVES WITH RESPECT TO X AND Y
C
      K = 0
      DO 250 I = 1,2
      DO 250 J = 1,2
      K = K + 1
  250 XJB(K) = XXJB(I,J)
      CALL GMMATS (XJB,2,2,0,DNC,2,8,0,DNL)
C
C           N1X N2X N3X N4X N5X N6X N7X N8X
C     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
C
C     SET UP THE BT MATRIX
C
      IC = 0
      DO 290 KK = 1,8
      IF (KMBGG(1) .EQ. 0) GO TO 256
C
      DO 255 I = 1,12
  255 BT(I) = 0.0
      BT(1) = DNX(KK)
      BT(3) = DNY(KK)
      BT(5) = DNY(KK)
      BT(6) = DNX(KK)
C
      CALL GMMATS (TEMPAR(1),2,3,0,G,3,3,0,TEMPAR(7))
C
C     MULTIPLY G MATRIX BY PRESENT RESULTS
C
C     LOOP FOR THE 8 6X6 PARTITIONS CORRESPONDING TO THE PRESENT
C     PIVOT POINT
C
  256 CONTINUE
      DO 290 N = KK,8
      IC = IC + 1
      IF (KMBGG(1) .EQ. 0) GO TO 281
C
C     SET UP THE B MATRIX
C
      DO 260 I = 1,12
  260 B( I) = 0.0
      B( 1) = DNX(N)
      B( 4) = DNY(N)
      B( 5) = DNY(N)
      B( 6) = DNX(N)
C                                    T
C     PERFORM MULTIPLICATION TO GET B *D*B
C
      CALL GMMATS (TEMPAR(7),2,3,0,B,3,2,0,TEMPAR(1))
C
C     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
C
      DO 270 I = 1,4
      TEMPAR(I) = TEMPAR(I)*DHH
  270 CONTINUE
C
C     ADD THE RESULTS OF THIS INTEGRATION TO THE PREVIOUS RESULTS
C
      LL = 4*(IC-1)
      DO 280 I = 1,4
      L = LL + I
      SAVE(L) = SAVE(L) + TEMPAR(I)
  280 CONTINUE
  281 CONTINUE
C
      IF (KMBGG(2) .EQ. 0) GO TO 289
C
      MIJ = DN(KK)*DN(N)*DHH
      SAVM(IC) = SAVM(IC) + MIJ
  289 CONTINUE
C
C     LOOP FOR MORE PARTITIONS
C
  290 CONTINUE
C
C     LOOP FOR MORE GAUSS POINTS
C
  300 CONTINUE
      IF (KMBGG(2) .EQ. 0) GO TO 306
      DO 305 I = 1,36
  305 SAVM(I) = SAVM(I)*DRHO
  306 CONTINUE
C
C     CHECK ON NECESSITY OF PRE-MULTIPLYING COORDINATE TRANSFORMATIONS
C
      IF (KMBGG(1) .EQ. 0) GO TO 500
      IC = 0
      DO 385 KK = 1,8
      ISUB = 4*KK + 10
      IF (NECPT(ISUB) .EQ. 0) GO TO 310
C
C     ELEMENT TO GLOBAL
C
      CALL TRANSS (NECPT(ISUB),TB)
      CALL GMMATS (E1T,2,3,0,TB,3,3,0,PREMUL)
      GO TO 350
  310 DO 320 I = 1,6
      PREMUL(I) = E1T(I)
  320 CONTINUE
  350 DO 380 N = KK,8
      IC = IC + 1
      LL = 4*IC - 3
      CALL GMMATS (PREMUL,2,3,1,SAVE(LL),2,2,0,TEMP)
C
C     STORE THE 3 X 2 IN TSAVE
C
      DO 370 I = 1,6
      L = 6*IC + I - 6
  370 TSAVE(L) = TEMP(I)
C
  380 CONTINUE
  385 CONTINUE
C
C     NOW CHECK ON THE NECESSITY FOR POST-MULTIPLYING TRANSFORMATIONS
C
      IC = 0
      DO 490 N = 1,8
      ISUB = 4*N + 10
      IF (NECPT(ISUB) .EQ. 0) GO TO 410
C
C     GLOBAL TO ELEMENT
C
      CALL TRANSS (NECPT(ISUB),TB)
      CALL GMMATS (E1T,2,3,0,TB,3,3,0,PSTMUL)
      GO TO 450
  410 DO 420 I = 1,6
      PSTMUL(I) = E1T(I)
  420 CONTINUE
C
C     POST-MULTIPLY
C
C     IND6 GIVES STARTING POSITIONS OF VERTICAL 3X3 PARTITIONS, SINCE
C     THE NTH COLUMN MULTIPLIES INTO THE NTH POST-MULTIPLIER
C
  450 DO 485 M = 1,N
      IC = IC + 1
      LL = IND6(IC)
      CALL GMMATS (TSAVE(LL),3,2,0,PSTMUL,2,3,0,TEMP)
      DO 486 I =  1,9
  486 TEMP(I) = TEMP(I)*THICK
C
C     PICK UP ROW AND COLUMN PARTITION NUMBERS AND CONVERT TO STARTING
C     POINTS IN OPEN CORE FOR THIS PARTITION  AND ITS TRANSPOSE.
C     TEMP IS PUT INTO ONE PARTITION AND TEMP-TRANSPOSE INTO THE OTHER
C
      NCOL = ISIL(N)
      NROW = ISIL(M)
      CALL INSERT (NCOL,NROW,3,8,JCORE,Z,Z,TEMP,TEMP,IPREC)
C
C     LOOP FOR ANOTHER PARTITION FOR THIS POST-MULTIPLIER
C
  485 CONTINUE
C
C     LOOP FOR ANOTHER POST-MULTIPLIER
C
  490 CONTINUE
C
C     ADD TO DICTIONARY
C
      DICT(5) = GE
      CALL EMGOUT (Z(JCORE),Z(JCORE),NSQ,1,DICT,1,IPREC)
C
  500 IF (KMBGG(2) .EQ. 0) GO TO 1000
C
      IC = 0
      DO 620 KK = 1,8
      DO 620 N = KK,8
      IC = IC + 1
      DO 510 I = 1,9
  510 TEMP(I) = 0.0
C
C     CHECK ON TEH NECESSITY OF COORDINATE TRANSFORMATIONS.
C     SINCE EACH PARTITION IS A MULTIPLE OF A 3X3 IDENTITY AND SINCE
C     THE TRANSFORAMATION MATRICES ARE ORTHOGONAL, NO EXPLICIT
C     TRANSFORMA-TIONS FROM THE ELEMENT COORDINATE SYSTEM ARE REQUIRED.
C     ALSO, NO TRANSFORAMTION IS REQUIRED IF TRANSFORMATION MATRICES ARE
C     THE SAME FOR THE GRIDS CORRESPONDING TO THE THE ROW AND COLUMN
C
      TERM  = SAVM(IC)
      IF (KK .EQ. N) GO TO 570
      ISUB  = 4*KK + 10
      ISUB1 =4*N + 10
      IF (NECPT(ISUB).EQ.0 .AND. NECPT(ISUB1).EQ.0) GO TO 570
      IF (NECPT(ISUB) .EQ. 0) GO TO 520
      CALL TRANSS (NECPT(ISUB),TEMP1)
      IF (NECPT(ISUB1) .EQ. 0) GO TO 530
  520 CALL TRANSS (NECPT(ISUB1),TEMP2)
      IF (NECPT(ISUB) .EQ. 0) GO TO 550
C
C     MULTIPLY THE TRANSFORMATION MATRICES
C
      CALL GMMATS (TEMP1,3,3,1,TEMP2,3,3,0,TEMP3)
      GO TO 580
  530 TEMP3(1) = TEMP1(1)
      TEMP3(2) = TEMP1(4)
      TEMP3(3) = TEMP1(7)
      TEMP3(4) = TEMP1(2)
      TEMP3(5) = TEMP1(5)
      TEMP3(6) = TEMP1(8)
      TEMP3(7) = TEMP1(3)
      TEMP3(8) = TEMP1(6)
      TEMP3(9) = TEMP1(9)
      GO TO 580
  550 DO 560 I = 1,9
  560 TEMP3(I) = TEMP2(I)
      GO TO 580
  570 TEMP(1) = TERM
      TEMP(5) = TERM
      TEMP(9) = TERM
      GO TO 600
C
  580 DO 590 I = 1,9
  590 TEMP(I) = TERM*TEMP3(I)
C
  600 NROW = ISIL(KK)
      NCOL = ISIL(N)
      CALL INSERT (NCOL,NROW,3,8,JCORE,Z,Z,TEMP,TEMP,IPREC)
  620 CONTINUE
C
      CALL EMGOUT (Z(JCORE),Z(JCORE),NSQ,1,DICT,2,IPREC)
      GO TO 1000
C
C     HEAT FORMULATION
C
C     COMPUTE MATERIAL PROPERTIES
C
  700 SINTH  = 0.
      COSTH  = 0.
      ELTEMP = TTEMP
      INFLAG = 2
      MATID  = MATID1
      CALL HMAT (ECPT(1))
      THICK  = T
      DC     = C*T
C
C     ZERO OUT THE SAVE MATRIX
C
      DO 720 I = 1,36
      SAVM(I) = 0.0
  720 SAVE(I) = 0.0
C
      DO 880 III = 1,ID1
      DO 880 JJJ = 1,ID1
C
C     COMPUTE DERIVATIVES WITH RESPECT TO XI AND ETA
C     EACH GRID POINT
C
      DO 730 N = 1,4
      IF (KMBGG(3) .NE. 0) DN(N) = .25*(1.0+PT(III)*XI(N))*
     1   (1.0+PT(JJJ)*ETA(N))*(PT(III)*XI(N)+PT(JJJ)*ETA(N)-1.0)
      DNXI(N)  = .25*XI(N)*(1.0+PT(JJJ)*ETA(N))*
     1           (2.0*PT(III)*XI(N)+PT(JJJ)*ETA(N))
      DNETA(N) = .25*ETA(N)*(1.0+PT(III)*XI(N))*
     1           (PT(III)*XI(N)+2.0*PT(JJJ)*ETA(N))
  730 CONTINUE
C
      DO 731 N = 5,7,2
      IF (KMBGG(3) .NE. 0) DN(N) = .50*(1.0-PT(III)*PT(III))*
     1   (1.0+PT(JJJ)*ETA(N))
      DNXI(N)  = -PT(III)*(1.0+PT(JJJ)*ETA(N))
      DNETA(N) = .50*(1.0-PT(III)*PT(III))*ETA(N)
  731 CONTINUE
C
      DO 732 N = 6,8,2
      IF (KMBGG(3) .NE. 0) DN(N) = .50*(1.0+PT(III)*XI(N))*
     1    (1.0-PT(JJJ)*PT(JJJ))
      DNXI(N)  = .50*XI(N)*(1.0-PT(JJJ)*PT(JJJ))
      DNETA(N) = -PT(JJJ)*(1.0+PT(III)*XI(N))
  732 CONTINUE
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
      CALL GMMATS (DNC,2,8,0,XX,8,2,0,XJB)
C
C     XJB IS ROW-STORED-IT MUST BE COLUMN-STORED AND DOUBLY DIMENSIONED
C     FOR INVERSION
C
      K = 0
      DO 740 I = 1,2
      DO 740 J = 1,2
      K = K + 1
  740 XXJB(I,J) = XJB(K)
C
C     COMPUTE INVERSE AND DETERMINANT OF JACOBEAN
C
      CALL INVERS (2,XXJB,2,DUMARG,0,DETERM,ISING,IWS)
      IF (ISING .NE. 2) GO TO 741
      CALL MESAGE (30,143,ECPT(1))
      ERROR =.TRUE.
      RETURN
C
  741 CONTINUE
      DHH = DETERM*H(III)*H(JJJ)
C
C     COMPUTE DERIVATIVES WITH RESPECT TO X,Y,AND Z
C
      K = 0
      DO 750 I = 1,2
      DO 750 J = 1,2
      K = K + 1
  750 XJB(K) = XXJB(I,J)
      CALL GMMATS (XJB,2,2,0,DNC,2,8,0,DNL)
C
C           N1X N2X N3X N4X N5X N6X N7X N8X
C     DNL = N1Y N2Y N3Y N4Y N5Y N6Y N7Y N8Y
C
C     SET UP THE BT MATRIX
C
      IC = 0
      DO 875 KK = 1,8
      IF (KMBGG(1) .EQ. 0) GO TO 800
      BT(1) = KX *DNX(KK) + KXY*DNY(KK)
      BT(2) = KXY*DNX(KK) + KY*DNY(KK)
C
C     DO NOT TRANSFORM FROM MATERIAL COORD SYSTEM TO BASIC AND GLOBAL
C     SINCE THIS IS A SCALAR PROBLEM
C
  800 DO 870 N = KK,8
      IC = IC + 1
C
C     SET UP THE B MATRIX
C
      IF (KMBGG(1) .EQ. 0) GO TO 810
      B(1) = DNX(N)
      B(2) = DNY(N)
C
C     O.K. NOW PERFORM FINAL MULTIPLICATION
C
      KIJ = BT(1)*B(1) + BT(2)*B(2)
C
C     THROW IN JACOBEAN DETERMINANT AND WEIGHT FACTORS
C
      KIJ = KIJ*DHH
C
C     ADD THE RESULTS OF THIS INTEGRATION TO PREVIOUS RESULTS
C
      SAVE(IC) = SAVE(IC) + KIJ
  810 IF (KMBGG(3) .EQ. 0) GO TO 870
      BIJ = DN(KK)*DN(N)*DHH
      SAVM(IC) = SAVM(IC) + BIJ
C
C     LOOP FOR MORE PARTITIONS
C
  870 CONTINUE
  875 CONTINUE
C
C     LOOP FOR ADDITIONAL GAUSS POINTS
C
  880 CONTINUE
C
      DO 890 I = 1,36
      IF (KMBGG(1) .NE. 0) SAVE(I) = SAVE(I)*THICK
      IF (KMBGG(3) .NE. 0) SAVM(I) = SAVM(I)*DC
  890 CONTINUE
C
C     INSERT INTO OVERALL STIFFNESS MATRIX
C
      IC = 0
      DO 900 I = 1,8
      DO 900 J = I,8
      IC = IC + 1
      NROW = ISIL(I)
      NCOL = ISIL(J)
      IF (KMBGG(1) .NE. 0) CALL INSERT (NCOL,NROW,1,8,JCORE,Z,Z,
     1    SAVE(IC),SAVE(IC),IPREC)
      IF (KMBGG(3) .NE. 0) CALL INSERT (NCOL,NROW,1,8,JCORE+64,Z,Z,
     1   SAVM(IC),SAVM(IC),IPREC)
  900 CONTINUE
C
      IF (KMBGG(1) .NE. 0) CALL EMGOUT (Z(JCORE),Z(JCORE),NSQ,1,DICT,1,
     1    IPREC)
      IF (KMBGG(3) .NE. 0) CALL EMGOUT (Z(JCORE+64),Z(JCORE+64),NSQ,1,
     1    DICT,3,IPREC)
      GO TO 5000
C
C     SAVE ELEMENT NAME, ID, THICKNESS, DENSITY, NO. OF GRID POINTS,
C     GRID POINT DATA, AND AREA IF USER REQUESTED VOLUME AND AREA
C     COMPUTATION
C
 1000 IF (VOLUME.LE.0.0 .AND. SURFAC.LE.0.0) GO TO 5000
      ECPT(2) = ECPT(13)
      ECPT(3) = RHO
      J = 4
      NECPT(J) = 8
      ECPT(46) = AREA
      CALL WRITE (SCR4,BCD,2,0)
      CALL WRITE (SCR4,ECPT(1),4,0)
      CALL WRITE (SCR4,DUMB(2),8,0)
      CALL WRITE (SCR4,ECPT(14),33,1)
 5000 RETURN
      END
