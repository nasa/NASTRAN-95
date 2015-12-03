      SUBROUTINE STRBS1 (IOPT)
C
C     PHASE ONE FOR STRESS RECOVERY
C
C           IOPT = 0    (BASIC BENDING TRIANGLE)
C           IOPT = 1    (SUB-CALCULATIONS FOR SQDPL1)
C           IOPT = 2    (SUB-CALCULATIONS FOR STRPL1)
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C
C           MAT    - MATERIAL DATA ROUTINE
C           TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
C           INVERS - SINGLE PRECISION INVERSE ROUTINE
C           GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C           MESAGE - ERROR MESSAGE WRITER
C
      LOGICAL         STRAIN
      INTEGER         SUBSCA,SUBSCB
      REAL            KS,J2X2,ST(3)
      DIMENSION       D(9),G2X2(4),J2X2(4),S(258),ECPT(25),G(9),HIC(18),
     1                HIB(18),TITE(18),T(9),KS(30),HINV(36)
C
C     COMMON /SDR2X3/ ESTWDS(32),ESTAWD(32),NGPS(32),STRSWD(32),
C    1                FORCWD(32)
C     THE ABOVE COMMON BLOCK IS NOT USED IN THIS ROUTINE DIRECTLY.
C
C     ESTWDS(I) = THE NUMBER OF WORDS IN THE EST INPUT BLOCK FOR
C                 THE  I-TH ELEMENT TYPE.
C     ESTAWD(I) = THE NUMBER OF WORDS COMPUTED IN PHASE-I FOR THE
C                 I-TH ELEMENT TYPE, AND INSERTED INTO THE ESTA ARRAY
C                 OF THE LABELED BLOCK SDR2X5
C     NGPS  (I) = THE NUMBER OF GRID POINTS ASSOCIATED WITH THE I-TH
C                 ELEMENT TYPE.
C     STRSWD(I) = THE NUMBER OF WORDS COMPUTED IN PHASE-II FOR
C                 THE I-TH ELEMENT TYPE, AND INSERTED INTO THE ESTA
C                 ARRAY OF THE LABELED BLOCK SDR2X5.
C     FORCWD(I) = THE NUMBER OF WORDS COMPUTED IN PHASE-II FOR
C                 THE I-TH ELEMENT TYPE, AND INSERTED INTO THE FORCES
C                 ARRAY OF THE LABELED BLOCK SDR2X5.
C
      COMMON /BLANK / IDUMMY(10), STRAIN
      COMMON /CONDAS/ CONSTS(5)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,G2X211,
     2                G2X212,G2X222
      COMMON /SDR2X6/ A(225),XSUBB,XSUBC,YSUBC,E(18),TEMP,XBAR,AREA,
     1                XCSQ,YBAR2,YCSQ,YBAR,XBSQ,PX2,XCYC,PY2,PXY2,XBAR3,
     2                YBAR3,DETERM,PROD9(9),TEMP9(9),NSIZED,DUMDUM(4),
     3                NPIVOT,THETA ,NSUBC,ISING,SUBSCA,SUBSCB,NERROR,
     4                NBEGIN,NTYPED,XC,YC,YC2,YC3,ISUB,XC3,DUM55(1)
      COMMON /SDR2X5/ NECPT(1),NGRID(3),ANGLE,MATID1,EYE,MATID2,T2,FMU,
     1                Z11,Z22,DUMMY1,X1,Y1,Z1,DUMMY2,X2,Y2,Z2,DUMMY3,
     2                X3,Y3,Z3,DUMB(76),PH1OUT(100),FORVEC(25)
      EQUIVALENCE     (CONSTS(4),DEGRA),(D(1),G(1),A(79)),
     1                (ECPT(1),NECPT(1)),(KS(1),PH1OUT(1)),
     2                (G2X2(1),A(88)),(S(1),A(55)),(TITE(1),A(127)),
     3                (J2X2(1),A(92)),(T(1),A(118)),(HIB(1),A(109)),
     4                (HIC(1),A(127)),(HINV(1),A(73)),(ST(1),PH1OUT(99))
C
C     ECPT LIST FOR BASIC BENDING TRIANGLE             NAME IN
C                                                      THIS
C     ECPT                                             ROUTINE    TYPE
C     --------   -----------------------------------   --------  -------
C     ECPT( 1) = ELEMENT ID                            NECPT(1)  INTEGER
C     ECPT( 2) = GRID POINT A                          NGRID(1)  INTEGER
C     ECPT( 3) = GRID POINT B                          NGRID(2)  INTEGER
C     ECPT( 4) = GRID POINT C                          NGRID(3)  INTEGER
C     ECPT( 5) = THETA = ANGLE OF MATERIAL             ANGLE     REAL
C     ECPT( 6) = MATERIAL ID 1                         MATID1    INTEGER
C     ECPT( 7) = I = MOMENT OF INERTIA                 EYE       REAL
C     ECPT( 8) = MATERIAL ID 2                         MATID2    INTEGER
C     ECPT( 9) = T2                                    T2        REAL
C     ECPT(10) = NON-STRUCTURAL-MASS                   FMU       REAL
C     ECPT(11) = Z1                                    Z11       REAL
C     ECPT(12) = Z2                                    Z22       REAL
C     ECPT(13) = COORD. SYSTEM ID 1                    NECPT(13) INTEGER
C     ECPT(14) = X1                                    X1        REAL
C     ECPT(15) = Y1                                    Y1        REAL
C     ECPT(16) = Z1                                    Z1        REAL
C     ECPT(17) = COORD. SYSTEM ID 2                    NECPT(17) INTEGER
C     ECPT(18) = X2                                    X2        REAL
C     ECPT(19) = Y2                                    Y2        REAL
C     ECPT(20) = Z2                                    Z2        REAL
C     ECPT(21) = COORD. SYSTEM ID 3                    NECPT(21) INTEGER
C     ECPT(22) = X3                                    X3        REAL
C     ECPT(23) = Y3                                    Y3        REAL
C     ECPT(24) = Z3                                    Z3        REAL
C     ECPT(25) = ELEMENT TEMPERATURE                   ELTEMP    REAL
C
      IF (IOPT .GT. 0) GO TO 30
      ELTEMP = ECPT(25)
C
C     SET UP  I, J, K VECTORS STORING AS FOLLOWS AND ALSO CALCULATE
C     X-SUB-B, X-SUB-C, AND Y-SUB-C.
C
C     E(11), E(14), E(17) WILL BE THE I-VECTOR.
C     E(12), E(15), E(18) WILL BE THE J-VECTOR.
C     E( 1), E( 4), E( 7) WILL BE THE K-VECTOR.
C
C     FIND I-VECTOR = RSUBB - RUBA (NON-NORMALIZED)
C
      E(11) = X2 - X1
      E(14) = Y2 - Y1
      E(17) = Z2 - Z1
C
C     FIND LENGTH = X-SUB-B COOR. IN ELEMENT SYSTEM
C
      XSUBB = SQRT(E(11)**2 + E(14)**2 + E(17)**2)
      IF (XSUBB .GT. 1.0E-06) GO TO 10
      CALL MESAGE (-30,37,ECPT(1))
C
C     NORMALIZE I-VECTOR WITH X-SUB-B
C
   10 E(11) = E(11)/XSUBB
      E(14) = E(14)/XSUBB
      E(17) = E(17)/XSUBB
C
C     TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN E(2), E(5), E(8)
C
      E(2) = X3 - X1
      E(5) = Y3 - Y1
      E(8) = Z3 - Z1
C
C     X-SUB-C = I . (RSUBC - RSUBA), THUS
C
      XSUBC = E(11)*E(2) + E(14)*E(5) + E(17)*E(8)
C
C     CROSSING I-VECTOR TO (RSUBC - RSUBA) GIVES THE K-VECTOR
C     (NON-NORMALIZED)
C
      E(1) = E(14)*E( 8) - E( 5)*E(17)
      E(4) = E( 2)*E(17) - E(11)*E( 8)
      E(7) = E(11)*E( 5) - E( 2)*E(14)
C
C     FIND LENGTH = Y-SUB-C COOR. IN ELEMENT SYSTEM
C
      YSUBC = SQRT(E(1)**2 + E(4)**2 + E(7)**2)
      IF (YSUBC .GT. 1.0E-06) GO TO 20
      CALL MESAGE (-30,37,ECPT(1))
C
C     NORMALIZE K-VECTOR WITH Y-SUB-C
C
   20 E(1) = E(1)/YSUBC
      E(4) = E(4)/YSUBC
      E(7) = E(7)/YSUBC
C
C     NOW HAVING I AND K VECTORS GET -- J = K CROSS I
C
      E(12) = E( 4)*E(17) - E(14)*E( 7)
      E(15) = E(11)*E( 7) - E( 1)*E(17)
      E(18) = E( 1)*E(14) - E(11)*E( 4)
C
C     NORMALIZE J-VECTOR FOR COMPUTER EXACTNESS JUST TO MAKE SURE
C
      TEMP  = SQRT(E(12)**2 + E(15)**2 + E(18)**2)
      E(12) = E(12)/TEMP
      E(15) = E(15)/TEMP
      E(18) = E(18)/TEMP
      E( 2) = 0.0
      E( 3) = 0.0
      E( 5) = 0.0
      E( 6) = 0.0
      E( 8) = 0.0
      E( 9) = 0.0
      E(10) = 0.0
      E(13) = 0.0
      E(16) = 0.0
C
C     CONVERT ANGLE FROM DEGREES TO RADIANS STORING IN THETA.
C
      THETA = ANGLE*DEGRA
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     SETTING UP G MATRIX
C
   30 MATID  = MATID1
      INFLAG = 2
      CALL MAT (ECPT(1))
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
C
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
C
C     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
C
      DO 50 I = 1,9
   50 D(I) = G(I)*EYE
      IF (IOPT .EQ. 0) CALL GMMATS (D,3,3,0, ALPHA1,3,1,0, ST(1))
C
      XBAR =(XSUBB + XSUBC)/3.0
      YBAR = YSUBC/3.0
      IF (IOPT .GT. 0) GO TO 60
      XC = XBAR
      YC = YBAR
C
C     FORMING K  5X6 AND STORING TEMPORARILY IN PH1OUT OUTPUT SPACE.
C              S                             (EQUIVALENCED)
C
   60 XC3 = 3.0*XC
      YC3 = 3.0*YC
      YC2 = 2.0*YC
      IF (STRAIN) GO TO 63
      KS( 1) = D(1)
      KS( 2) = D(3)
      KS( 3) = D(2)
      KS( 4) = D(1)*XC3
      KS( 5) = D(2)*XC + D(3)*YC2
      KS( 6) = D(2)*YC3
      KS( 7) = D(2)
      KS( 8) = D(6)
      KS( 9) = D(5)
      KS(10) = D(2)*XC3
      KS(11) = D(5)*XC + D(6)*YC2
      KS(12) = D(5)*YC3
      KS(13) = D(3)
      KS(14) = D(9)
      KS(15) = D(6)
      KS(16) = D(3)*XC3
      KS(17) = D(6)*XC + D(9)*YC2
      KS(18) = D(6)*YC3
C
C     ROWS 4 AND 5
C
      KS(19) = 0.0
      KS(20) = 0.0
      KS(21) = 0.0
      KS(22) =-D(1)*6.0
      KS(23) =-D(2)*2.0 - D(9)*4.0
      KS(24) =-D(6)*6.0
      KS(25) = 0.0
      KS(26) = 0.0
      KS(27) = 0.0
      KS(28) =-D(3)*6.0
      KS(29) =-D(6)*6.0
      KS(30) =-D(5)*6.0
      GO TO 67
   63 CONTINUE
      DO 65 I = 1,30
      KS(I) = 0.0
   65 CONTINUE
      KS( 1) = 1.0
      KS( 4) = XC3
      KS( 9) = 1.0
      KS(11) = XC
      KS(12) = YC3
      KS(14) = 0.5
      KS(17) = YC
   67 CONTINUE
C
C     MULTIPLY FIRST 3 ROWS BY 2.0
C
      DO 70 I = 1,18
   70 KS(I) = KS(I)*2.0
C
      XCSQ = XSUBC**2
      YCSQ = YSUBC**2
      XBSQ = XSUBB**2
      XCYC = XSUBC*YSUBC
C
C     F1LL  (HBAR) MATRIX STORING AT A(37) TRHU A(72)
C
      DO 90 I = 37,72
   90 A(I) = 0.0
C
      A(37) = XBSQ
      A(40) = XBSQ*XSUBB
      A(44) = XSUBB
      A(49) =-2.0*XSUBB
      A(52) =-3.0*XBSQ
      A(55) = XCSQ
      A(56) = XCYC
      A(57) = YCSQ
      A(58) = XCSQ*XSUBC
      A(59) = YCSQ*XSUBC
      A(60) = YCSQ*YSUBC
      A(62) = XSUBC
      A(63) = YSUBC*2.
      A(65) = XCYC*2.0
      A(66) = YCSQ*3.0
      A(67) =-2.0*XSUBC
      A(68) =-YSUBC
      A(70) =-3.0*XCSQ
      A(71) =-YCSQ
C
      IF (T2 .EQ. 0.0) GO TO 110
C
C     ALL OF THE FOLLOWING OPERATIONS THROUGH STATEMENT LABEL 500
C     ARE NECESSARY IF T2 IS NON-ZERO.
C
C     GET THE G2X2 MATRIX
C
      MATID  = MATID2
      INFLAG = 3
      CALL MAT (ECPT(1))
      IF (G2X211.EQ.0.0 .AND. G2X212.EQ.0.0 .AND. G2X222.EQ.0.0)
     1    GO TO 110
      G2X2(1) = G2X211*T2
      G2X2(2) = G2X212*T2
      G2X2(3) = G2X212*T2
      G2X2(4) = G2X222*T2
C
      DETERM  = G2X2(1)*G2X2(4) - G2X2(3)*G2X2(2)
      J2X2(1) = G2X2(4)/DETERM
      J2X2(2) =-G2X2(2)/DETERM
      J2X2(3) =-G2X2(3)/DETERM
      J2X2(4) = G2X2(1)/DETERM
C
C     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
C       YQ  RIGHT PORTION IS COMPUTED AND USED AS A  (2X3). THE LEFT
C           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
C           A(73)...A(78) UNTIL NOT NEEDED ANY FURTHER.
C
      TEMP  =  2.0*D(2) + 4.0*D(9)
      A(73) = -6.0*(J2X2(1)*D(1) + J2X2(2)*D(3))
      A(74) = -J2X2(1)*TEMP + 6.0*J2X2(2)*D(6)
      A(75) = -6.0*(J2X2(1)*D(6) + J2X2(2)*D(5))
      A(76) = -6.0*(J2X2(2)*D(1) + J2X2(4)*D(3))
      A(77) = -J2X2(2)*TEMP + 6.0*J2X2(4)*D(6)
      A(78) = -6.0*(J2X2(2)*D(6) + J2X2(4)*D(5))
C
C     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
C                                              YQ
C
C     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF(H  )(H  )
C                                                    UY   YQ
C     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
C     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
C          UY
C
C     THE FOLLOWING IS THEN PER STEPS 6 AND 7 PAGE -16- MS-17.
C
      DO 100 I = 1,3
      A(I+39) = A(I+39) + XSUBB*A(I+72)
  100 A(I+57) = A(I+57) + XSUBC*A(I+72) + YSUBC*A(I+75)
C
C     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
C
  110 CONTINUE
C
C     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
C     STORE INVERSE BACK IN A(37) THRU A(72)
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (6,A(37),6,A(73),0,DETERM,ISING,A(79))
C
C     CHECK TO SEE IF H WAS SINGULAR
C
      IF (ISING .NE. 2) GO TO 120
C
C
C     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
C
      CALL MESAGE (-30,38,ECPT(1))
C
C     SAVE H-INVERSE IF TRI-PLATE IS CALLING
C
  120 DO 130 I = 1,36
  130 HINV(I) = A(I+36)
C
C     FILL  S-MATRIX, EQUIVALENCED TO A(55).  (6X3)
C
      S( 1) = 1.0
      S( 2) = 0.0
      S( 3) =-XSUBB
      S( 4) = 0.0
      S( 5) = 1.0
      S( 6) = 0.0
      S( 7) = 0.0
      S( 8) = 0.0
      S( 9) = 1.0
      S(10) = 1.0
      S(11) = YSUBC
      S(12) =-XSUBC
      S(13) = 0.0
      S(14) = 1.0
      S(15) = 0.0
      S(16) = 0.0
      S(17) = 0.0
      S(18) = 1.0
C
C     COMPUTE  S , S ,  AND S    NO TRANSFORMATIONS
C               A   B        C
C
C                -1
C     S  = - K  H  S ,   S  = K  H   ,   S  = K  H
C      A      S           B    S  IB      C    S  IC
C
C     S   COMPUTATION.
C      A
C
      CALL GMMATS (HINV(1),6,6,0, S(1),6,3,0, A(16))
C
C     DIVIDE  H-INVERSE INTO A LEFT 6X3 AND RIGHT 6X3 PARTITION.
C
      I = 0
      J =-6
  150 J = J + 6
      K = 0
  160 K = K + 1
      I = I + 1
      ISUB = J + K
      HIB(I) = HINV(ISUB    )
      HIC(I) = HINV(ISUB + 3)
      IF (K .LT. 3 ) GO TO 160
      IF (J .LT. 30) GO TO 150
C
      CALL GMMATS (KS(1),5,6,0, A(16),6,3,0, A(1))
C
C     MULTIPLY S SUB A BY (-1)
C
      DO 170 I = 1,15
  170 A(I) = -A(I)
C
C     S  COMPUTATION
C      B
C
      CALL GMMATS (KS,5,6,0, HIB,6,3,0, A(16))
C
C     S  COMPUTATION
C      C
C
      CALL GMMATS (KS,5,6,0, HIC,6,3,0, A(31))
C
C     RETURN IF TRI OR QUAD PLATE ROUTINE IS CALLING.
C
      IF (IOPT .GT. 0) RETURN
C                                   T
C     TRANSFORM  S , S , S  WITH   E  T  , I = A,B,C
C                 A   B   C            I
C                              T         T
C     COMPUTING TRANSPOSE OF  E  T  =  T  E
C                                 I     I
      DO 200 I = 1,3
C
C     POINTER TO S MATRIX = 15*I - 14
C                 I
C     POINTER TO OUTPUT POSITION = 30*I - 21
C
C     CHECK TO SEE IF T IS NEEDED.
C
      IF (NECPT(4*I+9)) 180,190,180
  180 CALL TRANSS (NECPT(4*I+9),T)
      CALL GMMATS (T,3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATS (T,3,3,1, E(10),3,3,0, TITE(10))
      CALL GMMATS (A(15*I-14),5,3,0, TITE,6,3,1, PH1OUT(30*I-21))
      GO TO 200
  190 CALL GMMATS (A(15*I-14),5,3,0, E,6,3,1, PH1OUT(30*I-21))
  200 CONTINUE
      PH1OUT(1) = ECPT(1)
      PH1OUT(2) = ECPT(2)
      PH1OUT(3) = ECPT(3)
      PH1OUT(4) = ECPT(4)
C
C     PH1OUT(5) IS A DUMMY
C
      PH1OUT(6) = ECPT( 7)
      PH1OUT(7) = ECPT(11)
      PH1OUT(8) = ECPT(12)
C
C     PHASE I   BASIC BENDING TRIANGLE  SDR2 COMPLETE
C
      RETURN
      END
