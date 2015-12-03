      SUBROUTINE TRPLT (TI)
C
C     ELEMENT THERMAL LOADING FOR THE BENDING TRIANGULAR PLATE.
C
C                 DEFINITION
C       ECPT      BSC.BEND.TRI. AND THE TRI-PLATE
C     --------   ---------------------------------------
C     ECPT( 1) = ELEMENT ID         INTEGER
C     ECPT( 2) = GRID PT. A         INTEGER
C     ECPT( 3) = GRID PT. B         INTEGER
C     ECPT( 4) = GRID PT. C         INTEGER
C     ECPT( 5) = THETA              REAL
C     ECPT( 6) = MAT ID 1           INTEGER
C     ECPT( 7) = I  MOM. OF INERT.  REAL
C     ECPT( 8) = MAT ID 2           INTEGER
C     ECPT( 9) = T2                 REAL
C     ECPT(10) = NON-STRUCT. MASS   REAL
C     ECPT(11) = Z1                 REAL
C     ECPT(12) = Z2                 REAL
C     ECPT(13) = COORD. SYS. ID 1   INTEGER
C     ECPT(14) = X1                 REAL
C     ECPT(15) = Y1                 REAL
C     ECPT(16) = Z1                 REAL
C     ECPT(17) = COORD. SYS. ID 2   INTEGER
C     ECPT(18) = X2                 REAL
C     ECPT(19) = Y2                 REAL
C     ECPT(20) = Z2                 REAL
C     ECPT(21) = COORD. SYS. ID 3   INTEGER
C     ECPT(22) = X3                 REAL
C     ECPT(23) = Y3                 REAL
C     ECPT(24) = Z3                 REAL
C     ECPT(25) = ELEMENT TEMP       REAL
C
      INTEGER         SUBSCA,SUBSCB,SUBSCC
      REAL            L1,L2,KS,KHI,TI(6),IVECT,JVECT,KVECT
      DIMENSION       M(9),REQUIV(9),G(36),TITE(10),V(25),HQ(12),
     1                TEMP15(15),PROD15(15),NECPT(25),V1(3),V2(3),V3(3)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /TRIMEX/ ECPT(100)
      COMMON /SSGWRK/ A(45),T(9),S(18),HINV(36),PROD12(12),D1(3),D2(3),
     1                HABC(18),SSUM(60),R(2,4),IVECT(3),JVECT(3),
     2                KVECT(3),VV1(2),VV2(2),XSUBB,XSUBC,YSUBC,E(18),
     3                TEMP,L1,L2,C1,C2,S1,S2,X1,X2,Y1,Y2,NPOINT,DUM9,
     4                TEMP1,TEMP2,PROD9(9),TEMP9(9),DUM8,KM,SUBSCA,
     5                SUBSCB,SUBSCC,DUM11,THETA,NSUBC,ISING,U1,U2,
     6                SINANG,COSANG,DUM10,XC,YC,DETERM,DUM12(4)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /SSGTRI/ D(9),KHI(5),KS(30),P(5)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (CONSTS(4),DEGRA),(PROD15(1),PROD9(1)),
     1                (REQUIV(1),R(1,1)),(NECPT(1),ECPT(1)),
     2                (V1(1),ECPT(14)),(V2(1),ECPT(18)),
     3                (V3(1),ECPT(22)),(TITE(1),A(1)),
     4                (V(1),PROD12(1)),(HQ(1),A(1))
      DATA    M     / 1,2,4, 2,3,4, 3,1,4 /
C
      ELTEMP = ECPT(25)
      THETA  = ECPT(5)*DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
C
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES. (2X4) FOR THE TRIANGULAR PLATE.
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
C
      DO 10 I = 1,8
   10 REQUIV(I) = 0.0
C
      DO 20 I = 1,3
      D2(I) = V2(I) - V1(I)
   20 D1(I) = V3(I) - V1(I)
C
C     X2  GOES IN R(1,2)
C
      R(1,2) = SQRT(D2(1)**2 + D2(2)**2 + D2(3)**2)
      DO 30 I = 1,3
   30 IVECT(I) = D2(I)/R(1,2)
C
C     NON-NORMALIZED K-VECTOR
C
      KVECT(1) = IVECT(2)*D1(3) - D1(2)*IVECT(3)
      KVECT(2) = IVECT(3)*D1(1) - D1(3)*IVECT(1)
      KVECT(3) = IVECT(1)*D1(2) - D1(1)*IVECT(2)
C
C     Y3 GOES INTO R(2,3)
C
      R(2,3) =  SQRT(KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2)
      DO 40 I = 1,3
   40 KVECT(I) = KVECT(I)/R(2,3)
C
C     J-VECTOR = K X I  VECTORS
C
      JVECT(1) = KVECT(2)*IVECT(3) - IVECT(2)*KVECT(3)
      JVECT(2) = KVECT(3)*IVECT(1) - IVECT(3)*KVECT(1)
      JVECT(3) = KVECT(1)*IVECT(2) - IVECT(1)*KVECT(2)
C
C     NORMALIZE J VECTOR TO MAKE SURE
C
      TEMP = SQRT(JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2)
      DO 60 I = 1,3
   60 JVECT(I) = JVECT(I)/TEMP
C
C     X3 GOES INTO R(1,3) = D1 DOT IVECT
C
      R(1,3) = D1(1)*IVECT(1) + D1(2)*IVECT(2) + D1(3)*IVECT(3)
C
C     CENTROID POINT GOES INTO R(1,4) AND R(2,4)
C
      R(1,4) = (R(1,2) + R(1,3))/3.0
      R(2,4) = R(2,3)/3.0
C
C     COMPUTE SUB-TRIANGLE COORDINATES
C     CALL BASIC BENDING ROUTINE FOR ALL SUB-TRIANGLES.
C
      DO 80 I = 1,60
   80 SSUM(I) = 0.0
      DO 90 I = 1,36
   90 G(I) = 0.0
C
      DO 180 J = 1,3
      KM = 3*J - 3
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 100 I = 1,2
      VV1(I) = R(I,SUBSCB) - R(I,SUBSCA)
  100 VV2(I) = R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB  = SQRT(VV1(1)**2 + VV1(2)**2)
      U1 = VV1(1)/XSUBB
      U2 = VV1(2)/XSUBB
      XSUBC = U1*VV2(1) + VV2(2)*U2
      YSUBC = U1*VV2(2) - VV2(1)*U2
C
      XC = XSUBC
      YC = YSUBC
C
      SINTH = SINANG*U1 - COSANG*U2
      COSTH = COSANG*U1 + SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
C     TRIANGLE -J-
C
      CALL TRBSC (2,TI(1))
C
C     RETURNING FROM STRBS1 THE FOLLOWING QUANTITIES ARE AT HAND.
C
C       S   , S   , S   , EACH 5X3.   45 WORDS STORED IN A( 1)...A(45)
C        A     B     C
C
C     AND ALSO H-INVERSE IS AT A(73)...A(108) AND S IS AT A(55)...A(72)
C
C     COMPUTE KHI   (5X1)  MATRIX
C                E
C
C     THIS WILL BE USED AT THE END OF THE INTERMEDIATE COMPUTATIONS.
C     KHI-SUB-E MUST BE COMPUTED AFTER THE FIRST SUBTRIANGLE IN ORDER
C     TO USE THE -D- MATERIAL MATRIX WITH THE CORRECT ORIENTATION.
C
C     NFACTOR = 3.0 FOR THE CLOUGH TRIANGLE
C
      IF (J .EQ. 1) CALL SSGKHI (TI(1),TI(1),3.0)
C
C     SET UP OF T-MATRIX
C
      T(1) = 1.0
      T(2) = 0.0
      T(3) = 0.0
      T(4) = 0.0
      T(5) = U1
      T(6) = U2
      T(7) = 0.0
      T(8) =-U2
      T(9) = U1
C
C     SET UP V-MATRIX PER FMMS 51-A
C
      V( 1) = U1*U1/3.0
      V( 2) = U2*U2/3.0
      V(11) = U1*U2/3.0
      V( 3) =-V(11)*2.0
      V( 4) = 0.0
      V( 5) = 0.0
      V( 6) = V(2)
      V( 7) = V(1)
      V( 8) =-V(3)
      V( 9) = 0.0
      V(10) = 0.0
      V(12) =-V(11)
      V(13) = V(1) - V(2)
      V(14) = 0.0
      V(15) = 0.0
      V(16) = 0.0
      V(17) = 0.0
      V(18) = 0.0
      V(19) = U1/3.0
      V(20) =-U2/3.0
      V(21) = 0.0
      V(22) = 0.0
      V(23) = 0.0
      V(24) =-V(20)
      V(25) = V(19)
C
C     ADD IN S , S , S   TO THE 4 5X3 SSUM MATRICES
C             A   B   C
C
      DO 120 I = 1,3
      CALL GMMATS (V(1),5,5,0, A(15*I-14),5,3,0, TEMP15(1))
      CALL GMMATS (TEMP15(1),5,3,0, T(1),3,3,0,  PROD15(1))
C
C     POINTER TO SSUM MATRIX
C
      NPOINT = KM + I
      NPOINT = 15*M(NPOINT) - 15
      DO 110 K = 1,15
      NSUBC = NPOINT + K
  110 SSUM(NSUBC) = SSUM(NSUBC) + PROD15(K)
  120 CONTINUE
C
C     FORM HQ (2X6)
C
      TEMP1  = XSUBB - XSUBC
      TEMP2  = YSUBC**2
      L1     = SQRT(XSUBC**2 + TEMP2)
      L2     = SQRT(TEMP1**2 + TEMP2)
      S1     = XSUBC/L1
      S2     = TEMP1/L2
      C1     = YSUBC/L1
      C2     = YSUBC/L2
      X1     = XSUBC/2.0
      Y1     = YSUBC/2.0
      X2     = (XSUBB + XSUBC)/2.0
      Y2     = Y1
      HQ( 1) =-XSUBC*C1
      HQ( 2) = X1*S1 - Y1*C1
      HQ( 3) = 2.0*Y1*S1
      HQ( 4) =-3.0*X1*X1*C1
      HQ( 5) = Y1*(2.0*X1*S1 - Y1*C1)
      HQ( 6) = 3.0*Y1*Y1*S1
      HQ( 7) = 2.0*X2*C2
      HQ( 8) = X2*S2 + Y2*C2
      HQ( 9) = 2.0*Y2*S2
      HQ(10) = 3.0*X2*X2*C2
      HQ(11) = Y2*(2.0*X2*S2 + Y2*C2)
      HQ(12) = 3.0*Y2*Y2*S2
C
C                      I                    -1
C     COMPUTE (H       I  H     )  = (HQ)(H)    STORE IN PROD12
C               PSI,B  I   PSI,C
C                      I
C
C
      CALL GMMATS( HQ(1),2,6,0,  HINV(1),6,6,0,  PROD12(1) )
C
C
C     COMPUTE (H     ) = -(PROD12)(S)
C               PSI,A
C
      CALL GMMATS (PROD12(1),2,6,0, S(1),6,3,0, HABC(1))
      HABC(1) = -HABC(1)
      HABC(2) = -HABC(2) + S1
      HABC(3) = -HABC(3) + C1
      HABC(4) = -HABC(4)
      HABC(5) = -HABC(5) + S2
      HABC(6) = -HABC(6) - C2
C
C     SPLIT(H     ) AND (H     )  PARTITION
C            PSI,B        PSI,C
C
      HABC( 7) = PROD12( 1)
      HABC( 8) = PROD12( 2)
      HABC( 9) = PROD12( 3)
      HABC(10) = PROD12( 7)
      HABC(11) = PROD12( 8)
      HABC(12) = PROD12( 9)
      HABC(13) = PROD12( 4)
      HABC(14) = PROD12( 5)
      HABC(15) = PROD12( 6)
      HABC(16) = PROD12(10)
      HABC(17) = PROD12(11)
      HABC(18) = PROD12(12)
C
C     MAP  H , H , AND H  INTO THE G-MATRICES.
C           A   B       C
C
C     TRIANGLE NUMBER = J, THE THREE POINTS ARE SUBSCA,SUBSCB,SUBSCC.
C
      DO 170 I = 1,3
C
C     POINTER TO H  = 6*I - 6
C                 I
C
C     TRANSFORM H SUB I
C
      CALL GMMATS (HABC(6*I-5),2,3,0, T(1),3,3,0, TEMP9(1))
C
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) - 9
C
C     J = 1  ROW 1 OF H INTO ROW 1 OF G.
C            ROW 2 OF H INTO ROW 2 OF G.
C     J = 2  ROW 1 OF H INTO ROW 2 OF G.
C            ROW 2 OF H INTO ROW 3 OF G.
C     J = 3  ROW 1 OF H INTO ROW 3 OF G.
C            ROW 2 OF H INTO ROW 1 OF G.
C
      IF (J - 2) 140,130,160
C
  130 NPOINT = NPOINT + 3
  140 DO 150 K = 1,6
      NPOINT = NPOINT + 1
  150 G(NPOINT) = G(NPOINT) + TEMP9(K)
      GO TO 170
  160 G(NPOINT+7) = G(NPOINT+7) + TEMP9(1)
      G(NPOINT+8) = G(NPOINT+8) + TEMP9(2)
      G(NPOINT+9) = G(NPOINT+9) + TEMP9(3)
      G(NPOINT+1) = G(NPOINT+1) + TEMP9(4)
      G(NPOINT+2) = G(NPOINT+2) + TEMP9(5)
      G(NPOINT+3) = G(NPOINT+3) + TEMP9(6)
C
  170 CONTINUE
C
  180 CONTINUE
C
C     FILL E-MATRIX
C
      DO 190 I = 1,18
  190 E( I) = 0.0
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
C               *         *     -1
C     (S ) = (S  )  -  (S  )(G )  (G )           I=A,B,C
C       I      I         4    4     I
C
C
C        E            T                  T
C     (S  ) = (S ) (E) (C ) = (S ) (TITE)    I=A,B,C
C       I       I        I      I
C
C                                 *     -1
C     FIRST GET COMMON PRODUCT (S  )(G )
C                                4    4
C
C     INVERT  (G )  STORE INVERSE BACK INTO  (G )
C               4                              4
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,G(28),3,PROD9(1),0,DETERM,ISING,TEMP9(1))
C
C     CHECK FOR SINGULARITY.  ISING = 2 IMPLIES SINGULARITY...
C
      GO TO (210,200), ISING
  200 CALL MESAGE (-30,36,ECPT(1))
C
  210 CALL GMMATS (SSUM(46),5,3,0, G(28),3,3,0, PROD15(1))
C
      DO 260 I = 1,3
C
C    (PROD15) (G )
C               I
C
      CALL GMMATS (PROD15(1),5,3,0, G(9*I-8),3,3,0, TEMP15(1))
C
C     SUBTRACT TEMP15 FROM S
C                          I
C
      NPOINT = 15*I - 15
      DO 220 K = 1,15
      NPOINT = NPOINT + 1
  220 SSUM(NPOINT) = SSUM(NPOINT) - TEMP15(K)
C
C     DO WE NEED TRANSFORMATION T
C                                I
      NSUBC = 4*I + 9
      IF (NECPT(NSUBC) .EQ. 0) GO TO 230
      CALL GBTRAN (NECPT(NSUBC), NECPT(NSUBC+1), T(1))
      CALL GMMATS (T(1),3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATS (T(1),3,3,1, E(10),3,3,0, TITE(10))
      GO TO 250
C
  230 DO 240 K = 1,18
  240 TITE(K) = E(K)
C
  250 CALL GMMATS (SSUM(15*I -14),5,3,0, TITE(1),6,3,1, KS(1))
C
C     COMPUTE THE LOAD VECTOR AND INSERT IT INTO OPEN CORE.
C
      CALL GMMATS (KS(1),5,6,1, KHI(1),5,1,0, P(1))
      K = NECPT(I+1) - 1
      DO 255 L = 1,6
      K = K + 1
  255 Z(K) = Z(K) + P(L)
C
  260 CONTINUE
C
      RETURN
      END
