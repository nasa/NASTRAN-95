      SUBROUTINE SQDPL1
C
C     PHASE I OF STRESS DATA RECOVERY FOR TRI OR QUAD PLATE.
C
C     OUTPUTS FROM THIS PHASE FOR USE IN PHASE II ARE THE FOLLOWING.
C
C     1) ELEMENT ID
C     2) 4 SILS
C     3) I
C     4) Z1 AND Z2
C     5) 4  5X6 S-SUB-I ARRAYS
C     6) 3X1 S SUB T MATRIX
C     THUS, 131 WORDS FOR QUAD-PLATE
C
C     ECPT LISTS AS OF AUGUST 4, 1967
C
C                 DEFINITION                   DEFINITION
C       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
C     --------   --------------------------    -------------------------
C     ECPT( 1) = ELEMENT ID         INTEGER ** ELEMENT           INTEGER
C     ECPT( 2) = GRID PT. A         INTEGER ** GRID PT.A         INTEGER
C     ECPT( 3) = GRID PT. B         INTEGER ** GRID PT.B         INTEGER
C     ECPT( 4) = GRID PT. C         INTEGER ** GRID PT.C         INTEGER
C     ECPT( 5) = THETA              REAL    ** GRID PT.D         INTEGER
C     ECPT( 6) = MAT ID 1           INTEGER ** THETA             REAL
C     ECPT( 7) = I  MOM. OF INERT.  REAL    ** MAT ID 1          INTEGER
C     ECPT( 8) = MAT ID 2           INTEGER ** I  MOM. OF INERT. REAL
C     ECPT( 9) = T2                 REAL    ** MAT ID 2          INTEGER
C     ECPT(10) = NON-STRUCT. MASS   REAL    ** T2                REAL
C     ECPT(11) = Z1                 REAL    ** NON-STRUCT. MASS  REAL
C     ECPT(12) = Z2                 REAL    ** Z1                REAL
C     ECPT(13) = COORD. SYS. ID 1   INTEGER ** Z2                REAL
C     ECPT(14) = X1                 REAL    ** COORD. SYS. ID 1  INTEGER
C     ECPT(15) = Y1                 REAL    ** X1                REAL
C     ECPT(16) = Z1                 REAL    ** Y1                REAL
C     ECPT(17) = COORD. SYS. ID 2   INTEGER ** Z1                REAL
C     ECPT(18) = X2                 REAL    ** COORD. SYS. ID 2  INTEGER
C     ECPT(19) = Y2                 REAL    ** X2                REAL
C     ECPT(20) = Z2                 REAL    ** Y2                REAL
C     ECPT(21) = COORD. SYS. ID 3   INTEGER ** Z2                REAL
C     ECPT(22) = X3                 REAL    ** COORD. SYS. ID 3  INTEGER
C     ECPT(23) = Y3                 REAL    ** X3                REAL
C     ECPT(24) = Z3                 REAL    ** Y3                REAL
C     ECPT(25) = ELEMENT TEMP       REAL    ** Z3                REAL
C     ECPT(26) =                            ** COORD. SYS. ID 4  INTEGER
C     ECPT(27) =                            ** X4                REAL
C     ECPT(28) =                            ** Y4                REAL
C     ECPT(29) =                            ** Z4                REAL
C     ECPT(30) =                            ** ELEMENT TEMP      REAL
C
      INTEGER         SUBSCA,SUBSCB,SUBSCC
      REAL            IVECT,JVECT,KVECT,D(9)
      DIMENSION       NECPT(100),M(12),VQ1(3),VQ2(3),VQ3(3),VQ4(3),
     1                REQUIV(10)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X5/ ECPT(100),PH1OUT(128),ST(3)
      COMMON /SDR2X6/ A(45),TEMP15(15),PROD15(15),T(9),TITE(18),V(25),
     1                D1(3),D2(3),SPDUM1(18),U1,U2,SINANG,COSANG,
     2                SSUM(60),R(2,5),XSUBB,XSUBC,YSUBC,E(18),TEMP,
     3                VV1(2),VV2(2),H,A1(3),NPOINT,SPDUM2(5),IVECT(3),
     4                JVECT(3),KVECT(3),SPDUM3(15),THETA,NSUBC,
     5                SPDUM4(1),SUBSCA,SUBSCB,SUBSCC,SPDUM5(2),XC,YC,
     6                SPDUM6(5)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHA(3)
      EQUIVALENCE     (CONSTS(4),DEGRA),(ECPT(1),NECPT(1)),
     1                (VQ1(1),ECPT(15)),(VQ2(1),ECPT(19)),
     2                (VQ3(1),ECPT(23)),(VQ4(1),ECPT(27)),
     3                (REQUIV(1),R(1,1))
      DATA    M     / 2,4,1,  3,1,2,  4,2,3,  1,3,4 /
C
      IDSAVE = NECPT(7)
      EYE    = ECPT(8)
      THETA  = ECPT(6)*DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
C
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES. (2X5) FOR QUADRILATERAL PLATE.
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
C
      DO 10 I = 1,10
   10 REQUIV(I) = 0.0
C
C     SHIFT ECPT UP TO MATCH STRBS1 FOR CERTAIN VARIABLES.
C
      DO 30 I = 6,12
   30 ECPT(I) = ECPT(I+1)
C
      DO 40 I = 1,3
      D1(I) = VQ3(I) - VQ1(I)
      D2(I) = VQ4(I) - VQ2(I)
   40 A1(I) = VQ2(I) - VQ1(I)
C
C     NON-NORMALIZED K-VECTOR = D1 CROSS D2
C
      KVECT(1) = D1(2)*D2(3) - D2(2)*D1(3)
      KVECT(2) = D1(3)*D2(1) - D2(3)*D1(1)
      KVECT(3) = D1(1)*D2(2) - D2(1)*D1(2)
C
C     NORMALIZE K-VECTOR
C
      TEMP = SQRT(KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2)
      DO 50 I = 1,3
   50 KVECT(I) = KVECT(I)/TEMP
C
C     COMPUTE H = (A1 DOT KVECT)/2
C
      TEMP = (A1(1)*KVECT(1) + A1(2)*KVECT(2) + A1(3)*KVECT(3))/2.0
C
C     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
C
      DO 60 I = 1,3
   60 IVECT(I) = A1(I) - TEMP*KVECT(I)
C
C     NORMALIZE I-VECTOR
C
      TEMP =  SQRT(IVECT(1)**2 + IVECT(2)**2 + IVECT(3)**2)
      DO 70 I = 1,3
   70 IVECT(I) = IVECT(I)/TEMP
C
C     J-VECTOR = K X I  VECTORS
C
      JVECT(1) = KVECT(2)*IVECT(3) - IVECT(2)*KVECT(3)
      JVECT(2) = KVECT(3)*IVECT(1) - IVECT(3)*KVECT(1)
      JVECT(3) = KVECT(1)*IVECT(2) - IVECT(1)*KVECT(2)
C
C     NORMALIZE J VECTOR TO MAKE SURE
C
      TEMP =  SQRT(JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2)
      DO 80 I = 1,3
   80 JVECT(I) = JVECT(I)/TEMP
C
C     X3 GOES INTO R(1,3) = D1 DOT IVECT
C
      R(1,3) = D1(1)*IVECT(1) + D1(2)*IVECT(2) + D1(3)*IVECT(3)
C
C     X2 GOES INTO R(1,2) AND Y3 GOES INTO R(2,3)
C
      R(1,2) = A1(1)*IVECT(1) + A1(2)*IVECT(2) + A1(3)*IVECT(3)
      R(2,3) = D1(1)*JVECT(1) + D1(2)*JVECT(2) + D1(3)*JVECT(3)
C
C     X4 GOES INTO R(1,4) AND Y4 GOES INTO R(2,4)
C
      R(1,4) = D2(1)*IVECT(1) + D2(2)*IVECT(2) + D2(3)*IVECT(3) + R(1,2)
      R(2,4) = D2(1)*JVECT(1) + D2(2)*JVECT(2) + D2(3)*JVECT(3)
C
C     STRESS CALCULATION POINT WHICH IS THE DIAGONALS INTERSECTION.
C
      FTEMP  = R(1,3)*R(2,4) + R(2,3)*(R(1,2)-R(1,4))
      IF (FTEMP .EQ. 0.0) CALL MESAGE (-30,26,ECPT(1))
      R(1,5) = R(1,2)*R(1,3)*R(2,4)/FTEMP
      R(2,5) = R(1,2)*R(2,3)*R(2,4)/FTEMP
C
C     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
C
      IF (R(2,3).LE.0.0 .OR. R(2,4).LE.0.0) GO TO 90
      TEMP = R(1,2) - (R(1,2)-R(1,3))*R(2,4)/R(2,3)
      IF (R(1,4) .GE. TEMP) GO TO 90
      TEMP = R(2,3)*R(1,4)/R(2,4)
      IF (R(1,3) .GT. TEMP) GO TO 100
   90 CALL MESAGE (-30,35,ECPT(1))
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT
C
C     COMPUTE SUB-TRIANGLE COORDINATES
C     CALL BASIC BENDING ROUTINE FOR ALL SUB-TRIANGLES.
C
  100 ELTEMP = ECPT(30)
      DO 110 I = 1,60
  110 SSUM(I) = 0.0
C
      DO 160 J = 1,4
      KM     = 3*J - 3
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 120 I = 1,2
      VV1(I) = R(I,SUBSCB) - R(I,SUBSCA)
  120 VV2(I) = R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB  = SQRT(VV1(1)**2 + VV1(2)**2)
      U1     = VV1(1)/XSUBB
      U2     = VV1(2)/XSUBB
      XSUBC  = U1*VV2(1) + VV2(2)*U2
      YSUBC  = U1*VV2(2) - VV2(1)*U2
C
      XC    = SQRT((R(1,SUBSCA)-R(1,5))**2 + (R(2,SUBSCA)-R(2,5))**2)
      YC    = 0.0
C
      SINTH = SINANG*U1 - COSANG*U2
      COSTH = COSANG*U1 + SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
C     TRIANGLE -J-
C
      CALL STRBS1 (1)
C
C     RETURNING FROM STRBS1 THE FOLLOWING QUANTITIES ARE AT HAND.
C
C       S   , S   , S   , EACH 5X3.   45 WORDS STORED IN A( 1)...A(45)
C        A     B     C
C
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
      V( 1) = U1*U1*0.25
      V( 2) = U2*U2*0.25
      V(11) = U1*U2*0.25
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
      V(19) = U1*0.25
      V(20) =-U2*0.25
      V(21) = 0.0
      V(22) = 0.0
      V(23) = 0.0
      V(24) =-V(20)
      V(25) = V(19)
C
C     ADD IN S , S , S   TO THE 4 5X3 SSUM MATRICES
C             A   B   C
C
      DO 150 I = 1,3
      CALL GMMATS (V,5,5,0, A(15*I-14),5,3,0, TEMP15)
      CALL GMMATS (TEMP15,5,3,0, T,3,3,0, PROD15)
C
C     POINTER TO SSUM MATRIX
C
      NPOINT = KM + I
      NPOINT = 15*M(NPOINT) - 15
      DO 140 K = 1,15
      NSUBC = NPOINT + K
  140 SSUM(NSUBC) = SSUM(NSUBC) + PROD15(K)
  150 CONTINUE
C
  160 CONTINUE
C
C     FILL E-MATRIX
C
      DO 170 I = 1,18
  170 E( I) = 0.0
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
      DO 210 I = 1,4
C
C     DO WE NEED TRANSFORMATION T
C                                I
      NSUBC = 4*I + 10
      IF (NECPT(NSUBC) .EQ. 0) GO TO 180
      CALL TRANSS (NECPT(NSUBC),T)
      CALL GMMATS (T,3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATS (T,3,3,1, E(10),3,3,0, TITE(10))
      GO TO 200
C
  180 DO 190 K = 1,18
  190 TITE(K) = E(K)
C
  200 CALL GMMATS (SSUM(15*I-14),5,3,0, TITE,6,3,1, PH1OUT(30*I-21))
C
  210 CONTINUE
C
C     I,Z1,Z2,ELEM ID, 4 SILS FOR PHASE 2
C
      PH1OUT(1) = ECPT( 1)
      PH1OUT(2) = ECPT( 2)
      PH1OUT(3) = ECPT( 3)
      PH1OUT(4) = ECPT( 4)
      PH1OUT(5) = ECPT( 5)
      PH1OUT(6) = ECPT( 7)
      PH1OUT(7) = ECPT(11)
      PH1OUT(8) = ECPT(12)
C
C     GET S SUB T MATRIX
C
      MATID   = IDSAVE
      ECPT(8) = EYE
      STRESS  = 0
      SINTH   = SINANG
      COSTH   = COSANG
      INFLAG  = 2
      CALL MAT (ECPT(1))
      D(1) = G11*ECPT(8)
      D(2) = G12*ECPT(8)
      D(3) = G13*ECPT(8)
      D(4) = D(2)
      D(5) = G22*ECPT(8)
      D(6) = G23*ECPT(8)
      D(7) = D(3)
      D(8) = D(6)
      D(9) = G33*ECPT(8)
      CALL GMMATS (D(1),3,3,0, ALPHA(1),3,1,0, ST(1))
C
C     ALL PHASE ONE COMPLETE
C
      RETURN
      END
