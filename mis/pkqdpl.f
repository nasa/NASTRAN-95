      SUBROUTINE PKQDPL
C
C     THIS ROUTINE CALCULATES THE STIFFNESS MATRIX FOR QUAD-PLATES IN
C     PLA4
C
C     THIS ROUTINE GENERATES THE FOLLOWING
C
C     FOUR 6X6 STIFFNESS MATRICES WITH RESPECT TO ONE PIVOT POINT OF A
C     QUADRILATERAL PLATE ELEMENT.
C
C     REF.  FMMS-44   JULY  18, 1967   TRI.BENDING ELEMENT STIFF.
C           FMMS-48   AUGUST 1, 1967   QUAD. BENDING ELEMENT STIFF.
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C           PKTRBS - BASIC BENDING TRIANGLE
C           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
C           PLA4B  - INSERTION ROUTINE
C           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
C           MESAGE - ERROR MESSAGE WRITER
C
C
C     ECPT LISTS AS OF AUGUST 4, 1967
C
C                 DEFINITION                   DEFINITION
C       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
C     ==================================================================
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
      INTEGER          SUBSCA,SUBSCB,SUBSCC
      DOUBLE PRECISION KOUT,TITE,TJTE,DPDUM1,DPDUM2,D1,D2,IVECT,JVECT,
     1                 KVECT,A1,KSUM,T,V,VV,XSUBB,XSUBC,YSUBC,PROD9,
     2                 TEMP,TEMP9,H,U1,U2,E,A,TEMP18,REQUIV,R
      DIMENSION        M(12),NECPT(100),REQUIV(8),VQ1(3),VQ2(3),VQ3(3),
     1                 VQ4(3),A(1)
      COMMON /CONDAS/  CONSTS(5)
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                 T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,
     2                 G2X211,G2X212,G2X222
      COMMON /PLA42C/  NPVT,DUM1(148),NOGO
      COMMON /PLA4ES/  ECPT(100)
      COMMON /PLA42D/  KOUT(36),TITE(18),TJTE(18),TEMP18(18),DPDUM1(54),
     1                 IVECT(3),JVECT(3),KVECT(3),D1(3),D2(3),A1(3),
     2                 T(9),V(2),VV(2),H,U1,U2,R(2,4),KSUM(36),
     3                 DPDUM2(3),PROD9(9),TEMP9(9),XSUBB,XSUBC,YSUBC,
     4                 E(18),TEMP,SP1(28),SP2(2),KM,NBEGIN,JNOT,NPIVOT,
     5                 THETA,NSUBC,ISING,SUBSCA,SUBSCB,SUBSCC,SINANG,
     6                 COSANG,NPOINT
      EQUIVALENCE      (CONSTS(4),DEGRA),(NECPT(1),ECPT(1)),
     1                 (R(1,1),REQUIV(1)),(VQ1(1),ECPT(15)),
     4                 (VQ2(1),ECPT(19)),(VQ3(1),ECPT(23)),
     6                 (VQ4(1),ECPT(27)),(A(1),KOUT(1))
      DATA    M     /  2,4,1, 3,1,2, 4,2,3, 1,3,4 /
C
C     DETERMINE PIVOT POINT NUMBER
C
      DO 10 I = 1,4
      IF (NPVT .NE. NECPT(I+1)) GO TO 10
      NPIVOT = I
      GO TO 20
   10 CONTINUE
C
C     FALL THRU ABOVE LOOP IMPLIES ERROR CONDITION
C
      CALL MESAGE (-30,34,ECPT(1))
C
   20 THETA  = ECPT(6)*DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
C
      IF (NPIVOT-2) 30,30,40
   30 JNOT = NPIVOT + 2
      GO TO 50
   40 JNOT = NPIVOT - 2
C
C     FORMATION OF THE R-MATRIX CONTAINING COORDINATES OF THE
C     SUB TRIANGLES.  (2X4) FOR QUADRILATERAL PLATE...
C     FORMATION ALSO OF THE I,J, AND K VECTORS USED IN THE E-MATRIX.
C
C     ZERO OUT R-MATRIX
C
   50 DO 60 I = 1,8
   60 REQUIV(I) = 0.0D0
C
C     SHIFT ECPT UP TO MATCH PKTRBS FOR CERTAIN VARIABLES.
C
      DO 80 I = 6,12
   80 ECPT(I) = ECPT(I+1)
C
      DO 90 I = 1,3
      D1(I) = DBLE(VQ3(I)) - DBLE(VQ1(I))
      D2(I) = DBLE(VQ4(I)) - DBLE(VQ2(I))
   90 A1(I) = DBLE(VQ2(I)) - DBLE(VQ1(I))
C
C     NON-NORMALIZED K-VECTOR = D1 CROSS D2
C
      KVECT(1) = D1(2)*D2(3) - D2(2)*D1(3)
      KVECT(2) = D1(3)*D2(1) - D2(3)*D1(1)
      KVECT(3) = D1(1)*D2(2) - D2(1)*D1(2)
C
C     NORMALIZE K-VECTOR
C
      TEMP = DSQRT(KVECT(1)**2 + KVECT(2)**2 + KVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) GO TO 330
      DO 100 I = 1,3
  100 KVECT(I) = KVECT(I)/TEMP
C
C     COMPUTE H = (A1 DOT KVECT)/2
C
      TEMP = (A1(1)*KVECT(1) + A1(2)*KVECT(2) + A1(3)*KVECT(3))/2.0D0
C
C     I-VECTOR =(A1) - H*(KVECT)    NON-NORMALIZED
C
      DO 110 I = 1,3
  110 IVECT(I) = A1(I) - TEMP*KVECT(I)
C
C     NORMALIZE I-VECTOR
C
      TEMP = DSQRT(IVECT(1)**2 + IVECT(2)**2 + IVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) GO TO 330
      DO 120 I = 1,3
  120 IVECT(I) = IVECT(I)/TEMP
C
C     J-VECTOR = K CROSS I, AND X3 CALCULATION
C
      JVECT(1) = KVECT(2)*IVECT(3) - IVECT(2)*KVECT(3)
      JVECT(2) = KVECT(3)*IVECT(1) - IVECT(3)*KVECT(1)
      JVECT(3) = KVECT(1)*IVECT(2) - IVECT(1)*KVECT(2)
C
C     NORMALIZE J VECTOR TO MAKE SURE
C
      TEMP =  DSQRT(JVECT(1)**2 + JVECT(2)**2 + JVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) GO TO 330
      DO 130 I = 1,3
  130 JVECT(I) = JVECT(I)/TEMP
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
C     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
C
      IF (R(2,3).LE.0.0D0 .OR. R(2,4).LE.0.0D0) GO TO 140
      TEMP = R(1,2) - (R(1,2)-R(1,3))*R(2,4)/R(2,3)
      IF (R(1,4) .GE. TEMP) GO TO 140
      TEMP = R(2,3)*R(1,4)/ R(2,4)
      IF (R(1,3) .GT. TEMP) GO TO 150
  140 CALL MESAGE (30,35,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C 140 AT 140 THE COORDINATES OF THE PLATE IN THE ELEMENT
C     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
C     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
C     ROW 2 RESPECTIVELY.
C
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
C
C
C     COMPUTE SUB-TRIANGLE COORDINATES
C
C     ZERO OUT KSUM MATRICES
C
  150 DO 160 I = 1,36
  160 KSUM(I) = 0.0D0
C
      DO 220 J = 1,4
      IF (J .EQ. JNOT) GO TO 220
      KM = 3*J - 3
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 170 I = 1,2
      V(I) = R(I,SUBSCB) - R(I,SUBSCA)
  170 VV(I)= R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB = DSQRT(V(1)**2 + V(2)**2)
      U1 = V(1)/XSUBB
      U2 = V(2)/XSUBB
      XSUBC = U1*VV(1) + U2*VV(2)
      YSUBC = U1*VV(2) - U2*VV(1)
C
      SINTH = SINANG*U1  -  COSANG*U2
      COSTH = COSANG*U1  +  SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
C     TRIANGLE -J-
C
      CALL PKTRBS (1)
C                         U
C     NOW HAVE AT HAND  K    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
C                        IJ                 A(1) THROUGH A(81).
C
C     MAP THE 3 3X3-S FOR THE PIVOT ROW INTO THE SUMMATION ARRAYS...
C
C     SET UP OF T-MATRIX
C
      T(1) = 1.0D0
      T(2) = 0.0D0
      T(3) = 0.0D0
      T(4) = 0.0D0
      T(5) = U1
      T(6) = U2
      T(7) = 0.0D0
      T(8) =-U2
      T(9) = U1
C
C     FIND WHICH POINT OF THE SUBTRIANGLE IS ALSO THE PIVOT OF THE
C     QUADRILATERAL
C
      DO 180 I = 1,3
      NPOINT = KM + I
      IF (M(NPOINT) .NE. NPIVOT) GO TO 180
      NBEGIN = 27*I - 27
      GO TO 190
  180 CONTINUE
C
  190 DO 210 I = 1,3
      NPOINT = NBEGIN + 9*I - 8
      CALL GMMATD (T,3,3,1, A(NPOINT),3,3,0, TEMP9)
      CALL GMMATD (TEMP9,3,3,0, T,3,3,0, PROD9)
C
C     ADD THIS PRODUCT IN NOW.
C
      NPOINT = KM + I
      NPOINT = 9*M(NPOINT) - 9
      DO 200 K = 1,9
      NPOINT = NPOINT + 1
  200 KSUM(NPOINT) = KSUM(NPOINT) + PROD9(K)/2.0D0
  210 CONTINUE
C
  220 CONTINUE
C
C     FILL E-MATRIX
C
      DO 230 I = 1,18
  230 E(I)  = 0.0D0
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
C              T
C     FORM   T   E      STORE IN TITE-MATRIX (6X3)
C             I
C
      IF (NECPT(4*NPIVOT+10) .EQ. 0) GO TO 240
      CALL TRANSD (NECPT(4*NPIVOT+10),T)
      CALL GMMATD (T,3,3,1, E( 1),3,3,0, TITE( 1))
      CALL GMMATD (T,3,3,1, E(10),3,3,0, TITE(10))
      GO TO 260
C
  240 DO 250 K = 1,18
  250 TITE(K) = E(K)
C
  260 DO 320 J = 1,4
C
C     TRANSFORMATIONS AND INSERTION
C
      IF (NECPT(4*J+10) .EQ. 0) GO TO 270
      CALL TRANSD (NECPT(4*J+10),T)
      CALL GMMATD (T,3,3,1,  E(1),3,3,0, TJTE(1 ))
      CALL GMMATD (T,3,3,1, E(10),3,3,0, TJTE(10))
      GO TO 290
  270 DO 280 K = 1,18
  280 TJTE(K) = E(K)
  290 CALL GMMATD (KSUM(9*J-8),3,3,0, TJTE,6,3,1, TEMP18(1))
      CALL GMMATD (TITE(1),6,3,0, TEMP18(1),3,6,0, KOUT(1))
      CALL PLA4B (KOUT(1),NECPT(J+1))
C
  320 CONTINUE
      RETURN
C
  330 CALL MESAGE (30,26,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
      END
