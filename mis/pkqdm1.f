      SUBROUTINE PKQDM1
C  THIS ROUTINE CALCULATES PHASE I OUTPUT FOR THE QUAD-MEMBRAND IN
C   PLA4
C
      REAL IVEC,JVEC,KVEC
      INTEGER NECPT(100)
      DIMENSION M(12),R(6),NGRID(4),COORD(16),S(27)
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON /PLA42C/ DUMCL(149),NOGO
      COMMON /PLA42S/ DUMMY(100),SUM(36),STEMP(9),D1(3),D2(3),A1(3),
     1 A2(3),A3(3),A4(3),IVEC(3),JVEC(3),KVEC(3),VECL,H,V(8),ECPTSA(36),
     2 ST(3),NCOORD,NPOINT,NSUB1,NSUB2,NSUB3,T(9),COSANG,SINANG,U1,U2,
     3  THETA, DUMY(85)
      COMMON /PLA4ES/ ECPT(100),PH1OUT(200)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
C
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE (NECPT(1),ECPT(1))
      EQUIVALENCE (R(1),IVEC(1)),(NGRID(1),ECPTSA(2)),
     1   (COORD(1),ECPTSA(10))  ,  (S(1),PH1OUT(10))
C
      DATA  M / 1, 2, 4, 2, 3, 1, 3, 4, 2, 4, 1, 3 /
C     ******************************************************************
C          ECPT                       ECPT
C       RECEIVED BY                REQUIRED BY
C         SQDME1                     STRME1
C     ******************************************************************
C     ECPT( 1) = EL. ID          ECPT( 1) = EL. ID
C     ECPT( 2) = GRD. PT. A      ECPT( 2) = GRD. PT. A
C     ECPT( 3) = GRD. PT. B      ECPT( 3) = GRD. PT. B
C     ECPT( 4) = GRD. PT. C      ECPT( 4) = GRD. PT. C
C     ECPT( 5) = GRD. PT. D      ECPT( 5) = THETA
C     ECPT( 6) = THETA           ECPT( 6) = MATERIAL ID
C     ECPT( 7) = MATERIAL ID     ECPT( 7) = T
C     ECPT( 8) = T               ECPT( 8) = NON-STRUCT. MASS
C     ECPT( 9) = NON-STRUCT. MASSECPT( 9) = COORD. SYS. ID 1
C     ECPT(10) = COORD. SYS. ID 1ECPT(10) = X1
C     ECPT(11) = X1              ECPT(11) = Y1
C     ECPT(12) = Y1              ECPT(12) = Z1
C     ECPT(13) = Z1              ECPT(13) = COORD. SYS. ID 2
C     ECPT(14) = COORD. SYS. ID 2ECPT(14) = X2
C     ECPT(15) = X2              ECPT(15) = Y2
C     ECPT(16) = Y2              ECPT(16) = Z2
C     ECPT(17) = Z2              ECPT(17) = COORD. SYS. ID 3
C     ECPT(18) = COORD. SYS. ID 3ECPT(18) = X3
C     ECPT(19) = X3              ECPT(19) = Y3
C     ECPT(20) = Y3              ECPT(20) = Z3
C     ECPT(21) = Z3              ECPT(21) = ELEMENT TEMPERATURE
C     ECPT(22) = COORD. SYS. ID 4    NOTE. THE FOLLOWING ARE INTEGERS...
C     ECPT(23) = X4                  GRID POINTS, MAT ID, EL.ID,
C     ECPT(24) = Y4                  COORD. SYS. IDS.
C     ECPT(25) = Z4                  ALL OTHERS ARE REAL IN THE ECPT.
C     ECPT(26) = ELEMENT TEMPERATURE
C     ******************************************************************
C
C
C     VECTORS D1 AND D2  FMMS-46 PAGE 6
C     A1 A2 A3 A4
C
      DO 10 I=1,3
      D1(I) = ECPT(I + 18) - ECPT(I + 10)
      D2(I) = ECPT(I + 22) - ECPT(I + 14)
      A1(I) = ECPT(I + 14) - ECPT(I + 10)
      A2(I) = ECPT(I + 18) - ECPT(I + 14)
      A3(I) = ECPT(I + 22) - ECPT(I + 18)
   10 A4(I) = ECPT(I + 10) - ECPT(I + 22)
C
C     K-VECTOR = NORMALIZED D1 CROSS D2
C
      KVEC(1) = D1(2) * D2(3) - D1(3) * D2(2)
      KVEC(2) = D1(3) * D2(1) - D1(1) * D2(3)
      KVEC(3) = D1(1) * D2(2) - D1(2) * D2(1)
      VECL = SQRT ( KVEC(1)**2 + KVEC(2)**2 + KVEC(3)**2 )
      IF (VECL.LT.1.0E-06) GO TO 120
      KVEC(1) = KVEC(1)/VECL
      KVEC(2) = KVEC(2)/VECL
      KVEC(3) = KVEC(3)/VECL
C
C     I-VECTOR = NORMALIZED A SUB 12 - H * KVECTOR
C     GET H FIRST = ( A SUB 12 DOT KVECTOR)/2
C
      H = (A1(1)*KVEC(1) + A1(2)*KVEC(2) + A1(3)*KVEC(3))/2.0E0
C
      IVEC(1) = A1(1) - H * KVEC(1)
      IVEC(2) = A1(2) - H * KVEC(2)
      IVEC(3) = A1(3) - H * KVEC(3)
      VECL = SQRT ( IVEC(1)**2 + IVEC(2)**2 + IVEC(3)**2 )
      IF (VECL.LT.1.0E-06) GO TO 120
      IVEC(1) = IVEC(1)/VECL
      IVEC(2) = IVEC(2)/VECL
      IVEC(3) = IVEC(3)/VECL
C
C     J-VECTOR = K CROSS I
C
      JVEC(1) = KVEC(2) * IVEC(3) - KVEC(3) * IVEC(2)
      JVEC(2) = KVEC(3) * IVEC(1) - KVEC(1) * IVEC(3)
      JVEC(3) = KVEC(1) * IVEC(2) - KVEC(2) * IVEC(1)
C
      VECL = SQRT(JVEC(1)**2 + JVEC(2)**2 + JVEC(3)**2)
      JVEC(1) = JVEC(1)/VECL
      JVEC(2) = JVEC(2)/VECL
      JVEC(3) = JVEC(3)/VECL
C
      THETA = ECPT(6) * DEGRA
      SINANG = SIN(THETA)
      COSANG = COS(THETA)
C
      V(1) = 1.0E0
      V(2) = 0.0E0
C
C     R ARRAY IS EQUIVALENCED TO IVECTOR AND JVECTOR
C
      CALL GMMATS(R,2,3,0,  A2,3,1,0,  V(3))
      CALL GMMATS(R,2,3,0,  A3,3,1,0,  V(5))
      CALL GMMATS(R,2,3,0,  A4,3,1,0,  V(7))
C
C     NORMALIZE THE 4 2X1 V ARRAYS
C
      DO 20 I=1,4
      VECL = SQRT ( V(2*I-1)**2 + V(2*I)**2 )
      IF(VECL .LT. 1.0E-10) CALL MESAGE(-30,26,ECPT(1))
      V(2*I-1) = V(2*I-1)/VECL
   20 V(2*I  ) = V(2*I  )/VECL
C
C     MAPPING MATRIX M IS IN DATA STATEMENT.
C
C     NOW MAKE 4 CALLS TO PKTRM1 WHICH WILL RETURN
C     S , S , S , S , T SUB 0
C      A   B   C   T
C
C     SAVE GRID SILS AND COORDINATE SYSTEMS.
C
      DO 30 I=1,36
   30 ECPTSA(I) = ECPT(I)
C
      ECPT(6) = ECPT(7)
      ECPT(7) = ECPT(8)
      ECPT(8) = ECPT(9)
C
C     ZERO OUT SUM MATRICES
C
      DO 40 I=1,36
   40 SUM(I) = 0.0E0
      ST(1) = 0.0E0
      ST(2) = 0.0E0
      ST(3) = 0.0E0
C
      DO 90 I=1,4
C
C     POINTER TO THE SILS IN THE MAPPING MATRIX
      NCOORD = 8
      NPOINT = 3*I-3
      DO 60 J=2,4
      NPOINT = NPOINT + 1
      NSUB1 = M(NPOINT)
      DO 50 K=1,4
      NSUB3 = 4*NSUB1 - 4 + K
      NCOORD = NCOORD + 1
   50 ECPT(NCOORD) = COORD(NSUB3)
   60 NECPT(J) = NGRID( NSUB1 )
C
C     SET UP T MATRIX FOR THIS TRIANGLE.  T IS 3X3
C
      U1 = V(2*I-1)
      U2 = V(2*I  )
C
      T(1) = U1 ** 2
      T(2) = U2 ** 2
      T(7) = U1 * U2
      T(3) = -2.0E0 * T(7)
      T(4) = T(2)
      T(5) = T(1)
      T(6) = -T(3)
      T(8) = -T(7)
      T(9) = T(1) - T(2)
C
C     COMPUTE NET SINTH AND COSTH FOR ANISOTROPIC POSSIBILITY
C
      SINTH = SINANG * U1 - COSANG * U2
      COSTH = COSANG * U1 + SINANG * U2
C
      CALL PKTRM1 (1)
C
C
C     NOW TRANSFORM AND ADD THE S MATRICES INTO THE RESPECTIVE SUM
C     MATRICES.
C
      DO 80 J=1,3
C
C     POINTER TO TRIANGLE I ROW IN THE MAPPING MATRIX
C
      NPOINT = 3*I-3
C
C     TRANSFORM S
C
      CALL GMMATS( T,3,3,0,  S(9*J-8),3,3,0,  STEMP )
C
C     ADD STEMP INTO RESPECTIVE KSUM POSITIONS
C
C     ZERO POINTER INTO KSUM MATRICES
      NSUB1 = NPOINT + J
      NSUB1 = M(NSUB1)*9 - 9
      DO 70 K=1,9
      NSUB1 = NSUB1 + 1
   70 SUM(NSUB1) = SUM(NSUB1) + STEMP(K)
   80 CONTINUE
   90 CONTINUE
C
C     ALL MATRICES COMPLETE
C
C     FILL OUTPUT BLOCK
C
      DO 100 I=1,5
  100 PH1OUT(I) = ECPTSA(I)
      DO 110 I=1,36
  110 PH1OUT(I+9) = 0.25E0 * SUM(I)
C     PHASE 1 COMPLETE OUTPUT BLOCK CONTAINS 45 WORDS
C
      RETURN
  120 CALL MESAGE(30,26,ECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
      END
