      SUBROUTINE QDMEM(T,CORE)
      REAL IVEC,JVEC,KVEC,NGRID
      DIMENSION M(12),R(6),NGRID(4),COORD(16)
      DIMENSION D1(3),D2(3),A1(3),A2(3),A3(3),A4(3),IVEC(3),JVEC(3),
     1  KVEC(3),V(8),ECPTSA(36),T(1),CORE(1)
      COMMON /CONDAS/    PI       ,TWOPI    ,RADEG    ,DEGRA    ,
     1                   S4PISQ
      COMMON /TRIMEX/ ECPT(26)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      EQUIVALENCE (R(1),IVEC(1)),(NGRID(1),ECPTSA(2)),
     1  (COORD(1),ECPTSA(10)),(R(4),JVEC(1))
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
      IF(VECL .EQ. 0.0) CALL MESAGE(-30,26,ECPT(1))
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
      IF(VECL .EQ. 0.0) CALL MESAGE(-30,26,ECPT(1))
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
      THETA=ECPT(6)*DEGRA
      SINANG=SIN(THETA)
      COSANG=COS(THETA)
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
      IF(VECL .EQ. 0.0) CALL MESAGE(-30,26,ECPT(1))
      V(2*I-1) = V(2*I-1)/VECL
   20 V(2*I  ) = V(2*I  )/VECL
C
C     MAPPING MATRIX M IS IN DATA STATEMENT.
C
C     NOW MAKE 4 CALLS TO STRME1 WHICH WILL RETURN
C
C     SAVE GRID SILS AND COORDINATE SYSTEMS.
C
C
C     REDUCE THICKNESS BY 0.5
C
      ECPT(8) = ECPT(8)/2.0
      DO 30 I=1,36
   30 ECPTSA(I) = ECPT(I)
C
      ECPT(6) = ECPT(7)
      ECPT(7) = ECPT(8)
      ECPT(8) = ECPT(9)
C
      ECPT(21) = ECPT(26)
C
      DO 60 I=1,4
C
C     POINTER TO THE SILS IN THE MAPPING MATRIX
      NCOORD = 8
      NPOINT = 3*I-3
      TBAR = T(1)
      DO 50 J=2,4
      NPOINT = NPOINT + 1
      NSUB1 = M(NPOINT)
      DO 40 K=1,4
      NSUB3 = 4*NSUB1 - 4 + K
      NCOORD = NCOORD + 1
   40 ECPT(NCOORD) = COORD(NSUB3)
   50 ECPT(J) = NGRID(NSUB1)
C
C     SET UP T MATRIX FOR THIS TRIANGLE.  T IS 3X3
C
      U1 = V(2*I-1)
      U2 = V(2*I  )
C
C
C     COMPUTE NET SINTH AND COSTH FOR ANISOTROPIC POSSIBILITY
C
      SINTH = SINANG * U1 - COSANG * U2
      COSTH = COSANG * U1 + SINANG * U2
C
      CALL TRIMEM(1,TBAR,CORE)
   60 CONTINUE
      RETURN
      END
