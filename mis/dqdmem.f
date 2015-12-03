      SUBROUTINE DQDMEM
C
C
C     QUADRILATERAL MEMBRANE ROUTINE FOR DIFFERENTIAL STIFFNESS..
C
      REAL IVEC,JVEC,KVEC
C
      DIMENSION          M(12)         ,NECPT(5)
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON /DS1AET/    ECPT(100)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /DS1AAA/ NPVT,ICSTM,NCSTM
     1,                  DUMCL(32)          ,NOGO
      COMMON /DS1ADP/    DUMMY(400)    ,IVEC(3)
     1                  ,NGRID(4)      ,JVEC(3)
     2                  ,COORD(16)     ,KVEC(3)
     3                  ,SDISP(12)     ,PVEC(3)
     4                  ,VSUBK(3)      ,NPT1
     5                  ,JNOT          ,NPT2
     6                  ,NPIVOT        ,NPT3
     7                  ,MPOINT        ,NSUBSC
     8                  ,V(3)          ,U1
     9                  ,SI(3)         ,U2
     T                  ,VECL          ,ANGL
     1                  ,SINANG        ,COSANG
C
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE        (NECPT(1),ECPT(1))
C
      DATA  M / 1, 2, 4, 2, 3, 1, 3, 4, 2, 4, 1, 3 /
C     ******************************************************************
C          ECPT                       ECPT
C       RECEIVED BY                REQUIRED BY
C         DQDMEM                     DTRMEM
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
C     ECPT(21) = Z3              ECPT(21) = ELEMENT TEMP.
C     ECPT(22) = COORD. SYS. ID 4ECPT(22) = EL. DEF.
C     ECPT(23) = X4              ECPT(23) = LDTEMP
C     ECPT(24) = Y4              ECPT(24) = XT 1
C     ECPT(25) = Z4              ECPT(25) = YT 1
C     ECPT(26) = ELEMENT TEMP.   ECPT(26) = ZT 1
C     ECPT(27) = EL. DEF.        ECPT(27) = XT 2
C     ECPT(28) = LDTEMP          ECPT(28) = YT 2
C     ECPT(29) = XT 1            ECPT(29) = ZT 2
C     ECPT(30) = YT 1            ECPT(30) = XT 3
C     ECPT(31) = ZT 1            ECPT(31) = YT 3
C     ECPT(32) = XT 2            ECPT(32) = ZT 3
C     ECPT(33) = YT 2
C     ECPT(34) = ZT 2
C     ECPT(35) = XT 3
C     ECPT(36) = YT 3
C     ECPT(37) = ZT 3
C     ECPT(38) = XT 4
C     ECPT(39) = YT 4
C     ECPT(40) = ZT 4
C     ******************************************************************
C
C     THE FOLLOWING COMPUTATION IS PERFORMED FOR USE WITH THE
C     COMPUTATION OF SINTH AND COSTH BELOW (ANISOTROPIC MATERIAL
C     POSSIBILITY)  NOTE  FMMS-46 PAGE -9-
C
      ANGL = ECPT(6) * DEGRA
      COSANG = COS( ANGL )
      SINANG = SIN( ANGL )
      IVEC(1) = ECPT(15) - ECPT(11)
      IVEC(2) = ECPT(16) - ECPT(12)
      IVEC(3) = ECPT(17) - ECPT(13)
      VECL = SQRT( IVEC(1)**2 + IVEC(2)**2 + IVEC(3)**2 )
      IF (VECL.EQ.0.0E0) GO TO 150
      IVEC(1) = IVEC(1)/VECL
      IVEC(2) = IVEC(2)/VECL
      IVEC(3) = IVEC(3)/VECL
      VSUBK(1) =IVEC(2) *(ECPT(25)-ECPT(13))-IVEC(3)*(ECPT(24)-ECPT(12))
      VSUBK(2) =IVEC(3) *(ECPT(23)-ECPT(11))-IVEC(1)*(ECPT(25)-ECPT(13))
      VSUBK(3) =IVEC(1) *(ECPT(24)-ECPT(12))-IVEC(2)*(ECPT(23)-ECPT(11))
      VECL = SQRT(VSUBK(1)**2 + VSUBK(2)**2 + VSUBK(3)**2 )
      IF (VECL.EQ.0.0E0) GO TO 150
      KVEC(1) = VSUBK(1)/VECL
      KVEC(2) = VSUBK(2)/VECL
      KVEC(3) = VSUBK(3)/VECL
      JVEC(1) = KVEC(2) * IVEC(3) - KVEC(3) * IVEC(2)
      JVEC(2) = KVEC(3) * IVEC(1) - KVEC(1) * IVEC(3)
      JVEC(3) = KVEC(1) * IVEC(2) - KVEC(2) * IVEC(1)
      DO 10 I=1,3
   10 PVEC(I) = COSANG * IVEC(I) + SINANG * JVEC(I)
C
C
C     SAVE COORDINATE SYSTEMS, GRID POINT SIL NUMBERS, AND DISP VECTOR.
C
      NGRID(1) = NECPT(2)
      NGRID(2) = NECPT(3)
      NGRID(3) = NECPT(4)
      NGRID(4) = NECPT(5)
C
      DO 20 I=1,16
   20 COORD(I) = ECPT(I + 9)
C
      DO 30 I=1,12
   30 SDISP(I) = ECPT(I+28)
C
C     NOTE. COORD 1, 5, 9, AND 13  ARE INTEGER CSID NUMBERS.
C
C     CORRECT ECPT FOR MEMBRANE USE
      ECPT(5) = ECPT(6)
      ECPT(6) = ECPT(7)
      ECPT(7) = ECPT(8)/2.0E0
      ECPT(8) = ECPT(9)
      ECPT(21) = ECPT(26)
      ECPT(22) = ECPT(27)
      ECPT(23) = ECPT(28)
C
C     FOR EACH TRIANGLE THEN THE THREE GRID POINTS AND COORDINATES
C     ARE INSERTED INTO THE ECPT BEFORE THE CALL TO KTRMEM.
C
C     FILL MAP MATRIX  (PERFORMED IN DATA STATEMENT - DO NOT ALTER)
C              A              B              C
C           M1 = 1         M2 = 2         M3 = 4      (TRIANGLE    I)
C
C           M4 = 2         M5 = 3         M6 = 1      (TRIANGLE   II)
C
C           M7 = 3         M8 = 4         M9 = 2      (TRIANGLE  III)
C
C           M10= 4         M11= 1         M12= 3      (TRIANGLE   IV)
C
C     ******************************************************************
C     FIND WHICH POINT IS THE PIVOT POINT.
      DO 40 I=1,4
      IF(NPVT .NE. NGRID(I)) GO TO 40
      NPIVOT = I
      GO TO 50
   40 CONTINUE
C
C     FALL THRU ABOVE LOOP IMPLIES AN ERROR CONDITION.
C
      CALL MESAGE(-30,34,ECPT(1))
C
C     COMPUTE JNOT WHICH EQUALS THE ONE TRIANGLE OF THE FOUR NOT USED
C     AND THUS NOT COMPUTED FOR THE PIVOT POINT IN QUESTION.  (NOTE THE
C     ROWS OF THE MAPPING MATRIX ABOVE AND THE TRIANGLE NUMBERS)
C
   50 IF(NPIVOT - 2)60,60,70
   60 JNOT = NPIVOT + 2
      GO TO 80
   70 JNOT = NPIVOT - 2
C
C
   80 DO 140 J=1,4
      IF (J .EQ. JNOT) GO TO 140
C
C     FILL IN ECPT FOR TRIANGLE J
      MPOINT = 3*J - 3
      DO 110 I=1,3
      NPT1 = MPOINT + I
      NSUBSC = M(NPT1)
      NECPT(I+1) = NGRID(NSUBSC)
C
      NPT1 = 3*NSUBSC - 3
      NPT3 = 3 * I + 20
      DO 90 K=1,3
      NPT2 = NPT1 + K
      NPT3 = NPT3 + 1
   90 ECPT(NPT3) = SDISP(NPT2)
C
      NPT1 = 4*NSUBSC - 4
      DO 100 K=1,4
      NPT2 = NPT1 + K
      NPT3 = 4*I + 4 + K
  100 ECPT(NPT3) = COORD(NPT2)
  110 CONTINUE
C
C     ECPT IS COMPLETE FOR TRIANGLE J
C
C     SET UP SINTH AND COSTH FOR THIS SUB TRIANGLE
C
      IF( J.NE.1 ) GO TO 120
      SINTH = SINANG
      COSTH = COSANG
      GO TO 130
C
C     NOTE FMMS-46 PAGE-9 FOR FOLLOWING
C
  120 V(1) = ECPT(14) - ECPT(10)
      V(2) = ECPT(15) - ECPT(11)
      V(3) = ECPT(16) - ECPT(12)
      VECL = SQRT( V(1)**2 + V(2)**2 + V(3)**2 )
      IF (VECL.EQ.0.0E0) GO TO 150
      U1 = ( V(1)*PVEC(1) + V(2)*PVEC(2) + V(3)*PVEC(3) )/VECL
      SI(1) = V(2) * PVEC(3) - V(3) * PVEC(2)
      SI(2) = V(3) * PVEC(1) - V(1) * PVEC(3)
      SI(3) = V(1) * PVEC(2) - V(2) * PVEC(1)
      U2 = ( SI(1)*KVEC(1) + SI(2)*KVEC(2) + SI(3)*KVEC(3) )/VECL
      VECL = SQRT( U1**2 + U2**2 )
      U1 = U1 / VECL
      U2 = U2 / VECL
      SINTH = SINANG * U1 - COSANG * U2
      COSTH = COSANG * U1 + SINANG * U2
  130 IF( ABS(SINTH) .LT. 1.0E-06 ) SINTH = 0.0E0
C
      CALL DTRMEM(   1   )
C
C     INSERTIONS ARE PERFORMED BY DTRMEM
C
  140 C O N T I N U E
C
C     ******************************************************************
C
      RETURN
  150 CALL MESAGE(30,26,ECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
      END
