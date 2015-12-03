      SUBROUTINE KQDMEM
C
C     *** QUADRILATERAL MEMBRANE SUBROUTINE ***
C
C     CALLS FROM THIS ROUTINE ARE MADE TO THE FOLLOWING
C
C            KTRMEM - TRIANGULAR MEMBRANE SUBROUTINE
C            SMA1B  - INSERTION ROUTINE
C            MESAGE - ERROR MESSAGE WRITER
C
      LOGICAL         HRING,HEAT
      REAL            IVEC,JVEC,KVEC
      DOUBLE PRECISION KIJ,KSUM,K3X3,TEMP
      DIMENSION       M(12),K3X3(27),NECPT(8)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SMA1HT/ HEAT
      COMMON /SMA1ET/ ECPT(100)
      COMMON /SMA1IO/ DUM1(10),IFKGG,DUM2(1),IF4GG,DUM3(23)
      COMMON /SMA1CL/ IOPT4,K4GGSW,NPVT,DUMCL(7),LINK(10),IDETCK,
     1                DODET,NOGO
      COMMON /SMA1DP/ KIJ(36),DUM7(156),KSUM(36),TEMP,COSANG,SINANG ,
     1                VECL,IVEC(3),JVEC(3),KVEC(3),PVEC(3),VSUBK(3),
     2                V(3),SI(3),NPIVOT,MPOINT,MI,NSUBSC,NGRID(4),U1,U2,
     3                COORD(16),DUMM8(248)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ DUM99(11),GSUBE,DUM88(6)
      EQUIVALENCE     (CONSTS(4),DEGRA), (K3X3(1),KIJ(1)),
     1                (NECPT(1),ECPT(1))
      DATA    M     / 1, 2, 4, 2, 3, 1, 3, 4, 2, 4, 1, 3 /
      DATA    PIOVR3/ 1.0471975512 /
C
C     ******************************************************************
C          ECPT                       ECPT
C       RECEIVED BY                REQUIRED BY
C         KQDMEM                     KTRMEM
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
      HRING = .FALSE.
      IF (NECPT(8) .EQ. 1) HRING = .TRUE.
C
C     THE FOLLOWING COMPUTATION IS PERFORMED FOR USE WITH THE
C     COMPUTATION OF SINTH AND COSTH BELOW (ANISOTROPIC MATERIAL
C     POSSIBILITY)  NOTE  FMMS-46 PAGE -9-
C
      ANGL    = ECPT(6)*DEGRA
      COSANG  = COS(ANGL)
      SINANG  = SIN(ANGL)
      IVEC(1) = ECPT(15) - ECPT(11)
      IVEC(2) = ECPT(16) - ECPT(12)
      IVEC(3) = ECPT(17) - ECPT(13)
      VECL = SQRT(IVEC(1)**2 + IVEC(2)**2 + IVEC(3)**2)
      IF (VECL .EQ. 0.0) GO TO 200
      IVEC(1) = IVEC(1)/VECL
      IVEC(2) = IVEC(2)/VECL
      IVEC(3) = IVEC(3)/VECL
      VSUBK(1)= IVEC(2)*(ECPT(25)-ECPT(13))-IVEC(3)*(ECPT(24)-ECPT(12))
      VSUBK(2)= IVEC(3)*(ECPT(23)-ECPT(11))-IVEC(1)*(ECPT(25)-ECPT(13))
      VSUBK(3)= IVEC(1)*(ECPT(24)-ECPT(12))-IVEC(2)*(ECPT(23)-ECPT(11))
      VECL = SQRT(VSUBK(1)**2 + VSUBK(2)**2 + VSUBK(3)**2 )
      IF (VECL .EQ. 0.0) GO TO 200
      KVEC(1) = VSUBK(1)/VECL
      KVEC(2) = VSUBK(2)/VECL
      KVEC(3) = VSUBK(3)/VECL
      JVEC(1) = KVEC(2)*IVEC(3) - KVEC(3)*IVEC(2)
      JVEC(2) = KVEC(3)*IVEC(1) - KVEC(1)*IVEC(3)
      JVEC(3) = KVEC(1)*IVEC(2) - KVEC(2)*IVEC(1)
      DO 10 I = 1,3
   10 PVEC(I) = COSANG*IVEC(I) + SINANG*JVEC(I)
C
C
C     SAVE COORDINATE SYSTEMS AND GRID POINT SIL NUMBERS
C
      NGRID(1) = NECPT(2)
      NGRID(2) = NECPT(3)
      NGRID(3) = NECPT(4)
      NGRID(4) = NECPT(5)
      DO 20 I = 1,16
   20 COORD(I) = ECPT(I+9)
C
C     NOTE. COORD 1, 5, 9, AND 13  ARE INTEGER CSID NUMBERS.
C
C     CORRECT ECPT FOR MEMBRANE USE
      ECPT(5) = ECPT(6)
      ECPT(6) = ECPT(7)
      IF (HRING) GO TO 21
      ECPT(7) = ECPT(8)/2.0
   21 CONTINUE
      ECPT(8) = ECPT(9)
      ECPT(21)= ECPT(26)
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
C
C     FIND WHICH POINT IS THE PIVOT POINT.
C
      DO 30 I = 1,4
      IF (NPVT .NE. NGRID(I)) GO TO 30
      NPIVOT = I
      GO TO 40
   30 CONTINUE
C
C     FALL THRU ABOVE LOOP IMPLIES AN ERROR CONDITION.
C
      CALL MESAGE (-30,34,ECPT(1))
C
C     COMPUTE JNOT WHICH EQUALS THE ONE TRIANGLE OF THE FOUR NOT USED
C     AND THUS NOT COMPUTED FOR THE PIVOT POINT IN QUESTION.  (NOTE THE
C     ROWS OF THE MAPPING MATRIX ABOVE AND THE TRIANGLE NUMBERS)
C
   40 IF (NPIVOT-2) 50,50,60
   50 JNOT = NPIVOT + 2
      GO TO 70
   60 JNOT = NPIVOT - 2
C
C     ZERO OUT KSUM FOR 36 WORDS
C
   70 DO 80 I = 1,36
   80 KSUM(I) = 0.0D0
C
C     LOOP THRU 4 TRIANGLES
C
      DO 150 J = 1,4
      IF (J .EQ. JNOT) GO TO 150
C
C     FILL IN ECPT FOR TRIANGLE J
C
      MPOINT = 3*J - 3
      DO 100 I = 1,3
      NPT1   = MPOINT + I
      NSUBSC = M(NPT1)
      NECPT(I+1) = NGRID(NSUBSC)
C
      NPT1 = 4*NSUBSC - 4
      DO 90 K = 1,4
      NPT2 = NPT1 + K
      NPT3 = 4*I  + 4 + K
   90 ECPT(NPT3) = COORD(NPT2)
  100 CONTINUE
C
C     RECOMPUTE THICKNESS IF THIS IS A SUB-TRIANGLE OF A TRAPRG IN
C     A -HEAT- PROBLEM.
C
      IF (HRING) ECPT(7) = PIOVR3*(ECPT(10) + ECPT(14) + ECPT(18))
C
C     ECPT IS COMPLETE FOR TRIANGLE J
C
C     SET UP SINTH AND COSTH FOR THIS SUB TRIANGLE
C
      IF (J .NE. 1) GO TO 110
      SINTH = SINANG
      COSTH = COSANG
      GO TO 120
C
C     NOTE FMMS-46 PAGE-9 FOR FOLLOWING
C
  110 V(1)  = ECPT(14) - ECPT(10)
      V(2)  = ECPT(15) - ECPT(11)
      V(3)  = ECPT(16) - ECPT(12)
      VECL  = SQRT(V(1)**2 + V(2)**2 + V(3)**2)
      IF (VECL .EQ. 0.0) GO TO 200
      U1    = (V(1)*PVEC(1) + V(2)*PVEC(2) + V(3)*PVEC(3))/VECL
      SI(1) = V(2)*PVEC(3) - V(3)*PVEC(2)
      SI(2) = V(3)*PVEC(1) - V(1)*PVEC(3)
      SI(3) = V(1)*PVEC(2) - V(2)*PVEC(1)
      U2    = (SI(1)*KVEC(1) + SI(2)*KVEC(2) + SI(3)*KVEC(3))/VECL
      VECL  = SQRT(U1**2 + U2**2)
      IF (VECL.EQ.0.0E0) GO TO 200
      U1    = U1/VECL
      U2    = U2/VECL
      SINTH = U2
      COSTH = U1
  120 IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
      CALL KTRMEM (1)
C
C     RETURNING FROM KTRMEM THE 3 3X3 ARRAYS FOR THE PIVOT ARE STORED IN
C     COMMON UNDER THE NAME   K3X3(27)
C
C     NOW ADD THE 3 3X3 ARRAYS INTO THE 4 3X3 ARRAYS OF KSUM
C
      DO 140 I = 1,3
      NPT1 = 9*I - 9
C
C     NPT1   POINTS TO THE ZERO POSITION OF THE I-TH K3X3.
C     MPOINT POINTS TO THE ZERO POSITION OF THE J-TH ROW OF MAP MATRIX
C
      MI = MPOINT + I
      NPT2 = 9*M(MI) - 9
C     NPT2 NOW POINTS TO THE ZERO POSITION OF THE  M(MI) TH  SUM MATRIX
C
      DO 130 K = 1,9
      NPT3 = NPT2 + K
      MI   = NPT1 + K
  130 KSUM(NPT3) = KSUM(NPT3) + K3X3(MI)
  140 CONTINUE
C
  150 C O N T I N U E
C
C     ******************************************************************
C
C     NOW INSERT EACH OF THE 4-KSUM (3X3) MATRICES INTO A 6X6 AND
C     SHIP TO SMA1B
C
      IF (HEAT) GO TO 250
      DO 160 I = 1,36
  160 KIJ(I) = 0.0D0
C
      DO 190 J = 1,4
      MPOINT = 9*J - 9
C
C     MPOINT POINTS TO THE ZERO POSITION OF THE J-TH KSUM 3X3.
C
      KIJ( 1) = KSUM(MPOINT + 1)
      KIJ( 2) = KSUM(MPOINT + 2)
      KIJ( 3) = KSUM(MPOINT + 3)
      KIJ( 7) = KSUM(MPOINT + 4)
      KIJ( 8) = KSUM(MPOINT + 5)
      KIJ( 9) = KSUM(MPOINT + 6)
      KIJ(13) = KSUM(MPOINT + 7)
      KIJ(14) = KSUM(MPOINT + 8)
      KIJ(15) = KSUM(MPOINT + 9)
C
C     SHIP TO SMA1B
C
      CALL SMA1B (KIJ(1),NGRID(J),-1,IFKGG,0.0D0)
C
      IF (IOPT4.EQ.0 .OR. GSUBE.EQ.0.0) GO TO 190
      TEMP = GSUBE
      CALL SMA1B (KIJ(1),NGRID(J),-1,IF4GG,TEMP)
      K4GGSW = 1
  190 CONTINUE
C
C     ******************************************************************
C
      RETURN
  200 CALL MESAGE (30,26,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULA
C
      NOGO=1
      RETURN
C*****
C     HEAT FORMULATION.
C*****
  250 DO 260 J = 1,4
      CALL SMA1B (KSUM(9*J-8),NGRID(J),NPVT,IFKGG,0.0D0)
  260 CONTINUE
      RETURN
      END
