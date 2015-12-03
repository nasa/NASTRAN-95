      SUBROUTINE MQDPLT
C
C     THIS ROUTINE GENERATES FOUR 6X6 STIFFNESS MATRICES WITH RESPECT
C     TO ONE PIVOT POINT OF A QUADRILATERAL PLATE ELEMENT.
C
C     REF.  FMMS-66   JUNE 23, 1969   TRI.BENDING ELEMENT MASS
C           FMMS-66   JUNE 23, 1969   QUAD. BENDING ELEMENT MASS
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C           MTRBSC - BASIC BENDING TRI. ROUTINE.
C           TRANSD - SUPPLIES 3X3 TRANSFORMATIONS
C           SMA2B  - INSERTION ROUTINE
C           GMMATD - GENERAL MATRIX MULITPLY AND TRANSPOSE ROUTINE
C           MESAGE - ERROR MESSAGE WRITER
C
C     ALL WRITE STATEMENTS WHICH HAVE BEEN COMMENTED OUT, HAVE BEEN
C     LEFT IN THE PROGRAMMING FOR ANY FUTURE DEBUGGING USE.
C
C     ECPT LISTS AS OF AUGUST 4, 1967
C
C                 DEFINITION                   DEFINITION
C       ECPT      BSC.BEND.TRI.-----TYPE       QUAD.PLT.---------TYPE
C     ========   =============      =======    ===============   =======
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
      DOUBLE PRECISION MOUT,TITE,DPDUM1,TJTE,DPDUM2,IVECT,D1,JVECT,D2,
     1                 KVECT,A1,MSUM,T,V,VV,XSUBB,XSUBC,YSUBC,PROD9,
     2                 TEMP,TEMP9,TEMP36,U1,U2,H,E,A,REQUIV,R,IIZ,MIZ,
     3                 SIGN,PTMASS,M6X6
      DIMENSION        M(12),NECPT(100),REQUIV(8),VQ1(3),VQ2(3),VQ3(3),
     1                 VQ4(3),A(1),MSUM(36)
      COMMON /CONDAS/  CONSTS(5)
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                 T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,
     2                 G2X211,G2X212,G2X222,SPACE(2)
      COMMON /SMA2IO/  DUM1(10),IFMGG,DUM2(25)
      COMMON /SMA2CL/  DUM3(2),NPVT,DUMCL(7),LINK(10),NOGO
      COMMON /SMA2ET/  ECPT(100)
      COMMON /SMA2DP/  MOUT(36),TITE(9),TJTE(36),TEMP36(36),DPDUM1(27),
     1                 D1(3),D2(3),A1(3),T(9),V(2),VV(2),IIZ,MIZ,SIGN,
     2                 SPDUM(20),M6X6(36),DPDUM2(10),PROD9(9),TEMP9(9),
     3                 XSUBB,XSUBC,YSUBC,E(9),TEMP,SP1(33),KM,NBEGIN,
     4                 JNOT,NPIVOT,THETA,NSUBC,ISING,SUBSCA,SUBSCB,
     5                 SUBSCC,SINANG,COSANG,NPOINT,IVECT(3),JVECT(3),
     6                 KVECT(3),U1,U2,R(2,4),H,PTMASS
      EQUIVALENCE      (CONSTS(4),DEGRA),(NECPT(1),ECPT(1)),
     1                 (R(1,1),REQUIV(1)),(VQ1(1),ECPT(15)),
     2                 (VQ2(1),ECPT(19)),(VQ3(1),ECPT(23)),
     3                 (VQ4(1),ECPT(27)),(A(1),MOUT(1))
      DATA    M     /  2,4,1,  3,1,2,  4,2,3,  1,3,4 /
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
C     SHIFT ECPT UP TO MATCH MTRBSC FOR CERTAIN VARIABLES.
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
      IF (TEMP .EQ. 0.0D0) GO TO 360
      DO 100 I = 1,3
  100 KVECT(I) = KVECT(I)/TEMP
C
C     COMPUTE H = A1 DOT KVECT
C
      H = A1(1)*KVECT(1) + A1(2)*KVECT(2) + A1(3)*KVECT(3)
C
C     WRITE (6,109)
C     WRITE (6,119)
C     WRITE (6,1195) H,(D1(I),D2(I),A1(I),I=1,3)
C
C     I-VECTOR = (A1) - H*(KVECT)  NON-NORMALIZED
C
      DO 110 I = 1,3
  110 IVECT(I) = A1(I) - H*KVECT(I)
C
C     NORMALIZE I-VECTOR
C
      TEMP = DSQRT(IVECT(1)**2 + IVECT(2)**2 + IVECT(3)**2)
      IF (TEMP .EQ. 0.0D0) GO TO 360
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
      IF (TEMP .EQ. 0.0D0) GO TO 360
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
C     WRITE (6,129) (IVECT(I),I=1,3),(JVECT(I),I=1,3),(KVECT(I),I=1,3),
C    1              ((R(I,J),J=1,4),I=1,2)
C
C     CHECK OF 4 POINTS FOR ANGLE GREATER THAN OR EQUAL TO 180 DEGREES.
C
      IF (R(2,3).LE.0.0D0 .OR. R(2,4).LE.0.0D0) GO TO 140
      TEMP = R(1,2) - (R(1,2)-R(1,3))*R(2,4)/R(2,3)
      IF (R(1,4) .GE. TEMP) GO TO 140
      TEMP = R(2,3)*R(1,4)/R(2,4)
      IF (R(1,3) .GT. TEMP) GO TO 150
  140 CALL MESAGE (30,35,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C     AT 140 THE COORDINATES OF THE PLATE IN THE ELEMENT
C     SYSTEM ARE STORED IN THE R-MATRIX WHERE THE COLUMN DENOTES THE
C     POINT AND THE ROW DENOTES THE X OR Y COORDINATE FOR ROW 1 OR
C     ROW 2 RESPECTIVELY.
C
C     SET UP THE M-MATRIX FOR MAPPING TRIANGLES, IN DATA STATEMENT.
C
C     COMPUTE SUB-TRIANGLE COORDINATES
C
C     ZERO OUT MSUM MATRICES
C
  150 DO 160 I = 1,36
  160 MSUM(I) = 0.0D0
      PTMASS  = 0.0D0
      ELTEMP  = ECPT(30)
C
      DO 220 J = 1,4
      IF (J .EQ. JNOT) GO TO 220
      KM = 3*J - 3
      SUBSCA = M(KM+1)
      SUBSCB = M(KM+2)
      SUBSCC = M(KM+3)
C
      DO 170 I = 1,2
      V(I)  = R(I,SUBSCB) - R(I,SUBSCA)
  170 VV(I) = R(I,SUBSCC) - R(I,SUBSCA)
      XSUBB = DSQRT(V(1)**2 + V(2)**2)
      U1    = V(1)/XSUBB
      U2    = V(2)/XSUBB
      XSUBC = U1*VV(1) + U2*VV(2)
      YSUBC = U1*VV(2) - U2*VV(1)
C
      SINTH = SINANG*U1 - COSANG*U2
      COSTH = COSANG*U1 + SINANG*U2
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
C
C     AT THIS POINT, XSUBB, XSUBC, YSUBC ARE AT HAND FOR
C     TRIANGLE -J-
C
C     WRITE(6,139) XSUBB,XSUBC,YSUBC
C
      CALL MTRBSC
C                         U
C     NOW HAVE AT HAND  M    I,J, =1,2,3.   9-3X3 MATRICES STORED AT
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
  200 MSUM(NPOINT) = MSUM(NPOINT) + PROD9(K)/2.0D0
C
C
  210 CONTINUE
C
      PTMASS = PTMASS + DBLE(ECPT(10))/4.0D0 * XSUBB*YSUBC
  220 CONTINUE
      PTMASS = PTMASS/3.0D0
C
      DO 225 I = 1,36
  225 TJTE(I) = 0.0D0
C
C     FILL E-MATRIX
C
      DO 230 I = 1,9
  230 E(I) = 0.0D0
      DO 235 I = 1,3
      NPOINT = 3*I - 2
      E(NPOINT  ) = IVECT(I)
      E(NPOINT+1) = JVECT(I)
  235 E(NPOINT+2) = KVECT(I)
C
C
C              T
C     FORM   T   E      STORE IN TITE-MATRIX (6X3)
C             I
C
      IF (NECPT(4*NPIVOT+10) .EQ. 0) GO TO 240
      CALL TRANSD (NECPT(4*NPIVOT+10),T)
      CALL GMMATD (T,3,3,1, E(1),3,3,0, TITE(1))
C
C
      GO TO 260
C
  240 DO 250 K = 1,9
  250 TITE(K) = E(K)
C
C
C     TRANSFORMATIONS AND INSERTION
C
  260 DO 350 J = 1,4
      NBEGIN = 9*J - 9
      DO 265 I = 1,36
  265 M6X6(I) = 0.0D0
      DO 270 I = 1,3
      NPOINT = NBEGIN + I
      M6X6(I+14) = MSUM(NPOINT  )
      M6X6(I+20) = MSUM(NPOINT+3)
  270 M6X6(I+26) = MSUM(NPOINT+6)
C
C
      IF (NPIVOT .NE. J) GO TO 290
C
      SIGN     = (-1)**J
      TEMP     = PTMASS*H
      MIZ      = TEMP/2.0D0*SIGN
      IIZ      = TEMP*H/2.0D0
      M6X6( 1) = PTMASS
      M6X6( 5) = MIZ
      M6X6( 8) = M6X6(1)
      M6X6(10) =-MIZ
      M6X6(20) = M6X6(10)
      M6X6(22) = M6X6(22) + IIZ
      M6X6(25) = MIZ
      M6X6(29) = M6X6(29) + IIZ
C
C
  290 IF (NECPT(4*J+10) .EQ. 0) GO TO 320
      CALL TRANSD (NECPT(4*J+10),T)
      CALL GMMATD (E(1),3,3,1, T(1),3,3,0, TJTE(1))
      DO 300 I = 1,3
      NPOINT = I + 21
      TJTE(NPOINT   ) = TJTE(I  )
      TJTE(NPOINT+ 6) = TJTE(I+3)
  300 TJTE(NPOINT+12) = TJTE(I+6)
      DO 310 I = 1,3
      NPOINT = I + 21
      TJTE(I   ) = TJTE(NPOINT   )
      TJTE(I+ 6) = TJTE(NPOINT+ 6)
      TJTE(I+12) = TJTE(NPOINT+12)
  310 TJTE(I+ 3) = 0.0D0
C
      GO TO 340
C
  320 DO 330 I = 1,3
      NPOINT = 6*I - 5
      NPT    = NPOINT + 21
      TJTE(NPOINT  ) = E(I  )
      TJTE(NPOINT+1) = E(I+3)
      TJTE(NPOINT+2) = E(I+6)
      TJTE(NPT     ) = E(I  )
      TJTE(NPT   +1) = E(I+3)
  330 TJTE(NPT   +2) = E(I+6)
C
C
  340 CALL GMMATD (M6X6(1),6,6,0, TJTE(1),6,6,0, TEMP36(1))
      CALL GMMATD (TITE(1),3,3,0, TEMP36( 1),3,6,0, MOUT( 1))
      CALL GMMATD (TITE(1),3,3,0, TEMP36(19),3,6,0, MOUT(19))
C
C
      CALL SMA2B (MOUT(1),NECPT(J+1),-1,IFMGG,0.0D0)
C
  350 CONTINUE
      RETURN
C
C
  360 CALL MESAGE (30,26,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
      END
