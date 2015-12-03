      SUBROUTINE TTRAPR (TI,PG)
C
C     THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE TRAPEZOIDAL
C     CROSS SECTION RING
C
C     ECPT FOR THE TRAPEZOIDAL RING
C                                                          TYPE
C     ECPT( 1) ELEMENT IDENTIFICATION                        I
C     ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A             I
C     ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B             I
C     ECPT( 4) SCALAR INDEX NO. FOR GRID POINT C             I
C     ECPT( 5) SCALAR INDEX NO. FOR GRID POINT D             I
C     ECPT( 6) MATERIAL ORIENTATION ANGLE(DEGREES)           R
C     ECPT( 7) MATERIAL IDENTIFICATION                       I
C     ECPT( 8) COOR. SYS. ID. FOR GRID POINT A               I
C     ECPT( 9) X-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C     ECPT(10) Y-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C     ECPT(11) Z-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C     ECPT(12) COOR. SYS. ID. FOR GRID POINT B               I
C     ECPT(13) X-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C     ECPT(14) Y-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C     ECPT(15) Z-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C     ECPT(16) COOR. SYS. ID. FOR GRID POINT C               I
C     ECPT(17) X-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C     ECPT(18) Y-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C     ECPT(19) Z-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C     ECPT(20) COOR. SYS. ID. FOR GRID POINT D               I
C     ECPT(21) X-COOR. OF GRID POINT D (IN BASIC COOR.)      R
C     ECPT(22) Y-COOR. OF GRID POINT D (IN BASIC COOR.)      R
C     ECPT(23) Z-COOR. OF GRID POINT D (IN BASIC COOR.)      R
C     ECPT(24) EL. TEMPERATURE FOR MATERIAL PROPERTIES       R
C
C
      DIMENSION       TI(4),PG(1),IECPT(24),D(22),GAMBQ(64),R(4),Z(4),
     1                TEO(16),EE(16),DELINT(12),GAMQS(96),Q(32),
     2                GAMBL(144),ALFB(4),IGP(4),ICS(4),SP(24),HPRIM(16),
     3                TL(12),TS(4),JRZ(2)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /CONDAS/ CONSTS(5)
      COMMON /TRIMEX/ ECPT(24)
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO
      COMMON /SYSTEM/ IBUF,IOUT
      EQUIVALENCE     (CONSTS(2),TWOPI),(CONSTS(4),DEGRA),
     1                (IECPT(1),ECPT(1)),
     2                (R(1),R1),(R(2),R2),(R(3),R3),(R(4),R4),
     3                (Z(1),Z1),(Z(2),Z2),(Z(3),Z3),(Z(4),Z4),
     4                (GAMBL(1),EE(1)),(GAMBL(17),TEO(1)),
     5                (GAMBL(33),ALFB(1)),(GAMBL(37),TS(1)),
     6                (GAMBL(41),DELINT(1)),(GAMBL(1),GAMBQ(1)),
     7                (GAMBL(65),Q(1)),(GAMBL(97),HPRIM(1)),
     8                (GAMBL(113),SP(1)),(GAMBL(1),GAMQS(1))
C
C     STORE ECPT PARAMETERS IN LOCAL VARIABLES
C
      IDEL   = IECPT( 1)
      IGP(1) = IECPT( 2)
      IGP(2) = IECPT( 3)
      IGP(3) = IECPT( 4)
      IGP(4) = IECPT( 5)
      MATID  = IECPT( 7)
      ICS(1) = IECPT( 8)
      ICS(2) = IECPT(12)
      ICS(3) = IECPT(16)
      ICS(4) = IECPT(20)
      R(1)   = ECPT ( 9)
      D(1)   = ECPT (10)
      Z(1)   = ECPT (11)
      R(2)   = ECPT (13)
      D(2)   = ECPT (14)
      Z(2)   = ECPT (15)
      R(3)   = ECPT (17)
      D(3)   = ECPT (18)
      Z(3)   = ECPT (19)
      R(4)   = ECPT (21)
      D(4)   = ECPT (22)
      Z(4)   = ECPT (23)
      TEMPE  = ECPT (24)
      DGAMA  = ECPT ( 6)
C
C     TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,4
      IF (R(I) .LT. 0.0) CALL MESAGE (-30,37,IDEL)
      IF (D(I) .NE. 0.0) CALL MESAGE (-30,37,IDEL)
  200 CONTINUE
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z1,Z2,Z3,Z4)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
      Z4 = Z4 - ZMIN
      RMIN = AMIN1(R1,R2,R3,R4)
      RMAX = AMAX1(R1,R2,R3,R4)
      IF (RMIN .EQ. 0.) GO TO 206
      IF (RMAX/RMIN .LE. 10.) GO TO 206
C
C     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
C
      WRITE  (IOUT,205) UFM,IDEL
  205 FORMAT (A23,', TRAPRG ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ',
     1       'RADIUS RATIO EXCEEDING 10.'/5X,'ACCURACY OF NUMERICAL ',
     2       'INTEGRATION WOULD BE IN DOUBT.')
      CALL MESAGE (-61,0,0)
  206 CONTINUE
      ICORE = 0
      J = 1
      DO 210 I = 1,4
      IF (R(I) .NE. 0.) GO TO 210
      ICORE  = ICORE + 1
      JRZ(J) = I
      J = 2
  210 CONTINUE
      IF (ICORE.NE.0 .AND. ICORE.NE.2) CALL MESAGE (-61,0,0)
C
C     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
C     INDICATED BY THE FOLLOWING TABLE
C
C       DELINT( 1) - ( 0,0)
C       DELINT( 2) - ( 0,1)
C       DELINT( 3) - ( 0,2)
C       DELINT( 4) - ( 1,0)
C       DELINT( 5) - ( 1,1)
C       DELINT( 6) - ( 1,2)
C       DELINT( 7) - ( 2,0)
C       DELINT( 8) - ( 2,1)
C       DELINT( 9) - ( 2,2)
C       DELINT(10) - ( 3,0)
C       DELINT(11) - ( 3,1)
C
      I1 = 0
      DO 400 I = 1,4
      IP = I - 1
      DO 350 J = 1,3
      IQ = J - 1
      I1 = I1 + 1
      IF (I1 .EQ. 12) GO TO 350
      DELINT(I1) = RZINTS(IP,IQ,R,Z,4)
  350 CONTINUE
  400 CONTINUE
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
C
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      CALL  MAT (IDEL)
C
C     SET MATERIAL PROPERTIES IN LOCAL VARIABLES
C
      ER  = E(1)
      ET  = E(2)
      EZ  = E(3)
      VRT = ANU(1)
      VTZ = ANU(2)
      VZR = ANU(3)
      GRZ = G(3)
      TZ  = TZERO
      VTR = VRT*ET/ER
      VZT = VTZ*EZ/ET
      VRZ = VZR*ER/EZ
      DEL = 1.0 - VRT*VTR - VTZ*VZT - VZR*VRZ - VRT*VTZ*VZR
     1          - VRZ*VTR*VZT
C
C     GENERATE ELASTIC CONSTANTS MATRIX (4X4)
C
      EE( 1) = ER*(1.0 - VTZ*VZT)/DEL
      EE( 2) = ER*(VTR + VZR*VTZ)/DEL
      EE( 3) = ER*(VZR + VTR*VZT)/DEL
      EE( 4) = 0.0
      EE( 5) = EE(2)
      EE( 6) = ET*(1.0 - VRZ*VZR)/DEL
      EE( 7) = ET*(VZT + VRT*VZR)/DEL
      EE( 8) = 0.0
      EE( 9) = EE(3)
      EE(10) = EE(7)
      EE(11) = EZ*(1.0 - VRT*VTR)/DEL
      EE(12) = 0.0
      EE(13) = 0.0
      EE(14) = 0.0
      EE(15) = 0.0
      EE(16) = GRZ
C
C     FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
C     GEOMETRIC AXIS
C
      DGAMR   = DGAMA*DEGRA
      COSG    = COS(DGAMR)
      SING    = SIN(DGAMR)
      TEO( 1) = COSG**2
      TEO( 2) = 0.0
      TEO( 3) = SING**2
      TEO( 4) = SING*COSG
      TEO( 5) = 0.0
      TEO( 6) = 1.0
      TEO( 7) = 0.0
      TEO( 8) = 0.0
      TEO( 9) = TEO(3)
      TEO(10) = 0.0
      TEO(11) = TEO(1)
      TEO(12) =-TEO(4)
      TEO(13) =-2.0*TEO(4)
      TEO(14) = 0.0
      TEO(15) =-TEO(13)
      TEO(16) = TEO(1) - TEO(3)
C
C     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
C     TO ELEMENT GEOMETRIC AXIS
C
      CALL GMMATS (TEO,4,4,1, EE, 4,4,0, D )
      CALL GMMATS (D  ,4,4,0, TEO,4,4,0, EE)
C
C     COMPUTE THE THERMAL STRAIN VECTOR
C
      DO 600 I = 1,3
      ALFB(I) = ALF(I)
  600 CONTINUE
      ALFB(4) = 0.0
C
      CALL GMMATS (EE(1),4,4,0, ALFB(1),4,1,0, TS(1))
C
C     FORM THE Q MATRIX (8X4)
C
      D( 1) = TS(1) + TS(2)
      Q( 1) = TS(2)*DELINT(1)
      Q( 2) = TS(2)*DELINT(4)
      Q( 3) = TS(2)*DELINT(2)
      Q( 4) = TS(2)*DELINT(5)
      Q( 5) =  D(1)*DELINT(4)
      Q( 6) =  D(1)*DELINT(7)
      Q( 7) =  D(1)*DELINT(5)
      Q( 8) =  D(1)*DELINT(8)
      Q( 9) = TS(2)*DELINT(2)
      Q(10) = TS(2)*DELINT(5)
      Q(11) = TS(2)*DELINT(3)
      Q(12) = TS(2)*DELINT(6)
      Q(13) =  D(1)*DELINT(5)
      Q(14) =  D(1)*DELINT(8)
      Q(15) =  D(1)*DELINT(6)
      Q(16) =  D(1)*DELINT(9)
      DO 630 I = 17,24
      Q( I) = 0.0
  630 CONTINUE
      Q(25) = TS(3)*DELINT( 4)
      Q(26) = TS(3)*DELINT( 7)
      Q(27) = TS(3)*DELINT( 5)
      Q(28) = TS(3)*DELINT( 8)
      Q(29) = TS(3)*DELINT( 7)
      Q(30) = TS(3)*DELINT(10)
      Q(31) = TS(3)*DELINT( 8)
      Q(32) = TS(3)*DELINT(11)
C
C     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
C     GRID POINT DEGREES OF FREEDOM
C
      DO 650 I = 1,64
      GAMBQ(I) = 0.0
  650 CONTINUE
      GAMBQ( 1) = 1.0
      GAMBQ( 2) = R1
      GAMBQ( 3) = Z1
      GAMBQ( 4) = R1*Z1
      GAMBQ(13) = 1.0
      GAMBQ(14) = R1
      GAMBQ(15) = Z1
      GAMBQ(16) = GAMBQ(4)
      GAMBQ(17) = 1.0
      GAMBQ(18) = R2
      GAMBQ(19) = Z2
      GAMBQ(20) = R2*Z2
      GAMBQ(29) = 1.0
      GAMBQ(30) = R2
      GAMBQ(31) = Z2
      GAMBQ(32) = GAMBQ(20)
      GAMBQ(33) = 1.0
      GAMBQ(34) = R3
      GAMBQ(35) = Z3
      GAMBQ(36) = R3*Z3
      GAMBQ(45) = 1.0
      GAMBQ(46) = R3
      GAMBQ(47) = Z3
      GAMBQ(48) = GAMBQ(36)
      GAMBQ(49) = 1.0
      GAMBQ(50) = R4
      GAMBQ(51) = Z4
      GAMBQ(52) = R4*Z4
      GAMBQ(61) = 1.0
      GAMBQ(62) = R4
      GAMBQ(63) = Z4
      GAMBQ(64) = GAMBQ(52)
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (8,GAMBQ(1),8,D(10),0,D(11),ISING,SP)
C
      IF (ISING .EQ. 2) CALL MESAGE (-30,26,IDEL)
C
C     FORM THE HPRIM MATRIX (4X4)
C
      K  = 0
      DO 670 I = 1,4
      KK =  8*(I-1) - 1
      DO 670 J = 1,4
      K  = K  + 1
      KK = KK + 2
      HPRIM(K) = GAMBQ(KK)
  670 CONTINUE
C
C     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
C
      IF (ICORE .EQ. 0) GO TO 665
      JJ1 = 2*JRZ(1) - 1
      JJ2 = 2*JRZ(2) - 1
C
      DO 663 I = 1,8
      J = 8*(I-1)
      GAMBQ(I    ) = 0.0
      GAMBQ(I+ 16) = 0.0
      GAMBQ(J+JJ1) = 0.
      GAMBQ(J+JJ2) = 0.
  663 CONTINUE
  665 CONTINUE
C
C     FORM THE TEMPERATURE VECTOR
C
      DO 680 I = 1,4
      TI(I) = TI(I) - TZERO
  680 CONTINUE
C
C     COMPUTE THE THERMAL LOAD IN FIELD COORDINATES
C
      CALL GMMATS (HPRIM(1),4,4,0, TI(1),4,1,0, TL(1))
      CALL GMMATS (Q(1),    8,4,0, TL(1),4,1,0,  D(1))
C
C     TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS (GAMBQ(1),8,8,1, D(1),8,1,0, TL(1))
C
C     GENERATE THE TRANSFORMATION MATRIX FROM TWO TO THREE DEGREES OF
C     FREEDOM PER POINT
C
      DO 700 I = 1,96
      GAMQS( I) = 0.0
  700 CONTINUE
      GAMQS( 1) = 1.0
      GAMQS(15) = 1.0
      GAMQS(28) = 1.0
      GAMQS(42) = 1.0
      GAMQS(55) = 1.0
      GAMQS(69) = 1.0
      GAMQS(82) = 1.0
      GAMQS(96) = 1.0
C
C     TRANSFORM THE   THERMAL LOAD   FROM TWO TO THREE DEGREES OF
C     FREEDOM PER POINT
C
      CALL GMMATS (GAMQS(1),8,12,1, TL(1),8,1,0, D(10))
C
C     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
C
      DO 750 I = 1,144
      GAMBL(I) = 0.0
  750 CONTINUE
      DO 800 I = 1,4
      CALL GBTRAN (ICS(I),ECPT(4*I+4),D(1))
      K  = 39*(I-1) + 1
      DO 800 J = 1,3
      KK = K + 12*(J-1)
      JJ = 3*(J-1) + 1
      GAMBL(KK  ) = D(JJ  )
      GAMBL(KK+1) = D(JJ+1)
      GAMBL(KK+2) = D(JJ+2)
  800 CONTINUE
C
C     TRANSFORM THE   THERMAL LOAD   FROM BASIC TO LOCAL COORDINATES
C
      CALL GMMATS (GAMBL(1),12,12,1, D(10),12,1,0, TL(1))
      DO 850 I = 1,12
      TL(I) = TWOPI*TL(I)
  850 CONTINUE
C
C     ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
C
      K = 0
      DO 900 I = 1,4
      L = IGP(I) - 1
      DO 900 J = 1,3
      K = K + 1
      L = L + 1
      PG(L) = PG(L) + TL(K)
  900 CONTINUE
C
      RETURN
      END
