      SUBROUTINE STRAP1
C
C     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE
C     TRAPEZOIDAL CROSS SECTION RING
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
      DIMENSION       IECPT(24),ICS(4),GAMBQ(64),DZERO(32),SP(24),
     1                ALFB(4),TEO(16),EE(16),DELINT(12),GAMQS(96),JRZ(2)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X5/ ECPT(24),DUM5(76),IDEL,IGP(4),TZ,SEL(240),TS(4),
     1                AK(144)
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO
      COMMON /SDR2X6/ D(144),GAMBL(144),R(5),Z(5)
      COMMON /SYSTEM/ IBUF,IOUT
      EQUIVALENCE     (CONSTS(2),TWOPI),(CONSTS(4),DEGRA),
     1                (IECPT(1),ECPT(1)),(R(1),R1),(R(2),R2),(R(3),R3),
     2                (R(4),R4),(Z(1),Z1),(Z(2),Z2),(Z(3),Z3),(Z(4),Z4),
     3                (GAMBL(1),SP(1)),(GAMBL(1),TEO(1)),
     4                (GAMBL(17),DELINT(1))
C
C     STORE ECPT PARAMETERS IN LOCAL VARIABLES
C
      IDEL   = IECPT(1)
      IGP(1) = IECPT(2)
      IGP(2) = IECPT(3)
      IGP(3) = IECPT(4)
      IGP(4) = IECPT(5)
      MATID  = IECPT(7)
      ICS(1) = IECPT(8)
      ICS(2) = IECPT(12)
      ICS(3) = IECPT(16)
      ICS(4) = IECPT(20)
      R(1)   = ECPT(9)
      D(1)   = ECPT(10)
      Z(1)   = ECPT(11)
      R(2)   = ECPT(13)
      D(2)   = ECPT(14)
      Z(2)   = ECPT(15)
      R(3)   = ECPT(17)
      D(3)   = ECPT(18)
      Z(3)   = ECPT(19)
      R(4)   = ECPT(21)
      D(4)   = ECPT(22)
      Z(4)   = ECPT(23)
      TEMPE  = ECPT(24)
      DGAMA  = ECPT(6)
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
      Z1   = Z1 - ZMIN
      Z2   = Z2 - ZMIN
      Z3   = Z3 - ZMIN
      Z4   = Z4 - ZMIN
      RMIN = AMIN1(R1,R2,R3,R4)
      RMAX = AMAX1(R1,R2,R3,R4)
      IF (RMIN .EQ. 0.) GO TO 206
      IF (RMAX/RMIN .LE. 10.) GO TO 206
C
C     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
C
      WRITE  (IOUT,205) UFM,IDEL
  205 FORMAT (A23,', TRAPRG ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ',
     1       'RADIUS RATIO EXCEEDING 10.', /5X,
     2       'ACCURACY OF NUMERICAL INTEGRATION WOULD BE IN DOUBT.')
      CALL MESAGE (-30,37,IDEL)
  206 CONTINUE
      ICORE = 0
      J = 1
      DO 210 I = 1,4
      IF (R(I) .NE. 0.) GO TO 210
      ICORE  = ICORE + 1
      JRZ(J) = I
      J = 2
  210 CONTINUE
      IF (ICORE.NE.0 .AND. ICORE.NE.2) CALL MESAGE (-30,37,IDEL)
C
C     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
C     GRID POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,64
      GAMBQ(I) = 0.0
  300 CONTINUE
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
C     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
C
      IF (ICORE .EQ. 0) GO TO 305
      JJ1 = 2*JRZ(1) - 1
      JJ2 = 2*JRZ(2) - 1
C
      DO 303 I = 1,8
      J = 8*(I-1)
      GAMBQ(I    ) = 0.0
      GAMBQ(I+ 16) = 0.0
      GAMBQ(J+JJ1) = 0.
      GAMBQ(J+JJ2) = 0.
  303 CONTINUE
  305 CONTINUE
C
C     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
C     INDICATED BY THE FOLLOWING TABLE
C
C       DELINT( 1) - (-1,0)
C       DELINT( 2) - (-1,1)
C       DELINT( 3) - (-1,2)
C       DELINT( 4) - ( 0,0)
C       DELINT( 5) - ( 0,1)
C       DELINT( 6) - ( 0,2)
C       DELINT( 7) - ( 1,0)
C       DELINT( 8) - ( 1,1)
C       DELINT( 9) - ( 1,2)
C       DELINT(10) - ( 2,0)
C       DELINT(11) - ( 2,1)
C       DELINT(12) - ( 3,0)
C
      I1 = 0
      DO 400 I = 1,4
      IP = I - 2
      DO 350 J = 1,3
      IQ = J - 1
      I1 = I1 + 1
      IF (I1 .NE. 12) GO TO 340
      IP = 3
      IQ = 0
  340 CONTINUE
      IF (ICORE .EQ. 0) GO TO 345
      IF (I1    .GT. 3) GO TO 345
      DELINT(I1) = 0.0
      GO TO 350
  345 CONTINUE
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
      CALL GMMATS (TEO,4,4,1, EE ,4,4,0, D )
      CALL GMMATS (D  ,4,4,0, TEO,4,4,0, EE)
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
C
      EE48   = EE(4) + EE(8)
      D ( 1) = EE(1) + 2.0 * EE(2) + EE(6)
      AK( 1) = EE(6) * DELINT(1)
      AK( 2) = (EE(2) + EE(6)) * DELINT(4)
      AK( 3) = EE(6) * DELINT(2) + EE(8) * DELINT(4)
      AK( 4) = (EE(2) + EE(6)) * DELINT(5) + EE(8) * DELINT(7)
      AK( 5) = 0.0
      AK( 6) = EE(8) * DELINT(4)
      AK( 7) = EE(7) * DELINT(4)
      AK( 8) = EE(7) * DELINT(7) + EE(8) * DELINT(5)
      AK( 9) = AK(2)
      AK(10) = D(1) * DELINT(7)
      AK(11) = (EE(2) + EE(6)) * DELINT(5) + EE48 * DELINT(7)
      AK(12) = D(1) * DELINT(8) + EE48 * DELINT(10)
      AK(13) = 0.0
      AK(14) = EE48 * DELINT(7)
      AK(15) = (EE(3) + EE(7)) * DELINT(7)
      AK(16) = (EE(3) + EE(7)) * DELINT(10) + EE48 * DELINT(8)
      AK(17) = AK( 3)
      AK(18) = AK(11)
      AK(19) = EE(6) * DELINT(3) + EE(16)* DELINT(7)
     1         + (EE(8) + EE(14)) * DELINT(5)
      AK(20) = (EE(2) + EE(6)) * DELINT(6) + EE(16) * DELINT(10)
     1         + (EE(8) + EE(13) + EE(14)) * DELINT(8)
      AK(21) = 0.0
      AK(22) = EE(16) * DELINT(7) + EE(8) * DELINT(5)
      AK(23) = EE(7) * DELINT(5) + EE(15) * DELINT(7)
      AK(24) = (EE(7) + EE(16)) * DELINT(8)
     1         + EE(8) *DELINT(6) + EE(15) * DELINT(10)
      AK(25) = AK(4)
      AK(26) = AK(12)
      AK(27) = AK(20)
      AK(28) = D(1) * DELINT(9) + EE(16) * DELINT(12)
     1         + (EE48 + EE(13) + EE(14)) * DELINT(11)
      AK(29) = 0.0
      AK(30) = EE(16) * DELINT(10) + EE48 * DELINT(8)
      AK(31) = (EE(3) + EE(7)) * DELINT(8) + EE(15) * DELINT(10)
      AK(32) = (EE(3) + EE(7) + EE(16)) * DELINT(11)
     1         + EE(15) * DELINT(12) + EE48 * DELINT(9)
      AK(33) = 0.0
      AK(34) = 0.0
      AK(35) = 0.0
      AK(36) = 0.0
      AK(37) = 0.0
      AK(38) = 0.0
      AK(39) = 0.0
      AK(40) = 0.0
      AK(41) = AK( 6)
      AK(42) = AK(14)
      AK(43) = AK(22)
      AK(44) = AK(30)
      AK(45) = 0.0
      AK(46) = EE(16)*DELINT(7)
      AK(47) = EE(15)*DELINT(7)
      AK(48) = EE(16)*DELINT(8) + EE(15) * DELINT(10)
      AK(49) = AK( 7)
      AK(50) = AK(15)
      AK(51) = AK(23)
      AK(52) = AK(31)
      AK(53) = 0.0
      AK(54) = AK(47)
      AK(55) = EE(11)*DELINT( 7)
      AK(56) = EE(11)*DELINT(10) + EE(12) * DELINT(8)
      AK(57) = AK( 8)
      AK(58) = AK(16)
      AK(59) = AK(24)
      AK(60) = AK(32)
      AK(61) = 0.0
      AK(62) = AK(48)
      AK(63) = AK(56)
      AK(64) = EE(11) * DELINT(12) + EE(16) * DELINT(9)
     1       + (EE(12) + EE(13)) * DELINT(11)
C
      DO 600 I = 1,64
      AK(I) = TWOPI*AK(I)
  600 CONTINUE
C
C     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
C     TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS (GAMBQ,8,8,1, AK   ,8,8,0, D )
      CALL GMMATS (D    ,8,8,0, GAMBQ,8,8,0, AK)
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
C     TRANSFORM THE STIFFNESS MATRIX FROM TWO TO THREE DEGREES OF
C     FREEDOM PER POINT
C
      CALL GMMATS (GAMQS(1),8,12,1, AK(1)   ,8, 8,0, D(1) )
      CALL GMMATS (D(1)    ,12,8,0, GAMQS(1),8,12,0, AK(1))
C
C     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
C
      DO 750 I = 1,144
      GAMBL(I) = 0.0
  750 CONTINUE
      DO 800 I = 1,4
      CALL TRANSS (ICS(I),D(1))
      K = 39*(I-1) + 1
      DO 800 J = 1,3
      KK = K + 12*(J-1)
      JJ = 3 *(J-1) + 1
      GAMBL(KK  ) = D(JJ  )
      GAMBL(KK+1) = D(JJ+1)
      GAMBL(KK+2) = D(JJ+2)
  800 CONTINUE
C
C     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
C
      CALL GMMATS (GAMBL(1),12,12,1, AK(1)   ,12,12,0, D(1) )
      CALL GMMATS (D(1)    ,12,12,0, GAMBL(1),12,12,0, AK(1))
C
C     COMPUTE THE FIFTH GRID POINT TO BE THE AVERAGE OF THE FOUR
C     CORNER POINTS
C
      R(5) = (R1 + R2 + R3 + R4)/4.0
      Z(5) = (Z1 + Z2 + Z3 + Z4)/4.0
C
C     INITIALIZE THE CONSTANT PORTION OF THE D SUB 0 MATRIX
C
      DO 850 I = 1,32
      DZERO(I) = 0.0
  850 CONTINUE
      DZERO( 2) = 1.0
      DZERO(10) = 1.0
      DZERO(23) = 1.0
      DZERO(27) = 1.0
      DZERO(30) = 1.0
C
C     START THE LOOP TO COMPUTE THE STRESS MATRIX FOR EACH GRID POINT
C
      DO 950 J = 1,5
C
C     COMPUTE THE VARIABLE PORTION OF THE D SUB 0 MATRIX
C
      DZERO( 4) = Z(J)
      IF (ICORE .NE. 0) GO TO 875
      DZERO( 9) = 1.00/R(J)
      DZERO(11) = Z(J)/R(J)
  875 CONTINUE
      DZERO(12) = Z(J)
      DZERO(24) = R(J)
      DZERO(28) = R(J)
      DZERO(32) = Z(J)
C
C     COMPUTE THE STRESS MATRIX IN FIELD COORDINATES
C
      CALL GMMATS (EE(1),4,4,0, DZERO(1),4,8,0, D(1))
C
C     TRANSFORM THE STRESS MATRIX TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS (D(1),4,8,0, GAMBQ(1),8,8,0, D(37))
C
C     TRANSFORM THE STRESS MATRIX FROM TWO TO THREE DEGREES OF FREEDOM
C     PER POINT
C
      CALL GMMATS (D(37),4,8,0, GAMQS(1),8,12,0, D(73))
C
C     TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
C
      K = 48*(J-1) + 1
      CALL GMMATS (D(73),4,12,0, GAMBL(1),12,12,0, SEL(K))
C
  950 CONTINUE
C
C     COMPUTE THE THERMAL STRAIN VECTOR
C
      DO 900 I = 1,3
      ALFB(I) = ALF(I)
  900 CONTINUE
      ALFB(4) = 0.0
C
C     COMPUTE THE THERMAL STRESS VECTOR
C
      CALL GMMATS (EE(1),4,4,0, ALFB(1),4,1,0, TS(1))
      RETURN
      END
