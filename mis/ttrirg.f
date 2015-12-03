      SUBROUTINE TTRIRG( TI, PG )
C
C
C*****
C THIS ROUTINECOMPUTES THE THERMAL LOAD FOR A TRIANGULAR CROSS
C SECTION RING
C*****
C
C
C                        ECPT FOR THE TRIANGULAR RING
C
C
C                                                      TYPE
C ECPT( 1) ELEMENT IDENTIFICATION                        I
C ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A             I
C ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B             I
C ECPT( 4) SCALAR INDEX NO. FOR GRID POINT C             I
C ECPT( 5) MATERIAL ORIENTATION ANGLE(DEGREES)           R
C ECPT( 6) MATERIAL IDENTIFICATION                       I
C ECPT( 7) COOR. SYS. ID. FOR GRID POINT A               I
C ECPT( 8) X-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C ECPT( 9) Y-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C ECPT(10) Z-COOR. OF GRID POINT A (IN BASIC COOR.)      R
C ECPT(11) COOR. SYS. ID. FOR GRID POINT B               I
C ECPT(12) X-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C ECPT(13) Y-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C ECPT(14) Z-COOR. OF GRID POINT B (IN BASIC COOR.)      R
C ECPT(15) COOR. SYS. ID. FOR GRID POINT C               I
C ECPT(16) X-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C ECPT(17) Y-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C ECPT(18) Z-COOR. OF GRID POINT C (IN BASIC COOR.)      R
C ECPT(19) EL. TEMPERATURE FOR MATERIAL PROPERTIES       R
C
C
      DIMENSION          TI(3),    PG(1)
      DIMENSION          IECPT(19)
      DIMENSION
     1                   D(18) ,   GAMBQ(36),     R(3) ,    Z(3)
     2,                  TEO(16),  EE(16),   DELINT(8),  GAMQS(54)
     3,                  DZERO(24),     GAMBL(81),     ALFB(4)
     8,                  IGP(3),  ICS(3) , SP(18)
      DIMENSION          TL(9)
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON   /TRIMEX/
     1                   ECPT(19)
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
      COMMON   /MATOUT/
     1                   E(3)               ,ANU(3)
     2,                  RHO                ,G(3)
     3,                  ALF(3)             ,TZERO
C
      EQUIVALENCE ( CONSTS(2) , TWOPI  )
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE        (IECPT(1) , ECPT(1))
      EQUIVALENCE   (R(1),R1),     (R(2),R2),     (R(3),R3)
     1,             (Z(1),Z1),     (Z(2),Z2),     (Z(3),Z3)
      EQUIVALENCE        (GAMBL( 1),    EE(1))
      EQUIVALENCE        (GAMBL(17),    TEO(1))
      EQUIVALENCE        (GAMBL(33),    DZERO(1))
      EQUIVALENCE        (GAMBL(57),    ALFB(1))
      EQUIVALENCE        (GAMBL(61),    DELINT(1))
      EQUIVALENCE        (GAMBL(37),    SP(1))
      EQUIVALENCE        (GAMBL( 1),    GAMBQ(1))
      EQUIVALENCE        (GAMBL( 1),    GAMQS(1))
C
C ----------------------------------------------------------------------
C
C STORE ECPT PARAMETERS IN LOCAL VARIABLES
C
      IDEL  = IECPT(1)
      IGP(1)= IECPT(2)
      IGP(2)= IECPT(3)
      IGP(3)= IECPT(4)
      MATID = IECPT(6)
      ICS(1)= IECPT(7)
      ICS(2)= IECPT(11)
      ICS(3)= IECPT(15)
      R(1)  = ECPT(8)
      D(1)  = ECPT(9)
      Z(1)  = ECPT(10)
      R(2)  = ECPT(12)
      D(2)  = ECPT(13)
      Z(2)  = ECPT(14)
      R(3)  = ECPT(16)
      D(3)  = ECPT(17)
      Z(3)  = ECPT(18)
      TEMPE = ECPT(19)
      DGAMA = ECPT(5)
C
C
C TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,3
      IF (R(I) .LT. 0.0E0) CALL MESAGE (-30, 37, IDEL)
      IF (D(I) .NE. 0.0E0) CALL MESAGE (-30, 37, IDEL)
  200 CONTINUE
C
C
C COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z1, Z2, Z3)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
C
C CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
C INDICATED BY THE FOLLOWING TABLE
C
C              DELINT( 1) - (-1,0)
C              DELINT( 2) - (-1,1)
C              DELINT( 3) - (-1,2)
C              DELINT( 4) - ( 0,0)
C              DELINT( 5) - ( 0,1)
C              DELINT( 6) - ( 1,0)
C              DELINT( 7) - ( 0,2)
C              DELINT( 8) - ( 1,2)
C
C
C TEST FOR RELATIVE SMALL AREA OF INTEGRATION
C AND IF AREA IS SMALL THEN APPROXIMATE INTEGRALS
C
      DR = AMAX1 (  ABS(R1-R2) ,  ABS(R2-R3) ,  ABS(R3-R1) )
      RH = AMIN1 ( R1 , R2 , R3 ) / 10.0E0
      DZ = AMAX1 (  ABS(Z1-Z2) ,  ABS(Z2-Z3) ,  ABS(Z3-Z1) )
      ZH = AMIN1 ( Z1 , Z2 , Z3 ) / 10.0E0
      RA = (R1 + R2 + R3) / 3.0E0
      ZA = (Z1 + Z2 + Z3) / 3.0E0
      AREA =(R1*(Z2-Z3) + R2*(Z3-Z1) + R3*(Z1-Z2)) / 2.0E0
      KODE = 0
      IF ( ABS( (R2-R1)/R2 ) .LT. 1.0E-5) KODE = 1
      IF ( DR .LE. RH  .OR.  DZ .LE. ZH ) KODE = -1
C
C
  310 CONTINUE
      I1 = 0
      DO 400 I = 1,3
      IP = I - 2
      DO 350 J = 1,3
      IQ = J - 1
      IF (IP.EQ.1 .AND. IQ.EQ.1) GO TO 350
      I1 = I1 + 1
      IF (KODE) 320,330,340
  320 DELINT(I1) =((RA) ** IP)*((ZA) ** IQ) * AREA
      GO TO 350
  330 DELINT(I1) =    AI (1,3,1,2,1,3,IP,IQ,R,Z)
     1            +   AI (3,2,1,2,3,2,IP,IQ,R,Z)
      GO TO 350
  340 CONTINUE
      DELINT(I1) =    AI (1,3,3,2,1,3,IP,IQ,R,Z)
  350 CONTINUE
  400 CONTINUE
      D(1)      = DELINT(6)
      DELINT(6) = DELINT(7)
      DELINT(7) = D(1)
C
C
C TEST FOR EXCESSIVE ROUND-OFF ERROR IN INTEGRAL CALCULATIONS
C AND IF IT EXIST APPROXIMATE INTEGRALS
C
      IF (KODE .LT. 0) GO TO 500
      DO 450 I = 1,8
      IF (DELINT(I) .LT. 0.0E0) GO TO 475
  450 CONTINUE
      IF (DELINT(8) .LE. DELINT(7)) GO TO 475
      IF (DELINT(3) .GE. DELINT(8)) GO TO 475
      IF (DELINT(3) .GT. DELINT(7)) GO TO 475
      GO TO 500
  475 CONTINUE
      KODE = -1
      GO TO 310
  500 CONTINUE
C
C
C
C LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
C
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      CALL  MAT (IDEL)
C
C
C SET MATERIAL PROPERTIES IN LOCAL VARIABLES
C
      ER = E(1)
      ET = E(2)
      EZ = E(3)
      VRT = ANU(1)
      VTZ = ANU(2)
      VZR = ANU(3)
      GRZ = G(3)
      TZ = TZERO
      VTR = VRT * ET / ER
      VZT = VTZ * EZ / ET
      VRZ = VZR * ER / EZ
      DEL = 1.0E0 - VRT*VTR - VTZ*VZT - VZR*VRZ - VRT*VTZ*VZR
     1       - VRZ*VTR*VZT
C
C
C GENERATE ELASTIC CONSTANTS MATRIX (4X4)
C
      EE(1) = ER * (1.0E0 - VTZ*VZT) / DEL
      EE(2) = ER * (VTR + VZR*VTZ) / DEL
      EE(3) = ER * (VZR + VTR*VZT) / DEL
      EE(4) = 0.0E0
      EE(5) = EE(2)
      EE(6) = ET * (1.0E0 - VRZ*VZR) / DEL
      EE(7) = ET * (VZT + VRT*VZR) / DEL
      EE(8) = 0.0E0
      EE(9) = EE(3)
      EE(10)= EE(7)
      EE(11)= EZ * (1.0E0 - VRT*VTR) / DEL
      EE(12)= 0.0E0
      EE(13)= 0.0E0
      EE(14)= 0.0E0
      EE(15)= 0.0E0
      EE(16)= GRZ
C
C
C FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
C GEOMETRIC AXIS
C
      DGAMR = DGAMA * DEGRA
      COSG =  COS(DGAMR)
      SING =  SIN(DGAMR)
      TEO( 1) = COSG ** 2
      TEO( 2) = 0.0E0
      TEO( 3) = SING ** 2
      TEO( 4) = SING * COSG
      TEO( 5) = 0.0E0
      TEO( 6) = 1.0E0
      TEO( 7) = 0.0E0
      TEO( 8) = 0.0E0
      TEO( 9) = TEO(3)
      TEO(10) = 0.0E0
      TEO(11) = TEO(1)
      TEO(12) = -TEO(4)
      TEO(13) = -2.0E0 * TEO(4)
      TEO(14) = 0.0E0
      TEO(15) = -TEO(13)
      TEO(16) = TEO(1) - TEO(3)
C
C
C TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
C TO ELEMENT GEOMETRIC AXIS
C
      CALL GMMATS (TEO , 4, 4, 1, EE , 4, 4, 0, D )
      CALL GMMATS (D   , 4, 4, 0, TEO, 4, 4, 0, EE)
C
C
C
C FORM THE D-CURL MATRIX
C
      DO 600 I = 1,24
      DZERO(I) = 0.0E0
  600 CONTINUE
      DZERO( 2) = DELINT(6) * TWOPI
      DZERO( 7) = DELINT(4) * TWOPI
      DZERO( 8) = DZERO(2)
      DZERO( 9) = DELINT(5) * TWOPI
      DZERO(18) = DZERO(2)
      DZERO(21) = DZERO(2)
      DZERO(23) = DZERO(2)
C
C
C COMPUTE THE THERMAL STRAIN VECTOR
C
      D(1) = ( TI(1) + TI(2) + TI(3) )  / 3.0E0
      D(1) = D(1) - TZ
      DO 650 I = 1,3
      ALFB(I) = ALF(I) * D(1)
  650 CONTINUE
      ALFB(4) = 0.0E0
C
C
C COMPUTE THE THERMAL LOAD IN FIELD COORDINATES
C
      CALL GMMATS (EE(1), 4, 4, 0, ALFB(1), 4, 1, 0, TL(1) )
      CALL GMMATS (DZERO(1), 4, 6, 1, TL(1), 4, 1, 0, D(1) )
C
C
C FORM THE TRANSFORMATION MATRIX (6X6) FROM FIELD COORDINATES TO GRID
C POINT DEGREES OF FREEDOM
C
      DO 680 I = 1,36
      GAMBQ(I) = 0.0E0
  680 CONTINUE
      GAMBQ( 1) = 1.0E0
      GAMBQ( 2) = R1
      GAMBQ( 3) = Z1
      GAMBQ(10) = 1.0E0
      GAMBQ(11) = R1
      GAMBQ(12) = Z1
      GAMBQ(13) = 1.0E0
      GAMBQ(14) = R2
      GAMBQ(15) = Z2
      GAMBQ(22) = 1.0E0
      GAMBQ(23) = R2
      GAMBQ(24) = Z2
      GAMBQ(25) = 1.0E0
      GAMBQ(26) = R3
      GAMBQ(27) = Z3
      GAMBQ(34) = 1.0E0
      GAMBQ(35) = R3
      GAMBQ(36) = Z3
C
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERS (6, GAMBQ(1),6 , D(10), 0, D(11) , ISING , SP)
C
      IF (ISING .EQ. 2) CALL MESAGE(-30,26,IDEL)
C
C
C
C TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS (GAMBQ(1), 6, 6, 1, D(1), 6, 1, 0, TL(1) )
C
C
C GENERATE THE TRANSFORMATION MATRIX FROM TWO TO THREE DEGREES OF
C FREEDOM PER POINT
C
      DO 700 I = 1,54
      GAMQS( I) = 0.0E0
  700 CONTINUE
      GAMQS( 1) = 1.0E0
      GAMQS(12) = 1.0E0
      GAMQS(22) = 1.0E0
      GAMQS(33) = 1.0E0
      GAMQS(43) = 1.0E0
      GAMQS(54) = 1.0E0
C
C
C TRANSFORM THE THERMAL LOAD FROM TWO TO THREE DEGREES OF FREEDOM
C
      CALL GMMATS (GAMQS(1), 6, 9, 1, TL(1), 6, 1, 0, D(10) )
C
C
C LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
C
      DO 750 I = 1,81
      GAMBL(I) = 0.0E0
  750 CONTINUE
      DO 800 I = 1,3
      CALL GBTRAN(ICS(I),ECPT(4*I+7),D(1))
      K = 30* (I-1) + 1
      DO 800 J = 1,3
      KK = K + 9 * (J-1)
      JJ = 3 * (J-1) + 1
      GAMBL(KK  ) = D(JJ  )
      GAMBL(KK+1) = D(JJ+1)
      GAMBL(KK+2) = D(JJ+2)
  800 CONTINUE
C
C
C TRANSFORM THE THERMAL LOAD FROM BASIC TO LOCAL COORDINATES
C
      CALL GMMATS (GAMBL(1), 9, 9, 1, D(10),9, 1, 0, TL(1) )
C
C
C ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
C
      K = 0
      DO 900 I = 1,3
      L = IGP(I) - 1
      DO 900 J = 1,3
      K = K + 1
      L = L + 1
      PG(L) = PG(L) + TL(K)
  900 CONTINUE
C
C
C
      RETURN
      END
