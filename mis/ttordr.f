      SUBROUTINE TTORDR (TI, PG)
C
C
C*****
C THIS ROUTINE COMPUTES THE THERMAL LOAD FOR AN AXI-SYMMETRIC
C TOROIDAL THIN SHELL RING
C*****
C
C
C
C                        ECPT FOR THE TOROIDAL RING
C
C                                                       TYPE
C ECPT( 1) ELEMENT IDENTIFICATION                         I
C ECPT( 2) SCALAR INDEX NO. FOR GRID POINT A              I
C ECPT( 3) SCALAR INDEX NO. FOR GRID POINT B              I
C ECPT( 4) ANGLE OF CURVATURE AT GRID POINT A             R
C ECPT( 5) ANGLE OF CURVATURE AT GRID POINT B(NOT USED)   R
C ECPT( 6) MATERIAL ORIENTATION (NOT USED)                R
C ECPT( 7) MATERIAL IDENTIFICATION                        I
C ECPT( 8) MEMBRANE THICKNESS                             R
C ECPT( 9) FLEXURE THICKNESS                              R
C ECPT(10) COOR. SYS. ID. FOR GRID POINT A                I
C ECPT(11) X-COOR. OF GRID POINT A (IN BASIC COOR.)       R
C ECPT(12) Y-COOR. OF GRID POINT A (IN BASIC COOR.)       R
C ECPT(13) Z-COOR. OF GRID POINT A (IN BASIC COOR.)       R
C ECPT(14) COOR. SYS. ID. FOR GRID POINT B                I
C ECPT(15) X-COOR. OF GRID POINT B (IN BASIC COOR.)       R
C ECPT(16) Y-COOR. OF GRID POINT B (IN BASIC COOR.)       R
C ECPT(17) Z-COOR. OF GRID POINT B (IN BASIC COOR.)       R
C ECPT(18) EL. TEMPERATURE FOR MATERIAL PROPERTIES        R
C
C
      DIMENSION          TI(2),    PG(1)
      DIMENSION          IECPT(18)
      DIMENSION          GAMBQF(72),    GAMBQM(48)
      DIMENSION          EE(4), GAMBQ(144), GAMRS(144)
      DIMENSION          AKI(36),  DELINT(42)
      DIMENSION          IGP(2),   ICS(2)
      DIMENSION          GAMBL(144)
      DIMENSION          D( 36),   R(2),     Z(2),     ALPH(2)
      DIMENSION          FME(40),  FFE(40),  TL(12)
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON   /TRIMEX/
     1                   ECPT(18)
      COMMON   /MATIN/
     1                   MATIDC        ,MATFLG
     2,                  ELTEMP        ,STRESS
     3,                  SINTH         ,COSTH
      COMMON   /MATOUT/
     1                   E(3)          ,ANU(3)
     2,                  RHO           ,G(3)
     3,                  ALF(3)        ,TZERO,    GSUBE
C
      EQUIVALENCE ( CONSTS(2) , TWOPI  )
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE        (IECPT(1) , ECPT(1))
      EQUIVALENCE        (A1, ALPH(1)), (A2, ALPH(2))
      EQUIVALENCE        (R1, R(1)),    (R2, R(2))
      EQUIVALENCE        (Z1, Z(1)),    (Z2, Z(2))
      EQUIVALENCE        (GAMBQM(1), GAMBQ(1))
      EQUIVALENCE        (GAMBQF(1), GAMBQ(49))
      EQUIVALENCE        (DELINT(1), GAMBQ(1))
      EQUIVALENCE        (FME(1),    GAMBQ(43))
      EQUIVALENCE        (FFE(1),    GAMBQ(83))
      EQUIVALENCE        (GAMRS(1),  GAMBQ(1))
      EQUIVALENCE        (AKI(1),    GAMBQ(1))
      EQUIVALENCE        (GAMBL(1), GAMBQ(1))
C
C ----------------------------------------------------------------------
C
C STORE ECPT PARAMETERS IN LOCAL VARIABLES
C
      IDEL   = IECPT(1)
      IGP(1) = IECPT(2)
      IGP(2) =  IECPT(3)
      MATID  =  IECPT(7)
      ICS(1) =  IECPT(10)
      ICS(2) =  IECPT(14)
      ALPH(1)=  ECPT(4)
      ALPH(2)=  ECPT(5)
      TM     =  ECPT(8)
      TF     =  ECPT(9)
      R(1)   =  ECPT(11)
      D(1)   =  ECPT(12)
      Z(1)   =  ECPT(13)
      R(2)   =  ECPT(15)
      D(2)   =  ECPT(16)
      Z(2)   =  ECPT(17)
      TEMPE  =  ECPT(18)
C
C
C TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,2
      IF (R(I) .LT. 0.0E0) CALL MESAGE(-30,37,IDEL)
      IF (D(I) .NE. 0.0E0) CALL MESAGE(-30,37,IDEL)
  200 CONTINUE
C
C
C DETERMINE IF ELEMENT IS A TOROIDAL, CONICAL OR CYLINDRICAL RING
C
      ITORD = 0
      IF (ABS(A1-A2) .LE. .000001) ITORD = 1
      IF (ITORD .EQ. 1  .AND.  ABS(A1-90.0E0) .LE. .00001) ITORD = -1
C
C
C COMPUTE THE ELEMENT COORDINATES
C
      A1 = A1 * DEGRA
      A2 = A2 * DEGRA
      PHIB = A2 - A1
      SINA1 =  SIN(A1)
      COSA1 =  COS(A1)
      SINA2 =  SIN(A2)
      COSA2 =  COS(A2)
C
      IF (ITORD .NE. 0) GO TO 300
C
C FOR THE TOROIDAL RING
C
      RP =  SQRT( (R2-R1)**2 + (Z2-Z1)**2 )
     1      / (2.0E0 *  SIN(PHIB/2.0E0))
      S = PHIB * RP
      GO TO 350
C
C FOR THE CONICAL OR CYLINDRICAL RING
C
  300 CONTINUE
      RP = 0.0D0
      S  =  SQRT( (R2-R1)**2 + (Z2-Z1)**2 )
C
  350 CONTINUE
C
C
C COMPUTE THE BASIC AND REQUIRED INTEGRALS
C
C
C SET UP ARRAY OF CONSTANTS FOR ROMBER INTEGRATION ROUTINE
C
      D(21) = 0.0E0
      D(22) = RP
      D(23) = R1
      D(24) = COSA1
      D(25) = SINA1
C
C COMPUTE CONSTANTS NEEDED FOR INTEGRAL CALCULATIONS
C
      D(30) = R1 - RP * SINA1
      D(31) = RP * COSA1
      D(32) = RP * SINA1
      D(33) = COSA1 ** 2
      D(34) = SINA1 * COSA1
      D(35) = SINA1 ** 2
      D(36) = 0.5 - D(35)
C
C START LOOP  FOR CALCULATIONS OF INTEGRALS
C
      DO 500 JP1 = 1,7
      J = JP1 - 1
      K = (J * 6) + 1
      DJP1 = JP1
C
C TEST FOR ELEMENT SHAPE
C
      IF (ITORD) 470,400,430
C
C THE TOROIDAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1),...,D(6)
C
  400 CONTINUE
      D(20) = (RP ** JP1)
C
C COMPUTE I(J,1)
C
      D(1) = D(20) * (PHIB ** JP1) / DJP1
C
C COMPUTE I(J,2)
C
      D(2) = (PHIB ** (JP1+1)) / (DJP1 + 1.0E0)
      D(10) = 1.0E0
      DO 410 I = 1,20
      IP = JP1 + 2 * I + 1
      D(11) = 2 * I + 1
      D(10) = D(10) * D(11) * (D(11)-1.0E0)
      D(12) = (-1.0E0)** I  * PHIB ** IP
     1        / ((DJP1 + D(11)) * D(10))
      D(13) =  ABS( D(12) / D(2) )
      D(2) = D(2) + D(12)
      IF (D(13) .LE. 1.0E-10) GO TO 415
  410 CONTINUE
      CALL MESAGE(-30,26,IDEL)
  415 CONTINUE
      D(2) = D(20) * D(2)
C
C COMPUTE I(J,3)
C
      D(3) = (PHIB ** JP1) / DJP1
      D(10) = 1.0E0
      DO 420 I = 1,20
      IP = JP1 + 2 * I
      D(11) = 2 * I
      D(10) = D(10) * D(11) * (D(11) - 1.0E0)
      D(12) = (-1.0E0)** I  * PHIB ** IP
     1        / ((DJP1 + D(11)) * D(10))
      D(13) =  ABS( D(12) / D(3) )
      D(3) = D(3) + D(12)
      IF (D(13) .LE. 1.0E-10) GO TO 425
  420 CONTINUE
      CALL MESAGE(-30,26,IDEL)
  425 CONTINUE
      D(3) = D(20) * D(3)
      D(26) = DJP1
C
C COMPUTE I(J,4)
C
      CALL ROMBER (PHIB, D(10), IP, D(4), 1, D(21) )
      IF (IP .GE. 15) CALL MESAGE (30,26,IDEL)
      D(4) = D(20) * D(4)
C
C COMPUTE I(J,5)
C
      CALL ROMBER (PHIB, D(10), IP, D(5), 2, D(21) )
      IF (IP .GE. 15) CALL MESAGE (30,26,IDEL)
      D(5) = D(20) * D(5)
C
C COMPUTE I(J,6)
C
      CALL ROMBER (PHIB, D(10), IP, D(6), 3, D(21) )
      IF (IP .GE. 15) CALL MESAGE (30,26,IDEL)
      D(6) = D(20) * D(6)
C
C THE TOROIDAL RING REQUIRED INTEGRALS
C
      DELINT(K  ) = D(30) * D(1) + D(31) * D(2) + D(32) * D(3)
      DELINT(K+1) = COSA1 * D(2) + SINA1 * D(3)
      DELINT(K+2) = D(33) * D(4) + D(34) * D(5) + D(35) * D(6)
      DELINT(K+3) = COSA1 * D(3) - SINA1 * D(2)
      DELINT(K+4) = D(34) * (D(6)-D(4))  + D(36) * D(5)
      DELINT(K+5) = D(33) * D(6) - D(34) * D(5) + D(35) * D(4)
      GO TO 490
C
C THE CONICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1) AND D(2)
C
  430 CONTINUE
C
C COMPUTE I(J,1)
C
      D(1) = (S ** JP1) / DJP1
C
      IF (J - 1) 435,440,445
C
C COMPUTE I(0,2)
C
  435 CONTINUE
      D(2) = ALOG( (R1 + S*COSA1) / R1 ) / COSA1
      GO TO 460
C
C COMPUTE I(1,2)
C
  440 CONTINUE
      D(2) = (S - (R1/COSA1) * ALOG( (R1 + S*COSA1) / R1 )) / COSA1
      GO TO 460
C
C COMPUTE I(J,2) WHERE J .GT. 1
C
  445 CONTINUE
      D(2) = 1.0E0 / DJP1
      D(10) =-S * COSA1 / R1
      DO 450 I = 1,1000
      D(11) = JP1 + I
      D(12) = (D(10) ** I) / D(11)
      D(2) = D(2) + D(12)
      IF (D(12) .LT. 1.0E-4 ) GO TO 455
  450 CONTINUE
      CALL MESAGE(-30,26,IDEL)
  455 CONTINUE
      D(2) = ( (S ** JP1) / R1 ) * D(2)
  460 CONTINUE
C
C THE CONICAL RING REQUIRED INTEGRALS
C
      DELINT(K  ) = R1 * D(1) + COSA1 * ((S**(JP1+1)) / (DJP1+1.0E0))
      DELINT(K+1) = SINA1 * D(1)
      DELINT(K+2) = D(35) * D(2)
      DELINT(K+3) = COSA1 * D(1)
      DELINT(K+4) = D(34) * D(2)
      DELINT(K+5) = D(33) * D(2)
      GO TO 490
C
C THE CYLINDRICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1) AND D(2)
C
  470 CONTINUE
C
C COMPUTE I(J,1)
C
      D(1) = (S ** JP1) / DJP1
C
C COMPUTE I(J,2)
C
      D(2) = D(1) / R1
C
C THE CYLINDRICAL RING REQUIRED INTEGRALS
C
      DELINT(K  ) = R1 * D(1) + COSA1 * ((S**(JP1+1)) / (DJP1+1.0E0))
      DELINT(K+1) = SINA1 * D(1)
      DELINT(K+2) = D(35) * D(2)
      DELINT(K+3) = 0.0E0
      DELINT(K+4) = 0.0E0
      DELINT(K+5) = 0.0E0
C
  490 CONTINUE
  500 CONTINUE
C
C
C LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
C
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      CALL MAT(IDEL)
C
C
C SET MATERIAL PROPERTIES IN LOCAL VARIABLES
C
      EP = E(1)
      ET = E(2)
      VPT= ANU(1)
      TZ = TZERO
      VTP= VPT * ET / EP
      DEL = 1.0E0 - VPT * VTP
C
C
C GENERATE THE ELASTIC CONSTANTS MATRIX(2X2)
C
      EE(1) = EP / DEL
      EE(2) = ET * VPT / DEL
      EE(3) = EE(2)
      EE(4) = ET / DEL
C
C
C CALL THE FCURL   SUBROUTINE TO FORM THE FOUR (2X10) MATRICES OF
C INTEGRALS (TRANSPOSED)
C
C COMPUTE CONSTANTS NEEDED IN FCURL  SUBROUTINE
C
      D(1) = 0.0E0
      IF (ITORD .EQ. 0) D(1) = 1.0E0 / RP
C
C NOTE THE DOUBLE SUBSCRIPTING USED IN  FCURL  SUBROUTINE IS
C COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
C IS A ( 7X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
C PROGRAM AND IT IS A (6X 7) DOUBLY SUBSCRIPTED ARRAY (STORED
C COLUMNWISE) IN FCURL  ROUTINE.
C
C
      CALL FCURL (FME(1), FME(21), FFE(1), FFE(21), DELINT(1), S, D(1))
C
      D(1) = TWOPI * TM
      D(2) = TWOPI * (TF **3) / 12.0E0
      DO 550 I = 1,40
      FME(I) = D(1) * FME(I)
      FFE(I) = D(2) * FFE(I)
  550 CONTINUE
C
C
C FORM THE THERMAL STRAINS
C
      DTM1 = TI(1) - TZ
      DTM2 = TI(2) - TI(1)
      DTF1 = 0.0E0
      DTF2 = 0.0E0
C
C THE TERMS DTF1 AND DTF2 ARE FUNCTIONS OF THE FLEXURAL GRADIENT
C TEMPERATURE BUT SINCE THESE TEMPERATURES ARE NOT AVAILABLE
C THE TERMS WILL BE SET TO ZERO. THEY ARE USUALLY DEFINED AS FOLLOWS,
C     DTF1 = TF(1) - TZ
C     DTF2 = TF(2) - TF(1)
C WHERE TF(1) AND TF(2) ARE THE FLEXURAL GRADIENT TEMPERATURES AT
C GRID POINTS 1 AND 2 RESPECTIVELY.
C
      D(1) = DTM1 * ALF(1)
      D(2) = DTM1 * ALF(2)
      D(3) = DTM2 * ALF(1)
      D(4) = DTM2 * ALF(2)
      D(5) = DTF1 * ALF(1)
      D(6) = DTF1 * ALF(2)
      D(7) = DTF2 * ALF(1)
      D(8) = DTF2 * ALF(2)
C
C
C FORM THE   THERMAL LOAD   IN FIELD COORDINATES
C
      CALL GMMATS (EE(1), 2, 2, 0, D(1), 2, 1, 0, D(11) )
      CALL GMMATS (EE(1), 2, 2, 0, D(3), 2, 1, 0, D(13) )
      CALL GMMATS (EE(1), 2, 2, 0, D(5), 2, 1, 0, D(15) )
      CALL GMMATS (EE(1), 2, 2, 0, D(7), 2, 1, 0, D(17) )
C
C
      CALL GMMATS (FME( 1),  2,10, 1, D(11), 2, 1, 0, TL(1) )
      CALL GMMATS (FME(21),  2,10,-1, D(13), 2, 1, 0, TL(1) )
      CALL GMMATS (FFE( 1),  2,10,-1, D(15), 2, 1, 0, TL(1) )
      CALL GMMATS (FFE(21),  2,10,-1, D(17), 2, 1, 0, TL(1) )
C
C
C FORM THE TRANSFORMATION MATRIX(10X12) FROM FIELD COORDINATES TO GRID
C POINT DEGREES OF FREEDOM
C
      DO 600 I = 1,72
      GAMBQF(I) = 0.0E0
  600 CONTINUE
      D(1) = S
      D(2) = S ** 2
      D(3) = S ** 3
      D(4) = S ** 4
      D(5) = S ** 5
      GAMBQF( 3) = 1.0E0
      GAMBQF(16) = 1.0E0
      GAMBQF(30) = 0.5E0
      GAMBQF(39) = -10.0E0 / D(3)
      GAMBQF(40) = - 6.0E0 / D(2)
      GAMBQF(42) = - 1.5E0 / D(1)
      GAMBQF(45) = -GAMBQF(39)
      GAMBQF(46) = - 4.0E0 / D(2)
      GAMBQF(48) =   0.5E0 / D(1)
      GAMBQF(51) =  15.0E0 / D(4)
      GAMBQF(52) =   8.0E0 / D(3)
      GAMBQF(54) =   1.5E0 / D(2)
      GAMBQF(57) = -GAMBQF(51)
      GAMBQF(58) =   7.0E0 / D(3)
      GAMBQF(60) = - 1.0E0 / D(2)
      GAMBQF(63) = - 6.0E0 / D(5)
      GAMBQF(64) = - 3.0E0 / D(4)
      GAMBQF(66) = - 0.5E0 / D(3)
      GAMBQF(69) = -GAMBQF(63)
      GAMBQF(70) =  GAMBQF(64)
      GAMBQF(72) = -GAMBQF(66)
      DO 650 I = 1,48
      GAMBQM(I) = 0.0E0
  650 CONTINUE
      GAMBQM( 1) = 1.0E0
      GAMBQM(17) = 1.0E0
      GAMBQM(25) = - 3.0E0 / D(2)
      GAMBQM(29) = - 2.0E0 / D(1)
      GAMBQM(31) = -GAMBQM(25)
      GAMBQM(35) = - 1.0E0 / D(1)
      GAMBQM(37) =   2.0E0 / D(3)
      GAMBQM(41) =   1.0E0 / D(2)
      GAMBQM(43) = -GAMBQM(37)
      GAMBQM(47) =  GAMBQM(41)
C
C
C TRANSFORM THE   THERMAL LOAD   TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS(GAMBQ(1), 10, 12, 1, TL(1), 10,  1, 0, D(1) )
C
C
C FORM THE TRANSFORMATION MATRIX (12X12) FROM ELEMENT TO BASIC
C COORDINATES
C
      DO 700 I = 1,144
      GAMRS(I) = 0.0E0
  700 CONTINUE
      GAMRS( 1) =  COSA1
      GAMRS( 3) = -SINA1
      GAMRS(25) =  SINA1
      GAMRS(27) =  COSA1
      GAMRS(40) = -1.0E0
      GAMRS(53) =  1.0E0
      GAMRS(66) =  1.0E0
      GAMRS(79) =  COSA2
      GAMRS(81) = -SINA2
      GAMRS(103)=  SINA2
      GAMRS(105)=  COSA2
      GAMRS(118)= -1.0E0
      GAMRS(131)=  1.0E0
      GAMRS(144)=  1.0E0
C
C
C
C TRANSFORM THE   THERMAL LOAD   FROM ELEMENT TO BASIC COORDINATES
C
      CALL GMMATS(GAMRS(1), 12, 12, 1,  D(1), 12,  1, 0, TL(1) )
C
C
C LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL COORDINATES
C FOR THE TWO GRID POINTS AND EXPAND TO (6X6)
C
      DO 730 I = 1,144
      GAMBL(I) = 0.0E0
  730 CONTINUE
      DO 800 I = 1,2
      CALL GBTRAN(ICS(I),ECPT(4*I+10),D(1))
      K = 78 * (I - 1)
      DO 750 J = 1,3
      KK = K + 12* (J-1) + 1
      KL = 3 * (J-1) + 1
      KJ = K + 12* (J+2) + J + 3
      GAMBL(KK  ) = D(KL  )
      GAMBL(KK+1) = D(KL+1)
      GAMBL(KK+2) = D(KL+2)
      GAMBL(KJ) = 1.0E0
  750 CONTINUE
  800 CONTINUE
C
C
C
C TRANSFORM THE   THERMAL LOAD   FROM BASIC TO LOCAL COORDINATES
C
      CALL GMMATS (GAMBL(1), 12, 12, 1, TL(1), 12,  1, 0, D(1) )
C
C
C
C ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
C
      K = 0
      DO 900 I = 1,2
      L = IGP(I) - 1
      DO 900 J = 1,6
      K = K + 1
      L = L + 1
      PG(L) = PG(L) +  D(K)
  900 CONTINUE
C
C
      RETURN
      END
