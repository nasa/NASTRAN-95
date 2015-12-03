      SUBROUTINE STPAX1
C
C     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE AXI-
C     SYMMETRIC WITH A TRAPEZOIDAL CROSS SECTION
C
C
C     ECPT (01) = ELEMENT ID                                I
C     ECPT (02) = SIL A                                     I
C     ECPT (03) = SIL B                                     I
C     ECPT (04) = SIL C                                     I
C     ECPT (05) = SIL D
C     ECPT (06) = MATERIAL ORIENTATION ANGLE(DEGREES)       R
C     ECPT (08) = MATERIAL ID                               I
C     ECPT (09) TO ECPT (22) FOR PHI
C     ECPT (23) = COOR. SYS. FOR GRID POINT A               I
C     ECPT (24) = X-COOR. OF GRID POINT A (IN BASIC COOR)   R
C     ECPT (25) = Z-COOR. OF GRID POINT A (IN BASIC COOR)   R
C     ECPT (26) = 0.0
C     ECPT (27) = COOR. SYS. FOR GRID POINT B
C     ECPT (28) = X-COOR. OF GRID POINT B (IN BASIC COOR)   R
C     ECPT (29) = Z-COOR. OF GRID POINT B (IN BASIC COOR)   R
C     ECPT (30) = 0.0
C     ECPT (31) = COOR. SYS. FOR GRID POINT C               I
C     ECPT (32) = X-COOR. FOR GRID POINT C                  R
C     ECPT (33) = Z-COOR. FOR GRID POINT C                  R
C     ECPT (34) = 0.0
C     ECPT (35) = COOR. SYS. FOR GRID POINT D               I
C     ECPT (36) = X-COOR FOR GRID POINT D                   R
C     ECPT (37) = Z-COOR FOR GRID POINT D                   R
C     ECPT (38) = 0.0
C     ECPT (39) = EL. TEMPERATURE FOR MATERIAL PROP         R
C
C     ANY GROUP OF STATEMENTS PREFACED BY AN IF STATEMENT CONTAINING
C     ...KSYS78 OR LSYS78 ...  INDICATES CODING NECESSARY FOR THIS
C     ELEMENT*S PIEZOELECTRIC CAPABILITY
C
C     KSYS78 = 0   ELASTIC, NON-PIEZOELECTRIC MATERIAL
C     KSYS78 = 1   ELECTRICAL-ELASTIC COUPLED, PIEZOELETRIC MATERIAL
C     KSYS78 = 2   ELASTIC ONLY, PIEZOELECTRIC MATERIAL
C     LSYS78 = .TRUE. IF KSYS78 = 0, OR 2
C
      LOGICAL         PZMAT,LSYS78
      INTEGER         SP(50)
      DIMENSION       IECPT(40),DELINT(12),TEO(45),ACURL(208),
     1                ICS(4),D1(48),D2(16),ACURP1(48),ACURP2(16),
     2                GABABQ(12,12),GBP(4,4),ALFB(6),EE(63),WJP(3,4)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
C
C     ECPT COMMON BLOCK
C
      COMMON /SDR2X5/ ECPT(39),DUM5(61),IDEL,IGP(4),TZ,SEL(360),TS(06),
     1                AK(144),PHI(14),AKPH2(16),AKUPH(48),SELP1(120),
     2                SELP2(180),SELP3(60)
      COMMON /SDR2X6/ D(144),E1(36),WJ(6,12),R(5),Z(5)
     1
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO,GSUBE,MOSKP(9),
     1                SETMAT
      COMMON /MATPZ / PZOUT(51)
C     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
C                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
C                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
C                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
C                     EPS22,EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
      COMMON /SYSTEM/ IBUF,IOUT,DUM75(75),KSYS78
      COMMON /CONDAS/ CONSTS(5)
      EQUIVALENCE     (CONSTS(1),PI),(CONSTS(2),TWOPI),
     1                (CONSTS(4),DEGRAD),(ACURL(1),AK(1)),
     2                (IECPT(1),ECPT(1)),(R(1),R1),(R(2),R2),
     3                (R(3),R3),(R(4),R4),(Z(1),Z1),
     4                (Z(2),Z2),(Z(3),Z3),(Z(4),Z4),
     5                (ACURP1(1),ACURL(145)),(ACURP2(1),ACURL(193))
C
      LSYS78 = .FALSE.
      IF (KSYS78.EQ.0 .OR. KSYS78.EQ.2) LSYS78 = .TRUE.
C
C     START EXECUTION
C
C     STORE ECPT PARAMETERS IN LOCAL VARIABLES
C
      IDEL   = IECPT( 1)
      IGP(1) = IECPT( 2)
      IGP(2) = IECPT( 3)
      IGP(3) = IECPT( 4)
      IGP(4) = IECPT( 5)
      MATID  = IECPT( 8)
      ICS(1) = IECPT(23)
      ICS(2) = IECPT(27)
      ICS(3) = IECPT(31)
      R(1)   =  ECPT(24)
      D(1)   =  ECPT(26)
      Z(1)   =  ECPT(25)
      R(2)   =  ECPT(28)
      Z(2)   =  ECPT(29)
      D(2)   =  ECPT(30)
      R(3)   =  ECPT(32)
      Z(3)   =  ECPT(33)
      D(3)   =  ECPT(34)
      ICS(4) = IECPT(35)
      Z(4)   =  ECPT(37)
      D(4)   =  ECPT(38)
      R(4)   =  ECPT(36)
      TEMPE  =  ECPT(39)
      DGAMA  =  ECPT( 6)
C
C     TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,4
      IF (R(I) .LE. 0.0) GO TO 910
      IF (D(I) .NE. 0.0) GO TO 910
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
      IF (RMAX/RMIN .LE. 10.) GO TO 206
C
C     RATIO OF RADII IS TOO LARGE FOR GAUSS QUADRATURE FOR IP=-1
C
      IDEL1 = IDEL/1000
      WRITE  (IOUT,205) UFM,IDEL1
  205 FORMAT (A23,', TRAPAX ELEMENT',I9,' HAS A MAXIMUM TO MINIMUM ',
     1       'RADIUS RATIO EXCEEDING 10.', /5X,'ACCURACY OF NUMERICAL',
     2       ' INTEGRATION WOULD BE IN DOUBT.')
      GO TO 910
  206 CONTINUE
C
C     FORM THE TRANSFORMMATION MATRIX(12X12) FROM FIELD COOR, TO GRID
C     POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,144
  300 GABABQ( I, 1) = 0.0
      GABABQ( 1, 1) = 1.0
      GABABQ( 2, 1) = R1
      GABABQ( 3, 1) = Z1
      GABABQ( 4, 1) = R1*Z1
      GABABQ( 5, 2) = 1.0
      GABABQ( 6, 2) = R1
      GABABQ( 7, 2) = Z1
      GABABQ( 8, 2) = GABABQ(4,1)
      GABABQ( 9, 3) = 1.0
      GABABQ(10, 3) = R1
      GABABQ(11, 3) = Z1
      GABABQ(12, 3) = GABABQ(4,1)
      GABABQ( 1, 4) = 1.0
      GABABQ( 2, 4) = R2
      GABABQ( 3, 4) = Z2
      GABABQ( 4, 4) = R2*Z2
      GABABQ( 5, 5) = 1.0
      GABABQ( 6, 5) = R2
      GABABQ( 7, 5) = Z2
      GABABQ( 8, 5) = GABABQ(4,4)
      GABABQ( 9, 6) = 1.0
      GABABQ(10, 6) = R2
      GABABQ(11, 6) = Z2
      GABABQ(12, 6) = GABABQ(4,4)
      GABABQ( 1, 7) = 1.0
      GABABQ( 2, 7) = R3
      GABABQ( 3, 7) = Z3
      GABABQ( 4, 7) = R3*Z3
      GABABQ( 5, 8) = 1.0
      GABABQ( 6, 8) = R3
      GABABQ( 7, 8) = Z3
      GABABQ( 8, 8) = GABABQ(4,7)
      GABABQ( 9, 9) = 1.0
      GABABQ(10, 9) = R3
      GABABQ(11, 9) = Z3
      GABABQ(12, 9) = GABABQ(4,7)
      GABABQ( 1,10) = 1.0
      GABABQ( 2,10) = R4
      GABABQ( 3,10) = Z4
      GABABQ( 4,10) = R4*Z4
      GABABQ( 5,11) = 1.0
      GABABQ( 6,11) = R4
      GABABQ( 7,11) = Z4
      GABABQ( 8,11) = GABABQ(4,10)
      GABABQ( 9,12) = 1.0
      GABABQ(10,12) = R4
      GABABQ(11,12) = Z4
      GABABQ(12,12) = GABABQ(4,10)
C
      IF (LSYS78) GO TO 305
      GBP(1,1) = 1.0
      GBP(2,1) = R(1)
      GBP(3,1) = Z(1)
      GBP(4,1) = R(1)*Z(1)
      GBP(1,2) = 1.0
      GBP(2,2) = R(2)
      GBP(3,2) = Z(2)
      GBP(4,2) = R(2)*Z(2)
      GBP(1,3) = 1.0
      GBP(2,3) = R(3)
      GBP(3,3) = Z(3)
      GBP(4,3) = R(3)*Z(3)
      GBP(1,4) = 1.0
      GBP(2,4) = R(4)
      GBP(3,4) = Z(4)
      GBP(4,4) = R(4)*Z(4)
  305 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (12,GABABQ,12,D(10),0,D(11),ISING,SP)
      IF (ISING .EQ. 2) GO TO 920
C
      IF (KSYS78 .EQ. 1) CALL INVERS (4,GBP,4,D(10),0,D(11),ISING,SP)
      IF (ISING  .EQ. 2) GO TO 920
C
C     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
C
C     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT
C
C       DELINT(1) = (-1,0)
C       DELINT(02)= (-1,1)
C       DELINT(03)= (-1,2)
C       DELINT(04)= ( 0,0)
C       DELINT(05)= ( 0,1)
C       DELINT(06)= ( 0,2)
C       DELINT(07)= ( 1,0)
C       DELINT(08)= ( 1,1)
C       DELINT(09)= ( 1,2)
C       DELINT(10)= ( 2,0)
C       DELINT(11)= ( 2,1)
C       DELINT(12)= ( 3,0)
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
      DELINT(I1) = RZINTS(IP,IQ,R,Z,4)
  350 CONTINUE
  400 CONTINUE
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
C
      MATIDC = MATID
      MATFLG = 7
      IF (KSYS78 .GT. 0) MATFLG = 9
      ELTEMP = TEMPE
      DGAMR  = DGAMA*DEGRAD
      SINTH  = SIN(DGAMR)
      COSTH  = COS(DGAMR)
      COSG   = COSTH
      SING   = SINTH
      CALL MAT (IDEL)
      PZMAT  = .FALSE.
      IF (SETMAT.EQ.4. .OR. SETMAT.EQ.5.) PZMAT = .TRUE.
      IF (PZMAT) GO TO 410
      KSAVE  = KSYS78
      KSYS78 = 0
      LSYS78 = .TRUE.
      GO TO 420
  410 RHO    = PZOUT(46)
      ALF(1) = PZOUT(47)
      ALF(2) = PZOUT(48)
      ALF(3) = PZOUT(49)
      TZERO  = PZOUT(50)
      GSUBE  = PZOUT(51)
  420 CONTINUE
      IF (SETMAT .EQ. 2.0) GO TO 915
      TZ = TZERO
      IF (KSYS78 .GT. 0) GO TO 500
C
C     SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
C
      ER  = E(1)
      ET  = E(2)
      EZ  = E(3)
      VRO = ANU(1)
      VOZ = ANU(2)
      VZR = ANU(3)
      GOR = G(1)
      GZO = G(2)
      GRZ = G(3)
      VOR = VRO*ET/ER
      VZO = VOZ*EZ/ET
      VRZ = VZR*ER/EZ
      DEL = 1.0/(1.0 - VRO*VOR - VOZ*VZO - VZR*VRZ - VRO*VOZ*VZR -
     1           VRZ*VOR*VZO)
C
C     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
C
  500 CONTINUE
      DO 510 I = 1,45
  510 TEO (I) = 0.0
C
      IF (KSYS78 .GT. 0) GO TO 520
      TEO ( 1) = ER*(1.0 - VOZ*VZO)*DEL
      TEO ( 2) = ER*(VZR + VZO*VOR)*DEL
      TEO ( 3) = EZ*(1.0 - VRO*VOR)*DEL
      TEO ( 4) = ER*(VOR + VZR*VOZ)*DEL
      TEO ( 5) = ET*(VZO + VRO*VZR)*DEL
      TEO ( 6) = ET*(1.0 - VRZ*VZR)*DEL
      TEO (10) = GRZ
      TEO (15) = GOR
      TEO (21) = GZO
      GO TO 530
  520 CONTINUE
C
C     PIEZOELECTRIC MATERIAL PROPERTIES STORED IN TEO(22-39)
C     DIELECTRIC MATERIAL PROPERTIES STORED IN TEO(40-45)
C     TEO(22-39) CONTAINS E-TRANSPOSE
C
      TEO( 1) = PZOUT( 1)
      TEO( 2) = PZOUT( 2)
      TEO( 3) = PZOUT( 7)
      TEO( 4) = PZOUT( 3)
      TEO( 5) = PZOUT( 8)
      TEO( 6) = PZOUT(12)
      TEO( 7) = PZOUT( 4)
      TEO( 8) = PZOUT( 9)
      TEO( 9) = PZOUT(13)
      TEO(10) = PZOUT(16)
      TEO(11) = PZOUT( 5)
      TEO(12) = PZOUT(10)
      TEO(13) = PZOUT(14)
      TEO(14) = PZOUT(17)
      TEO(15) = PZOUT(19)
      TEO(16) = PZOUT( 6)
      TEO(17) = PZOUT(11)
      TEO(18) = PZOUT(15)
      TEO(19) = PZOUT(18)
      TEO(20) = PZOUT(20)
      TEO(21) = PZOUT(21)
C
      IF (KSYS78 .EQ. 2) GO TO 530
      TEO(22) = PZOUT(22)
      TEO(23) = PZOUT(28)
      TEO(24) = PZOUT(34)
      TEO(25) = PZOUT(23)
      TEO(26) = PZOUT(29)
      TEO(27) = PZOUT(35)
      TEO(28) = PZOUT(24)
      TEO(29) = PZOUT(30)
      TEO(30) = PZOUT(36)
      TEO(31) = PZOUT(25)
      TEO(32) = PZOUT(31)
      TEO(33) = PZOUT(37)
      TEO(34) = PZOUT(26)
      TEO(35) = PZOUT(32)
      TEO(36) = PZOUT(38)
      TEO(37) = PZOUT(27)
      TEO(38) = PZOUT(33)
      TEO(39) = PZOUT(39)
      TEO(40) =-PZOUT(40)
      TEO(41) =-PZOUT(41)
      TEO(42) =-PZOUT(42)
      TEO(43) =-PZOUT(43)
      TEO(44) =-PZOUT(44)
      TEO(45) =-PZOUT(45)
  530 CONTINUE
C
      C2   = COSG*COSG
      C4   = C2*C2
      S2   = SING*SING
      S4   = S2*S2
      C2S2 = C2*S2
      C3   = COSG*C2
      S3   = SING*S2
      CS2  = COSG*S2
      SC2  = SING*C2
      CS   = COSG*SING
C
      EE( 1) = TEO(1)*C4 + TEO(3)*S4 + 2.0*C2S2 * (TEO(2) + 2.0
     1       * TEO(10))
      EE( 2) = TEO(2)*(C4+S4) + C2S2 * (TEO(1)+TEO(3)-4.0*TEO(10))
      EE( 3) = TEO(1)*S4 + 2.0*C2S2 * (TEO(2)+2.0*TEO(10))
     3       +  TEO(3)*C4
      EE( 4) = TEO(4)*C2 + TEO(5)*S2
      EE( 5) = TEO(4)*S2 + TEO(5)*C2
      EE( 6) = TEO(6)
      EE( 7) = COSG*SING*S2 * (TEO(2)-TEO(3)+2.0*TEO(10))
     7       + SING*COSG*C2 * (TEO(1)-TEO(2)-2.0*TEO(10))
      EE( 8) = SING*COSG*C2 * (TEO(2)-TEO(3)+2.0*TEO(10))
     8       + COSG*SING*S2 * (TEO(1)-TEO(2)-2.0*TEO(10))
      EE( 9) = SING*COSG * (TEO(4) - TEO(5))
      EE(10) = C2S2 * (TEO(1)-2.0*TEO(2)+TEO(3)) + TEO(10)*(C2-S2)**2
      EE(12) = 0.0
      EE(13) = 0.0
      EE(15) = TEO(15)*C2 + TEO(21)*S2
      EE(20) = COSG*SING * (TEO(15) - TEO(21))
      EE(21) = TEO(15)*S2 + TEO(21)*C2
C
      IF (LSYS78) GO TO 540
C
C     PIEZOELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINATES
C
      EE(37) = C3*TEO(22) - S3*TEO(26) + CS2*(TEO(25)+2.0*TEO(32)) -
     7         SC2*(TEO(23)+2.0*TEO(31))
      EE(38) = C3*TEO(23) + S3*TEO(25) + CS2*(TEO(26)-2.0*TEO(31)) +
     8         SC2*(TEO(22)-2.0*TEO(32))
      EE(39) = S2*TEO(27) + C2*TEO(24) - 2.0*CS*TEO(33)
      EE(40) = C3*TEO(25) - S3*TEO(23) + CS2*(TEO(22)-2.0*TEO(32)) -
     O         SC2*(TEO(26)-2.0*TEO(31))
      EE(41) = C3*TEO(26) + S3*TEO(22) + CS2*(TEO(23)+2.0*TEO(31)) +
     1         SC2*( TEO(25)+2.0*TEO(32))
      EE(42) = S2*TEO(24) + C2*TEO(27) + 2.0*CS*TEO(33)
      EE(43) = COSG*TEO(28) - SING*TEO(29)
      EE(44) = COSG*TEO(29) + SING*TEO(28)
      EE(45) = TEO(30)
      EE(46) = C3*TEO(31) + S3*TEO(32) - CS2*(TEO(23)-TEO(26)+TEO(31)) +
     6         SC2*(-TEO(32)-TEO(25)+TEO(22))
      EE(47) = C3*TEO(32) - S3*TEO(31) - CS2*(TEO(25)-TEO(22)+TEO(32)) +
     7         SC2*(TEO(23)+TEO(31)-TEO(26))
      EE(48) = (C2-S2)*TEO(33) + CS*(TEO(24)-TEO(27))
      EE(49) = C2*TEO(34) + S2*TEO(38) - CS*(TEO(35)+TEO(37))
      EE(50) = C2*TEO(35) - S2*TEO(37) + CS*(TEO(34)-TEO(38))
      EE(51) = COSG*TEO(36) - SING*TEO(39)
      EE(52) = C2*TEO(37) - S2*TEO(35) - CS*(TEO(38)-TEO(34))
      EE(53) = C2*TEO(38) + S2*TEO(34) + CS*(TEO(35)+TEO(37))
      EE(54) = COSG*TEO(39) + SING*TEO(36)
C
C     DIELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINTES
C
      EE(55) = S2*TEO(43) - 2.0*CS*TEO(41) + C2*TEO(40)
      EE(56) = (C2-S2)*TEO(41) - CS*(TEO(43)-TEO(40))
      EE(57) =-SING*TEO(44) + COSG*TEO(42)
      EE(59) = C2*TEO(43) + 2.0*CS*TEO(41) + S2*TEO(40)
      EE(60) = COSG*TEO(44) + SING*TEO(42)
      EE(63) = TEO(45)
  540 CONTINUE
C
C     COMPUTE HARMONIC COEFFICIENT
C
      IECPT(1) = IECPT(1) - (IECPT(1)/1000)*1000 - 1
      AJHO  = IECPT(1)
      AJJHO = AJHO*AJHO
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
C
      ACURL ( 1) = (EE(6) + AJJHO * EE(15)) * DELINT(1)
      ACURL ( 2) = (EE(4) + EE(6) + AJJHO * EE(15)) * DELINT(4)
      ACURL ( 3) = (EE(6) + AJJHO * EE(15)) * DELINT(2)
     1           +  EE(9) * DELINT (4)
      ACURL ( 4) = (EE(4) + EE(6) + AJJHO * EE(15))* DELINT(5)
     1           +  EE(9) * DELINT(7)
      ACURL ( 5) = AJHO * (EE(6) + EE(15)) * DELINT(1)
      ACURL ( 6) = AJHO * EE(6) * DELINT(4)
      ACURL ( 7) = AJHO * (EE(6) + EE(15))* DELINT(2) - AJHO * EE(20)
     1           * DELINT(4)
      ACURL ( 8) = AJHO * EE(6) * DELINT(5) - AJHO * EE(20) * DELINT(7)
      ACURL ( 9) = AJJHO * EE(20) * DELINT(1)
      ACURL (10) = DELINT(4) * (EE(9) + AJJHO*EE(20))
      ACURL (11) = DELINT(4) * EE(5) + AJJHO * DELINT(2) * EE(20)
      ACURL (12) = DELINT(7) * EE(5) + DELINT(5)*(EE(9)+AJJHO*EE(20))
      ACURL (14) = (EE(1) + 2.0 * EE(4) + EE(6) + AJJHO * EE(15))
     1           * DELINT(7)
      ACURL (15) = (EE(4) + EE(6) + AJJHO * EE(15)) * DELINT(5)
     1           + (EE(7) + EE(9)) * DELINT(7)
      ACURL (16) = (EE(1) + 2.0 * EE(4) + AJJHO * EE(15) + EE(6))
     1           * DELINT(8) + (EE(7) + EE(9)) * DELINT(10)
      ACURL (17) = AJHO * (EE(4) + EE(6) + EE(15)) * DELINT(4)
      ACURL (18) = AJHO * (EE(4) + EE(6)) * DELINT(7)
      ACURL (19) = AJHO * (EE(4) + EE(6) + EE(15)) * DELINT(5) - AJHO
     1           * EE(20) * DELINT(7)
      ACURL (20) = AJHO * (EE(4) + EE(6)) * DELINT(8) - AJHO * EE(20)
     1           * DELINT(10)
      ACURL (21) = AJJHO * EE(20) * DELINT(4)
      ACURL (22) = DELINT(7) * (EE(7) + EE(9) + AJJHO*EE(20))
      ACURL (23) = DELINT(7)*(EE(2)+EE(5))+AJJHO*DELINT(5)*EE(20)
      ACURL (24) = DELINT(10)*(EE(2)+EE(5))+DELINT(8)*(EE(7)+EE(9)) +
     1             DELINT(8)*AJJHO*EE(20)
      ACURL (27) = (EE(6) + AJJHO * EE(15)) * DELINT(3) + 2.0
     1           * EE(9) * DELINT(5) + EE(10) * DELINT (7)
      ACURL (28) = (EE(4) + EE(6) + AJJHO * EE(15)) * DELINT(6)
     1           + EE(10) * DELINT(10) + (EE(7) + 2.0 * EE(9))
     2           * DELINT (8)
      ACURL (29) = AJHO * (EE(6) + EE(15)) * DELINT(2) + AJHO
     1           * EE(9) * DELINT(4)
      ACURL (30) = AJHO * EE(6) * DELINT(5) + AJHO * EE(9) * DELINT(7)
      ACURL (31) = AJHO * (EE(6) + EE(15)) * DELINT(3) + AJHO * (EE(9)
     1           - EE(20)) * DELINT(5)
      ACURL (32) = AJHO * (EE(9) - EE(20))* DELINT(8) + AJHO
     1           * EE(6) * DELINT(6)
      ACURL (33) = AJJHO * EE(20) * DELINT(2)
      ACURL (34) = DELINT(7)*EE(10) + DELINT(5)*(EE(9) + AJJHO*EE(20))
      ACURL (35) = DELINT(7)*EE(8) + DELINT(5)*EE(5) + AJJHO*DELINT(3)
     1           * EE(20)
      ACURL (36) = DELINT(10)*EE(8)+DELINT(8)*(EE(5)+EE(10)) +
     1             DELINT(6)*(EE(9)+AJJHO*EE(20))
      ACURL (40) = (EE(1) + 2.0 * EE(4) + EE(6) + AJJHO * EE(15))
     1           * DELINT(9) + (2.0 * EE(7) + 2.0 * EE(9))
     2           * DELINT(11)+ EE(10) * DELINT(12)
      ACURL (41) = AJHO * (EE(4) + EE(6) + EE(15)) * DELINT(5)
     1           + AJHO * EE(9) * DELINT(7)
      ACURL (42) = AJHO * (EE(4) + EE(6)) * DELINT(8) + AJHO * EE(9)
     1           * DELINT(10)
      ACURL (43) = AJHO * (EE(4) + EE(6) + EE(15))* DELINT(6)
     1           + AJHO * (EE(9) - EE(20)) * DELINT(8)
      ACURL (44) = AJHO * (EE(4) + EE(6)) * DELINT(9) + AJHO
     1           * (EE(9) - EE(20)) * DELINT(11)
      ACURL (45) = AJJHO * EE(20) * DELINT(5)
      ACURL (46) = DELINT(8)*(EE(7) + EE(9) + AJJHO*EE(20)) + DELINT(10)
     1           * EE(10)
      ACURL (47) = DELINT(8)*(EE(2) + EE(5)) + DELINT(10)*EE(8) +
     1             AJJHO*DELINT(6)*EE(20)
      ACURL (48) = DELINT(11)*(EE(2)+EE(5)+EE(10)) + DELINT(12)*EE(8) +
     1             DELINT(9)*(EE(7)+EE(9)+AJJHO*EE(20))
      ACURL (53) = (EE(15) + AJJHO * EE(6)) * DELINT(1)
      ACURL (54) = AJJHO * EE(6) * DELINT(4)
      ACURL (55) = (EE(15) + AJJHO * EE(6)) * DELINT(2) - EE(20)
     1           * DELINT(4)
      ACURL (56) = AJJHO * EE(6) * DELINT(5) - EE(20) * DELINT(7)
      ACURL (57) = AJHO * EE(20) * DELINT(1)
      ACURL (58) = AJHO*DELINT(4)*(EE(9)+EE(20))
      ACURL (59) = AJHO*(DELINT(4)*EE(5) + DELINT(2)*EE(20))
      ACURL (60) = AJHO*(DELINT(7)*EE(5)+DELINT(5)*(EE(9)+EE(20)))
      ACURL (66) = AJJHO * EE(6) * DELINT(7)
      ACURL (67) = AJJHO * EE(6) * DELINT(5)
      ACURL (68) = AJJHO * EE(6) * DELINT(8)
      ACURL (69) = 0.0
      ACURL (70) = AJHO*DELINT(7)*EE(9)
      ACURL (71) = AJHO*DELINT(7)*EE(5)
      ACURL (72) = AJHO*(DELINT(10)*EE(5)+DELINT(8)*EE(9))
      ACURL (79) = (EE(15) + AJJHO * EE(6)) * DELINT(3) - 2.0
     1           * EE(20) * DELINT(5) + EE(21) * DELINT(7)
      ACURL (80) = AJJHO * EE(6) * DELINT(6) - EE(20) * DELINT(8)
     1           + EE(21) * DELINT(10)
      ACURL (81) = AJHO * (EE(20) * DELINT(2) - EE(21) * DELINT(4))
      ACURL (82) = AJHO*(DELINT(5)*(EE(9)+EE(20))-DELINT(7)*EE(21))
      ACURL (83) = AJHO*(DELINT(5)*(EE(5)-EE(21))+DELINT(3)*EE(20))
      ACURL (84) = AJHO*(DELINT(8)*(EE(5)-EE(21))+DELINT(6)*(EE(9) +
     1             EE(20)))
      ACURL (92) = EE(21) * DELINT(12) + AJJHO * EE(6) * DELINT(9)
      ACURL (93) =-AJHO * EE(21) * DELINT(7)
      ACURL (94) = AJHO*(DELINT(8)*EE(9)-DELINT(10)*EE(21))
      ACURL (95) = AJHO* DELINT(8) * (EE(5)-EE(21))
      ACURL (96) = AJHO*(DELINT(11)*(EE(5)-EE(21))+DELINT(9)*EE(9))
      ACURL (105) = AJJHO * EE(21) * DELINT(1)
      ACURL (106) = AJJHO*DELINT(4)*EE(21)
      ACURL (107) = AJJHO*DELINT(2)*EE(21)
      ACURL (108) = AJJHO*DELINT(5)*EE(21)
      ACURL (118) = DELINT(7)*(EE(10)+AJJHO*EE(21))
      ACURL (119) = DELINT(7)*EE(8)+AJJHO*DELINT(5)*EE(21)
      ACURL (120) = DELINT(10)*EE(8)+DELINT(8)*(EE(10)+AJJHO*EE(21))
      ACURL (131) = DELINT(7)*EE(3)+AJJHO*DELINT(3)*EE(21)
      ACURL (132) = DELINT(10)*EE(3)+DELINT(8)*EE(8)+AJJHO*DELINT(6)*
     1              EE(21)
      ACURL (144) = DELINT(12)*EE(3) + 2.0*DELINT(11)*EE(8) +
     1              DELINT(9)*(EE(10)+AJJHO*EE(21))
C
      IF (LSYS78) GO TO 550
      ACURL(145) = DELINT(1)*AJHO*(AJHO*EE(51)-EE(45))
      ACURL(146) = DELINT(4)*(EE(43)+AJHO*(AJHO*EE(51)-EE(49)-EE(45)))
      ACURL(147) = DELINT(2)*AJHO*(AJHO*EE(51)-EE(45))+DELINT(4)*
     7             (EE(44)-AJHO*EE(50))
      ACURL(148) = DELINT(5)*(EE(43)+AJHO*(AJHO*EE(51)-EE(49)-EE(45)))
     8           + DELINT(7)*(EE(44)-AJHO*EE(50))
      ACURL(149) = DELINT(4)*AJHO*(AJHO*EE(51)-EE(45)-EE(39))
      ACURL(150) = DELINT(7)*(EE(43)+EE(37)+AJHO*(AJHO*EE(51)-EE(49)
     O           - EE(45)-EE(39)))
      ACURL(151) = DELINT(5)*AJHO*(AJHO*EE(51)-EE(45)-EE(39))+DELINT(7)
     1           * (EE(44)+EE(38)-AJHO*EE(50))
      ACURL(152) = DELINT(8)*(EE(43)+EE(37)+AJHO*(AJHO*EE(51)-EE(49)-
     2             EE(45)-EE(39)))+DELINT(10)*(EE(44)+EE(38)-AJHO*
     2             EE(50))
      ACURL(153) = DELINT(2)*AJHO*(AJHO*EE(51)-EE(45))-DELINT(4)*AJHO
     3           * EE(48)
      ACURL(154) = DELINT(5)*(EE(43)+AJHO*(AJHO*EE(51)-EE(49)-EE(45)))
     4           + DELINT(7)*(EE(46)-AJHO*EE(48))
      ACURL(155) = DELINT(3)*AJHO*(AJHO*EE(51)-EE(45))+DELINT(5)*
     5             (EE(44)-AJHO*(EE(50)+EE(48)))+DELINT(7)*EE(47)
      ACURL(156) = DELINT(6)*(EE(43)+AJHO*(AJHO*EE(51)-EE(49)-EE(45)))
     6           + DELINT(8)*(EE(46)+EE(44)-AJHO*(EE(50)+EE(48)))+
     6             DELINT(10)*EE(47)
      ACURL(157) = DELINT(5)*AJHO*(AJHO*EE(51)-EE(45)-EE(39))-DELINT(7)
     7           * AJHO*EE(48)
      ACURL(158) = DELINT(8)*(EE(43)+EE(47)+AJHO*(AJHO*EE(51)-EE(49)-
     8             EE(45)-EE(39)))-DELINT(10)*(EE(46)-AJHO*EE(48))
      ACURL(159) = DELINT(6)*AJHO*(AJHO*EE(51)-EE(45)-EE(39))+DELINT(8)
     9           * (EE(44)+EE(38)-AJHO*(EE(50)+EE(48)))+DELINT(10)*
     9             EE(47)
      ACURL(160) = DELINT(9)*(EE(43)+EE(37)+AJHO*(AJHO*EE(51)-EE(49)-
     O             EE(45)-EE(39)))+DELINT(11)*(EE(46)+EE(44)+EE(38)-
     O             AJHO*(EE(50)+EE(48)))+DELINT(12)*EE(47)
      ACURL(161) = DELINT(1)*AJHO*(EE(51)-AJHO*EE(45))
      ACURL(162) = DELINT(4)*(-EE(49)+AJHO*(EE(51)+EE(43)-AJHO*EE(45)))
      ACURL(163) = DELINT(2)*AJHO*(EE(51)-AJHO*EE(45))+DELINT(4)*
     3             (AJHO*EE(44)-EE(50))
      ACURL(164) = DELINT(5)*(-EE(49)+AJHO*(EE(51)+EE(43)-AJHO*EE(51)))
     4           + DELINT(7)*(AJHO*EE(44)-EE(50))
      ACURL(165) =-DELINT(4)*AJJHO*EE(45)
      ACURL(166) = DELINT(7)*AJHO*(EE(43)-AJHO*EE(45))
      ACURL(167) = DELINT(7)*AJHO*EE(44)-DELINT(5)*AJJHO*EE(45)
      ACURL(168) = DELINT(8)*AJHO*(EE(43)-AJHO*EE(45))+DELINT(10)*
     8             AJHO*EE(44)
      ACURL(169) = DELINT(2)*AJHO*(EE(51)-AJHO*EE(45))-DELINT(4)*AJHO*
     9             EE(54)
      ACURL(170) = DELINT(5)*(-EE(49)+AJHO*(EE(51)+EE(43)-AJHO*EE(45)))
     O           + DELINT(7)*(EE(52)-AJHO*EE(54))
      ACURL(171) = DELINT(3)*AJHO*(EE(51)-AJHO*EE(45))+DELINT(5)*
     1             (AJHO*(EE(44)-EE(54))-EE(50))+DELINT(7)*EE(53)
      ACURL(172) = DELINT(6)*(-EE(49)+AJHO*(EE(51)+EE(43)-AJHO*EE(45)))
     2           + DELINT(8)*(EE(52)-EE(50)+AJHO*(EE(44)-EE(54)))
     2           + DELINT(10)*EE(53)
      ACURL(173) =-DELINT(5)*AJJHO*EE(45)-DELINT(7)*AJHO*EE(54)
      ACURL(174) = DELINT(8)*AJHO*(EE(43)-AJHO*EE(45))+DELINT(10)*
     4             (EE(54)-AJHO*EE(54))
      ACURL(175) =-DELINT(6)*AJJHO*EE(45)+DELINT(8)*AJHO*(EE(44)-
     5             EE(54))+DELINT(10)*EE(53)
      ACURL(176) = DELINT(9)*AJHO*(EE(43)-AJHO*EE(45))+DELINT(11)*
     6             (EE(52)+AJHO*(EE(44)-EE(54)))+DELINT(12)*EE(53)
      ACURL(177) = DELINT(1)*AJJHO*EE(54)
      ACURL(178) = DELINT(4)*AJHO*(AJHO*EE(54)-EE(52))
      ACURL(179) = DELINT(2)*AJJHO*EE(54)-DELINT(4)*AJHO*EE(53)
      ACURL(180) = DELINT(5)*AJHO*(AJHO*EE(54)-EE(52))-DELINT(7)*AJHO
     O           * EE(53)
      ACURL(181) = DELINT(4)*AJHO*(AJHO*EE(54)-EE(48))
      ACURL(182) = DELINT(7)*(EE(46)+AJHO*(AJHO*EE(54)-EE(52)-EE(48)))
      ACURL(183) = DELINT(5)*AJHO*(AJHO*EE(54)-EE(48))+DELINT(7)*
     3             (EE(47)-AJHO*EE(53))
      ACURL(184) = DELINT(8)*(EE(46)+AJHO*(AJHO*EE(54)-EE(52)-EE(48)))
     4           + DELINT(10)*(EE(47)-AJHO*EE(53))
      ACURL(185) = DELINT(2)*AJJHO*EE(54)-DELINT(4)*AJHO*EE(42)
      ACURL(186) = DELINT(5)*AJHO*(AJHO*EE(54)-EE(52))+DELINT(7)*(EE(40)
     6           - AJHO*EE(42))
      ACURL(187) = DELINT(3)*AJJHO*EE(54)-DELINT(5)*AJHO*(EE(53)+EE(42))
     7           + DELINT(7)*EE(41)
      ACURL(188) = DELINT(6)*AJHO*(AJHO*EE(54)-EE(52))+DELINT(8)*
     8             (EE(40)-AJHO*(EE(53)+EE(42)))+DELINT(10)*EE(41)
      ACURL(189) =-DELINT(5)*AJHO*EE(48)+DELINT(4)*AJJHO*EE(54)
     9           - DELINT(7)*AJHO*EE(42)
      ACURL(190) = DELINT(8)*(EE(46)-AJHO*EE(48))+DELINT(7)*AJHO*
     O             (AJHO*EE(54)-EE(52))+DELINT(10)*(EE(40)-AJHO*EE(42))
      ACURL(191) =-DELINT(6)*AJHO*EE(48)+DELINT(5)*AJJHO*EE(54)+
     1             DELINT(8)*(EE(47)-AJHO*EE(42))-DELINT(7)*AJHO*EE(53)
     1           + DELINT(10)*EE(41)
      ACURL(192) = DELINT(9)*(EE(46)-AJHO*EE(48))+DELINT(8)*AJHO*
     2             (AJHO*EE(54)-EE(52))+DELINT(11)*(EE(47)+EE(40)-
     2             AJHO*EE(42))-DELINT(10)*AJHO*EE(53)+DELINT(12)*EE(41)
C
      ACURL(193) = DELINT(1)*AJJHO*EE(63)
      ACURL(194) = DELINT(4)*AJHO*(AJHO*EE(63)-EE(57))
      ACURL(195) = DELINT(2)*AJJHO*EE(63)-DELINT(4)*AJHO*EE(60)
      ACURL(196) = DELINT(5)*AJHO*(AJHO*EE(63)-EE(57))-DELINT(7)*
     6             AJHO*EE(60)
      ACURL(197) = DELINT(4)*AJHO*(AJHO*EE(63)-EE(57))
      ACURL(198) = DELINT(7)*(AJJHO*EE(63)-2.0*AJHO*EE(57)+EE(55))
      ACURL(199) = DELINT(5)*AJHO*(AJHO*EE(63)-EE(57))+DELINT(7)*(EE(56)
     9           - AJHO*EE(60))
      ACURL(200) = DELINT(8)*(AJJHO*EE(63)-2.0*AJHO*EE(57)+EE(55))
     O           + DELINT(10)*(EE(56)-AJHO*EE(60))
      ACURL(201) = DELINT(2)*AJJHO*EE(63)-DELINT(4)*AJHO*EE(60)
      ACURL(202) = DELINT(5)*AJHO*(AJHO*EE(63)-EE(57))+DELINT(7)*
     2             (EE(56)-AJHO*EE(60))
      ACURL(203) = DELINT(3)*AJJHO*EE(63)-DELINT(5)*2.0*AJHO*EE(60)
     3           + DELINT(7)*EE(59)
      ACURL(204) = DELINT(6)*AJHO*(AJHO*EE(63)-EE(57))+DELINT(8)*
     4             (EE(56)-2.0*AJHO*EE(60))+DELINT(10)*EE(59)
      ACURL(205) = DELINT(5)*AJHO*(AJHO*EE(63)-EE(57))-DELINT(7)*
     5             AJHO*EE(60)
      ACURL(206) = DELINT(8)*(AJJHO*EE(63)-2.0*EE(57)+EE(55))+DELINT(10)
     6           * (EE(56)-AJHO*EE(60))
      ACURL(207) = DELINT(6)*AJHO*(AJHO*EE(63)-EE(57))+DELINT(8)*(EE(56)
     7           - 2.0*AJHO*EE(60))+DELINT(10)*EE(59)
      ACURL(208) = DELINT(9)*(AJJHO*EE(63)-2.0*AJHO*EE(57)+EE(55))+
     8             2.0*DELINT(11)*(EE(56)-AJHO*EE(60))+DELINT(12)*EE(59)
  550 CONTINUE
C
C     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
C     TO GRID POINT DEGREES OF FREEDOM
C
C     EXPAND ACURL INTO (12X12)
C
      DO 610 IB = 2,12
      IC = 13*IB - 25
      I  = IC
      DO 605 J = IB,12
      IC = IC + 12
      I  = I + 1
  605 ACURL(IC) = ACURL(I)
  610 CONTINUE
C
      DGAMA = PI
      IF (AJHO .EQ. 0.0) DGAMA = TWOPI
      DO 630 I = 1,144
  630 ACURL(I) = ACURL(I)*DGAMA
C
      IF (LSYS78) GO TO 638
      DO 632 I = 145,208
  632 ACURL(I) = ACURL(I)*DGAMA
  638 CONTINUE
C
      CALL GMMATS (GABABQ,12,12,1, ACURL, 12,12,0, D )
      CALL GMMATS (     D,12,12,0, GABABQ,12,12,0, AK)
C
      IF (LSYS78) GO TO 639
      CALL GMMATS (GABABQ,12,12,1, ACURP1,12,4,0, D1)
      CALL GMMATS (D1,12,4,0, GBP,4,4,0, AKUPH)
      CALL GMMATS (GBP,4,4,1, ACURP2,4,4,0, D2)
      CALL GMMATS (D2,4,4,0, GBP,4,4,0, AKPH2)
  639 CONTINUE
C
C     ********** COORDINATE SYSTEM NOT POSSIBLE ***********************
C     *** WITH RINGAX.  THE FOLLOWING CODE WILL IMPLEMENT IT  *********
C     IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZO-
C     ELECTRIC
C
C     ZERO OUT THE AKI MATRIX
C
C.    DO 700 I = 1,144
C.    AKI(I) = 0.0
C.700 CONTINUE
C.    DO 800 I = 1,4
C.    CALL TRANSS (ICS(I),D(1))
C.    K = 39 * (I-1) + 1
C.    DO 800 J = 1, 3
C.    KK = K + 12*(J-1)
C.    JJ = 3*(J-1) + 1
C.    AKI (KK  ) = D (JJ  )
C.    AKI (KK+1) = D (JJ+1)
C.    AKI (KK+2) = D (JJ+2)
C.800 CONTINUE
C
C     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
C
C.    CALL GMMATS (AKI(1),12,12,1, AK(1),12,12,0, D(1))
C.    CALL GMMATS (D(1),12,12,0, AKI(1),12,12,0, AK(1))
C
C     COMPUTE THE FIFTH GRID POINT
C
      R(5) = (R1 + R2 + R3 + R4)/4.0
      Z(5) = (Z1 + Z2 + Z3 + Z4)/4.0
C
C     FORM WJ MATRIX
C
      DO 9001 IKI = 1,5
      DO 1000 I = 1,72
 1000 WJ(I, 1) = 0.0
      RSUM     = R(IKI)
      ZSUM     = Z(IKI)
      ZDR      = ZSUM/RSUM
      WJ(1, 2) = 1.0
      WJ(1, 4) = ZSUM
      WJ(2,11) = 1.0
      WJ(2,12) = RSUM
      WJ(3, 1) = 1.0/RSUM
      WJ(3, 2) = 1.0
      WJ(3, 3) = ZDR
      WJ(3, 4) = ZSUM
      WJ(3, 5) = AJHO/RSUM
      WJ(3, 6) = AJHO
      WJ(3, 7) = AJHO*ZDR
      WJ(3, 8) = AJHO*ZSUM
      WJ(4, 3) = 1.0
      WJ(4, 4) = RSUM
      WJ(4,10) = 1.0
      WJ(4,12) = ZSUM
      WJ(5, 1) =-AJHO/RSUM
      WJ(5, 2) =-AJHO
      WJ(5, 3) =-AJHO*ZDR
      WJ(5, 4) =-AJHO*ZSUM
      WJ(5, 5) =-1.0/RSUM
      WJ(5, 7) =-ZDR
      WJ(6, 7) = 1.0
      WJ(6, 8) = RSUM
      WJ(6, 9) =-AJHO/RSUM
      WJ(6,10) =-AJHO
      WJ(6,11) =-AJHO*ZDR
      WJ(6,12) =-AJHO*ZSUM
C
      IF (LSYS78) GO TO 1060
C
C     FORM WJP MATRIX
C
      DO 1050 I = 1,3
      DO 1050 J = 1,4
 1050 WJP(I,J) = 0.0
C
      WJP(1,2) = 1.0
      WJP(1,4) = ZSUM
      WJP(2,3) = 1.0
      WJP(2,4) = RSUM
      WJP(3,1) =-AJHO/RSUM
      WJP(3,2) =-AJHO
      WJP(3,3) =-AJHO*ZDR
      WJP(3,4) =-AJHO*ZSUM
 1060 CONTINUE
C
C     EXPAND EE(21) INTO E1(36)
C
      DO 1065 I = 1,36
 1065 E1( I) = 0.0
      E1( 1) = EE( 1)
      E1( 2) = EE( 2)
      E1( 3) = EE( 4)
      E1( 4) = EE( 7)
      E1( 7) = EE( 2)
      E1( 8) = EE( 3)
      E1( 9) = EE( 5)
      E1(10) = EE( 8)
      E1(13) = EE( 4)
      E1(14) = EE( 5)
      E1(15) = EE( 6)
      E1(16) = EE( 9)
      E1(19) = EE( 7)
      E1(20) = EE( 8)
      E1(21) = EE( 9)
      E1(22) = EE(10)
      E1(29) = EE(15)
      E1(36) = EE(21)
C
C     COMPUTE THE STRESS MATRICES
C
      K = 72*(IKI-1) + 1
      CALL GMMATS (WJ,12,6,1, GABABQ,12,12,0, D(1))
      CALL GMMATS (E1(1),6,6,0, D(1),6,12,0, SEL(K))
C
      IF (LSYS78) GO TO 1070
      KP1 = 24*(IKI-1) + 1
      CALL GMMATS (WJP,4,3,1, GBP,4,4,0, D2(1))
      CALL GMMATS (EE(37),6,3,0, D2(1),3,4,0, SELP1(KP1))
      KP2 = 36*(IKI-1) + 1
      CALL GMMATS (EE(37),6,3,1, D(1),6,12,0, SELP2(KP2))
      KP3 = 12*(IKI-1) + 1
      CALL GMMATS (EE(55),3,3,0, D2(1),3,4,0, SELP3(KP3))
 1070 CONTINUE
C
C     ** COORDINATE SYSTEMS NOT POSSIBLE WITH RINGAX *******************
C     ** THE FOLLOWING CODE WILL IMPLEMENT IT **************************
C     ** NOTE THAT WJ IS SEL(K) IN FOLLOWING GMMATS ********************
C
C     ** IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZO-
C     ELECTRIC TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL
C     COORDINATES
C..   CALL GMMATS (WJ,6,12,0, AKI(1),12,12,0, SEL(K) )
C
C
 9001 CONTINUE
C
C     COMPUTE THE THERMAL STRAIN
C
      ALFB(1) = ALF(1)
      ALFB(2) = ALF(3)
      ALFB(3) = ALF(2)
      ALFB(4) = 0.0
      ALFB(5) = 0.0
      ALFB(6) = 0.0
C
C     COMPUTE THE THERMAL STRESS
C
      TS(1) = EE(1)*ALFB(1) + EE(2)*ALFB(2) + EE(4)*ALFB(3)
      TS(2) = EE(2)*ALFB(1) + EE(3)*ALFB(2) + EE(5)*ALFB(3)
      TS(3) = EE(4)*ALFB(1) + EE(5)*ALFB(2) + EE(6)*ALFB(3)
      TS(4) = EE(7)*ALFB(1) + EE(8)*ALFB(2) + EE(9)*ALFB(3)
      TS(5) = 0.0
      TS(6) = 0.0
C
C     SAVE ECPT(9) TO ECP(22)
C
      DO 9006 IKI = 1,14
      PHI (IKI) = ECPT(8+IKI)
 9006 CONTINUE
      GO TO 940
C
C     SET FATAL ERROR FLAG AND ALLOWING ERROR MESSAGES TO ACCUMLATE
C
  910 I = 37
       GO TO 930
  915 I = 126
       GO TO 930
  920 I = 26
  930 CALL MESAGE (-30,I,IDEL)
  940 IF (.NOT.PZMAT) KSYS78 = KSAVE
      RETURN
      END
