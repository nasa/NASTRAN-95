      SUBROUTINE STRAX1
C
C     THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE AXI-
C     SYMMETRIC RING WITH TRIANGULAR CROSS SECTION RING
C
C     ECPT ( 1) = ELEMENT ID
C     ECPT ( 2) = SIL A                                   I
C     ECPT ( 3) = SIL B                                   I
C     ECPT ( 4) = SIL C                                   I
C     ECPT ( 5) = MATERIAL ORIENTATION ANGLE(DEGREES)     R
C     ECPT ( 7) = MATERIAL ID                             I
C     ECPT ( 8) TO ECPT(21) = STRESS PHASE ANG.           R
C     ECPT (22) = CORD. SYS. GRID POINT A (NOT USED)      I
C     ECPT (23) = R-CORD OF GRID A                        R
C     ECPT (24) = Z-CORD OF GRID A                        R
C     ECPT (25) = 0.0                                     R
C     ECPT (26) = CORD. SYS. GRID POINT B (NOT USED)      I
C     ECPT (27) = R-CORD OF GRID B                        R
C     ECPT (28) = Z-CORD OF GRID B                        R
C     ECPT (29) = 0.0                                     R
C     ECPT (30) = CORD. SYS. GRID POINT C (NOT USED)      I
C     ECPT (31) = R-CORD OF GRID C                        R
C     ECPT (32) = Z-CORD OF GRID C                        R
C     ECPT (33) = 0.0                                     R
C     ECPT (34) = EL. TEMPERATURE FOR MATERIAL PROP       R
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
      DIMENSION       IECPT(35),DELINT(12),TEO(45),ACURL(117),R(3),
     1                Z(3),ICS(3),D1(27),D2(9),ACURP1(27),ACURP2(9),
     2                GABABP(3,3),EE(63),WJP(3,3)
C     ECPT COMMON BLOCK
      COMMON /SDR2X5/ ECPT(34),DUM5(66),IDEL,IGP(3),TZ,SEL(54),TS(6),
     1                AK(81),PHI(14),SELP1(18),AKPH2(9),AKUPH(27),
     2                SELP2(27),SELP3(9)
      COMMON /SDR2X6/ GABABQ(9,9), D(81), WJ(6,9)
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO,GSUBE,MOSKP(9),
     1                SETMAT
      COMMON /MATPZ / PZOUT(51)
C     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
C                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
C                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
C                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,EPS2
C                     EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SYSTEM/ KSYSTM(77),KSYS78
      EQUIVALENCE     (IECPT(1),ECPT(1)), (Z(1),Z1), (Z(2),Z2),
     1                (R(1),R1), (R(2),R2), (R(3),R3), (Z(3),Z3),
     2                (CONSTS(1),PI), (CONSTS(4),DEGRAD),
     3                (ACURP1(1),ACURL(82)), (ACURP2(1),ACURL(109))
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
      MATID  = IECPT( 7)
      ICS(1) = IECPT(22)
      R(1)   = ECPT( 23)
      Z(1)   = ECPT( 24)
      D(1)   = ECPT( 25)
      ICS(2) = IECPT(26)
      R(2)   = ECPT( 27)
      Z(2)   = ECPT( 28)
      D(2)   = ECPT( 29)
      ICS(3) = IECPT(30)
      R(3)   = ECPT( 31)
      Z(3)   = ECPT( 32)
      D(3)   = ECPT( 33)
      DGAMA  = ECPT(  5)
      TEMPE  = ECPT( 34)
C
C     TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,3
      IF (R(I) .LE. 0.0) GO TO 910
      IF (D(I) .NE. 0.0) GO TO 910
  200 CONTINUE
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z1,Z2,Z3)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
C
C     FORM THE TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES
C     TO GRID POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,81
  300 GABABQ(I,1) = 0.0
      AA = R2*Z3 + R1*Z2 + Z1*R3 - Z2*R3 - R1*Z3 - R2*Z1
      AA = 1.0/AA
      C1 = AA*(R2*Z3 - Z2*R3)
      C2 =-AA*(Z3 - Z2)
      C3 = AA*(R3 - R2)
      GABABQ(1,1) = C1
      GABABQ(1,2) = C2
      GABABQ(1,3) = C3
      GABABQ(2,4) = C1
      GABABQ(2,5) = C2
      GABABQ(2,6) = C3
      GABABQ(3,7) = C1
      GABABQ(3,8) = C2
      GABABQ(3,9) = C3
      IF (LSYS78) GO TO 302
      GABABP(1,1) = C1
      GABABP(1,2) = C2
      GABABP(1,3) = C3
  302 CONTINUE
      C1 =-AA*(R1*Z3 - Z1*R3)
      C2 = AA*(Z3 - Z1)
      C3 =-AA*(R3 - R1)
      GABABQ(4,1) = C1
      GABABQ(4,2) = C2
      GABABQ(4,3) = C3
      GABABQ(5,4) = C1
      GABABQ(5,5) = C2
      GABABQ(5,6) = C3
      GABABQ(6,7) = C1
      GABABQ(6,8) = C2
      GABABQ(6,9) = C3
      IF (LSYS78) GO TO 304
      GABABP(2,1) = C1
      GABABP(2,2) = C2
      GABABP(2,3) = C3
  304 CONTINUE
      C1 = AA*(R1*Z2 - Z1*R2)
      C2 =-AA*(Z2 - Z1)
      C3 = AA*(R2 - R1)
      GABABQ(7,1) = C1
      GABABQ(7,2) = C2
      GABABQ(7,3) = C3
      GABABQ(8,4) = C1
      GABABQ(8,5) = C2
      GABABQ(8,6) = C3
      GABABQ(9,7) = C1
      GABABQ(9,8) = C2
      GABABQ(9,9) = C3
      IF (LSYS78) GO TO 306
      GABABP(3,1) = C1
      GABABP(3,2) = C2
      GABABP(3,3) = C3
  306 CONTINUE
C
C     COMPUTE THE INTEGRAL VALUES IN ARRAY DELINT THE ORDER IS INDICATED
C     BY THE FOLLOWING TABLE
C
C       DELINT(01) = (-1,0)
C       DELINT(02) = (-1,1)
C       DELINT(03) = (-1,2)
C       DELINT(04) = (0, 0)
C       DELINT(05) = (0, 1)
C       DELINT(06) = (1, 0)
C
      RA  = (R1 + R2 + R3)/3.0
      ZA  = (Z1 + Z2 + Z3)/3.0
      RH  = AMIN1(R1,R2,R3)/10.0
      DR  = AMAX1(ABS(R1-R2),ABS(R2-R3),ABS(R3-R1))
      AREA= (R1*(Z2-Z3) + R2*(Z3-Z1) + R3*(Z1-Z2))/2.0
      I1  = 0
      DO 400 I = 1,2
      IP  = I - 2
      DO 350 J = 1,3
      IQ  = J  - 1
      I1  = I1 + 1
      IF (I1 .NE. 6) GO TO 310
      IP  = 1
      IQ  = 0
  310 IF (DR .GT. RH) GO TO 320
      DELINT(I1) = ((RA**IP)*(ZA**IQ))*AREA
      GO TO 330
  320 DELINT(I1) = AIS(3,IP,IQ,R,Z)
  330 DELINT(I1) = ABS(DELINT(I1))
  350 CONTINUE
  400 CONTINUE
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
C
      DGAMR  = DGAMA*DEGRAD
      COSG   = COS(DGAMR)
      SING   = SIN(DGAMR)
      SINTH  = SING
      COSTH  = COSG
      MATIDC = MATID
      MATFLG = 7
      IF (KSYS78 .GT. 0) MATFLG = 9
      ELTEMP = TEMPE
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
      IF (SETMAT .EQ. 2.0) GO TO 920
      TZ = TZERO
      IF (KSYS78 .GT. 0) GO TO 500
C
C     SET MATERIAL PROPERTIES IN LOCAL VARIABLES (AGAIN)
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
      DEL = 1.0/(1.0-VRO*VOR-VOZ*VZO-VZR*VRZ-VRO*VOZ*VZR-VRZ*VOR*VZO)
C
C     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
C
  500 CONTINUE
      DO 510 I = 1,45
  510 TEO (I) = 0.0
      IF (KSYS78 .GT. 0) GO TO 512
      TEO ( 1) = ER*(1.0 - VOZ*VZO)*DEL
      TEO ( 2) = ER*(VZR + VZO*VOR)*DEL
      TEO ( 3) = EZ*(1.0 - VRO*VOR)*DEL
      TEO ( 4) = ER*(VOR + VZR*VOZ)*DEL
      TEO ( 5) = ET*(VZO + VRO*VZR)*DEL
      TEO ( 6) = ET*(1.0 - VRZ*VZR)*DEL
      TEO (10) = GRZ
      TEO (15) = GOR
      TEO (21) = GZO
      GO TO 514
  512 CONTINUE
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
      IF (KSYS78 .EQ. 2) GO TO 514
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
  514 CONTINUE
C
      DO 520 I = 5,63
  520 EE(I) = 0.0
      C2  = COSG*COSG
      C4  = C2  *C2
      S2  = SING*SING
      S4  = S2  *S2
      C2S2= C2  *S2
      C3  = COSG*C2
      S3  = SING*S2
      CS2 = COSG*S2
      SC2 = SING*C2
      CS  = COSG*SING
C
      EE( 1) = TEO(1)*C4 + TEO(3)*S4 + 2.0*C2S2*(TEO(2) + 2.0*TEO(10))
      EE( 2) = TEO(2)*(C4+S4) + C2S2*(TEO(1) + TEO(3)-4.0*TEO(10))
      EE( 3) = TEO(4)*C2 + TEO(5)*S2
      EE( 4) = COSG*SING*S2*(TEO(2)-TEO(3) + 2.0*TEO(10))
     4       + SING*COSG*C2*(TEO(1)-TEO(2) - 2.0*TEO(10))
      EE( 7) = EE(2)
      EE( 8) = TEO(1)*S4 + 2.0*C2S2*(TEO(2) + 2.0*TEO(10)) + TEO(3)*C4
      EE( 9) = TEO(4)*S2 + TEO(5)*C2
      EE(10) = SING*COSG*C2 * (TEO(2)-TEO(3) + 2.0*TEO(10))
     O       + COSG*SING*S2 * (TEO(1)-TEO(2) - 2.0*TEO(10))
      EE(13) = EE(3)
      EE(14) = EE(9)
      EE(15) = TEO(6)
      EE(16) = SING*COSG*(TEO(4)-TEO(5))
      EE(19) = EE(4)
      EE(20) = EE(10)
      EE(21) = EE(16)
      EE(22) = C2S2*(TEO(1) - 2.0*TEO(2) + TEO(3)) + TEO(10)*(C2-S2)**2
      EE(29) = TEO(15)*C2 + TEO(21)*S2
      EE(30) = SING*COSG*(TEO(15)-TEO(21))
      EE(35) = EE(30)
      EE(36) = TEO(15)*S2 + TEO(21)*C2
      IF (LSYS78) GO TO 530
C
C     PIEZOELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINATES
C
      EE(37) = C3*TEO(22) - S3*TEO(26) + CS2*(TEO(25)+2.0*TEO(32))
     7       - SC2*(TEO(23)+2.0*TEO(31))
      EE(38) = C3*TEO(23) + S3*TEO(25) + CS2*(TEO(26)-2.0*TEO(31))
     8       + SC2*(TEO(22)-2.0*TEO(32))
      EE(39) = S2*TEO(27) + C2*TEO(24) - 2.0*CS*TEO(33)
      EE(40) = C3*TEO(25) - S3*TEO(23) + CS2*(TEO(22)-2.0*TEO(32))
     O       - SC2*(TEO(26)-2.0*TEO(31))
      EE(41) = C3*TEO(26) + S3*TEO(22) + CS2*(TEO(23)+2.0*TEO(31))
     1       + SC2*( TEO(25)+2.0*TEO(32))
      EE(42) = S2*TEO(24) + C2*TEO(27) + 2.0*CS*TEO(33)
      EE(43) = COSG*TEO(28) - SING*TEO(29)
      EE(44) = COSG*TEO(29) + SING*TEO(28)
      EE(45) = TEO(30)
      EE(46) = C3*TEO(31) + S3*TEO(32) - CS2*(TEO(23)-TEO(26)+TEO(31))
     6       + SC2*(-TEO(32)-TEO(25)+TEO(22))
      EE(47) = C3*TEO(32) - S3*TEO(31) - CS2*(TEO(25)-TEO(22)+TEO(32))
     7       + SC2*(TEO(23)+TEO(31)-TEO(26))
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
  530 CONTINUE
C
      IECPT(1) = IECPT(1) - (IECPT(1)/1000)*1000 - 1
      AJHO  = IECPT(1)
      AJJHO = AJHO*AJHO
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
C
      ACURL( 1) = (EE(15) + AJJHO*EE(29))*DELINT(1)
      ACURL( 2) = (EE( 3) + EE(15) + AJJHO*EE(29))*DELINT(4)
      ACURL( 3) = (EE(15) + AJJHO*EE(29))*DELINT(2) + EE(16)*DELINT(4)
      ACURL( 4) = (EE(15) + EE(29))*AJHO*DELINT(1)
      ACURL( 5) = EE(15)*AJHO*DELINT(4)
      ACURL( 6) = (EE(15)+EE(29))*AJHO*DELINT(2) - EE(30)*AJHO*DELINT(4)
      ACURL( 7) = AJJHO*DELINT(1)*EE(35)
      ACURL( 8) = (EE(16) + AJJHO*EE(35))*DELINT(4)
      ACURL( 9) = EE(9)*DELINT(4) + AJJHO*DELINT(2)*EE(35)
      ACURL(11) = (EE(1) + 2.0*EE(3) + EE(15) + AJJHO*EE(29))*DELINT(6)
      ACURL(12) = (EE(3) + EE(15) + AJJHO*EE(29))*DELINT(5)
     1          + (EE(4) + EE (16))*DELINT(6)
      ACURL(13) = (EE(3) + EE(15) + EE(29))*AJHO*DELINT(4)
      ACURL(14) = (EE(3) + EE(15))*DELINT(6)*AJHO
      ACURL(15) = (EE(3) + EE(15) + EE(29))*AJHO*DELINT(5)
     1          - AJHO*EE(30)*DELINT(6)
      ACURL(16) = AJJHO*DELINT(4)*EE(35)
      ACURL(17) = (EE(4) + EE(16) + AJJHO*EE(35))*DELINT(6)
      ACURL(18) = (EE(2) + EE(9))*DELINT(6) + AJJHO*DELINT(5)*EE(35)
      ACURL(21) = (EE(15) + AJJHO*EE(29))*DELINT(3) + EE(22)*DELINT(6)
     1          + 2.0*EE(16)*DELINT(5)
      ACURL(22) = (EE(15) + EE(29))*AJHO*DELINT(2)
     1          + AJHO*DELINT(4)*EE(16)
      ACURL(23) = EE(15)*AJHO*DELINT(5) + AJHO*DELINT(6)*EE(16)
      ACURL(24) = (EE(15) + EE(29))*AJHO*DELINT(3)
     1          + (EE(16) - EE(30))*AJHO*DELINT(5)
      ACURL(25) = AJJHO*DELINT(2)*EE(35)
      ACURL(26) = EE(22)*DELINT(6) + (EE(21) + AJJHO*EE(35))*DELINT(5)
      ACURL(27) = EE(9)*DELINT(5) + EE(10)*DELINT(6)
     1          + AJJHO*DELINT(3)*EE(35)
      ACURL(31) = (EE(29) + AJJHO*EE(15))*DELINT(1)
      ACURL(32) = EE(15)*AJJHO*DELINT(4)
      ACURL(33) = (EE(29) + AJJHO*EE(15))*DELINT(2) - EE(30)*DELINT(4)
      ACURL(34) = AJHO*DELINT(1)*EE(35)
      ACURL(35) = AJHO*(EE(16) + EE(35))*DELINT(4)
      ACURL(36) = EE(9)*AJHO*DELINT(4) + AJHO*DELINT(2)*EE(35)
      ACURL(41) = AJJHO*DELINT(6)*EE(15)
      ACURL(42) = EE(15)*AJJHO*DELINT(5)
      ACURL(43) = 0.0
      ACURL(44) = AJHO*DELINT(6)*EE(16)
      ACURL(45) = EE(9)*AJHO*DELINT(6)
      ACURL(51) = (EE(29) + AJJHO*EE(15))*DELINT(3) + EE(36)*DELINT(6)
     1          - 2.0*DELINT(5)*EE(35)
      ACURL(52) = AJHO*(DELINT(2)*EE(30) - DELINT(4)*EE(36))
      ACURL(53) = -EE(36)*AJHO*DELINT(6) + AJHO*(EE(16)
     1          + EE(35))*DELINT(5)
      ACURL(54) = (EE(9) - EE(36))*AJHO*DELINT(5)
     1          + AJHO*DELINT(3)*EE(35)
      ACURL(61) = EE(36)*AJJHO*DELINT(1)
      ACURL(62) = EE(36)*AJJHO*DELINT(4)
      ACURL(63) = (EE(36))*AJJHO*DELINT(2)
      ACURL(71) = (EE(22) + AJJHO*EE(36))*DELINT(6)
      ACURL(72) = EE(36)*AJJHO*DELINT(5) + EE(20)*DELINT(6)
      ACURL(81) = EE(36)*AJJHO*DELINT(3) + EE(8)*DELINT(6)
C
      IF (LSYS78) GO TO 540
      ACURL(82) =-(EE(45)-AJHO*EE(51))*AJHO*DELINT(1)
      ACURL(83) = (EE(43)-AJHO*EE(45)-AJHO*EE(49)
     3          + AJJHO*EE(51))*DELINT(4)
      ACURL(84) = (EE(44)-AJHO*EE(50))*DELINT(4)-(EE(45)
     4          - AJHO*EE(51))*AJHO*DELINT(2)
      ACURL(85) =-(EE(39)+EE(45)-AJHO*EE(51))*AJHO*DELINT(4)
      ACURL(86) = (EE(37)+EE(43)-(EE(39)+EE(45)+EE(49)
     6          - AJHO*EE(51))*AJHO)*DELINT(6)
      ACURL(87) = (EE(38)+EE(44)-AJHO*EE(50))*DELINT(6)-(EE(39)+EE(45)
     7          - AJHO*EE(51))*AJHO*DELINT(5)
      ACURL(88) =-(EE(45)-AJHO*EE(51))*AJHO*DELINT(2)-EE(48)*AJHO*
     8            DELINT(4)
      ACURL(89) = (EE(43)-AJHO*EE(45)-AJHO*EE(49)+AJJHO*EE(51))*
     9            DELINT(5) +(EE(46)-EE(48)*AJHO)*DELINT(6)
      ACURL(90) = (EE(44)-AJHO*EE(48)-AJHO*EE(50))*DELINT(5)+EE(47)*
     O            DELINT(6) - (EE(45)-AJHO*EE(51))*AJHO*DELINT(3)
      ACURL(91) =-(EE(45)*AJHO-EE(51))*AJHO*DELINT(1)
      ACURL(92) = (AJHO*EE(43)-AJJHO*EE(45)-EE(49)+AJHO*EE(51))*
     2            DELINT(4)
      ACURL(93) = (EE(44)*AJHO-EE(50))*DELINT(4)-(EE(45)*AJHO-EE(51))*
     3            AJHO*DELINT(2)
      ACURL(94) =-EE(45)*AJJHO*DELINT(4)
      ACURL(95) = (EE(43)-AJHO*EE(45))*AJHO*DELINT(6)
      ACURL(96) = EE(44)*AJHO*DELINT(6)-EE(45)*AJJHO*DELINT(5)
      ACURL(97) =-(EE(45)*AJHO-EE(51))*AJHO*DELINT(2)-EE(54)*AJHO*
     7            DELINT(4)
      ACURL(98) = (EE(43)*AJHO-AJJHO*EE(45)-EE(49)+EE(51)*AJHO)*
     8            DELINT(5)+(EE(52)-AJHO*EE(54))*DELINT(6)
      ACURL(99) = (EE(44)*AJHO-EE(50)-EE(54)*AJHO)*DELINT(5)+EE(53)*
     9            DELINT(6)-(EE(45)*AJHO-EE(51))*AJHO*DELINT(3)
      ACURL(100) = EE(54)*AJJHO*DELINT(1)
      ACURL(101) =-(EE(52)-EE(54)*AJHO)*AJHO*DELINT(4)
      ACURL(102) =-(EE(53)*DELINT(4)-EE(54)*AJHO*DELINT(2))*AJHO
      ACURL(103) =-(EE(48)-EE(54)*AJHO)*AJHO*DELINT(4)
      ACURL(104) = (EE(46)-EE(48)*AJHO-EE(52)*AJHO+EE(54)*AJJHO)*
     4             DELINT(6)
      ACURL(105) = (EE(47)-EE(53)*AJHO)*DELINT(6)-(EE(48)-EE(54)*AJHO)*
     5             AJHO*DELINT(5)
      ACURL(106) = EE(54)*AJJHO*DELINT(2)-EE(42)*AJHO*DELINT(4)
      ACURL(107) = (EE(40)-EE(42)*AJHO)*DELINT(6)-(EE(52)-EE(54)*AJHO)*
     7             AJHO*DELINT(5)
      ACURL(108) = EE(41)*DELINT(6)+(-EE(42)-EE(53))*AJHO*DELINT(5)+
     8             EE(54)*AJJHO*DELINT(3)
      ACURL(109) = EE(63)*AJJHO*DELINT(1)
      ACURL(110) = (-EE(57)+EE(63)*AJHO)*AJHO*DELINT(4)
      ACURL(111) =-EE(60)*AJHO*DELINT(4)+EE(63)*AJJHO*DELINT(2)
      ACURL(112) = ACURL(110)
      ACURL(113) = (EE(55)-2.0*EE(57)*AJHO+EE(63)*AJJHO)*DELINT(6)
      ACURL(114) = (EE(56)-EE(60)*AJHO)*DELINT(6)+(-EE(57)+EE(63)*AJHO)*
     4             AJHO*DELINT(5)
      ACURL(115) = ACURL(111)
      ACURL(116) = ACURL(114)
      ACURL(117) = EE(59)*DELINT(6)-2.0*EE(60)*AJHO*DELINT(5)+EE(63)*
     7             AJJHO*DELINT(3)
  540 CONTINUE
C
C     EXPAND ACURL INTO (9X9)
C
      DO 610 IB = 2,9
      IC = 10*IB - 19
      I  = IC
      DO 605 J = IB,9
      IC = IC + 9
      I  = I + 1
  605 ACURL(IC) = ACURL(I)
  610 CONTINUE
      DGAMR = PI
      IF (AJHO .EQ. 0) DGAMR = 2.0*PI
      DO 630 I = 1,81
  630 ACURL(I) = DGAMR*ACURL(I)
C
      IF (LSYS78) GO TO 640
      DO 635 I = 82,117
  635 ACURL(I) = ACURL(I)*DGAMR
  640 CONTINUE
C
C     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
C     TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATS (GABABQ,9,9,1, ACURL,9,9,0,D )
      CALL GMMATS (     D,9,9,0,GABABQ,9,9,0,AK)
C
      IF (LSYS78) GO TO 650
      CALL GMMATS (GABABQ,9,9,1,ACURP1,9,3,0,D1   )
      CALL GMMATS (    D1,9,3,0,GABABP,3,3,0,AKUPH)
      CALL GMMATS (GABABP,3,3,1,ACURP2,3,3,0,D2   )
      CALL GMMATS (    D2,3,3,0,GABABP,3,3,0,AKPH2)
  650 CONTINUE
C
C
C     LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
C     **** COORDINATE SYS NOT POSSIBLE WITH RINGAX ********
C     **** THE FOLLOWING CODE COULD IMPLEMENT IT   ********
C     **   IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR
C          PIEZOELECTRIC
C.    DO 750 I = 1,81
C.750 AKI(I) = 0.00
C.    DO 800 I = 1,3
C.    CALL TRANSS (ICS(I),D(1))
C.    K = 30*(I-1) + 1
C.    DO 800 J = 1,3
C.    KK = K + 9*(J-1)
C.    JJ = 3*(J-1) + 1
C.    AKI(KK  ) = D(JJ  )
C.    AKI(KK+1) = D(JJ+1)
C.    AKI(KK+2) = D(JJ+2)
C.800 CONTINUE
C
C     TRANSFORM THE STIFFNESS MATRIX FROM BASIC TO LOCAL COORDINATES
C
C.    CALL GMMATS (AKI,9,9,1, AK,9,9,0,D )
C.    CALL GMMATS (  D,9,9,0,AKI,9,9,0,AK)
C
C     FORM WJ MATRIX
C
      DO 1000 I = 1,6
      DO 1000 J = 1,9
 1000 WJ (I,J) = 0.0
      RSUM = 0.0
      ZSUM = 0.0
      DO 1040 I = 1,3
      RSUM = RSUM + R(I)
 1040 ZSUM = ZSUM + Z(I)
      RSUM = RSUM/3.0
      ZSUM = ZSUM/3.0
      ZDR  = ZSUM/RSUM
      WJ (1,2) = 1.0
      WJ (2,9) = 1.0
      WJ (3,1) = 1.0/RSUM
      WJ (3,2) = 1.0
      WJ (3,3) = ZDR
      WJ (3,4) = AJHO/RSUM
      WJ (3,5) = AJHO
      WJ (3,6) = AJHO*ZDR
      WJ (4,3) = 1.0
      WJ (4,8) = 1.0
      WJ (5,1) =-AJHO/RSUM
      WJ (5,2) =-AJHO
      WJ (5,3) =-AJHO*ZDR
      WJ (5,4) =-1.0/RSUM
      WJ (5,6) =-ZDR
      WJ (6,6) = 1.0
      WJ (6,7) =-AJHO/RSUM
      WJ (6,8) =-AJHO
      WJ (6,9) =-AJHO*ZDR
C
      IF (LSYS78) GO TO 1060
C
C     FORM WJP MATRIX
C
      DO 1050 I = 1,3
      DO 1050 J = 1,3
 1050 WJP(I,J) = 0.0
C
      WJP(1,2) = 1.0
      WJP(2,3) = 1.0
      WJP(3,1) = -AJHO/RSUM
      WJP(3,2) = -AJHO
      WJP(3,3) = -AJHO*ZDR
 1060 CONTINUE
C
C     COMPUTE THE STRESS MATRIX
C
      CALL GMMATS (   WJ,9,6,1,GABABQ,9,9,0,D(1))
      CALL GMMATS (EE(1),6,6,0,  D(1),6,9,0,SEL )
      IF (LSYS78) GO TO 1070
      CALL GMMATS (   WJP,3,3,1,GABABP,3,3,0,D2(1))
      CALL GMMATS (EE(37),6,3,0, D2(1),3,3,0,SELP1)
      CALL GMMATS (EE(37),6,3,1,  D(1),6,9,0,SELP2)
      CALL GMMATS (EE(55),3,3,0, D2(1),3,3,0,SELP3)
 1070 CONTINUE
C
C    *** MORE CORD SYS REMOVAL.  FEL ABOVE IS WJ **********
C    **  IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR
C        PIEZOELECTRIC
C
C.    CALL GMMATS (WJ,6,9,0, AK,9,9,0, SEL)
C
C
C     TRANSFORM THE STRESS MATRIX FROM BASIC TO LOCAL COORDINATES
C
C     COMPUTE THE THE THERMAL STRESS
C
      TS(1) = EE( 1)*ALF(1) + EE( 2)*ALF(3) + EE( 3)*ALF(2)
      TS(2) = EE( 7)*ALF(1) + EE( 8)*ALF(3) + EE( 9)*ALF(2)
      TS(3) = EE(13)*ALF(1) + EE(14)*ALF(3) + EE(15)*ALF(2)
      TS(4) = EE(19)*ALF(1) + EE(20)*ALF(3) + EE(21)*ALF(2)
      TS(5) = 0.0
      TS(6) = 0.0
      DO 1100 IKI = 1,14
      PHI(IKI) = ECPT(7+IKI)
 1100 CONTINUE
      GO TO 940
C
C     SET FATAL ERROR FLAG AND ALLOWING ERROR MESSAGES TO ACCUMULATE
C
  910 I = 37
      GO TO 930
  920 I = 126
  930 CALL MESAGE (30,I,IDEL)
      NOGO = 1
  940 IF (.NOT.PZMAT) KSYS78 = KSAVE
      RETURN
      END
