      SUBROUTINE TRIAAX
C
C     THIS SUBROUTINE COMPUTES THE STIFFNESS AND MASS MATRICES FOR THE
C     ASSYMMETRIC RING WITH A TRIANGULAR CROSS SECTION, TO BE USED BY
C     THE ELEMENT MATRIX GENERATOR.
C
C     SINGLE PRECISION VERSION
C
C     THIS CURRENT VERSION ALLOWS FOR COORDINATE SYSTEM
C     THIS SUBROUTINE USES THE ADDITIONAL ROUTINES DKL, DELTKL
C
C     THE ECPT FOR THE TRIAX ELEMENT IS
C
C     ECPT (01) = ELEMENT ID                              I
C     ECPT (02) = SIL A                                   I
C     ECPT (03) = SIL B                                   I
C     ECPT (04) = SIL C                                   I
C     ECPT (05) = MATERIAL ORIENTATION ANGLE(DEGREES)     R
C     ECPT (07) = MATERIAL ID                             I
C     ECPT (08) TO ECPT(21) = STRESS PHASE ANG.           R
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
C
      LOGICAL         NOGO,HEAT,PZMAT,LSYS78
      INTEGER         DICT(11),ELID,ESTID,ISORT(3)
      REAL            GABABQ(9,9),R(3),Z(3),EE(63),TEO(45),DELINT(12),
     1                ECPT(10),DELM(12),BMASS(9,9),AKM(81),AKJM(81),
     2                AMT(9),GABABP(3,3)
      DOUBLE PRECISION CONSTS
      DIMENSION       IECPT(34),AKI(81),AKT(16),ACURL(117),D(81),AK(81),
     1                AKJ(144),ICS(3),D1(27),D2(9),ACURP1(27),ACURP2(9),
     2                AKUPH(27),AKPH2(9),AKIP(9)
      COMMON /SYSTEM/ KSYSTM(77),KSYS78
      COMMON /EMGPRM/ IXTRA,DUM(14),ISMB(3),IPREC,NOGO,HEAT,ICMBAR
      COMMON /EMGDIC/ DXX,LDICT,NGRIDS,ELID,ESTID
      COMMON /EMGEST/ IDEL,IGP(3),DGAMA,DM1,MATID,SPA(14),ICS1,R1,ZZ1,
     1                ZER,ICS2,R2,ZZ2,ZER2,ICS3,R3,ZZ3,ZER3,TEMPE
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO,GSUBE,MOSKP(9),
     1                SETMAT
      COMMON /CONDAD/ CONSTS(5)
      COMMON /MATPZ / PZOUT(51)
C
C     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
C                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
C                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
C                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
C                     EPS22,EPS23,EPS33,RHO,A1,A2,A12,TREF,GE
C
      EQUIVALENCE     (IECPT(1),ECPT(1),IDEL), (DICT(5),DICT5),
     1                (Z(1),Z1), (Z(2),Z2), (Z(3),Z3),
     2                (AKI(1),GABABQ(1,1)), (BMASS(1,1),AKM(1)),
     3                (CONSTS(1),PI), (CONSTS(4),DEGRAD),
     4                (CONSTS(2),TWOPI), (AKIP(1),GABABP(1,1)),
     5                (ACURP1(1),ACURL(82)), (ACURP2(1),ACURL(109))
      DATA            IDEL2,JAX / 0, 4HTRIA/
C
      LSYS78 = .FALSE.
      IF (KSYS78.EQ.0 .OR. KSYS78.EQ.2) LSYS78 = .TRUE.
      IDEL1 = IDEL/1000
C
C     INITALIZE
C
      DO 50 I = 1,3
      R(I) = ECPT(4*I+19)
      Z(I) = ECPT(4*I+20)
   50 ICS(I) = IECPT(4*I+18)
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 12
      DICT(4) = 15
      IPR     = IPREC
C
      IF (R1.LE.0. .OR. R2.LE.0. .OR. R3.LE.0.) GO TO 7770
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z1,Z2,Z3)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
C
C     FORM TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES TO
C     GRID POINT DEGREES OF FREEDOM
C
      DO 100 I = 1,9
      DO 100 J = 1,9
  100 GABABQ(I,J) = 0.
C
      AA = 1./(R2*Z3 + R1*Z2 + Z1*R3 - Z2*R3 - R1*Z3 - R2*Z1)
      C1 =  AA*(R2*Z3 - Z2*R3)
      C2 = -AA*(Z3 - Z2)
      C3 =  AA*(R3 - R2)
      GABABQ(1,1) = C1
      GABABQ(1,2) = C2
      GABABQ(1,3) = C3
      GABABQ(2,4) = C1
      GABABQ(2,5) = C2
      GABABQ(2,6) = C3
      GABABQ(3,7) = C1
      GABABQ(3,8) = C2
      GABABQ(3,9) = C3
      IF (LSYS78) GO TO 102
      GABABP(1,1) = C1
      GABABP(1,2) = C2
      GABABP(1,3) = C3
  102 CONTINUE
      C1 = -AA*(R1*Z3 - Z1*R3)
      C2 =  AA*(Z3 - Z1)
      C3 = -AA*(R3 - R1)
      GABABQ(4,1) = C1
      GABABQ(4,2) = C2
      GABABQ(4,3) = C3
      GABABQ(5,4) = C1
      GABABQ(5,5) = C2
      GABABQ(5,6) = C3
      GABABQ(6,7) = C1
      GABABQ(6,8) = C2
      GABABQ(6,9) = C3
      IF (LSYS78) GO TO 104
      GABABP(2,1) = C1
      GABABP(2,2) = C2
      GABABP(2,3) = C3
  104 CONTINUE
      C1 =  AA*(R1*Z2 - Z1*R2)
      C2 = -AA*(Z2 - Z1)
      C3 =  AA*(R2 - R1)
      GABABQ(7,1) = C1
      GABABQ(7,2) = C2
      GABABQ(7,3) = C3
      GABABQ(8,4) = C1
      GABABQ(8,5) = C2
      GABABQ(8,6) = C3
      GABABQ(9,7) = C1
      GABABQ(9,8) = C2
      GABABQ(9,9) = C3
      IF (LSYS78) GO TO 110
      GABABP(3,1) = C1
      GABABP(3,2) = C2
      GABABP(3,3) = C3
  110 CONTINUE
C
C     COMPUTE THE INTEGRAL VALUES IN ARRAY DELINT THE ORDER IS INDICATED
C     THE FOLLOWING TABLE
C
C     DELINT(01) = (-1,0)
C     DELINT(02) = (-1,1)
C     DELINT(03) = (-1,2)
C     DELINT(04) = (0, 0)
C     DELINT(05) = (0, 1)
C     DELINT(06) = (1, 0)
C
C     OR FOR THE MASS MATRIX
C
C     DELINT(1)  =  (1,0)
C     DELINT(2)  =  (1,1)
C     DELINT(3)  =  (1,2)
C     DELINT(4)  =  (2,0)
C     DELINT(5)  =  (2,1)
C     DELINT(7)  =  (3,0)
C
C
      IF (ISMB(1) .EQ. 0) GO TO 180
      RA = (R1 + R2 + R3)/3.0
      ZA = (Z1 + Z2 + Z3)/3.0
      RH = AMIN1(R1,R2,R3)/10.0
      DR = AMAX1(ABS(R1-R2),ABS(R2-R3),ABS(R3-R1))
      AREA = (R1*(Z2-Z3) + R2*(Z3-Z1) + R3*(Z1-Z2))/2.0
C
      I1 = 0
      DO 160 I = 1,2
      IP = I - 2
      DO 140 J = 1,3
      IQ =  J  - 1
      I1 =  I1 + 1
      IF (I1 .NE. 6) GO TO 120
      IP = 1
      IQ = 0
  120 IF (DR .GT. RH) GO TO 130
      DELINT(I1) = ((RA**IP)*(ZA**IQ))*AREA
      GO TO 135
  130 DELINT(I1) = DKLS(3,IP,IQ,R,Z)
  135 DELINT(I1) = ABS (DELINT(I1))
  140 CONTINUE
  160 CONTINUE
C
C     MASS MATRIX
C
      IF (ISMB(2) .EQ. 0) GO TO 200
  180 CALL DELKLS (AKJ,R,Z,0)
      DELM (1) = AKJ(2)
      DELM (2) = AKJ(7)
      DELM (3) = AKJ(8)
      DELM (4) = AKJ(10)
      DELM (5) = AKJ(9)
      DELM (7) = AKJ(12)
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
C
  200 DGAMR  = DGAMA*DEGRAD
      COSG   = COS(DGAMR)
      SING   = SIN(DGAMR)
      SINTH  = SING
      COSTH  = COSG
      MATIDC = MATID
      MATFLG = 7
      IF (KSYS78 .GT. 0) MATFLG = 9
      ELTEMP = TEMPE
      CALL MAT (IDEL)
      PZMAT = .FALSE.
      IF (SETMAT.EQ.4. .OR. SETMAT.EQ.5.) PZMAT = .TRUE.
      IF (PZMAT) GO TO 210
      KSAVE  = KSYS78
      KSYS78 = 0
      LSYS78 = .TRUE.
      GO TO 220
  210 RHO    = PZOUT(46)
      ALF(1) = PZOUT(47)
      ALF(2) = PZOUT(48)
      ALF(3) = PZOUT(49)
      TZERO  = PZOUT(50)
      GSUBE  = PZOUT(51)
  220 CONTINUE
      IF (SETMAT .EQ. 2.) GO TO 7780
      DICT5 = G SUB E
      IF (KSYS78 .GT. 0) GO TO 249
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
      DEL = 1./(1. - VRO*VOR  - VOZ*VZO  - VZR*VRZ
     1    - VRO*VOZ*VZR - VRZ*VOR*VZO)
C
C     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
C
  249 CONTINUE
      DO 250 I = 1,45
  250 TEO(I) = 0.
C
      IF (KSYS78 .GT. 0) GO TO 251
      TEO(1) = ER*(1.  - VOZ*VZO)*DEL
      TEO(2) = ER*(VZR + VZO*VOR)*DEL
      TEO(3) = EZ*(1.  - VRO*VOR)*DEL
      TEO(4) = ER*(VOR + VZR*VOZ)*DEL
      TEO(5) = ET*(VZO + VRO*VZR)*DEL
      TEO(6) = ET*(1.  - VRZ*VZR)*DEL
      TEO(10)= GRZ
      TEO(15)= GOR
      TEO(21)= GZO
      GO TO 252
  251 CONTINUE
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
      IF (KSYS78 .EQ. 2) GO TO 252
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
  252 CONTINUE
      C2 = COSG*COSG
      C4 = C2  *C2
      S2 = SING*SING
      S4 = S2  *S2
      C2S2 = C2*S2
      C3 = COSG*C2
      S3 = SING*S2
      CS2= COSG*S2
      SC2= SING*C2
      CS = COSG*SING
C
      EE( 1) = TEO(1)*C4 + TEO(3)*S4 + 2.*C2S2 *(TEO(2) + 2.*TEO(10))
      EE( 2) = TEO(2)*(C4+S4) + C2S2*(TEO(1)+TEO(3) - 4.*TEO(10))
      EE( 3) = TEO(4)*C2 + TEO(5)*S2
      EE( 4) = COSG*SING*S2*(TEO(2) - TEO(3) + 2.*TEO(10))
     4       + SING*COSG*C2*(TEO(1) - TEO(2) - 2.*TEO(10))
      EE( 7) = EE(2)
      EE( 8) = TEO(1)*S4 + 2.*C2S2*(TEO(2) + 2.*TEO(10))+ TEO(3)*C4
      EE( 9) = TEO(4)*S2 + TEO(5)*C2
      EE(10) = SING*COSG*C2*(TEO(2) - TEO(3) + 2.*TEO(10))
     O       + COSG*SING*S2*(TEO(1) - TEO(2) - 2.*TEO(10))
      EE(13) = EE(3)
      EE(14) = EE(9)
      EE(15) = TEO(6)
      EE(16) = SING*COSG*(TEO(4)-TEO(5))
      EE(19) = EE(4)
      EE(20) = EE(10)
      EE(21) = EE(16)
      EE(22) = C2S2*(TEO(1) - 2.*TEO(2) + TEO(3)) + TEO(10)*(C2-S2)**2
      EE(29) = TEO(15)*C2 + TEO(21)*S2
      EE(30) = SING*COSG*(TEO(15)-TEO(21))
      EE(35) = EE(30)
      EE(36) = TEO(15)*S2 + TEO(21)*C2
C
      IF (LSYS78) GO TO 254
C
C     PIEZOELECTRIC MATERIAL PROPERTIES IN ELEMENT COORDINATES
C
      EE(37) = C3*TEO(22) - S3*TEO(26) + CS2*(TEO(25)+2.0*TEO(32)) -
     7         SC2*(TEO(23) + 2.0*TEO(31))
      EE(38) = C3*TEO(23) + S3*TEO(25) + CS2*(TEO(26)-2.0*TEO(31)) +
     8         SC2*(TEO(22) - 2.0*TEO(32))
      EE(39) = S2*TEO(27) + C2*TEO(24) - 2.0*CS*TEO(33)
      EE(40) = C3*TEO(25) - S3*TEO(23) + CS2*(TEO(22)-2.0*TEO(32)) -
     O         SC2*(TEO(26) - 2.0*TEO(31))
      EE(41) = C3*TEO(26) + S3*TEO(22) + CS2*(TEO(23)+2.0*TEO(31)) +
     1         SC2*(TEO(25) + 2.0*TEO(32))
      EE(42) = S2*TEO(24) + C2*TEO(27) + 2.0*CS*TEO(33)
      EE(43) = COSG*TEO(28) - SING*TEO(29)
      EE(44) = COSG*TEO(29) + SING*TEO(28)
      EE(45) = TEO(30)
      EE(46) = C3*TEO(31) + S3*TEO(32) - CS2*(TEO(23)-TEO(26)+TEO(31)) +
     6         SC2*(-TEO(32) - TEO(25) + TEO(22))
      EE(47) = C3*TEO(32) - S3*TEO(31) - CS2*(TEO(25)-TEO(22)+TEO(32)) +
     7         SC2*(TEO(23) + TEO(31) - TEO(26))
      EE(48) = (C2-S2)*TEO(33) + CS*(TEO(24) - TEO(27))
      EE(49) = C2*TEO(34) + S2*TEO(38) - CS*(TEO(35) + TEO(37))
      EE(50) = C2*TEO(35) - S2*TEO(37) + CS*(TEO(34) - TEO(38))
      EE(51) = COSG*TEO(36) - SING*TEO(39)
      EE(52) = C2*TEO(37) - S2*TEO(35) - CS*(TEO(38) - TEO(34))
      EE(53) = C2*TEO(38) + S2*TEO(34) + CS*(TEO(35) + TEO(37))
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
254   CONTINUE
C
C     COMPUTE HARMONIC COEFFICIENT
C
      MJHO = MOD(IECPT(1),1000) - 1
      AJHO = MJHO
      AJJHO= AJHO*AJHO
      RHOD = RHO *PI
      IF (AJHO   .EQ. 0.) RHOD = 2.*RHOD
      IF (ISMB(1) .EQ. 0) GO TO 300
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
C
      ACURL( 1) = (EE(15) + AJJHO*EE(29))*DELINT(1)
      ACURL( 2) = (EE(03) + EE(15) + AJJHO*EE(29))*DELINT(4)
      ACURL( 3) = (EE(15) + AJJHO*EE(29))*DELINT(2) + EE(16)*DELINT(4)
      ACURL( 4) = (EE(15) + EE(29))*AJHO*DELINT(1)
      ACURL(05) = EE(15)*AJHO*DELINT(4)
      ACURL(06) = (EE(15) + EE(29))*AJHO*DELINT(2)
     6          - EE(30)*AJHO*DELINT(4)
      ACURL(07) = AJJHO*DELINT(1)*EE(35)
      ACURL(08) = (EE(16) + AJJHO*EE(35))*DELINT(4)
      ACURL(09) = EE(9)*DELINT(4) + AJJHO*DELINT(2)*EE(35)
      ACURL(11) = (EE(1) + 2.*EE(3) + EE(15) + AJJHO*EE(29))*DELINT(6)
      ACURL(12) = (EE(3) + EE(15) + AJJHO*EE(29))*DELINT(5)
     2          + (EE(4) + EE (16))*DELINT(6)
      ACURL(13) = (EE(3) + EE(15) + EE(29))*AJHO*DELINT(4)
      ACURL(14) = (EE(3) + EE(15))*DELINT(6)*AJHO
      ACURL(15) = (EE(3) + EE(15) + EE(29))*AJHO*DELINT(5)
     5          - AJHO*EE(30)*DELINT(6)
      ACURL(16) = AJJHO*DELINT(4)*EE(35)
      ACURL(17) = (EE(4) + EE(16) + AJJHO*EE(35))*DELINT(6)
      ACURL(18) = (EE(2) + EE(9))*DELINT(6) + AJJHO*DELINT(5)*EE(35)
      ACURL(21) = (EE(15) + AJJHO*EE(29))*DELINT(3) + EE(22)
     1          * DELINT(6) +  2.*EE(16)*DELINT(5)
      ACURL(22) = (EE(15) + EE(29))*AJHO*DELINT (2) + AJHO
     2          * DELINT(4)*EE(16)
      ACURL(23) = EE(15)*AJHO*DELINT(5) + AJHO*DELINT(6)*EE(16)
      ACURL(24) = (EE(15) + EE(29))*AJHO*DELINT(3) + (EE(16) - EE(30))
     4          * AJHO*DELINT(5)
      ACURL(25) = AJJHO*DELINT(2)*EE(35)
      ACURL(26) = EE(22)*DELINT(6) + (EE(21) + AJJHO*EE(35))*DELINT(5)
      ACURL(27) = EE(9)*DELINT(5) + EE(10)*DELINT(6) + AJJHO
     1          * DELINT(3)*EE(35)
      ACURL(31) = (EE(29) + AJJHO*EE(15))*DELINT(1)
      ACURL(32) = EE(15)*AJJHO*DELINT(4)
      ACURL(33) = (EE(29) + AJJHO*EE(15))*DELINT(2) - EE(30)*DELINT(4)
      ACURL(34) = AJHO*DELINT(1)*EE(35)
      ACURL(35) = AJHO*(EE(16) + EE(35))*DELINT(4)
      ACURL(36) = EE(9)*AJHO*DELINT(4) + AJHO*DELINT(2)*EE(35)
      ACURL(41) = AJJHO*DELINT(6)*EE(15)
      ACURL(42) = EE(15)*AJJHO*DELINT(5)
      ACURL(43) = 0.
      ACURL(44) = AJHO*DELINT(6)*EE(16)
      ACURL(45) = EE(9)*AJHO*DELINT(6)
      ACURL(51) = (EE(29) + AJJHO*EE(15))*DELINT(3) + EE(36)
     1          * DELINT(6) - 2.*EE(35)*DELINT(5)
      ACURL(52) = AJHO*(DELINT(2)*EE(30) - DELINT(4)*EE(36))
      ACURL(53) = -EE(36)*AJHO*DELINT(6) + AJHO*(EE(16) + EE(35))
     3          * DELINT(5)
      ACURL(54) = (EE(9) - EE(36))*AJHO*DELINT(5) + AJHO
     1          * DELINT(3)*EE(35)
      ACURL(61) = EE(36)*AJJHO*DELINT(1)
      ACURL(62) = EE(36)*AJJHO*DELINT(4)
      ACURL(63) = (EE(36))*AJJHO*DELINT(2)
      ACURL(71) = (EE(22) + AJJHO*EE(36))*DELINT(6)
      ACURL(72) = EE(36)*AJJHO*DELINT(5) + EE(20)*DELINT(6)
      ACURL(81) = EE(36)*AJJHO*DELINT(3) + EE(8)*DELINT(6)
      IF (LSYS78) GO TO 256
      ACURL(82) =-(EE(45) - AJHO*EE(51))*AJHO*DELINT(1)
      ACURL(83) = (EE(43) - AJHO*EE(45) - AJHO*EE(49) + AJJHO*EE(51))
     3          * DELINT(4)
      ACURL(84) = (EE(44) - AJHO*EE(50))*DELINT(4) - (EE(45)
     4          - AJHO*EE(51))*AJHO*DELINT(2)
      ACURL(85) =-(EE(39) + EE(45) - AJHO*EE(51))*AJHO*DELINT(4)
      ACURL(86) = (EE(37) + EE(43) - (EE(39) + EE(45) + EE(49)
     6          - AJHO*EE(51))*AJHO)*DELINT(6)
      ACURL(87) = (EE(38) + EE(44) - AJHO*EE(50))*DELINT(6) - (EE(39)
     7          + EE(45) - AJHO*EE(51))*AJHO*DELINT(5)
      ACURL(88) =-(EE(45) - AJHO*EE(51))*AJHO*DELINT(2) - EE(48)*AJHO
     8          * DELINT(4)
      ACURL(89) = (EE(43) - AJHO*EE(45) - AJHO*EE(49) + AJJHO*EE(51))
     9          * DELINT(5) + (EE(46) - EE(48)*AJHO)*DELINT(6)
      ACURL(90) = (EE(44) - AJHO*EE(48) - AJHO*EE(50))*DELINT(5)
     O          + EE(47)*DELINT(6) - (EE(45)-AJHO*EE(51))*AJHO*DELINT(3)
      ACURL(91) =-(EE(45)*AJHO - EE(51))*AJHO*DELINT(1)
      ACURL(92) = (AJHO*EE(43) - AJJHO*EE(45) - EE(49) + AJHO*EE(51))
     2          * DELINT(4)
      ACURL(93) = (EE(44)*AJHO - EE(50))*DELINT(4) - (EE(45)*AJHO
     3          - EE(51))*AJHO*DELINT(2)
      ACURL(94) =-EE(45)*AJJHO*DELINT(4)
      ACURL(95) = (EE(43) - AJHO*EE(45))*AJHO*DELINT(6)
      ACURL(96) = EE(44)*AJHO*DELINT(6) - EE(45)*AJJHO*DELINT(5)
      ACURL(97) =-(EE(45)*AJHO - EE(51))*AJHO*DELINT(2) - EE(54)*AJHO
     7          * DELINT(4)
      ACURL(98) = (EE(43)*AJHO - AJJHO*EE(45) - EE(49) + EE(51)*AJHO)
     8          * DELINT(5) + (EE(52) - AJHO*EE(54))*DELINT(6)
      ACURL(99) = (EE(44)*AJHO - EE(50) - EE(54)*AJHO)*DELINT(5)
     9          + EE(53)*DELINT(6) - (EE(45)*AJHO-EE(51))*AJHO*DELINT(3)
      ACURL(100)= EE(54)*AJJHO*DELINT(1)
      ACURL(101)=-(EE(52) - EE(54)*AJHO)*AJHO*DELINT(4)
      ACURL(102)=-(EE(53)*DELINT(4) - EE(54)*AJHO*DELINT(2))*AJHO
      ACURL(103)=-(EE(48) - EE(54)*AJHO)*AJHO*DELINT(4)
      ACURL(104)= (EE(46) - EE(48)*AJHO - EE(52)*AJHO+EE(54)*AJJHO)
     4          *  DELINT(6)
      ACURL(105)= (EE(47) - EE(53)*AJHO)*DELINT(6) - (EE(48) - EE(54)
     5          * AJHO)*AJHO*DELINT(5)
      ACURL(106)= EE(54)*AJJHO*DELINT(2) - EE(42)*AJHO*DELINT(4)
      ACURL(107)= (EE(40) - EE(42)*AJHO)*DELINT(6) - (EE(52) - EE(54)
     7          * AJHO)*AJHO*DELINT(5)
      ACURL(108)= EE(41)*DELINT(6) + (-EE(42) - EE(53))*AJHO*DELINT(5)
     8          + EE(54)*AJJHO*DELINT(3)
      ACURL(109)= EE(63)*AJJHO*DELINT(1)
      ACURL(110)= (-EE(57) + EE(63)*AJHO)*AJHO*DELINT(4)
      ACURL(111)=-EE(60)*AJHO*DELINT(4) + EE(63)*AJJHO*DELINT(2)
      ACURL(112)= ACURL(110)
      ACURL(113)= (EE(55) - 2.0*EE(57)*AJHO + EE(63)*AJJHO)*DELINT(6)
      ACURL(114)= (EE(56) - EE(60)*AJHO)*DELINT(6) + (-EE(57) + EE(63)
     4          * AJHO)*AJHO*DELINT(5)
      ACURL(115)= ACURL(111)
      ACURL(116)= ACURL(114)
      ACURL(117)= EE(59)*DELINT(6) - 2.0*EE(60)*AJHO*DELINT(5) + EE(63)
     7          * AJJHO*DELINT(3)
  256 CONTINUE
C
C     EXPAND ACURL INTO (9X9)
C
      DO 270 IB = 2,9
      IC = 10*IB - 19
      I  = IC
      DO 260 J = IB,9
      IC = IC + 9
      I  = I  + 1
  260 ACURL(IC) = ACURL(I)
  270 CONTINUE
      DGAMA = PI
      IF (AJHO .EQ. 0.) DGAMA = TWOPI
      DO 280 I = 1,81
  280 ACURL(I) = ACURL(I)*DGAMA
      IF (LSYS78) GO TO 300
C
      DO 290 I = 82,117
  290 ACURL(I) = ACURL(I)*DGAMA
C
  300 IF (ISMB(2).EQ. 0) GO TO 400
      IF (ICMBAR .LT. 0) GO TO 350
C
C     CONSISTENT MASS IN FIELD COORDINATES
C
      DO 320 I = 1,9
      DO 320 J = 1,9
  320 BMASS(I,J) = 0.
      BMASS(1,1) = RHOD*DELM(1)
      BMASS(1,2) = RHOD*DELM(4)
      BMASS(1,3) = RHOD*DELM(2)
      BMASS(2,1) = RHOD*DELM(4)
      BMASS(2,2) = RHOD*DELM(7)
      BMASS(2,3) = RHOD*DELM(5)
      BMASS(3,1) = RHOD*DELM(2)
      BMASS(3,2) = RHOD*DELM(5)
      BMASS(3,3) = RHOD*DELM(3)
      BMASS(4,4) = RHOD*DELM(1)
      BMASS(4,5) = RHOD*DELM(4)
      BMASS(4,6) = RHOD*DELM(2)
      BMASS(5,4) = RHOD*DELM(4)
      BMASS(5,5) = RHOD*DELM(7)
      BMASS(5,6) = RHOD*DELM(5)
      BMASS(6,4) = RHOD*DELM(2)
      BMASS(6,5) = RHOD*DELM(5)
      BMASS(6,6) = RHOD*DELM(3)
      BMASS(7,7) = RHOD*DELM(1)
      BMASS(7,8) = RHOD*DELM(4)
      BMASS(7,9) = RHOD*DELM(2)
      BMASS(8,7) = RHOD*DELM(4)
      BMASS(8,8) = RHOD*DELM(7)
      BMASS(8,9) = RHOD*DELM(5)
      BMASS(9,7) = RHOD*DELM(2)
      BMASS(9,8) = RHOD*DELM(5)
      BMASS(9,9) = RHOD*DELM(3)
      GO TO 400
  350 AREA  = (R1*(Z2-Z3) + R2*(Z3-Z1) + R3*(Z1-Z2))/2.
      CONVM =  RHOD*(R1 + R2 + R3)/3.*AREA
C
C     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD SYSTEM
C     TO GRID POINT DEGREES OF FREEDOM
C
  400 IF (ISMB(1) .EQ. 0) GO TO 410
      CALL GMMATS (AKI,9,9,1,ACURL,9,9,0,D)
      CALL GMMATS (D,  9,9,0,AKI,  9,9,0,AK)
      IF (LSYS78) GO TO 405
      CALL GMMATS (AKI,9,9,1,ACURP1,9,3,0,D1)
      CALL GMMATS (D1, 9,3,0,AKIP,  3,3,0,AKUPH)
      CALL GMMATS (AKIP,3,3,1,ACURP2,3,3,0,D2)
      CALL GMMATS (D2,  3,3,0,AKIP,  3,3,0,AKPH2)
  405 CONTINUE
C
      IF (ISMB(2).EQ.0 .OR. ICMBAR.LT.0) GO TO 450
  410 CALL GMMATS (AKI,9,9,1,BMASS,9,9,0,D)
      CALL GMMATS (D,  9,9,0,AKI,  9,9,0,AKM)
C
  450 DO 460 I = 1,81
      AKJ(I)  = 0.
  460 AKJM(I) = 0.
      DO 462 I = 82,117
  462 AKJ(I) = 0.0
C
      GO TO 480
C
C     COORDINATE SYSTEMS POSSIBLE WITH RINGAX THRU CODE BELOW
C **  IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZOELECTRI
C
C     DO 470 I = 1,3
C     IF (ICS(I) .EQ. 0) GO TO 470
C     K = 9*(I-1) + 1
C     CALL TRANSS (ICS(I),D(K))
C 470 CONTINUE
C
C     CREATE AN ARRAY OF SORTED GRID POINTS
C
  480 DO 482 I = 1,3
      ISORT(I) = IGP(I)
  482 CONTINUE
      I = -3
  484 J = 0
      DO 486 K = 1,3
      IF (ISORT(K) .LT. J) GO TO 486
      J = ISORT(K)
      L = K
  486 CONTINUE
      ISORT(L) = I
      I = I + 1
      IF (I .LT. 0) GO TO 484
      DO 490 I = 1,3
      ISORT(I) = -ISORT(I)
  490 CONTINUE
C
C     TRANSFORM 3 X 3 TO 6 X 6 FOR COORD SYSTEM TRANSFORMATIONS
C
      DO 600 ISIL = 1,3
      IPP = ISORT(ISIL)
C
      IR1 = 3*(ISIL-1) + 1
      DO 590 II = 1,3
      I   = ISORT(II)
      IC1 = 3*(II-1) + 1
      IRC = (IR1 -1)*9 + IC1
      DO 500 J = 1,3
      J1   = (J-1)*4 + 1
      IRCC = IRC + (J-1)*9
      IF (ISMB(1) .EQ. 0) GO TO 495
      AKT(J1  ) = AK(IRCC  )
      AKT(J1+1) = AK(IRCC+1)
      AKT(J1+2) = AK(IRCC+2)
      IF (LSYS78) GO TO 492
      M = IRCC/3 + 1
      N = (M-1)/9 + 1 + (II-1)*9 + (J-1)*3
      AKT(J1+3) = AKUPH(M)
      AKT(J1+15-J*3) = AKUPH(N)
      AKT(16) = AKPH2(IR1+II-1)
  492 CONTINUE
C
  495 IF (ISMB(2).EQ.0 .OR. ICMBAR.LT.1) GO TO 500
      J1 = (J-1)*3 + 1
      AMT(J1  ) =AKM(IRCC  )
      AMT(J1+1) =AKM(IRCC+1)
      AMT(J1+2) =AKM(IRCC+2)
  500 CONTINUE
C
      GO TO 540
C
C    ABOVE GO TO MAKES CST CODE BELOW INTO DEAD CODE
C    COORDINATE SYSTEM TRANSFORMATION CODE
C ** IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR PIEZOELECTRIC
C
C     IF (ICS(IPP) .EQ. 0) GO TO 520
C     IAA = 9*(IPP-1) + 1
C     CALL GMMATS (D(IAA),3,3,1,AKT(1),3,3,0,D(28))
C     CALL GMMATS (D(IAA),3,3,1,AMT(1),3,3,0,D(37))
C     DO 510 J = 1,9
C     AKT(J) = D(J+27)
C 510 AKM(J) = D(J+36)
C
C 520 IF (ICS(I) .EQ. 0) GO TO 540
C     IAI = 9*(I-1) + 1
C     CALL GMMATS (AKT(1),3,3,0,D(IAI),3,3,0,D(28))
C     CALL GMMATS (AMT(1),3,3,0,D(IAI),3,3,0,D(37))
C     DO 530 J = 1,9
C     AKT(J) = D(J+27)
C 530 AMT(J) = D(J+36)
C
C     NOW INSERT  AKT AND AMT INTO THE OVERALL STIFFNESS MATRICES
C     ACCORDING TO INCREASING SIL VALUE
C
  540 DO 550 IJ = 1,3
      DO 550 JJ = 1,3
      KI = (IJ-1)*3 + JJ
      IOUT = (IPP-1)*27 + (I-1)*3 + (IJ-1)*9 + JJ
  550 AKJM(IOUT)= AMT(KI)
      DO 560 IJ = 1,4
      DO 560 JJ = 1,4
      KI = (IJ-1)*4 + JJ
      IOUT = (IPP-1)*48 + (I-1)*4 + (IJ-1)*12 + JJ
  560 AKJ(IOUT) = AKT(KI)
  590 CONTINUE
  600 CONTINUE
C
C     NOW OUTPUT THE MATRIX VIA EMG OUT
C
      DICT(2) = 1
      IF (ISMB(1) .EQ. 0) GO TO 650
      CALL EMGOUT (AKJ,AKJ,144,1,DICT,1,IPR)
  650 IF (ISMB(2).EQ.0 .AND. .NOT.PZMAT) KSYS78 = KSAVE
      IF (ISMB(2) .EQ. 0) RETURN
      DICT(3) = 9
      DICT(4) = 7
      IF (ICMBAR .LT. 0) GO TO 670
      CALL EMGOUT (AKJM,AKJM,81,1,DICT,2,IPR)
      GO TO 700
C
C     GENERATE LUMPED MASS MATRIX HERE
C
  670 DO 680 I = 1,9
  680 AKJM(I) = CONVM/3.0
      DICT(2) = 2
      CALL EMGOUT (AKJM,AKJM,9,1,DICT,2,IPR)
  700 IF (.NOT.PZMAT) KSYS78 = KSAVE
      RETURN
C
C     ERROR EXITS
C
 7770 I = 37
 7777 IF (IDEL1 .EQ. IDEL) GO TO 7778
      IDEL2  = IDEL1
      ICS(1) = IDEL1
      ICS(2) = JAX
      CALL MESAGE (30,I,ICS)
 7778 NOGO = .TRUE.
      GO TO 700
 7780 I = 126
      GO TO 7777
      END
