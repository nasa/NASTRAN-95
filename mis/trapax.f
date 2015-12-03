      SUBROUTINE TRAPAX
C
C     THIS SUBROUTINE CALCULATES THE STIFFNESS AND MASS MATRICES FOR THE
C     ASSYMETRIC RING ELEMENT WITH A TRAPEZOIDAL CROSS SECTION
C
C     SINGLE PRECISION VERSION
C
C     ECPT FOR THE TRAPAX ELEMENT
C
C     ECPT ( 1) = ELEMENT ID                                I
C     ECPT ( 2) = SIL A                                     I
C     ECPT ( 3) = SIL B                                     I
C     ECPT ( 4) = SIL C                                     I
C     ECPT ( 5) = SIL D
C     ECPT ( 6) = MATERIAL ORIENTATION ANGLE(DEGREES)       R
C     ECPT ( 8) = MATERIAL ID                               I
C     ECPT ( 9) TO ECPT (22) FOR PHI
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
      LOGICAL         PZMAT,LSYS78,IHEAT,NOGO
      INTEGER         ELID,ESTID,DICT(14),IPART(4)
      REAL            BMASS(12,12),BMBSS(144)
      REAL            ECPT(20),R(4),Z(4),DELINT(12),EE(63),TEO(45),
     1                SP(36),GB(12,12),GBP(4,4)
      DIMENSION       ACURL(208),D(144),AK(144),AKJ(256),ICS(4),
     1                IECPT(39),D1(48),D2(16),ACURP1(48),ACURP2(16),
     2                AKUPH(48),AKPH2(16)
C     DIMENSION       AKT(27)
      COMMON /SYSTEM/ KSYSTM(77),KSYS78,KDUM2(2),IHEAT
      COMMON /EMGPRM/ DUM(15),ISMB(3),IPREC,NOGO,HEAT,ICMBAR
      COMMON /EMGDIC/ IDM,LDICT,NGRIDS,ELID,ESTID
      COMMON /EMGEST/ IDEL,IGP(4),DGAMA,GAM,MATID,IPHI(13),CSDAT(16),
     1                TEMPE
      COMMON /CONDAS/ PI,TWOPI,XQ,DEGRAD
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO,GSUBE,MOSKP(9),
     1                SETMAT
      COMMON /MATPZ / PZOUT(51)
C     COMMON /MATPZ / CE11,CE12,CE13,CE14,CE15,CE16,CE22,CE23,CE24,CE25,
C                     CE26,CE33,CE34,CE35,CE36,CE44,CE45,CE46,CE55,CE56,
C                     CE66,E11,E12,E13,E14,E15,E16,E21,E22,E23,E24,E25,
C                     E26,E31,E32,E33,E34,E35,E36,EPS11,EPS12,EPS13,
C                     EPS22,
      EQUIVALENCE     (KSYSTM(2),IOUT),(ECPT(1),IECPT(1),IDEL),
     1                (BMASS(1,1),ACURL(1),BMBSS(1)),(DICT5,DICT(5)),
     2                (ACURP1(1),ACURL(145)),(ACURP2(1),ACURL(193))
      DATA    IDEL2 , JAX /    0,   4HTRAP  /
C
      LSYS78 = .FALSE.
      IF (KSYS78.EQ.0 .OR. KSYS78.EQ.2) LSYS78 = .TRUE.
      IDEL1 = IDEL/1000
      ISORT = 0
      MASOR = 0
C
C     IF STIFFNESS MATRIX NOT NEEDED GO CALCULATE MASS MATRIX
C
      DO 50 I = 1,4
      ICS(I) = IECPT(4*I+19)
      R(I) = ECPT(4*I+20)
      Z(I) = ECPT(4*I+21)
   50 D(I) = ECPT(4*I+22)
C
C     TEST THE VALIDITY OF THE GRID POINT COORDINATES
C     NOTE THAT INTEGRATION ROUTINE FAILS FOR R = 0.0
C
      DO 200 I = 1,4
      IF (R(I) .LE. 0.) GO TO 7770
      IF (D(I) .NE. 0.) GO TO 7770
  200 CONTINUE
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z(1),Z(2),Z(3),Z(4))
      DO 120 I = 1,4
  120 Z(I) = Z(I) - ZMIN
C
C     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE
C
      RMIN = AMIN1(R(1),R(2),R(3),R(4))
      RMAX = AMAX1(R(1),R(2),R(3),R(4))
      IF (RMIN .EQ. 0.0) GO TO 206
      IF (RMAX/RMIN .GT. 10.) GO TO 7760
C
  206 IF (R(1).GE.R(2) .OR. R(4).GE.R(3) .OR. Z(4).LE.Z(1)) GO TO 7770
      IF (ABS(Z(1)-Z(2)) .GT. .001) GO TO 7770
      IF (ABS(Z(3)-Z(4)) .GT. .001) GO TO 7770
      D(5) = (R(1)+R(4))/2.
      D(6) = (R(2)+R(3))/2.
      IF (D(5) .EQ. 0.0) GO TO 210
      IF (ABS((R(1)-R(4))/D(5)) .GT. .005) GO TO 210
      R(1) = D(5)
      R(4) = D(5)
  210 CONTINUE
      IF (D(6) .EQ. 0.0) GO TO 220
      IF (ABS((R(2)-R(3))/D(6)) .GT. .005) GO TO 220
      R(2) = D(6)
      R(3) = D(6)
  220 CONTINUE
C
C     FORM THE TRANSFORMMATION MATRIX(12X12) FROM FIELD COOR, TO GRID
C     POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,144
  300 GB( I, 1) = 0.
      GB( 1, 1) = 1.
      GB( 2, 1) = R(1)
      GB( 3, 1) = Z(1)
      GB( 4, 1) = R(1)*Z(1)
      GB( 5, 2) = 1.
      GB( 6, 2) = R(1)
      GB( 7, 2) = Z(1)
      GB( 8, 2) = GB(4,1)
      GB( 9, 3) = 1.
      GB(10, 3) = R(1)
      GB(11, 3) = Z(1)
      GB(12, 3) = GB(4,1)
      GB( 1, 4) = 1.
      GB( 2, 4) = R(2)
      GB( 3, 4) = Z(2)
      GB( 4, 4) = R(2)*Z(2)
      GB( 5, 5) = 1.
      GB( 6, 5) = R(2)
      GB( 7, 5) = Z(2)
      GB( 8, 5) = GB(4,4)
      GB( 9, 6) = 1.
      GB(10, 6) = R(2)
      GB(11, 6) = Z(2)
      GB(12, 6) = GB(4,4)
      GB( 1, 7) = 1.
      GB( 2, 7) = R(3)
      GB( 3, 7) = Z(3)
      GB( 4, 7) = R(3)*Z(3)
      GB( 5, 8) = 1.
      GB( 6, 8) = R(3)
      GB( 7, 8) = Z(3)
      GB( 8, 8) = GB(4,7)
      GB( 9, 9) = 1.
      GB(10, 9) = R(3)
      GB(11, 9) = Z(3)
      GB(12, 9) = GB(4,7)
      GB( 1,10) = 1.
      GB( 2,10) = R(4)
      GB( 3,10) = Z(4)
      GB( 4,10) = R(4)*Z(4)
      GB( 5,11) = 1.
      GB( 6,11) = R(4)
      GB( 7,11) = Z(4)
      GB( 8,11) = GB(4,10)
      GB( 9,12) = 1.
      GB(10,12) = R(4)
      GB(11,12) = Z(4)
      GB(12,12) = GB(4,10)
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
      IF (ISMB(1) .EQ. 0) GO TO 800
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY
C
      ISING = -1
      CALL INVERS (12,GB,12,D(10),0,D(11),ISING,SP)
      IF (ISING .EQ. 2) GO TO 7790
C
      IF (KSYS78 .EQ. 1) CALL INVERS (4,GBP,4,D(10),0,D(11),ISING,SP)
      IF (ISING  .EQ. 2) GOTO 7790
      IF (NOGO) RETURN
C
C     DELINT(01) = (-1,0)
C     DELINT(02) = (-1,1)
C     DELINT(03) = (-1,2)
C     DELINT(04) = ( 0,0)
C     DELINT(05) = ( 0,1)
C     DELINT(06) = ( 0,2)
C     DELINT(07) = ( 1,0)
C     DELINT(08) = ( 1,1)
C     DELINT(09) = ( 1,2)
C     DELINT(10) = ( 2,0)
C     DELINT(11) = ( 2,1)
C     DELINT(12) = ( 3,0)
C
      I1 = 0
      DO 400 I = 1,4
      IP = I - 2
      DO 350 J = 1,3
      IQ = J - 1
      I1 = I1 + 1
      IF (I1 .NE. 12)  GO TO 340
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
C
      GAMR   = DGAMA*DEGRAD
      COSG   = COS(GAMR)
      SING   = SIN(GAMR)
      SINTH  = SING
      COSTH  = COSG
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
C
      IF (SETMAT .EQ. 2.) GO TO 7780
CWKBI SPR94002 5/94
      DICT5 = GSUBE
      IF (KSYS78 .GT.  0) GO TO 500
      V   = ANU(1)*E(2)/E(1)
      VZ  = ANU(2)*E(3)/E(2)
      VR  = ANU(3)*E(1)/E(3)
      DEL = 1./(1. - V*ANU(1)  - VZ *ANU(2)- VR*ANU(3) - ANU(1)*ANU(2)*
     1      ANU(3) - V*VZ*VR )
C
C     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
C
  500 CONTINUE
      DO 510 I = 1,45
  510 TEO(I) = 0.
C
      IF (KSYS78 .GT. 0) GO TO 520
      TEO( 1) = E(1)*(1. - ANU(2)*VZ)*DEL
      TEO( 2) = E(1)*(ANU(3) + VZ*V)*DEL
      TEO( 3) = E(3)*(1. - ANU(1)*V)*DEL
      TEO( 4) = E(1)*(V  + ANU(3)*ANU(2))*DEL
      TEO( 5) = E(2)*(VZ + ANU(1)*ANU(3))*DEL
      TEO( 6) = E(2)*(1. - VR*ANU(3))*DEL
      TEO(10) = G(3)
      TEO(15) = G(1)
      TEO(21) = G(2)
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
C     MATRIX EG STORED AS FOLLOWS IN EE
C       1
C      2  3
C      4  5  6
C      7  8  9 10
C     11 12 13 14 15
C     16 17 18 19 20 21
C
      C2  = COSG*COSG
      S2  = SING*SING
      C4  = C2*C2
      S4  = S2*S2
      C2S2= C2*S2
      C3  = COSG*C2
      S3  = SING*S2
      CS2 = COSG*S2
      SC2 = SING*C2
      CS  = COSG*SING
C
      EE( 1) = TEO(1)*C4 + TEO(3)*S4 + 2.*C2S2*(TEO(2)+2.*TEO(10))
      EE( 2) = TEO(2)*(C4+S4) + C2S2*(TEO(1)+TEO(3)-4.0D0*TEO(10))
      EE( 3) = TEO(1)*S4 + 2.*C2S2*(TEO(2) + 2.*TEO(10))
     3       + TEO(3)*C4
      EE( 4) = TEO(4)*C2 + TEO(5)*S2
      EE( 5) = TEO(4)*S2 + TEO(5)*C2
      EE( 6) = TEO(6)
      EE( 7) = COSG*SING*S2*(TEO(2)-TEO(3)+2.*TEO(10))
     7       + SING*COSG*C2*(TEO(1)-TEO(2)-2.*TEO(10))
      EE( 8) = SING*COSG*C2*(TEO(2)-TEO(3)+2.*TEO(10))
     8       + COSG*SING*S2*(TEO(1)-TEO(2)-2.*TEO(10))
      EE( 9) = SING*COSG*(TEO(4) - TEO(5))
      EE(10) = C2S2*(TEO(1) - 2.*TEO(2) + TEO(3)) + TEO(10)*(C2-S2)**2
      EE(11) = 0.
      EE(12) = 0.
      EE(13) = 0.
      EE(14) = 0.
      EE(15) = TEO(15)*C2 + TEO(21)*S2
      EE(20) = COSG*SING*(TEO(15) - TEO(21))
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
      MJHO  = MOD(IECPT(1),1000) - 1
      AJHO  = MJHO
      AJJHO = AJHO*AJHO
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD SYSTEM
C
      ACURL( 1) = (EE(6) + AJJHO*EE(15))*DELINT(1)
      ACURL( 2) = (EE(4) + EE(6) + AJJHO*EE(15))*DELINT(4)
      ACURL( 3) = (EE(6) + AJJHO*EE(15))*DELINT(2) + EE(9)*DELINT(4)
      ACURL( 4) = (EE(4) + EE(6) + AJJHO*EE(15))*DELINT(5)
     4          + EE(9)* DELINT(7)
      ACURL( 5) = AJHO*(EE(6) + EE(15))*DELINT(1)
      ACURL( 6) = AJHO*EE(6)*DELINT(4)
      ACURL( 7) = AJHO*(EE(6) +EE(15))*DELINT(2) -AJHO*EE(20)*DELINT(4)
      ACURL( 8) = AJHO*EE(6)*DELINT(5) - AJHO*EE(20)*DELINT(7)
      ACURL( 9) = AJJHO*EE(20)*DELINT(1)
      ACURL(10) = DELINT(4)*(EE(9) + AJJHO*EE(20))
      ACURL(11) = DELINT(4)*EE(5) + AJJHO*DELINT(2)*EE(20)
      ACURL(12) = DELINT(7)*EE(5) + DELINT(5)*(EE(9)+AJJHO*EE(20))
      ACURL(14) = (EE(1) + 2.*EE(4) + EE(6) + AJJHO*EE(15))*DELINT(7)
      ACURL(15) = (EE(4) + EE(6) + AJJHO*EE(15))*DELINT(5) + (EE(7)
     5          + EE(9))*DELINT(7)
      ACURL(16) = (EE(1) + 2.*EE(4) + AJJHO*EE(15) + EE(6))*DELINT(8)
     6          + (EE(7) + EE(9))*DELINT(10)
      ACURL(17) = AJHO*(EE(4) + EE(6) + EE(15))*DELINT(4)
      ACURL(18) = AJHO*(EE(4) + EE(6))*DELINT(7)
      ACURL(19) = AJHO*(EE(4) + EE(6) + EE(15))*DELINT(5) - AJHO*EE(20)
     9          * DELINT(7)
      ACURL(20) = AJHO*(EE(4) + EE(6))*DELINT(8) -AJHO*EE(20)*DELINT(10)
      ACURL(21) = AJJHO*EE(20)*DELINT(4)
      ACURL(22) = DELINT(7)*(EE(7) + EE(9) + AJJHO*EE(20))
      ACURL(23) = DELINT(7)*(EE(2) + EE(5)) + AJJHO*DELINT(5)*EE(20)
      ACURL(24) = DELINT(10)*(EE(2) + EE(5)) + DELINT(8)*(EE(7)+EE(9))
     4          + DELINT(8)*AJJHO*EE(20)
      ACURL(27) = (EE(6) + AJJHO*EE(15))*DELINT(3) + 2.*EE(9)*DELINT(5)
     7          + EE(10)*DELINT(7)
      ACURL(28) = (EE(4) + EE(6) + AJJHO*EE(15))*DELINT(6)
     8          + EE(10)*DELINT(10) + (EE(7) + 2.*EE(9))*DELINT(8)
      ACURL(29) = AJHO*(EE(6) + EE(15))*DELINT(2) + AJHO*EE(9)*DELINT(4)
      ACURL(30) = AJHO*EE(6)*DELINT(5) + AJHO*EE(9)*DELINT(7)
      ACURL(31) = AJHO*(EE(6) + EE(15))*DELINT(3) + AJHO*(EE(9)
     1          - EE(20))*DELINT(5)
      ACURL(32) = AJHO*(EE(9) - EE(20))*DELINT(8) + AJHO*EE(6)*DELINT(6)
      ACURL(33) = AJJHO*EE(20)*DELINT(2)
      ACURL(34) = DELINT(7)*EE(10) + DELINT(5)*(EE(9) + AJJHO*EE(20))
      ACURL(35) = DELINT(7)*EE(8) + DELINT(5)*EE(5) + AJJHO*DELINT(3)
     5          * EE(20)
      ACURL(36) = DELINT(10)*EE(8) + DELINT(8)*(EE(5) + EE(10))
     6          + DELINT(6)*(EE(9) + AJJHO*EE(20))
      ACURL(40) = (EE(1) + 2.*EE(4) + EE(6)  + AJJHO*EE(15))*DELINT(9)
     O          + (2.*EE(7) + 2.*EE(9))*DELINT(11) + EE(10)*DELINT(12)
      ACURL(41) = AJHO*(EE(4) + EE(6) + EE(15))*DELINT(5)
     1          + AJHO*EE(9)*DELINT(7)
      ACURL(42) = AJHO*(EE(4) + EE(6))*DELINT(8) + AJHO*EE(9)*DELINT(10)
      ACURL(43) = AJHO*(EE(4) + EE(6) + EE(15))*DELINT(6)
     3          + AJHO*(EE(9) - EE(20))*DELINT(8)
      ACURL(44) = AJHO*(EE(4) + EE(6))*DELINT(9) + AJHO*(EE(9) - EE(20))
     4          * DELINT(11)
      ACURL(45) = AJJHO*EE(20)*DELINT(5)
      ACURL(46) = DELINT(8)*(EE(7) + EE(9) + AJJHO*EE(20)) + DELINT(10)
     6          * EE(10)
      ACURL(47) = DELINT(8)*(EE(2) + EE(5)) + DELINT(10)*EE(8)
     7          + AJJHO*DELINT(6)*EE(20)
      ACURL(48) = DELINT(11)*(EE(2) + EE(5) + EE(10)) + DELINT(12)*EE(8)
     8          + DELINT(9)*(EE(7) + EE(9) + AJJHO*EE(20))
      ACURL(53) = (EE(15) + AJJHO*EE(6))*DELINT(1)
      ACURL(54) = AJJHO*EE(6)*DELINT(4)
      ACURL(55) = (EE(15) + AJJHO*EE(6))*DELINT(2) - EE(20)*DELINT(4)
      ACURL(56) = AJJHO*EE(6)*DELINT(5) - EE(20)*DELINT(7)
      ACURL(57) = AJHO*EE(20)*DELINT(1)
      ACURL(58) = AJHO*DELINT(4)*(EE(9) + EE(20))
      ACURL(59) = AJHO*(DELINT(4)*EE(5) + DELINT(2)*EE(20))
      ACURL(60) = AJHO*(DELINT(7)*EE(5) + DELINT(5)*(EE(9) + EE(20)))
      ACURL(66) = AJJHO*EE(6)*DELINT(7)
      ACURL(67) = AJJHO*EE(6)*DELINT(5)
      ACURL(68) = AJJHO*EE(6)*DELINT(8)
      ACURL(69) = 0.
      ACURL(70) = AJHO*DELINT(7)*EE(9)
      ACURL(71) = AJHO*DELINT(7)*EE(5)
      ACURL(72) = AJHO*(DELINT(10)*EE(5) + DELINT(8)*EE(9))
      ACURL(79) = (EE(15) + AJJHO*EE(6))*DELINT(3) - 2.*EE(20)*DELINT(5)
     9          + EE(21)*DELINT(7)
      ACURL(80) = AJJHO*EE(6)*DELINT(6) - EE(20)*DELINT(8)
     O          + EE(21)*DELINT(10)
      ACURL(81) = AJHO*(EE(20)*DELINT(2) - EE(21)*DELINT(4))
      ACURL(82) = AJHO*(DELINT(5)*(EE(9) + EE(20)) - DELINT(7)*EE(21))
      ACURL(83) = AJHO*(DELINT(5)*(EE(5) - EE(21)) + DELINT(3)*EE(20))
      ACURL(84) = AJHO*(DELINT(8)*(EE(5) - EE(21)) + DELINT(6)*(EE(9)
     4          + EE(20)))
      ACURL(92) = EE(21)*DELINT(12) + AJJHO*EE(6)*DELINT(9)
      ACURL(93) =-AJHO*EE(21)*DELINT(7)
      ACURL(94) = AJHO*(DELINT(8)*EE(9) - DELINT(10)*EE(21))
      ACURL(95) = AJHO*DELINT(8)*(EE(5) - EE(21))
      ACURL(96) = AJHO*(DELINT(11)*(EE(5) - EE(21)) + DELINT(9)*EE(9))
      ACURL(105) = AJJHO*EE(21)*DELINT(1)
      ACURL(106) = AJJHO*DELINT(4)*EE(21)
      ACURL(107) = AJJHO*DELINT(2)*EE(21)
      ACURL(108) = AJJHO*DELINT(5)*EE(21)
      ACURL(118) = DELINT(7)*(EE(10) + AJJHO*EE(21))
      ACURL(119) = DELINT(7)*EE(8) + AJJHO*DELINT(5)*EE(21)
      ACURL(120) = DELINT(10)*EE(8) + DELINT(8)*(EE(10) + AJJHO*EE(21))
      ACURL(131) = DELINT(7)*EE(3) + AJJHO*DELINT(3)*EE(21)
      ACURL(132) = DELINT(10)*EE(3) + DELINT(8)*EE(8) + AJJHO*DELINT(6)
     2           * EE(21)
      ACURL(144) = DELINT(12)*EE(3) + 2.*DELINT(11)*EE(8) + DELINT(9)
     4           * (EE(10) + AJJHO*EE(21))
C
      IF (LSYS78) GO TO 550
      ACURL(145) = DELINT(1)*AJHO*(AJHO*EE(51) - EE(45))
      ACURL(146) = DELINT(4)*(EE(43) + AJHO*(AJHO*EE(51) - EE(49)
     6           - EE(45)))
      ACURL(147) = DELINT(2)*AJHO*(AJHO*EE(51) - EE(45)) + DELINT(4)
     7           * (EE(44) - AJHO*EE(50))
      ACURL(148) = DELINT(5)*(EE(43) + AJHO*(AJHO*EE(51) - EE(49)
     8           - EE(45))) + DELINT(7)*(EE(44) - AJHO*EE(50))
      ACURL(149) = DELINT(4)*AJHO*(AJHO*EE(51) - EE(45) - EE(39))
      ACURL(150) = DELINT(7)*(EE(43) + EE(37) + AJHO*(AJHO*EE(51)
     O           - EE(49) - EE(45) - EE(39)))
      ACURL(151) = DELINT(5)*AJHO*(AJHO*EE(51) - EE(45) - EE(39))
     1           + DELINT(7)*(EE(44) + EE(38) - AJHO*EE(50))
      ACURL(152) = DELINT(8)*(EE(43) + EE(37) + AJHO*(AJHO*EE(51)
     2           - EE(49) - EE(45) - EE(39))) + DELINT(10)*(EE(44)
     2           + EE(38) - AJHO*EE(50))
      ACURL(153) = DELINT(2)*AJHO*(AJHO*EE(51) - EE(45)) - DELINT(4)
     3           * AJHO*EE(48)
      ACURL(154) = DELINT(5)*(EE(43) + AJHO*(AJHO*EE(51) - EE(49)
     4           - EE(45))) + DELINT(7)*(EE(46) - AJHO*EE(48))
      ACURL(155) = DELINT(3)*AJHO*(AJHO*EE(51) - EE(45)) + DELINT(5)
     1           * (EE(44) - AJHO*(EE(50) + EE(48))) + DELINT(7)*EE(47)
      ACURL(156) = DELINT(6)*(EE(43) + AJHO*(AJHO*EE(51) - EE(49)
     6           - EE(45))) + DELINT(8)*(EE(46) + EE(44) - AJHO*(EE(50)
     6           + EE(48))) + DELINT(10)*EE(47)
      ACURL(157) = DELINT(5)*AJHO*(AJHO*EE(51) - EE(45) - EE(39))
     7           - DELINT(7)*AJHO*EE(48)
      ACURL(158) = DELINT(8)*(EE(43) + EE(47) + AJHO*(AJHO*EE(51)
     8           - EE(49) - EE(45) - EE(39))) - DELINT(10)*(EE(46)
     8           - AJHO*EE(48))
      ACURL(159) = DELINT(6)*AJHO*(AJHO*EE(51) - EE(45) - EE(39))
     9           + DELINT(8)*(EE(44) + EE(38) - AJHO*(EE(50) + EE(48)))
     9           + DELINT(10)*EE(47)
      ACURL(160) = DELINT(9)*(EE(43) + EE(37) + AJHO*(AJHO*EE(51)
     O           - EE(49) - EE(45) - EE(39))) + DELINT(11)*(EE(46)
     O           + EE(44) + EE(38) - AJHO*(EE(50) + EE(48)))
     O           + DELINT(12)*EE(47)
      ACURL(161) = DELINT(1)*AJHO*(EE(51) - AJHO*EE(45))
      ACURL(162) = DELINT(4)*(-EE(49) + AJHO*(EE(51) + EE(43)
     2           - AJHO*EE(45)))
      ACURL(163) = DELINT(2)*AJHO*(EE(51) - AJHO*EE(45)) + DELINT(4)
     3           * (AJHO*EE(44) - EE(50))
      ACURL(164) = DELINT(5)*(-EE(49) + AJHO*(EE(51) + EE(43) - AJHO
     4           * EE(51))) + DELINT(7)*(AJHO*EE(44) - EE(50))
      ACURL(165) =-DELINT(4)*AJJHO*EE(45)
      ACURL(166) = DELINT(7)*AJHO*(EE(43) - AJHO*EE(45))
      ACURL(167) = DELINT(7)*AJHO*EE(44) - DELINT(5)*AJJHO*EE(45)
      ACURL(168) = DELINT(8)*AJHO*(EE(43) - AJHO*EE(45)) + DELINT(10)
     8           * AJHO*EE(44)
      ACURL(169) = DELINT(2)*AJHO*(EE(51) - AJHO*EE(45)) - DELINT(4)
     9           * AJHO*EE(54)
      ACURL(170) = DELINT(5)*(-EE(49) + AJHO*(EE(51) + EE(43) - AJHO
     O           * EE(45))) + DELINT(7)*(EE(52) - AJHO*EE(54))
      ACURL(171) = DELINT(3)*AJHO*(EE(51) - AJHO*EE(45)) + DELINT(5)
     1           * (AJHO*(EE(44) - EE(54)) - EE(50)) + DELINT(7)*EE(53)
      ACURL(172) = DELINT(6)*(-EE(49) + AJHO*(EE(51) + EE(43) - AJHO
     2           * EE(45))) + DELINT(8)*(EE(52) - EE(50) + AJHO
     2           * (EE(44) - EE(54))) + DELINT(10)*EE(53)
      ACURL(173) =-DELINT(5)*AJJHO*EE(45) - DELINT(7)*AJHO*EE(54)
      ACURL(174) = DELINT(8)*AJHO*(EE(43) - AJHO*EE(45)) + DELINT(10)
     4           * (EE(54) - AJHO*EE(54))
      ACURL(175) =-DELINT(6)*AJJHO*EE(45) + DELINT(8)*AJHO*(EE(44)
     5           - EE(54)) + DELINT(10)*EE(53)
      ACURL(176) = DELINT(9)*AJHO*(EE(43) - AJHO*EE(45)) + DELINT(11)
     6           * (EE(52) + AJHO*(EE(44) - EE(54))) + DELINT(12)*EE(53)
      ACURL(177) = DELINT(1)*AJJHO*EE(54)
      ACURL(178) = DELINT(4)*AJHO*(AJHO*EE(54) - EE(52))
      ACURL(179) = DELINT(2)*AJJHO*EE(54) - DELINT(4)*AJHO*EE(53)
      ACURL(180) = DELINT(5)*AJHO*(AJHO*EE(54) - EE(52)) - DELINT(7)
     O           * AJHO*EE(53)
      ACURL(181) = DELINT(4)*AJHO*(AJHO*EE(54) - EE(48))
      ACURL(182) = DELINT(7)*(EE(46) + AJHO*(AJHO*EE(54) - EE(52)
     2           - EE(48)))
      ACURL(183) = DELINT(5)*AJHO*(AJHO*EE(54) - EE(48)) + DELINT(7)
     3           * (EE(47)-AJHO*EE(53))
      ACURL(184) = DELINT(8)*(EE(46) + AJHO*(AJHO*EE(54) - EE(52)
     4           - EE(48))) + DELINT(10)*(EE(47) - AJHO*EE(53))
      ACURL(185) = DELINT(2)*AJJHO*EE(54) - DELINT(4)*AJHO*EE(42)
      ACURL(186) = DELINT(5)*AJHO*(AJHO*EE(54) - EE(52)) + DELINT(7)
     6           * (EE(40) - AJHO*EE(42))
      ACURL(187) = DELINT(3)*AJJHO*EE(54) - DELINT(5)*AJHO*(EE(53)
     7           + EE(42)) + DELINT(7)*EE(41)
      ACURL(188) = DELINT(6)*AJHO*(AJHO*EE(54) - EE(52)) + DELINT(8)
     8           * (EE(40) - AJHO*(EE(53) + EE(42))) + DELINT(10)*EE(41)
      ACURL(189) =-DELINT(5)*AJHO*EE(48) + DELINT(4)*AJJHO*EE(54)
     9           - DELINT(7)*AJHO*EE(42)
      ACURL(190) = DELINT(8)*(EE(46) - AJHO*EE(48)) + DELINT(7)*AJHO
     O           * (AJHO*EE(54) - EE(52)) + DELINT(10)*(EE(40)
     O           - AJHO*EE(42))
      ACURL(191) =-DELINT(6)*AJHO*EE(48) + DELINT(5)*AJJHO*EE(54)
     1           + DELINT(8)*(EE(47) - AJHO*EE(42)) - DELINT(7)*AJHO
     1           * EE(53) + DELINT(10)*EE(41)
      ACURL(192) = DELINT(9)*(EE(46) - AJHO*EE(48)) + DELINT(8)*AJHO
     2           * (AJHO*EE(54) - EE(52)) + DELINT(11)*(EE(47) + EE(40)
     2           - AJHO*EE(42)) - DELINT(10)*AJHO*EE(53) + DELINT(12)
     2           * EE(41)
      ACURL(193) = DELINT(1)*AJJHO*EE(63)
      ACURL(194) = DELINT(4)*AJHO*(AJHO*EE(63) - EE(57))
      ACURL(195) = DELINT(2)*AJJHO*EE(63) - DELINT(4)*AJHO*EE(60)
      ACURL(196) = DELINT(5)*AJHO*(AJHO*EE(63) - EE(57)) - DELINT(7)
     6           * AJHO*EE(60)
      ACURL(197) = DELINT(4)*AJHO*(AJHO*EE(63) - EE(57))
      ACURL(198) = DELINT(7)*(AJJHO*EE(63) - 2.0*AJHO*EE(57) + EE(55))
      ACURL(199) = DELINT(5)*AJHO*(AJHO*EE(63) - EE(57)) + DELINT(7)
     9           * (EE(56) - AJHO*EE(60))
      ACURL(200) = DELINT(8)*(AJJHO*EE(63) - 2.0*AJHO*EE(57) + EE(55))
     O           + DELINT(10)*(EE(56) - AJHO*EE(60))
      ACURL(201) = DELINT(2)*AJJHO*EE(63) - DELINT(4)*AJHO*EE(60)
      ACURL(202) = DELINT(5)*AJHO*(AJHO*EE(63) - EE(57)) + DELINT(7)
     2           * (EE(56) - AJHO*EE(60))
      ACURL(203) = DELINT(3)*AJJHO*EE(63) - DELINT(5)*2.0*AJHO*EE(60)
     3           + DELINT(7)*EE(59)
      ACURL(204) = DELINT(6)*AJHO*(AJHO*EE(63) - EE(57)) + DELINT(8)
     4           * (EE(56) - 2.0*AJHO*EE(60)) + DELINT(10)*EE(59)
      ACURL(205) = DELINT(5)*AJHO*(AJHO*EE(63) - EE(57)) - DELINT(7)
     5           * AJHO*EE(60)
      ACURL(206) = DELINT(8)*(AJJHO*EE(63) - 2.0*EE(57) + EE(55))
     6           + DELINT(10)*(EE(56) - AJHO*EE(60))
      ACURL(207) = DELINT(6)*AJHO*(AJHO*EE(63) - EE(57)) + DELINT(8)
     7           * (EE(56) - 2.0*AJHO*EE(60)) + DELINT(10)*EE(59)
      ACURL(208) = DELINT(9)*(AJJHO*EE(63) - 2.0*AJHO*EE(57) + EE(55))
     8           + 2.0*DELINT(11)*(EE(56) - AJHO*EE(60)) + DELINT(12)
     8           * EE(59)
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
      I  = I  + 1
  605 ACURL(IC) = ACURL(I)
  610 CONTINUE
C
      DGAMA = PI
      IF (AJHO .EQ. 0.) DGAMA = TWOPI
      DO 630 I = 1,144
      ACURL(I) = ACURL(I)*DGAMA
  630 CONTINUE
C
      IF (LSYS78) GO TO 638
      DO 632 I = 145,208
  632 ACURL(I) = ACURL(I)*DGAMA
  638 CONTINUE
C
      CALL GMMATS (GB,12,12,1, ACURL,12,12,0, D )
      CALL GMMATS (D ,12,12,0, GB   ,12,12,0, AK)
C
      IF (LSYS78) GO TO 639
      CALL GMMATS (GB,12,12,1, ACURP1,12,4,0, D1)
      CALL GMMATS (D1,12,4,0, GBP,4,4,0, AKUPH)
      CALL GMMATS (GBP,4,4,1, ACURP2,4,4,0, D2)
      CALL GMMATS (D2,4,4,0, GBP,4,4,0, AKPH2)
  639 CONTINUE
C
      DO 640 I = 1,256
  640 AKJ(I) = 0.
      GO TO 655
C
C     COORDINATE SYSTEMS NOT POSSIBLE WITH RINGAX  CODE BELOW COULD
C     IMPLEMENT IT.
C
C     IF FOLLOWING CODE IS IMPLEMENTED MUST BE MODIFIED FOR
C     PIEZOELECTRIC
C
C     DO 650 I = 1,4
C     IF (ICS(I) .EQ. 0) GOTO 650
C     K = 9*(I-1) + 1
C     CALL TRANSS (ICS(I),D(K))
C 650 CONTINUE
C
C     SELECT THE APPROPRIATE SUB MATRIX FOR TRANSFORMATION
C
C     DO 690 IPP = 1,4
C     IR1 = 3*(IPP-1) + 1
C     IAPP= 9*(IPP-1) + 1
C     DO 680 I = 1,4
C     IC1 = 3*(I-1) + 1
C     IRC = (IR1-1)*12 + IC1
C     AKT(1) = AK(IRC   )
C     AKT(2) = AK(IRC+ 1)
C     AKT(3) = AK(IRC+ 2)
C     AKT(4) = AK(IRC+12)
C     AKT(5) = AK(IRC+13)
C     AKT(6) = AK(IRC+14)
C     AKT(7) = AK(IRC+24)
C     AKT(8) = AK(IRC+25)
C     AKT(9) = AK(IRC+26)
C
C     MORE COORDINATE SYSTEM CHANGE CODE
C
C     TRANSFORM THE STIFFNESS  SUB MATRICES  AS NECESSARY
C
C     IAKT = 1
C     IF (ICS(IPP) .EQ. 0) GO TO 660
C     CALL GMMATS (D(IAPP),3,3,1, AKT(1),3,3,0, AKT(10))
C     IAKT = 10
C     IF (ICS(I).EQ.0 .AND. ICS(IPP).EQ.0) GO TO 680
C 660 IF (ICS(I) .EQ.0) GO TO 670
C     IAI = 9*(I-1) + 1
C     CALL GMMATS (AKT(IAKT),3,3,0, D(IAI),3,3,0, AKT(IAKT+9))
C     IAKT = IAKT + 9
C
C     REPLACE THE TRANSFORMED MATRICES IN ORIGINAL MATRIX
C
C 670 AK(IRC   ) = AKT(IAKT  )
C     AK(IRC+ 1) = AKT(IAKT+1)
C     AK(IRC+ 2) = AKT(IAKT+2)
C     AK(IRC+12) = AKT(IAKT+3)
C     AK(IRC+13) = AKT(IAKT+4)
C     AK(IRC+14) = AKT(IAKT+5)
C     AK(IRC+24) = AKT(IAKT+6)
C     AK(IRC+25) = AKT(IAKT+7)
C     AK(IRC+26) = AKT(IAKT+8)
C
C 680 CONTINUE
C 690 CONTINUE
C
C     CREATE AN ARRAY POINTING TO THE GRIDS ACCORDING TO INCREASING
C     SIL VALUE
C
  655 CONTINUE
C
      ASSIGN 780 TO K OR M
  700 CONTINUE
      DO 705 I = 1,4
      IPART(I) = IECPT(I+1)
  705 CONTINUE
      I = -4
  710 J = 0
      DO 715 KK = 1,4
      IF (IPART(KK) .LT. J) GO TO 715
      J = IPART(KK)
      L = KK
  715 CONTINUE
      IPART(L) = I
      I = I + 1
      IF (I .LT. 0) GO TO 710
      DO 720 I = 1,4
      IPART(I) = -IPART(I)
  720 CONTINUE
      ISORT = 1
      GO TO K OR M, (780,880)
C
C     REARRANGE  AK  INTO AKJ BY INCREASING SIL VALUE
C     NOTE AKJ ALREADY INITALIZED TO ZERO
C
  780 DO 770 I = 1,4
      IT = IPART(I)
      DO 760 J = 1,4
      JT = IPART(J)
      DO 750 K = 1,3
      DO 740 L = 1,3
      IKJ = (IT-1)*64 + (JT-1)*4 + (K-1)*16 + L
      IF (MASOR .EQ. 1) IKJ = (IT-1)*36 + (JT-1)*3 + (K-1)*12 + L
      IK  = (I-1)*36 + (J-1) *3 +(K-1)*12 + L
      AKJ(IKJ) = AK(IK)
C
      IF (MASOR .EQ. 1) GO TO 740
      IF (LSYS78) GO TO 740
      IKJA = IKJ - L + 4
      IKA  = (IK-L)/3 + 1
      IKJB = (JT-1)*64 + 48 + (IT-1)*4 + K
      IKJC = (IT-1)*64 + 52 + (JT-1)*4
      IKC  = (I-1)*4 + J
      AKJ(IKJA) = AKUPH(IKA)
      AKJ(IKJB) = AKUPH(IKA)
      AKJ(IKJC) = AKPH2(IKC)
  740 CONTINUE
  750 CONTINUE
  760 CONTINUE
  770 CONTINUE
      IF (MASOR .EQ. 1) GO TO 895
C
C     SET UP CONSTANTS AND OUTPUT AKJ
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 16
      DICT(4) = 15
CWKBD SPR94002 5/94      DICT5   =  GSUBE
      IP      = IPREC
      CALL EMGOUT (AKJ,AKJ,256,1,DICT,1,IP)
C
C    COME HERE TO CALCULATE THE MASS  MATRIX. THIS ROUTINE WILL
C    CALCULATE EITHER THE CONSISTENT OR LUMPED MASS MATRICES
C    DEPENDING ON THE PARAMETER ICM BAR
C
C
C     IF STIFFNESS MATRIX NOT NEEDED WE HAVE ALL WE NEED FOR THE
C     MASS MATRIX CALCULATIONS
  800 IF (ISMB(2).EQ.0 .AND. .NOT.PZMAT) KSYS78 = KSAVE
      IF (ISMB(2) .EQ. 0) RETURN
      IF (ICMBAR  .LT. 0) GO TO 820
      I1 = 0
      DO 810 I = 1,3
      IP = I
      DO 810 J = 1,3
      IQ = J - 1
      I1 = I1 + 1
      DELINT(I1) = RZINTS(IP,IQ,R,Z,4)
  810 CONTINUE
C
  820 IF (ISMB(1) .NE. 0) GO TO 830
      MATIDC = MATID
      MATFLG = 7
      IF (KSYS78 .GT. 0) MATFLG = 9
      ELTEMP = TEMPE
      GAMR   = DGAMA*DEGRAD
      COSTH  = COS(GAMR)
      SINTH  = SIN(GAMR)
      CALL MAT (IDEL)
      IF (KSYS78 .GT.  0) RHO = PZOUT(46)
      IF (SETMAT .EQ. 2.) GO TO 7780
C
C     COMPUTE THE HARMONIC COEFFICIENT
C
  830 MJHO = MOD(IECPT(1),1000) - 1
      AJHO = MJHO
      RHOD = RHO*PI
      IF (AJHO  .EQ. 0.) RHOD = 2.*RHOD
      IF (ICMBAR .LT. 0) GO TO 900
C
C     COMPUTE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
C
      DO 840 I = 1,144
  840 BMASS( I, 1) = 0.
      BMASS( 1, 1) = DELINT(1)
      BMASS( 1, 2) = DELINT(4)
      BMASS( 1, 3) = DELINT(2)
      BMASS( 1, 4) = DELINT(5)
      BMASS( 2, 2) = DELINT(7)
      BMASS( 2, 3) = DELINT(5)
      BMASS( 2, 4) = DELINT(8)
      BMASS( 3, 3) = DELINT(3)
      BMASS( 3, 4) = DELINT(6)
      BMASS( 4, 4) = DELINT(9)
      BMASS( 5, 5) = DELINT(1)
      BMASS( 5, 6) = DELINT(4)
      BMASS( 5, 7) = DELINT(2)
      BMASS( 5, 8) = DELINT(5)
      BMASS( 6, 6) = DELINT(7)
      BMASS( 6, 7) = DELINT(5)
      BMASS( 6, 8) = DELINT(8)
      BMASS( 7, 7) = DELINT(3)
      BMASS( 7, 8) = DELINT(6)
      BMASS( 8, 8) = DELINT(9)
      BMASS( 9, 9) = DELINT(1)
      BMASS( 9,10) = DELINT(4)
      BMASS( 9,11) = DELINT(2)
      BMASS( 9,12) = DELINT(5)
      BMASS(10,10) = DELINT(7)
      BMASS(10,11) = DELINT(5)
      BMASS(10,12) = DELINT(8)
      BMASS(11,11) = DELINT(3)
      BMASS(11,12) = DELINT(6)
      BMASS(12,12) = DELINT(9)
      DO 860 IB = 2,12
      IC = 13*IB - 25
      I  = IC
      DO 850 J = IB,12
      IC = IC + 12
      I  = I + 1
  850 BMBSS(I) = BMBSS(IC)
  860 CONTINUE
      DO 870 I = 1,144
  870 BMBSS(I) = BMBSS(I)*RHOD
C
C     TRANSFORM THE ELEMENT MASS MATRIX FROM FIELD COORDINATES TO
C     GRID  POINT DEGREES  OF FREEDOM
C
      CALL GMMATS (GB,12,12,1, BMASS,12,12,0, D)
      CALL GMMATS (D ,12,12,0, GB,12,12,0, AK)
      DO 875 I = 1,256
  875 AKJ(I) = 0.
      IF (ISORT .EQ. 1) GO TO 880
      ASSIGN 880 TO K OR M
      GO TO  700
C
C     REARRANGE AK INTO AKJ BY INCREASING SIL VALUE
C
  880 MASOR = 1
      GO TO 780
C
  895 DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 12
      DICT(4) =  7
CWKBD SPR94002 5/94      DICT5   =  0.
      IP      = IPREC
C
      CALL EMGOUT (AKJ,AKJ,144,1,DICT,2,IP)
      GO TO 940
C
C
C     LUMPED MASS CALCULATIONS HANDLED HERE
C
  900 AR = (R(1)*(Z(2)-Z(4)) + R(2)*(Z(3)-Z(1)) + R(3)*(Z(4)-Z(2)) +
     1      R(4)*(Z(1)-Z(3)))/2.
      AKJ(1) = RHOD*(R(1) + R(2) + R(3) + R(4))/4.*AR
      AKJ(1) = AKJ(1)/4.0
      DO 920 I = 2,12
  920 AKJ(I) = AKJ(1)
C
      DICT(1) = ESTID
      DICT(2) = 2
      DICT(3) = 12
      DICT(4) = 7
CWKBD SPR94002 5/94      DICT5   = 0.
      IP      = IPREC
C
      CALL EMGOUT (AKJ,AKJ,12,1,DICT,2,IP)
  940 IF (.NOT.PZMAT) KSYS78 = KSAVE
      RETURN
C
C     SET FATAL ERROR FLAG AND ALLOWING ERROR MESSAGES TO ACCUMULATE
C
 7760 I = 218
      GO TO 7800
 7770 I = 37
      GO TO 7800
C
C     MAT2 NOT LEGAL
C
 7780 I = 126
      GO TO 7800
 7790 I =  26
 7800 IF (IDEL1 .EQ. IDEL2) GO TO 7810
      IDEL2  = IDEL1
      ICS(1) = IDEL1
      ICS(2) = JAX
      CALL MESAGE (30,I,ICS)
 7810 NOGO = .TRUE.
      GO TO 940
      END
