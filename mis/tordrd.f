      SUBROUTINE TORDRD
C
C     THIS SUBROUTINE COMPUTES THE STIFFNESS MATRIX AND THE MASS MATRIX
C     FOR AN AXI-SYMMETRIC TORDIDAL THIN SHELL RING
C
C     DOUBLE PRECISION VERSION
C
C     THIS  SUBROUTINE USES ROUTINES  ROMBDK , DMATRX
C
C
C*****
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
C*****
C
      DOUBLE PRECISION CONSTD
      DIMENSION  IECPT(18),        ICS(2)
      REAL ECPT(17)
      DOUBLE PRECISION
     X   AM(144),GAMBQF(72),GAMBQM(48),EE(4),AK(144),GAMRS(144),AKI(36),
     X         DELINT(66),D(144),R(2),Z(2),KOUT(144),GAMBQ(144)
      DOUBLE PRECISION A1,A2,AKM(36),MOUT(144)
      DOUBLE PRECISION TWOPI,DEGRAD,PHIB,RP,S,SINA1,
     X                 SINA2,COSA1,COSA2,R1,R2,Z1,Z2,EP,ET,VPT,VTP,DEL,
     X                 DJP1
      INTEGER  DICT (9),ELID,ESTID
      LOGICAL  NOGO,HEAT
C
C
      COMMON /CONDAD/ CONSTD(5)
C
      COMMON   /MATIN/
     1                   MATIDC        ,MATFLG
     2,                  ELTEMP        ,STRESS
     3,                  SINTH         ,COSTH
      COMMON   /MATOUT/
     1                   E(3)          ,ANU(3)
     2,                  RHO           ,G(3)
     3,                  ALF(3)        ,TZERO,    GSUBE
      COMMON  /SYSTEM/ KSYSTM(55),HEAT
C
      COMMON  /EMGPRM/ DUM(15), ISMB(3),IPREC,NOGO,IHEAT
      COMMON  /EMGDIC/ IDM, LDICT,NGRIDS, ELID,ESTID
C
      COMMON  /EMGEST/ IDEL,IGP(2),ALPH(2),OM,MATID,TM,TF,ICS1,
     X                 XYZ(3),ICS2,XYZ2(3),TEMPE
C
      EQUIVALENCE (DICT5,DICT(5))
      EQUIVALENCE (IECPT(1),ECPT(1),IDEL)
      EQUIVALENCE ( CONSTD(2) , TWOPI  )
      EQUIVALENCE ( CONSTD(4) , DEGRAD )
      EQUIVALENCE        (GAMBQF(1), GAMBQ(1))
      EQUIVALENCE        (GAMBQM(1), GAMBQ(73))
      EQUIVALENCE        (DELINT(1), GAMBQ(1))
      EQUIVALENCE        (GAMRS(1),  GAMBQ(1))
      EQUIVALENCE (R1,R(1)),(R2,R(2)),(Z1,Z(1)),(Z2,Z(2))
C
C
C ----------------------------------------------------------------------
C
C     SET UP THE DICT ARRAY
C
      IPR= IPREC
      DICT(1) = ESTID
      DICT(3) = 12
      DICT(4) = 63
      ICS(1)= IECPT(10)
      ICS(2)= IECPT(14)
      R(1) = ECPT(11)
      D1   = ECPT(12)
      Z(1) = ECPT(13)
      R(2) = ECPT(15)
      D2   = ECPT(16)
      Z(2) = ECPT(17)
C
C
C TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      IF (R1 .LT. 0. .OR. R2 .LT. 0.) GO TO 7770
      IF (D1 .NE. 0. .OR. D2 .NE. 0.) GO TO 7770
C
C
C DETERMINE IF ELEMENT IS A TOROIDAL, CONICAL OR CYLINDRICAL RING
C
      ITORD = 0
      IF (ABS(ALPH(1) - ALPH(2)) .LE. 1.E-6)  ITORD = 1
      IF (ITORD .EQ. 1 .AND. ABS(ALPH(1)-90.) .LE. 1.E-5) ITORD=-1
C
C
C COMPUTE THE ELEMENT COORDINATES
C
      A1 = DBLE(ALPH(1)) * DEGRAD
      A2 = DBLE(ALPH(2)) * DEGRAD
      PHIB = A2 - A1
      SINA1 =  DSIN(A1)
      COSA1 =  DCOS(A1)
      SINA2 =  DSIN(A2)
      COSA2 =  DCOS(A2)
C
      IF (ITORD .NE. 0) GO TO 100
C
C FOR THE TOROIDAL RING
C
      RP = DSQRT((R2-R1)**2  + (Z2-Z1)**2) /(2.D0*DSIN(PHIB/2.D0))
      S  = PHIB * RP
      GO TO 110
C
C  FOR THE CONICAL OR CYLINDRICAL RING
C
  100 RP = 0.D0
      S  = DSQRT((R2-R1)**2  +  (Z2-Z1)**2)
C
C  COMPUTE THE BASIC AND REQUIRED INTEGRALS
C
C  SET UP THE ARRAY OF CONSTANTS FOR ROMBER INTEGRATION ROUTINE
C
  110 D(21) = 0.D0
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
      DO  260 JP1=1,11
      J = JP1 - 1
      K = (J * 6) + 1
      DJP1 = JP1
C
C TEST FOR ELEMENT SHAPE
C
      IF (ITORD) 240,120,170
C
C THE TOROIDAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1),...,D(6)
C
  120 D(20) = (RP** JP1)
C
C COMPUTE I(J,1)
C
      D(1) = D(20) * (PHIB ** JP1) / DJP1
C
C COMPUTE I(J,2)
C
      D(2) = (PHIB**(JP1+1))/ (DJP1 + 1.)
      D(10)= 1.
      DO  130 I=1,20
      IP = JP1 + 2 * I + 1
      D(11) = 2 * I + 1
      D(10)= D(10)*D(11)*(D(11)-1.)
      D(12)= (-1.)**I * PHIB**IP/((DJP1+D(11))*D(10))
      D(13) =  DABS (D(12)/D(2))
      D(2) = D(2)+ D(12)
      IF (D(13) .LE. 1.D-10) GO TO 140
  130 CONTINUE
      GO TO 7780
  140 D(2) = D(20)*D(2)
C
C COMPUTE I(J,3)
C
      D(3) = (PHIB ** JP1) / DJP1
      D(10) = 1.
      DO 150 I=1,20
      IP = JP1 + 2 * I
      D(11) = 2 * I
      D(10) = D(10)*D(11)*(D(11)-1.)
      D(12) = (-1.)**I  * PHIB**IP/((DJP1+D(11)) *D(10))
      D(13) = DABS (D(12)/D(3))
      D(3)  =  D(3) + D(12)
      IF (D(13) .LE. 1.D-10) GO TO 160
  150 CONTINUE
      GO TO 7780
  160 CONTINUE
      D(3) = D(20) * D(3)
      D(26) = DJP1
C
C COMPUTE I(J,4)
C
      CALL ROMBDK (PHIB,D(10),IP, D(4),1,D(21))
      IF (IP .GE. 15) CALL MESAGE (30,26,IDEL)
      D(4) = D(20) * D(4)
C
C COMPUTE I(J,5)
C
      CALL ROMBDK (PHIB,D(10),IP,D(5),2,D(21))
      IF (IP .GE. 15) CALL MESAGE (30,26,IDEL)
      D(5) = D(20) * D(5)
C
C COMPUTE I(J,6)
C
      CALL ROMBDK (PHIB,D(10),IP,D(6),3,D(21))
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
      GO TO 250
C
C THE CONICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1) AND D(2)
C
C
C COMPUTE I(J,1)
C
  170 D(1) = (S **JP1)/DJP1
      IF (J-1) 180,190,200
C
C   COMPUTE  I(0,2)
C
  180 D(2) = DLOG(( R1+ S*COSA1)/R1)/COSA1
      GO TO 230
C
C    COMPUTE I(1,2)
C
  190 D(2) = (S- (R1/COSA1) * DLOG((R1+S*COSA1)/R1)) /COSA1
      GO TO 230
C
C    COMPUTE I(J,2) WHERE J .GT.1
C
  200 D(2) =1./DJP1
      D(10)=  -S*COSA1/R1
      DO  210 I= 1,1000
      D(11) = JP1 + I
      D(12) = (D(10) ** I) / D(11)
      D(2) = D(2) + D(12)
      IF (D(12) .LT.  1.D-4)  GO TO 220
  210 CONTINUE
      GO TO 7780
  220 D(2)=  ((S**JP1)/R1)* D(2)
C
C THE CONICAL RING REQUIRED INTEGRALS
C
  230 DELINT(K  ) = R1*D(1) + COSA1*(S**(JP1+1)/(DJP1+1.))
      DELINT(K+1) = SINA1 * D(1)
      DELINT(K+2) = D(35) * D(2)
      DELINT(K+3) = COSA1 * D(1)
      DELINT(K+4) = D(34) * D(2)
      DELINT(K+5) = D(33) * D(2)
      GO TO 250
C
C THE CYLINDRICAL RING BASIC INTEGRALS WILL BE COMPUTED IN
C LOCATIONS D(1) AND D(2)
C
C
C COMPUTE I(J,1)
C
  240 D(1) = (S**JP1)/DJP1
C
C COMPUTE I(J,2)
C
      D(2) = D(1) / R1
C
C THE CYLINDRICAL RING REQUIRED INTEGRALS
C
      DELINT(K  ) =  R1*D(1) + COSA1*(S**(JP1+1)/(DJP1+1.))
      DELINT(K+1) = SINA1 * D(1)
      DELINT(K+2) = D(35) * D(2)
      DELINT(K+3) = 0.
      DELINT(K+4) = 0.
      DELINT(K+5) = 0.
C
  250 CONTINUE
C
  260 CONTINUE
C
C   IF STIFFNESS MATRIX NOT REQUIRED  GO TO MASS ROUTINE
C
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
      VTP= VPT * ET / EP
      DEL = 1. - VPT*VTP
      DICT5 = G SUBE
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
C FORM THE STIFFNESS MATRIX IN FIELD COORDINATES
C
C COMPUTE CONSTANTS NEEDED IN DMATRX SUBROUTINE
C
      D(1) = EP / ET
      D(7) = 0.
      IF (ITORD .EQ. 0) D(7) = 1./RP
      D(2) = D(1) * D(7)
      D(3) = D(2) * D(7)
      D(4) = VPT * D(7)
      D(5) =(EP * TM / (D(1) - VPT**2)) * TWOPI
      D(6) = (EP*TF**3)/(12.*(D(1)-VPT**2))*TWOPI
C
C CALL THE DMATRIX SUBROUTINE TO COMPUTE THE STIFFNESS MATRIX (10X10)
C
C NOTE THE DOUBLE SUBSCRIPTING USED IN DMATRIX SUBROUTINE IS
C COMPATIBLE WITH THE CALLING PROGRAM. THE DELINT ARRAY OF INTEGRALS
C IS A (11X6) SINGLY SUBSCRIPTED ARRAY (STORED ROWWISE) IN THE CALLING
C PROGRAM AND IT IS A (6X11) DOUBLY SUBSCRIPTED ARRAY (STORED
C COLUMNWISE) IN DMATRX ROUTINE.
C
      IF (ISMB(1) .EQ.  0) GO TO 270
      CALL DMATRX (AK(1),VPT,D(1),D(2),D(3),D(4),D(5),D(6),DELINT(1))
  270 IF (ISMB(2) .EQ. 0)  GO TO 279
      DO 275 I=1,100
  275 AM(I) = 0.
      AM(  1) = DELINT( 1)
      AM(  2) = DELINT( 7)
      AM(  3) = DELINT(13)
      AM(  4) = DELINT(19)
      AM( 11) = DELINT( 7)
      AM( 12) = DELINT(13)
      AM( 13) = DELINT(19)
      AM( 14) = DELINT(25)
      AM( 21) = DELINT(13)
      AM( 22) = DELINT(19)
      AM( 23) = DELINT(25)
      AM( 24) = DELINT(31)
      AM( 31) = DELINT(19)
      AM( 32) = DELINT(25)
      AM( 33) = DELINT(31)
      AM( 34) = DELINT(37)
      AM( 45) = DELINT( 1)
      AM( 46) = DELINT( 7)
      AM( 47) = DELINT(13)
      AM( 48) = DELINT(19)
      AM( 49) = DELINT(25)
      AM( 50) = DELINT(31)
      AM( 55) = DELINT( 7)
      AM( 56) = DELINT(13)
      AM( 57) = DELINT(19)
      AM( 58) = DELINT(25)
      AM( 59) = DELINT(31)
      AM( 60) = DELINT(37)
      AM( 65) = DELINT(13)
      AM( 66) = DELINT(19)
      AM( 67) = DELINT(25)
      AM( 68) = DELINT(31)
      AM( 69) = DELINT(37)
      AM( 70) = DELINT(43)
      AM( 75) = DELINT(19)
      AM( 76) = DELINT(25)
      AM( 77) = DELINT(31)
      AM( 78) = DELINT(37)
      AM( 79) = DELINT(43)
      AM( 80) = DELINT(49)
      AM( 85) = DELINT(25)
      AM( 86) = DELINT(31)
      AM( 87) = DELINT(37)
      AM( 88) = DELINT(43)
      AM( 89) = DELINT(49)
      AM( 90) = DELINT(55)
      AM( 95) = DELINT(31)
      AM( 96) = DELINT(37)
      AM( 97) = DELINT(43)
      AM( 98) = DELINT(49)
      AM( 99) = DELINT(55)
      AM(100) = DELINT(61)
C
      D(1) = TWO PI * RHO * TM
      DO  278  I=1,100
  278 AM(I)= D(1) * AM(I)
  279 CONTINUE
C
C
C
C FORM THE TRANSFORMATION MATRIX(10X12) FROM FIELD COORDINATES TO GRID
C POINT DEGREES OF FREEDOM
C
      DO 280 I=1,72
  280 GAMBQF(I)=0.
      D(1) = S
      D(2) = S ** 2
      D(3) = S ** 3
      D(4) = S ** 4
      D(5) = S ** 5
      GAMBQF(3) = 1.
      GAMBQF(16)= 1.
      GAMBQF(30)= .5
      GAMBQF(39)=-10./D(3)
      GAMBQF(40)= -6./D(2)
      GAMBQF(42)= -1.5/D(1)
      GAMBQF(45) = -GAMBQF(39)
      GAMBQF(46) = -4./D(2)
      GAMBQF(48) =  .5/D(1)
      GAMBQF(51) = 15./D(4)
      GAMBQF(52) =  8./D(3)
      GAMBQF(54) = 1.5/D(2)
      GAMBQF(57) = -GAMBQF(51)
      GAMBQF(58) =  7./D(3)
      GAMBQF(60) = -1./D(2)
      GAMBQF(63) = -6./D(5)
      GAMBQF(64) = -3./D(4)
      GAMBQF(66) = -.5/D(3)
      GAMBQF(69) = -GAMBQF(63)
      GAMBQF(70) =  GAMBQF(64)
      GAMBQF(72) = -GAMBQF(66)
      DO 290 I=1,48
  290 GAMBQM(I) = 0.
      GAMBQM(1) = 1.
      GAMBQM(17)= 1.
      GAMBQM(25)=  -3./D(2)
      GAMBQM(29) = -2./D(1)
      GAMBQM(31)= -GAMBQM(25)
      GAMBQM(35) = -1./D(1)
      GAMBQM(37) = 2./D(3)
      GAMBQM(41) = 1./D(2)
      GAMBQM(43) = -GAMBQM(37)
      GAMBQM(47) =  GAMBQM(41)
C
C
C TRANSFORM THE STIFFNESS MATRIX TO GRID POINT DEGREES OF FREEDOM
C
      IF(ISMB(1).EQ. 0) GO TO 295
      CALL GMMATD (GAMBQ(1),10,12,1,  AK(1),10,10,0, D(1))
      CALL GMMATD (D(1), 12,10,0, GAMBQ(1), 10,12,0, AK(1))
  295 IF (ISMB(2).EQ.0)  GO TO 299
C     REARRANGE GAMBQ FOR MASS MATRIX CALCULATIONS
      DO 296 I=1,72
296   D(I+48) = GAMBQ(I)
      DO 297 I=1,48
297   D(I) = GAMBQ(I+72)
      DO 298 I=1,120
 298  GAMBQ(I)=D(I)
      CALL GMMATD (GAMBQ(1), 10,12,1, AM(1), 10,10,0,  D(1))
      CALL GMMATD (D(1), 12,10,0, GAMBQ(1), 10,12,0, AM(1))
C
  299 CONTINUE
C
C
C FORM THE TRANSFORMATION MATRIX (12X12) FROM ELEMENT TO BASIC
C COORDINATES
C
      DO 300 I=1,144
  300 GAMRS(I)=0.
      GAMRS( 1) =  COSA1
      GAMRS( 3) = -SINA1
      GAMRS(25) =  SINA1
      GAMRS(27) =  COSA1
      GAMRS(40) = -1.
      GAMRS(53) =  1.
      GAMRS(66) =  1.
      GAMRS(79) =  COSA2
      GAMRS(81) = -SINA2
      GAMRS(103)=  SINA2
      GAMRS(105)=  COSA2
      GAMRS(118) = -1.
      GAMRS(131) = 1.
      GAMRS(144) = 1.
C
C
C TRANSFORM THE STIFFNESS MATRIX FROM ELEMENT TO BASIC COORDINATES
C
      IF (ISMB(1).EQ.0)  GO TO 310
      CALL GMMATD (GAMRS(1), 12,12,1, AK(1), 12,12,0, D(1))
      CALL GMMATD (D(1), 12,12,0, GAMRS(1), 12,12,0, AK(1))
  310 IF (ISMB(2) .EQ.0)  GO TO 315
      CALL GMMATD (GAMRS(1), 12,12,1, AM(1), 12,12,0, D(1))
      CALL GMMATD (D(1), 12,12,0, GAMRS(1), 12,12,0, AM(1))
  315 CONTINUE
C
C
C LOCATE THE TRANSFORMATION MATRICES FROM BASIC TO LOCAL COORDINATES
C FOR THE TWO GRID POINTS AND EXPAND TO (6X6)
C THE TWO MATRICES WILL BE STORED IN D(1),...,D(36) AND D(37),...,D(72)
C RESPECTIVELY
C
      DO 320 I =1,72
  320 D(I) = 0.
      DO 350 I =1,2
      IF (ICS(I).EQ. 0) GO TO 350
      K = 36 * (I - 1)
      CALL TRANSD (ICS(1), D(73))
      DO 340 J=1,3
      KK = K + 6 * (J-1) + 1
      KL = 3 * (J-1) + 73
      KJ = K + 6 * (J+2) + J + 3
      D(KK  ) = D(KL  )
      D(KK+1) = D(KL+1)
      D(KK+2) = D(KL+2)
      D(KJ) = 1.
  340 CONTINUE
  350 CONTINUE
C
C  DIVIDE THE STIFFNESS MATRIX INTO 4 SUBMATRICES WHICH CAN THEN BE
C  TRANSFORMED FROM BASIC TO LOCAL COORDINATES THEN REINSERTED IN THE
C  STIFFNESS MATRIX IN INCREASING SIL ORDER
C
      DO 500 IP =1,2
      IPP = IP
      IF (IGP(1) .LT. IGP(2)) GO TO 400
      IPP = 3 - IP
  400 IR = 72 * (IPP-1)
      IAPP = 36* (IPP-1) +1
      DO 490 JI=1,2
      I= JI
      IF (IP .EQ. IPP ) GO TO 405
      I = 3-JI
C
C   PLACE THE APPROPRIATE SUBMATRIX INTO A (6X6) MATRIX
C
  405 IC =  6 *(I-1)
      K  = 0
      DO  410 II=1,6
      KL = IR + 12 *(II-1) +IC
      DO 410  IJ =1,6
      K = K+1
      KK = KL+ IJ
      AKI(K) = AK(KK)
      AKM(K) = AM(KK)
  410 CONTINUE
C
C   TRANSFORM FROM BASIC TO LOCAL  COORDINATES
C
      IF (ICS(IPP) .EQ. 0) GO TO 430
      IF (ISMB(1) .EQ. 0 ) GO TO 425
      CALL GMMATD (D(IAPP), 6,6,1, AKI(1), 6,6,0, D(73))
      DO  420 J=1,36
  420 AKI(J)= D(J+72)
  425 IF (ISMB(2) .EQ. 0) GO TO 430
      CALL GMMATD (D(IAPP), 6,6,1, AKM(1), 6,6,0, D(73))
      DO 428 J=1,36
  428 AKM(I) = D(J+72)
C
  430 IF (ICS(I) .EQ. 0) GO TO 450
      IAI =  36*(I-1) +1
      IF (ISMB(1) .EQ. 0) GO TO 445
      CALL GMMATD (AKI(1), 6,6,0, D(IAI), 6,6,0,  D(73))
      DO 440 J=1,36
  440 AKI(J) =  D(J+72)
  445 IF (ISMB(2) .EQ. 0) GO TO 450
      CALL GMMATD (AKM(1), 6,6,0, D(IAI), 6,6,0,  D(73))
      DO 448 J=1,36
  448 AKM(J) =D(J+72)
C
C    REINSERT INTO OVERALL STIFFNESS MATRIX ACCORDING TO INCREASING SIL
C
  450 DO 460 II = 1,6
      DO 460 JJ = 1,6
      KI = (II-1)*6 + JJ
      IOUT=  (IP-1)* 72 + (JI-1)*6  + (II-1)*12 + JJ
      KOUT(IOUT) = AKI(KI)
  460 MOUT(IOUT) = AKM(KI)
  490 CONTINUE
  500 CONTINUE
C
C     OUTPUT THE MATRIX BY EMGOUT
C
      DICT(2) = 1
      IF (ISMB(1) .EQ.0) GO TO 550
      CALL EMGOUT(KOUT,KOUT,144,1,DICT,1,IPR)
C
  550 IF (ISMB(2) .EQ.0) GO TO 600
      CALL EMGOUT(MOUT,MOUT,144,1,DICT,2,IPR)
C
  600 RETURN
C
C
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
 7770 CALL MESAGE (30,37,IDEL)
 7777 NOGO = .TRUE.
      RETURN
 7780 CALL MESAGE(30,26,IDEL)
      GO TO 7777
C
      END
