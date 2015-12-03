      SUBROUTINE KTRIRG
C
C
C*****
C THIS ROUTINE COMPUTES THE STIFFNESS MATRIX FOR A AXI-SYMMETRIC RING
C WITH A TRIANGULAR CROSS SECTION
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
      DOUBLE PRECISION CONSTD, DEGRAD
      DOUBLE PRECISION
     1                   D ,  GAMBQ,    R,   Z
     2,                  TEO, EE,  DELINT,   AK,  AKI
     3,                  AKT
      DOUBLE PRECISION   R1,  R2,  R3,  Z1,  Z2,  Z3,  ZMIN, DGAMA
     1,                  DR,  RH,  DZ,  ZH,  RA,  ZA,  AREA
     2,                  ER,  ET,  EZ,  VRT, VTR, VTZ, VZT
     3,                  VZR, VRZ, GRZ, DEL, COSG,SING,DGAMR
     4,                  TWOPI,    DKI
      DOUBLE PRECISION   DAMPC
C
      DIMENSION          IECPT(19)
      DIMENSION          AKI(36),  AKT(9)
C
      COMMON /CONDAD/ CONSTD(5)
      COMMON   /SMA1IO/
     1                   DUM1(10)
     2,                  IFKGG
     3,                  IGKGG,    IF4GG,    DUM2(21)
      COMMON   /SMA1CL/
     1                   IOPT4,    K4GGSW
     2,                  NPVT
     3,                   DUM4(7)
     4,                  LINK(10)           ,IDETCK
     5,                  DODET              ,NOGO
      COMMON   /SMA1ET/
     1                   ECPT(19)
     2,                  DUM5(81)
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
      COMMON   /MATOUT/
     1                   E(3)               ,ANU(3)
     2,                  RHO                ,G(3)
     3,                  ALF(3)        ,TZERO,    GSUBE
      COMMON   /SMA1DP/
     1                   D(36) ,   GAMBQ(36),     R(3) ,    Z(3)
     2,                  TEO(16),  EE(16),   DELINT(8),     AK(36)
     4,                  DGAMA,    ZMIN
     5,                  DR,  RH,  DZ,  ZH,  RA,  ZA,  AREA
     6,                  ER,  ET,  EZ,  VRT, VTR, VTZ, VZT
     7,                  VZR, VRZ, GRZ, DEL, COSG,SING,DGAMR
     8,                  IGP(3) , ICS(3) , SP(18)
     9,                  TEMPE
C
      EQUIVALENCE ( CONSTD(2) , TWOPI  )
      EQUIVALENCE ( CONSTD(4) , DEGRAD )
      EQUIVALENCE        (IECPT(1) , ECPT(1))
      EQUIVALENCE   (R(1),R1),     (R(2),R2),     (R(3),R3)
     1,             (Z(1),Z1),     (Z(2),Z2),     (Z(3),Z3)
      EQUIVALENCE        (AKI(1),  GAMBQ(1))
      EQUIVALENCE        (AKT(1),  TEO(1))
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
C CHECK INTERNAL GRID POINTS FOR PIVOT POINT
C
      IPP = 0
      DO 100 I = 1,3
      IF (NPVT .EQ. IGP(I)) IPP = I
  100 CONTINUE
      IF (IPP .EQ. 0) CALL MESAGE (-30,34,IDEL)
C
C
C TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      IEROR1 = 0
      DO 200 I = 1,3
      IF (R(I).GT.0.0D0) GO TO 200
      IF (IEROR1.NE.0) GO TO 200
      CALL MESAGE (30, 211, IDEL)
      IEROR1 = 1
  200 CONTINUE
      IEROR2 = 0
      DO 210 I = 1, 3
      IF (D(I).EQ.0.0D0) GO TO 210
      IF (IEROR2.NE.0) GO TO 210
      CALL MESAGE (30, 212, IDEL)
      IEROR2 = 1
  210 CONTINUE
      IF (IEROR1.EQ.0.AND.IEROR2.EQ.0) GO TO 220
      NOGO = 2
      RETURN
  220 IF ((R2 - R1)*(Z3 - Z1) - (R3 - R1)*(Z2 - Z1).LT.0.0D0) GO TO 920
C
C
C COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = DMIN1(Z1, Z2, Z3)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
C
C
C
C FORM THE TRANSFORMATION MATRIX (6X6) FROM FIELD COORDINATES TO GRID
C POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,36
      GAMBQ(I) = 0.0D0
  300 CONTINUE
      GAMBQ( 1) = 1.0D0
      GAMBQ( 2) = R1
      GAMBQ( 3) = Z1
      GAMBQ(10) = 1.0D0
      GAMBQ(11) = R1
      GAMBQ(12) = Z1
      GAMBQ(13) = 1.0D0
      GAMBQ(14) = R2
      GAMBQ(15) = Z2
      GAMBQ(22) = 1.0D0
      GAMBQ(23) = R2
      GAMBQ(24) = Z2
      GAMBQ(25) = 1.0D0
      GAMBQ(26) = R3
      GAMBQ(27) = Z3
      GAMBQ(34) = 1.0D0
      GAMBQ(35) = R3
      GAMBQ(36) = Z3
C
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERD (6, GAMBQ(1),6 , D(10), 0, D(11) , ISING , SP)
C
      IF (ISING.EQ.2) GO TO 920
C
C
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
      DR = DMAX1 ( DABS(R1-R2) , DABS(R2-R3) , DABS(R3-R1) )
      RH = DMIN1 ( R1 , R2 , R3 ) / 10.0D0
      DZ = DMAX1 ( DABS(Z1-Z2) , DABS(Z2-Z3) , DABS(Z3-Z1) )
      ZH = DMIN1 ( Z1 , Z2 , Z3 ) / 10.0D0
      RA = (R1 + R2 + R3) / 3.0D0
      ZA = (Z1 + Z2 + Z3) / 3.0D0
      AREA =(R1*(Z2-Z3) + R2*(Z3-Z1) + R3*(Z1-Z2)) / 2.0D0
      KODE = 0
      IF (DABS( (R2-R1)/R2 ) .LT. 1.0D-5) KODE = 1
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
  330 DELINT(I1) =   DKI (1,3,1,2,1,3,IP,IQ,R,Z)
     1            +  DKI (3,2,1,2,3,2,IP,IQ,R,Z)
      GO TO 350
  340 CONTINUE
      DELINT(I1) =   DKI (1,3,3,2,1,3,IP,IQ,R,Z)
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
      IF (DELINT(I) .LT. 0.0D0) GO TO 475
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
C SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
C
      ER = E(1)
      ET = E(2)
      EZ = E(3)
      VRT = ANU(1)
      VTZ = ANU(2)
      VZR = ANU(3)
      GRZ = G(3)
      VTR = VRT * ET / ER
      VZT = VTZ * EZ / ET
      VRZ = VZR * ER / EZ
      DEL = 1.0D0 - VRT*VTR - VTZ*VZT - VZR*VRZ - VRT*VTZ*VZR
     1       - VRZ*VTR*VZT
C
C
C GENERATE ELASTIC CONSTANTS MATRIX (4X4)
C
      EE(1) = ER * (1.0D0 - VTZ*VZT) / DEL
      EE(2) = ER * (VTR + VZR*VTZ) / DEL
      EE(3) = ER * (VZR + VTR*VZT) / DEL
      EE(4) = 0.0D0
      EE(5) = EE(2)
      EE(6) = ET * (1.0D0 - VRZ*VZR) / DEL
      EE(7) = ET * (VZT + VRT*VZR) / DEL
      EE(8) = 0.0D0
      EE(9) = EE(3)
      EE(10)= EE(7)
      EE(11)= EZ * (1.0D0 - VRT*VTR) / DEL
      EE(12)= 0.0D0
      EE(13)= 0.0D0
      EE(14)= 0.0D0
      EE(15)= 0.0D0
      EE(16)= GRZ
C
C
C FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
C GEOMETRIC AXIS
C
      DGAMR = DGAMA * DEGRAD
      COSG = DCOS(DGAMR)
      SING = DSIN(DGAMR)
      TEO( 1) = COSG ** 2
      TEO( 2) = 0.0D0
      TEO( 3) = SING ** 2
      TEO( 4) = SING * COSG
      TEO( 5) = 0.0D0
      TEO( 6) = 1.0D0
      TEO( 7) = 0.0D0
      TEO( 8) = 0.0D0
      TEO( 9) = TEO(3)
      TEO(10) = 0.0D0
      TEO(11) = TEO(1)
      TEO(12) = -TEO(4)
      TEO(13) = -2.0D0 * TEO(4)
      TEO(14) = 0.0D0
      TEO(15) = -TEO(13)
      TEO(16) = TEO(1) - TEO(3)
C
C
C TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
C TO ELEMENT GEOMETRIC AXIS
C
      CALL GMMATD (TEO , 4, 4, 1, EE , 4, 4, 0, D )
      CALL GMMATD (D   , 4, 4, 0, TEO, 4, 4, 0, EE)
C
C
C
C FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
C
      AK( 1) = EE(6) * DELINT(1)
      AK( 2) = (EE(2) + EE(6)) * DELINT(4)
      AK( 3) = EE(6) * DELINT(2) + EE(8) * DELINT(4)
      AK( 4) = 0.0D0
      AK( 5) = EE(8) * DELINT(4)
      AK( 6) = EE(7) * DELINT(4)
      AK( 7) = AK(2)
      AK( 8) = (EE(1) + 2.0D0*EE(2) + EE(6)) * DELINT(6)
      AK( 9) = (EE(2) + EE(6)) * DELINT(5) + (EE(4) + EE(8)) *DELINT(6)
      AK(10) = 0.0D0
      AK(11) = (EE(4) + EE(8)) * DELINT(6)
      AK(12) = (EE(3) + EE(7)) * DELINT(6)
      AK(13) = AK(3)
      AK(14) = AK(9)
      AK(15) = EE(6) * DELINT(3) + 2.0D0*EE(8) * DELINT(5)
     1         + EE(16) * DELINT(6)
      AK(16) = 0.0D0
      AK(17) = EE(8) * DELINT(5) + EE(16) * DELINT(6)
      AK(18) = EE(7) * DELINT(5) + EE(12) * DELINT(6)
      AK(19) = 0.0D0
      AK(20) = 0.0D0
      AK(21) = 0.0D0
      AK(22) = 0.0D0
      AK(23) = 0.0D0
      AK(24) = 0.0D0
      AK(25) = AK(5)
      AK(26) = AK(11)
      AK(27) = AK(17)
      AK(28) = 0.0D0
      AK(29) = EE(16) * DELINT(6)
      AK(30) = EE(12) * DELINT(6)
      AK(31) = AK(6)
      AK(32) = AK(12)
      AK(33) = AK(18)
      AK(34) = 0.0D0
      AK(35) = AK(30)
      AK(36) = EE(11) * DELINT(6)
C
      DO 600 I = 1,36
      AK(I) = TWOPI * AK(I)
  600 CONTINUE
C
C TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
C TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATD (GAMBQ , 6, 6, 1, AK , 6, 6, 0, D )
      CALL GMMATD (D  , 6, 6, 0, GAMBQ , 6, 6, 0, AK)
C
C
C
C ZERO OUT THE (6X6) MATRIX USED AS INPUT TO THE INSERTION ROUTINE
C
      DO 700 I = 1,36
      AKI(I) = 0.0D0
  700 CONTINUE
C
C
C LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
C
      DO 800 I = 1,3
      IF (ICS(I) .EQ. 0) GO TO 800
      K = 9 * (I-1) + 1
      CALL TRANSD (ICS(I) , D(K))
  800 CONTINUE
C
C
C
C START THE LOOP FOR INSERTION OF THE THREE (6X6) MATRICES
C INTO THE MASTER STIFFNESS MATRIX
C
      IR1  = 2 * IPP - 1
      IAPP = 9 * (IPP-1) + 1
      DO 900 I = 1,3
C
C PLACE THE APPROIATE (2X2) SUBMATRIX OF THE STIFFNESS MATRIX
C IN A (3X3) MATRIX FOR TRANSFORMATION
C
      IC1 = 2 * I - 1
      IRC = (IR1 - 1) * 6 + IC1
      AKT(1) = AK(IRC)
      AKT(2) = 0.0D0
      AKT(3) = AK(IRC+1)
      AKT(4) = 0.0D0
      AKT(5) = 0.0D0
      AKT(6) = 0.0D0
      AKT(7) = AK(IRC+6)
      AKT(8) = 0.0D0
      AKT(9) = AK(IRC+7)
C
C TRANSFORM THE (3X3) STIFFNESS MATRIX
C
      IF (ICS(IPP) .EQ. 0) GO TO 820
      CALL GMMATD (D(IAPP) , 3, 3, 1, AKT(1) , 3, 3, 0, D(28) )
      DO 810 J = 1,9
      AKT(J) = D(J+27)
  810 CONTINUE
  820 CONTINUE
      IF (ICS(I) .EQ. 0) GO TO 840
      IAI = 9 * (I - 1) + 1
      CALL GMMATD (AKT(1) , 3, 3, 0, D(IAI) , 3, 3, 0, D(28) )
      DO 830 J = 1,9
      AKT(J) = D(J+27)
  830 CONTINUE
  840 CONTINUE
C
C PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR
C THE INSERTION ROUTINE
C
      J = 0
      DO 850 J1 = 1,18,6
      DO 850 J2 = 1,3
      J = J + 1
      K = J1 + J2 - 1
      AKI(K) = AKT(J)
  850 CONTINUE
C
C CALL THE INSERTION ROUTINE
C
      CALL SMA1B (AKI(1) , IGP(I), -1, IFKGG, 0.0D0)
      IF (IOPT4 .EQ. 0 .OR. GSUBE .EQ. 0.0) GO TO 900
      K4GGSW = 1
      DAMPC = GSUBE
      CALL SMA1B (AKI(1) , IGP(I) , -1,IF4GG , DAMPC )
  900 CONTINUE
      RETURN
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
  915 NOGO=1
      RETURN
  920 CALL MESAGE(30,26,IDEL)
      GO TO 915
C
      END
