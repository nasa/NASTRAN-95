      SUBROUTINE MTRIRG
C
C
C*****
C THIS ROUTINE COMPUTES THE   MASS    MATRIX FOR A AXI-SYMMETRIC RING
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
      DOUBLE PRECISION CONSTD, D2PI
      DOUBLE PRECISION
     1                   D ,  GAMBQ,    R,   Z
     2,                            DELINT,   AK,  AKI
     3,                  AKT,  AM
      DOUBLE PRECISION   R1,  R2,  R3,  Z1,  Z2,  Z3,  ZMIN, DGAMA
     1,                  DR,  RH,  DZ,  ZH,  RA,  ZA,  AREA
     2,                  TWOPI,    RHOD,     DKI
C
      DIMENSION          IECPT(19)
      DIMENSION  AM(36)
C
      COMMON /CONDAD/ CONSTD(5)
      COMMON   /SMA2IO/
     1                   DUM1(10)
     2,                  IFMGG
     3,                  DUM2(25)
      COMMON   /SMA2CL/
     1                   DUM3(2)
     2,                  NPVT
     3,                  DUM4(7)
     4,                  LINK(10)           ,NOGO
      COMMON   /SMA2ET/
     1                   ECPT(19)
     2,                  DUM5(81)
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
      COMMON   /MATOUT/
     1                   E(3)               ,ANU(3)
     2,                  RHO                ,G(3)
     3,                  ALF(3)             ,TZERO
      COMMON   /SMA2DP/
     1                   D(36) ,   GAMBQ(36),     R(3) ,    Z(3)
     2,                                      DELINT(8),     AK(36)
     3,                  AKI(36),  AKT(9)
     4,                  DGAMA,    ZMIN,     RHOD,     TWOPI
     5,                  DR,  RH,  DZ,  ZH,  RA,  ZA,  AREA
     8,                  IGP(3) , ICS(3) , SP(18)
     9,                  TEMPE
C
      EQUIVALENCE        (IECPT(1) , ECPT(1))
      EQUIVALENCE   (R(1),R1),     (R(2),R2),     (R(3),R3)
     1,             (Z(1),Z1),     (Z(2),Z2),     (Z(3),Z3)
      EQUIVALENCE  (AM(1) , AK(1))
      EQUIVALENCE ( CONSTD(2) , D2PI   )
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
C
C
C COMPUTE THE ELEMENT COORDINATES
C
      IF (IEROR1.EQ.0.AND.IEROR2.EQ.0) GO TO 220
      NOGO = 2
      RETURN
  220 ZMIN = DMIN1 (Z1, Z2, Z3)
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
C     NONEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
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
C              DELINT( 1) - ( 1,0)
C              DELINT( 2) - ( 1,1)
C              DELINT( 3) - ( 1,2)
C              DELINT( 4) - ( 2,0)
C              DELINT( 5) - ( 2,1)
C              DELINT( 6) - ( 0,2)
C              DELINT( 7) - ( 3,0)
C              DELINT( 8) - (-1,2)
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
      IP = I
      DO 350 J = 1,3
      IQ = J - 1
      IF (IP.EQ.2 .AND. IQ.EQ.2) IP = 0
      IF (IP.EQ.3 .AND. IQ.EQ.2) IP = -1
      IF (IP.EQ.3 .AND. IQ.EQ.1) GO TO 350
      I1 = I1 + 1
      IF (KODE) 320,330,340
  320 DELINT(I1) =((RA) ** IP)*((ZA) ** IQ) * AREA
      GO TO 350
  330 DELINT(I1) = DKI(1,3,1,2,1,3,IP,IQ,R,Z)
     1           + DKI(3,2,1,2,3,2,IP,IQ,R,Z)
      GO TO 350
  340 CONTINUE
      DELINT(I1) = DKI(1,3,3,2,1,3,IP,IQ,R,Z)
  350 CONTINUE
  400 CONTINUE
C
C
C TEST FOR EXCESSIVE ROUND-OFF ERROR IN INTEGRAL CALCULATIONS
C AND IF IT EXIST APPROXIMATE INTEGRALS
C
      IF (KODE .LT. 0) GO TO 500
      DO 450 I = 1,8
      IF (DELINT(I) .LT. 0.0D0) GO TO 475
  450 CONTINUE
      IF (DELINT(3) .LE. DELINT(6)) GO TO 475
      IF (DELINT(8) .GE. DELINT(3)) GO TO 475
      IF (DELINT(8) .GT. DELINT(6)) GO TO 475
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
      RHOD = RHO
C
C GENERATE THE CONSISTENT MASS MATRIX IN FIELD COORDINATES
C
      DO 600 I = 1,36
      AM(I) = 0.0D0
  600 CONTINUE
      TWOPI = D2PI * RHOD
      AM( 1)= TWOPI *       DELINT(1)
      AM( 2)= TWOPI *       DELINT(4)
      AM( 3)= TWOPI *       DELINT(2)
      AM( 7)= AM( 2)
      AM( 8)= TWOPI *       DELINT(7)
      AM( 9)= TWOPI *       DELINT(5)
      AM(13)= AM( 3)
      AM(14)= AM( 9)
      AM(15)= TWOPI *       DELINT(3)
      AM(22)= AM( 1)
      AM(23)= AM( 2)
      AM(24)= AM( 3)
      AM(28)= AM(23)
      AM(29)= AM( 8)
      AM(30)= AM( 9)
      AM(34)= AM(24)
      AM(35)= AM(30)
      AM(36)= AM(15)
C
C TRANSFORM THE ELEMENT   MASS    MATRIX FROM FIELD COORDINATES
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
C INTO THE MASTER MASS MATRIX
C
      IR1  = 2 * IPP - 1
      IAPP = 9 * (IPP-1) + 1
      DO 900 I = 1,3
C
C PLACE THE APPROIATE (2X2) SUBMATRIX OF THE MASS MATRIX
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
C TRANSFORM THE (3X3) MASS MATRIX
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
      CALL SMA2B (AKI(1) , IGP(I), -1, IFMGG, 0.0D0)
  900 CONTINUE
      RETURN
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
  915 NOGO=1
      RETURN
  920 CALL MESAGE(30,37,IDEL)
      GO TO 915
C
      END
