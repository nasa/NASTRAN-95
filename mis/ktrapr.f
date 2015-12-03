      SUBROUTINE KTRAPR
C
C     THIS ROUTINE COMPUTES THE STIFFNESS MATRIX FOR A AXI-SYMMETRIC
C     RING WITH A TRAPEZOIDAL CROSS SECTION
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
      DOUBLE PRECISION CONSTD,   DEGRAD,   D ,       GAMBQ,    R,
     1                 Z,        EE48,     TEO,      EE,       DELINT,
     2                 AK,       AKI,      R1,       R2,       R3,
     3                 R4,       Z1,       Z2,       Z3,       Z4,
     4                 ZMIN,     DGAMA,    ER,       ET,       EZ,
     5                 VRT,      VTR,      VTZ,      VZT,      VZR,
     6                 VRZ,      GRZ,      DEL,      COSG,     SING,
     7                 DGAMR,    AKT,      TWOPI,    DAMPC,    RMIN,
     8                 RMAX,     RZINTD
      DIMENSION        JRZ(2),   IECPT(24),AKI(36),  AKT(9)
      COMMON /SYSTEM/  IBUF,     IOUT
      COMMON /CONDAD/  CONSTD(5)
      COMMON /MSGX  /  NMSG,     MMSG,     MSG(4,1)
      COMMON /SMA1IO/  DUM1(10), IFKGG,    IGKGG,    IF4GG,    DUM2(21)
      COMMON /SMA1CL/  IOPT4,    K4GGSW,   NPVT,     DUM4(7),  LINK(10),
     1                 IDETCK,   DODET,    NOGO
      COMMON /SMA1ET/  ECPT(24), DUM5(76)
      COMMON /SMA1DP/  D(64),    GAMBQ(64), R(4),    Z(4),     TEO(16),
     1                 EE(16),   DELINT(12),AK(64),  DGAMA,    ZMIN,
     2                 ER,       ET,       EZ,       VRT,      VTR,
     3                 VTZ,      VZT,      VZR,      VRZ,      GRZ,
     4                 DEL,      COSG,     SING,     DGAMR,    IGP(4),
     5                 ICS(4),   SP(24),   TEMPE
      COMMON /MATIN /  MATIDC,   MATFLG,   ELTEMP,   STRESS,   SINTH,
     1                 COSTH
      COMMON /MATOUT/  E(3),     ANU(3),   RHO,      G(3),     ALF(3),
     1                 TZERO,    G SUB E
      EQUIVALENCE      (CONSTD(2),TWOPI),  (CONSTD(4),DEGRAD),
     1                 (IECPT(1) ,ECPT(1)),
     2                 (R(1),R1),(R(2),R2),(R(3),R3), (R(4),R4),
     3                 (Z(1),Z1),(Z(2),Z2),(Z(3),Z3), (Z(4),Z4),
     4                 (AKI(1),GAMBQ(1))  ,(AKT(1),GAMBQ(37))
      DATA    IRG   /  4HTRAP    /
C
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
      R(1)   = ECPT( 9)
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
      DGAMA  = ECPT( 6)
C
C     CHECK INTERNAL GRID POINTS FOR PIVOT POINT
C
      IPP = 0
      DO 100 I = 1,4
      IF (NPVT .EQ. IGP(I)) IPP = I
  100 CONTINUE
      IF (IPP .EQ. 0) CALL MESAGE (-30,34,IDEL)
C
C     TEST THE VALIDITY OF THE GRID POINT COORDINATES
C
      DO 200 I = 1,4
      IF (R(I) .LT. 0.0D0) GO TO 910
      IF (D(I) .NE. 0.0D0) GO TO 910
  200 CONTINUE
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = DMIN1(Z1,Z2,Z3,Z4)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
      Z4 = Z4 - ZMIN
C
C     FATAL IF RATIO OF RADII IS TO LARGE FOR GUASS QUADRATURE FOR
C     IP =-1
C
      RMIN = DMIN1(R1,R2,R3,R4)
      RMAX = DMAX1(R1,R2,R3,R4)
      IF (RMIN .EQ. 0.D0) GO TO 206
      IF (RMAX/RMIN .GT. 10.D0) GO TO 930
C
  206 IF (R1.GE.R2 .OR. R4.GE.R3 .OR. Z4.LE.Z1) GO TO 910
      IF (DABS(Z1-Z2) .GT. 1.0D-3) GO TO 910
      IF (DABS(Z3-Z4) .GT. 1.0D-3) GO TO 910
      D(5) = (R1+R4)/2.0D0
      D(6) = (R2+R3)/2.0D0
      IF (D(5) .EQ. 0.0D0) GO TO 210
      IF (DABS((R1-R4)/D(5)) .GT. 0.5D-2) GO TO 210
      R1 = D(5)
      R4 = D(5)
  210 CONTINUE
      IF (D(6) .EQ. 0.0D0) GO TO 220
      IF (DABS((R2-R3)/D(6)) .GT. 0.5D-2) GO TO 220
      R(2) = D(6)
      R(3) = D(6)
  220 CONTINUE
C
      ICORE = 0
      J = 1
      DO 230 I = 1,4
      IF (R(I) .NE. 0.D0) GO TO 230
      ICORE  = ICORE + 1
      JRZ(J) = I
      J = 2
  230 CONTINUE
      IF (ICORE.NE.0 .AND. ICORE.NE.2) GO TO 910
C
C     FORM THE TRANSFORMATION MATRIX (8X8) FROM FIELD COORDINATES TO
C     GRID POINT DEGREES OF FREEDOM
C
      DO 300 I = 1,64
      GAMBQ(I) = 0.0D0
  300 CONTINUE
      GAMBQ( 1) = 1.0D0
      GAMBQ( 2) = R1
      GAMBQ( 3) = Z1
      GAMBQ( 4) = R1*Z1
      GAMBQ(13) = 1.0D0
      GAMBQ(14) = R1
      GAMBQ(15) = Z1
      GAMBQ(16) = GAMBQ(4)
      GAMBQ(17) = 1.0D0
      GAMBQ(18) = R2
      GAMBQ(19) = Z2
      GAMBQ(20) = R2*Z2
      GAMBQ(29) = 1.0D0
      GAMBQ(30) = R2
      GAMBQ(31) = Z2
      GAMBQ(32) = GAMBQ(20)
      GAMBQ(33) = 1.0D0
      GAMBQ(34) = R3
      GAMBQ(35) = Z3
      GAMBQ(36) = R3*Z3
      GAMBQ(45) = 1.0D0
      GAMBQ(46) = R3
      GAMBQ(47) = Z3
      GAMBQ(48) = GAMBQ(36)
      GAMBQ(49) = 1.0D0
      GAMBQ(50) = R4
      GAMBQ(51) = Z4
      GAMBQ(52) = R4*Z4
      GAMBQ(61) = 1.0D0
      GAMBQ(62) = R4
      GAMBQ(63) = Z4
      GAMBQ(64) = GAMBQ(52)
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (8,GAMBQ(1),8,D(10),0,D(11),ISING,SP)
      IF (ISING.EQ.2) GO TO 920
C
C     MODIFY THE TRANSFORMATION MATRIX IF ELEMENT IS A CORE ELEMENT
C
      IF (ICORE .EQ. 0) GO TO 305
      JJ1 = 2*JRZ(1) - 1
      JJ2 = 2*JRZ(2) - 1
C
      DO 303 I = 1,8
      J = 8*(I-1)
      GAMBQ(I   ) = 0.0D0
      GAMBQ(I+16) = 0.0D0
      GAMBQ(J+JJ1)= 0.D0
      GAMBQ(J+JJ2)= 0.D0
  303 CONTINUE
  305 CONTINUE
C
C     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT WHERE THE ORDER IS
C     INDICATED BY THE FOLLOWING TABLE
C
C        DELINT( 1) - (-1,0)
C        DELINT( 2) - (-1,1)
C        DELINT( 3) - (-1,2)
C        DELINT( 4) - ( 0,0)
C        DELINT( 5) - ( 0,1)
C        DELINT( 6) - ( 0,2)
C        DELINT( 7) - ( 1,0)
C        DELINT( 8) - ( 1,1)
C        DELINT( 9) - ( 1,2)
C        DELINT(10) - ( 2,0)
C        DELINT(11) - ( 2,1)
C        DELINT(12) - ( 3,0)
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
      IF (I1 .GT. 3) GO TO 345
      DELINT(I1) = 0.0D0
      GO TO 350
  345 CONTINUE
      DELINT(I1) = RZINTD(IP,IQ,R,Z,4)
  350 CONTINUE
  400 CONTINUE
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3 TABLE
C
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      CALL MAT (IDEL)
C
C     SET MATERIAL PROPERTIES IN DOUBLE PRECISION VARIABLES
C
      ER  = E(1)
      ET  = E(2)
      EZ  = E(3)
      VRT = ANU(1)
      VTZ = ANU(2)
      VZR = ANU(3)
      GRZ = G(3)
      VTR = VRT*ET/ER
      VZT = VTZ*EZ/ET
      VRZ = VZR*ER/EZ
      DEL = 1.0D0 - VRT*VTR - VTZ*VZT - VZR*VRZ - VRT*VTZ*VZR
     1    - VRZ*VTR*VZT
C
C     GENERATE ELASTIC CONSTANTS MATRIX (4X4)
C
      EE(1) = ER*(1.0D0-VTZ*VZT)/DEL
      EE(2) = ER*(VTR + VZR*VTZ)/DEL
      EE(3) = ER*(VZR + VTR*VZT)/DEL
      EE(4) = 0.0D0
      EE(5) = EE(2)
      EE(6) = ET*(1.0D0-VRZ*VZR)/DEL
      EE(7) = ET*(VZT+VRT*VZR)/ DEL
      EE(8) = 0.0D0
      EE(9) = EE(3)
      EE(10)= EE(7)
      EE(11)= EZ*(1.0D0-VRT*VTR)/DEL
      EE(12)= 0.0D0
      EE(13)= 0.0D0
      EE(14)= 0.0D0
      EE(15)= 0.0D0
      EE(16)= GRZ
C
C     FORM TRANSFORMATION MATRIX (4X4) FROM MATERIAL AXIS TO ELEMENT
C     GEOMETRIC AXIS
C
      DGAMR   = DGAMA*DEGRAD
      COSG    = DCOS(DGAMR)
      SING    = DSIN(DGAMR)
      TEO( 1) = COSG**2
      TEO( 2) = 0.0D0
      TEO( 3) = SING**2
      TEO( 4) = SING*COSG
      TEO( 5) = 0.0D0
      TEO( 6) = 1.0D0
      TEO( 7) = 0.0D0
      TEO( 8) = 0.0D0
      TEO( 9) = TEO(3)
      TEO(10) = 0.0D0
      TEO(11) = TEO(1)
      TEO(12) =-TEO(4)
      TEO(13) =-2.0D0*TEO(4)
      TEO(14) = 0.0D0
      TEO(15) =-TEO(13)
      TEO(16) = TEO(1) - TEO(3)
C
C     TRANSFORM THE ELASTIC CONSTANTS MATRIX FROM MATERIAL
C     TO ELEMENT GEOMETRIC AXIS
C
      CALL GMMATD (TEO,4,4,1, EE, 4,4,0, D )
      CALL GMMATD (D  ,4,4,0, TEO,4,4,0, EE)
C
C     FORM THE ELEMENT STIFFNESS MATRIX IN FIELD COORDINATES
C
      EE48   = EE(4) + EE(8)
      D ( 1) = EE(1) + 2.0D0*EE(2) + EE(6)
      AK( 1) = EE(6)*DELINT(1)
      AK( 2) = (EE(2) + EE(6))*DELINT(4)
      AK( 3) = EE(6)*DELINT(2) + EE(8)*DELINT(4)
      AK( 4) = (EE(2) + EE(6))*DELINT(5) + EE(8)*DELINT(7)
      AK( 5) = 0.0D0
      AK( 6) = EE(8)*DELINT(4)
      AK( 7) = EE(7)*DELINT(4)
      AK( 8) = EE(7)*DELINT(7) + EE(8)*DELINT(5)
      AK( 9) = AK(2)
      AK(10) = D(1)*DELINT(7)
      AK(11) = (EE(2) + EE(6))*DELINT(5) + EE48*DELINT(7)
      AK(12) = D(1)*DELINT(8) + EE48*DELINT(10)
      AK(13) = 0.0D0
      AK(14) = EE48*DELINT(7)
      AK(15) = (EE(3)+EE(7))*DELINT(7)
      AK(16) = (EE(3)+EE(7))*DELINT(10) + EE48*DELINT(8)
      AK(17) = AK( 3)
      AK(18) = AK(11)
      AK(19) = EE(6)*DELINT(3) + EE(16)*DELINT(7) + (EE(8) +
     1         EE(14))*DELINT(5)
      AK(20) = (EE(2) + EE(6))*DELINT(6) + EE(16)*DELINT(10) + (EE(8) +
     1          EE(13) + EE(14))*DELINT(8)
      AK(21) = 0.0D0
      AK(22) = EE(16)*DELINT(7) + EE(8)*DELINT(5)
      AK(23) = EE( 7)*DELINT(5) + EE(15)*DELINT(7)
      AK(24) = (EE(7) + EE(16))*DELINT(8) + EE(8)*DELINT(6) +
     1         EE(15)*DELINT(10)
      AK(25) = AK( 4)
      AK(26) = AK(12)
      AK(27) = AK(20)
      AK(28) = D(1)*DELINT(9) + EE(16)*DELINT(12) + (EE48 + EE(13) +
     1         EE(14))*DELINT(11)
      AK(29) = 0.0D0
      AK(30) = EE(16)*DELINT(10) + EE48*DELINT(8)
      AK(31) = (EE(3) + EE(7))*DELINT(8) + EE(15)*DELINT(10)
      AK(32) = (EE(3) + EE(7) + EE(16))*DELINT(11) + EE(15)*DELINT(12) +
     1         EE48*DELINT(9)
      AK(33) = 0.0D0
      AK(34) = 0.0D0
      AK(35) = 0.0D0
      AK(36) = 0.0D0
      AK(37) = 0.0D0
      AK(38) = 0.0D0
      AK(39) = 0.0D0
      AK(40) = 0.0D0
      AK(41) = AK( 6)
      AK(42) = AK(14)
      AK(43) = AK(22)
      AK(44) = AK(30)
      AK(45) = 0.0D0
      AK(46) = EE(16)*DELINT(7)
      AK(47) = EE(15)*DELINT(7)
      AK(48) = EE(16)*DELINT(8) + EE(15)*DELINT(10)
      AK(49) = AK( 7)
      AK(50) = AK(15)
      AK(51) = AK(23)
      AK(52) = AK(31)
      AK(53) = 0.0D0
      AK(54) = AK(47)
      AK(55) = EE(11)*DELINT( 7)
      AK(56) = EE(11)*DELINT(10) + EE(12)*DELINT(8)
      AK(57) = AK( 8)
      AK(58) = AK(16)
      AK(59) = AK(24)
      AK(60) = AK(32)
      AK(61) = 0.0D0
      AK(62) = AK(48)
      AK(63) = AK(56)
      AK(64) = EE(11)*DELINT(12) + EE(16)*DELINT(9) + (EE(12) +
     1         EE(15))*DELINT(11)
C
      DO 600 I = 1,64
      AK(I) = TWOPI*AK(I)
  600 CONTINUE
C
C     TRANSFORM THE ELEMENT STIFFNESS MATRIX FROM FIELD COORDINATES
C     TO GRID POINT DEGREES OF FREEDOM
C
      CALL GMMATD (GAMBQ,8,8,1, AK,8,8,0, D)
      CALL GMMATD (D,8,8,0, GAMBQ,8,8,0, AK)
C
C     ZERO OUT THE (6X6) MATRIX USED AS INPUT TO THE INSERTION ROUTINE
C
      DO 700 I = 1,36
      AKI(I) = 0.0D0
  700 CONTINUE
C
C     LOCATE THE TRANSFORMATION MATRICES FOR THE FOUR  GRID POINTS
C
      DO 800 I = 1,4
      IF (ICS(I) .EQ. 0) GO TO 800
      K = 9*(I-1) + 1
      CALL TRANSD (ICS(I),D(K))
  800 CONTINUE
C
C     START THE LOOP FOR INSERTION OF THE FOUR  (6X6) MATRICES
C     INTO THE MASTER STIFFNESS MATRIX
C
      IR1  = 2*IPP - 1
      IAPP = 9*(IPP-1) + 1
      DO 900 I = 1,4
C
C     PLACE THE APPROIATE (2X2) SUBMATRIX OF THE STIFFNESS MATRIX
C     IN A (3X3) MATRIX FOR TRANSFORMATION
C
      IC1    = 2*I - 1
      IRC    = (IR1-1)*8 + IC1
      AKT(1) = AK(IRC)
      AKT(2) = 0.0D0
      AKT(3) = AK(IRC+1)
      AKT(4) = 0.0D0
      AKT(5) = 0.0D0
      AKT(6) = 0.0D0
      AKT(7) = AK(IRC+8)
      AKT(8) = 0.0D0
      AKT(9) = AK(IRC+9)
C
C     TRANSFORM THE (3X3) STIFFNESS MATRIX
C
      IF (ICS(IPP) .EQ. 0) GO TO 820
      CALL GMMATD (D(IAPP),3,3,1, AKT(1),3,3,0, D(37))
      DO 810 J = 1,9
      AKT(J) = D(J+36)
  810 CONTINUE
  820 CONTINUE
      IF (ICS(I) .EQ. 0) GO TO 840
      IAI = 9*(I-1) + 1
      CALL GMMATD (AKT(1),3,3,0, D(IAI),3,3,0, D(37))
      DO 830 J = 1,9
      AKT(J) = D(J+36)
  830 CONTINUE
  840 CONTINUE
C
C     PLACE THE TRANSFORMED (3X3) MATRIX INTO A (6X6) MATRIX FOR
C     THE INSERTION ROUTINE
C
      J = 0
      DO 850 J1 = 1,18,6
      DO 850 J2 = 1,3
      J = J + 1
      K = J1 + J2 - 1
      AKI(K) = AKT(J)
  850 CONTINUE
C
C     CALL THE INSERTION ROUTINE
C
      CALL SMA1B (AKI(1),IGP(I),-1,IFKGG,0.0D0)
      IF (IOPT4.EQ.0 .OR. GSUBE.EQ.0.0) GO TO 900
      K4GGSW = 1
      DAMPC  = GSUBE
      CALL SMA1B (AKI(1),IGP(I),-1,IF4GG,DAMPC)
  900 CONTINUE
      RETURN
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
  910 I = 37
      GO TO 950
  920 I = 26
      GO TO 950
  930 I = 221
C ...     221 WILL PRINT USER MESSAGE 2218
C
  950 IF (NMSG .EQ. 0) GO TO 970
      IF (NMSG .GE. MMSG) RETURN
      DO 960 J = 1,NMSG
      IF (MSG(3,J).EQ.IDEL .AND. MSG(2,J).EQ.I) RETURN
  960 CONTINUE
  970 ICS(1) = IDEL
      ICS(2) = IRG
      CALL MESAGE (30,I,ICS)
      NOGO = 1
      RETURN
C
      END
