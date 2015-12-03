      SUBROUTINE TPZTEM (TI,PG)
C
C     THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE AXI-SYMMETRIC
C     TRAPEZOIDAL CROSS SECTION RING
C
C     ECPT COMMON BLOCK IS,
C
C     ECPT( 1) = ELEMENT ID                                I
C     ECPT( 2) = SIL A                                     I
C     ECPT( 3) = SIL B                                     I
C     ECPT( 4) = SIL C                                     I
C     ECPT( 5) = SIL D
C     ECPT( 6) = MATERIAL ORIENTATION ANGLE(DEGREES)       R
C     ECPT( 8) = MATERIAL ID                               I
C     ECPT( 9) TO ECPT (22) FOR PHI
C     ECPT(23) = COOR. SYS. FOR GRID POINT A               I
C     ECPT(24) = X-COOR. OF GRID POINT A (IN BASIC COOR)   R
C     ECPT(25) = Z-COOR. OF GRID POINT A (IN BASIC COOR)   R
C     ECPT(26) = 0.0
C     ECPT(27) = COOR. SYS. FOR GRID POINT B
C     ECPT(28) = X-COOR. OF GRID POINT B (IN BASIC COOR)   R
C     ECPT(29) = Z-COOR. OF GRID POINT B (IN BASIC COOR)   R
C     ECPT(30) = 0.0
C     ECPT(31) = COOR. SYS. FOR GRID POINT C               I
C     ECPT(32) = X-COOR. FOR GRID POINT C                  R
C     ECPT(33) = Z-COOR. FOR GRID POINT C                  R
C     ECPT(34) = 0.0
C     ECPT(35) = COOR. SYS. FOR GRID POINT D               I
C     ECPT(36) = X-COOR FOR GRID POINT D                   R
C     ECPT(37) = Z-COOR FOR GRID POINT D                   R
C     ECPT(38) = 0.0
C     ECPT(39) = EL. TEMPERATURE FOR MATERIAL PROP         R
C
      INTEGER         SP(36)
      DIMENSION       TI(4),PG(1),R(4),Z(4),GABABQ(12,12),DELINT(15),
     1                D(144),TEO(21),HTN(12,4),IGP(4),IECPT(39),ICS(4),
     2                H(4,4),AKI(144),TL(12)
      COMMON /TRIMEX/ ECPT(39)
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E(3),ANU(3),RHO,G(3),ALF(3),TZERO,GSUBE,MOSKP(9),
     1                SETMAT
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SYSTEM/ IBUF,IOUT
      EQUIVALENCE     (IECPT(1),ECPT(1)),(Z(1),Z1),(Z(2),Z2),
     1                (Z(3),Z3),(R(1),R1),(R(2),R2),(R(3),R3),
     2                (R(4),R4),(Z(4),Z4),(GABABQ(1,1),AKI(1)),
     3                (CONSTS(1),PI),(CONSTS(4),DEGRAD)
      DATA    IDEL2 , JAX / 0, 4HAX    /
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
      ICS(3) = IECPT(31)
      ICS(2) = IECPT(27)
      R(1)   = ECPT (24)
      R(2)   = ECPT (28)
      R(3)   = ECPT (32)
      ICS(4) = IECPT(35)
      Z(1)   = ECPT (25)
      D(1)   = ECPT (26)
      Z(2)   = ECPT (29)
      D(2)   = ECPT (30)
      Z(3)   = ECPT (33)
      D(3)   = ECPT (34)
      Z(4)   = ECPT (37)
      D(4)   = ECPT (38)
      R(4)   = ECPT (36)
      TEMPE  = ECPT (39)
      DGAMA  = ECPT ( 6)
      IDEL1  = IDEL/1000
C
C     COMPUTE THE ELEMENT COORDINATES
C
      ZMIN = AMIN1(Z1,Z2,Z3,Z4)
      Z1   = Z1 - ZMIN
      Z2   = Z2 - ZMIN
      Z3   = Z3 - ZMIN
      Z4   = Z4 - ZMIN
C
C     FATAL IF RATIO OF RADII IS TOO LARGE FOR GUASS QUADRATURE
C
      RMIN = AMIN1(R1,R2,R3,R4)
      RMAX = AMAX1(R1,R2,R3,R4)
      IF (RMIN .EQ. 0.0) GO TO 206
      IF (RMAX/RMIN .GT. 10.) GO TO 915
C
  206 CONTINUE
      IF (ABS(Z1-Z2) .GT. .001) GO TO 910
      IF (ABS(Z3-Z4) .GT. .001) GO TO 910
      D(5) = (R1+R4)/2.0
      D(6) = (R2+R3)/2.0
      IF (D(5) .EQ. 0.0) GO TO 210
      IF (ABS((R1-R4)/D(5)) .GT. .005) GO TO 210
      R1 = D(5)
      R4 = D(5)
  210 CONTINUE
      IF (D(6) .EQ. 0.0) GO TO 220
      IF (ABS((R2-R3)/D(6)) .GT. .005) GO TO 220
      R2 = D(6)
      R3 = D(6)
  220 CONTINUE
C
C     FORM THE TRANSFORMMATION MATRIX(12X12) FROM FIELD COOR, TO GRID
C     POINT DEGREES OF FREEDOM
C
      DO 300 I = 1, 144
  300 GABABQ( I, 1) = 0.000
      GABABQ( 1, 1) = 1.000
      GABABQ( 2, 1) = R1
      GABABQ( 3, 1) = Z1
      GABABQ( 4, 1) = R1*Z1
      GABABQ( 5, 2) = 1.000
      GABABQ( 6, 2) = R1
      GABABQ( 7, 2) = Z1
      GABABQ( 8, 2) = GABABQ(4,1)
      GABABQ( 9, 3) = 1.000
      GABABQ(10, 3) = R1
      GABABQ(11, 3) = Z1
      GABABQ(12, 3) = GABABQ(4,1)
      GABABQ( 1, 4) = 1.000
      GABABQ( 2, 4) = R2
      GABABQ( 3, 4) = Z2
      GABABQ( 4, 4) = R2*Z2
      GABABQ( 5, 5) = 1.000
      GABABQ( 6, 5) = R2
      GABABQ( 7, 5) = Z2
      GABABQ( 8, 5) = GABABQ(4,4)
      GABABQ( 9, 6) = 1.000
      GABABQ(10, 6) = R2
      GABABQ(11, 6) = Z2
      GABABQ(12, 6) = GABABQ(4,4)
      GABABQ( 1, 7) = 1.000
      GABABQ( 2, 7) = R3
      GABABQ( 3, 7) = Z3
      GABABQ( 4, 7) = R3*Z3
      GABABQ( 5, 8) = 1.000
      GABABQ( 6, 8) = R3
      GABABQ( 7, 8) = Z3
      GABABQ( 8, 8) = GABABQ(4,7)
      GABABQ( 9, 9) = 1.000
      GABABQ(10, 9) = R3
      GABABQ(11, 9) = Z3
      GABABQ(12, 9) = GABABQ(4,7)
      GABABQ( 1,10) = 1.000
      GABABQ( 2,10) = R4
      GABABQ( 3,10) = Z4
      GABABQ( 4,10) = R4*Z4
      GABABQ( 5,11) = 1.000
      GABABQ( 6,11) = R4
      GABABQ( 7,11) = Z4
      GABABQ( 8,11) = GABABQ(4,10)
      GABABQ( 9,12) = 1.000
      GABABQ(10,12) = R4
      GABABQ(11,12) = Z4
      GABABQ(12,12) = GABABQ(4,10)
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (12,GABABQ,12,D(10),0,D(11),ISING,SP)
C
C     CALCULATE THE INTEGRAL VALUES IN ARRAY DELINT
C
C       DELINT(04) = (0,0)
C       DELINT(05) = (0,1)
C       DELINT(06) = (0,2)
C       DELINT(07) = (1,0)
C       DELINT(08) = (1,1)
C       DELINT(09) = (1,2)
C       DELINT(10) = (2,0)
C       DELINT(11) = (2,1)
C       DELINT(12) = (2,2)
C       DELINT(13) = (3,0)
C       DELINT(14) = (3,1)
C       DELINT(15) = (3,2)
C
      I1 = 3
      DO 320 I = 1,4
      IP = I - 1
      DO 310 J = 1,3
      IQ = J - 1
      I1 = I1 + 1
      DELINT(I1) = RZINTS(IP,IQ,R,Z,4)
  310 CONTINUE
  320 CONTINUE
C
C     LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
C
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      DGAMR  = DGAMA*DEGRAD
      SINTH  = SIN(DGAMR)
      COSTH  = COS(DGAMR)
      SING   = SINTH
      COSG   = COSTH
      CALL MAT (IDEL)
      IF (SETMAT .EQ. 2.0) GO TO 910
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
      DELA= 1.0/(1.0 - VRO*VOR - VOZ*VZO - VZR*VRZ - VRO*VOZ*VZR -
     1          VRZ*VOR*VZO)
C
C     COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
C
      DO 510 I = 1,21
  510 TEO( I) = 0.0
      TEO( 1) = ER*(1.0 - VOZ*VZO)*DELA
      TEO( 2) = ER*(VZR + VZO*VOR)*DELA
      TEO( 3) = EZ*(1.0 - VRO*VOR)*DELA
      TEO( 4) = ER*(VOR + VZR*VOZ)*DELA
      TEO( 5) = ET*(VZO + VRO*VZR)*DELA
      TEO( 6) = ET*(1.0 - VRZ*VZR)*DELA
      TEO(10) = GRZ
      TEO(15) = GOR
      TEO(21) = GZO
      SING2   = SING**2
      COSG2   = COSG**2
      SING4   = SING**4
      COSG4   = COSG**4
      EE01    = TEO(1)*COSG4 + TEO(3)*SING4 + (TEO(2) + 2.0*TEO(10))*2.0
     1          *SING2*COSG2
      EE02    = TEO(2)*(SING4 + COSG4) + (TEO(1) + TEO(3) - 4.0*TEO(10))
     2          *SING2* COSG2
      EE03    = TEO(1)*SING4 + TEO(3)*COSG4 + (2.0*TEO(2) + 4.0*TEO(10))
     3          *SING2*COSG2
      EE04    = TEO(4)*COSG2 + TEO(5)*SING2
      EE05    = TEO(4)*SING2 + TEO(5)*COSG2
      EE06    = TEO(6)
      EE07    = (TEO(1)*COSG2 - TEO(3)*SING2 + (TEO(2) + 2.0*TEO(10))
     7          *(SING2 - COSG2))*SING*COSG
      EE08    = (TEO(1)*SING2 - TEO(3)*COSG2 + (TEO(2) + 2.0*TEO(10))
     8          *(COSG2 - SING2))*SING*COSG
      EE09    = SING*COSG*(TEO(4) - TEO(5))
C
C     COMPUTE HARMONIC COEFFICIENT
C
      AJHO = IECPT(1) - (IECPT(1)/1000)*1000 - 1
C
C     COMPUTE THERMAL LOAD
C
      A1 = EE01*ALF(1) + EE02*ALF(3) + EE04*ALF(2)
      A2 = EE02*ALF(1) + EE03*ALF(3) + EE05*ALF(2)
      A3 = EE04*ALF(1) + EE05*ALF(3) + EE06*ALF(2)
      A4 = EE07*ALF(1) + EE08*ALF(3) + EE09*ALF(2)
C
C     FORM HTN MATRIX
C
      HTN( 1,1) = A3*DELINT(4)
      HTN( 1,2) = A3*DELINT(7)
      HTN( 1,3) = A3*DELINT(5)
      HTN( 1,4) = A3*DELINT(8)
      HTN( 2,1) = (A1+A3)*DELINT(7)
      HTN( 2,2) = (A1+A3)*DELINT(10)
      HTN( 2,3) = (A1+A3)*DELINT(8)
      HTN( 2,4) = (A1+A3)*DELINT(11)
      HTN( 3,1) = A3*DELINT( 5) + A4*DELINT( 7)
      HTN( 3,2) = A3*DELINT( 8) + A4*DELINT(10)
      HTN( 3,3) = A3*DELINT( 6) + A4*DELINT( 8)
      HTN( 3,4) = A3*DELINT( 9) + A4*DELINT(11)
      HTN( 4,1) = (A1+A3)*DELINT( 8) + A4*DELINT(10)
      HTN( 4,2) = (A1+A3)*DELINT(11) + A4*DELINT(13)
      HTN( 4,3) = (A1+A3)*DELINT( 9) + A4*DELINT(11)
      HTN( 4,4) = (A1+A3)*DELINT(12) + A4*DELINT(14)
      HTN( 5,1) = AJHO*A3*DELINT(4)
      HTN( 5,2) = AJHO*A3*DELINT(7)
      HTN( 5,3) = AJHO*A3*DELINT(5)
      HTN( 5,4) = AJHO*A3*DELINT(8)
      HTN( 6,1) = AJHO*A3*DELINT(7)
      HTN( 6,2) = AJHO*A3*DELINT(10)
      HTN( 6,3) = AJHO*A3*DELINT(8)
      HTN( 6,4) = AJHO*A3*DELINT(11)
      HTN( 7,1) = AJHO*A3*DELINT(5)
      HTN( 7,2) = AJHO*A3*DELINT(8)
      HTN( 7,3) = AJHO*A3*DELINT(6)
      HTN( 7,4) = AJHO*A3*DELINT(9)
      HTN( 8,1) = AJHO*A3*DELINT(8)
      HTN( 8,2) = AJHO*A3*DELINT(11)
      HTN( 8,3) = AJHO*A3*DELINT(9)
      HTN( 8,4) = AJHO*A3*DELINT(12)
      HTN( 9,1) = 0.0
      HTN( 9,2) = 0.0
      HTN( 9,3) = 0.0
      HTN( 9,4) = 0.0
      HTN(10,1) = A4*DELINT(7)
      HTN(10,2) = A4*DELINT(10)
      HTN(10,3) = A4*DELINT(8)
      HTN(10,4) = A4*DELINT(11)
      HTN(11,1) = A2*DELINT(7)
      HTN(11,2) = A2*DELINT(10)
      HTN(11,3) = A2*DELINT(8)
      HTN(11,4) = A2*DELINT(11)
      HTN(12,1) = A2*DELINT(10) + A4*DELINT( 8)
      HTN(12,2) = A2*DELINT(13) + A4*DELINT(11)
      HTN(12,3) = A2*DELINT(11) + A4*DELINT( 9)
      HTN(12,4) = A2*DELINT(14) + A4*DELINT(12)
C
C     COMPUTE LITTLE H MATRIX (INVERSE OF PARTITION OF GABABQ)
C
      IF (ABS(R2-R1) .LT. 1.0E-16) GO TO 930
      IF (ABS(R3-R4) .LT. 1.0E-16) GO TO 930
      IF (ABS(Z4-Z1) .LT. 1.0E-16) GO TO 930
      A      = 1.0/((R2-R1)*(R3-R4)*(Z4-Z1))
      R34A   = A*(R3-R4)
      R21A   = A*(R2-R1)
      H(1,1) = R34A*R2*Z4
      H(1,2) =-R1*Z4*R34A
      H(1,3) = R4*Z1*R21A
      H(1,4) =-R3*Z1*R21A
      H(2,1) =-Z4*R34A
      H(2,2) = Z4*R34A
      H(2,3) =-Z1*R21A
      H(2,4) = Z1*R21A
      H(3,1) =-R2*A*(R2-R4)
      H(3,2) = R1*R34A
      H(3,3) =-R4*R21A
      H(3,4) = R3*R21A
      H(4,1) = R34A
      H(4,2) =-R34A
      H(4,3) = R21A
      H(4,4) =-R21A
C
C     COMPUTE TI
C
      DGAMR = TZERO
      IF (AJHO .GT. 0.0) DGAMR = 0.0
      DO 680 I = 1,4
      TI(I) = TI(I) - DGAMR
  680 CONTINUE
C
C     COMPUTE THE THEMAL LOAD IN FIELD COORDINATES
C
      CALL GMMATS (H,  4, 4,1, TI(1),4,1,0, TL(1))
      CALL GMMATS (HTN,4,12,1, TL(1),4,1,0, D(1) )
C
C     TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
C     ***  COORDINATE SYSTEMS NOT POSSIBLE  *******
C     ***  WITH RINGAX.  THE FOLLOWING CODE WILL IMPLEMENT IT. LRK ***
C     ***  THE FOLLOWING GMMATS HAS D(20) INSTEAD OF TL(1)        ****
C
      CALL GMMATS (GABABQ,12,12,1, D(1),12,1,0,TL(1))
C
C     LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
C.    DO 750 I = 1,144
C.    AKI (I) = 0.0
C.750 CONTINUE
C.    DO 800 I = 1,4
C.    CALL GBTRAN (ICS(I),IECPT(4*I+20),D)   $ THIS IS WRONG ANYWAY
C.    K = 39*(I-1) + 1
C.    DO 800 J = 1,3
C.    KK = K + 12*(J-1)
C.    JJ = 3*(J-1) + 1
C.    AKI(KK  ) = D(JJ  )
C.    AKI(KK+1) = D(JJ+1)
C.    AKI(KK+2) = D(JJ+2)
C.800 CONTINUE
C
C     ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
C
C.    CALL GMMATS ( AKI(1), 12, 12, 1, D(20), 12, 1, 0, TL(1) )
C
      DGAMR = PI
      IF (AJHO .EQ. 0.0) DGAMR = 2.0*PI
C
      DO 850 I = 1,12
      TL(I) = DGAMR*TL(I)
  850 CONTINUE
C
      K = 0
      DO 900 I = 1,4
      L = IGP(I) - 1
      DO 900 J = 1,3
      K = K + 1
      L = L + 1
      PG(L) = PG(L) + TL(K)
  900 CONTINUE
      GO TO 950
C
  910 I = 37
      GO TO 925
  915 I = 218
      GO TO 935
  925 J =-30
      GO TO 945
  930 I = 31
      GO TO 925
  935 J = 30
      IF (IDEL1 .EQ. IDEL2) GO TO 950
      IDEL2 = IDEL1
      SP(2) = JAX
  945 SP(1) = IDEL1
      CALL MESAGE (J,I,SP)
  950 RETURN
      END
