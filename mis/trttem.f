      SUBROUTINE TRTTEM ( TI, PG)
C****
C THIS ROUTINE COMPUTES THE THERMAL LOAD FOR THE ASSYMMETRIC RING ELE
C WITH A TRIANGULAR CROSS SECTION
C****
C ECPT (01) = ELEMENT ID                         I
C ECPT (02) = SIL A                              I
C ECPT (03) = SIL B                              I
C ECPT (04) = SIL C                              I
C ECPT (05) = MATERIAL ORIENTATION ANGLE(DEGREES)R
C ECPT (07) = MATERIAL ID                        I
C ECPT (08) TO  ECPT (21) = PHI                  R
C ECPT (22) = COOR. SYS. FOR GRID POINT A        I
C ECPT (23) = R-CORD OF GRID A                   R
C ECPT (24) = Z-CORD OF GRID A                   R
C ECPT (25) = 0.0                                R
C ECPT (26) = CORD. SYS. GRID POINT B (NOT USED) I
C ECPT (27) = R-CORD OF GRID B                   R
C ECPT (28) = Z-CORD OF GRID B                   R
C ECPT (29) = 0.0                                R
C ECPT (30) = CORD. SYS. GRID POINT C (NOT USED) I
C ECPT (31) = R-CORD OF GRID C                   R
C ECPT (32) = Z-CORD OF GRID C                   R
C ECPT (33) = 0.0                                R
C ECPT (34) = EL. TEMPERATURE FOR MATERIAL       R
C
      DIMENSION R(3), Z(3), GABABQ(9,9),DELINT(12),TEO(21)
     1,         DTT(9),IGP(3),IECPT(34),ICS(3),PG(1),FIJ(9)
     2,         D(3)  ,TL(9) ,TI(3)
C
C  . ECPT COMMON BLOCK
      COMMON    /TRIMEX/  ECPT(34)
C
C  . MATERIAL INPUT AND OUTPUT...
      COMMON    /MATIN/
     1                   MATIDC            ,MATFLG
     2,                  ELTEMP            ,STRESS
     3,                  SINTH             ,COSTH
C
      COMMON    /MATOUT/
     1                   E(3)              ,ANU(3)
     2,                  RHO               ,G(3)
     3,                  ALF(3)            ,TZERO      ,GSUBE
     4,                   MOSKP(9)            ,SETMAT
      COMMON    /CONDAS/  CONSTS(5)
      EQUIVALENCE (IECPT(1), ECPT(1)),  (Z(1), Z1),  (Z(2), Z2)
     1,             ( Z(3), Z3)
     2,        ( R(1), R1),  ( R(2), R2),  (R(3), R3)
      EQUIVALENCE (CONSTS(1),PI), (CONSTS(4),DEGRAD)
C
C START EXECUTION
C
C STORE ECPT PARAMETERS IN LOCAL VARIABLES
      IDEL = IECPT(1)
      IGP(1) = IECPT(2)
      IGP(2) = IECPT(3)
      IGP(3) = IECPT(4)
      MATID  = IECPT(07)
      ICS(1) = IECPT(22)
      ICS(2) = IECPT(26)
      ICS(3) = IECPT(30)
      R(1)   = ECPT(23)
      R(2)   = ECPT(27)
      R(3)   = ECPT(31)
      Z(2)   = ECPT(28)
      D(2)   = ECPT(29)
      Z(1)   = ECPT(24)
      D(1)   = ECPT(25)
      Z(3)   = ECPT(32)
      D(3)   = ECPT(33)
      DGAMA  = ECPT(05)
      TEMPE  = ECPT(34)
C
C COMPUTE THE ELEMENT COORDINATES
      ZMIN = AMIN1(Z1, Z2, Z3)
      Z1 = Z1 - ZMIN
      Z2 = Z2 - ZMIN
      Z3 = Z3 - ZMIN
C
C FORM THE TRANSFORMATION MATRIX GABABQ (9X9) FROM FIELD COORDINATES TO
C GRID POINT DEGREES OF FREEDOM
      DO 300 I = 1,9
      DO 300 J = 1,9
  300 GABABQ (I,J) = 0.000
      AA = R2 * Z3 + R1 * Z2 + Z1 * R3 - Z2 * R3 - R1 * Z3 - R2 * Z1
      AA = 1.0E0 / AA
      C1 = AA * ( R2 * Z3 - Z2 * R3)
      C2 = - AA * ( Z3 - Z2 )
      C3 = AA * ( R3 - R2 )
      GABABQ (1,1) = C1
      GABABQ (2,4) = C1
      GABABQ (3,7) = C1
      GABABQ (1,2) = C2
      GABABQ (2,5) = C2
      GABABQ (3,8) = C2
      GABABQ (1,3) = C3
      GABABQ(2,6) = C3
      GABABQ(3,9) = C3
      C1 = -AA * ( R1 * Z3 - Z1 * R3)
      C2 =  AA * ( Z3 - Z1 )
      C3 = -AA * ( R3 - R1 )
      GABABQ(4,1) = C1
      GABABQ(4,2) = C2
      GABABQ(4,3) = C3
      GABABQ(5,4) = C1
      GABABQ(5,5) = C2
      GABABQ(5,6) = C3
      GABABQ(6,7) = C1
      GABABQ(6,8) = C2
      GABABQ(6,9) = C3
      C1 = AA * ( R1 * Z2 - Z1 * R2)
      C2 = -AA * ( Z2 - Z1)
      C3 = AA * ( R2 - R1 )
      GABABQ(7,1) = C1
      GABABQ(7,2) = C2
      GABABQ(7,3) = C3
      GABABQ(8,4) = C1
      GABABQ(8,5) = C2
      GABABQ(8,6) = C3
      GABABQ(9,7) = C1
      GABABQ(9,8) = C2
      GABABQ(9,9) = C3
C
C LOCATE THE MATERIAL PROPERTIES IN THE MAT1 OR MAT3
      DGAMR = DGAMA * DEGRAD
      COSG = COS (DGAMR)
      SING = SIN (DGAMR)
      COSTH = COSG
      SINTH = SING
      MATIDC = MATID
      MATFLG = 7
      ELTEMP = TEMPE
      CALL MAT (IDEL)
      IF (SETMAT.EQ.2.0) GO TO 910
C
C  . SET MATERIAL PROPERTIES IN LOCAL VARIABLES...
      ER = E(1)
      ET = E(2)
      EZ = E(3)
      VRO = ANU(1)
      VOZ = ANU(2)
      VZR = ANU(3)
      GOR = G(1)
      GZO = G(2)
      GRZ = G(3)
      VOR = VRO * ET / ER
      VZO = VOZ * EZ / ET
      VRZ = VZR * ER / EZ
      DEL = 1.0E0 / (1.0E0 - VRO * VOR - VOZ * VZO - VZR * VRZ
     1     - VRO * VOZ * VZR - VRZ * VOR * VZO )
C
C COMPUTE ELASTIC CONSTANTS MATRIX FROM MATERIAL TO ELEMENT AXIS
      DO 510 I = 1,21
  510 TEO (I) = 0.0E0
      TEO (1) = ER * ( 1.0E0 - VOZ * VZO) * DEL
      TEO (2) = ER * ( VZR + VZO * VOR ) * DEL
      TEO (3) = EZ * ( 1.0E0 - VRO * VOR ) * DEL
      TEO (4) = ER * ( VOR + VZR * VOZ) * DEL
      TEO (5) = ET * (VZO + VRO * VZR ) * DEL
      TEO (6) = ET * ( 1.0E0 - VRZ * VZR ) * DEL
      TEO (10) = GRZ
      TEO (15) = GOR
      TEO (21) = GZO
      C2 = COSG * COSG
      C4 = C2 * C2
      S2 = SING * SING
      S4 = S2 * S2
      C2S2 = C2 * S2
      EE01 = TEO(1)*C4 + TEO(3)*S4 + (TEO(2) + 2.0E0*TEO(10))
     1      * 2.0E0*C2S2
      EE02 = TEO(2) * (S4+C4) + (TEO(1) + TEO(3) - 4.0E0*TEO(10))*C2S2
      EE03 = TEO(4)*C2 + TEO(5)*S2
      EE04 = SING*COSG * (TEO(1)*C2 - TEO(3)*S2 + (TEO(2) + 2.0E0
     4      *TEO(10)) * (S2-C2))
      EE08 = TEO(1)*S4 + TEO(3)*C4 + (2.0E0*TEO(2) + 4.0E0*TEO(10))
     8      * C2S2
      EE09 = TEO(4)*S2 + TEO(5)*C2
      EE10 = SING*COSG * (TEO(1)*S2 - TEO(3)*C2 + (TEO(2) + 2.0E0
     *      * TEO(10)) * (C2 - S2))
      EE15 = TEO(6)
      EE16 = SING*COSG * (TEO(4) - TEO(5))
C
C COMPUTE HARMONIC COEFFICIENT
      AJHO = IECPT(1) - (IECPT(1) /1000) * 1000 - 1
C
C  . CALCULATE THE INTEGRAL VALUES IN DELINT...
C
C         DELINT(4) = 0,0
C         DELINT(5) = 0,1
C         DELINT(6) = 1,0
C
      DELINT(4) = AIS (3,0,0,R,Z)
      DELINT(5) = AIS (3,0,1,R,Z)
      DELINT(6) = AIS (3,1,0,R,Z)
C
      T1 = EE01*ALF(1) + EE02*ALF(3) + EE03*ALF(2)
      T2 = EE02*ALF(1) + EE08*ALF(3) + EE09*ALF(2)
      T3 = EE03*ALF(1) + EE09*ALF(3) + EE15*ALF(2)
      T4 = EE04*ALF(1) + EE10*ALF(3) + EE16*ALF(2)
C GENERATE DTT MATRIX
      DTT (1) = DELINT (4) * T3
      DTT (2) = DELINT (6) * ( T1 + T3 )
      DTT (3) = DELINT (5) * T3 + DELINT(6) * T4
      DTT (4) = AJHO * DTT (1)
      DTT (5) = AJHO * DELINT (6)*T3
      DTT (6) = AJHO * DELINT (5) * T3
      DTT (7) = 0.0
      DTT (8) = DELINT (6) * T4
      DTT (9) = DELINT (6) * T2
C
C TRANSFORM THE THERMAL LOAD TO GRID POINT DEGREES OF FREEDOM
      CALL GMMATS (GABABQ, 9, 9, 1, DTT, 9, 1, 0, FIJ)
      T = TZERO
      IF (AJHO.GT.0.0) T = 0.0
      T = ((TI(1) + TI(2) + TI(3))/3.0E0 - T) * PI
      IF ( AJHO .EQ. 0.0 ) T = T * 2.0E0
      DO 959 I = 1, 9
  959 TL(I) = T * FIJ(I)
C
C**** THE FOLLOWING CODE REMOVED.  CORD.SYS. NOT POSSIBLE WITH RINGAX **
C.959 FIJ(I) = T*FIJ(I)
C.
C. LOCATE THE TRANSFORMATION MATRICES FOR THE THREE GRID POINTS
C.    DO 750 I=1,81
C.750 AKI(I) = 0.0
C.     DO 800 I = 1,3
C.    CALL GBTRAN(ICS(I),IECPT(4*I+22),DTT(1))  **R,TH,Z NEEDED**
C.    K=30*(I-1) + 1
C.     DO 800 J=1,3
C.    KK = K+9*(J-1)
C.    JJ=3*(J-1)+1
C.    AKI(KK) = DTT(JJ)
C.    AKI(KK+1) = DTT(JJ+1)
C.    AKI(KK+2) = DTT(JJ+2)
C.800 CONTINUE
C.
C. TRANSFORM THE THERMAL LOAD FROM BASIC TO LOCAL COORD...
C.    CALL GMMATS (AKI(1),9,9,1, FIJ(1),9,1,0, TL(1))
C
C ADD THE ELEMENT THERMAL LOAD TO THE STRUCTURE THERMAL LOAD
      K = 0
      DO 900 I = 1, 3
      L = IGP(I) - 1
      DO 900 J = 1,3
      K = K + 1
      L = L + 1
      PG(L) = PG(L) +  TL(K)
  900 CONTINUE
       GO TO 920
  910 CALL MESAGE (-30,37,ECPT(1))
  920 RETURN
      END
