      SUBROUTINE PSBAR
C*****
C THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES K(NPVT,NPVT) AND
C K(NPVT,J) FOR A BAR ELEMENT HAVING END POINTS NUMBERED NPVT AND J.
C*****
C
C                     E C P T  F O R  T H E  B A R
C
C ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
C ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
C ECPT( 3)  -    ...          *
C ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
C ECPT( 5)  -    ...          $
C ECPT( 6)  -    ...          $
C ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
C ECPT( 8)  -  IPINFL(2)      * PIN FLAGS
C ECPT( 9)  -    ...          *
C ECPT(10)  -  ZA(3)          $ OFFSET VECTOR FOR POINT A
C ECPT(11)  -    ...          $
C ECPT(12)  -    ...          $
C ECPT(13)  -  ZB(3)          * OFFSET VECTOR FOR POINT B
C ECPT(14)  -    ...          *
C ECPT(15)  -    ...          *
C ECPT(16)  -  IMATID         MATERIAL ID.
C ECPT(17)  -  A              CROSS-SECTIONAL AREA
C ECPT(18)  -  I1             $ AREA MOMENTS OF INERTIA
C ECPT(19)  -  I2             $
C ECPT(20)  -  FJ             POLAR MOMENT OF INERTIA
C ECPT(21)  -  NSM            NON-STRUCTURAL MASS
C ECPT(22)  -  FE             FORCE ELEMENT DESCRIPTIONS (FORCE METHOD)
C ECPT(23)  -  C1             * STRESS RECOVERY COEFFICIENTS
C ECPT(24)  -  C2             *
C ECPT(25)  -  D1             *
C ECPT(26)  -  D2             *
C ECPT(27)  -  F1             *
C ECPT(28)  -  F2             *
C ECPT(29)  -  G1             *
C ECPT(30)  -  G2             *
C ECPT(31)  -  K1             $ AREA FACTORS FOR SHEAR
C ECPT(32)  -  K2             $
C ECPT(33)  -  I12            AREA MOMENT OF INERTIA
C ECPT(34)  -  MCSIDA         COOR. SYS. ID. FOR GRID POINT A
C ECPT(35)  -  GPA(3)         * BASIC COORDINATES FOR GRID POINT A
C ECPT(36)  -    ...          *
C ECPT(37)  -    ...          *
C ECPT(38)  -  MCSIDB         COOR. SYS. ID. FOR GRID POINT B
C ECPT(39)  -  GPB(3)         $ BASIC COORDINATES FOR GRID POINT B
C ECPT(40)  -    ...          $
C ECPT(41)  -    ...          $
C ECPT(42)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
C ECPT(43)  -  EPSIN1         PREVIOUS STRAIN VALUE ONCE REMOVED
C ECPT(44)  -  EPSIN2         PREVIOUS STRAIN VALUE
C ECPT(45)  -  ESTAR          PREVIOUSLY COMPUTED MODULUS OF ELASTICITY
C ECPT(46)  -  V1STAR         * ELEMENT FORCES, INITIALLY ZERO
C ECPT(47)  -   V2STAR        *
C ECPT(48)  -   TSTAR         *
C ECPT(49)  -   M1ASTR        *
C ECPT(50)  -   M2ASTR        *
C ECPT(51)  -   UAIN(6)       $ INCREMENTAL DISPLACEMENT VECTOR AT PT. A
C ECPT(52)  -   ...           $
C ECPT(53)  -   ...           $
C ECPT(54)  -   ...           $
C ECPT(55)  -   ...           $
C ECPT(56)  -   ...           $
C ECPT(57)  -   UBIN(6)       * INCREMENTAL DISPLACEMENT VECTOR AT PT. B
C ECPT(58)  -   ...           *
C ECPT(59)  -   ...           *
C ECPT(60)  -   ...           *
C ECPT(61)  -   ...           *
C ECPT(62)  -   ...           *
C
      REAL
     1                   L                  ,LSQ
     2,                  LCUBE              ,I1
     3,                  I2                 ,K1
     4,                  K2                 ,KE
     5,                  KEP                ,I12
     6,                  NSM                ,LR1
     7,                  LR2                ,LB
     8,                  L2B3               ,L2B6
      REAL
     1                   M1ASTR             ,M2ASTR
     2,                  M1A                ,M2A
     3,                  M1B                ,M2B
     4,                  K1A                ,K2A
     5,                  K1B                ,K2B
      LOGICAL
     1                   ABASIC             ,BBASIC
     2,                  BASIC              ,AOFSET
     3,                  BOFSET             ,OFFSET
      DIMENSION
     1                   VECI(3)            ,VECJ(3)
     2,                  VECK(3)            ,ECPT(100)
     3,                  IECPT(100)         ,IPIN(10)
     5,                  TA(18)             ,TB(9)
     6,                  SMALV0(6)          ,U(24)
     7,                  D(9)               ,SA(72)
     8,                  SB(36)             ,FA(6)
     9,                  FB(6)
C
      COMMON   /PLA32E/
     1                   IELID              ,ISILNO(2)
     2,                  SMALLV(3)          ,ICSSV
     3,                  IPINFL(2)          ,ZA(3)
     4,                  ZB(3)              ,IMATID
     5,                  A                  ,I1
     6,                  I2                 ,FJ
     7,                  NSM                ,FE
     8,                  C1                 ,C2
     9,                  D1                 ,D2
     T,                  F1                 ,F2
     1,                  G1                 ,G2
     2,                  K1                 ,K2
     3,                  I12
     4,                  MCSIDA             ,GPA(3)
     5,                  MCSIDB             ,GPB(3)
     6,                  ELTEMP             ,EPSIN1
     7,                  EPSIN2             ,ESTAR
     8,                  V1STAR             ,V2STAR
      COMMON   /PLA32E/
     1                   TSTAR              ,M1ASTR
     2,                  M2ASTR             ,UAIN(6)
     3,                  UBIN(6)
      COMMON   /PLA32S/
     1                   KE(144)            ,KEP(144)
     2,                  DELA(6)            ,DELB(6)
      COMMON   /PLA32C/
     1                   GAMMA              ,GAMMAS
      COMMON   /SOUT/
     1                   ISELID             ,SIG1A
     2,                  SIG2A              ,SIG3A
     3,                  SIG4A              ,SIGAX
     4,                  SIGAMX             ,SIGAMN
     5,                  MSTEN              ,SIG1B
     6,                  SIG2B              ,SIG3B
     7,                  SIG4B              ,SIGBMX
     8,                  SIGBMN             ,MSCOM
     9,                  DUM14(14)
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  TEMDUM             ,PLAARG
     3,                  DUM2(2)
      COMMON   /MATOUT/
     1                   E SUB 0            ,G SUB 0
     2,                  NU                 ,RHO
     3,                  ALPHA              ,T SUB 0
     4,                  GSUBE              ,SIGMAT
     5,                  SIGMAC             ,SIGMAS
C
      EQUIVALENCE
     1                   (IELID,ECPT(1),IECPT(1))
     2,                  (TA(10),TB(1))
     3,                  (ECPT(71),D(1))
     4,                  (E SUB 0,PLAANS)
     5,                  (SA(37),SB(1))
     6,                  (MSTEN,SMTEN)
     7,                  (MSCOM,SMCOM)
C
C-----------------------------------------------------------------------
C
C SET UP POINTERS TO COOR. SYS. IDS., OFFSET VECTORS, AND PIN FLAGS.
C ICSIDA AND ICSIDB ARE COOR. SYS. IDS.
C
      JCSIDA = 34
      JCSIDB = 38
      JOFSTA = 10
      JOFSTB = 13
      JPINA  =  8
      JPINB  =  9
      ICSIDA = IECPT(34)
      ICSIDB = IECPT(38)
C
C NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL AXIS
C PLANE  (FMMS - 36 P. 4)
C
      FL = 0.0
      DO 50 I = 1,3
   50 FL = FL + SMALLV(I)**2
      FL = SQRT(FL)
      DO 60 I = 1,3
   60 SMALLV(I) = SMALLV(I) / FL
C
C DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
C
      ABASIC = .TRUE.
      BBASIC = .TRUE.
      IF (ICSIDA .NE. 0) ABASIC = .FALSE.
      IF (ICSIDB .NE. 0) BBASIC = .FALSE.
C
C COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
C
      IF (.NOT. ABASIC) CALL TRANSS (ECPT(JCSIDA),TA)
      IF (.NOT. BBASIC) CALL TRANSS (ECPT(JCSIDA),TB)
C
C DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
C
      AOFSET = .TRUE.
      J = JOFSTA - 1
      DO 70 I = 1,3
      J = J + 1
      IF (ECPT(J) .NE. 0.0) GO TO 80
   70 CONTINUE
      AOFSET = .FALSE.
   80 BOFSET = .TRUE.
      J = JOFSTB - 1
      DO 90 I = 1,3
      J = J + 1
      IF (ECPT(J) .NE. 0.0) GO TO 100
   90 CONTINUE
      BOFSET = .FALSE.
C
C FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
C
  100 VECI(1) = ECPT(JCSIDA+1) - ECPT(JCSIDB+1)
      VECI(2) = ECPT(JCSIDA+2) - ECPT(JCSIDB+2)
      VECI(3) = ECPT(JCSIDA+3) - ECPT(JCSIDB+3)
C
C TRANSFORM THE OFFSET VECTORS IF NECESSARY
C
      IF ( .NOT. AOFSET  .AND.  .NOT. BOFSET )  GO TO 150
C
C TRANSFORM THE OFFSET VECTOR FOR POINT A IF NECESSARY.
C
      IDELA = 1
      J = JOFSTA - 1
      DO 110 I = 1,3
      J = J + 1
  110 DELA(I) = ECPT(J)
      IF (ABASIC) GO TO 120
      IDELA = 4
      CALL GMMATS (TA,3,3,0, DELA(1),3,1,0, DELA(4) )
C
C TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
C
  120 IDELB = 1
      J = JOFSTB - 1
      DO 130 I = 1,3
      J = J + 1
  130 DELB(I) = ECPT(J)
      IF (BBASIC) GO TO 140
      IDELB = 4
      CALL GMMATS (TB,3,3,0, DELB(1),3,1,0, DELB(4) )
C
C SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
C
  140 VECI(1) = VECI(1) + DELA(IDELA  ) - DELB(IDELB  )
      VECI(2) = VECI(2) + DELA(IDELA+1) - DELB(IDELB+1)
      VECI(3) = VECI(3) + DELA(IDELA+2) - DELB(IDELB+2)
C
C COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
C
  150 VECI(1) = -VECI(1)
      VECI(2) = -VECI(2)
      VECI(3) = -VECI(3)
      FL = SQRT (VECI(1)**2  +  VECI(2)**2  +  VECI(3)**2)
      DO 160 I = 1,3
  160 VECI(I) = VECI(I) / FL
C
C COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ****CHECK THIS LOGIC****
C
      DO 165 I = 1,3
  165 SMALV0(I) = SMALLV(I)
      ISV = 1
      IF (ICSSV .EQ. 0) GO TO 180
      ISV = 4
      CALL GMMATS (TA,3,3,0, SMALV0(1),3,1,0, SMALV0(4) )
C
C COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
C
  180 VECK(1) =  VECI(2) * SMALV0(ISV+2)  -  VECI(3) * SMALV0(ISV+1)
      VECK(2) =  VECI(3) * SMALV0(ISV  )  -  VECI(1) * SMALV0(ISV+2)
      VECK(3) =  VECI(1) * SMALV0(ISV+1)  -  VECI(2) * SMALV0(ISV)
      FLL     =  SQRT ( VECK(1)**2  +  VECK(2)**2  +  VECK(3)**2  )
      VECK(1) =  VECK(1) / FLL
      VECK(2) =  VECK(2) / FLL
      VECK(3) =  VECK(3) / FLL
C
C COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
C
      VECJ(1) =  VECK(2) * VECI(3)  -  VECK(3) * VECI(2)
      VECJ(2) =  VECK(3) * VECI(1)  -  VECK(1) * VECI(3)
      VECJ(3) =  VECK(1) * VECI(2)  -  VECK(2) * VECI(1)
      FLL     =  SQRT ( VECJ(1)**2  +  VECJ(2)**2  +  VECJ(3)**2 )
      VECJ(1) =  VECJ(1) / FLL
      VECJ(2) =  VECJ(2) / FLL
      VECJ(3) =  VECJ(3) / FLL
C
C SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX CALCULATION
C
      L = FL
      LSQ = L**2
      LCUBE = LSQ * L
C
C STORE INCREMENTAL DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
C
      DO 182 I = 1,6
      U(I)    = UAIN(I)
  182 U(I+12) = UBIN(I)
C*****
C COMPUTE ON FIRST PASS C  * E  * U   AND C  * E  * U  ON SECOND PASS
C                        B    B    B       A    A    A
C*****
      IPASS = 1
      BASIC = BBASIC
      OFFSET = BOFSET
      JOFSET = JOFSTB
      JCSID = 10
      INDEX = 13
C
C IF THERE ARE OFFSETS FOR THIS POINT, CONSTRUCT THE 3 X 3 MATRIX D.
C
  184 IF (.NOT. OFFSET) GO TO 188
      D(1) = 0.0
      D(2) =  ECPT(JOFSET+2)
      D(3) = -ECPT(JOFSET+1)
      D(4) = -D(2)
      D(5) = 0.0
      D(6) =  ECPT(JOFSET)
      D(7) = -D(3)
      D(8) = -D(6)
      D(9) = 0.0
C
C COMPUTE THE 3 VECTOR  D * U , WHERE U  IS THE VECTOR OF THE 3
C                            R         R
C ROTATIONAL DISPLACEMENTS
C
      CALL GMMATS (D,3,3,0, U(INDEX+3),3,1,0, U(INDEX+6))
C
C ADD OFFSET CONTRIBUTION TO THE TRANSLATION COMPONENTS OF THE DISPLACE-
C MENT VECTOR
C
      J = INDEX
      DO 186 I = 1,3
      U(J) = U(J) + U(J+6)
  186 J = J + 1
C
C TRANSFORM TRANSLATIONAL COMPONENTS TO BASIC COORDINATES IF NECESSARY
C
  188 IF (BASIC) GO TO 190
      CALL GMMATS (TA(JCSID),3,3,0, U(INDEX),3,1,0, U(INDEX+3) )
C
C STORE TRANSFORMED VECTOR BACK INTO ITS ORIGINAL D.P. LOCATION
C
      U(INDEX  ) = U(INDEX+3)
      U(INDEX+1) = U(INDEX+4)
      U(INDEX+2) = U(INDEX+5)
  190 IF (IPASS .EQ. 2) GO TO 192
      IPASS = 2
      BASIC  = ABASIC
      OFFSET = AOFSET
      JOFSET = JOFSTA
      JCSID  = 1
      INDEX  = 1
      GO TO 184
C
C FORM THE DIFFERENCE OF THE TRANSLATIONAL COMPONENTS OF THE TRANSFORMED
C DISPLACEMENT VECTORS
C
  192 DO 194 I = 1,3
  194 U(I+12) = U(I+12) - U(I)
C
C FORM DOT PRODUCT
C
      CALL GMMATS (VECI,3,1,1, U(13),3,1,0, D(1) )
C
C CALCULATE THE INCREMENTAL ELEMENT STRAIN
C
      DEPS1 = D(1) / L
C
C PERFORM EXTENSIONAL STRAIN CALCULATIONS
C
      DEPS2  = EPSIN2 - EPSIN1
      EPS1   = EPSIN2 + DEPS1
      EPS2 = EPSIN2 + (DEPS1 + GAMMAS**2 * DEPS2)* (GAMMA + 1.0E0)
     1                /(GAMMAS + 1.0E0)
     2              + GAMMAS * (DEPS1 - GAMMAS*DEPS2) * (GAMMA+1.0E0)**2
     3                /(GAMMAS + 1.0E0)
C
C CALL MAT ROUTINE TO GET SIGMA1 AND SIGMA2 AS FUNCTIONS OF EPS1,EPS2
C
      MATIDC = IMATID
      MATFLG = 1
      CALL MAT (IECPT(1))
      E SUB 0 L = E SUB 0
      G SUB 0 L = G SUB 0
      MATFLG = 6
      PLAARG = EPS1
      CALL MAT (IECPT(1))
      SIGMA1 = PLAANS
      PLAARG = EPS2
      CALL MAT (IECPT(1))
      SIGMA2 = PLAANS
C
C NOTE THAT E1 IS USED IN THIS ROUTINE ONLY TO UPDATE THE EST (ECPT)
C ENTRY
C
      IF (EPS1 .EQ. EPS2) GO TO 200
      E1 = (SIGMA2 - SIGMA1) / (EPS2 - EPS1)
      GO TO 202
  200 E1 = ESTAR
C
C BEGIN ELEMENT STRESS MATRIX CALCULATIONS.
C
  202 E = ESTAR
      G = ESTAR * G SUB 0 L / E SUB 0 L
      EI1  = E * I1
      EI2  = E * I2
      IF (K1 .EQ. 0.0  .OR.  I12 .NE. 0.0) GO TO 210
      GAK1 = G * A * K1
      R1 = (12.0 * EI1 * GAK1) / (GAK1 * LCUBE + 12.0 * L * EI1)
      GO TO 220
  210 R1 =  12.0 * EI1 / LCUBE
  220 IF (K2 .EQ. 0.0  .OR.  I12 .NE. 0.0) GO TO 230
      GAK2 = G * A * K2
      R2 = (12.0 * EI2 * GAK2) / (GAK2 * LCUBE + 12.0 * L * EI2)
      GO TO 240
  230 R2 =  12.0 * EI2 / LCUBE
C
C COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
C
  240 SK1 = 0.25 * R1 * LSQ  +  EI1 / L
      SK2 = 0.25 * R2 * LSQ  +  EI2 / L
      SK3 = 0.25 * R1 * LSQ  -  EI1 / L
      SK4 = 0.25 * R2 * LSQ  -  EI2 / L
C
C COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
C
      AEL = A * E / L
      LR1 = L * R1 / 2.0
      LR2 = L * R2 / 2.0
      GJL = G * FJ / L
C
C CONSTRUCT THE 12 X 12 MATRIX KE
C
      DO 250 I = 1,144
  250 KE(I) = 0.0
      KE(  1) =  AEL
      KE(  7) = -AEL
      KE( 14) =  R1
      KE( 18) =  LR1
      KE( 20) = -R1
      KE( 24) =  LR1
      KE( 27) =  R2
      KE( 29) = -LR2
      KE( 33) = -R2
      KE( 35) = -LR2
      KE( 40) =  GJL
      KE( 46) = -GJL
      KE( 51) = -LR2
      KE( 53) =  SK2
      KE( 57) =  LR2
      KE( 59) =  SK4
      KE( 62) =  LR1
      KE( 66) =  SK1
      KE( 68) = -LR1
      KE( 72) =  SK3
      KE( 73) = -AEL
      KE( 79) =  AEL
      KE( 86) = -R1
      KE( 90) = -LR1
      KE( 92) =  R1
      KE( 96) = -LR1
      KE( 99) = -R2
      KE(101) =  LR2
      KE(105) =  R2
      KE(107) =  LR2
      KE(112) = -GJL
      KE(118) =  GJL
      KE(123) = -LR2
      KE(125) =  SK4
      KE(129) =  LR2
      KE(131) =  SK2
      KE(134) =  LR1
      KE(138) =  SK3
      KE(140) = -LR1
      KE(144) =  SK1
      IF (I12 .EQ. 0.0) GO TO 255
      BETA = 12.0 * E * I12 / LCUBE
      LB   = L * BETA / 2.0
      L2B3 = LSQ * BETA / 3.0
      L2B6 = LSQ * BETA / 6.0
      KE( 15) =  BETA
      KE( 17) = -LB
      KE( 21) = -BETA
      KE( 23) = -LB
      KE( 26) =  BETA
      KE( 30) =  LB
      KE( 32) = -BETA
      KE( 36) =  LB
      KE( 50) = -LB
      KE( 54) = -L2B3
      KE( 56) =  LB
      KE( 60) = -L2B6
      KE( 63) =  LB
      KE( 65) = -L2B3
      KE( 69) = -LB
      KE( 71) = -L2B6
      KE( 87) = -BETA
      KE( 89) =  LB
      KE( 93) =  BETA
      KE( 95) =  LB
      KE( 98) = -BETA
      KE(102) = -LB
      KE(104) =  BETA
      KE(108) = -LB
      KE(122) = -LB
      KE(126) = -L2B6
      KE(128) =  LB
      KE(132) = -L2B3
      KE(135) =  LB
      KE(137) = -L2B6
      KE(141) = -LB
      KE(143) = -L2B3
C
C DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
C
  255 KA = IECPT(JPINA)
      KB = IECPT(JPINB)
      IF (KA .EQ. 0  .AND.  KB .EQ. 0) GO TO 325
C
C SET UP THE IPIN ARRAY
C
      DO 260 I = 1,5
      IPIN(I  ) = MOD(KA,10)
      IPIN(I+5) = MOD(KB,10) + 6
      IF (IPIN(I+5) .EQ. 6) IPIN(I+5) = 0
      KA = KA / 10
  260 KB = KB / 10
C
C ALTER KE MATRIX DUE TO PIN FLAGS.
C
      DO 320 I = 1,10
      IF (IPIN(I) .EQ. 0) GO TO 320
      II = 13 * IPIN(I)  -  12
      IF (KE(II) .NE. 0.0) GO TO 280
      IL = IPIN(I)
      II = II - IL
      DO 270 J = 1,12
      II = II + 1
      KE(II) = 0.0
      KE(IL) = 0.0
      IL = IL + 12
  270 CONTINUE
      GO TO 320
  280 DO 300 J = 1,12
      JI  = 12 * (J-1) + IPIN(I)
      IJ = 12 * (IPIN(I) - 1) + J
      DO 290 LL = 1,12
      JLL = 12 * (J-1) + LL
      ILL = 12 * (IPIN(I) - 1) + LL
      KEP(JLL) = KE(JLL) - (KE(ILL)/KE(II)) * KE(JI)
  290 CONTINUE
      KEP(IJ) = 0.0
      KEP(JI) = 0.0
  300 CONTINUE
      DO 310 K = 1,144
  310 KE(K) = KEP(K)
  320 CONTINUE
C
C        E
C STORE K   IN KEP(1),...,KEP(36) AND
C        AA
C
C        E
C STORE K   IN KEP(37),...,KEP(72)
C        AB
C
  325 J = 0
      DO 340 I = 1,72,12
      LOW = I
      LIM = LOW + 5
      DO 330 K = LOW,LIM
      J = J + 1
      KEP(J) = KE(K)
  330 KEP(J+36) = KE(K+6)
  340 CONTINUE
C
C                                                        T
C STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
C
      KE(1) = VECI(1)
      KE(2) = VECI(2)
      KE(3) = VECI(3)
      KE(4) = VECJ(1)
      KE(5) = VECJ(2)
      KE(6) = VECJ(3)
      KE(7) = VECK(1)
      KE(8) = VECK(2)
      KE(9) = VECK(3)
C
C SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
C
      BASIC = ABASIC
      JCSID  = JCSIDA
      OFFSET = AOFSET
      JOFSET = JOFSTA
      IWBEG  = 0
      IKEL   = 1
      IAB = 1
      INDEX = ISILNO(1)
C
C ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
C MATRICES WILL RESIDE.                              A      B
C
      DO 350 I = 28,108
  350 KE(I) = 0.0
C
C SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX.
C G = AT X TI
C
  360 IG = 1
      IF (BASIC) GO TO 370
      CALL TRANSS (ECPT(JCSID),KE(10))
      CALL GMMATS (KE(1),3,3,0, KE(10),3,3,0, KE(19) )
      IG = 19
C
C IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X3 MATRIX.
C
  370 IF ( .NOT. OFFSET ) GO TO 380
      KE(10) = 0.0
      KE(11) =  ECPT(JOFSET+2)
      KE(12) = -ECPT(JOFSET+1)
      KE(13) = -KE(11)
      KE(14) = 0.0
      KE(15) =  ECPT(JOFSET)
      KE(16) = -KE(12)
      KE(17) = -KE(15)
      KE(18) = 0.0
C
C FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
C
      CALL GMMATS (KE(IG),3,3,0, KE(10),3,3,0, KE(28) )
C
C
C FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
C           A                B
C UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE STORED
C IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO, WILL BE
C STORED IN THE UPPER RIGHT CORNER.
C
C
  380 KE(IWBEG+37) = KE(IG  )
      KE(IWBEG+38) = KE(IG+1)
      KE(IWBEG+39) = KE(IG+2)
      KE(IWBEG+43) = KE(IG+3)
      KE(IWBEG+44) = KE(IG+4)
      KE(IWBEG+45) = KE(IG+5)
      KE(IWBEG+49) = KE(IG+6)
      KE(IWBEG+50) = KE(IG+7)
      KE(IWBEG+51) = KE(IG+8)
      KE(IWBEG+58) = KE(IG  )
      KE(IWBEG+59) = KE(IG+1)
      KE(IWBEG+60) = KE(IG+2)
      KE(IWBEG+64) = KE(IG+3)
      KE(IWBEG+65) = KE(IG+4)
      KE(IWBEG+66) = KE(IG+5)
      KE(IWBEG+70) = KE(IG+6)
      KE(IWBEG+71) = KE(IG+7)
      KE(IWBEG+72) = KE(IG+8)
      IF ( .NOT. OFFSET ) GO TO 390
      KE(IWBEG+40) = KE(28)
      KE(IWBEG+41) = KE(29)
      KE(IWBEG+42) = KE(30)
      KE(IWBEG+46) = KE(31)
      KE(IWBEG+47) = KE(32)
      KE(IWBEG+48) = KE(33)
      KE(IWBEG+52) = KE(34)
      KE(IWBEG+53) = KE(35)
      KE(IWBEG+54) = KE(36)
C
C                          E                      E
C FORM THE PRODUCT  S  =  K    X  W   OR  S   = K    X  W  , DEPENDING
C                    A     AA      A       B     AB      B
C UPON WHICH POINT WE ARE WORKING WITH.
C
  390 CALL GMMATS (KEP(IKEL),6,6,0, KE(IWBEG+37),6,6,0, SA(IAB) )
C
C IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED.  IF NOT,
C SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
C
      IF (IWBEG .EQ. 36) GO TO 500
      BASIC = BBASIC
      JCSID = JCSIDB
      OFFSET = BOFSET
      JOFSET = JOFSTB
      IWBEG  = 36
      IKEL   = 37
      IAB = 37
      INDEX  = ISILNO(2)
      DO 400 I = 28,36
  400 KE(I) = 0.0
      GO TO 360
C
C COMPUTE FORCES AND MOMENTS FROM  S   AND   S   AND DISPLACEMENT
C                                   A         B
C VECTORS
C
  500 CALL GMMATS (SA,6,6,0, UAIN,6,1,0, FA)
      CALL GMMATS (SB,6,6,0, UBIN,6,1,0, FB)
      FX = A * SIGMA1
      V1  = -FA(2) - FB(2) + V1STAR
      V2  = -FA(3) - FB(3) + V2STAR
      T   = -FA(4) - FB(4) + TSTAR
      M2A =  FA(5) + FB(5) + M2ASTR
      M1A = -FA(6) - FB(6) + M1ASTR
      M1B = M1A - V1*L
      M2B = M2A - V2*L
C*****
C COMPUTE ELEMENT STRESSES AT 4 POINTS
C*****
C
C COMPUTE K1A AND K2A
C
      IF (I12 .NE. 0.0) GO TO 530
      IF (I1 .NE. 0.0) GO TO 520
      K1A = 0.0
      GO TO 540
  520 K1A = -M1A / I1
      GO TO 540
  530 K1A = (M2A * I12  -  M1A * I2) / (I1 * I2  -  I12**2)
      K2A = (M1A * I12  -  M2A * I1) / (I1 * I2  -  I12**2)
      GO TO 560
  540 IF (I2 .NE. 0.0) GO TO 550
      K2A = 0.0
      GO TO 560
  550 K2A = -M2A / I2
C
C COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
C
  560 SIG1A = K1A * C1  +  K2A * C2
      SIG2A = K1A * D1  +  K2A * D2
      SIG3A = K1A * F1  +  K2A * F2
      SIG4A = K1A * G1  +  K2A * G2
C
C COMPUTE K1B AND K2B
C
      IF (I12 .NE. 0.0) GO TO 580
      IF (I1 .NE. 0.0) GO TO 570
      K1B = 0.0
      GO TO 590
  570 K1B = -M1B / I1
      GO TO 590
  580 K1B = (M2B * I12  -  M1B * I2) / (I1 * I2  -  I12**2)
      K2B = (M1B * I12  -  M2B * I1) / (I1 * I2  -  I12**2)
      GO TO 610
  590 IF (I2 .NE. 0.0) GO TO 600
      K2B = 0.0
      GO TO 610
  600 K2B = -M2B / I2
C
C COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
C
  610 SIG1B = K1B * C1  +  K2B * C2
      SIG2B = K1B * D1  +  K2B * D2
      SIG3B = K1B * F1  +  K2B * F2
      SIG4B = K1B * G1  +  K2B * G2
C
C COMPUTE AXIAL STRESS
C
      SIGAX = 0.0
      IF (A .NE. 0.0) SIGAX = FX / A
C
C COMPUTE MAXIMA AND MINIMA
C
      SIGAMX = SIGAX + AMAX1(SIG1A,SIG2A,SIG3A,SIG4A)
      SIGBMX = SIGAX + AMAX1(SIG1B,SIG2B,SIG3B,SIG4B)
      SIGAMN = SIGAX + AMIN1(SIG1A,SIG2A,SIG3A,SIG4A)
      SIGBMN = SIGAX + AMIN1(SIG1B,SIG2B,SIG3B,SIG4B)
C
C COMPUTE MARGIN OF SAFETY IN TENSION
C
      IF(SIGMAT.LE.0.0)GO TO 620
      IF(AMAX1(SIGAMX,SIGBMX).LE.0.0) GO TO 620
      Q=SIGMAT/AMAX1(SIGAMX,SIGBMX)
      SMTEN=Q-1.0
      GO TO 630
  620 MSTEN=1
C
C      COMPUTE MARGIN OF SAFETY IN COMPRESSION
C
  630 SIGMAC=-ABS(SIGMAC)
      IF(AMIN1(SIGAMN,SIGBMN).GE.0.0) GO TO 640
      W=SIGMAC/AMIN1(SIGAMN,SIGBMN)
      SMCOM=W-1.0
      GO TO 650
  640 MSCOM=1
  650 ISELID = IELID
C
C UPDATE EST (ECPT) ENTRIES
C
      EPSIN1 = EPSIN2
      EPSIN2 = EPS1
      ESTAR  = E1
      V1STAR = V1
      V2STAR = V2
      TSTAR  = T
      M1ASTR = M1A
      M2ASTR = M2A
      RETURN
      END
