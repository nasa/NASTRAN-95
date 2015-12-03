      SUBROUTINE BAR (Z,IDEFM,NOGPTT,NOEDT)
C
C     THIS IS THE ELEMENT TEMPERATURE AND DEFORMATION LOADING ROUTINE
C     FOR THE BAR ELEMENT.
C
C     THIS ROUTINE IS VERY MUCH SIMILIAR TO THAT OF SUBROUTINES KBAR AND
C     SBAR1 THUS ANY ALTERS HERE MAY BE REQUIRED IN THESE OTHER TWO
C     ROUTINES ALSO.
C
C     ECPT FOR THE BAR
C
C     ECPT( 1)  -  IELID         ELEMENT ID. NUMBER
C     ECPT( 2)  -  ISILNO(2)     * SCALAR INDEX NOS. OF THE GRID POINTS
C     ECPT( 3)  -    ...         *
C     ECPT( 4)  -  SMALLV(3)     $ REFERENCE VECTOR
C     ECPT( 5)  -    ...         $
C     ECPT( 6)  -    ...         $
C     ECPT( 7)  -  ICSSV         COOR. SYS. ID FOR SMALLV VECTOR
C     ECPT( 8)  -  IPINFL(2)     * PIN FLAGS
C     ECPT( 9)  -    ...         *
C     ECPT(10)  -  ZA(3)         $ OFFSET VECTOR FOR POINT A
C     ECPT(11)  -    ...         $
C     ECPT(12)  -    ...         $
C     ECPT(13)  -  ZB(3)         * OFFSET VECTOR FOR POINT B
C     ECPT(14)  -    ...         *
C     ECPT(15)  -    ...         *
C     ECPT(16)  -  IMATID        MATERIAL ID.
C     ECPT(17)  -  A             CROSS-SECTIONAL AREA
C     ECPT(18)  -  I1            $ AREA MOMENTS OF INERTIA
C     ECPT(19)  -  I2            $
C     ECPT(20)  -  FJ            TORSIONAL CONSTANT
C     ECPT(21)  -  NSM           NON-STRUCTURAL MASS
C     ECPT(22)  -  FE            FORCE ELEM DESCRIPTIONS (FORCE METHOD)
C     ECPT(23)  -  C1            * STRESS RECOVERY COEFFICIENTS
C     ECPT(24)  -  C2            *
C     ECPT(25)  -  D1            *
C     ECPT(26)  -  D2            *
C     ECPT(27)  -  F1            *
C     ECPT(28)  -  F2            *
C     ECPT(29)  -  G1            *
C     ECPT(30)  -  G2            *
C     ECPT(31)  -  K1            $ AREA FACTORS FOR SHEAR
C     ECPT(32)  -  K2            $
C     ECPT(33)  -  I12           AREA MOMENT OF INERTIA
C     ECPT(34)  -  MCSIDA        COOR. SYS. ID. FOR GRID POINT A
C     ECPT(35)  -  GPA(3)        * BASIC COORDINATES FOR GRID POINT A
C     ECPT(36)  -    ...         *
C     ECPT(37)  -    ...         *
C     ECPT(38)  -  MCSIDB        COOR. SYS. ID. FOR GRID POINT B
C     ECPT(39)  -  GPB(3)        $ BASIC COORDINATES FOR GRID POINT B
C     ECPT(40)  -    ...         $
C     ECPT(41)  -    ...         $
C     ECPT(42)  -  ELTEMP        AVG. ELEMENT TEMPERATURE
C
      LOGICAL           ABASIC   ,BBASIC   ,BASIC    ,AOFSET ,BOFSET   ,
     1                  OFFSET
      REAL              L        ,LSQ      ,LCUBE    ,I1     ,I2       ,
     1                  K1       ,K2       ,KE       ,KEP    ,I12      ,
     2                  NSM      ,LR1      ,LR2      ,LB     ,L2B3     ,
     3                  L2B6     ,UA(6)
      DIMENSION         VECI(3)  ,VECJ(3)  ,VECK(3)  ,Z(1)   ,TA(18)   ,
     1                  TB(9)    ,ECPT(100),IECPT(38),IPIN(10)         ,
     2                  SMALV0(6)
C
C     SDR2 PHASE I INPUT AND OUTPUT COMMON BLOCK
C
      COMMON   /TRIMEX/ IELID    ,ISILNO(2),SMALLV(3),ICSSV  ,IPINFL(2),
     1                  ZA(3)    ,ZB(3)    ,IMATID   ,A      ,I1       ,
     2                  I2       ,FJ       ,NSM      ,FE     ,C1       ,
     3                  C2       ,D1       ,D2       ,F1     ,F2       ,
     4                  G1       ,G2       ,K1       ,K2     ,I12      ,
     5                  MCSIDA   ,GPA(3)   ,MCSIDB   ,GPB(3) ,TEMPEL
C
      COMMON   /SSGWRK/ KE(144)  ,KEP(144) ,DELA(6)  ,DELB(6)
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON   /MATIN / MATIDC   ,MATFLG   ,ELTEMP   ,STRESS  ,SINTH   ,
     1                  COSTH
      COMMON   /MATOUT/ E        ,G        ,NU       ,RHO     ,ALPHA   ,
     1                  T SUB 0  ,G SUB E  ,SIGT     ,SIGC    ,SIGS
      EQUIVALENCE       (IELID,ECPT(1),IECPT(1))     ,(TA(10),TB(1))
C
C
C     SET UP POINTERS TO COOR. SYS. IDS., OFFSET VECTORS, AND PIN FLAGS.
C     ICSIDA AND ICSIDB ARE COOR. SYS. IDS.
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
C     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
C     AXIS PLANE  (FMMS - 36 P. 4)
C
      FL = 0.0
      DO 50 I = 1,3
   50 FL = FL + SMALLV(I)**2
      FL = SQRT(FL)
      DO 60 I = 1,3
   60 SMALLV(I) = SMALLV(I)/FL
C
C     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
C
      ABASIC = .TRUE.
      BBASIC = .TRUE.
      IF (ICSIDA .NE. 0) ABASIC = .FALSE.
      IF (ICSIDB .NE. 0) BBASIC = .FALSE.
C
C     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
C
      IF (.NOT.ABASIC) CALL GBTRAN (ECPT(JCSIDA),ECPT(JCSIDA+1),TA)
      IF (.NOT.BBASIC) CALL GBTRAN (ECPT(JCSIDB),ECPT(JCSIDB+1),TB)
C
C     DETERMINE IF WE HAVE NON-ZERO OFFSET VECTORS.
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
C     FORM THE CENTER AXIS OF THE BEAM WITHOUT OFFSETS.
C
  100 VECI(1) = ECPT(JCSIDA+1) - ECPT(JCSIDB+1)
      VECI(2) = ECPT(JCSIDA+2) - ECPT(JCSIDB+2)
      VECI(3) = ECPT(JCSIDA+3) - ECPT(JCSIDB+3)
C
C     TRANSFORM THE OFFSET VECTORS IF NECESSARY
C
      IF (.NOT.AOFSET .AND. .NOT.BOFSET) GO TO 150
C
C     TRANSFORM THE OFFSET VECTOR FOR POINT A IF NECESSARY.
C
      IDELA = 1
      J = JOFSTA - 1
      DO 110 I = 1,3
      J = J + 1
  110 DELA(I) = ECPT(J)
      IF (ABASIC) GO TO 120
      IDELA = 4
      CALL GMMATS (TA,3,3,0, DELA(1),3,1,0, DELA(4))
C
C     TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
C
  120 IDELB = 1
      J = JOFSTB - 1
      DO 130 I = 1,3
      J = J + 1
  130 DELB(I) = ECPT(J)
      IF (BBASIC) GO TO 140
      IDELB = 4
      CALL GMMATS (TB,3,3,0, DELB(1),3,1,0, DELB(4))
C
C     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
C
  140 VECI(1) = VECI(1) + DELA(IDELA  ) - DELB(IDELB  )
      VECI(2) = VECI(2) + DELA(IDELA+1) - DELB(IDELB+1)
      VECI(3) = VECI(3) + DELA(IDELA+2) - DELB(IDELB+2)
C
C     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
C
  150 VECI(1) = -VECI(1)
      VECI(2) = -VECI(2)
      VECI(3) = -VECI(3)
      FL = SQRT (VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      DO 160 I = 1,3
  160 VECI(I) = VECI(I)/FL
C
C     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
C
      DO 165 I = 1,3
  165 SMALV0(I) = SMALLV(I)
      ISV = 1
      IF (ICSSV .EQ. 0) GO TO 180
      ISV = 4
      CALL GMMATS (TA,3,3,0, SMALV0(1),3,1,0, SMALV0(4))
C
C     COMPUTE THE K VECTOR, VECK = VECI X SMALV0, AND NORMALIZE
C
  180 VECK(1) =  VECI(2)*SMALV0(ISV+2) - VECI(3)*SMALV0(ISV+1)
      VECK(2) =  VECI(3)*SMALV0(ISV  ) - VECI(1)*SMALV0(ISV+2)
      VECK(3) =  VECI(1)*SMALV0(ISV+1) - VECI(2)*SMALV0(ISV)
      FLL     =  SQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      VECK(1) =  VECK(1)/FLL
      VECK(2) =  VECK(2)/FLL
      VECK(3) =  VECK(3)/FLL
C
C     COMPUTE THE J VECTOR, VECJ = VECK X VECI, AND NORMALIZE
C
      VECJ(1) =  VECK(2)*VECI(3) - VECK(3)*VECI(2)
      VECJ(2) =  VECK(3)*VECI(1) - VECK(1)*VECI(3)
      VECJ(3) =  VECK(1)*VECI(2) - VECK(2)*VECI(1)
      FLL     =  SQRT(VECJ(1)**2 + VECJ(2)**2 + VECJ(3)**2)
      VECJ(1) =  VECJ(1)/FLL
      VECJ(2) =  VECJ(2)/FLL
      VECJ(3) =  VECJ(3)/FLL
C
C     CALL MAT TO GET MATERIAL PROPERTIES.
C
      MATIDC = IMATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
C     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
C     CALCULATION
C
      L    = FL
      LSQ  = L**2
      LCUBE= LSQ*L
      EI1  = E*I1
      EI2  = E*I2
      IF (K1.EQ.0.0 .OR. I12 .NE.0.0) GO TO 210
      GAK1 = G*A*K1
      R1   = (12.0*EI1*GAK1)/(GAK1*LCUBE + 12.0*L*EI1)
      GO TO 220
  210 R1   =  12.0*EI1/LCUBE
  220 IF (K2.EQ.0.0 .OR. I12.NE.0.0) GO TO 230
      GAK2 = G*A*K2
      R2   = (12.0*EI2*GAK2)/(GAK2*LCUBE + 12.0*L*EI2)
      GO TO 240
  230 R2   =  12.0*EI2/LCUBE
C
C     COMPUTE THE -SMALL- K-S, SK1, SK2, SK3, AND SK4
C
  240 SK1 = 0.25*R1*LSQ + EI1/L
      SK2 = 0.25*R2*LSQ + EI2/L
      SK3 = 0.25*R1*LSQ - EI1/L
      SK4 = 0.25*R2*LSQ - EI2/L
C
C     COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
C
      AEL = A*E /L
      LR1 = L*R1/2.0
      LR2 = L*R2/2.0
      GJL = G*FJ/L
C
C     CONSTRUCT THE 12 X 12 MATRIX KE
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
      BETA = 12.0*E*I12/LCUBE
      LB   = L  *BETA/2.0
      L2B3 = LSQ*BETA/3.0
      L2B6 = LSQ*BETA/6.0
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
C     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
C
  255 KA = IECPT(JPINA)
      KB = IECPT(JPINB)
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 325
C
C     SET UP THE IPIN ARRAY
C
      DO 260 I = 1,5
      IPIN(I  ) = MOD(KA,10)
      IPIN(I+5) = MOD(KB,10) + 6
      IF (IPIN(I+5) .EQ. 6) IPIN(I+5) = 0
      KA = KA/10
  260 KB = KB/10
C
C     ALTER KE MATRIX DUE TO PIN FLAGS.
C
      DO 320 I = 1,10
      IF (IPIN(I) .EQ. 0) GO TO 320
      II = 13*IPIN(I) - 12
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
      JI = 12*(J-1) + IPIN(I)
      IJ = 12*(IPIN(I) - 1) + J
      DO 290 LL = 1,12
      JLL = 12*(J-1) + LL
      ILL = 12*(IPIN(I) - 1) + LL
      KEP(JLL) = KE(JLL) - (KE(ILL)/KE(II))*KE(JI)
  290 CONTINUE
      KEP(IJ) = 0.0
      KEP(JI) = 0.0
  300 CONTINUE
      DO 310 K = 1,144
  310 KE(K) = KEP(K)
  320 CONTINUE
C
C            E
C     STORE K   IN KEP(1),...,KEP(36) AND
C            AA
C
C            E
C     STORE K   IN KEP(37),...,KEP(72)
C            AB
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
C                                                            T
C     STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
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
C     SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
C
      BASIC = ABASIC
      JCSID = JCSIDA
      OFFSET= AOFSET
      JOFSET= JOFSTA
      IWBEG = 0
      IKEL  = 1
      ISASB = 73
      INDEX = ISILNO(1)
C
C     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
C     MATRICES WILL RESIDE.                              A      B
C
      DO 350 I = 28,108
  350 KE(I) = 0.0
C
C     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
C     G = AT X TI
C
  360 IG = 1
      IF (BASIC) GO TO 370
      CALL GBTRAN (ECPT(JCSID),ECPT(JCSID+1),KE(10))
      CALL GMMATS (KE(1),3,3,0,KE(10),3,3,0, KE(19))
      IG = 19
C
C     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
C     MATRIX.
C
  370 IF (.NOT. OFFSET) GO TO 380
      KE(10) =  0.0
      KE(11) =  ECPT(JOFSET+2)
      KE(12) = -ECPT(JOFSET+1)
      KE(13) = -KE(11)
      KE(14) =  0.0
      KE(15) =  ECPT(JOFSET)
      KE(16) = -KE(12)
      KE(17) = -KE(15)
      KE(18) =  0.0
C
C     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
C
      CALL GMMATS (KE(IG),3,3,0, KE(10),3,3,0, KE(28))
C
C     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
C               A                B
C     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
C     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
C     WILL BE STORED IN THE UPPER RIGHT CORNER.
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
      IF (.NOT.OFFSET) GO TO 390
      KE(IWBEG+40) = KE(28)
      KE(IWBEG+41) = KE(29)
      KE(IWBEG+42) = KE(30)
      KE(IWBEG+46) = KE(31)
      KE(IWBEG+47) = KE(32)
      KE(IWBEG+48) = KE(33)
      KE(IWBEG+52) = KE(34)
      KE(IWBEG+53) = KE(35)
      KE(IWBEG+54) = KE(36)
C                              E                    E
C     FORM THE PRODUCT  S  =  K   X  W   OR  S  = K   X  W  , DEPENDING
C                        A     AA     A       B    AB     B
C
C     UPON WHICH POINT WE ARE WORKING WITH.
C
  390 CALL GMMATS (KEP(IKEL),6,6,0, KE(IWBEG+37),6,6,0, KEP(ISASB))
C
C     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED.  IF
C     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
C
      IF (IWBEG .EQ. 36) GO TO 500
      BASIC  = BBASIC
      JCSID  = JCSIDB
      OFFSET = BOFSET
      JOFSET = JOFSTB
      IWBEG  = 36
      IKEL   = 37
      ISASB  = 109
      INDEX  = ISILNO(2)
      DO 400 I = 28,36
  400 KE(I) = 0.0
      GO TO 360
C
C     NOW PERFORM THE ELEMENT TEMPERATURE AND DEFORMATION LOADING.
C
  500 TBAR = TSUB0
      IF (NOGPTT .EQ. 0) GO TO 510
      CALL SSGETD (ECPT(1),KE(1),0)
      TBAR = (KE(1) + KE(2))/2.0
  510 DELTA = 0.0
      IF (NOEDT .EQ. 0) GO TO 520
      KE(3) =  0.0
      KE(4) =  0.0
      KE(5) =  0.0
      KE(6) =  0.0
      CALL FEDT (ECPT(1),DELTA,IDEFM)
      GO TO 530
  520 DELTA = 0.0
C
C     ELEMENT TEMPERATURE DATA BEGINS AT KE(1)
C     ELEMENT DEFORMATION DATA = DELTA
C
C     S  BEGINS AT KEP(73)             (6 X 6)
C      A
C
C     S  BEGINS AT KEP(109)            (6 X 6)
C      B
C
C     NOW FILL THE U  MATRIX           (6 X 1)
C                   A
C
  530 ALPHAL = ALPHA*L
C
      UA(1) = -ALPHAL*(TBAR - TSUB0) - DELTA
      UA(2) = -ALPHAL*L*(KE(3) + 2.0*KE(4))/6.0
      UA(3) = -ALPHAL*L*(KE(5) + 2.0*KE(6))/6.0
      UA(4) =  0.0
      UA(5) = -ALPHAL*(KE(5) + KE(6))/2.0
      UA(6) =  ALPHAL*(KE(3) + KE(4))/2.0
C
C     COMPUTE P  AND P  AND STORE THEM INTO Z (OPEN CORE)
C              A      B
C
      DO 600 I = 1,2
      CALL GMMATS (KEP(36*I+37),6,6,1, UA(1),6,1,0, KE(1))
      K = IECPT(I+1) - 1
      DO 550 J = 1,6
      K = K + 1
  550 Z(K) = Z(K) + KE(J)
  600 CONTINUE
C
      RETURN
      END
