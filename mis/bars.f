      SUBROUTINE BARS
C
C     SINGLE PRECISION VERSION
C
C     THIS SUBROUTINE PROCESSES BAR  ELEMENT DATA TO PRODUCE STIFFNESS
C     AND MASS MATRICES. IF THE HEAT TRANSFER OPTION IS ON, CONDUCTIVITY
C     AND CAPACITY MATRICES ARE PRODUCED.
C
C     THIS ROUTINE WILL PRODUCE MASS MATRICES BY EITHER THE CONSISTENT
C     OR CONVENTIONAL MASS METHODS.
C     THE ECPT/EST ENTRIES FOR THE BAR (ELEMENT TYPE 34) ARE
C
C     ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
C     ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
C     ECPT( 3)  -    ...          *
C     ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
C     ECPT( 5)  -    ...          $
C     ECPT( 6)  -    ...          $
C     ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
C     ECPT( 8)  -  IPINFL(2)      * PIN FLAGS
C     ECPT( 9)  -    ...          *
C     ECPT(10)  -  ZA(3)          $ OFFSET VECTOR FOR POINT A
C     ECPT(11)  -    ...          $
C     ECPT(12)  -    ...          $
C     ECPT(13)  -  ZB(3)          * OFFSET VECTOR FOR POINT B
C     ECPT(14)  -    ...          *
C     ECPT(15)  -    ...          *
C     ECPT(16)  -  IMATID         MATERIAL ID.
C     ECPT(17)  -  A              CROSS-SECTIONAL AREA
C     ECPT(18)  -  I1             $ AREA MOMENTS OF INERTIA
C     ECPT(19)  -  I2             $
C     ECPT(20)  -  FJ             TORSIONAL CONSTANT
C     ECPT(21)  -  NSM            NON-STRUCTURAL MASS
C     ECPT(22)  -  FE             FORCE ELEM DESCRIPTIONS (FORCE METHOD)
C     ECPT(23)  -  C1             * STRESS RECOVERY COEFFICIENTS
C     ECPT(24)  -  C2             *
C     ECPT(25)  -  D1             *
C     ECPT(26)  -  D2             *
C     ECPT(27)  -  F1             *
C     ECPT(28)  -  F2             *
C     ECPT(29)  -  G1             *
C     ECPT(30)  -  G2             *
C     ECPT(31)  -  K1             $ AREA FACTORS FOR SHEAR
C     ECPT(32)  -  K2             $
C     ECPT(33)  -  I12            AREA MOMENT OF INERTIA
C     ECPT(34)  -  MCSIDA         COOR. SYS. ID. FOR GRID POINT A
C     ECPT(35)  -  GPA(3)         * BASIC COORDINATES FOR GRID POINT A
C     ECPT(36)  -    ...          *
C     ECPT(37)  -    ...          *
C     ECPT(38)  -  MCSIDB         COOR. SYS. ID. FOR GRID POINT B
C     ECPT(39)  -  GPB(3)         $ BASIC COORDINATES FOR GRID POINT B
C     ECPT(40)  -    ...          $
C     ECPT(41)  -    ...          $
C     ECPT(42)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
C
      LOGICAL         BASIC,OFFSET,NOGO,AOFSET,BOFSET
      INTEGER         DICT(7),IS12OR(4),IS21OR(4),GSUBE,ESTID,IECPT(38)
      REAL            K1,K2,I1,I2,I12,NSM,KE,KK,KEP,M,MEP,ME,LR1,LR2,LB,
     1                L2B3,L2B6,LIMIT
      DIMENSION       VECI(3),VECJ(3),VECK(3),ECPT(42),IPIN(10),IKK(4)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /SYSTEM/ KSYSTM(100)
      COMMON /EMGEST/ IELID,ISILNO(2),SMALLV(3),ICSSV,IPINFL(2),ZA(3),
     1                ZB(3),IMATID,A,I1,I2,FJ,NSM,FE,C1,C2,D1,D2,F1,F2,
     2                G1,G2,K1,K2,I12,MCSIDA,GPA(3),MCSIDB,GPB(3),TEMPEL
      COMMON /EMGPRM/ IXTRA,JCORE,NCORE,DUM(12),ISTIF,IMASS,IDAMP,
     1                IPREC,NOGO,HEAT,ICMBAR,LCSTM,LMAT,LHMAT
      COMMON /EMGDIC/ IDUMM, LDICT,NGRIDS,ELID,ESTID
      COMMON /EMGTRX/ KE(144),KEP(144),M(12,12),ME(144),MEP(144),
     1                KK(144),SMALVN(6),TA(18),TB(9),VEC(10),
     2                DELA(6),DELB(6)
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E,G,NU,RHO,ALPHA,TSUBO,GSUBE,SIGT,SIGC,SIGS
      COMMON /HMTOUT/ FK
      EQUIVALENCE     (KSYSTM(2),IOUTPT), (KSYSTM(56),IHEAT),
     1                (ECPT(1),IECPT(1),IELID), (KSYSTM(87),KSY87),
     2                (VEC(1),VECI(1)), (VEC(4),VECJ(1)),
     3                (VEC(7),VECK(1))
      DATA    IKK   / 1,7,73,79   /, EPSI,EPSI2 / 1.0E-18,1.0E-7 /
      DATA    IS12OR/ 1,37,109,73 /, IS21OR     / 73,109,37,1    /
C
C
      DICT(1) = ESTID
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
      LIMIT  = IABS(KSY87)*.01
C
C     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
C     AXIS PLANE  (FMMS - 36 P. 4)
C
      FL = SQRT(SMALLV(1)**2 + SMALLV(2)**2 + SMALLV(3)**2)
      IF (ABS(FL) .LT. EPSI) GO TO 7770
      DO 50 I = 1,3
   50 SMALVN(I) = SMALLV(I)/FL
C
C     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES OR NOT.
C     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
C
      IF (ICSIDA .NE. 0) CALL TRANSS (ECPT(JCSIDA),TA)
      IF (ICSIDB .NE. 0) CALL TRANSS (ECPT(JCSIDB),TB)
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
  100 DO 105 I = 1,3
      JTA = I + JCSIDA
      JTB = I + JCSIDB
  105 VECI(I) = ECPT(JTA) - ECPT(JTB)
C
C     SAVE IN A2B THE LENGTH OF BAR WITHOUT OFFSET, FROM GRID PT. A TO B
C
      A2B = SQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
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
      IF (ICSIDA .EQ.0) GO TO 120
      IDELA = 4
      CALL GMMATS (TA,3,3,0,DELA(1),3,1,0,DELA(4))
C
C     TRANSFORM THE OFFSET VECTOR FOR POINT B IF NECESSARY
C
  120 IDELB = 1
      J = JOFSTB - 1
      DO 130 I = 1,3
      J = J + 1
  130 DELB(I) = ECPT(J)
      IF (ICSIDB .EQ. 0) GOTO 140
      IDELB = 4
      CALL GMMATS (TB,3,3,0,DELB(1),3,1,0, DELB(4))
C
C     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
C
  140 DO 145 I = 1,3
      JTA = I - 1 + IDELA
      JTB = I - 1 + IDELB
  145 VECI(I) = VECI(I)+DELA(JTA) - DELB(JTB)
C
C     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
C
  150 FL = 0.
      DO 155 I = 1,3
      VECI(I) = - VECI(I)
  155 FL = FL + VECI(I)**2
      FL = SQRT(FL)
      IF (ABS(FL) .LT. EPSI) GO TO 7770
      DO 160 I = 1,3
  160 VECI(I) = VECI(I)/FL
C
C     NOW THAT LENGTH HAS BEEN COMPUTED, CHECK POSSIBLE OFFSET ERROR
C     ISSUE WARNING MESSAGE IF OFFSET EXCEEDS A2B BY 'LIMIT' PERCENT.
C     (DEFAULT IS 15 PERCENT, KSYSTM(87) WORD)
C
      IF (ABS(FL-A2B)/A2B .LE. LIMIT) GO TO 170
      WRITE  (IOUTPT,165) UWM,IELID
  165 FORMAT (A25,' - UNUSUALLY LARGE OFFSET IS DETECTED FOR CBAR ',
     1       'ELEMENT ID =',I8,' ***')
      IF (KSY87 .LE. 0) GO TO 170
      WRITE  (IOUTPT,167) KSY87
  167 FORMAT (/5X,'(OFFSET BAR LENGTH EXCEEDS NON-OFFSET LENGTH BY',
     1        I4,' PERCENT, SET BY SYSTEM 87TH WORD)')
      KSY87 = -KSY87
C
C     BRANCH IF THIS IS A -HEAT- FORMULATION.
C
  170 IF (IHEAT .EQ.1) GO TO 500
C
C     COMPUTE THE  SMALV0  VECTOR
C
      ISV = 1
      IF (ICSSV .EQ. 0) GO TO 180
      ISV = 4
      CALL GMMATS (TA,3,3,0,SMALVN(1),3,1,0,SMALVN(4))
C
C     COMPUTE THE K VECTOR, VECK = VECI  X  SMALV0, AND NORMALIZE
C
  180 VECK(1) = VECI(2)*SMALVN(ISV+2) - VECI(3)*SMALVN(ISV+1)
      VECK(2) = VECI(3)*SMALVN(ISV  ) - VECI(1)*SMALVN(ISV+2)
      VECK(3) = VECI(1)*SMALVN(ISV+1) - VECI(2)*SMALVN(ISV  )
      FLL= SQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      IF (ABS(FLL) .LT. EPSI2) GO TO 7770
      DO 190 I = 1,3
  190 VECK(I) = VECK(I)/FLL
C
C     COMPUTE THE J VECTOR, VECJ = VECK  X  VECI, AND NORMALIZE
C
      VECJ(1) = VECK(2)*VECI(3) - VECK(3)*VECI(2)
      VECJ(2) = VECK(3)*VECI(1) - VECK(1)*VECI(3)
      VECJ(3) = VECK(1)*VECI(2) - VECK(2)*VECI(1)
      FLL = SQRT(VECJ(1)**2 + VECJ(2)**2 + VECJ(3)**2)
      IF (ABS(FLL) .LT. EPSI2) GO TO 7770
      VECJ(1) = VECJ(1)/FLL
      VECJ(2) = VECJ(2)/FLL
      VECJ(3) = VECJ(3)/FLL
C
C     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
C     CONSTANT.
C
      MATIDC = IMATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
      IF (ISTIF .EQ. 0) GOTO 600
C
C     IF ELASTICITY AND SHEAR MODULES BOTH ZERO, SKIP STIFFNESS
C     CALCULATION
C
      IF (E.EQ.0. .AND. G.EQ.0.) GO TO 600
C
C     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
C     CALCULATION
C
      ASSIGN 305 TO K OR M
  205 BL    = FL
      BLSQ  = FL**2
      BLCUBE= BLSQ*BL
C
C     COMPUTE SOME TERMS TO BE USED IN STIFFNESS MATRIX KE
C
      EI1  = E*I1
      EI2  = E*I2
      IF (K1.EQ.0.0 .OR. I12.NE.0.0) GO TO 210
      GAK1 = G*A*K1
      R1   = (12.*EI1*GAK1)/(GAK1*BLCUBE + 12.*BL*EI1)
      GO TO 220
  210 R1   = 12.*EI1/BLCUBE
  220 IF (K2.EQ.0.0 .OR. I12.NE.0.0) GO TO 230
      GAK2 = G*A*K2
      R2   = (12.*EI2*GAK2)/(GAK2*BLCUBE + 12.*BL*EI2)
      GO TO 240
  230 R2   = 12.*EI2/BLCUBE
C
  240 SK1 = .25*R1*BLSQ + EI1/BL
      SK2 = .25*R2*BLSQ + EI2/BL
      SK3 = .25*R1*BLSQ - EI1/BL
      SK4 = .25*R2*BLSQ - EI2/BL
C
      AEL =  A*E/BL
      LR1 =  BL*R1/2.
      LR2 =  BL*R2/2.
      GJL =  G*FJ/BL
C
C     CONSTRUCT  THE GENERAL 12X12 MATRIX FOR THE BAR ELEMENT
C
C                      **       **
C                      * K    K  *
C                      *  AA   AB*
C                K =   *  T      *
C                      * K    K  *
C                      *  AB   BB*
C                      **       **
C
C
C     FIRST SET THE COMPONENT CODE AND THE DOF
C
      ICODE = 63
      NDOF  = 12
      NSQ   = NDOF**2
C
C     CONSTRUCT THE 12 X 12 MATRIX KE
C
      DO 300 I = 1,144
  300 KE(I)   = 0.
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
      IF (I12 .EQ. 0.) GO TO 303
      BETA    =  12.*E*I12/BLCUBE
      LB      =  BL*BETA/2.0
      L2B3    =  BLSQ*BETA/3.0
      L2B6    =  BLSQ*BETA/6.0
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
  303 GO TO K OR M, (305,640,465,750)
C
C     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
C
  305 KA = IECPT(JPINA)
      KB = IECPT(JPINB)
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 345
C
C     SET UP THE IPIN ARRAY
C
      DO 310 I = 1,5
      IPIN(I  ) = MOD(KA,10)
      IPIN(I+5) = MOD(KB,10) + 6
      IF (IPIN(I+5) .EQ. 6) IPIN(I+5) = 0
      KA = KA/10
  310 KB = KB/10
C
C     ALTER KE MATRIX DUE TO PIN FLAGS.
C
      DO 340 I = 1,10
      IF (IPIN(I) .EQ.0) GO TO 340
      II = 13*IPIN(I) - 12
      IF (KE(II) .NE. 0.) GO TO 320
      IL = IPIN(I)
      II = II - IL
      DO 315 J = 1,12
      II = II + 1
      KE(II) = 0.
      KE(IL) = 0.
      IL = IL + 12
  315 CONTINUE
      GO TO 340
  320 DO 330 J = 1,12
      JI = 12*(J-1) + IPIN(I)
      IJ = 12*(IPIN(I)-1) + J
      DO 325 LL = 1,12
      JLL = 12*(J-1) + LL
      ILL = 12*(IPIN(I)-1) + LL
      KEP(JLL) = KE(JLL) - (KE(ILL)/KE(II))*KE(JI)
  325 CONTINUE
      KEP(IJ) = 0.
      KEP(JI) = 0.
  330 CONTINUE
      DO 335 K = 1,144
  335 KE(K) = KEP(K)
  340 CONTINUE
C
C     DIVIDE KE INTO FOUR SUBMATRICES AND STORE IN OPEN CORE
C
C      E                   E                   E
C     K   = KK(1 TO 36)   K   = KK(37 TO 72)  K   = KK(73 TO 108)
C      AA                  AB                  BA
C
C      E
C     K   =  KK(109 TO 144)
C      BB
C
C
  345 J = 0
      DO 355 I = 1,72,12
      LOW = I
      LIM = I + 5
      DO 350 K = LOW,LIM
      J = J + 1
      KK(J    ) = KE(K   )
      KK(J+ 36) = KE(K+ 6)
      KK(J+ 72) = KE(K+72)
  350 KK(J+108) = KE(K+78)
  355 CONTINUE
C
      ASSIGN 465 TO K OR M
C
C     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W AND W  6 X 6
C     MATRICES WILL RESIDE.                                A      B
C      T
C     A   MATRIX NOW STORED IN KE
C
  358 DO 357 I = 1,9
  357 KE(I) = VEC(I)
C
      DO 360 I = 28,144
  360 KE(I) = 0.
C
C     SET UP POINTERS
C
      BASIC  = ICSIDA.EQ.0
      JCSID  = JCSIDA
      OFFSET = AOFSET
      JOFSET = JOFSTA
      DO 395 I = 1,2
      IWBEG = I*36
C
C     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
C     G = AT X TI
C
      IG = 1
      IF (BASIC) GO TO 380
      CALL TRANSS (ECPT(JCSID),KE(10))
      CALL GMMATS (KE(1),3,3,0, KE(10),3,3,0, KE(19) )
      IG = 19
C
C     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
C     MATRIX.
C
  380 IF (.NOT.OFFSET) GO TO 385
      KE(10) =  0.
      KE(11) =  ECPT(JOFSET+2)
      KE(12) = -ECPT(JOFSET+1)
      KE(13) = -KE(11)
      KE(14) =  0.
      KE(15) =  ECPT(JOFSET)
      KE(16) = -KE(12)
      KE(17) = -KE(15)
      KE(18) =  0.
C
C     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
C
      CALL GMMATS (KE(IG),3,3,0, KE(10),3,3,0,KE(28))
C
C
C     FORM THE W SUBMATRICES IN KE(37) AND KE(73)
C
C
  385 KE(IWBEG+ 1) = KE(IG  )
      KE(IWBEG+ 2) = KE(IG+1)
      KE(IWBEG+ 3) = KE(IG+2)
      KE(IWBEG+ 7) = KE(IG+3)
      KE(IWBEG+ 8) = KE(IG+4)
      KE(IWBEG+ 9) = KE(IG+5)
      KE(IWBEG+13) = KE(IG+6)
      KE(IWBEG+14) = KE(IG+7)
      KE(IWBEG+15) = KE(IG+8)
      KE(IWBEG+22) = KE(IG  )
      KE(IWBEG+23) = KE(IG+1)
      KE(IWBEG+24) = KE(IG+2)
      KE(IWBEG+28) = KE(IG+3)
      KE(IWBEG+29) = KE(IG+4)
      KE(IWBEG+30) = KE(IG+5)
      KE(IWBEG+34) = KE(IG+6)
      KE(IWBEG+35) = KE(IG+7)
      KE(IWBEG+36) = KE(IG+8)
      IF (.NOT.OFFSET) GO TO 390
      KE(IWBEG+ 4) = KE(28)
      KE(IWBEG+ 5) = KE(29)
      KE(IWBEG+ 6) = KE(30)
      KE(IWBEG+10) = KE(31)
      KE(IWBEG+11) = KE(32)
      KE(IWBEG+12) = KE(33)
      KE(IWBEG+16) = KE(34)
      KE(IWBEG+17) = KE(35)
      KE(IWBEG+18) = KE(36)
  390 BASIC  = ICSIDB.EQ.0
      JCSID  = JCSIDB
      OFFSET = BOFSET
      JOFSET = JOFSTB
  395 CONTINUE
C
C     CONVERT THE K PARTITIONS TO GLOBAL COORDINATES AND STORE IN KEP
C
      IAFT = 37
      DO 400 I = 1,4
      IKX  = (I-1)*36 + 1
      IK   = IKX
      IF (I .GE. 3) IKX = (7-I-1)*36  + 1
      IFORE = ((I-1)/2)*36 + 37
      CALL GMMATS (KE(IFORE),6,6,1, KK(IKX),6,6,0, KE(109))
      CALL GMMATS (KE(109), 6,6,0, KE(IAFT),6,6,0, KEP(IK))
      IAFT = 73
      IF (I .EQ. 3) IAFT = 37
  400 CONTINUE
C
C     REFORM THE K MATRIX (12X12) FROM THE FOUR SUBMATRICES (6X6) AND
C     ORDER  THE SUBMATRICES BY INCREASING SIL VALUE
C
      DO 460 II = 1,4
      IX1 = IKK(II)
      IX2 = IX1 + 60
      IS  = IS12OR(II)
      IF (ISILNO(1) .GT. ISILNO(2)) IS = IS21OR(II)
      DO 450 I = IX1,IX2,12
      IP5 = I + 5
      DO 440 J = I,IP5
      KE(J) = KEP(IS)
  440 IS = IS + 1
  450 CONTINUE
  460 CONTINUE
C
      GO TO K OR M, (305,640,465,750)
C
C     OUTPUT THE STIFFNESS MATRIX
C
  465 DICT(2) = 1
      DICT(3) = NDOF
      DICT(4) = ICODE
      DICT(5) = GSUBE
      CALL EMGOUT (KE(1),KE(1),NSQ,1,DICT,1,IPREC)
      GO TO 600
C
C     THE MASS MATRIX IS GENERATED HERE.  IF THE PARAMETER ICMBAR IS
C     .LT. 0, CALL THE CONVENTIONAL MASS MATRIX GENERATION ROUTINE FOR
C     THE BAR.  OTHERWISE CALL THE ROUTINE TO GENERATE CONSISTENT MASS
C     MATRICES FOR THE BAR.
C
  600 CONST = (FL*(RHO*A + NSM))/420.
      IF (IMASS.EQ.0 .OR. CONST.EQ.0.) RETURN
      IF (ICMBAR .LT. 0) GO TO 800
C
C     CALCULATE THE CONSISTENT/CONVENTIONAL MASS MATRIX
C
C     CALL THE MAT ROUTINE TO FETCH SINGLE PRECISION MATERIAL PROPERTIES
C
      MATIDC = IMATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
C
C     COMPUTE TERMS OF THE ELEMENT MASS MATRIX
C
      BL22  = 22.*FL
      BL13  = 13.*FL
      BLSQ4 = 4.0*FL**2
      BLSQ3 = 3.0*FL**2
C
C     CONSTRUCT THE ELEMENT MASS MATRIX.
C
      DO 610 I = 1,12
      DO 610 J = 1,12
  610 M( I, J) = 0.
      M( 1, 1) = 175.
      M( 1, 7) = 35.
      M( 2, 2) = 156.
      M( 2, 6) = BL22
      M( 2, 8) = 54.
      M( 2,12) =-BL13
      M( 3, 3) = 156.
      M( 3, 5) =-BL22
      M( 3, 9) = 54.
      M( 3,11) = BL13
      M( 5, 5) = BLSQ4
      M( 5, 9) =-BL13
      M( 5,11) =-BLSQ3
      M( 6, 6) = BLSQ4
      M( 6, 8) = BL13
      M( 6,12) =-BLSQ3
      M( 7, 7) = 175.
      M( 8, 8) = 156.
      M( 8,12) =-BL22
      M( 9, 9) = 156.
      M( 9,11) = BL22
      M(11,11) = BLSQ4
      M(12,12) = BLSQ4
C
C     STORE THE UPPER TRIANGULAR PART OF THE MATRIX IN THE LOWER PART.
C
      DO 625 I = 1,10
      LOW = I + 1
      DO 620 J = LOW,12
      M(J,I) = M(I,J)
  620 CONTINUE
  625 CONTINUE
C
C     MULTIPLY BY CONSTANT AND STORE ROW-WISE IN THE ARRAY ME
C
      K = 0
      DO 630 I = 1,12
      DO 630 J = 1,12
      K = K + 1
  630 ME(K) = CONST*M(I,J)
C
C     IF THERE ARE NO PIN FLAGS THERE IS NO NEED TO CALCULATE THE
C     ELEMENT STIFFNESS MATRIX
C
      KA = IECPT(JPINA)
      KB = IECPT(JPINB)
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 705
C
C     COMPUTE THE STIFFNESS MATRIX KE
C
      ASSIGN 640 TO K OR M
      GO TO 205
C
C     RETURN HERE AFTER COMPUTING THE STIFFNESS MATRIX
C
C
C     SET UP TNHE IPIN ARRAY
C
  640 DO 645 I = 1,5
      IPIN(I  ) = MOD(KA,10)
      IPIN(I+5) = MOD(KB,10) + 6
      IF (IPIN(I+5) .EQ. 6) IPIN(I+5) = 0
      KA = KA/10
  645 KB = KB/10
C
C     ALTER THE ELEMENT MASS MATRIX DUE TO PIN FLAGS.  NOTE THAT THE
C     FOLLOWING CODE IS CONGRUENT AS IT WERE TO THE CODE IN SUBROUTINE
C     DBEAM IN THE DSMG1 MODULE.
C
      DO 700 J = 1,10
      IF (IPIN(J) .EQ. 0) GO TO 700
      JJ = 12*(IPIN(J)-1) + IPIN(J)
      IF (KE(JJ) .EQ. 0.) GO TO 680
      DO 660 I = 1,12
      JI = 12*(IPIN(J)-1) + I
      IJ = 12*(I-1) + IPIN(J)
      DO 650 L1 = 1,12
      IL = 12*(I-1) + L1
      LJ = 12*(L1-1) + IPIN(J)
      MEP(IL) = ME(IL) - KE(LJ)*ME(JI)/KE(JJ) - KE(JI)*ME(LJ)/KE(JJ)
     2                 + KE(LJ)*KE(JI)*ME(JJ)/KE(JJ)**2
  650 CONTINUE
  660 CONTINUE
      DO 670 K = 1,144
  670 ME(K) = MEP(K)
C
C     ZERO OUT THE IPIN(J) TH ROW AND COLUMN OF ME
C
  680 J1 = JJ - IPIN(J)
      J2 = IPIN(J)
      DO 690 K = 1,12
      J1 = J1 + 1
      ME(J1) = 0.
      ME(J2) = 0.
  690 J2 = J2 + 12
  700 CONTINUE
C
C            E                  E                    E
C     STORE M  AT KK(1 TO 36), M  AT KK (37 TO 72), M  AT KK(73 TO 108)
C            AA                 AB                   BA
C
C            E
C     AND   M  AT KK(109 TO 144)
C            BB
C
  705 J = 0
      DO 720 I = 1,72,12
      LOW = I
      LIM = LOW + 5
      DO 710 K = LOW,LIM
      J = J + 1
      KK(J) = ME(K)
      KK(J+ 36) = ME(K+ 6)
      KK(J+ 72) = ME(K+72)
  710 KK(J+108) = ME(K+78)
  720 CONTINUE
C
C     CALCULATE THE TRANSFORMATION VECTORS
C
      ASSIGN 750 TO K OR M
      GO TO 358
C
C     OUTPUT THE CONSISTENT MASS MATRIX
C
  750 DICT(2) = 1
      DICT(3) = NDOF
      DICT(4) = ICODE
      DICT(5) = 0
      CALL EMGOUT (KE(1),KE(1),144,1,DICT,2,IPREC)
      RETURN
C
C     CALCULATE THE CONVENTIONAL MASS MATRIX HERE
C
C     GET RHO FROM MPT BY CALLING MAT
C
  800 MATIDC = IMATID
      MATFLG = 4
      ELTEMP = TEMPEL
      CALL MAT (ECPT(1))
      DO 810 I = 1,72
  810 MEP(I) = 0.
      FM = .5*FL*(RHO*A + NSM)
C
C     DETERMINE IF THE GRID POINT IS ASSOCIATED WITH A NON-ZERO OFFSET.
C
      JOFSET = 9
      DO 850 II = 1,2
      IX = (II-1)*36
      J  = JOFSET
      DO 815 I = 1,3
      J = J + 1
      IF (ECPT(J) .NE. 0.) GO TO 820
  815 CONTINUE
      GO TO 840
C
C     FORM UPPER RIGHT CORNER OF THE MATRIX
C
  820 MEP(IX+ 1) =  1.
      MEP(IX+ 8) =  1.
      MEP(IX+15) =  1.
      MEP(IX+ 5) =  ECPT(JOFSET+3)
      MEP(IX+ 6) = -ECPT(JOFSET+2)
      MEP(IX+12) =  ECPT(JOFSET+1)
      MEP(IX+10) = -MEP(IX+ 5)
      MEP(IX+16) = -MEP(IX+ 6)
      MEP(IX+17) = -MEP(IX+12)
      MEP(IX+20) = -MEP(IX+ 5)
      MEP(IX+21) = -MEP(IX+ 6)
      MEP(IX+25) = -MEP(IX+10)
      MEP(IX+27) = -MEP(IX+12)
      MEP(IX+31) = -MEP(IX+16)
      MEP(IX+32) = -MEP(IX+17)
      MEP(IX+22) =  ECPT(JOFSET+3)**2 + ECPT(JOFSET+2)**2
      MEP(IX+29) =  ECPT(JOFSET+3)**2 + ECPT(JOFSET+1)**2
      MEP(IX+36) =  ECPT(JOFSET+2)**2 + ECPT(JOFSET+1)**2
      MEP(IX+23) = -ECPT(JOFSET+1)*ECPT(JOFSET+2)
      MEP(IX+24) = -ECPT(JOFSET+1)*ECPT(JOFSET+3)
      MEP(IX+30) = -ECPT(JOFSET+2)*ECPT(JOFSET+3)
      MEP(IX+28) =  MEP(IX+23)
      MEP(IX+34) =  MEP(IX+24)
      MEP(IX+35) =  MEP(IX+30)
C
C     MULTIPLY M BY THE CONSTANT FL
C
      DO 830 I = 1,36
      IS = IX + I
  830 MEP(IS) = MEP(IS)*FM
      GO TO 850
C
C     HERE WE HAVE A ZERO OFFSET VECTOR
C
  840 MEP(IX+ 1) = FM
      MEP(IX+ 8) = FM
      MEP(IX+15) = FM
  850 JOFSET = 12
C
C     INSERT M  AND M  SUBMATRICES INTO M ACCORDING TO INCREASING SIL
C             A      B
C
      DO 860 I = 1,144
  860 ME(I) = 0.
C
      IF (ISILNO(1)-ISILNO(2)) 870,870,880
  870 IX1 = 1
      IX2 = 37
      GO TO 890
  880 IX1 = 37
      IX2 = 1
  890 CONTINUE
      DO 900 JJ = 1,36
      MM = MOD(JJ,6)
      IF (MM .EQ. 0) MM = 6
      I = ((JJ-1)/6)*12  + MM
      J = I + 78
      ME(I) = MEP(IX1)
      ME(J) = MEP(IX2)
      IX1 = IX1 + 1
  900 IX2 = IX2 + 1
C
C     OUTPUT THE CONVENTIONAL MASS MATRIX
C
      DICT(2) = 1
      DICT(3) = NDOF
      DICT(4) = ICODE
      DICT(5) = 0
C
      CALL EMGOUT ( ME,ME,144,1,DICT,2,IPREC)
C
      RETURN
C
C     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
C     HMAT
C
  500 MATFLG  = 1
      MATIDC  = IECPT(16)
      ELTEMP  = ECPT(42)
      DICT(2) = 1
      DICT(3) = 2
      DICT(4) = 1
      DICT(5) = 0
      IF (ISTIF .EQ. 0) GO TO 540
      CALL HMAT (IELID)
C
      KK(1) = FK*ECPT(17)/FL
      IF (KK(1) .EQ. 0.) GO TO 520
      KK(2) =-KK(1)
      KK(3) = KK(2)
      KK(4) = KK(1)
      CALL EMGOUT (KK(1),KK(1),4,1,DICT,1,IPREC)
C
  520 MATFLG = 4
C
C     ERROR IN NEXT CARD FOR HEAT FORMULATION. REMOVED BY
C     G.CHAN/UNISYS, 1984
C     ALSO, CHANGE  GO TO 520  TO 540, 11-TH CARD ABOVE, AND
C     CALL EMGOUT BELOW AND WRITE TO THE 3-RD FILE INSTEAD OF THE 2-ND.
C
C
      CALL HMAT (IELID)
      KK(1) = (FK*ECPT(17))*FL/2.
      IF (KK(1) .EQ. 0.) RETURN
      KK(2) = KK(1)
      DICT(2) = 2
      CALL EMGOUT (KK(1),KK(1),2,1,DICT,3,IPREC)
  540 RETURN
C
C     ERROR RETURNS
C
 7770 CONTINUE
      WRITE  (IOUTPT,7775) UFM,IELID
 7775 FORMAT (A23,' 3176, BAR ELEMENT ID',I9,
     1       ' HAS ILLEGAL GEOMETRY OR CONNECTIONS.')
      NOGO = .TRUE.
      RETURN
      END
