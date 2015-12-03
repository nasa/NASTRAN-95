      SUBROUTINE DBAR
C
C     THIS ROUTINE COMPUTES THE 2 6X6 DIFFERENTIAL STIFFNESS MATRICES
C     K(NPVT,NPVT) AND K(NPVT,J) FOR A BEAM HAVING END POINTS OF SIL
C     NOS. NPVT AND J.
C
C     ECPT FOR THE BEAM
C
C     ECPT( 1)  -  IELID          ELEMENT ID. NO.
C     ECPT( 2)  -  ISILNO(2)      SCALAR INDEX NOS.
C     ECPT( 3)  -   ...
C     ECPT( 4)  -  SMALLV(3)      REFERENCE VECTOR
C     ECPT( 5)  -   ...
C     ECPT( 6)  -   ...
C     ECPT( 7)  -  IGSUB0         OPTION FOR DEFINING REFERENCE NUMBER.
C     ECPT( 8)  -  IPINFL(2)      PIN FLAGS
C     ECPT( 9)  -   ...
C     ECPT(10)  -  ZA(3)          OFFSET VECTOR AT POINT A
C     ECPT(11)  -   ...
C     ECPT(12)  -   ...
C     ECPT(13)  -  ZB(3)          OFFSET VECTOR AT POINT B
C     ECPT(14)  -   ...
C     ECPT(15)  -   ...
C     ECPT(16)  -  GEF(4)         ECCENTRICITIES FOR FORCE
C     ECPT(17)  -   ...
C     ECPT(18)  -   ...
C     ECPT(19)  -   ...
C     ECPT(20)  -  IMATID         MATERIAL ID.
C     ECPT(21)  -  A              CROSS-SECTIONAL AREA
C     ECPT(22)  -  C1             STRESS COEFFICIENTS
C     ECPT(23)  -  C2                    ...
C     ECPT(24)  -  I1             AREA MOMENTS OF INERTIA
C     ECPT(25)  -  I2                    ...
C     ECPT(26)  -  I3                    ...
C     ECPT(27)  -  FJ             TORSIONAL CONSTANT
C     ECPT(28)  -  FMU            NON-STRUCTURAL MASS
C     ECPT(29)  -  K1             AREA FACTORS FOR SHEAR
C     ECPT(30)  -  K2                    ...
C     ECPT(31)  -  C3 (D1)        STRESS COEFFICIENTS
C     ECPT(32)  -  C4 (D2)               ...
C     ECPT(33)  -  B1             WIDTHS FOR FORCE
C     ECPT(34)  -  B2                    ...
C     ECPT(35)  -  HS1            DEPTHS FOR FORCE
C     ECPT(36)  -  HS2                   ...
C     ECPT(37)  -  HT1                   ...
C     ECPT(38)  -  HT2                   ...
C     ECPT(39)  -  MCSIDA         COOR. SYS. ID. FOR GRID PT. A
C     ECPT(40)  -  GPA(3)         BASIC COORDINATES FOR PT. A
C     ECPT(41)  -   ...                  ...
C     ECPT(42)  -   ...                  ...
C     ECPT(43)  -  MCSIDB         COOR. SYS. ID. FOR GRID PT. B
C     ECPT(44)  -  GPB(3)         BASIC COORDINATES FOR PT. B
C     ECPT(45)  -   ...                  ...
C     ECPT(46)  -   ...                  ...
C     ECPT(47)  -  ELTEMP         ELEMENT TEMPERATURE
C     ECPT(48)  -  ELDEF          ELEMENT DEFORMATION
C     ECPT(49)  -  TEMPER         ELEMENT LOADING TEMPERATURE
C     ECPT(50)  -  UAS(1)                ...
C     ECPT(51)  -  UAS(2)                ...
C     ECPT(52)  -  UAS(3)         SINGLE PRECISION DISPLACEMENTS
C     ECPT(53)  -  UAS(4)               FOR GRID POINT A
C     ECPT(54)  -  UAS(5)                ...
C     ECPT(55)  -  UAS(6)                ...
C     ECPT(56)  -  UBS(1)                ...
C     ECPT(57)  -  UBS(2)                ...
C     ECPT(58)  -  UBS(3)         SINGLE PRECISION DISPLACEMENTS
C     ECPT(59)  -  UBS(4)               FOR GRID POINT B
C     ECPT(60)  -  UBS(5)                ...
C     ECPT(61)  -  UBS(6)                ...
C
      LOGICAL          ABASIC,BBASIC,BASIC,AOFSET,BOFSET,OFFSET
      REAL             K1,K2,I1,I2
      DOUBLE PRECISION TA(18),TB(9),SMALV0(6),DELA,DELB,KE,KEP,VECI,
     1                 VECJ,VECK,FL,FLL,EI1,EI2,GAK1,GAK2,RRV1,RRV2,
     2                 SK1,SK2,SK3,SK4,TERM1,TERM2,TERM3,TERM4,L,LSQ,
     3                 LCUBE,DP(8)
      DOUBLE PRECISION E,DA,ALPHA,T SUB 0,SA(72),SB(36),UA(6),UB(6),
     1                 DPVECA(6),DPVECB(6),FX,VY,VZ,MAY,MAZ,MBY,MBZ,
     2                 KD(144),KC(12,12),TERM5,TERM6,TERM7,TERM8,TERM9,
     3                 TERM10,TERM11,KES(144),KDP(144),DFJ
      DIMENSION        VECI(3),VECJ(3),VECK(3),ECPT(100),IECPT(100),
     1                 IPIN(10),IZ(1)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /DS1AAA/  NPVT,ICSTM,NCSTM,DUMCL(32),NOGO
      COMMON /DS1AET/  IELID,ISILNO(2),SMALLV(3),IGSUB0,IPINFL(2),ZA(3),
     1                 ZB(3),GEF(4),IMATID,A,DUMMY1,DUMMY2,I1,I2,DUMMY3,
     2                 FJ,FMU,K1,K2,DUM2(8),MCSIDA,GPA(3),MCSIDB,GPB(3),
     3                 TEMPEL,ELDEF,TEMPER,UAS(6),UBS(6),DUM3(38)
      COMMON /DS1ADP/  KE(144),KEP(144),DELA(6),DELB(6)
      COMMON /MATIN /  MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  E S,G S,NU,RHO,ALPHA S,T SUB 0 S,GSUBE,SIGT,
     1                 SIGC,SIGS
      EQUIVALENCE      (IELID,ECPT(1),IECPT(1)),(IZ(1),Z(1)),
     1                 (TA(10),TB(1)),(ECPT(71),DP(1)),(KC(1,1),KD(1)),
     2                 (SA(37),SB(1))
C
C     DETERMINE WHICH SIL IS THE PIVOT POINT.
C
C     IPVT = 0
      IPVT = 1
      IF (ISILNO(1) .EQ. NPVT) GO TO 20
      IPVT = 2
      IF (ISILNO(2) .NE. NPVT) CALL MESAGE (-30,34,IECPT(1))
C
C     JCSIDA IS AN INDEX WHICH POINTS TO THE COOR. SYS. ID. OF POINT A.
C     JOFSTA IS AN INDEX WHICH POINTS TO THE OFFSET VECTOR FOR POINT A.
C     SIMILARY FOR JCSIDB AND JOFSTB AND POINT B.
C
   20 JCSIDA = 39
      JCSIDB = 43
      JOFSTA = 10
      JOFSTB = 13
      JPINA  = 8
      JPINB  = 9
      ICSIDA = IECPT(JCSIDA)
      ICSIDB = IECPT(JCSIDB)
C
C     NORMALIZE THE REFERENCE VECTOR WHICH LIES IN THE FIRST PRINCIPAL
C     AXIS PLANE  (FMMS - 36 P. 4)
C     WE STORE SMALLV IN SMALV0 SO THAT ARITHMETIC WILL BE DOUBLE
C     PRECISION
C
      DO 50 I = 1,3
   50 SMALV0(I) = SMALLV(I)
      FL = DSQRT(SMALV0(1)**2 + SMALV0(2)**2 + SMALV0(3)**2)
      IF (FL .LE. 0.0D0) GO TO 700
      DO 60 I = 1,3
   60 SMALV0(I) = SMALV0(I)/FL
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
      IF (.NOT.ABASIC) CALL TRANSD (ECPT(JCSIDA),TA)
      IF (.NOT.BBASIC) CALL TRANSD (ECPT(JCSIDB),TB)
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
C     FIRST WE STORE THE COORDINATES IN THE ARRAY DP SO THAT ALL
C     ARITHMETIC WILL BE DOUBLE PRECISION.
C
  100 DP(1)   = ECPT(JCSIDA+1)
      DP(2)   = ECPT(JCSIDA+2)
      DP(3)   = ECPT(JCSIDA+3)
      DP(4)   = ECPT(JCSIDB+1)
      DP(5)   = ECPT(JCSIDB+2)
      DP(6)   = ECPT(JCSIDB+3)
      VECI(1) = DP(1) - DP(4)
      VECI(2) = DP(2) - DP(5)
      VECI(3) = DP(3) - DP(6)
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
      CALL GMMATD (TA,3,3,0, DELA(1),3,1,0, DELA(4))
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
      CALL GMMATD (TB,3,3,0, DELB(1),3,1,0, DELB(4))
C
C     SINCE THERE WAS AT LEAST ONE NON-ZERO OFFSET VECTOR RECOMPUTE VECI
C
  140 VECI(1) = VECI(1) + DELA(IDELA  ) - DELB(IDELB  )
      VECI(2) = VECI(2) + DELA(IDELA+1) - DELB(IDELB+1)
      VECI(3) = VECI(3) + DELA(IDELA+2) - DELB(IDELB+2)
C
C     COMPUTE THE LENGTH OF THE BIG V (VECI) VECTOR AND NORMALIZE
C
  150 FL = DSQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      IF (FL .EQ. 0.0D0) GO TO 700
      DO 160 I = 1,3
  160 VECI(I) = VECI(I)/FL
C
C     COMPUTE THE SMALL V SUB 0 VECTOR, SMALV0.  ***CHECK THIS LOGIC***
C
      ITA = 1
      ISV = 1
      IF (MCSIDA.EQ.0 .OR. IGSUB0.EQ.0) GO TO 180
      IF (JCSIDA .NE. 39) ITA = 10
      ISV = 4
      CALL GMMATD (TA(ITA),3,3,0, SMALV0(1),3,1,0, SMALV0(4))
C
C     COMPUTE THE K VECTOR, VECK = VECI X SMALV0, AND NORMALIZE
C
  180 VECK(1) =  VECI(2)*SMALV0(ISV+2) - VECI(3)*SMALV0(ISV+1)
      VECK(2) =  VECI(3)*SMALV0(ISV  ) - VECI(1)*SMALV0(ISV+2)
      VECK(3) =  VECI(1)*SMALV0(ISV+1) - VECI(2)*SMALV0(ISV  )
      FLL = DSQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      IF (FLL .EQ. 0.0D0) GO TO 700
      VECK(1) =  VECK(1)/FLL
      VECK(2) =  VECK(2)/FLL
      VECK(3) =  VECK(3)/FLL
C
C     COMPUTE THE J VECTOR, VECJ = VECK X VECI, AND NORMALIZE
C
      VECJ(1) =  VECK(2)*VECI(3) - VECK(3)*VECI(2)
      VECJ(2) =  VECK(3)*VECI(1) - VECK(1)*VECI(3)
      VECJ(3) =  VECK(1)*VECI(2) - VECK(2)*VECI(1)
      FLL = DSQRT(VECJ(1)**2 + VECJ(2)**2 + VECJ(3)**2)
      IF (FLL .EQ. 0.0D0) GO TO 700
      VECJ(1) =  VECJ(1)/FLL
      VECJ(2) =  VECJ(2)/FLL
      VECJ(3) =  VECJ(3)/FLL
C
C     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
C     CONSTANT.
C
      MATIDC = IMATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
C     COMPUTE THE RECIPROCALS OF RV1 AND RV2 (CALLING THEM RRV1 AND
C     RRV2)
C
      L = FL
      LSQ = L**2
      LCUBE = LSQ*L
C
C     STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS.
C
      DP(1) = E S
      DP(2) = G S
      DP(3) = I1
      DP(4) = I2
      DP(5) = A
      EI1   = DP(1)*DP(3)
      EI2   = DP(1)*DP(4)
      IF (K1 .EQ. 0.0) GO TO 210
      DP(6) = K1
      GAK1  = DP(2)*DP(5)*DP(6)
      RRV1  = (12.0D0*EI1*GAK1)/(GAK1*LCUBE+12.0D0*L*EI1)
      GO TO 220
  210 RRV1  = 12.0D0*EI1/LCUBE
  220 IF (K2 .EQ. 0.0) GO TO 230
      DP(7) = K2
      GAK2  = DP(2)*DP(5)*DP(7)
      RRV2  = (12.0D0*EI2*GAK2)/(GAK2*LCUBE+12.0D0*L*EI2)
      GO TO 240
  230 RRV2  = 12.0D0*EI2/LCUBE
C
C     COMPUTE THE -SMALL- K-S, SK1, SK2, SK3 AND SK4
C
  240 SK1 = 0.25D0*RRV1*LSQ + EI1/L
      SK2 = 0.25D0*RRV2*LSQ + EI2/L
      SK3 = 0.25D0*RRV1*LSQ - EI1/L
      SK4 = 0.25D0*RRV2*LSQ - EI2/L
C
C     COMPUTE THE TERMS THAT WILL BE NEEDED FOR THE 12 X 12 MATRIX KE
C
      TERM1 = DP(5)*DP(1)/L
      TERM2 = 0.5D0*L*RRV1
      TERM3 = 0.5D0*L*RRV2
      DP(8) = FJ
      TERM4 = DP(2)*DP(8)/L
C
C     CONSTRUCT THE 12 X 12 MATRIX KE
C
      DO 250 I = 1,144
  250 KE(  I) =  0.0D0
      KE(  1) =  TERM1
      KE(  7) = -TERM1
      KE( 14) =  RRV1
      KE( 18) = -TERM2
      KE( 20) = -RRV1
      KE( 24) = -TERM2
      KE( 27) =  RRV2
      KE( 29) =  TERM3
      KE( 33) = -RRV2
      KE( 35) =  TERM3
      KE( 40) =  TERM4
      KE( 46) = -TERM4
      KE( 51) =  TERM3
      KE( 53) =  SK2
      KE( 57) = -TERM3
      KE( 59) =  SK4
      KE( 62) = -TERM2
      KE( 66) =  SK1
      KE( 68) =  TERM2
      KE( 72) =  SK3
      KE( 73) = -TERM1
      KE( 79) =  TERM1
      KE( 86) = -RRV1
      KE( 90) =  TERM2
      KE( 92) =  RRV1
      KE( 96) =  TERM2
      KE( 99) = -RRV2
      KE(101) = -TERM3
      KE(105) =  RRV2
      KE(107) = -TERM3
      KE(112) = -TERM4
      KE(118) =  TERM4
      KE(123) =  TERM3
      KE(125) =  SK4
      KE(129) = -TERM3
      KE(131) =  SK2
      KE(134) = -TERM2
      KE(138) =  SK3
      KE(140) =  TERM2
      KE(144) =  SK1
C
C     DETERMINE IF THERE ARE NON-ZERO PIN FLAGS.
C
      KA = IECPT(JPINA)
      KB = IECPT(JPINB)
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 325
C
C     SAVE THE KE (UNPINNED) MATRIX IN KES.
C
      DO 255 I = 1,144
  255 KES(I) = KE(I)
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
      IF (KE(II) .NE. 0.0D0) GO TO 280
      IL = IPIN(I)
      II = II - IL
      DO 270 J = 1,12
      II = II + 1
      KE(II) = 0.0D0
      KE(IL) = 0.0D0
      IL = IL + 12
  270 CONTINUE
      GO TO 320
  280 DO 300 J = 1,12
      JI  = 12*(J-1) + IPIN(I)
      IJ  = 12*(IPIN(I)-1) + J
      DO 290 LL = 1,12
      JLL = 12*(J-1) + LL
      ILL = 12*(IPIN(I)-1) + LL
      KEP(JLL) = KE(JLL) - (KE(ILL)/KE(II))*KE(JI)
  290 CONTINUE
      KEP(IJ) = 0.0D0
      KEP(JI) = 0.0D0
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
      KEP(J   ) = KE(K  )
  330 KEP(J+36) = KE(K+6)
  340 CONTINUE
C
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
      BASIC  = ABASIC
      JCSID  = JCSIDA
      OFFSET = AOFSET
      JOFSET = JOFSTA
      IWBEG  = 0
      IKEL   = 1
      IAB    = 1
C
C     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W  AND W
C     6 X 6 MATRICES WILL RESIDE.                          A      B
C
      DO 360 I = 28,108
  360 KE(I) = 0.0D0
C
C     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G MATRIX
C     G = AT X TI
C
  365 IG = 1
      IF (BASIC) GO TO 370
      CALL TRANSD (ECPT(JCSID),KE(10))
      CALL GMMATD (KE(1),3,3,0, KE(10),3,3,0, KE(19))
      IG = 19
C
C     IF THERE IS A NON-ZERO OFFSET FOR THE POINT, SET UP THE D 3 X 3
C     MATRIX.
C
  370 IF (.NOT.OFFSET) GO TO 380
      KE(10) =  0.0D0
      KE(11) =  ECPT(JOFSET+2)
      KE(12) = -ECPT(JOFSET+1)
      KE(13) = -KE(11)
      KE(14) =  0.0D0
      KE(15) =  ECPT(JOFSET)
      KE(16) = -KE(12)
      KE(17) = -KE(15)
      KE(18) =  0.0D0
C
C     FORM THE 3 X 3 PRODUCT H = G X D, I.E., KE(28) = KE(IG) X KE(10)
C
      CALL GMMATD (KE(IG),3,3,0, KE(10),3,3,0, KE(28))
C
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
C
C                                 E                     E
C     COMPUTE THE PRODUCT S   =  K   X  W   OR  S   =  K    X  W
C                          A      AA     A       B      AB      B
C     WHERE
C                  T                           T
C           W  =  T   X  C  X  E   AND  W  =  T   X  C   X  E
C            A     EB     A     A        B     EB     B      B
C
C     W AT KE(37) AND W AT KE(73) WILL BE USED AGAIN BEFORE FINAL STEPS.
C      A               B
C
  390 CALL GMMATD (KEP(IKEL),6,6,0, KE(IWBEG+37),6,6,0, SA(IAB))
C
C     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED. IF
C     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
C
      IF (IWBEG .EQ. 36) GO TO 500
      BASIC  = BBASIC
      JCSID  = JCSIDB
      OFFSET = BOFSET
      JOFSET = JOFSTB
      IWBEG  = 36
      IKEL   = 37
      IAB    = 37
      DO 400 I = 28,36
  400 KE(I)  = 0.0D0
      GO TO 365
C
C     BEGIN DIFFERENTIAL STIFFNESS PORTION OF THIS ROUTINE.
C
C     STORE DISPLACEMENT VECTORS IN DOUBLE PRECISION LOCATIONS
C
  500 DO 510 I = 1,6
      UA(I) = ECPT(I+49)
  510 UB(I) = ECPT(I+55)
C
C     COMPUTE  S  X  U   AND  S  X  U
C               A     A        B     B
C
      CALL GMMATD (SA(1),6,6,0, UA,6,1,0, DPVECA)
      CALL GMMATD (SB(1),6,6,0, UB,6,1,0, DPVECB)
C
C     COMPUTE THE NEEDED COMPONENTS OF THE FORCE VECTOR.
C
      FX  = DPVECA(1) + DPVECB(1)
      VY  = DPVECA(2) + DPVECB(2)
      VZ  = DPVECA(3) + DPVECB(3)
      MAY = DPVECA(5) + DPVECB(5)
      MAZ = DPVECA(6) + DPVECB(6)
      MBZ = -MAZ - VY*L
      MBY = -MAY + VZ*L
      E   = E S
      FX  = FX - E*ELDEF/L
      IF (IECPT(49) .EQ. -1) GO TO 520
      ALPHA = ALPHAS
      TSUB0 = TSUB0S
      DP(1) = TEMPER
      FX  = FX - A*ALPHA*E*(DP(1)-TSUB0)
C
C     ZERO OUT THE KD (KC) MATRIX
C
  520 DO 530 I = 1,144
  530 KD(I) = 0.0D0
C
C     FORM THE ELEMENT DIFFERENTIAL STIFFNESS MATRIX (UPPER HALF)
C
      TERM1  = 6.0D0*FX/(5.0D0*L)
      TERM2  = -MAY/L
      TERM3  = FX/10.0D0
      TERM4  = -MBY/L
      TERM5  = -MAZ/L
      TERM6  = -MBZ/L
      DFJ    = I1 + I2
      DA     = A
      TERM7  = DFJ*FX/(L*DA)
      TERM8  = L*VY/6.0D0
      TERM9  = L*VZ/6.0D0
      TERM10 = 2.0D0*L*FX/15.0D0
      TERM11 = L*FX/30.0D0
      KC( 2, 2) =  TERM1
      KC( 2, 4) =  TERM2
      KC( 2, 6) = -TERM3
      KC( 2, 8) = -TERM1
      KC( 2,10) =  TERM4
      KC( 2,12) = -TERM3
      KC( 3, 3) =  TERM1
      KC( 3, 4) =  TERM5
      KC( 3, 5) =  TERM3
      KC( 3, 9) = -TERM1
      KC( 3,10) =  TERM6
      KC( 3,11) =  TERM3
      KC( 4, 4) =  TERM7
      KC( 4, 5) = -TERM8
      KC( 4, 6) = -TERM9
      KC( 4, 8) = -TERM2
      KC( 4, 9) = -TERM5
      KC( 4,10) = -TERM7
      KC( 4,11) =  TERM8
      KC( 4,12) =  TERM9
      KC( 5, 5) =  TERM10
      KC( 5, 9) = -TERM3
      KC( 5,10) =  TERM8
      KC( 5,11) = -TERM11
      KC( 6, 6) =  TERM10
      KC( 6, 8) =  TERM3
      KC( 6,10) =  TERM9
      KC( 6,12) = -TERM11
      KC( 8, 8) =  TERM1
      KC( 8,10) = -TERM4
      KC( 8,12) =  TERM3
      KC( 9, 9) =  TERM1
      KC( 9,10) = -TERM6
      KC( 9,11) = -TERM3
      KC(10,10) =  TERM7
      KC(10,11) = -TERM8
      KC(10,12) = -TERM9
      KC(11,11) =  TERM10
      KC(12,12) =  TERM10
C
C     STORE THE UPPER HALF IN THE LOWER HALF.
C
      DO 550 I = 2,10
      LOW = I + 1
      DO 540 J = LOW,12
      KC(J,I) = KC(I,J)
  540 CONTINUE
  550 CONTINUE
C
C     IF THERE PIN FLAGS, ALTER THE KD MATRIX
C
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 620
C
C     ALTER KD DUE TO PIN FLAGS.
C
      DO 610 J = 1,10
      IF (IPIN(J) .EQ. 0) GO TO 610
      JJ = 12*(IPIN(J)-1) + IPIN(J)
      IF (KES(JJ) .EQ. 0.0D0) GO TO 605
      DO 590 I = 1,12
      JI = 12*(IPIN(J)-1) + I
      IJ = 12*(I-1) + IPIN(J)
      DO 580 L1 = 1,12
      IL = 12*(I-1) + L1
      LJ = 12*(L1-1) + IPIN(J)
      KDP(IL) = KD(IL) - KES(LJ)*KD(JI)/KES(JJ) - KES(JI)*KD(LJ)/KES(JJ)
     2        + KES(LJ)* KES(JI)*KD(JJ)/KES(JJ)**2
  580 CONTINUE
  590 CONTINUE
      DO 600 KK = 1,144
  600 KD(KK) = KDP(KK)
C
C     ZERO OUT THE IPIN(J) TH ROW AND COLUMN OF KD.
C
  605 J1 = JJ - IPIN(J)
      J2 = IPIN(J)
      DO 608 KK = 1,12
      J1 = J1 + 1
      KD(J1) = 0.0D0
      KD(J2) = 0.0D0
  608 J2 = J2 + 12
  610 CONTINUE
C
C            D
C     STORE K        AT KEP(1),...,KEP(36)  AND
C            NPVT,A
C
C            D
C           K        AT KEP(37),...,KEP(72)
C            NPVT,B
C
C
  620 J = 0
      IF (IPVT .EQ. 2) GO TO 625
      ILOW = 1
      ILIM = 72
      GO TO 628
  625 ILOW = 73
      ILIM = 144
  628 DO 640 I = ILOW,ILIM,12
      LOW  = I
      LIM  = LOW +5
      DO 630 K = LOW,LIM
      J = J + 1
      KEP(J   ) = KD(K  )
  630 KEP(J+36) = KD(K+6)
  640 CONTINUE
C
C     COMPUTE THE FINAL 2 6X6 DIFFERENTIAL STIFFNESS MATRICES FOR THIS
C     BEAM.
C
      IWLEFT = 37
      IF (IPVT .EQ. 2) IWLEFT = 73
      I = 1
      IKDE = 1
      IWRGHT = 37
  650 CALL GMMATD (KE(IWLEFT),6,6,1, KEP(IKDE),6,6,0, KEP(73))
      CALL GMMATD (KEP(73),6,6,0, KE(IWRGHT),6,6,0, KEP(109))
      CALL DS1B (KEP(109),ISILNO(I))
      IF (I .EQ. 2) RETURN
      I = 2
      IKDE = 37
      IWRGHT = 73
      GO TO 650
C
C     FATAL ERROR
C
  700 CALL MESAGE (30,26,IECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
      END
